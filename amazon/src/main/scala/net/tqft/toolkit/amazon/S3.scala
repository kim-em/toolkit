package net.tqft.toolkit.amazon

import java.util.zip.GZIPOutputStream
import java.io.PrintWriter
import java.io.ByteArrayOutputStream
import java.util.zip.GZIPInputStream
import org.jets3t.service.model.StorageObject
import java.io.OutputStream
import java.io.InputStream
import org.jets3t.service.security.AWSCredentials
import org.jets3t.service.StorageService
import org.jets3t.service.impl.rest.httpclient.RestS3Service
import scala.io.Source
import org.jets3t.service.model.S3Object
import net.tqft.toolkit.collections.MapTransformer
import net.tqft.toolkit.collections.NonStrictIterable
import net.tqft.toolkit.Logging

trait S3 {
  def AWSAccount: String
  def AWSSecretKey: String

  lazy val credentials = new AWSCredentials(AWSAccount, AWSSecretKey)
  lazy val s3Service: StorageService = new RestS3Service(credentials)

  def apply(bucket: String): S3Bucket[String] = new S3BucketPlain(s3Service, bucket)
  def GZIP(bucket: String): S3Bucket[String] = GZIPkeys(new S3BucketGZIP(s3Service, bucket))
  def source(bucket: String): S3Bucket[Source] = new S3BucketSource(s3Service, bucket)
  def sourceGZIP(bucket: String): S3Bucket[Source] = GZIPkeys(new S3BucketSourceGZIP(s3Service, bucket))

  private def GZIPkeys[V](s3Bucket: S3Bucket[V]): S3Bucket[V] = {
    import MapTransformer._
    new KeyTransformer(s3Bucket, { key: String => { if (key.endsWith(".gz")) Some(key.dropRight(3)) else None } }, { key: String => key + ".gz" }) with S3Bucket[V] {
      def s3Service = s3Bucket.s3Service
      def bucket = s3Bucket.bucket
    }
  }
}

trait S3Bucket[A] extends scala.collection.mutable.Map[String, A] {
  def s3Service: StorageService
  def bucket: String
  def keysWithPrefix(prefix: String, queryChunkSize: Int = 1000) = {
    val chunks = try {
      def initial = s3Service.listObjectsChunked(bucket, prefix, null, queryChunkSize, null)
      NonStrictIterable.iterateUntilNone(initial)({ chunk =>
        {
          if (chunk.getObjects().size < queryChunkSize) {
            None
          } else {
            try {
              Some(s3Service.listObjectsChunked(bucket, prefix, null, queryChunkSize, chunk.getObjects().last.getKey()))
            } catch {
              case e: Exception =>
                Logging.error("Exception while listing objects in S3 bucket.", e)
                None
            }
          }
        }
      })
    } catch {
      case e: Exception =>
        Logging.error("Exception while listing objects in S3 bucket.", e)
        NonStrictIterable()
    }
    chunks.map(_.getObjects().map(_.getKey)).flatten
  }
}

private class S3BucketPlain(val s3Service: StorageService, val bucket: String) extends S3BucketWrapper(new S3BucketStreaming(s3Service, bucket)) with S3Bucket[String]
private class S3BucketGZIP(val s3Service: StorageService, val bucket: String) extends S3BucketWrapper(new S3BucketStreamingGZIP(s3Service, bucket)) with S3Bucket[String]
private class S3BucketSource(val s3Service: StorageService, val bucket: String) extends S3BucketSourceWrapper(new S3BucketStreaming(s3Service, bucket)) with S3Bucket[Source]
private class S3BucketSourceGZIP(val s3Service: StorageService, val bucket: String) extends S3BucketSourceWrapper(new S3BucketStreamingGZIP(s3Service, bucket)) with S3Bucket[Source]

private class S3BucketWrapper(map: scala.collection.mutable.Map[String, Either[InputStream, Array[Byte]]]) extends MapTransformer.ValueTransformer[String, Either[InputStream, Array[Byte]], String](
  map,
  { e: Either[InputStream, Array[Byte]] =>
    {
      e match {
        case Left(stream) => {
          try {
            val result = Source.fromInputStream(stream).getLines.mkString("\n")
            stream.close
            result
          } catch {
            case e: Exception =>
              Logging.error("Exception while listing objects in S3 bucket.", e)
              ""
          }
        }
        case Right(_) => throw new UnsupportedOperationException
      }
    }
  },
  { s: String =>
    {
      Right(s.getBytes)
    }
  })

private class S3BucketSourceWrapper(map: scala.collection.mutable.Map[String, Either[InputStream, Array[Byte]]]) extends MapTransformer.ValueTransformer[String, Either[InputStream, Array[Byte]], Source](
  map,
  { e: Either[InputStream, Array[Byte]] =>
    {
      e match {
        case Left(stream) => Source.fromInputStream(stream)
        case Right(_) => throw new UnsupportedOperationException
      }
    }
  },
  { s: Source =>
    {
      Right(s.toArray map { _.toByte })
    }
  })

private class S3BucketStreamingGZIP(s3Service: StorageService, val bucket: String) extends MapTransformer.ValueTransformer[String, Either[InputStream, Array[Byte]], Either[InputStream, Array[Byte]]](
  new S3BucketStreaming(s3Service, bucket),
  { e: Either[InputStream, Array[Byte]] =>
    {
      e match {
        case Left(stream) => Left(new GZIPInputStream(stream))
        case Right(_) => throw new UnsupportedOperationException
      }
    }
  },
  { e: Either[InputStream, Array[Byte]] =>
    e match {
      case Left(stream) => throw new UnsupportedOperationException
      case Right(bytes) => {
        try {
          val baos = new ByteArrayOutputStream()
          val gzos = new GZIPOutputStream(baos)
          gzos.write(bytes)
          gzos.close
          Right(baos.toByteArray)
        } catch {
          case e: Exception =>
            Logging.error("exception while reading GZIPOutputStream from S3.", e)
            Right(new Array[Byte](0))
        }
      }
    }
  })

private class S3BucketStreaming(val s3Service: StorageService, val bucket: String) extends S3Bucket[Either[InputStream, Array[Byte]]] {
  val s3bucket = s3Service.getOrCreateBucket(bucket)

  override def hashCode = s3bucket.toString.hashCode
  override def equals(other: Any) = other match {
    case other: S3Bucket[_] => bucket == other.bucket
  }

  override def contains(key: String) = {
    try {
      s3Service.isObjectInBucket(bucket, key)
    } catch {
      case e: Exception =>
        Logging.error("exception while looking for an object in S3.", e)
        false
    }

  }

  def contentLength(key: String): Option[Long] = {
    if (contains(key)) {
      try {
        Some(s3Service.getObject(bucket, key).getContentLength)
      } catch {
        case e: Exception =>
          Logging.error("exception while checking ContentLength on an object in S3.", e)
          None
      }
    } else {
      None
    }
  }

  override def get(key: String): Option[Left[InputStream, Array[Byte]]] = {
    try {
      if (s3Service.isObjectInBucket(bucket, key)) {
        Some(Left(s3Service.getObject(bucket, key).getDataInputStream))
      } else {
        None
      }
    } catch {
      case e: Exception =>
        Logging.error("exception while reading an object from S3.", e)
        None
    }
  }

  override def keys: Iterable[String] = {
    keysWithPrefix("")
  }

  override def size = {
    s3Service.listObjects(bucket).length
  }
  
  override def clear = {
    for(so <- s3Service.listObjects(bucket).par) {
      s3Service.deleteObject(bucket, so.getKey())
    }
  }
  
  override def iterator: Iterator[(String, Left[InputStream, Array[Byte]])] = {
    val keys = try {
      (s3Service.listObjects(bucket) map { _.getKey() }).iterator
    } catch {
      case e: Exception =>
        Logging.error("exception while listing objects in S3.", e)
        Iterator.empty
    }
    keys map { k: String => (k, get(k).get) }
  }

  override def +=(kv: (String, Either[InputStream, Array[Byte]])) = {
    kv match {
      case (key, Right(bytes)) => try {
        s3Service.putObject(bucket, new S3Object(key, bytes))
      } catch {
        case e: Exception =>
          Logging.error("exception while writing an object to S3.", e)
      }
      case (key, Left(_)) => throw new UnsupportedOperationException
    }
    this
  }
  override def -=(key: String) = {
    try {
      s3Service.deleteObject(bucket, key)
    } catch {
      case e: Exception =>
        Logging.error("exception while deleting an object from S3.", e)
    }
    this
  }
}

object S3 extends S3 {
  private val pair = scala.io.Source.fromFile(System.getProperty("user.home") + "/home/ec2-keys/access").getLines.filter(!_.startsWith("#")).next.split(",").toList
  val AWSAccount = pair(0)
  val AWSSecretKey = pair(1)
}