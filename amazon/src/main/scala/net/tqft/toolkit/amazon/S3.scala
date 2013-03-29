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
import org.apache.commons.io.IOUtils

trait S3 {
  def AWSAccount: String
  def AWSSecretKey: String

  lazy val credentials = new AWSCredentials(AWSAccount, AWSSecretKey)
  lazy val s3Service: StorageService = new RestS3Service(credentials)

  def apply(bucket: String): S3Bucket[String] = new S3BucketPlain(s3Service, bucket)
  def GZIP(bucket: String): S3Bucket[String] = GZIPkeys(new S3BucketGZIP(s3Service, bucket))
  def source(bucket: String): S3Bucket[Source] = new S3BucketSource(s3Service, bucket)
  def sourceGZIP(bucket: String): S3Bucket[Source] = GZIPkeys(new S3BucketSourceGZIP(s3Service, bucket))
  def bytes(bucket: String): S3Bucket[Array[Byte]] = new S3BucketBytes(s3Service, bucket)
  def bytesGZIP(bucket: String): S3Bucket[Array[Byte]] = new S3BucketBytesGZIP(s3Service, bucket)

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

  override def toString = "https://s3.amazonaws.com/" + bucket
  override def hashCode = toString.hashCode

  override def equals(other: Any) = other match {
    case other: S3Bucket[_] => bucket == other.bucket
    case _ => false
  }

  def putIfAbsent(key: String, value: A): Boolean = {
    if(!contains(key)) {
      put(key, value)
      true
    } else {
      false
    }
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

  override def keys: Iterable[String] = {
    keysWithPrefix("")
  }

  override def size = {
    //    s3Service.listObjects(bucket).length
    keys.size
  }

  override def clear = {
    for (so <- s3Service.listObjects(bucket).par) {
      s3Service.deleteObject(bucket, so.getKey())
    }
  }

  override def iterator: Iterator[(String, A)] = {
    //    val keys = try {
    //      s3Service.listObjects(bucket).iterator.map({ _.getKey() })
    //    } catch {
    //      case e: Exception =>
    //        Logging.error("exception while listing objects in S3.", e)
    //        Iterator.empty
    //    }
    keys.iterator.map({ k: String => (k, get(k).get) })
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

private class S3BucketBytes(val s3Service: StorageService, val bucket: String) extends S3BucketBytesWrapper(new S3BucketStreaming(s3Service, bucket)) with S3Bucket[Array[Byte]]
private class S3BucketBytesGZIP(val s3Service: StorageService, val bucket: String) extends S3BucketBytesWrapper(new S3BucketStreamingGZIP(s3Service, bucket)) with S3Bucket[Array[Byte]]

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

private class S3BucketBytesWrapper(map: scala.collection.mutable.Map[String, Either[InputStream, Array[Byte]]]) extends MapTransformer.ValueTransformer[String, Either[InputStream, Array[Byte]], Array[Byte]](
  map,
  { e: Either[InputStream, Array[Byte]] =>
    {
      e match {
        case Left(stream) => IOUtils.toByteArray(stream);
        case Right(bytes) => bytes
      }
    }
  },
  { bytes: Array[Byte] =>
    {
      Right(bytes)
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

  override def iterator: Iterator[(String, Left[InputStream, Array[Byte]])] = {
    keys.iterator.map({ k: String => (k, get(k).get) })
  }
}

object S3 extends S3 {
  private val accessPath = System.getProperty("user.home") + "/.ec2/access"
  private val accounts = scala.io.Source.fromFile(accessPath).getLines.filter(!_.startsWith("#")).toStream.map(_.split(",").map(_.trim).toList)
  override lazy val AWSAccount = accounts(0)(0)
  override lazy val AWSSecretKey = accounts(0)(1)

  def withAccount(accountName: String): S3 = {
    accounts.find(_.head == accountName) match {
      case None => throw new IllegalArgumentException("No AWS account named " + accountName + " found in " + accessPath)
      case Some(accountName :: secretKey :: _) => {
        new S3 {
          override val AWSAccount = accountName
          override val AWSSecretKey = secretKey
        }
      }
      case _ => throw new IllegalArgumentException("Encountered a problem parsing " + accessPath)
    }
  }
}

object AnonymousS3 extends S3 {
    override def AWSAccount = null
    override def AWSSecretKey = null
    override lazy val credentials = null
}