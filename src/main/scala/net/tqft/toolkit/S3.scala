package net.tqft.toolkit

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

trait S3 {

  def AWSAccount: String
  def AWSSecretKey: String

  lazy val credentials = new AWSCredentials(AWSAccount, AWSSecretKey)
  lazy val s3Service: StorageService = new RestS3Service(credentials)

  def apply(bucket: String): scala.collection.mutable.Map[String, String] = new S3Bucket(s3Service, bucket)
  def GZIP(bucket: String): scala.collection.mutable.Map[String, String] = new S3BucketGZIP(s3Service, bucket)
  def source(bucket: String): scala.collection.mutable.Map[String, Source] = new S3BucketSource(s3Service, bucket)
  def sourceGZIP(bucket: String): scala.collection.mutable.Map[String, Source] = new S3BucketSourceGZIP(s3Service, bucket)
}

//class S3Bucket(s3Service: StorageService, val bucket: String) extends scala.collection.mutable.Map[String, String] {
//  val s3bucket = s3Service.getOrCreateBucket(bucket)
//
//  override def hashCode = s3bucket.toString.hashCode
//  override def equals(other: Any) = other match {
//    case other: S3Bucket => bucket == other.bucket
//  }
//
//  override def get(key: String): Option[String] = {
//    if (s3Service.isObjectInBucket(bucket, key)) {
//      Some(Source.fromInputStream(s3Service.getObject(bucket, key).getDataInputStream).getLines.mkString)
//    } else {
//      None
//    }
//  }
//  override def iterator: Iterator[(String, String)] = {
//    val keys = (s3Service.listObjects(bucket) map { _.getKey() }).iterator
//    keys map { k: String => (k, get(k).get) }
//  }
//
//  override def +=(kv: (String, String)) = {
//    kv match {
//      case (key, value) => s3Service.putObject(bucket, new S3Object(key, value))
//    }
//    this
//  }
//  override def -=(key: String) = {
//    s3Service.deleteObject(bucket, key)
//    this
//  }
//}

private class S3Bucket(s3Service: StorageService, val bucket: String) extends S3BucketWrapper(new S3BucketStreaming(s3Service, bucket))
private class S3BucketGZIP(s3Service: StorageService, val bucket: String) extends S3BucketWrapper(new S3BucketStreamingGZIP(s3Service, bucket))
private class S3BucketSource(s3Service: StorageService, val bucket: String) extends S3BucketSourceWrapper(new S3BucketStreaming(s3Service, bucket))
private class S3BucketSourceGZIP(s3Service: StorageService, val bucket: String) extends S3BucketSourceWrapper(new S3BucketStreamingGZIP(s3Service, bucket))

private class S3BucketWrapper(map: scala.collection.mutable.Map[String, Either[InputStream, Array[Byte]]]) extends MapTransformer.ValueTransformer[String, Either[InputStream, Array[Byte]], String](
  map,
  { e: Either[InputStream, Array[Byte]] =>
    {
      e match {
        case Left(stream) => Source.fromInputStream(stream).getLines.mkString
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
        val baos = new ByteArrayOutputStream()
        val gzos = new GZIPOutputStream(baos)
        gzos.write(bytes)
        gzos.close
        Right(baos.toByteArray)
      }
    }
  })

private class S3BucketStreaming(s3Service: StorageService, val bucket: String) extends scala.collection.mutable.Map[String, Either[InputStream, Array[Byte]]] {
  val s3bucket = s3Service.getOrCreateBucket(bucket)

  override def hashCode = s3bucket.toString.hashCode
  override def equals(other: Any) = other match {
    case other: S3Bucket => bucket == other.bucket
  }

  override def get(key: String): Option[Left[InputStream, Array[Byte]]] = {
    if (s3Service.isObjectInBucket(bucket, key)) {
      Some(Left(s3Service.getObject(bucket, key).getDataInputStream))
    } else {
      None
    }
  }
  override def iterator: Iterator[(String, Left[InputStream, Array[Byte]])] = {
    val keys = (s3Service.listObjects(bucket) map { _.getKey() }).iterator
    keys map { k: String => (k, get(k).get) }
  }

  override def +=(kv: (String, Either[InputStream, Array[Byte]])) = {
    kv match {
      case (key, Right(bytes)) => s3Service.putObject(bucket, new S3Object(key, bytes))
      case (key, Left(_)) => throw new UnsupportedOperationException
    }
    this
  }
  override def -=(key: String) = {
    s3Service.deleteObject(bucket, key)
    this
  }
}

object S3 extends S3 {
  val AWSAccount = "0D4BTQXQJ7SAKKQHF982"
  val AWSSecretKey = "wQsXfibiPzfPFDZ84jWXIjNb9UfqnLh42+FHhqtp"
}