package net.tqft.toolkit
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

  def apply(bucket: String) = new S3Bucket(s3Service, bucket)
}

class S3Bucket(s3Service: StorageService, val bucket: String) extends scala.collection.mutable.Map[String, String] {
  val s3bucket = s3Service.getOrCreateBucket(bucket)
  
  override def hashCode = s3bucket.toString.hashCode
  override def equals(other: Any) = other match {
    case other: S3Bucket => bucket == other.bucket
  }
  
  override def contains(key: String) = { 
    s3Service.isObjectInBucket(bucket, key)
  }
  
  def contentLength(key: String): Option[Long] = {
    if(contains(key)) {
    	Some(s3Service.getObject(bucket, key).getContentLength)
    } else {
      None
    }
  }
  
  override def get(key: String): Option[String] = {
    if (s3Service.isObjectInBucket(bucket, key)) {
      Some(Source.fromInputStream(s3Service.getObject(bucket, key).getDataInputStream).getLines.mkString)
    } else {
      None
    }
  }
  
  override def keys: Iterable[String] = {
    (s3Service.listObjects(bucket) map { _.getKey() })
  }
  
  override def iterator: Iterator[(String, String)] = {
    val keys = (s3Service.listObjects(bucket) map { _.getKey() }).iterator
    keys map { k: String => (k, get(k).get) }
  }

  override def +=(kv: (String, String)) = {
    kv match {
      case (key, value) => s3Service.putObject(bucket, new S3Object(key, value))
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