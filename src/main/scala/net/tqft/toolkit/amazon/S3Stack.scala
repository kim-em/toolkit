package net.tqft.toolkit.amazon
import net.tqft.toolkit.collections.Stack

object S3Stack {
  def apply(bucket: String, prefix: String = ""): Stack[String] = {
    val s3Bucket = S3(bucket)

    def timestamp = (Long.MaxValue - System.currentTimeMillis()).toString()

    new Stack[String] {
      def push(s: String) = s3Bucket += ((prefix + timestamp, s))
      def pop = s3Bucket.keysWithPrefix(prefix, 1).headOption.flatMap({ k => val r = s3Bucket.get(k); s3Bucket -= k; r })
      def clear = for (k <- s3Bucket.keysWithPrefix(prefix)) s3Bucket -= k
    }
  }
}

