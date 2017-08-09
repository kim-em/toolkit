import com.xerox.amazonws.ec2.Jec2
import scala.collection.JavaConversions.asScalaBuffer
import scala.collection.JavaConversions.seqAsJavaList

object DescribeInstancesApp extends App {
  import com.xerox.amazonws.ec2._

  val credentialsFile = scala.io.Source.fromFile(System.getProperty("user.home") + "/.ec2/access").getLines.filter(!_.startsWith("#"))
  val credentials = credentialsFile.next.split(",")
  val EC2 = new Jec2(credentials(0), credentials(1))

  import scala.collection.JavaConversions._
  val instances = EC2.describeInstances(List()) flatMap { _.getInstances }
  for (instance <- instances; if instance.isRunning) println(instance.getDnsName)
}