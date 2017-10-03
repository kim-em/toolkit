import com.xerox.amazonws.ec2.Jec2

object DescribeInstancesApp extends App {
  import com.xerox.amazonws.ec2._

  val credentialsFile = scala.io.Source.fromFile(System.getProperty("user.home") + "/.ec2/access").getLines.filter(!_.startsWith("#"))
  val credentials = credentialsFile.next.split(",")
  val EC2 = new Jec2(credentials(0), credentials(1))

  import scala.collection.JavaConverters._
  val instances = EC2.describeInstances(List[String]().asJava).asScala.flatMap(_.getInstances.asScala)
  for (instance <- instances; if instance.isRunning) println(instance.getDnsName)
}