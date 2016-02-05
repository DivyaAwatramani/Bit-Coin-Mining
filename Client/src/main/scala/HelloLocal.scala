import java.nio.CharBuffer
import java.security.MessageDigest

import akka.actor.Actor
import akka.actor.ActorSystem
import akka.actor.Props
import akka.actor.actorRef2Scala
import akka.routing.RoundRobinRouter

import scala.util.Random



object BitcoinClient extends App {

  case class Bitcoin(bitcoinString: StringBuffer)
  case class SHAGenerate(leadingZeros: Int)
  //def main(args: Array[String]) {
    implicit val system = ActorSystem("LocalSystem")
    val ClientActor = system.actorOf(Props[ClientActor], name = "LocalActor")  
    ClientActor ! "START"  
  //}

class SHAGen1 extends Actor {
  def receive = {
    case SHAGenerate(leadingZeros) => {
            val stringLength=10
             val RandomGenerator=Random.alphanumeric.take(stringLength).mkString
          for (i <- 0 to 100) { 
                //println("actor instance"+ self.path.name)
                val randomstring ="dawatram" + RandomGenerator;
                  val uniqueString="dawatram"+randomstring
                 // println("random string" + uniqueString)
                  val SHAAlgorithm: MessageDigest=MessageDigest.getInstance("SHA-256")
                   val ByteArray: Array[Byte]=  uniqueString.getBytes("UTF-8")
                    SHAAlgorithm.reset()
                    SHAAlgorithm.update(ByteArray)
                    val messageDigest: Array[Byte]=SHAAlgorithm.digest
                    val hexString: StringBuffer=new StringBuffer
                    messageDigest foreach { digest => 
                    val hex= Integer.toHexString(0xff & digest)
                        if(hex.length==1)
                          hexString.append('0')
hexString.append(hex)
                      }
                    val LeadingZeros=hexString.substring(0, leadingZeros)
                     val x=CharBuffer.allocate(leadingZeros).toString().replace('\0', '0')
                  println(uniqueString + "\t" + hexString)
                      if(LeadingZeros==x)
                      {
                         
                          sender ! Bitcoin(hexString)
                      }               
                  }
    }
                  case _ => println("no match")
   
}
}
    
    
    

class WorkDistributer extends Actor{
 
  val noOfChildActors=10
  
val remote1 = context.actorFor("akka.tcp://BitcoinServer@"+ args(0)+ ":5150/user/master")
  val clientActors= context.actorOf(Props[SHAGen1].withRouter(RoundRobinRouter(noOfChildActors)),name="ClienactorRoute")
    def receive={
  
          case Bitcoin(bitcoinString) =>
            {
              println(bitcoinString)  
               remote1 ! bitcoinString
              
            }
      
          case msg: Int =>
            {
               for (i<-0 to 10)
               clientActors ! SHAGenerate(msg)
               
            }
  
          }
 
}
class ClientActor extends Actor {

  val remote = context.actorFor("akka.tcp://BitcoinServer@"+ args(0)+ ":5150/user/master")
  
  var counter = 0

  def receive = {
    case "START" =>
        remote ! "REQUEST"

    case msg: Int =>
        {
        println("No. of leading zeros"+ msg)
        val WorkDistributor = system.actorOf(Props[WorkDistributer], name = "WorkDistributor")  
        WorkDistributor ! msg
        }
        
    case bitcoin: StringBuffer =>
      {
println(" finally")
        sender ! bitcoin
      }
}
  }

}
