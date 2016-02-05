package remote

import akka.actor._          
import scala.util.Random        
import akka.routing.RoundRobinRouter
import java.security.MessageDigest   
import java.nio.CharBuffer

object Server extends App  {
	var zero = 2
	case object start
	case class SHAGenerate(leadingZeros: Int)

	val system = ActorSystem("BitcoinServer")

	val MainServer = system.actorOf(Props(new masterServer(zero)),name="master")
	MainServer ! start

	class masterServer(LeadingZeros: Int) extends  Actor {
		val noOfChildActors=3
		val worker= context.actorOf(Props[SHAGen1].withRouter(RoundRobinRouter(noOfChildActors)),name="Worker")
		def receive={

			case "REQUEST" => {
				println("Request Recieved from Client; Start Mining")
				sender ! LeadingZeros
			}	
			case msg : StringBuffer => {
				println(msg+ "\n")  
			}

			case start => {
				println("Started")
				for (i<-0 to 1000)
					worker ! SHAGenerate(LeadingZeros)
			}
		}
	}

	class SHAGen1 extends Actor {
	  def receive = {
		case SHAGenerate(leadingZeros) => {
				var count = 0
				val stringLength=20
				for (i <- 0 to 10000) {
					val randomstring = Random.alphanumeric.take(stringLength).mkString;
					val uniqueString="dawatram"+randomstring
					val SHAAlgorithm: MessageDigest=MessageDigest.getInstance("SHA-256")
					val ByteArray: Array[Byte]=  uniqueString.getBytes
					SHAAlgorithm.reset()
					SHAAlgorithm.update(ByteArray)
					val messageDigest: Array[Byte]=SHAAlgorithm.digest
					val hexString: StringBuffer=new StringBuffer
					messageDigest foreach { digest => 
						val hex= Integer.toHexString(0xFF & digest)
						if(hex.length==1)
							hexString.append('0')
						hexString.append(hex)
					}
					val LeadingZeros=hexString.substring(0, leadingZeros)
					val x=CharBuffer.allocate(leadingZeros).toString().replace('\0', '0')
				  
					if(LeadingZeros==x) {
						count=count+1;
						println(uniqueString+"\t"+ hexString+"\n")
					}  
				}
			}
			case _ => println("no match")
	   
		}
	}
}
