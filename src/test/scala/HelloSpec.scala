import org.scalatest._
import  zw.co.base2theory.paynow._

class HelloSpec extends FlatSpec with Matchers {
  println("[=] Starting http test server")
  "Hello" should "have tests" in {
    true should === (true)
  }

  //TODO: Add test
  def main(args: Array[String]){
		val cfg = Config.makeDefault("ikey", "secret")
		val init = Transaction.init(cfg)
		val trans = init(90.00, "INV-1001", "http://ping.eu/after-payment", "http://ping.eu/payment/response")
		trans.process match{
			case res: Response.Success => {
				val t = Transaction.poll(cfg)("http://localhost:9090/interface/checkpayment")
				println(t.resultUrl)
			}
		}
		println(trans.process)
		//println(Transaction.handler(cfg, "http://paynow.co.zw/callback")(Map[String, String]()))
	}
}
