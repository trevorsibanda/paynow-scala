package zw.co.base2theory.paynow

trait Transaction{
	val reference: String
	val amount: Double
	val additionalInfo: Option[String] = None
	val returnUrl: String
	val resultUrl: String
	val authEmail: Option[String] = None
	val status : String = "Message"
	val config: Config
	def id: String = config.integrationID
	
	private def _process = {
		val map : Map[String, String] = Map(
			"id"-> id,
			"reference"->reference,
			"amount"->amount.toString,
			"additionalinfo"->additionalInfo.getOrElse(""),
			"returnurl"->returnUrl,
			"resulturl"->resultUrl,
			"authemail"->authEmail.getOrElse(""),
			"status"-> status,
			"hash"-> hash
		) 
		(handler: Request=>Response)=> {
			handler(Request.POST(config.endPointURL, map))
		}
	}

	def hash = {
		val digest = Seq(config.integrationID, reference, amount, additionalInfo.getOrElse(""), returnUrl, authEmail.getOrElse(""), status, config.integrationKey).mkString("")
		java.security.MessageDigest.getInstance("SHA-512")
			.digest(digest.map{_.toByte}.toArray)
			.map{
				c=>{
					val t = "%x".format(c); 
					if(t.length == 1) s"0$t" else t
				}
			}.mkString.toUpperCase
	}
}

object  Transaction{
	def init(cfg: Config) = (amount: Double, reference: String, returnUrl: String, resultUrl: String) => create(cfg, amount, reference, returnUrl, resultUrl)
	private def create(cfg: Config, _amount: Double, _reference: String, _returnUrl: String, _resultUrl: String) = 
	new Transaction{
		val reference: String = _reference
		val amount: Double = _amount
		override val additionalInfo: Option[String] = None
		val returnUrl: String = _returnUrl
		val resultUrl: String = _resultUrl
		override val authEmail: Option[String] = None
		val config: Config = cfg

		def process = super._process{
			rqst => 
			rqst.response match {
				case s: Response.Success => s //TODO: Verify hash
				case e: Response.Error => e
			}
		}
	}

	def handler(cfg: Config)(url: String, map: Map[String, String]): Transaction = new Transaction{
		val reference: String = map("reference")
		val amount: Double = map("amount").toDouble
		override val additionalInfo: Option[String] = None
		val returnUrl: String = ""
		val resultUrl: String = url
		override val authEmail: Option[String] = None
		val config: Config = cfg
		val lhash : String = map("Hash")
		val paynowReference: String = map("paynowreference")
		val pollUrl = map("pollurl")

		def _process : Response = if (lhash == hash) {
			val _t = this
			new Response.Success(lhash){
				val transaction: Transaction = _t
			}
		} else Response.ForgedRequest

		def process = _process
	}

	def poll(cfg: Config)(statusURL: String) = {
		import scala.util.{Try, Success, Failure}
		val rqst = Request.POST(statusURL, Map())
		val raw = rqst.raw match{
			case s: Success[String] => s.get
			case f: Failure[String] => throw new Exception(s"Failed to poll $statusURL")
		}
		val map = (raw.split("&").map{_.split("=")match{case a=>(a(0)->java.net.URLDecoder.decode(a(1)))}}).toMap
		handler(cfg)(statusURL, map)
	}

	def handle_status_update(cfg: Config)(postData: Map[String, String]) = {
		handler(cfg)("status", postData)
	}


}
