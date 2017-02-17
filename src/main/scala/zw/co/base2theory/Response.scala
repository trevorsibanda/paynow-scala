package zw.co.base2theory.paynow

trait Response{
	val Status: String
}

object Response{
	case class Field[T](key: String, value: T)
	
	case class Success(val hash: String) extends Response{
		val Status = "Ok"

		val pollURL = ""
	}
	case class Error(val error: String)	extends Response{
		val Status = "Error"
	}
	case object ForgedRequest extends Response{
		val Status = "Error"
	}

	def parse(raw: String) : Response = {
		val map = (raw.split("&").map{_.split("=")match{case a=>(a(0)->java.net.URLDecoder.decode(a(1)))}}).toMap
		map.get("Status") match{
			case Some("Ok") => new Success(map("Hash")){
				val browserURL = map("BrowserUrl")
				override val pollURL = map("PollUrl")

				override def toString = s"SUCCESS: $browserURL | $pollURL | $hash"
			}
			case Some("Error")=> new Error(map("Error")){
				override def toString = s"ERROR: ${error}"
			}
			case None => new Error("Unknown Response")
		}
	}

	def apply(s: String) : Response= {
		parse(s)
	}

}