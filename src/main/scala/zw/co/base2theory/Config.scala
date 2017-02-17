package zw.co.base2theory.paynow


trait Config{
	val endPointURL: String
	val integrationID: String
	val integrationKey: String
}

object Config{
	def makeDefault(ik: String, is: String) = new Config{
		val endPointURL = "https://paynow.co.zw/interface/initiatetransaction"
		val integrationID = ik
		val integrationKey = is

	}
}