/**
 *
 *
 *
 */
package zw.co.base2theory.paynow

class Paynow(val config: Config){
	private val t_launcher = Transaction.init(config)
	private val p_launcher = Transaction.poll(config) _
	private val h_launcher = Transaction.handle_status_update(config) _

	/**
	 * Initialise a transaction.
	 */
	def init(amount: Double, _reference: String, _returnUrl: String, _resultUrl: String) = t_launcher(amount, _reference, _returnUrl, _resultUrl)

	/**
	 * Poll for status update
	 */
	def poll(url: String) = p_launcher(url)

	/**
	 * Handle a status update
	 */
	def handle_update(postData: Map[String, String]) = h_launcher(postData)	

}

object Paynow{
	def apply(apiKey: String, apiSecret: String) = {
		new Paynow(Config.makeDefault(apiKey, apiSecret))
	}

}