package net.tqft.toolkit.amazon

object QueueService {
	def apply(amazonAccount: String, secretKey: String) = {
		new com.xerox.amazonws.sqs2.QueueService(amazonAccount, secretKey)
	}
}

object QueueServiceDefaultAccount {
	val amazonAccount = "0D4BTQXQJ7SAKKQHF982"
	val secretKey = "wQsXfibiPzfPFDZ84jWXIjNb9UfqnLh42+FHhqtp"

	def apply() = {
		QueueService(amazonAccount, secretKey)
	}
}
