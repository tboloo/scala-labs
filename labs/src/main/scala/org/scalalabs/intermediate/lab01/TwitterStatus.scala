package org.scalalabs.intermediate.lab01

import scala.xml._
import java.util.Locale
import org.joda.time.format._

class TwitterStatus(s: Node) {
	def createdAt = DateTimeFormat.forPattern("EE MMM dd HH:mm:ss Z yyyy").withLocale(Locale.US).parseDateTime((s \ "created_at" text))
	def id = (s \ "id" text).toLong
	def text = s \ "text" text
	def source = s \ "source"
	def truncated = (s \ "truncated" text).toBoolean
	def inReplyToStatusId = try {
			Some((s \ "in_reply_to_status_id" text).toLong)
		} catch {
			case e: NumberFormatException => None
		}
	def inReplyToUserId = try {
		Some((s \ "in_reply_to_user_id" text).toLong)
		} catch {
			case e: NumberFormatException => None
		}
	def favorited = try {
		(s \ "favorited" text).toBoolean
		} catch {
			case e: NumberFormatException => false
		}
	def inReplyToScreenName = try {
		Some((s \ "in_reply_to_screen_name" text).toLong)
		} catch {
			case e: NumberFormatException => None
		}
	def user = TwitterUser( s \ "user")
}

object TwitterStatus {
	def apply(s: Node) = new TwitterStatus(s)
}