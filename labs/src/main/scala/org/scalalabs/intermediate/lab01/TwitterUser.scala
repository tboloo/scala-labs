package org.scalalabs.intermediate.lab01

import scala.xml._

class TwitterUser(s: NodeSeq) {
	def id = (s \ "id" text).toLong
	def name = s \ "name" text
	def screen_name = s \ "screen_name" text
	def description = s \ "description" text
	def location = s \ "location" text
	def url = s \ "url" text
	def profileImageUrl = s \ "profile_image_url" text
	def statusesCount = (s \ "statuses_count" text) toLong
	def friendsCount = (s \ "friends_count" text) toLong
	def followersCount = (s \ "followers_count" text) toLong
}

object TwitterUser {
	def apply(s: NodeSeq) = new TwitterUser(s)
}