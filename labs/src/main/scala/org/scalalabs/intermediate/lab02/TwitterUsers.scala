package org.scalalabs.intermediate.lab02


object TwitterUsers {
    def thatArePopular(users: List[TwitterUser]) = (users partition(_.followersCount >= 2000))._1
    def thatArePopularByScreenName(users: List[TwitterUser]) = thatArePopular(users) map (_.screen_name)
    def thatArePopularByScreenNameSortedbyPopularity(users: List[TwitterUser]) = thatArePopular(users) sortBy(_.followersCount) map (_.screen_name) reverse
    def thatArePopularByScreenNameAndPopularitySortedbyPopularity(users: List[TwitterUser]) = (thatArePopular(users) sortBy(_.followersCount) reverse) map(u => (u.screen_name, u.followersCount))
    def thatAreInBothLists(friends: List[TwitterUser], followers: List[TwitterUser]) = friends intersect followers
}