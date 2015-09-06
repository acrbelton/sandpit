# create randomm community data

# create a random community

create_random_community <- function() {

  # set seet to ensure reproducibilty
  
  set.seed(12345)
    
  community_size <- sample(12:20, 1)
  
  twitter_id <- sample(10:10000, community_size, replace = FALSE)
  
  segment <-
    sample (c("Politicians","Media", "Advisors", "3rd Parties"), community_size, replace = TRUE)
  
  party <-
    sample(c("Labour", "Conservative", "Liberal Democrat", "UKIP"), community_size, replace = TRUE)
  
  no_of_tweets <- sample (0:200, community_size)
  
  no_of_RTs <- sample(0:100, community_size)
  
  community_data <-
    data.frame(twitter_id, segment, party, no_of_tweets, no_of_RTs)
}


# generate randoms scored interactions between community members
# it must be true that twitter_id_A < twitter_id_B
# and that the pair ( twitter_id_A, twitter_id_B ) appears at most once

create_random_interactions <- function(community_data) {
  
  # set seet to ensure reproducibilty
  
  set.seed(12345)
  
  community_size <- nrow(community_data)
  
  interaction_matrix <-
    replicate(community_size, pmax(runif(community_size) - 0.5, rep(0, community_size)))
  
  twitter_id_A <-
    community_data$twitter_id[which(interaction_matrix != 0, arr.ind = TRUE)[,1]]
  
  twitter_id_B <-
    community_data$twitter_id[which(interaction_matrix != 0, arr.ind = TRUE)[,2]]
  
  interaction_data <- data.frame(twitter_id_A, twitter_id_B)
  
  interaction_data <-
    subset(interaction_data,  twitter_id_A < twitter_id_B)
  
  interaction_data$score <-
    sample(1:20, nrow(interaction_data), replace = TRUE)
  
  return(interaction_data)
}