# functions to call the database

library(RPostgreSQL)
source("globals.R")

# query function (taken for Will's amazeRs code)

pgQuery <- function(query){    
  
  # Open a connection to twibalist on twibalist_data
  # This assumes that you have an SSH tunnel open, with remote port 5432 forwarded to local port 5433
  
  con <- dbConnect(drv = dbDriver("PostgreSQL"),
                   host = '10.182.1.191',
                   port = '5432',
                   dbname = "twibalist",
                   user = '',            # your name here
                   password = '' )       # your password here
  
  rs <- fetch(dbSendQuery(con, query), n = -1)
  
  dbDisconnect(con)
  
  return(as.data.frame(rs))
}

# return twitter handle for twitter_id = x

getTwitterHandle <- function(x) {
  
  #handle
  #  <- pgQuery(paste("SELECT twitter_handle FROM twibalist_dashboards.person WHERE person_id = '", x, "'", sep = ""))
  handle <- paste("@", x, sep="")
  
}

# returns a dataframe with any new community interactions
# with columnds as follows
# active_id = id of person making the tweet
# passive_id = id of the person who the tweet concerns
#  - they have been RTed, or a tweet has been issued mentioning them
# is_RT = TRUE if an RT, false otherwise

newInteractions <- function(session) {
    
    # get some interactions from a queue on the db here
    
    ### code to generate random interactions follows
    
    if (sample(c("TRUE", "FALSE"), 1))
      return()
    
    cD <-  create_random_community()    # this is the same community, since the seed is set
    n <- sample(1:10, 1)
    active_id <- sample(cD$twitter_id, n, replace = TRUE)
    passive_id <- sample(cD$twitter_id, n, replace = TRUE)
    is_RT <- sample(c("TRUE", "FALSE"), n, replace = TRUE)
    ints <- data.frame(active_id, passive_id, is_RT)
    ints <- ints[!(ints$active_id == ints$passive_id),]    # remove self-interactions
  
}