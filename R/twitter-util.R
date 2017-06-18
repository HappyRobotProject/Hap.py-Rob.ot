#
# Twitter Helper Functions
#

#Simple function to allow another script to check if this is loaded
twitter-util-version <- function(){
  return("0.0.1")
}


# Extract authentication keys from a csv file
twkeys <- function(){
  auth <-read.csv(file = "~/data/auth/twitter-HRP.csv", header = TRUE, stringsAsFactors = FALSE)
  ck <- auth$consumer.key
  cs <- auth$consumer.secret
  at <- auth$access.token
  as <- auth$access.secret
  return (list(consumer_key=ck,consumer_secret=cs,access_token=at,access_secret=as))
}

