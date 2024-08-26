## A script that goes through the Centrelink reddit to find COVID supplement related discussion
# Author: Matt Nolan
# Last edit: 20/08/2024

### Set up libraries ----
# 
# library(tidyverse)
# library(data.table)
# library(theme61)
# library(httr)
# library(jsonlite)
# library(lubridate)
# library(RedditExtractoR)
# library(fst)
# 
# rm(list=ls())

#### Use RedditExtractoR
# 
# dt <- find_thread_urls(subreddit =  "Centrelink",period="all")
# 
# #write_fst(dt,"Backup Reddit pull.fst")
# 
# dt2020 <- dt[grep("2020", dt$date), ]
# 
# min(dt2020$date)
# 
# setDT(dt2020)
# 
# dt2020[,month := month(date_utc)]
# 
# dt2020[,.N,by=.(month)][order(month)]
# 
# # 2023 test
# dt2023 <- dt[grep("2023", dt$date), ]
# 
# min(dt2023$date)
# 
# setDT(dt2023)
# 
# dt2023[,month := month(date_utc)]
# 
# dt2023[,.N,by=.(month)][order(month)]

### Do these for the different subreddits
# 
# subreddits <- c("Centrelink","CentrelinkOz","australian")
# 
# for (i in 1:length(subreddits)){
#   temp <- find_thread_urls(subreddit = subreddits[i],period="all",keywords = "COVID")
#   
#   print(min(temp$date)) # So this does go to an earlier date!
#   print(subreddits[i])
#   
#   temp <- temp[grep("2020", temp$date), ]
#   
#   setDT(temp)
#   
#   temp[,month := month(date_utc)]
#   
#   print(temp[,.N,by=.(month)][order(month)])
#   
#   assign(paste(subreddits[i],"dt",sep="_"),temp)
# }


#### Use Claude as this isn't giving most of the actual pages - requires we set up Reddit API details
# Set the subreddit and search term
subreddit <- "Centrelink"
search_term <- "COVID"

# Set your Reddit API credentials
client_id <- "your_client_id"
client_secret <- "your_client_secret"
user_agent <- "your_user_agent"

# Set the date range for 2020
start_date <- as.POSIXct("2020-01-01")
end_date <- as.POSIXct("2020-12-31")

# Authenticate with the Reddit API
token <- GET(
  "https://www.reddit.com/api/v1/access_token",
  authenticate(client_id, client_secret, type = "basic"),
  body = list(grant_type = "client_credentials"),
  add_headers(
    "User-Agent" = user_agent
  )
) %>%
  content("parsed")

# Search the subreddit for the specified term and date range
results <- GET(
  paste0("https://oauth.reddit.com/r/", subreddit, "/search"),
  add_headers(
    "User-Agent" = user_agent,
    "Authorization" = paste("Bearer", token$access_token)
  ),
  query = list(
    q = search_term,
    after = format(start_date, "%Y-%m-%dT%H:%M:%SZ"),
    before = format(end_date, "%Y-%m-%dT%H:%M:%SZ"),
    limit = 100,
    sort = "new"
  )
) %>%
  content("parsed") %>%
  pull(data) %>%
  select(title, url, created_utc = created_utc)

head(results)



###### None working raw attempt due to security

### Reddit access function
# 
# get_reddit_data <- function(subreddit, query, start_date, end_date) {
#   base_url <- "https://api.pushshift.io/reddit/search/submission/"
#   url <- paste0(base_url,
#                 "?q=", URLencode(query),
#                 "&subreddit=", subreddit,
#                 "&after=", as.integer(as.POSIXct(start_date)),
#                 "&before=", as.integer(as.POSIXct(end_date)),
#                 "&size=500")  # size=500 to get 500 results at a time
#   
#   response <- GET(url, add_headers(`User-Agent` = "R script"))
#   
#   if (response$status_code == 200) {
#     content <- content(response, "text", encoding = "UTF-8")
#     data <- fromJSON(content)
#     return(data$data)
#   } else if (response$status_code == 403) {
#     stop("Failed to retrieve data: 403 Forbidden. The server refused the request.")
#   } else {
#     stop("Failed to retrieve data: ", response$status_code)
#   }
# }
# 
# ### Query
# 
# # Query data with a delay to avoid rate limiting
# subreddit <- "Centrelink"
# query <- "supplement extension"
# 
# years <- 2018:2023
# results <- list()
# 
# for (year in years) {
#   start_date <- paste0(year, "-01-01")
#   end_date <- paste0(year, "-12-31")
#   
#   year_data <- tryCatch({
#     Sys.sleep(1)  # Adding a delay to avoid rate limiting
#     get_reddit_data(subreddit, query, start_date, end_date)
#   }, error = function(e) {
#     message("Error encountered: ", e)
#     return(NULL)
#   })
#   
#   if (!is.null(year_data)) {
#     results[[as.character(year)]] <- year_data
#   }
# }
# 
# # Combine the results into a single data frame if data was retrieved
# if (length(results) > 0) {
#   all_results <- do.call(rbind, lapply(results, function(x) do.call(rbind, x)))
#   
#   # Count the mentions
#   count_mentions <- function(data) {
#     return(nrow(data))
#   }
#   
#   mention_counts <- sapply(results, count_mentions)
#   
#   mention_counts_df <- data.frame(
#     year = names(mention_counts),
#     mentions = mention_counts
#   )
#   
#   print(mention_counts_df)
# } else {
#   message("No data was retrieved for the specified query and timeframe.")
# }
# 
# 
# 
# # Combine the results into a single data frame
# all_results <- do.call(rbind, lapply(results, function(x) do.call(rbind, x)))
# 
# ### Results
# 
# count_mentions <- function(data) {
#   return(nrow(data))
# }
# 
# mention_counts <- sapply(results, count_mentions)
# 
# mention_counts_df <- data.frame(
#   year = names(mention_counts),
#   mentions = mention_counts
# )
# 
# print(mention_counts_df)

