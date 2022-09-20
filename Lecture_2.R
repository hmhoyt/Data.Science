install.packages("purrr")
library(purrr)
library(tidyverse)
#Toy Data

df <- tibble(
  a = rnorm(10),
  b = rnorm(10),
  c = rnorm(10),
  d = rnorm(10),
  e = rnorm(10)
)

#column summaries
mean(df$a)
mean(df$b)
mean(df$c)
mean(df$d)
mean(df$e)

class(df$a)

for(i in 1:ncol(df)){
  print(mean(df[[i]]))
}

#function seq_along creates index of columns
seq_along(df)
column_summary <- function(df, fun){
out <- vector("double", length(df))
for(i in seq_along(df)){
  out[i] <- 
  fun(df[[i]])
}
out
}

column_summary(df,mean)

#map functions ----
map_dbl(df,mean)

map_dbl(df, mean, trim = .1)

#mean(x)/ sd(x)

#Solution 1
std_mean <- 
  function(x){
    mean(x)/sd(x)
  }
#Solution 2
map_dbl(df,std_mean)
map_dbl(df, ~ mean(.)/sd(.))

# map and  regressions ----
mtcars %>%
  split(.$cyl)%>%
  map(~lm(mpg~wt, data = .))%>%
  map(summary()) %>%
  map_dbl(~.$r.squared) %>%
  
#dealing with errors ----

x <- list(1, 10, "a")

y <- map(x, safely(log))
map(x, log)

#iterations over multiple lists ----
mu <- list(-10000, 0, 10000)
sd <- list(1,5,10)
n <-  list(1,100,25)

list(mean = mu,
     sd = sd,
     n = n) %>%
  pmap(rnorm)

map2(mu,sd,rnorm, n=5)

library(httr)
library(jsonlite)
library(ggplot2)
library(tidyverse)
library(magrittr)
library(rlang)
library(lubridate)
library(anytime)
# Today we are going to work with some more advanced topics in 
# terms of data handling and processing. We will play with an API
# from Vegvesenet. Vegvesenet has an API we can query for data
# on traffic volumes at many sensor stations in Norway. 
# 
# The API uses graphQL for requests. This is a relatively
# new language we might see more of in the future?
# 
# Let's define a function where we can submit queries to an external API. 
GQL <- function(query,
                ...,
                .token = NULL,
                .variables = NULL,
                .operationName = NULL,
                .url = url) {
  pbody <-
    list(query = query,
         variables = .variables,
         operationName = .operationName)
  if (is.null(.token)) {
    res <- POST(.url, body = pbody, encode = "json", ...)
  } else {
    auth_header <- paste("bearer", .token)
    res <-
      POST(
        .url,
        body = pbody,
        encode = "json",
        add_headers(Authorization = auth_header),
        ...
      )
  }
  res <- content(res, as = "parsed", encoding = "UTF-8")
  if (!is.null(res$errors)) {
    warning(toJSON(res$errors))
  }
  res$data
}


# The URL we will use is stored below: 
url <- "https://www.vegvesen.no/trafikkdata/api/"


# Let's figure out which sensor stations that are operable. 
# The query below extracts all the stations, with a date for 
# when the station was in operation as well as a long/latitude. 
qry <-
  '
{
    trafficRegistrationPoints {
        id
        name
        latestData {
            volumeByDay
        }
        location {
            coordinates {
                latLon {
                    lat
                    lon
                }
            }
        }
    }
}
'

# Allright - let's try submitting the query: 
stations <-GQL(qry) 


# We now have the a long list in memory - 11mb! - with just 
# a little information on each station. We can note that this 
# is a list, not a dataframe. For our purposes, it would be better if
# the list was instead a data frame, with one row pr. sensor station. 


# Note that the list only has one entry..   
length(stations)

#transforming to a dataframe ----

t <- stations[[1]] %>%
  map_dfr(as_tibble) %>%
  mutate(
    latestData =
      map_chr(latestData, 1, .default = NA_character_),
    latestData = 
      as_datetime(latestData, tz = "Europe/Berlin")
  )%>%
  mutate(
    lat = map_dbl(location, ~ .$latLon$lat),
    long = map_dbl(location, ~ .$latLon$lon) 
  )

#unnest
t <- stations[[1]] %>%
  map_dfr(as_tibble) %>%
  mutate(
    latestData =
      map_chr(latestData, 1, .default = NA_character_),
    latestData = 
      as_datetime(latestData, tz = "Europe/Berlin")
  )%>%
 unnest_wider(location) %>%
 unnest_wider(latLon)


