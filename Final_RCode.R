#yoyoyoy
#Text Analysis and language processing
library(tm)
library(jsonlite)
library(quanteda)
library(textstem)
library(topicmodels)
library(stm)
library(dplyr)
library(stringr)
library(mlogit)
library(lubridate)
library(sylcount)
library(ngram)
library(brant)
library(MASS)


setwd("C:/Users/Dell/Desktop")
rm(list = ls())
setwd("~/NHH/Customer Analytics/Final")
#setwd("/cloud/project")
software_reviews=readRDS("software_reviews.RDS")

#data exploration
head(software_reviews)
summary(software_reviews)
class(software_reviews$overall)
class(software_reviews$verified)
class(software_reviews$reviewTime)
class(software_reviews$reviewerID)
class(software_reviews$asin)
class(software_reviews$reviewerName)
class(software_reviews$reviewText)
class(software_reviews$summary)
class(software_reviews$unixReviewTime)
class(software_reviews$vote)
class(software_reviews$HVote_Cat)
summary(as.numeric(software_reviews$overall))
summary(as.numeric(software_reviews$vote))
summary(as.numeric(software_reviews$HVote_Cat))
3497 - length(na.omit(software_reviews$vote))
length(boxplot.stats(software_reviews$vote)$out)
3497-length(na.omit(software_reviews$reviewText))
3497-length(na.omit(software_reviews$summary))
summary(software_reviews$image)
3497-length(na.omit(software_reviews$image))

#clean the data
software_reviews = subset(software_reviews, select = -c(6))
software_reviews = subset(software_reviews, select = -c(11))
software_reviews$reviewText <- tolower(software_reviews$reviewText)
my_stop_words <- c(stopwords(),"really","great","worst","good")
software_reviews$reviewText <- removeWords(software_reviews$reviewText, my_stop_words)
software_reviews$reviewText <- removePunctuation(software_reviews$reviewText)
software_reviews$reviewText <- gsub("[^[:alnum:]]"," ",software_reviews$reviewText)
software_reviews$reviewText <- gsub("[^a-zA-Z#]"," ",software_reviews$reviewText)
software_reviews$reviewText <- stripWhitespace(software_reviews$reviewText)
software_reviews$reviewText <- removeNumbers(software_reviews$reviewText)

#clean summary data
software_reviews$summary <- tolower(software_reviews$summary)
my_stop_words <- c(stopwords(),"really","great","worst","good")
software_reviews$summary <- removeWords(software_reviews$summary, my_stop_words)
software_reviews$summary <- removePunctuation(software_reviews$summary)
software_reviews$summary <- gsub("[^[:alnum:]]"," ",software_reviews$summary)
software_reviews$summary <- gsub("[^a-zA-Z#]"," ",software_reviews$summary)
software_reviews$summary <- stripWhitespace(software_reviews$summary)
software_reviews$summary <- removeNumbers(software_reviews$summary)

#FREQUENCY BAR CHART (Review Text)
words_combined <- paste(software_reviews$reviewText)
words_combined <- unlist(strsplit(words_combined,split = " "))
words_combined <- words_combined[words_combined != ""]
freqTab <- as.data.frame(table(words_combined))
freqTab <- freqTab[order(freqTab$Freq, decreasing = T),]
topfreqtab <- freqTab[1:15,]
barplot(height = topfreqtab$Freq, # the height of the bars
        horiz = F, # FALSE if draw the bar vertically
        col = "lightblue", # color of the bars
        names.arg = topfreqtab$words_combined, # bar labels
        las=2,
        main = "ReviewText Word Frequency Bar Chart") # title of the plo

#FREQUENCY BAR CHART (summary)
words_combined2 <- paste(software_reviews$summary)
words_combined2 <- unlist(strsplit(words_combined,split = " "))
words_combined2 <- words_combined[words_combined != ""]
freqTab2 <- as.data.frame(table(words_combined))
freqTab2 <- freqTab[order(freqTab$Freq, decreasing = T),]
topfreqtab2 <- freqTab[1:15,]
barplot(height = topfreqtab2$Freq, # the height of the bars
        horiz = F, # FALSE if draw the bar vertically
        col = "lightblue", # color of the bars,
        names.arg = topfreqtab2$words_combined, # bar labels
        las=2,
        main = "Summary Word Frequency Bar Chart") # title of the plo

#tokenization
toks_review <- quanteda::tokens(software_reviews$reviewText,what = c("word"))
head(toks_review,3)
#stemming and lemmatization
reviews_stem <- lapply(X = toks_review,
                       FUN = stem_words)
reviews_lem <- lapply(X = toks_review,
                      FUN = lemmatize_words)
software_reviews$text_clean <- sapply(reviews_stem,
                                      FUN = paste,
                                      collapse = " ")
#summary tokenization
toks_review2 <- quanteda::tokens(software_reviews$summary,what = c("word"))
head(toks_review,3)
#stemming and lemmatization
reviews_stem2 <- lapply(X = toks_review2,
                        FUN = stem_words)
reviews_lem2 <- lapply(X = toks_review2,
                       FUN = lemmatize_words)
software_reviews$summary_clean <- sapply(reviews_stem2,
                                         FUN = paste,
                                         collapse = " ")
#word frequency chart
words_combined <- paste(software_reviews$summary_clean)
# split the sentences into words
words_combined <- unlist(strsplit(words_combined, split = " "))
# remove the empty character
words_combined <- words_combined[words_combined != ""]
#------------------------------------------
# Step 2: Count the times each word appears
# create a frequency table
freqTab <- as.data.frame(table(words_combined))
# sort the table
freqTab <- freqTab[order(freqTab$Freq, decreasing = T), ]
#-------------------------
# Step 3: Plot a bar chart
# select the top 20 mist frequent words
topFreqTab <- freqTab[1:20, ]
# create a bar chart
barplot(height = topFreqTab$Freq, # the height of the bars
        horiz = F, # FALSE if draw the bar vertically
        col = "lightblue", # color of the bars
        las=2,
        names.arg = topFreqTab$words_combined, # bar labels
        main = "Summary Word Frequency Bar Chart") # title of the plot


#NEW VARIABLES 
#Readability indices 
#binding raw data from original file
software=readRDS("software_reviews.RDS")
software_reviews=cbind(software_reviews, software$reviewText)
colnames(software_reviews)[14] <- "raw"
#calculate indices
readability=readability(software_reviews$raw, nthreads = sylcount.nthreads())
software_reviews=cbind(software_reviews,readability$ari,readability$cl,readability$smog)
colnames(software_reviews)[15] <- "ari"
colnames(software_reviews)[16] <- "cl"
colnames(software_reviews)[17] <- "smog"

software_reviews$ID=1:nrow(software_reviews)
software_reviews[is.na(software_reviews)] <- 0

software_reviews$vote <- as.numeric(software_reviews$vote)
software_reviews$overall <- as.numeric(software_reviews$overall)



#sentiment score
text_sent <- tokens_lookup(as.tokens(reviews_lem),dictionary = data_dictionary_LSD2015[1:2])
head(text_sent)
software_reviews$sent_score <- sapply(X = text_sent,
                                      FUN = function(x)(length(x[x == "positive"]) -
                                                          length(x[x == "negative"]))/length(x))
summary(software_reviews$sent_score)

#summary sent
#sentiment score
text_sent2 <- tokens_lookup(as.tokens(reviews_lem2),dictionary = data_dictionary_LSD2015[1:2])

software_reviews$sent_score2 <- sapply(X = text_sent2,
                                       FUN = function(x)(length(x[x == "positive"]) -
                                                           length(x[x == "negative"]))/length(x))
summary(software_reviews$sent_score2)

software_reviews$sent_score <- as.numeric(software_reviews$sent_score)

#topic modeling
review_dfm <- dfm(as.tokens(reviews_stem))
review_dfm_trimmed <- dfm_trim(x = review_dfm,
                               min_docfreq = 0.075,
                               max_docfreq = 0.90,
                               docfreq_type = "prop")
#LDA model
num_words <- apply(X=review_dfm_trimmed,
                   MARGIN = 1,
                   FUN = sum)
review_dfm_trimmed_new <- review_dfm_trimmed[num_words > 0,]
k <- 2
lda_results <- LDA(review_dfm_trimmed_new,
                   k = k,
                   control = list(seed = 123))
terms(lda_results, 7)



software_reviews[is.na(software_reviews)] <- 0



#Multinomial model
software_mlogit1 <- mlogit(HVote_Cat~0 | overall+sent_score+cl+vote, # formula
                         data = software_reviews, # the estimation data set
                         choice = "HVote_Cat", # the customer's choice
                         shape = "wide", # the format of the data
                         reflevel = "1") # the reference category


summary(software_mlogit1)

###Ordered Logit
software_ordlogit <- polr(as.factor(HVote_Cat)~ overall+sent_score+cl+vote,# formula
                          data = software_reviews, # the estimation data set
                          method = "logistic") # ordered logit (not probit)


LL1 <- as.numeric(logLik(software_ordlogit))
brant(software_ordlogit)


#####DATA VALIDATION
#Alternative models with removing variables have also been tested in lines 221-280
cust_list <- unique(software_reviews$ID)
set.seed(123)
indices <- sample(1:2, # the group index (1 vs. 2)
                  length(cust_list), # for how many customers do we need the index
                  replace = T, # reusing the two groups for all customers
                  prob = c(0.7, 0.3)) # probabilities of being in groups 1 and 2

cust_list_estimation <- cust_list[indices == 1]
cust_list_validation <- cust_list[indices == 2]
# 1) ESTIMATION DATA
software_estimation <- software_reviews[software_reviews$ID %in% cust_list_estimation, ]
# number of customers in the estimation sample
length(unique(software_estimation$ID))

# 2) VALIDATION DATA
software_validation <- software_reviews[software_reviews$ID %in% cust_list_validation, ]
length(unique(software_validation$ID))

software_train <- mlogit(HVote_Cat~0 | overall+sent_score+vote+cl, # formula
                        data = software_estimation, # the estimation data set
                        choice = "HVote_Cat", # the customer's choice
                        shape = "wide", # the format of the data
                        reflevel = "1") # the reference category

summary(software_train)

software_valid_dat <- mlogit.data(software_validation,
                                 shape = "wide",
                                 choice = "HVote_Cat")



software_validation[, c("pred_prob_1",
                       "pred_prob_2",
                       "pred_prob_3",
                       "pred_prob_4",
                       "pred_prob_5")] <- predict(software_train,
                                                   newdata = software_valid_dat,
                                                   type = "response")

head(software_validation[, c(21:25)], 6)

paste0("pred_prob", c(1, 2, 3, 4, 5))

which.max(software_validation[1, c(21:25)])
weight_list <- c(1, 2, 3, 4, 5)
weight_list[which.max(software_validation[1, c(21:25)])]

software_validation$pred_choice <- apply(software_validation[, c(21:25)],
                                        MARGIN = 1,
                                        FUN = function(x) weight_list[which.max(x)])


head(software_validation[, 21:25])
class_tab <- table(software_validation$pred_choice, software_validation$HVote_Cat)
class_tab
hitRate <- mean(software_validation$pred_choice == software_validation$HVote_Cat)
summary(software_train)
hitRate
AIC(software_train)

#linear
linear <- lm(HVote_Cat~cl+overall+sent_score,data = software_estimation)
summary(linear)

software_validation[, c("pred_prob_12",
                       "pred_prob_22",
                       "pred_prob_32",
                       "pred_prob_42",
                       "pred_prob_52")] <- predict(linear,
                                                   newdata = software_validation,
                                                   type = "response")
round(software_validation[, c("pred_prob_12",
                             "pred_prob_22",
                             "pred_prob_32",
                             "pred_prob_42",
                             "pred_prob_52")])

head(software_validation[, c(23:27)], 6)


which.max(software_validation[1, c(23:27)])
weight_list <- c(1, 2, 3, 4, 5)
weight_list[which.max(software_validation[1, c(23:27)])]

software_validation$pred_choice <- apply(software_validation[, c(23:27)],
                                        MARGIN = 1,
                                        FUN = function(x) weight_list[which.max(x)])


head(software_validation[, 23:27])
class_tab <- table(software_validation$pred_choice, software_validation$HVote_Cat)
class_tab
hitRate <- mean(software_validation$pred_choice == software_validation$HVote_Cat)
hitRate

#Final model
software_mlogit <- mlogit(HVote_Cat~0 | cl + sent_score+overall, # formula
                         data = software_reviews, # the estimation data set
                         choice = "HVote_Cat", # the customer's choice
                         shape = "wide", # the format of the data
                         reflevel = "1") # the reference category

summary(software_mlogit)
(exp(coef(software_mlogit1))-1)*100
options(scipen=999)



#SOCIAL NETWORK ANALYSIs
setwd("C:/Users/Dell/Desktop")
rm(list = ls())
load("Facebook_data.Rdata")

library(igraph)
library(lessR)
library(dplyr)

edges$Source=as.factor(edges$Source)
edges$Target =as.factor(edges$Target)
edges_list <- as.matrix(edges)
facebook_network <- graph_from_edgelist(el = edges_list,
                                        directed = F)
facebook_network
colnames(nodes)
colnames(edges)

#Descriptive statistics/Visualization
Gender <- factor(c(rep("Female", 175),
                   rep("Male", 158)))
cat <- data.frame(Gender)
cols <-  hcl.colors(length(levels(Gender)), "Peach")
PieChart(Gender, data = cat, hole = 0,
         fill = cols,
         labels_cex = 0.6)

education=BarChart(Education, data=nodes, sort = "-", fill="GrandBudapest1")

summary(nodes$Age)


#Compute centrality measures 
#DEGREE CENTRALITY
V(facebook_network)$degree <- degree(facebook_network)
head(sort(V(facebook_network)$degree, decreasing = T),n=20)
#Printing the nodes with the highest degree centrality scores
degree <- degree(facebook_network)
degree
degree.df <- as.data.frame(degree)
degree.df$nodes=row.names(degree.df)
degree.df <- degree.df[order(-degree.df$degree), ]
head(degree.df$nodes, n=20)
#"56"  "67"  "271" "322" "25"  "26"  "252" "277" "21"  "122" "119" "239" "9"   "200" "203" "315" "304" "98"  "188" "285"

#the node with the highest degree centrality 
V(facebook_network)$name[V(facebook_network)$degree == max(V(facebook_network)$degree)]
#56
nodes %>% filter(Name=="56")

#BETWEENESS CENTRALITY
V(facebook_network)$between <- betweenness(facebook_network,
                                           normalized = T)
round(V(facebook_network)$between,2)
#Printing the nodes with the highest degree centrality scores
between <- betweenness(facebook_network)
between
between.df <- as.data.frame(between)
between.df$nodes=row.names(between.df)
between.df <- between.df[order(-between.df$between), ]
head(between.df$nodes, n=20)
# [1] "277" "175" "19"  "23"  "25"  "119" "339" "40"  "152" "230" "115" "322" "41"  "307" "245" "21"  "315" "329" "170" "192"
#the node with the highest betweeness centrality 
V(facebook_network)$name[V(facebook_network)$between == max(V(facebook_network)$between)]
#"277"
library(dplyr)
nodes %>% filter(Name=="277")

#CLOSENESS CENTRALITY
V(facebook_network)$close <- closeness(facebook_network,
                                       normalized = T)
round(V(facebook_network)$close,2)

#Printing the nodes with the highest degree centrality scores
closeness <- closeness(facebook_network)
closeness
closeness.df <- as.data.frame(closeness)
closeness.df$nodes=row.names(closeness.df)
closeness.df <- closeness.df[order(-closeness.df$closeness), ]
head(closeness.df$nodes, n=20)
#"277" "25"  "322" "67"  "119" "56"  "271" "315" "21"  "26"  "40"  "170" "122" "203" "252" "332" "239" "200" "304" "9" 

#the node with the highest closeness centrality 
V(facebook_network)$name[V(facebook_network)$close == max(V(facebook_network)$close)]
#277

#EIGENVECTOR CENTRALITY
eigen_scores <- eigen_centrality(facebook_network,
                                 directed = F,
                                 scale = F)
eigen_scores <- eigen_scores$vector
V(facebook_network)$eigen <- eigen_scores
round(V(facebook_network)$eigen,2)
#Printing the nodes with the highest degree centrality scores
eigen.df <- eigen_centrality(facebook_network)
eigen.df
eigen.df <- as.data.frame(eigen.df)
eigen.df$nodes=row.names(eigen.df)
eigen.df <- eigen.df[order(-eigen.df$vector), ]
head(eigen.df$nodes, n=20)
#"56"  "67"  "271" "26"  "252" "122" "239" "277" "21"  "322" "200" "203" "25"  "9"   "98""304" "119" "188" "170" "199"
#the node with the highest Eigenvector centrality 
V(facebook_network)$name[V(facebook_network)$eigen == max(V(facebook_network)$eigen)]
#56


#PICK A NODE AND DRAW A NETWORK
group_number <- 10 # Fill in your group number here
set.seed(10)
chosen_node <- sample(1:5, 1) # This is your chosen node
# print your chosen node to the console
chosen_node #3

#Creating subgraph of graph with node 3 and its connections
edges$Source=as.factor(edges$Source)
edges$Target =as.factor(edges$Target)
edges_3=edges %>% filter(Source=="3"|Target=="3")
nodes_3=nodes %>% filter(Name==3|Name==9|Name==25|Name==26|Name==67|Name==72|Name==85|Name==122|Name==142|Name==170|Name==188|Name==200|Name==228|Name==274|Name==280|Name==283|Name==323)
network <- graph_from_data_frame(d = edges_3,
                                 vertices=nodes_3,
                                 directed = F)


#Visualizing node 3 and its connections 
plot(network,
     vertex.size = 20, # size of the node
     vertex.frame.color = "yellow", # color of the node frame,
     #vertex.label = NA, # remove the labels
     vertex.label.cex = 1, # label size
     vertex.label.color = "black", # label color
     edge.curved = .1, # how curvy the edge is (should not be too large)
     edge.arrow.size = .3, # arrow size
     edge.width = .7) # width of the edge


# plot the network color coding the gender and use betweenness 
V(network)$color <- ifelse(V(network)$Gender == "Female",
                           "darkred", # color for females
                           "darkgrey") # color for male


# plot the network
plot(network,
     vertex.size = V(network)$between, # size of the node
     vertex.frame.color = NA, # color of the node frame,
     #vertex.label = NA, # remove the labels
     vertex.label.cex = 1, # label size
     vertex.label.color = "white", # label color
     edge.curved = .1, # how curvy the edge is (should not be too large)
     edge.arrow.size = .3, # arrow size
     edge.width = .7) # width of the edge

