########################################################
###########Automated content analysis with R ###########
########################################################

########################################################
#SENTIMENT ANALYSIS
########################################################

#As a first step, we set our working directory.
#This is a folder with all data files used in the R session or that will be generated during the R session. 

setwd("/Users/johanna/Desktop/Workshop text analysis")
#alternative using R-Studio: Session --> set working directory --> choose directory --> choose the folder

########################################################
#Then we install and load all relevant packages:

#As you learnd in the last workshops, install.packages() is used to download 
#(you only need to do this once on your computer) 
#and library() is used to make the functions from this package available for use 
#(required each session that you use the package).
########################################################

install.packages("readr")
install.packages("dplyr")
install.packages("reshape2")
install.packages("tidyr")
install.packages("stringr")
install.packages("tidytext")
install.packages("tidyverse")
install.packages("tidyselect")
install.packages("widyr")
install.packages("scales")
install.packages("ggraph")
install.packages("ggplot2")
install.packages("textdata")

library(readr) 
library(dplyr) 
library(reshape2) 
library(tidyr) 
library(stringr) 
library(tidytext) 
library(tidyverse)
library(tidyselect) 
library(widyr) 
library(scales)
library(ggraph)
library(ggplot2)
library(textdata)

#DATA

#We use a data set about Rotten Tomatoes movie reviews. To skip the preprocessing we use the csv file "preprocessed.csv". 

#Read data from a local file on your computer 
#replace your path!

#There are two frequently used commands for reading CSV files: read.csv reads files in which columns are 
#separated with commas; read.csv2 reads files in which columns are separated with semicolons. Otherwise 
#these functions have the same structure. 

##Read data
reviews <- read.csv("/Users/johanna/Desktop/Workshop text analysis/data/datasets/preprocessed.csv")

#Overview variables:
#review_content = original text 
#text = original text after cleaning -> use for further analysis


#Tokenization 
tidy_reviews <- unnest_tokens(reviews, word_token, text, token = "words", format = "text", to_lower = TRUE)

#Stopword removal

#We save the English-language stopword lists included in tidy in a data frame. 

stop_words_en <- stop_words
View(stop_words_en)

#We remove stopwords with the anti_join function (comparing tokens with entries in the stopword-list, removing matches)

tidy_reviews <- tidy_reviews %>%
  anti_join(stopwords_en, by = c("word_token" = "word"))

########################################################
#SENTIMENT ANALYSIS
########################################################

#The aim of sentiment analysis is to determine the polarity of a text (i.e., whether the emotions expressed in it are rather 
#positive or negative). This is often done by word lists and by counting terms that were previously assigned to the categories 
#positive or negative. It should be considered that sentiment analysis is a heuristic approach that 
#can lead to incorrect individual classifications. 

#bing dictionary by Bing Liu et al. (https://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html)

#A well-known and widely used sentiment dictionary for English-language texts 
#is the Sentiment Lexicon by Bing Liu, which consists of two word lists, one of positive terms and the other of negative terms.
#It is often used for social media sentiment analysis, not least because it also contains common misspellings. 
#In total, it contains nearly 6800 words. 

#load bing dictionary

bing <- tidytext::get_sentiments("bing")
table(bing$sentiment)

#AFINN dictionary by Finn Arup Nielsen (http://www2.imm.dtu.dk/pubdb/pubs/6010-full.html)

#The bing dictionary is a categorical dictionary that gives equal weight to all terms in a category. 
#For some applications, however, this is an oversimplifying assumption, since it is likely that certain terms are more strongly 
#associated with a construct than others; for example, the word "hate" is likely to express stronger negative affect than the 
#word "dislike." Weighted lexicons attempt to account for this by assigning each term a numerical value that expresses the 
#strength of its association with the construct. The AFINN dictionary includes 2477 words with annotations of sentiment-values 
#(from -5 (very negative) to 5 (very positive)). 

#load the AFINN dictionary
afinn <- tidytext::get_sentiments("afinn")
table(afinn$value)  

#nrc dictionary by Saif Mohammad and Peter Turney (http://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm)
#The NRC Emotion Lexicon is a list of 13875 English words and their associations with eight basic emotions 
#(anger, fear, anticipation, trust, surprise, sadness, joy, and disgust) and two sentiments (negative and positive). 
#The annotations were manually done by crowdsourcing.

#load the nrc dictionary
nrc <- tidytext::get_sentiments("nrc")
table(nrc$sentiment)

#ANNOTATION OF SENTIMENTS 

#We want to define the sentiment score with the afinn-dictionary.

#First, we need to join sentiment-tokens with sentiment scores.
tidy_sentiments <- tidy_reviews %>%
  inner_join(afinn, by = c("word_token" = "word"))

#calculate average sentiment score per review and name columns
sentiment_scores <- aggregate(
  list(tidy_sentiments$value),
  by=list(tidy_sentiments$review_id),
  FUN=mean
)

#rename the colums
names(sentiment_scores) <- c("review_id", "sentiment_score")

#merge new sentiment_score data frame with review dataframe

#Using the 'merge' function to combine the 'reviews' and 'sentiment_scores' dataframes based on the common 'review_id' column.
#The 'all.x = TRUE' argument ensures that all rows from the 'reviews' dataframe are included, even if there's no corresponding match in 'sentiment_scores'.
reviews <- merge(reviews, sentiment_scores, by = "review_id", all.x = TRUE)

#Replace any missing (NA) values in the 'sentiment_score' column with a default value of 0.
reviews$sentiment_score[is.na(reviews$sentiment_score)] <- 0

print(reviews$sentiment_score)
mean(reviews$sentiment_score)

#aggregated sentiments per movie:
#Identify most positive and negative evaluated movies 

#Defining the keyword 'Horror' to filter for horror movies.
horror_keyword <- 'Horror'

#Creating a tibble (data frame) containing the 'horror_keyword'.
select_keyword <- tibble(horror_keyword)

#Extracting the 'Horror' genre label from the 'genres' column in the 'tidy_reviews' dataframe.
tidy_reviews$horror <- str_extract(tidy_reviews$genres, paste(select_keyword))

#Replacing NA values in the 'horror' column with 'other'.
tidy_reviews["horror"][is.na(tidy_reviews["horror"])] <- "other"

#filter for horror movies
horror_reviews <- filter(tidy_reviews, horror == 'Horror')

#count word_token per movie
words_by_movie <- horror_reviews %>%
  count(movie_title, word_token, sort = TRUE) %>%
  ungroup()

#annotate and aggregate sentiment per movie
movie_sentiments <- words_by_movie %>%
  inner_join(afinn, by = c("word_token" = "word")) %>%
  group_by(movie_title) %>%
  #he summarize function calculates the weighted average sentiment score per movie. 
  #It does this by summing the product of sentiment values (value) and their corresponding 
  #occurrences (n) for each movie. The resulting sum is then divided by the total number of 
  #occurrences (sum(n)) to obtain the average sentiment score.
  summarize(value = sum(value * n) / sum(n))

#plot sentiments per movie 
movie_sentiments %>%
  mutate(movie_title = reorder(movie_title, value)) %>%
  ggplot(aes(value, movie_title, fill = value > 0)) +
  geom_col(show.legend = FALSE) +
  labs(x = "Average sentiment value", y = NULL)

#In a next step, we want to identify words that contribute most to the sentiment-score. 
#For this we use the bing dictionary.

#Joining the 'tidy_reviews' dataframe with sentiment labels from the 'bing' dictionary,
#based on matching 'word_token' and 'word'.
#Counting occurrences of each 'word_token' and sentiment combination, and sorting the results in descending order.
senti_words <- tidy_reviews %>%
  inner_join(bing, by=c("word_token" = "word")) %>%
  count(word_token, sentiment, sort = TRUE) %>%
  ungroup()

#group by positive and negative words and count & plot most frequent words per sentiment
senti_words %>%
  group_by(sentiment) %>% #Grouping the 'senti_words' dataframe by sentiment.
  top_n(20) %>% #Selecting the top 20 words with the highest occurrence within each sentiment category.
  ungroup() %>% #Ungrouping the data to prepare for plotting.
  mutate(word_token = reorder(word_token, n)) %>% #Reordering the 'word_token' based on the occurrence count (n).
  ggplot(aes(word_token, n, fill = sentiment)) + #Creating a grouped bar plot using ggplot.
  geom_col(show.legend = FALSE) + #Adding the bar columns to the plot, hiding the legend.
  facet_wrap(~sentiment, scales = "free_y") + #Creating separate facets for each sentiment category, allowing y scales to vary.
  labs(y = "Contribution to sentiment", #Adding labels to the plot.
       x = NULL) +
  coord_flip() # Flipping the coordinates to create a horizontal bar plot.

#In a next step, we analyze the word contribution to sentiment per genre by using the nrc dictionary. 
#For this we focus on movies of the "Adventure" genre. 

#Filtering the dataset to focus on movies of the "Adventure" genre.

#Defining the keyword 'Adventure' to filter for adventure genre movies.
adventure_keyword <- 'Adventure'

#Creating a tibble (data frame) containing the 'adventure_keyword'.
select_keyword <- tibble(adventure_keyword)

#Extracting the 'Adventure' genre label from the 'genres' column in the 'tidy_reviews' dataframe.
tidy_reviews$Type <- str_extract(tidy_reviews$genres, paste(select_keyword))

#Replacing NA values in the 'Type' column with 'other'.
tidy_reviews["Type"][is.na(tidy_reviews["Type"])] <- "other"

#Applying the nrc dictionary to identify sentiment-contributing words.
senti_words_nrc <- tidy_reviews %>%
  filter(Type == "Adventure") %>% #Filtering the 'tidy_reviews' dataset to include only movies of the "Adventure" genre.
  inner_join(nrc, by=c("word_token" = "word")) %>% #Joining the filtered dataset with sentiment labels from the 'nrc' dictionary, based on matching 'word_token' and 'word'.
  count(word_token, sentiment, sort = TRUE) %>% #Counting occurrences of each 'word_token' and sentiment combination, sorting the results in descending order.
  ungroup() #Ungrouping the data to prepare for plotting.

#Creating a grouped bar plot to visualize word contribution to sentiment.
senti_words_nrc %>%
  group_by(sentiment) %>% #Grouping the 'senti_words_nrc' dataframe by sentiment.
  top_n(15) %>% #Selecting the top 15 words with the highest occurrence within each sentiment category.
  ungroup() %>% #Ungrouping the data to prepare for plotting.
  mutate(word_token = reorder(word_token, n)) %>%  #Reordering the 'word_token' based on the occurrence count (n).
  ggplot(aes(word_token, n, fill = sentiment)) + #Creating a grouped bar plot using ggplot.
  geom_col(show.legend = FALSE) + #Adding the bar columns to the plot, hiding the legend.
  facet_wrap(~sentiment, scales = "free_y") +  #Creating separate facets for each sentiment category, allowing y scales to vary.
  labs(y = "Contribution to sentiment", #Adding labels to the plot.
       x = NULL) +
  coord_flip() #Flipping the coordinates to create a horizontal bar plot.

#Do sentiment scores correspond to movie ratings? 

#Because ratings are not standardized, we filter the ratings and just look at the 1-5-scale. 

#Creating a list of valid rating values in the 1-5 scale.
rating_list <- c("1/5", "2/5", "3/5", "4/5", "5/5")

#Subsetting the 'tidy_reviews' dataset to include only rows with review scores within the defined rating list.
tidy_ratings <- subset(tidy_reviews, tidy_reviews$review_score %in% rating_list)

#annotating sentiments, counting review_score and sentiments per genre
sentiment_rate <- tidy_ratings %>%
  inner_join(bing, by = c("word_token"="word")) %>% 
  count(Type, review_score, sentiment) %>% #Counting occurrences of sentiment categories per genre and review score combination.
  spread(sentiment, n, fill = 0) %>% #Spreading the data to create columns for each sentiment, filling missing values with 0.
  mutate(sentiment = positive - negative)  #Calculating the overall sentiment by subtracting the count of negative sentiments from positive sentiments.

#plotting sentiment scores per rating and genre
ggplot(sentiment_rate, aes(review_score, sentiment, fill = Type)) + #Mapping review scores to the x-axis, sentiment scores to the y-axis, and genre to fill colors.
  geom_col(show.legend = FALSE) + #Adding the grouped bar columns to the plot, hiding the legend.
  facet_wrap(~Type, ncol = 2, scales = "free_x") #Creating separate facets for each genre, arranging in 2 columns.


