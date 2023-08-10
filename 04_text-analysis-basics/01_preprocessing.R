########################################################
###########Automated content analysis with R ###########
########################################################


########################################################
#PRERPOCESSING
########################################################

#As a first step, we set our working directory.
#This is a folder with all data files used in the R session or that will be generated during the R session. 

setwd("/Users/johanna/Desktop/Workshop text analysis")
# alternative using R-Studio: Session --> set working directory --> choose directory --> choose the folder

########################################################
#Then we install and load all relevant packages:

#As you learnd in the last workshops, install.packages() is used to download 
#(you only need to do this once on your computer) 
#and library() is used to make the functions from this package available for use 
#(required each session that you use the package).
########################################################

#Read data
install.packages("readr")
library(readr) ##fast reading of data sets, encoding works sometimes better than in the R base package (e.g. for German language texts)

#Base statistics
install.packages("pastecs")
library(pastecs) ## basic statistic calculations

#Data management, Preprocessing
install.packages("dplyr")
library(dplyr) ## Package to organize data based on the tidy logic
install.packages("reshape2")
library(reshape2) ## converts data frames to a matrix, needed e.g. to compute comparison clouds
install.packages("tidyr")
library(tidyr) ## follow-up of reshape2 
install.packages("ellipsis")
library(ellipsis)
install.packages("stringr")
library(stringr) ## Package to handle strings, e.g. to edit individual characters within strings
install.packages("SnowballC")
library(SnowballC) ## Stemmer algorithms in various languages
install.packages("plyr")
library(plyr) ## to efficiently work with a set of operations
install.packages("data.table")
library(data.table) ## efficiently managing data frames

#Packages from the tidy universe  
install.packages("tidytext")
library(tidytext) ## tokenization, other text-mining tasks like sentiment analysis, frequency counting etc.
install.packages("tidyverse")
library(tidyverse) ## package that bundles different functions and packages from the tidy universe, e.g. tibble, purrr
install.packages("tidyselect")
library(tidyselect) ## addition to tidyverse (useful for handling of strings)
install.packages("widyr")
library(widyr) ## convert tidy-format to matrix and vice versa (e.g. for co-occurence analysis)

#Visiulization
install.packages("scales")
library(scales)
install.packages("igraph")
library(igraph)
install.packages("ggraph")
library(ggraph)
install.packages("ggplot2")
library(ggplot2)
install.packages("RcppArmadillo")
library(RcppArmadillo)
install.packages("wordcloud")
library(wordcloud)

##OPTIONAL##
##other textmining packages 
install.packages("tm")
library(tm) ##Documentation: https://cran.r-project.org/web/packages/tm/vignettes/tm.pdf 
install.packages("quanteda")
library(quanteda) ##Documentation: https://github.com/quanteda 

### Warning: 
### Sometimes functions from different packages are conflicting if they share the same name,
### R-packages are continously updated, if some packages in this script do not function correctly at a later point in time
### it could be the reason that a function is existing in several packages, you can check this with help("name_of_function"),
### such an issue can be solved by detaching the function that is currently not used

########################################################

#DATA

#To illustrate how to preprocess text data in R, we will use a data set about Rotten Tomatoes movie reviews. 

#Read data from a local file on your computer 
#replace your path!

#There are two frequently used commands for reading CSV files: read.csv reads files in which columns are 
#separated with commas; read.csv2 reads files in which columns are separated with semicolons. Otherwise 
#these functions have the same structure. 

reviews <- read.csv("/Users/johanna/Desktop/Workshop text analysis/data/datasets/rotten_tomatoes_meta.csv")

#OVERVIEW OF THE DATA SET

#We take a quick look at the data to understand the structure of the dataset 
#and make sure that everything has been read in correctly.

View(reviews)

#The glimpse() function gives an overview of the information contained in a dataset. 
#We can see the number of observations (rows), and the number of variables, (columns). 
#We also see the name of each variable and its type. 

glimpse(reviews)

#The head() and tail() command allow us the view the first, or last, rows of a dataset.

head(reviews)

tail(reviews)

#frequencies
table(reviews$publisher_name)

table(reviews$top_critic)

table(reviews$review_type)

#PREPROCESSING

#We copy the variable containing the full texts for further analysis to also keep the original column for later detail analysis/rechecking.

reviews$text <- reviews$review_content

#In a next step, we remove boilerplate like html-markup, mail addresses, punctuation, numbers etc. To do this, we use regular expressions and the
#functions gsub() or str_replace_all(), (which search the strings for occurances of the regular expressions and
#replace them by blank spaces).

#combining words separated by apostrophes (like "it's" and "don't" etc.)
reviews$text  <- gsub("(\\w)['´’]([trs]\\w*)", "\\1\\2", reviews$text, ignore.case=TRUE)

#combining words separated by hyphens
reviews$text  <- gsub("([a-zA-ZöÖüÜäÄß0-9_])-([a-zA-ZöÖüÜäÄß0-9_])", "\\1\\2", reviews$text, ignore.case = TRUE)

#removing URLs (with different toplevel domains)
reviews$text <- gsub("https?://.*?(\\s|$)", " ", reviews$text, ignore.case = TRUE)
reviews$text <- gsub("(www.)?\\w+\\.de", " ", reviews$text, ignore.case = TRUE)
reviews$text <- gsub("(www.)?\\w+\\.com", " ", reviews$text, ignore.case = TRUE)
reviews$text <- gsub("(www.)?\\w+\\.org", " ", reviews$text, ignore.case = TRUE)

#removing @-mentions and hashtags
reviews$text <- gsub("@\\S*", " ", reviews$text, ignore.case = TRUE)
reviews$text <- gsub("#\\S*", " ", reviews$text, ignore.case = TRUE)

#removing punctuation
reviews$text <- gsub("[[:punct:]]+", " ", reviews$text, ignore.case = TRUE)

#removing numbers
reviews$text  <- gsub("[[:digit:]]+", " ", reviews$text, ignore.case = TRUE)

#Now we can compare both columns and see what happened! :)

#Tokenization: the tidy_format

#Tokens define semantic elements of strings and are important for many applications of text analysis.
#They are redominantly separated by white space and punctuation marks.
#Converting text to tokens raises several questions:
# - what unit of analysis? words? sentences?
# - which rules (algorithms) should be used to define tokens?
# - which tokens are not useful for the analysis?

#From tokens to “bag of words” (bow)
#Disassembling texts into tokens is the foundation for the bag of words model (bow).
#bow is a very simplified representation of text where only token frequencies are considered.
#advantages: easy to implement, scales well, often good enough
#disadvantages: discards language properties like polisemy and word order

#With the following lines of code, we choose words as tokens. 
#If you want other units of analysis to act as tokens
#you can instead use token = "sentences", "lines" or "paragraphs"

#The following code takes a dataset 'reviews' and processes the 'text' column to tokenize the words.
#It creates a new dataset 'tidy_reviews'.

tidy_reviews <- unnest_tokens(
  reviews,          #Input dataset
  word_token,       #Name for the new column containing tokens
  text,             #Column to tokenize
  token = "words",  #Tokenization method: split into words
  format = "text",  #Input data format: text
  to_lower = TRUE   #Convert text to lowercase
)

View(tidy_reviews)

#most frequent words

tidy_reviews %>%
  count(word_token, sort = TRUE) #sort -> argument ensures the results are sorted in descending order based on word frequencies

#removing stopwords
#Stopwords are words that are not very informative for detecting similarities and differences between texts. 
#tidy features a built-in (English language) stopwords list - that's great! :-) 
#If you want to remove stopwords in a different language, you have to use an external dictionary (some can be found on the web, 
#or create your own list). You can also use the quanteda package, it contains a number of lists of stopwords in different languages.

#We save the English-language stopword lists included in tidy in a data frame. 
stop_words_en <- stop_words
View(stop_words_en)

#We remove stopwords with the anti_join function (comparing tokens with entries in the stopword-list, removing matches)
tidy_reviews <- tidy_reviews %>%
  anti_join(stop_words_en, by = c("word_token" = "word"))

#counting word frequencies again
tidy_reviews %>%
  count(word_token, sort = TRUE)

#We can also add additional stopwords to the existing list. We can export and import the list using the csv-format
#or we add (or remove) values in the data frame in R. 
add <- data.frame(NA, 'own')

#define the columns names
names(add) <- c("word", "lexicon")

#combining two data frames row-wise
stop_words_en_added <- rbind(stop_words_en, add)

#combining words separated by apostroph also in the stop word list
stop_words_en_added$word  <- gsub("(\\w)['´’]([trs]\\w*)", "\\1\\2", stop_words_en_added$word, ignore.case=TRUE)

#Still this doesn't help us to catch everything! 
#Some things (like frequent typos, mistakes of the stopword dictionaries,still have to be kicked out manually
#by adapting the stopword dictionary
add2 <- data.frame(c("ll", "ve"), c("own", "own"))
names(add2) <- c("word", "lexicon")
stop_words_en_added <- rbind(stop_words_en_added, add2)

View(stop_words_en_added)

#save the stopword list
write.csv(stop_words_en_added, 'data/datasets/stopwords_en.csv')

#We use again the anti-join function with our updated stopword list.
tidy_reviews <- tidy_reviews %>%
  anti_join(stop_words_en_added, by = c("word_token" = "word"))


#visualization of the most frequent words

#The following code generates a visualization of the most frequent words in the 'tidy_reviews' dataset.
#It counts the occurrences of each unique word, filters out words with a count less than 10,000,
#reorders words based on frequency, and creates a horizontal bar chart.

tidy_reviews %>%
  count(word_token, sort = TRUE) %>%
  filter(n > 10000) %>%
  mutate(word = reorder(word_token, n)) %>%
  ggplot(aes(word, n)) +              #Initialize a ggplot object with 'word' on the x-axis and frequency (n) on the y-axis.
  geom_col() +                        #Add a column (bar) chart layer to the plot.
  xlab(NULL) +                        #Remove the x-axis label.
  coord_flip()                        #Flip the x and y axes, making it a horizontal bar chart.

########################################################

#advanced preprocessing steps: 

#Another common preprocessing step used to normalize text is to reduce words to their base form (lemmatizing) or their root (stemming). 
#For example, a text might contain the words “decide”, “deciding”, and “decided”.
#The problem: We know that these different features substantially describe the same thing, namely that something is being decided.
#In order for R to recognize this, we need to normalize these words by reducing them:

#Stemming
#-> cutting off word endings to combine similar words (e.g. "fighting" and "fighter" both become "fight")
#function: tokens_wordstem() 

#Lemmatization 
#-> reducing words to their simplest form (e.g. "is" and "were" both become "be")
#available via packages like spacyr and udpipe

#Both come with their own pros and cons - whether this generates useful features or not varies by use case.

########################################################

#Visualization with wordclouds

library(wordcloud)

tidy_reviews %>%
  count(word_token) %>%
  with(wordcloud(word_token, n, max.words = 100))

#comparison clouds 
#We compare the most frequent words from the reviews on Star Wars and Harry Potter movies.

#To filter for Star Wars and Harry Potter reviews, we first create a keyword list with the movie titles
keywords_selected_films <- c("Harry Potter", "Star Wars")

#Create a tibble (data frame) with the selected film keywords
selected_films_dict <- tibble(keywords_selected_films)

#str_extract searches the variable reviews$movie_title for matches with our keyword list and annotates them in a new column
tidy_reviews$selection <- str_extract(tidy_reviews$movie_title, paste(keywords_selected_films, collapse="|"))

#Create a subset 'star_potter' containing only rows with non-NA values in the 'selection' column
#(This means it includes rows with film selections that matched the keywords)
star_potter<-subset(tidy_reviews, !is.na(tidy_reviews$selection))

#Replace NA values in the 'selection' column of 'tidy_reviews' with the label "other"
#(This is done to categorize rows that didn't match the selected films)
tidy_reviews["selection"][is.na(tidy_reviews["selection"])] <- "other"

#view the new data subset 
View(star_potter)

#Generate a comparison word cloud based on the 'star_potter' subset
library(reshape2)

#Create a data frame 'cloud' by counting word frequencies and arranging them in a matrix format
cloud <- star_potter %>%
  count(word_token, selection, sort = TRUE) %>% #Counts the occurrences of words in the 'word_token' column for each 'selection' category (film) in the 'star_potter' subset. The results are sorted in descending order.
  acast(word_token ~ selection, value.var = "n", fill = 0) #Arranges the counted frequencies in a matrix format where rows represent words and columns represent 'selection' categories. The 'n' values (word frequencies) are filled with 0 for any missing combinations.

#Generate a comparison word cloud using the 'comparison.cloud' function

comparison.cloud(cloud, #data
                 max.words=100, #Limits the word cloud to a maximum of 100 words.
                 random.order=FALSE, #Displays words in descending order of frequency.
                 rot.per=.1,#Specifies the rotation proportion for words.
                 colors=brewer.pal(max(3,ncol(cloud)),"Dark2"),#Determines the colors for the word cloud based on the 'Dark2' color palette from the 'RColorBrewer' package.
                 use.r.layout=FALSE, #Disables automatic layout by the 'layout' function 
                 title.size=3, #Sets the title font size
                 title.colors=NULL, #Does not apply specific colors to the title
                 match.colors=FALSE) #Words are not color-matched to the palette


#TF-IDF
#Calculating the ratio between the frequency of words per document (i.e. group of movies) compared to the frequency of these
#words in the whole text (to find typical words for classes of documents).
#Words that are frequent in all documents (e.g. "film", "actor") are weighted down, 
#focusing on typical words for the specific class of documents.

#tf = term frequency, frequency of a word in a document (here: review) 
#idf = inverse document frequency, number of documents / number of documents that feature the word
#tf-idf = tf * idf

#counting words
#Calculate word frequencies in the 'tidy_reviews' dataset, considering 'word_token' and 'selection' columns.
#After counting, remove grouping to prepare for further analysis.
reviews_tf_idf <- tidy_reviews %>%
  count(word_token, selection) %>%
  ungroup()

#counting words in reviews on certain groups of movies (Star Wars vs. Harry Potter),
#using our selection variable created with the keyword list
#Calculate the total words in reviews for each group of movies ('selection'),
#based on the previously calculated 'reviews_tf_idf' data frame.
reviews_total_words <- reviews_tf_idf %>% 
  group_by(selection) %>% 
  summarize(total = sum(n))

#combining both values in a data frame
#Combine the word frequency data ('reviews_tf_idf') with the total word count data ('reviews_total_words')
#using a left join operation, resulting in an updated 'reviews_tf_idf' data frame.
reviews_tf_idf <- left_join(reviews_tf_idf, reviews_total_words)

#calculating tf-idf-values using the bind_tf_idf() function:
#Calculate tf-idf values for each word in the 'word_token' column,
#based on word frequencies ('n') and total word counts ('total') for each 'selection' group.
reviews_tf_idf <- reviews_tf_idf %>%
  bind_tf_idf(word_token, selection, n)

#looking at the most typical words in reviews on Harry Potter and Star Wars movies
reviews_tf_idf %>%
  select(-total) %>% #Removes the 'total' column from the data frame using the select() function.
  arrange(desc(tf_idf)) #Arranges the remaining data in descending order of tf-idf values. This means that words with higher tf-idf values (indicating they are more unique to a specific movie group) will appear at the top of the result.

#Visualization 

#Prepare the 'plot_tfidf_reviews' data frame for visualization by arranging data and creating a factor for 'word_token'
plot_tfidf_reviews <- reviews_tf_idf %>%
  plot_tfidf_reviews <- reviews_tf_idf %>%
  arrange(desc(tf_idf)) %>%
  mutate(word_token = factor(word_token, levels = rev(unique(word_token)))) #Transforms the 'word_token' column into a factor with levels in reverse order. This step is important for correctly displaying the words on the visualization.

#Create a bar plot visualization using tf-idf values

plot_tfidf_reviews %>% 
  top_n(20) %>% #Selects the top 20 words with the highest tf-idf values
  ggplot(aes(word_token, tf_idf, fill = selection)) + #'word_token' on the x-axis, tf-idf values on the y-axis, fill colors based on movie selection
  geom_col() + #Add a column (bar) plot layer to the ggplot object
  labs(x = NULL, y = "tf-idf") + #Set the x-axis label to NULL and label the y-axis as "tf-idf"
  coord_flip() #Flip the x and y axes for a horizontal bar plot

#using tf-idf-values separated by group of movies (Star Wars vs. Harry Potter)
plot_tfidf_reviews %>% 
  group_by(selection) %>% #Groups the data by the 'selection' variable (Star Wars or Harry Potter).
  top_n(15) %>% #Filters the top 15 words with the highest tf-idf values for each movie group.
  ungroup %>% #Removes grouping to ensure the following operations are applied to the entire data.
  ggplot(aes(word_token, tf_idf, fill = selection)) + #'word_token' on the x-axis, tf-idf values on the y-axis, fill colors based on movie selection
  geom_col(show.legend = FALSE) + #Adds a column (bar) plot layer to the ggplot object and removes the legend.
  labs(x = NULL, y = "tf-idf") + #Sets the x-axis label to NULL and labels the y-axis as "tf-idf".
  facet_wrap(~selection, ncol = 2, scales = "free") + #Facets the plot, creating separate facets for each movie group (Star Wars and Harry Potter), with 2 columns per row, and free y-axis scales.
  coord_flip() ##Flip the x and y axes for a horizontal bar plot

########################################################

#NAMED ENTITY RECOGNITION 

#For bigger data sets use SpacyR or OpenNLP (advanced and taking time),
#smaller data sets can be parsed with the StanfordNLP Tool (https://nlp.stanford.edu/software/CRF-NER.shtml#Download).
#The annotated document can be saved and loaded into R.

########################################################

#First we are saving the review texts (plus review ids) of a subset of our data set (e.g. a specific movie)
#as a text file that we then load into the StanfordNLP tool (outside of R).

#Filter the 'reviews' data frame to include only rows where the 'movie_title' column is "Casino Royale".
NER_subset <- subset(reviews, reviews$movie_title == "Casino Royale")

#Create a new data frame 'NER_input' using the subset, containing only the 'review_id' and 'review_content' columns.
NER_input <- NER_subset[, c("review_id", "review_content")]

#Write the NER_input data frame to a text file

write.table(NER_input, file = "data/ner_input.txt", sep = ";",
            row.names = FALSE, col.names = TRUE)

########################################################

#Use StanfordNLP Tool

########################################################

#After exporting the file from the StanfordNLP tool with the tags we then load it back into R

NER_example <- read_csv2("/Users/johanna/Desktop/Workshop text analysis/data/datasets/ner_output_bond.csv")

View(NER_example)

#Filter by named entities (e.g. persons)

#Extract text portions enclosed within "<PERSON>" and "</PERSON>" tags from 'review_content'.
NER_example$NER_pers <- str_extract(NER_example$review_content, paste("(<PERSON>)(.*?)</PERSON>", collapse="|"))

#Replace "<PERSON>" tags with a space.
NER_example$NER_pers <- gsub("<PERSON>", " ", NER_example$NER_pers, ignore.case = TRUE)

#Replace "</PERSON>" tags with a space.
NER_example$NER_pers <- gsub("</PERSON>", " ", NER_example$NER_pers, ignore.case = TRUE)

#counting the most frequently mentioned persons in our subset of movie reviews
NER_count <- NER_example %>%
  count(NER_pers, sort = TRUE)

View(NER_count)

#save the files if you want to use them later
write.csv(tidy_reviews, 'data/datasets/prep_tokenized.csv')
write.csv(reviews, 'data/datasets/preprocessed.csv')
