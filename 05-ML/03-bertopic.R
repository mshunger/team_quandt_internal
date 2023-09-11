# run this ahead of time!
install.packages('devtools')
install_github("abresler/bertopic")
install.packages('reticulate')
library(reticulate)
reticulate::install_python()
reticulate::py_install('bertopic')
reticulate::py_install('flair')
reticulate::py_install('scikit-learn==1.2.2')
reticulate::py_install('numpy==1.24')

# imports
library(bertopic)
library(dplyr)
library(tidyverse)
reticulate::import('flair')

nltk <- reticulate::import('nltk')
nltk$download('stopwords')

import_bertopic()
import_sentence_transformers()

model_st <- sentence_transformer() # modular

#model_roberta <- roberta_embeddings() # modular

# sample data from the "20 newsgroups" dataset
df <- read_csv('/Users/ungers/git/team_quandt_internal/local/rotten_tomatoes_meta.csv')
#df_sample <- sample_n(df, 10000)
#docs <- df_sample$review_content

split <- sample(2, nrow(df), replace=TRUE, prob=c(.98, .02))
df$split <- as.factor(split)

# sample and rest (for prediction)
sample <- df %>%
  dplyr::filter(split==2) 
rest <- df %>% 
  dplyr::filter(split==1) 

docs <- sample$review_content


# initiallize the model
model <- bert_topic(
  verbose=T, # we want info while it runs
  embedding_model = sentence_transformer(), # here we can use different pre-trained models
  low_memory=T # if you don't have a lot of memory
  #embedding_model=roberta_embeddings() # alternative to sentence_transformer, much slower
)

# what about preprocessing?

# running the model, this takes some time!
topic_model <- model$fit_transform(documents = docs)

# what did the model cluster together?
# does this make sense?
summary <- bert_topic_info(obj = model, topic_number = NULL)
view(summary)


# topics similar to interesting terms
magic = c('fantasy', 'magic', 'wizard')
action = c('violence', 'gun', 'fight')
horror = c('scary', 'fear', 'horror')
# it even works with biterms!
scifi = c('science fiction', 'space ship', 'alien')
top_sim <- bert_similar_terms_topics(obj = model, terms = horror)  %>% 
  group_by(term)
view(top_sim)

# getting topic information on documents
doc_info <- bert_document_info(model, docs)
view(doc_info)

# predicting topics for "new" documents (random -> change values if necessary)
rest$review_content[2]

to_predict <- rest$review_content[2]
to_predict

predicted <- model$transform(to_predict)
predicted[1]

topic_6 <- bert_topic_info(model, 6)
topic_6$label_bertopic
topic_6$text_representative_documents


# built in visualization

## barchart of word scores for most prevalent topics
bars <- model$visualize_barchart()
bars$write_html("/Users/ungers/git/team_quandt_internal/local/barviz.html")

## documents in the topic space

# calculate embeddings first (takes some time!)
embeddings <-
  model$embedding_model$embed(documents = docs, verbose = T)

viz <-
  model$visualize_documents(
    docs = docs,
    embeddings = embeddings,
    width = 2000L,
    height = 1000L,
    sample = .2,
    hide_annotations = T
    
  )
viz$write_html("/Users/ungers/git/team_quandt_internal/local/docviz.html")

## hierarchical topic tree
topic_tree <-
  model$get_topic_tree(hier_topics = model$hierarchical_topics(docs = docs))
viz <- model$visualize_hierarchy()
viz$write_html("/Users/ungers/git/team_quandt_internal/local/hierarchy.html")

# the original python version is even more powerful supporting temporal modeling
# soft clusterin*, and more!