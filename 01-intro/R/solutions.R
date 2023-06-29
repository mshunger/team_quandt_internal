#### additional training ####

# import the "midwest" dataset from ggplot2 (tidyverse package)
library(tidyverse)
mw <- ggplot2::midwest

# briefly describe what the dataframe is generally about (no statististics)
?ggplot2::midwest

# how many states and counties are in the data?

mw %>%
  select(state) %>% 
  distinct(state) %>% 
  count()

mw %>% select(county) %>% 
  distinct(county) %>% 
  count()

# calculate all population statistics on the state level
# you can create a new dataframe for each step or do it in one go

## 1. add population counts and area grouped by state
## hints: use "group_by" and "summarise" with "across" and "everything()"
## also: you are allowed to use the internet for help!
a <- mw %>% 
  select(
  state, area, poptotal, popwhite,
  popblack, popasian, popamerindian, popother
  ) %>% 
  group_by(state) %>% 
  summarise(
    across(
      everything(), sum)
    )

## 2. build a function to calculate percentages
## inputs: total population and part of population
## output: the percentage value

percs <- function(total, part){
  perc <- part/total
  return(perc)
}

## 3. produce a new column with statewide percentages
## for all populations

### a) do this with the function defined above
a <- a %>% mutate(percwhite=percs(poptotal, popwhite))
a <- a %>% mutate(percblack=percs(poptotal, popblack))
a <- a %>% mutate(percasian=percs(poptotal, popasian))
a <- a %>% mutate(percamerindian=percs(poptotal, popamerindian))
a <- a %>% mutate(percother=percs(poptotal, popother))

### b) do this within tidy
a <- a %>% mutate(percwhite=popwhite / poptotal)
a <- a %>% mutate(percblack=popblack / poptotal)
a <- a %>% mutate(percasian=popasian / poptotal)
a <- a %>% mutate(percamerindian=popamerindian / poptotal)
a <- a %>% mutate(percother=popother / poptotal)

# which state has the highest percentage of inhabitants identifying as
# amerindian?
a %>% 
  select(state, percamerindian) %>% 
  arrange(desc(percamerindian)) %>% 
  head(1)

# save the dataframe with statewide populations counts and percentages as a .csv
write_csv(a, '/Users/ungers/Documents/statewide_midwest.csv')

# For-Loops:
# Below is a vector with small text snippets and a vextor containing words that 
# we want to remove from them (-> stopwords).
# Write a for-loop that 
## first prints the snippet
## then removes the stopwords and
## lastly prints the snippet without the stopwords
snippets <- c(
  "import the 'midwest' dataset from ggplot2 (tidyverse package)",
  "briefly describe what the dataframe is generally about (no statististics)",
  "how many states and counties are in the data?",
  "calculate all population statistics on the state level, you can create a new dataframe for each step or do it in one go"
)

stopwords <- c(
  ",", "?", "the", "is", "and", "'"
)

# you can remove words with the "gsub()" by replacing a word with an empty
# string -> ""
a <- "this is a string" # remove "a":
a <- gsub("a", "", a)

for (snip in snippets){
  print(snip)
  for (sw in stopwords){
    snip <- gsub(sw, "", snip)
  }
  print(snip)
}