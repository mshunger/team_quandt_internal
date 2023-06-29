#### additional training ####

# import the "midwest" dataset from ggplot2 (tidyverse package)

# briefly describe what the dataframe is generally about (no statististics)

# how many states and counties are in the data?


# calculate all population statistics on the state level
# you can create a new dataframe for each step or do it in one go

## 1. add population counts and area grouped by state
## hints: use "group_by" and "summarise" with "across" and "everything()"
## also: you are allowed to use the internet for help!


## 2. build a function to calculate percentages
## inputs: total population and part of population
## output: the percentage value


## 3. produce a new column with statewide percentages
## for all populations


### a) do this with the function defined above


### b) do this within tidy


# which state has the highest percentage of inhabitants identifying as
# amerindian?


# save the dataframe with statewide populations counts and percentages as a .csv


# For-Loops
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