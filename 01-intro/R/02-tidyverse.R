### intro to the 'tidyverse' ###

# but first: packages!
# many programming languages (like R and Python) make it easy to use
# functions written by other developers as 'packages'
# we can download them by simply calling "install.packages('package_name')"
# the packages are hosted on the "Comprehensive R Archive Network" or "CRAN"

# you don't have run this if you already installed the package
install.packages('tidyverse')

# to actually use a package we have to first import (or 'attach') it to our 'library'
library(tidyverse)
# alternatively we can use 'require(package_name)', this will not throw an 
# error if the package is not installed!

# now we can actually use tidy functions, it is customary to put installations
# and imports at the top of a script, but you don't have to do that

# it is also helpfull (but not technically required) to specify the package
# that you are using fucntions from, e.g. package_name::function()
tidyverse::tidyverse_logo()

## the 'tidyverse' tries to make data handling easier, readable and extendable;
## it is a little bit different to "raw" R, but widely accepted as a 
## best practice to use

# building from scratch
ids <- c(1,2,3,4,5)
name <- c('Maja', 'Flip', 'Thekla', 'Willi', 'Puck')
age <- c(5, 10, 20, 5, 5)
type <- c('bee', 'grasshopper', 'spider', 'bee', 'fly')

maja_df <- dplyr::tibble(ids, name, age, type)
# inspect and note the automatic capture of column names!

# using example data:
mpg_df <- ggplot2::mpg # ggplot2 is included in the tidyverse

# loading local data, intro dataset, replace your path!
bands <- readr::read_csv(
  '/Users/ungers/git/team_quandt_internal/01-intro/local/save.csv'
)
# inspect the dataset!
# drop the useless first row
bands <- bands[,-1]

# the 'tidy' part:
# the powerful thing about the tidyverse is 'piping' functions into functions 
# into functions etc.
# to do this, we use the 'pipe' operator %>% 
# it sometimes helps to read it as "and then"
# tip: use cmd + shift + m (MacOS) or ctr + shift + m so you don't have to
# type it every time!
# example: examining the top 2 rows of a dataframe 
# or tibble (tidy datframe format), using the head() function
bands %>% head(2)

## filtering
maja_df %>% filter(age == 5)
maja_df %>% filter(age == 5 & type == 'bee')
maja_df %>% filter(type == 'bee' | type == 'spider') # | -> OR
# comparison to dataframes:
maja_df[maja_df$age == 5,] # don't forget the comma
maja_df[maja_df$age == 5 & maja_df$type == 'bee',]
maja_df[maja_df$type == 'bee' | maja_df$type == 'spider',]
# there is no right or wrong, decide for yourself which one is better

# try to get everyone that is called Flip or Willi and older than 6
# this should let you only get Flip ;)

# maja_df %>% filter( (name == 'Flip' | name == 'Willi') & age > 6)

# other useful filter options are:
mpg_df %>% filter(year %in% c(1999, 2008)) # %>%  view() -> 

mpg_df %>% filter(str_detect(trans, '5'))

mpg_df %>% filter(is.na(cyl)) # try '!is.na()'

mpg_df %>% filter(complete.cases(cyl))

## subsetting variables with 'select()'

mpg_df %>% select(cty, hwy) # city and highway miles per gallon
# negative selection:
mpg_df %>% select(-manufacturer, -model)

# you can also select columns by starting value or parts of the name:
mpg_df %>% select(contains('r'))
mpg_df %>% select(starts_with('m')) # try it with '!' or '-'

# or reorder parts of the dataset:
reorder <- mpg_df %>% select(cty, hwy, everything()) # how?
reorder %>% head(10)

# sorting:
reorder %>% arrange(hwy) # try it with multiple variables
reorder %>% arrange(desc(year))

# chaining:
# try getting cars from 1999 and then
# selecting manufacturer and model and then
# sorting them alphabetically and then
# showing the view of your selection!

# below is the typical chaining style to avoid long lines
mpg_df %>%
  filter(year == 1999) %>% 
  select(manufacturer, model) %>%
  arrange(manufacturer, model) %>% 
  view()

# manipulating data -> mutate
mpg_df %>%
  mutate(c_diff = hwy - cty) %>% 
  filter(c_diff >= 10) %>% 
  # select(manufacturer, model, year, cty, hwy, c_diff, everything()) %>% 
  view()
# -> explain what happened here step by step

# more data inspection and overview
## general summary
mpg_df %>% summary()
## specific summary statistics
mpg_df %>% 
  summarize(
    min_year = min(year),
    max_year = max(year),
    mean_cty = mean(cty),
    mean_hwy = mean(hwy),
    n_rows = n()
  )

## grouped summary statistics
mpg_df %>% 
  group_by(year) %>% 
  summarize(
    min_year = min(year),
    max_year = max(year),
    mean_cty = mean(cty),
    mean_hwy = mean(hwy),
    n_rows = n()
  )

### challenge ###
# import the 'diamonds" data set included with ggplot2
# 1 USD == 0.93 EUR
diamond_df <- # ...

# calculate prices in Euro 1 USD == 0.93 EUR and add as a column (EUR)
# extract number of rows, min, max, mean and standard deviation (sd())
# for carat and price (EUR) grouped by cut
# save as a .csv file locally

  
  
  
  
  























































### Challenge Solution ###
diamond_df <- ggplot2::diamonds

summaries <- diamond_df %>% 
  filter(carat >= 0.25) %>% 
  mutate(EUR = price * 0.93) %>% 
  group_by(cut) %>% 
  summarize(
    min_EUR = min(EUR),
    max_EUR = max(EUR),
    mean_EUR = mean(EUR),
    std_EUR = sd(EUR),
    min_carat = min(carat),
    max_carat = max(carat),
    mean_carat = mean(carat),
    std_carat = sd(carat),
    n_rows = n()
  )
  
write_csv(
  summaries,
  '/Users/ungers/git/team_quandt_internal/01-intro/local/diamond_sum.csv',
)

# can you include the write function in the piped block?