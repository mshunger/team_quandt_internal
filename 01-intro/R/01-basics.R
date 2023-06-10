# <- this is a comment, whenever you want you can put a '#' sign down
# to mark something as a comment, nothing precluded by this sign will be 
# executed

### The very basics ###

# R is a calculator!
# execute the following line by highlighting everything and pressing the 
# 'run' button above (slightly to the right ;) )
1 + 1 # = ?
# also try the following: 
## click at the end of the line and press 'ctrl' + 'enter'
## click the number before the line and ececute the code

# more mathematical notation for R:
2 * 3  # -> multiplication
2 - 1  # -> subtraction
6 / 3  # -> division
2^2    # -> exponentiation
2 ** 2 # -> exponentiation 
(2 + 2) * (6 - 2) # -> calculation with brackets

# useful operators
1 < 2 # -> greater than
10 > 5 # -> lesser than

3 == 3 # important! '=': assignment; '==': 'equals'
4 != 5 # -> unequal

6 >= 2 # -> greater/equals
7 <= 9 # -> lesser/equals

# variables
# -> assigning values (and even other variables, functions etc.) to memory

a <- 1
b = 2
a + b -> c
# the difference is style, in the R community '<-' is preferred,
# it matters in edge cases

print(c) # a function! more on that later

# data types
## we mostly use basic types for variables
# numeric types
numeric <- 5 # just use this ;)
int <- 5L
comp <- 5.0 + 3i
# string
st <- 'hello world'

# logical aka boolean
a <- TRUE # alternatively 'T'
b <- FALSE # alternatively 'F'
## try a + a, what happens?

# containers: variables to keep variables
vec <- c(1,2,3) # -> initiallized with c(), 'atomic'; converts implicitly
print(vec)

lst <- list(1,2,3) # -> 'recursive', can hold different types

## try c(1,2,3,'test') and print vs list(1,2,3,'test)

# both vectors and lists can be named for data retrieval and inspection
naming <- c('a', 'b', 'c')
vec = c(1,2,3)
lst <- list(1,'test',FALSE)
names(vec) <- naming
names(lst) <- naming

# data retrieval from containers -> 'indexing'
# R indexes start at 1!
print(vec[2])
print(lst[3])
# or by name, for named vectors and lists
print(vec['b'])
print(lst['c'])

# changing container content:
print(vec)
vec[2] <- 5
print(vec)

# checking datatypes:
class(a)
class(vec) # -> numeric vector!
class(lst)

### Dataframes ###
ids <- c(1,2,3,4,5)
name <- c('Maja', 'Flip', 'Thekla', 'Willi', 'Puck')
age <- c(5, 10, 20, 5, 2)
type <- c('bee', 'grasshopper', 'spider', 'bee', 'fly')

df <- data.frame(
  ids,
  name,
  age,
  type
)

# alternatively: df <- data.frame(ids, name, age, type); or:
df_list <- list(ids, name, age, type)
df <- data.frame(df_list) # -> produces really weird names, rename:
names(df) <- c('ids', 'name', 'age', 'type')

# accessing dataframes by names:
df$name # also check what class that is!
# indexing and slicing
df[1] # -> what does that do?
df[2,2] # row, column -> cell indexing
# what if we want the name and age of the second character?
df[2,2:3]

# why dataframes? -> e.g. statistical operations like the mean age
mean(df$age)

# or rowwise calculations:
df2 <- data.frame(
  id <- c(1:5), # what could this be?
  name <- c('Wardruna', 'Skinflower', 'Behemoth', 'Konvent', 'Ultha'),
  country <- c('Norway', 'Netherlands', 'Poland', 'Denmark', 'Germany'),
  albums <- c(5, 0, 12, 2, 4),
  singles_ep <- c(3, 1, 12, 0, 4),
  splits <- c(1, 0, 1, 0, 3),
  formed <- c(2003, 2015, 1991, 2015, 2014)
)
names(df2) <- c('id', 'name', 'country', 'albums', 'singles_ep', 'splits', 'formed')
mean.albums <- mean(df2$albums)
print(mean.albums)

# adding a new row:
new <- data.frame(
  id <- c(6),
  name <- c('Chthonic'),
  country <- c('Taiwan'),
  albums <- c(11),
  singles_ep <- c(14),
  splits <- c(2),
  formed <- c(1995)
)
names(new) <- c('id', 'name', 'country', 'albums', 'singles_ep', 'splits', 'formed')

df2 <- rbind(df2, new)

# all CDs? -> assign a new column with the calculation
df2$all.cds <- df2$albums + df2$singles_ep + df2$splits
print(df2$all.cds)

# productivity: all.cds / (band age = this year - formed):
01-basics
mean(df2$productivity)
max(df2$productivity)
# what band is that?

df2[df2$productivity==max(df2$productivity),]

# Further inspections
str(df2)
summary(df2)

# Data Export and import to csv
## but first: filepaths (windows, either use '\' or '//')
fpath <- '/Users/ungers/git/team_quandt_internal/01-intro/local/save.csv' # explicit path
fpath <- 'save.csv' # implicit path, starts from where we are, which is...
getwd()
# and if we want to specify where we actually work:
setwd('/Users/ungers/Documents/') # or alternatively in my case: setwd('Documents')
getwd() # -> this should be where you want to be now!

# writing the dataframe as a csv-file
write.csv(
  df2,
  file = fpath, # use your path implicitly or explicitly
)

load <- read.csv(fpath, sep=',') # or, again, read.csv

### Important Programming ###
# funtions and for-loops

# Functions; typical functions
# tip: click on the function name and press 'F1' (while using RStudio)
print('hello world!') # you are already using functions!
write.csv('...')
class('...')
a <- paste('hello', 'world', sep='; ')

# what if you want a specific function?
# Google is your friend, maybe it already exists...if not, build your own!

# name it  # tell it to be a function   # define inputs  # define tasks
my_function <- function                 (inputs)         {tasks}

my_function <- function(num, expo){
  num <- num ** expo
  return(num) # return the calculated value
}

test <- my_function(2, 3)
print(test)

# control flow
# if: check for conditions, important for most functions and loops!

# initiallize # define condition  # define tasks if condition holds
if            (condition)         {tasks}

cond <- 2
if(cond == 2){
  print(paste(cond, 'equals', 2, sep=' '))
}

# else if: more conditions!
cond = 5
if(cond == 2){
  print(paste(cond, 'equals', 2, sep=' '))
} else if(cond > 2){
  print(paste(cond, 'is greater than', 2, sep=' '))
} 

# else: if none of the conditions hold, do ...
cond = 1
if(cond == 2){
  print(paste(cond, 'equals', 2, sep=' '))
} else if(cond > 2){
  print(paste(cond, 'is greater than', 2, sep=' '))
} else {
  print(paste(cond, 'is lesser than', 2, sep=' '))
}


# Loops
## performing repetitive tasks

# for-loop: do something for every element of a container
# example: we want to multiply each number from 1 to 5 with itself

# initiallize  # define container  # define tasks
for            ()                  {}

for(i in 1:5){
  xp <- my_function(i, i)
  print(xp)
}

# while-loop: do something until a condition is fulfilled 
# HINT -> controll flow

source <- 1
target <- 0

while(target < 5000){
  target <- my_function(source, source)
  print(target)
  source <- source + 1
}