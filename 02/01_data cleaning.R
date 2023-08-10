
#DATA

#To illustrate how to clean data in R, we will use existing representative survey data 
#to analyze how support for migrants or refugees in Europe changes over time and differs per country. 
#The Eurobarometer (freely available at the Leibniz Institute for the Social Sciences – GESIS) 
#has contained these specific questions since 2015. 

#But first - installing packages!
#As you learned in the last workshop, install.packages() is used to download and install the package 
#(you only need to do this once on your computer) 
#and library() is used to make the functions from this package available for use 
#(required each session that you use the package).

install.packages('tidyverse') 
library(tidyverse)

install.packages('readr')
library(readr)

install.packages('dplyr')
library(dplyr)

install.packages("car")
library(car)


#Read data from a local file on your computer 
#replace your path!

#There are two frequently used commands for reading CSV files: read.csv reads files in which columns are 
#separated with commas; read.csv2 reads files in which columns are separated with semicolons. Otherwise 
#these functions have the same structure. 

data <- read_csv2('eurobarometer_dataframe.csv')

###PREVIEW THE DATA

#We take a quick look at the data to understand the structure of the dataset 
#and make sure that everything has been read in correctly.
View(data)

#The glimpse() function gives an overview of the information contained in a dataset. 
#We can see the number of observations (rows), and the number of variables, (columns). 
#We also see the name of each variable and its type. 

glimpse(data)

#The head() and tail() command allow us the view the first, or last, rows of a dataset.

head(data)

tail(data)

#The summary function provides additional information. 
#It can be used for the entire dataset, or individual variables.

summary(data)

#the broad overview of the data already shows us that there are missing values 
#for some variables - we keep this in mind for later analyses! 


###DATA CLEANING 

#Renaming, Filtering, and Selecting

#Selecting and renaming columns
#A first clean up step we often want to do is removing unnecessary columns and renaming columns 
#with unclear or overly long names. In particular, it is often convenient to rename columns that 
#contain spaces or non-standard characters, so it is easier to refer to them later.

#rename type_community in community and ...1 in count
data <- rename(data, community='type_community')
data <- rename(data, count='...1')

#Filter by column
#It is possible that we don't want to keep all variables. In our case the variable "count" was read in 
#by mistake and we want to delete it from our data frame.

#delete the column "count" 
data <- data %>% select(-count)

#If the dataset contains a large number of variables, narrow down to the ones you are interested in working with. 
#This can be done with the select() command. 

data_subset <- data %>% select(country, marital_status, age, gender, support_refugees, support_migrants)

#Filter by rows
#In addition, we might want to analyze only certain cases (rows). For this, we need to filter our dataset by row. 
#For example, it could be the case that we are only interested in respondents from Germany for our analysis.

#Option 1: subset
#The subset function allows us to filter a data set based on values that have rows for one or more variables.

data_germany <- subset(data,data$country=="Germany")
table(data_germany$country)

#Option 2: dplyr
#With filter() we could create a new object data_ger_aut, which contains only cases, 
#which take the value "Germany" and "Austria" for the variable country.

data_ger_aut <- data %>% filter(country == "Germany"|country == "Austria")
table(data_ger_aut$country)

#Filter by column and row
#Of course, you may also want to reduce your dataset to certain columns and rows - for example, 
#because you want to analyze only certain variables and cases.
#In this example, we want to create a new data frame that contains only participants from Finland 
#and includes the variables "Age" and "Gender". 

data_ger <- data[(data$country == "Germany"), c("age", "gender")]

#Sorting data
#In some cases, we may also want our data in a certain order - for example, numerically or alphabetically. 
#To do this, we can use the order command. For example, we can sort our data by country:

data_sorted <- data[order(data$country),]

#We could even sort by several variables - for example, 
#first by country and then, within countries, by age:

data_sorted <- data[order(data$country, data$age),]

#Grouping

#Sometimes, we want to compute summary statistics of groups of rows.
#For example, we can calculate the average age of the respondents by country 
#and save these values in a new data frame. For this we use again dplyr:

data_country <- data %>% 
  group_by(country) %>% 
  summarize(age = mean(age))

#Modifying the data 

#Add a new variable 
#With mutate() we can add new variables to datasets (or overwrite old ones). To do this, we specify the new variable name, 
#followed by an = and the calculation or construction of the new variable. If a variable name that already exists in the 
#dataset is used as the variable name, this variable will be overwritten. Several new variables can also be created 
#separated by commas.

data <- data %>% mutate(age_mean=mean(age))

#Replace selected values
#Sometimes you don't just want to filter your data, but it's necessary to substitute values for certain cases.
#We see that both "general mangagement, etc." and "middle management, etc." were coded as possible values for 
#the variable "occupation". It might make sense to combine these two values because they cannot really 
#be distinguished in terms of content.

table(data$occupation)

#We create a new variable: occupation_edited
data$occupation_edited <- data$occupation

#We replace the value for selected cases
data$occupation_edited[
  data$occupation_edited=="General management, etc."|
  data$occupation_edited=="Middle management, etc."
] <- "Management"
                   
table(data$occupation_edited)

#Recoding items 

#Sometimes we also want to recode the items of a variable, 
#for example to group the single values or to recode scale values with a negative sign. 

#In our data set, we want to create a new variable from the variable "age" in which the data is grouped into three age groups.
#Group 1 includes all persons between 15-39, Group 2 includes all between 40-69, and Group 3 includes all between 70-99.

data$age_grouped <- recode(data$age, "15:39=1; 40:69=2; 70:99=3")
table(data$age_grouped)  

#Create dummy variables 

#With nominal or categorical variables, we can't calculate a (multiple) linear regression. To do this, we must first create dummy variables.
#In our data set we want to create dummy variables for each age group, that we created in the last step. 

#We are interested in the three characteristics 1) age 15-39 2) age 40-69 3) age 70-99. 
#From the variable "age_grouped" we now create three separate variables: (1) Dummy_young, (2) Dummy_mid, (3) Dummy_old. 
#These variables always have the value 1 if the characteristic converted to the dummy occurs in the original variable. 
#Example: Dummy_young has the value "1" if the age_grouped is "15-39". Dummy_mid is "1" if the age_grouped is "40-69".

data$dummy_young <- recode(data$age_grouped, "1=1; 2:3=0")
data$dummy_mid <- recode(data$age_grouped, "1=0; 2=1; 3=0")
data$dummy_old <- recode(data$age_grouped, "1:2= 0; 3=1")

#As a result, we now get three variables that are dichotomous and always take the value 1 
#if the initial variable age_grouped contains the respective value.


#The generated data frame then shows us the mean value of the respondents' age by country
View(data_country)

#Missing values

#In R, empty cells in a data frame are encoded as Missing Values (NA). These values represent 
#an important amount of data in many real social and communication analysis. 
#From our rough overview of the data at the beginning of the session, 
#we already know that we have some missing values. 

#Identify missing values
#We can use the is.na() inbuilt function in R to check for NA values. This function returns a vector 
#that contains only logical value (either True or False). For the NA values in the original dataset, 
#the corresponding vector value should be True otherwise it should be False.

#Identify the sum of missing values for the variable support_refugees
sum(is.na(data$support_refugees))

#Removing missing values for the variable support_refugees
data_removed <- data %>% filter(!(is.na(support_refugees))) 
sum(is.na(data_removed$support_refugees)) #have the missing values been deleted?

#We can also create the subset data_clean, which contains only the cases where we have 
#no missing values in any of the variables. This can be done with the command complete.cases():
data_clean <- data[complete.cases(data),]
sum(is.na(data_clean$support_refugees)) #have the missing values been deleted?
sum(is.na(data_clean$support_migrants))







