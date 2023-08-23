
###DATA###

#In this section, we’ll analyze a dataset called pirates! :-) 
#The dataset contains data from a survey of 1,000 pirates. 
#The data is contained in the yarrr package, so make sure you’ve installed and loaded the package:

# Install the yarrr package
install.packages('yarrr')

# Load the package
library(yarrr)

#Next, we’ll look at the help menu for the pirates dataset using the question mark ?pirates. 
#When you run this, you should see a small help window open up in RStudio that gives you 
#some information about the dataset.

?pirates

#create a data.frame 

pirates <- pirates

#Exploring the data 

# Look at the first few rows of the data
head(pirates)

# What are the names of the columns?
names(pirates)

# View the entire dataset in a new window
View(pirates)


###DESCRIPTIVE STATISTICS###

##Categorical variables

#Frequencies
#Frequency tables are one of the most important tools to get an overview of the data.

#Calculating absolute frequencies

#How many pirates wear a headband?
table(pirates$headband)

#Calculating relative frequencies

#What percentage of pirates were at "Jack Sparro's School of Fashion and Piratery (JSSFP)" 
#and what percentage were at "Captain Chunk's Cannon Crew" (CCCC) college? 

prop.table(table(pirates$college))
round(prop.table(table(pirates$college)),2) #rounded to two decimal

#Calculating cumulative frequencies

#With cumsum() we can add the percentages successively. 

cumsum(prop.table(table(pirates$college)))
round(cumsum(prop.table(table(pirates$college))),2) #rounded to two decimal

#To summarize absolute, relative and cumulative frequencies in one table, we use cbind(). 
#Let's try it for the variable favorite.pirate! Who is the favorite pirate?

frequ_a <- table(pirates$favorite.pirate)
frequ_r <- round(prop.table(table(pirates$favorite.pirate)),2)
frequ_c <- round(cumsum(prop.table(table(pirates$favorite.pirate))),2)
frequ_table <- cbind(frequ_a, frequ_r, frequ_c)
print(frequ_table)

#We can also use the dplyr functions group_by and summarize to create a frequency table:

pirates %>%
  group_by(favorite.pirate) %>%
  summarise(frequency = n()) %>%
  mutate(rel_freq = frequency / sum(frequency))  

####################################################

#GOOD TO KNOW: How can we visualize our data? 

#An important step in data exploration is the visualization of data, as this allows us to find and clarify anomalies 
#and correlations that remain hidden through purely descriptive evaluations.
#The basic version of R already provides some data visualization functions, including the plot() function, 
#which generates a corresponding visualization depending on the object(s) passed.
#In the Tidyverse, the package ggplot2 performs all steps of data visualization and provides its own 
#syntax - a "grammar of graphics" - which opens up many possibilities and flexibility for data visualization. 

#The three steps - Encoding, Geometrics, Scales - represent the essential decisions of data visualization:

#Encodings (or Aesthetics): 
#Which data or which variables should be transferred into which visual elements (e.g. X-axis, Y-axis, colors)?
###Options for Aesthetics are among others:
###x: values on X-axis
###y: values on Y-axis
###color: line colors (for points, lines, contours of areas)
###fill: fill colors (for areas, e.g. bars)
###shape: dot shape (only when using dot diagrams)
###linetype: line type (e.g. solid, dashed, etc.; correspondingly only for line charts)
###alpha: transparency

#Geometrics: 
#How should these visual elements be represented (e.g. as points, lines, bars) in the (mostly) two-dimensional 
#space provided by a graphical representation?
###Options for Geometrics are among others:
###geom_point(): points
###geom_line(): Lines
###geom_bar(): bars for frequencies of the x-variable (y-coding need not be specified)
###geom_col(): bars whose height is specified by a y variable
###geom_hist(): histograms
###geom_boxplot(): Boxplots

#Scales: 
#On which scales should the relevant scales be mapped?


####################################################

#Visualize frequencies 

#To create a simple bar chart for the number of favorite pirates, 
#we define favorite.pirate as x-Aesthetic and add a geom_bar():

pirates %>% 
  ggplot(aes(x = favorite.pirate)) +
  geom_bar()

#If we also want to color the bars differently depending on the pirate, 
#we have to double code favorite.pirate - both as x, and as fill (the fill color of the bars):

pirates %>% 
  ggplot(aes(x = favorite.pirate, fill=favorite.pirate)) +
  geom_bar()

#We can also add a title:

pirates %>% 
  ggplot(aes(x = favorite.pirate, fill=favorite.pirate)) +
  geom_bar() +
  ggtitle("Favorite Pirates")

#Now we want to make the labels of the individual scales a little more informative.

#We start with the Y axis; it is the Aesthetic y and a numeric scale. The correct scale function 
#would therefore be scale_y_continuous(), which we can now add to our plot object by +.
#In this function we can now define arguments how the scale should be changed - for example name for the name 
#of the scale, limits for the lower and upper limits of the scale, and breaks for the sections of the scale:

pirates %>% 
  ggplot(aes(x = favorite.pirate, fill=favorite.pirate)) +
  geom_bar() +
  ggtitle("Favorite Pirates") +
  scale_y_continuous(name = "Count favorite pirates", limits = c(0, 500), breaks = seq(0, 500, 50))

#And now we have already adjusted the Y-axis. We continue with the x-axis. Here we are dealing with categorical, 
#i.e. also discrete values, so the corresponding scale function is scale_x_discrete(). Here, we also specify a 
#new name and the values in the desired order by limits.

pirates %>% 
  ggplot(aes(x = favorite.pirate, fill=favorite.pirate)) +
  geom_bar() +
  ggtitle("Favorite Pirates") +
  scale_y_continuous(name = "Count favorite pirates", limits = c(0, 500), breaks = seq(0, 500, 50)) +
  scale_x_discrete(name = "Pirate name", 
                   limits = c("Lewis Scot", "Jack Sparrow", "Hook", "Edward Low", "Blackbeard", "Anicetus"))

#This are just some ggplot basics - ggplot offers a wide range of possibilities for data visualization! 

##Continuous variables

#Measures of central tendency

#Mean
#The mean is the center of a distribution. It is the sum of all existing values divided by the number of existing values.
#How many tattoos does a pirate have on average? 

mean(pirates$tattoos)

#Median
#The median is the value that stands in the middle of a sequence of values ordered by size.

median(pirates$tattoos)

#Measures of dispersion
#Measures of dispersion indicate whether the distribution is homogeneous or heterogeneous.

#Variance
#Variance is the average squared deviations from the mean.

var(pirates$tattoos)

#Standard deviation
#The standard deviation is the square root of variance and describes the average deviation of 
#all measured values from the mean. Low standard deviation means data are clustered around the mean, 
#and high standard deviation indicates data are more spread out.

sd(pirates$tattoos)

#Interquartile range
#The interquartile range describes the difference between the upper (75%) and lower (25%) quartile limits.

quantile(pirates$tattoos)
IQR(pirates$tattoos)

#Range
#The range is the difference between the maximum and minimum of the distribution.

min(pirates$tattoos) #minimum
max(pirates$tattoos) #maximum
range(pirates$tattoos) #the first element represents the minimum, the second the maximum

#A nice summary of the values is provided by the function summary():

summary(pirates$tattoos)
round(summary(pirates$tattoos),2)

#Shape of the distribution

#Install the moments package
install.packages("moments")

#Load the package
library(moments)

#Skewness
#The skewness indicates whether a distribution is symmetric, right skewed or left skewed. 
#Values close to zero describe symmetrical figures. Negative values stand for left-skewed and 
#positive values for right-skewed distributions.

skewness(pirates$tattoos)

#Kurtosis
#The kurtosis of a distribution indicates whether the distribution is rather steep or flat. 
#Negative values stand for flat distributions, positive values for steep distributions. 

kurtosis(pirates$tattoos)

###Good to know: Modality
###The modality of a distribution determines how many high points or relative maxima the distribution has. 
###Unimodal distributions have a single high point, i.e. a value that occurs most frequently (mode). 
###Bimodal distributions have two high points, multimodal distributions even several.

#Skewness, kurtosis and modality are not reported in parameters by default. 
#The shape of the distribution is often better represented visually with histograms, density plots or boxplots.

##Visualize the shape of the distribution

#Histogram
#Histograms are useful for displaying the distribution of a single quantitative variable.
#We use again ggplot! 

pirates %>% 
  ggplot(aes(x=tattoos)) + 
  geom_histogram(fill="lightblue", color="white") + 
  ggtitle("Histogram: Number of Tattoos") +
  xlab("Number of tattoos") + 
  ylab("Frequency")

#Density Plots
#Density plots show the distribution for a quantitative variable. 
#Scores can be compared across categories.

pirates %>% 
  ggplot(aes(x=tattoos, color=sex, fill=sex)) + 
  geom_density(alpha=0.2) + #alpha: ranging from 0 to 1 dictates transparency
  ggtitle("Density Plot: Number of Tattoos") +
  xlab("Number of tattoos") + 
  ylab("Frequency")

#Boxplots
#Boxplots focus not on the shape but on the location and dispersion of distributions.
#They can be also used to compare a quantitative variable with a categorical variable.

pirates %>% 
  ggplot(aes(x=sex, y=tattoos)) + 
  geom_boxplot() + 
  ggtitle("Boxplot: Number of Tattoos and Gender") +
  xlab("Gender") + 
  ylab("Number of tattoos")

#Violin Plot
#Violin plots are an alternative to boxplots. 
#The width of the violin tells us the density of observations in a given range.

pirates %>% 
  ggplot(aes(x=sex, y=tattoos, fill=sex)) + 
  geom_violin() + 
  ggtitle("Violin Plot: Number of Tattoos and Gender") +
  xlab("Gender") + 
  ylab("Number of tattoos")







