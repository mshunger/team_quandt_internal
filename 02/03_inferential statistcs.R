
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


###INFERENTIAL STATISTICS###

##Categorical variables

#Cross tabulations

#Cross tabulations count the frequencies for the co-occurrence of two characteristics. 
#In simpler terms, we can use cross tabulations to see how often two variable characteristics occur in combination.

# Install the gmodels package
install.packages('gmodels')

# Load the package
library(gmodels)


CrossTable(pirates$college, pirates$headband, 
           prop.c = FALSE, #If TRUE, column proportions will be included
           prop.r = FALSE, #If TRUE, row proportions will be included
           prop.t = FALSE, #If TRUE, table proportions will be included
           chisq = TRUE, #If TRUE, chi-square contribution of each cell will be included
           prop.chisq = FALSE, #If TRUE, chi-square contribution of each cell will be included
           fisher = TRUE, #If TRUE, the results of a Fisher Exact test will be included
           dnn = c("College", "Headband")) #the names to be given to the dimensions in the result

#So now the question is whether the values in the cross tabulation prove a correlation or not.

#Chi-square test

#If you want to see if the frequency of one categorial variable depends on a second categorial variable, 
#you’d conduct a chi-square test. The chi-square test checks whether statistically significant differences exist 
#between expected and observed frequencies. For this purpose, the squared deviations of the observed frequencies 
#from the expected frequencies are calculated and divided by the expected frequencies.


#For example, we might want to know if there is a relationship between 
#the college a pirate went to, and whether or not he/she wears a headband. 

chisq.test(pirates$college, pirates$headband)

#Interpretation of the result:
#At the traditional p = .05 threshold for significance, we would conclude that we fail to reject the null hypothesis 
#and state that we do not have enough information to determine if pirates from different colleges differ in how likely 
#they are to wear headbands.

##GOOD TO KNOW:
##Fisher Exact test
##This is used whenever at least one of the observed cell frequencies is below 5. Why? 
##The approximate calculation of the p-value via the chi-square distribution is biased.
##fisher.test(pirates$college, pirates$headband)


#The chi-square test only (and also the Fisher Exact test) indicates whether there is a statistically significant 
#correlation or not. To find out how strong this correlation is, we need to calculate Cramer's V. 

#Effect size: Cramer's V

#Cramer's V is the square root of the quotient of the χ2-test statistic divided by the product of the number of cases (n) 
#multiplied by the number of columns (j) or the number of rows (i) minus one, where only the lower number is taken into account. 
#The number of columns is used if there are fewer columns than rows, otherwise the number of rows is used.

#The values of Cramer's V are normalized and range between 0 and 1. 0 is equivalent to stochastic independence, 
#i.e. no systematic correlation between the variables. Values close to one indicate almost perfect correlations and 
#thus contradict the null hypothesis. Typically, correlations are estimated as follows (Cohen, 1992):
#1) Values between 0.1 and 0.3 are considered a low correlation,
#2) values between 0.3 and 0.5 are considered a medium correlation, and
#3) values above 0.5 are considered a strong correlation.

cramerV(x=pirates$college, y=pirates$headband)

##Continuous variables

#Covariance 
#A systematic relationship between two metric variables is called covariance. The covariance is the mean deviation of the product 
#of the deviation of the measured values of the first variable from their mean value and the deviation of the values of the second 
#variable from their mean value.

#Let’s see if there is a relationship between a pirate’s age and number of parrots they’ve had in their lifetime.

cov(x=pirates$age, y=pirates$parrots)

#The result of the covariance can be judged by its value and sign. If there is no correlation between the variables, 
#the value of the covariance approaches zero. If both variables covary equally, the values of the covariance become positive; 
#if both variables covary in the opposite direction, the values of the covariance become negative.

#The covariance is a non-standardized correlation measure and therefore has only a low comparability. 
#We can determine the correlation from the covariance. This is standardized and therefore allows a higher comparability.

#Correlation 

#In a correlation test, you are accessing the relationship between two variables on a ratio or interval scale. 
#The Pearson correlation coefficient (r) is the most common way of measuring a linear correlation. The coefficient can range 
#from -1 to +1, with -1 meaning a strong negative relationship, and +1 meaning a strong positive relationship. 
#The null hypothesis in a correlation test is a correlation of 0, which means no relationship at all.

#To run a correlation test between two variables x and y, use the cor.test() function. You can do this in one of two ways, 
#if x and y are columns in a dataframe, use the formula notation (formula = ~ x + y). If x and y are separate vectors 
#(not in a dataframe), use the vector notation (x, y).

# Is there a correlation between a pirate's age and the number of parrots (s)he's owned?

# Method 1: Formula notation
cor.test(formula = ~ age + parrots,data = pirates)

# Method 2: Vector notation
cor.test(x = pirates$age,y = pirates$parrots)

#Interpretation: 
#The value at the bottom of the output (cor) is the Pearson correlation coefficient r. We also report the p-value (significance).

##GOOD TO KNOW:
##The Pearson correlation coefficient is bound to certain prerequisites: For example, the variables for the Pearson correlation and 
##the significance test should ideally be normally distributed, metrically scaled and represent a linear relationship. 
##The Pearson correlation cannot be used, if the data set is ordinal. An alternative is the Spearman's rank correlation coefficient 
##or the  Kendall rank correlation coefficient.
##cor.test(x, y, method = "spearman")
##cor.test(x, y, method = "kendall")

#Visualizing relationships

#Scatterplot
#A Scatterplot displays the relationship between 2 numeric variables. Each dot represents an observation. 
#Their position on the X (horizontal) and Y (vertical) axis represents the values of the 2 variables. 
#With scatterplots we can read the linearity of the correlation as well as outliers.

#correlation between a pirate's age and the number of parrots (s)he's owned

pirates %>% 
  ggplot(aes(x=age, y=parrots, 
             color=sex, 
             shape=sex,)) +
  geom_point(size=4) + 
  ggtitle("Scatterplot: Age and Parrots") +
  xlab("Age of Pirate") + 
  ylab("Number of Parrots")

#correlation between a pirate's height and weight

pirates %>% 
  ggplot(aes(x=height, y=weight))+
  geom_smooth(color="#69b3a2") +
  geom_point(size=4) + 
  ggtitle("Scatterplot: Height and Weight") +
  xlab("Height of Pirate") + 
  ylab("Weight of Pirate")


#Correlation matrix
#A correlation matrix or correlogram allows to analyse the relationship between each pair of numeric variables 
#in a dataset. It gives a quick overview of the whole dataset. It is more used for exploratory purpose. 

install.packages("GGally")
library(GGally)

#Create a data frame with selected variables
subset <- pirates %>% select(age, weight, height, tattoos, parrots)

#Check correlation between variables
cor(subset)

# Visualization of correlations
ggcorr(subset, method = c("everything", "pearson")) 

#Comparison of two means 

#Independent sample t-Test 
#The t-test for independent samples tests whether different mean values exist for two unrelated 
#(independent) samples with respect to a dependent test variable.

#Assumptions:
#1) Assumption of Independence: you need two independent, categorical groups that represent your independent variable. 
#2) Assumption of normality: the dependent variable should be approximately normally distributed. 
#The dependent variable should also be measured on a continuous scale. 
#3)Assumption of Homogeneity of Variance: The variances of the dependent variable should be equal (-> Levene test).
#In case of unequal variances, the so-called Welch test or Welch t-test has to be calculated.

#Let’s test a prediction that pirates who wear eye patches have fewer tattoos on average than those who don’t wear eye patches.
#IV = eyepatch (0 or 1)
#DV = tattoos

#Check assumption of normality
pirates %>% 
  ggplot(aes(x=tattoos)) + 
  geom_histogram(fill="lightblue", color="white") + 
  ggtitle("Histogram: Number of Tattoos") +
  xlab("Number of tattoos") + 
  ylab("Frequency")

#Check assumption of Homogeneity of Variance: Levene test

#The Levene test tests (in the form of an F-test) on the basis of the F-distribution whether the variances of two or more groups are 
#homogeneous. The Levene test has the null hypothesis of variance homogeneity. The alternative hypothesis assumes variance heterogeneity. 
#Consequently, it is desirable in this test not to be able to reject the null hypothesis. This has the consequence that (approximately) 
#equal variances can be assumed and therefore parametric procedures such as the t-test for independent samples or the ANOVA can be 
#calculated. If, however, variance heterogeneity is given, there is still the possibility of a Welch test or a Welch ANOVA.

install.packages("car")
library(car)

leveneTest(pirates$tattoos, #x is our test variable
           pirates$eyepatch) #y is our group variable

#Interpretation: The Levene test is not significant - that's great! Now we can continue with the t-test. 
#We report the degrees of freedom, F-value, and significance: F(1,998) = 0.82, p = 0.37


#t-Test 

t.test(pirates$tattoos ~ pirates$eyepatch, #x is our test variable, y is our group variable
       var.equal = TRUE, #indicates whether we have equal or unequal variances - we checked this in advance with the Levene test!
       alternative = "two.sided") #indicates whether the test is one-sided or two-sided. One-sided means you know which group has a larger or smaller value, so there is a concrete assumption of effect.

#Interpretation:
#First, at the bottom of the output are the two group means. For group 0 = 9.608187 and for group 1 = 9.335866. 
#These two means are tested against each other.
#The p-value = 0.2253 is not significant, so the two tested groups do not differ significantly regarding their means. 
#In addition to the p-value, we report the t-statistic as well as the degrees of freedom: t(df)=t-statistic, p-value
#For our example: t(998)=1.21, p=0.26

#Calculate effect size: 
#If the effect is significant, it should be quantified. A p-value says nothing about the strength of a difference. 
#Therefore we typically use Cohens d. In the lsr package there is the function cohensD(). This is analogous to the t-test:

#install.packages("lsr")
#library(lsr)
#cohensD(x~y)

#Interpretation of effect size: 
#The output shows us a value for Cohens d, which we still have to interpret:
#from 0.2 -> small effect, from 0.5 -> medium effect, from 0.8 -> large effect







