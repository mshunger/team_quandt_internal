#MULTIVARIATE ANALYSIS OF VARIANCE

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

###MANOVA###

#The Multivariate Analysis Of Variance (MANOVA) is an ANOVA with two or more continuous outcome variables.
#The one-way MANOVA tests simultaneously statistical differences for multiple response variables by one grouping variables.

#Why don't we just calculate multiple ANOVAs?
#Statistical reason: When analyzing multiple dependent variables (indicators) simultaneously by one or more factors, 
#calculating a MANOVA avoids alpha-error inflation.
#Content reason: Additional insights regarding the set of dependent variables considered can be derived from the results.
#Are two or more (sub)samples different from each other? Which factor can claim the highest explanatory power?

#Suppose we examine three groups and measure each participant on four dependent variables. 
#The MANOVA would give an overall F and overall significance level for the difference between the three groups in terms 
#of how much they differ on the combination of the four dependent variables.

#If we find an overall significant difference between the groups with MANOVA, it means that the groups differ in the combination of the 
#dependent variables - somehow, somewhere. However, we only know that the groups differ, but not exactly which group it is. 
#We usually then want to know if the groups differ in some or all of the dependent variables considered individually. 
#Thus, a MANOVA is usually followed by a one-way analysis of variance (ANOVA) for each of the dependent variables, 
#which are then followed by pairwise t-tests if they are significant.

#In our example, we would like to know to what extent height and weight differ between the groups of female, male and other pirates. 
#IV = sex (female, male, other)
#DP = weight + height 

#Assumptions:
#1) Assumption of Independence: you need two independent, categorical groups that represent your independent variable. 

#2) Assumption of normality: the dependent variables should be approximately normally distributed. 
#The dependent variables should also be measured on a continuous scale. 

#Check assumption of normality: height 
pirates %>% 
  ggplot(aes(x=height)) + 
  geom_histogram(fill="lightblue", color="white") + 
  ggtitle("Histogram: Height of Pirates") +
  xlab("Height of Pirates") + 
  ylab("Frequency")

##Check assumption of normality: weight
pirates %>% 
  ggplot(aes(x=weight)) + 
  geom_histogram(fill="lightblue", color="white") + 
  ggtitle("Histogram: Weight of Pirates") +
  xlab("Weight of Pirates") + 
  ylab("Frequency")

#3) Absence of multicollinearity: Ideally, we want that the dependent variables are moderately correlated with each other. 
#If the correlation(s) are too high (greater than .9), this is a sign of multicollinearity. This is problematic for 
#the MANOVA calculation, as the variables effectively become redundant and do not explain any additional variance.

#Compute pairwise Pearson correlation coefficients between the outcome variable. In the following R code, we’ll use the 
#function cor_test() [rstatix package]. If you have more than two outcome variables, consider using the function cor_mat():

cor.test(pirates$weight, pirates$height)

#In our example, the assumption of absence of multicollinearity is not fulfilled - to show, how to compute a MANOVA, we continue at this point :-) 

#4)Assumption of Homogeneity of Variance: The variances of the dependent variables should be equal (-> Levene test).

leveneTest(pirates$weight, 
           pirates$sex)

leveneTest(pirates$height, 
           pirates$sex)

##Interpretation: The Levene test is not significant - that's great! Now we can continue with the MANOVA. 

#5)Assumption of Homogeneity of variance-covariance matrices: The Box’s M Test can be used to check the equality of covariance 
#between the groups. This is the equivalent of a multivariate homogeneity of variance.

#This can be evaluated using the Box’s M-test implemented in the rstatix package.

box_m(pirates[, c("weight", "height")], #dependent variables
      pirates$sex) #independet varibale

#Interpretation: The p-value of the test should be significant! 
#Note that, if you have balanced design (i.e., groups with similar sizes), you don’t need to worry too much about violation of the 
#homogeneity of variances-covariance matrices and you can continue your analysis.
#However, having an unbalanced design is problematic. Possible solutions include: 1) transforming the dependent variables; 
#2) running the test anyway, but using Pillai’s multivariate statistic instead of Wilks’ statistic.

#In our example, the test is not significant - but to show, how to compute a MANOVA, we continue at this point :-) 

#6) Multivariate normality:  The R function mshapiro_test( )[in the rstatix package] can be used to perform the 
#Shapiro-Wilk test for multivariate normality.

pirates %>%
  select(weight, height) %>%
  mshapiro_test()

#Interpretation: The test is not significant (p > 0.05), so we can assume multivariate normality.

#Compute MANOVA

#There are four different types of multivariate statistics that can be used for computing MANOVA. 
#These are: “Pillai”, “Wilks”, “Hotelling-Lawley”, or “Roy”.
#The most commonly recommended multivariate statistic to use is Wilks’ Lambda.
#However, Pillai’s Trace is more robust and is recommended when you have unbalanced design and 
#also have a statistically significant Box’s M result (as in our example).
#Note that, “Pillai” is the default in the R Manova() function [car package].

model <- lm(cbind(weight, height) ~ sex, pirates)
Manova(model, test.statistic = "Pillai")

#Interpretation: 
#There is a statistically significant difference between the three groups on the combined dependent variables weight and height.
#Report result: F(4, 1994) = 89.885, p < 0.0001

#Post-hoc tests
#A statistically significant one-way MANOVA can be followed up by univariate one-way ANOVA examining, 
#separately, each dependent variable. The goal is to identify the specific dependent variables that contributed 
#to the significant global effect.