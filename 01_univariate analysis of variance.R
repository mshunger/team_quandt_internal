#UNIVARIATE ANALYSIS OF VARIANCE

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

###ANOVA###

#With t-tests for independent samples we can test differences between two groups. With the analysis of variance (ANOVA) 
#we can test differences between more than two groups. Thus, we can use an ANOVA to test the influence of one or more categorial variables (IV)
#on a numeric dependent variable (DV). In this context, IVs are also referred to as factors, which have more than two characteristics. 
#Example: If the IV has three characteristics, we have three groups to be compared.

#Depending on the number of factors examined, a distinction is made between one-way ANOVA, two-way ANOVA, and multi-factor ANOVA.

###ONE-WAY ANOVA###

#Assumptions:
#1) Assumption of Independence: you need (more than) two independent, categorical groups that represent your independent variable. 
#2) Assumption of normality: the dependent variable should be approximately normally distributed. 
#The dependent variable should also be measured on a continuous scale. 
#3)Assumption of Homogeneity of Variance: The variances of the dependent variable should be equal (-> Levene test).

#Let’s test a prediction that female, male and other pirates differ in terms of their average weight. 
#IV = sex (male, female, other)
#DV = weight

#Check assumption of normality
pirates %>% 
  ggplot(aes(x=weight)) + 
  geom_histogram(fill="lightblue", color="white") + 
  ggtitle("Histogram: Weight of Pirate") +
  xlab("Weight of Pirate") + 
  ylab("Frequency")

#Check assumption of Homogeneity of Variance: Levene test

#The Levene test tests (in the form of an F-test) on the basis of the F-distribution whether the variances of two or more groups are 
#homogeneous. The Levene test has the null hypothesis of variance homogeneity. The alternative hypothesis assumes variance heterogeneity. 
#Consequently, it is desirable in this test not to be able to reject the null hypothesis. This has the consequence that (approximately) 
#equal variances can be assumed and therefore parametric procedures such as the t-test for independent samples or the ANOVA can be 
#calculated. If, however, variance heterogeneity is given, there is still the possibility of a Welch test or a Welch ANOVA.

install.packages("car")
library(car)

leveneTest(pirates$weight, #x is our test variable
           pirates$sex) #y is our group variable

#Interpretation: The Levene test is not significant - that's great! Now we can continue with the ANOVA. 
#We report the degrees of freedom, F-value, and significance: F(2,997) = 1.19, p = 0.30


#The one-factorial variance analytic model is estimated in R using the command aov.

anova <- aov(weight~sex, #The variables are specified in the formula notation according to the scheme DV ~ IV 
                     data = pirates) #the argument data is used to specify in which data.frame object these variables are to be found

#When we apply the summary command to the aov object, all the important information for the first interpretation is automatically displayed.
summary(anova)

#There is only one value that is really interesting here: the p-value, that can be found under Pr(>F). If the p-value is significantly 
#smaller than 0.05, the null hypothesis of equality of means across the groups can be rejected. We report this as F(2, 997) = 187, p < 0.001.

#The crucial question now is between which of the three groups a difference exists. It is possible that a difference exists only between 
#two groups or between all three groups. To answer this question, we need a post-hoc analysis.

#Post-hoc analysis: pairwise t-test

#For the post-hoc analysis, we perform pairwise t-tests using the pairwise.t.test() command. However, we need to adjust the p-value because 
#testing multiple times on the same sample leads to increased alpha error. But don't worry, R has a built-in function called p.adjust(). 
#There are several arguments for p.adjust(), usually we choose the most conservative "bonferroni". If we don't specify an argument, 
#the slightly less strict Holm method is used to correct.  

pairwise.t.test(pirates$weight,pirates$sex, p.adjust="bonferroni")

#The result is a small summary table containing only p-values. These are adjusted according to Bonferroni, as can be seen in the last line.
#In the table we can see that there is a significant difference between all three groups. 

#Effect size 
#The effect size f is not provided by R. f indicates how strong the detected statistically significant effect of the ANOVA is.

#To calculate the f value, we first need Eta²
#The closer Eta² is to one, the higher is the explained proportion of the total deviation.
#The higher this proportion is, the stronger the overall effect is to be estimated.
#Eta² indicates how much % of the variance of the DV is explained by the IV.

install.packages("DescTools")
library(DescTools)

#In the package exists the function "EtaSq", which reads Eta² from our defined model.
EtaSq(anova)

#In the output we get two values: Once Eta² (eta.sq) and once the partial Eta² (eta.sq.part). The partial Eta² is only interesting in 
#case of an ANCOVA, because it separates influences of other variables. In the case of a one-way ANOVA, the values are always identical. 
#This is also the case here. 

#We now put Eta² i following formula to calculate f: 

sqrt(0.2728259 /(1-0.2728259))

#The f-value for the ANOVA is 0.6125247.

##GOOD TO KNOW
##According to Cohen (1988), the limits for effect size are .01 (small effect), .06 (medium effect), and .14 (large effect).

#Effect size of pairwise comparisons
#In addition to the effect size of the one-way ANOVA, we are particularly interested in the effect size of the pairwise comparisons to classify 
#the significant differences between the groups. There is no simple function for this in R. However, we can apply the dplyr package, 
#the rstatix package and the cohens_d() function: 

install.packages("dplyr")
library(dplyr)

install.packages("rstatix")
library(rstatix)

pirates %>%
  cohens_d(weight~sex) %>% 
  as.data.frame()

#Interpretation of effect size: 
#In the output we see the effect sizes as a value for Cohens d in the "effsize" column for all three pairwise comparisons.
#We still have to interpret d, as a remider: from 0.2 -> small effect, from 0.5 -> medium effect, from 0.8 -> large effect

###TWO-WAY ANOVA###

#In the one-way ANOVA, we looked at the effect of a single categorical variable on a metric dependent variable. 
#However, it is possible to examine the effect of multiple categorical variables on a metric dependent variable simultaneously. 
#And that's not all - we can also look at interactions between these categorical IVs.

#Just like for a one-way ANOVA, we first check the assumptions and then use the aov function to estimate the model.

anova_two <- aov(weight~sex+eyepatch+sex*eyepatch,#In addition to the second variable, we also include the interaction between the IV and DV in the formula. 
                 data=pirates)

#To proceed with the two-way ANOVA, we still need to test the normality of residuals as an additional assumption. 
#For this purpose, we use a Q-Q plot. If the distribution of the residuals within the plot is linear, 
#we can assume normally distributed residuals. 

plot(anova_two, 2) #the 2 in this line indicates, that we want to have a Q-Q plot

#When we apply the summary command to the aov object, all the important information for the first interpretation is automatically displayed.

summary(anova_two)

#Interpretaion
#In our case, neither the interaction of the two categorical variables nor the eyepatch variable newly included in the model is significant. 
#For significant results: Just as with the one-way ANOVA, we can find more about which groups differ using post-hoc analysis 
#with pairwise t-test. 

###ANCOVA###

#Analysis of variance with covariates (ANCOVA for short), like ANOVA, tests independent samples to determine whether the 
#mean values of a dependent variable are different in more than two independent samples. However, it also tests another very probable 
#influencing factor (the covariate).

#In addition to the question of how female, male and other pirates differ in terms of weight, we now also want to control for the influence of 
#the average number of cups of grogg consumed per day. We therefore include the variable grogg as a covariate in our model. 

#Assumptions:
#In addition to the assumptions of an ANOVA, we need to check some specific assumptions before performing the ANCOVA: 

#1) The covariate should be similar/homogeneous across groups.
#First, a simple ANOVA should be calculated to check whether the covariate is similar across groups.

#Levene-test 
leveneTest(pirates$grogg, #x is our test variable
           pirates$sex) 

#The one-factorial variance analytic model is estimated in R using the command aov.
anova_ass1 <- aov(grogg~sex,  
             data = pirates)

#When we apply the summary command to the aov object, all the important information for the first interpretation is automatically displayed.
summary(anova_ass1)

#Interpreation of result:
#The one-way ANOVA is not significant - that's great! :) Because: as long as the significance is above 0.05, we can't reject the 
#null hypothesis of homogeneity. Consequently, we can assume homogeneity of the covariate across groups. Or in other words: 
#the average number of grogg cups is roughly the same in both groups, which means that no bias is to be expected. 
#The first special assumption for the ANCOVA is thus fulfilled.

#2) Homogeneity of the regression slopes: 
#The slopes of the regression lines, formed by the covariate and the outcome variable, should be the same for each group. 
#This assumption evaluates that there is no interaction between the outcome and the covariate.
#This can be done graphically, or you can also run the ANCOVA again and include the interaction between the independent variable 
#and the covariate in the model. If this interaction is significant, we can't assume homogeneity of the regression slopes!

anova_ass2 <- aov(weight~sex*grogg,
                 data=pirates)

summary(anova_ass2)

#Interpretation
#There is homogeneity of regression slopes as the interaction term is not statistically significant. 

#ANCOVA model
#The order of variables matters when computing the ANCOVA: You want to remove the effect of the covariate first (control for it),
#prior to entering your main variable or interest.

ancova <- aov(weight~grogg+sex,
                  data=pirates)

summary(ancova)

#Interpretation: 
#After the adjustment for cups of grogg per day, there is still a statistically significant difference in the weight between the groups. 
#Just as with the one-way ANOVA, we can find more about which groups differ using post-hoc analysis 
#with pairwise t-test. 













