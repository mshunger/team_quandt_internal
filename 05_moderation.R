
###DATA###

#We use a dataset in the yarrr package called diamonds. The dataset includes data on 150 diamonds sold at an auction. 

# Install the yarrr package
install.packages('yarrr')

# Load the package
library(yarrr)

##create a data.frame 

diamonds <- diamonds

#Exploring the data 

# Look at the first few rows of the data
head(diamonds)

# What are the names of the columns?
names(diamonds)

# View the entire dataset in a new window
View(diamonds)

###MODERATION####

#A moderation has a weakening or strengthening influence of an additional variable M (= moderator) on a relationship 
#between an independent variable (X) and a dependent variable (Y).

#In our diamond example, we want to test the assumption: 
#The positive influence of weight on the value of the diamond is increased by a higher value of clarity. 
# y = value (DV)
# x = weight (ID)
# m = clarity (M) 

#For the testing of some assumptions, the model needs to be already calculated. Therefore, we start with the definition of the model.

#Definition of the model:
#Statistically, the interaction term is formed by multiplying the independent variable (X) and moderator (M) (=X*M). 
#However, X and M are still included in the model as independent variables. Therefore, we define a model wiht the three independet 
#variables X, M und X*M. With the definition of a product in the formula, its factors are automatically included. 
#So, in the end, you only write the interaction term into the model. We use the lm() function: 

model <- lm(value ~ weight*clarity, #y-variable and interaction term with x-variable and moderator variable
            data=diamonds) #data frame

#Assumptions:
#1) The dependent and independent variables are interval scaled.
#2) No outliers: Outliers are a problem for most parametric statistical methods. A single outlier can already nullify an otherwise significant trend.
#3) Linearity of the relationship: A linear relationship is modeled between the dependent and independent variables.
#We check this assumption with a scatter plot: 

diamonds %>% 
  ggplot(aes(x=weight, y=value)) +
  geom_point(size=4) + 
  ggtitle("Scatterplot: Weight and Value") +
  xlab("Diamond Weight") + 
  ylab("Diamond Value")

diamonds %>% 
  ggplot(aes(x=clarity, y=value)) +
  geom_point(size=4) + 
  ggtitle("Scatterplot: Clarity and Value") +
  xlab("Diamond Clarity") + 
  ylab("Diamond Value")

#4) Independence of the residuals: 
#If the residuals are not independent, we call this an autocorrelation.

#To check this assumption, we perform a Durbin-Watson test using the durbinWatsonTest() function from the car package:

install.packages("car")
library(car)

#perform Durbin-Watson test
durbinWatsonTest(model)

#Interpreation:
#This test uses the following hypotheses:
#null hypothesis: There is no correlation among the residuals.
#alternative hypothesis: The residuals are autocorrelated.
#In the output we can see that the test statistic is 1.968049 and the corresponding p-value is 0.836. 
#Since this p-value is above 0.05, we can assume the null hypothesis and conclude that the residuals in this 
#regression model are not autocorrelated.

#5) Homoscedasticity (equality of variances) of the residuals:
#Similar to other statistical models, multiple linear regression expects the variance of the residuals to be equal. 
#If this condition is violated, the model makes more accurate predictions for one section of the data than for another.

#The simplest and fastest way to check for Homoscedasticity in a regression in R is graphically. 
#To do this, you can display a scatter plot that contains the predicted values and the residuals.

plot(model, 1)

#Interpretation:
#At first, we only look at the dispersion of the points. Since in the diagram no increase or decrease of the dispersion 
#is recognizable, i.e. we can't recognize a funnel opened to the left or to the right, we would rather assume no heteroskedasticity 
#here at first.The diagram helps additionally with a red line, which should be as straight as possible. If it is wavy or 
#has a positive or negative slope, we would definitely speak of heteroskedasticity.

#Analytical detection of heteroskedasticity
#For this purpose, we use either the White test or the Breusch-Pagan test. The former is more often used for slight violations of 
#the normal distribution assumption. We use the package "lmtest". In the function bptest() we insert the model name.

install.packages("lmtest")
library(lmtest)

bptest(model)

#Interpretation: 
#The Breusch-Pagan test tests the null hypothesis of homoskedasticity. A significant p-value rejects this 
#and accepts the alternative hypothesis of heteroskedasticity. In our example, the p-value is not significant, 
#thus we can assume homoscedasticity of the residuals.

#6) Normal distribution of the residuals:
#The residuals should not only be independent and homoscedastically distributed, but also (approximately) normally distributed. 
#Normally distributed residuals allow a more valid interpretation of the results and a reliable calculation of confidence intervals.

#As part of the calculation of the linear model, R automatically calculates standardized and non-standardized residuals and stores 
#them in the model summary. These must now be checked for normal distribution. We can check them with

rstandard(model)
#or
residuals(model)

#but we continue to use them in the context of a histogram and Q-Q diagram.

#Histogram 

hist(rstandard(model))
hist(residuals(model))

#Q-Q Plot
#we get the Q-Q-diagram with the qqnorm() command, in which we again insert rstandard(model):

qqnorm(rstandard(model))

#Perfect normal distribution would exist if an origin straight line could be recognized. 
#In order to be able to recognize it more easily, we can plot an origin line with the function qqline().

qqline(rstandard(model))

#Shapiro-Wilk test
#The Shapiro-Wilk test assumes in its null hypothesis that there is normal distribution of the data. 
#Our goal is therefore not to reject the null hypothesis. For this it is necessary that the p-value is above 0.05.

shapiro.test(rstandard(model))

#7) Absence of multicollinearity: Ideally, we want that the independent variables are moderately correlated with each other. 
#If the correlations are too high (greater than 0.9), this is a sign of multicollinearity. If this is the case, one could ask whether 
#two highly correlated x-variables do not measure the same thing. Omitting one of the two variables would therefore be conceivable.

#Compute pairwise Pearson correlation coefficients between the x and m variable. 
#In the following R code, we’ll use the function cor_test() [rstatix package]. 
#If you have more than two outcome variables, consider using the function cor_mat():

cor.test(diamonds$weight, diamonds$clarity)

#In the output we see that no correlation is above 0.9 - so we do not assume multicollinearity.

#Interpretation of the model 

#We use the summary() function to print the results of the calculation of "model".
summary(model)

#Significance of the regression model:
#To check whether the regression model is significant overall, an F-test is performed (F-statistic at the bottom of the output). This tests 
#whether the prediction of the dependent variable is improved by the addition of the independent variable. In other words, 
#the F-test checks whether the model as a whole makes an explanatory contribution.
#Our p-value is significant, meaning that the model as a whole is significant (F(3,146) = 85.34, p < .001). 
#For this reason, we continue with the analysis. If the model as a whole were not significant, we would not continue with the analysis.

#Goodness of fit
#We identify the goodness of the model of the calculated regression by looking at R². The R² (Multiple R-Squared) is defined 
#by default between 0 and 1. R² indicates what percentage of the variance of the dependent variable (here: value) is explained.
#In our example, the model explains 63.7% of the variance, since the (Multiple R-sqaured) R²=0.6368. 
#The Adjusted R-squared adjusts for an automatic and unintended increase in the R². It is to be reported in addition to the normal R² 
#and is also always smaller. 

#Interaction effect
#If the interaction effect is significant, the effects that make it up (weight and clarity) should no longer be interpreted individually. 
#It has been identified that their interaction is significant, and therefore the isolated effect should no longer be considered.
#If the interaction effect is NOT significant, the effects that compose it can be interpreted as usual. 
#Alternatively, the model can be recalculated without the interaction effect and interpreted as usual.

#Create interaction diagram

#For the graphical representation the package interactions can be used. 
install.packages("interactions")
library(interactions)

#In the interactions package, the interact_plot() function can now be used. The name of the calculated model (here: model), 
#the independent variable (pred) and the moderator (modx) are used:

interact_plot(model=model, pred = weight, modx = clarity)

#Interpretation of the plot
#Generally, the higher the weight (x-axis), the higher the value of the diamond (y-axis).The clarity is plotted in increasing order 
#with I) mean value minus standard deviation, II) mean value and III) mean value plus standard deviation in three different straight lines. 
#See also the legend at the right edge of the graph. The lowest line represents low clarity and the top line represents high clarity. 
#In our case, the slopes of the straight lines do not differ. If moderation were present, the upper straight line (high clarity) 
#would have a steeper slope than the lower straight line.  


