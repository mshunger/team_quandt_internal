#Pirates like diamonds. But as much as pirates love diamonds, they hate getting ripped off. 
#For this reason, a pirate needs to know how to accurately assess the value of a diamond. For example, 
#how much should a pirate pay for a diamond with a weight of 2.0 grams, a clarity value of 1.0, and a color 
#gradient of 4 out of 10? To answer this, we’d like to know how the attributes of diamonds 
#(e.g.; weight, clarity, color) relate to its value. We can get these values using linear regression.

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

###SIMPLE LINEAR REGRESSION###

#A simple linear regression analysis aims to explain a dependent variable (y) with an independent variable (x).
#In the context of regression analyses, the dependent variable is also referred to as the "criterion variable" 
#and the independent variable as the "predictor variable".

#In our diamond example, we want to know: How does the weight influence the value of the diamond? 
# y = value (DV)
# x = weight (ID)

#For the testing of some assumptions, the regression model needs to be already calculated. Therefore, we start with the definition of the model.

#Definition of the linear model:
#In our example, we try to explain the value of diamonds by their weight. Consequently, the dependent (y) variable is the value 
#and the independent (x) variable is the weight. 
#For simple linear regression we use the lm() function. lm stands for linear model.

model <- lm(value ~ weight, #y=value, x=weight 
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
#In the output we can see that the test statistic is 1.891715 and the corresponding p-value is 0.508. 
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


#Interpretation of the regression model 

#We use the summary() function to print the results of the calculation of "model".
summary(model)

#Significance of the regression model:
#To check whether the regression model is significant overall, an F-test is performed (F-statistic at the bottom of the output). This tests 
#whether the prediction of the dependent variable is improved by the addition of the independent variable. In other words, 
#the F-test checks whether the model as a whole makes an explanatory contribution.
#Our p-value is significant, meaning that the model as a whole is significant (F(1,148) = 85.52, p < .001). 
#For this reason, we continue with the analysis. If the model as a whole were not significant, we would not continue with the analysis.

#Significance of the regression coefficients
#Now we check whether the regression coefficients (betas) are also significant. A t-test is performed for each of the regression coefficients.
#The regression coefficient (here: weight) should be significant (p<0.05). Why? To avoid that the null hypothesis is falsely rejected.
#The significance is 2.36e-16 and thus the weight has a significant effect on the value of the diamond. 
#Below "Estimate" you can see the interpretable effect of this coefficient. In our example, this is the value of the diamond by which 
#the dependent variable changes when the independent variable increases by 1. 
#In general, positive coefficients have a positive influence on the y variable and negative coefficients have a negative influence.

#Goodness of fit
#We identify the goodness of the model of the calculated regression by looking at R². The R² (Multiple R-Squared) is defined 
#by default between 0 and 1. R² indicates what percentage of the variance of the dependent variable (here: value) is explained.
#In our example, the model explains 36.6% of the variance, since the (Multiple R-sqaured) R²=0.3662. 
#The adjusted R² (Adjusted R-squared) plays no role in a simple linear regression and is only used in a multiple linear regression.





