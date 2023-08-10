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

###MULTIPLE LINEAR REGRESSION###

#Regression analyses are statistical analysis methods that aim to model relationships between a dependent variable and 
#one or more independent variables. They are used in particular when relationships are to be described quantitatively or 
#values of the dependent variable are to be predicted. Multiple linear regression is a statistical technique that attempts 
#to explain an observed dependent variable by multiple independent variables. Multiple linear regression represents a 
#generalization of simple linear regression. The word "linear" means that the dependent variable is modeled as a linear 
#combination (not necessarily) of linear functions of the independent variables. 

#In our diamond example, we want to understand how the value of a diamond relates to the three independent variables weight, clarity and color.
# y = value (DV)
# x = weight + clarity + color (ID) 

#For the testing of some assumptions, the regression model needs to be already calculated. Therefore, we start with the definition of the model.

#Definition of the linear model:
#In our example, we try to explain the value of diamonds by their weight, clarity and color. Consequently, the dependent (y) variable is the value 
#and the independent (x) variables aere the weight, clarity and color. 
#For multiple linear regression we also use the lm() function. 

model <- lm(value ~ weight+clarity+color, #A formula in the form y ~ x1 + x2 + x3 (...) where y is the DV, and x1, x2, x3 (..) are the IVs
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

diamonds %>% 
  ggplot(aes(x=color, y=value)) +
  geom_point(size=4) + 
  ggtitle("Scatterplot: Color and Value") +
  xlab("Diamond Color") + 
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
#In the output we can see that the test statistic is 2.0098 and the corresponding p-value is 0.922. 
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

#7) Absence of multicollinearity: Ideally, we want that the x-variables are moderately correlated with each other. 
#If the correlations are too high (greater than 0.9), this is a sign of multicollinearity. If this is the case, one could ask whether 
#two highly correlated x-variables do not measure the same thing. Omitting one of the two variables would therefore be conceivable.

#Correlation Matrix
#In the following R code, we’ll use the 
#function cor_mat() [rstatix package] to compute pairwise Pearson correlation coefficients between the x-variables:

diamonds %>% cor_mat(weight, clarity, color)

#In the output we see that no correlation is above 0.9 - so we do not assume multicollinearity. 

#Interpretation of the regression model 

#We use the summary() function to print the results of the calculation of "model".
summary(model)

#Significance of the regression model:
#To check whether the regression model is significant overall, an F-test is performed (F-statistic at the bottom of the output). This tests 
#whether the prediction of the dependent variable is improved by the addition of the independent variable. In other words, 
#the F-test checks whether the model as a whole makes an explanatory contribution.
#Our p-value is significant, meaning that the model as a whole is significant (F(3,146) = 85.49, p < .001). 
#For this reason, we continue with the analysis. If the model as a whole were not significant, we would not continue with the analysis.

#Significance of the regression coefficients
#Now we check whether the regression coefficients (betas) are also significant. A t-test is performed for each of the regression coefficients.
#The regression coefficients (here: weight, clarity and color) should be significant (p<0.05). Why? To avoid that the null hypothesis is falsely rejected.
#In our example, only weight and clarity are significant. 

#Comparison of the coefficients
#As you already know from the simple linear regression, we find the interpretable effect of the coefficient below "Estimate". In our example, 
#this is the value of the diamond by which the dependent variable changes when the independent variable increases by 1. 
#In general, positive coefficients have a positive influence on the y variable and negative coefficients have a negative influence.

#Now we would like to clarify the question which of the independent variables exerts a greater influence on the dependent variable. 
#However, since the value ranges of the independent variables are different, we cannot compare them. To do this, we need the standardized coefficients. 
#They can be obtained by z-standardizing all independent variables and the dependent variable used in the regression in advance using the scale() function. 

model <- lm(scale(value) ~ scale(weight)+scale(clarity)+scale(color), #A formula in the form y ~ x1 + x2 + x3 (...) where y is the DV, and x1, x2, x3 (..) are the IVs
            data=diamonds) 

summary(model)

#Nothing changes in the model and all result variables. Only the estimates of the independent variables change in this calculation.
#In our example, we now see that weight (0.5517) has a slightly stronger influence than clarity (0.5108). 

#Goodness of fit
#We identify the goodness of the model of the calculated regression by looking at R². The R² (Multiple R-Squared) is defined 
#by default between 0 and 1. R² indicates what percentage of the variance of the dependent variable (here: value) is explained.
#In our example, the model explains 36.6% of the variance, since the (Multiple R-sqaured) R²=0.6373. 
#The Adjusted R-squared adjusts for an automatic and unintended increase in the R². It is to be reported in addition to the normal R² 
#and is also always smaller.  



