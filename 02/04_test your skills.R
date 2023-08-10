###TEST YOUR SKILLS###


#1)
#Are male pirates significantly taller than female pirates? 
#Test this by conducting a t-test on the relevant data in the pirates dataset. 
pirates <- pirates
sub <- pirates %>% select(sex, height)

#(Hint: You’ll have to select just the female and male pirates and remove the ‘other’ ones using subset() or filter())


#2)
#Are pirates whose favorite pixar movie is Up are more or less likely to wear an eye patch than those whose 
#favorite pixar movie is Inside Out? Test this by conducting a chi-square test on the relevant data in the pirates dataset. 

#(Hint: Create a new dataframe that only contains data from pirates whose favorite movie is either Up or Inside Out. 
#Then do the test on this new dataframe.)


























###SOLUTION###

#1)

#Check assumption of normality
pirates %>% 
  ggplot(aes(x=height)) + 
  geom_histogram(fill="lightblue", color="white") + 
  ggtitle("Histogram: Height") +
  xlab("Height") + 
  ylab("Frequency")

#Filter and create new data frame

pirates_f_m <- pirates %>% filter(sex == "female"|sex == "male")

#Check assumption of Homogeneity of Variance: Levene test

leveneTest(pirates_f_m$height, pirates_f_m$sex)

#t-Test
t.test(pirates_f_m$height ~ pirates_f_m$sex, 
       var.equal = TRUE, 
       alternative = "two.sided") 

#2)

#Filter and create new data frame 

pirates_movie <- pirates %>% filter(fav.pixar == "Up"|fav.pixar == "Inside Out")

#Chi-square test

chisq.test(pirates_movie$fav.pixar, pirates_movie$eyepatch)

#Effect size: Cramer's V

cramersV(x=pirates_movie$fav.pixar, y=pirates_movie$eyepatch)














