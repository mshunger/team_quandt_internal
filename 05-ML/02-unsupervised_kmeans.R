library(dplyr)
library(ggplot2)

df_run <- iris %>% 
  select(Petal.Length, Petal.Width, Sepal.Length, Sepal.Width)
df_run <- as.matrix(df_run)

# k-means clustering initializes random group membership and iteratively
# assigns members to groups based on feature means until only little/no 
# reassignment can happen
mod <- kmeans(df_run, centers = 3, nstart = 20)
summary(mod)

plot(df_run, 
     col = mod$cluster)

# the core evaluation criterion for k-means clustering is the "within cluster
# sum of squares" which we are trying to minimize. This way we can compare 
# multiple solutions (e.g. for "k")

# run the algorithm with 1, 2, ..., 10 clusters
ks <- 1:10
tot_within_ss <- sapply(ks, function(k) {
  cl <- kmeans(df_run, k, nstart = 20) # actual algorithm
  cl$tot.withinss # extract within cluster sum of squares
})
# plot results
plot(ks, tot_within_ss, type = "b")

# -> "elbow" criterion: the best solution is at the "elbow" of the distribution
# which interestingly is 2 here, 3 is however still a reasonable value and 
# odd numbers can sometimes be desirable to avoid extreme edge cases

# inspect the solution
iris$cluster <- mod$cluster
tab <- table(iris$Species, iris$cluster)
tab

# recode if necessary, 2 seems to be virginica and 3 seems to be versicolor
iris$c_spec <- iris$cluster %>% 
  recode('1'='versicolor', '2'='virginica', '3'='setosa')
tab <- table(iris$Species, iris$c_spec)
tab

# and out of interest, how accurate was the unsupervised model?
correct = 50 + 48 + 36
all = 150

accuracy <- correct/all
accuracy
# quite good for an out of the box unsupervised model :)

# -> back to the presentation, ramping up the complexity!