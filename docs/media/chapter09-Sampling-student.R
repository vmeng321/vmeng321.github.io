####################
# chapter 09- demonstrating sampling variability 
# that result from different sampling methods
###################
library(tidyverse)

##############
### generate a population (of elementary school kids)
############
set.seed(100)
mean_height <- c(110, 120, 134, 140, 155, 163, 165)
#plot(mean_height)
prop_grade <- c(0.18, 0.18, 0.14, 0.16, 0.14, 0.1, 0.1)
(N <- sum(round(prop_grade*500)))
dd <- tibble(ID = 1:N, grade = (rep(1:7, times= rep(N*prop_grade))),
             `commute_time (min)`=rgamma(N, 6, 0.8))
dd <- mutate(dd, `height (cm)` = mean_height[grade]+rnorm(N,sd=5), grade = as_factor(grade))

#### plotting population distribution
par(mfrow=c(1,1))
(hist_commute <- hist(dd$`commute_time (min)`))
(hist_height <- hist(dd$`height (cm)`,breaks=seq(85,185, by=8), xlim=c(85, 185)))
(boxplot_height <- ggplot(dd, aes(y=`height (cm)`, x=(grade)))+ geom_boxplot())
(boxplot_commute <- ggplot(dd, aes(y=`commute_time (min)`, x=(grade)))+ geom_boxplot())

##############
# set some sampling parameters (sample size, etc)
##############
n = 100
B=1e3 # number of repeats

################################
#### simple random sampling
################################
# repeat the following 4 lines of code if you want to visualize the histogram
# of a new simple random sample.
dd_sample <- sample_n(dd, size=sum(round(prop_grade*n)))
par(mfrow=c(2,1))
hist(dd$`height (cm)`, freq=F, breaks=seq(85,185, by=8), xlim=c(85, 185), main="distribution of height in population")
hist(dd_sample$`height (cm)`, freq=F, breaks=seq(85,185, by=8), xlim=c(85, 185), main="distribution of height in sample")

#repeated SR sampling, calculate sample average each time.
ybar_height <- rep(NULL, B)
ybar_commute <- rep(NULL, B)
for(j in 1:B){
  dd_sample <- sample_n(dd, size=sum(round(prop_grade*n)))
  ybar_height[j] <- mean(dd_sample$`height (cm)`)
  ybar_commute[j] <- mean(dd_sample$`commute_time (min)`)
}

#########################
### stratified sampling
#########################
# repeat the following 9 lines of code if you want to visualize the histogram
# of a new stratified sample.
dd_sample_strat <- tibble()
for(i in 1:7){
  dd_sample_strat <-rbind(dd_sample_strat, dd |> 
                            filter(grade == i) |> 
                            sample_n(size=round(prop_grade[i]*n)))
}
par(mfrow=c(2,1))
hist(dd$`height (cm)`, freq=F, breaks=seq(85,185, by=8), xlim=c(85, 185), main="distribution of height in population")
hist(dd_sample_strat$`height (cm)`, freq=F, breaks=seq(85,185, by=8), xlim=c(85, 185), main="distribution of height in sample")


### repeated stratified sampling, calculating sample mean each time.
ybar_height_strat = rep(NULL, B)
ybar_commute_strat = rep(NULL, B)

for(j in 1:B){
  dd_sample_strat <- tibble()
  for(i in 1:7){
    dd_sample_strat <-rbind(dd_sample_strat, dd |> 
                              filter(grade == i) |> 
                              sample_n(size=round(prop_grade[i]*n)))
  }
  ybar_height_strat[j] <- mean(dd_sample_strat$`height (cm)`)
  ybar_commute_strat[j] <- mean(dd_sample_strat$`commute_time (min)`)

}


################################
### boxplot of sampling distribution of sample mean (of height)
par(mfrow=c(1,1))
mean_height_repeated <- tibble(sample_mean = c(ybar_height, ybar_height_strat), 
       type=rep(c("Simple random", "Stratified"), each=B))
boxplot(sample_mean~type, mean_height_repeated, main="Boxplot of sample mean (of height variable)", xlab="sampling method")
abline(h= mean(dd$`height (cm)`), col="green", lty=2, lwd=2)#overlay a line indicating population mean height

### boxplot of sampling distribution of sample mean (of commute time)
mean_commute_repeated <- tibble(sample_mean = c(ybar_commute, ybar_commute_strat), 
                                type=rep(c("Simple random", "Stratified"), each=B))
boxplot(sample_mean~type, mean_commute_repeated, main="Boxplot of sample mean (of commute time)", xlab="sampling method")
abline(h= mean(dd$`commute_time (min)`), col="green", lty=2, lwd=2)#overlay a line indicating population mean commute time


