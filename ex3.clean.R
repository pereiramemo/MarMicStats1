############
#1.1)
############
# Make sure to write the path to the folder where bioenv-2 is. 
# You can use tab to auto complete and check if the path is correct.
bioenv3 <- read.csv("bioenv-3.csv",header=T,row.names=1)

# inspect the table 
head(bioenv3)

# compute the mean and the standard deviation
a.mean <- mean(bioenv3$a)
a.sd <- sd(bioenv3$a)

# compute the t statistic and the p-value 
t.stat <- (a.mean - 10 )/(a.sd/sqrt(length(bioenv3$a)))
t.stat
pt(t.stat, df=24,lower.tail=F )

# visualize the test
plot(seq(-4,4,by=0.1),dt(seq(-4,4,by=0.1),df=24),type="l")
abline(v=t.stat)

# do the test using a function from R 
t.test(bioenv3$a,mu=10,alternative="greater")

############
#1.2)
############

# compute the mean and the standard deviation
b.mean <- mean(bioenv3$b)
b.sd <- sd(bioenv3$b)

# compute the t statistic and the p-value   
t.stat <- (b.mean - 10 )/(b.sd/sqrt(length(bioenv3$b)))
t.stat
pt(t.stat, df=24,lower.tail=F )*2

# do the test using a function from R 
t.test(bioenv3$b,mu=10,alternative="two.sided")

############
#1.3)
############

# compute the t statistic and the p-value     
t.stat <- (a.mean - b.mean)/sqrt( (a.sd^2)/length(bioenv3$a) + (b.sd^2)/length(bioenv3$b)  )
t.stat
pt(t.stat,df=48,lower.tail=T)

# do the test using a function from R 
# how are the degrees of freedom computed?
t.test(bioenv3$a,bioenv3$b,alternative="less")
t.test(bioenv3$a,bioenv3$b,alternative="less", var.equal = TRUE)

############
#2)
############

# statistical power: the probability of accepting the alternative hypothesis (H1) when it is true.

# first install and load the package pwr
install.packages("pwr")
library("pwr")

# test 1.1)
pwr.t.test(n = 25, d=0.4, sig.level = 0.05 ,type="one.sample")

############
#3)
############

# confidence interval: range of values that act as good estimates of the unknown population parameter

set.seed(1)
mynorm.35.3.5 <- rnorm(35,3,5)

m <- mean(mynorm.35.3.5)
s <- sd(mynorm.35.3.5)

s.error = qnorm(0.975)*(s/sqrt(35))

up.lim <- m + s.error
low.lim <- m - s.error

up.lim
low.lim

# double check
x<-seq(0,6,by=0.01)
plot(x,dnorm(x,m,s/sqrt(35)),type="l")
abline(v=up.lim)
abline(v=low.lim)

pnorm(up.lim,m,s/sqrt(35))
pnorm(low.lim,m,s/sqrt(35))

###############
#4)
###############

# ANOVA: analysis of variance 
# If there are more than 2 samples, the ANOVA should be performed instead of pairwise t tests.
# ANOVA will consider all samples at the same time in order to determine if there is a significant difference.
# If the ANOVA shows a significant p-value, then pairwise t tests or tukesy tests can be performed in order to identify the means that are significantly different.

set.seed(1)
norm.a <- rnorm(50,4,5)
norm.b <- rnorm(50,6,5)
norm.c <- rnorm(50,12,4.5)

groups = factor(rep(c("a","b","c"), each = 50))
mydata <- c(norm.a,norm.b,norm.c)

mydata.df <- data.frame(mydata,groups)

# Boxplots are a convenient way of graphically depicting groups of numerical data through their quartiles.
# The spacings between the different parts of the box indicate the degree of dispersion (spread) and skewness in the data, and show outliers. 
plot(mydata ~ groups)

# boxplot example
par(mfrow=c(2,1),mar=c(1,1,1,1))
boxplot(mynorm.1,horizontal = T)
hist(mynorm.1)

# test homogeneity of variance (assumption of the ANOVA)
bartlett.test(mydata,groups) # the null hypothesis is that the variances are homogeneous

# perform the ANOVA
a.results <- aov(mydata ~ groups,data=mydata.df)
summary(a.results)

# Df = degree of freedom
# Sum Sq = deviance (within groups, and residual)
# Mean Sq = variance (within groups, and residual)
# F value = the value of the Fisher statistic test, computed as (variance within groups) / (variance residual)
# Pr(>F) = p-value

# We clearly reject the null hypothesis of equal means for all three groups. 
# We have to find out which ones are different. 
# One way to do this is applying pairwise t-test 
pairwise.t.test(mydata, groups, p.adjust="bonferroni")

# The Bonferroni correction is based on the idea that if an experimenter is testing m hypotheses,
# then one way of maintaining the family - wise error rate (FWER) is to test each individual hypothesis 
# at a statistical significance level of 1/m times what it would be if only one hypothesis were tested.
# So, if the desired significance level for the whole family of tests should be (at most) alpha 
# then the Bonferroni correction would test each individual hypothesis at a significance level of alpha/m.

# Other option is applying the function TukeyHSD().
TukeyHSD(a.results, conf.level=0.95)

###############
#5) chi squared and fisher's exact tests
###############

# these are used to test the correspondence between categorical variables
# first the variables have to be converted into a contingency table

bioenv3$Biofilm[bioenv3$Biofilm=="Present"]="present"
bioenv3 <- droplevels(bioenv3)

chisq.test(table(bioenv3$Salinity,bioenv3$Biofilm))
fisher.test(table(bioenv3$Salinity,bioenv3$Biofilm))

###############
#6)
###############

# test by randomization if the difference between two samples differ. 

# generate two different, non normal the samples
set.seed(1)
p.comb <- c(rpois(50,3),rpois(50,4))

# create the null hypothesis distribution, which states that there is no difference between the means. 
rand <- vector (mode="numeric",length=10000)

for (i in 1:length(rand)) {
  tmp <- sample(p.comb) 
  rand[i] <- mean(tmp[1:50]) - mean(tmp[51:100])
}

par(mfrow=c(1,1))
hist(rand)

# see where the real difference falls under the null hypothesis
r.stat <- mean(p.comb[1:50]) - mean(p.comb[51:100])
r.stat
sum(rand<=r.stat)/length(rand)
abline(v=r.stat)

