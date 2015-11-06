#Practical class 2

############
#1)
############

# plot the density function for N(0,1)
xseq <- seq(-5,5,0.01)
xdnorm.0.1 <- dnorm(x,0,1)
plot(xseq,xdnorm.0.1,type="l",lwd=2.5)

# add the density function for T(2) in the plot
xdt.2 <- dt(xseq,df=2)
points(xseq,xdt.2,col="blue",type="l")

# add the density function for T(5) in the plot
xdt.5 <- dt(xseq,df=5)
points(xseq,xdt.5,col="blue",type="l")

# add the density function for T(5) in the plot
xdt.10 <- dt(xseq,df=10)
points(xseq,xdt.10,col="blue",type="l")

# add the density function for T(5) in the plot
xdt.30 <- dt(xseq,df=30)
points(xseq,xdt.30,col="blue",type="l")

# add the density function for T(5) in the plot
xdt.100 <- dt(xseq,df=100)
points(xseq,xdt.100,col="blue",type="l")


# As the sample size increases (or degrees of freedom), the variance from the t-student distribution decreases and approaches the standard normal distribution.
# See that the degrees of freedom are computed as n-1, where n is the sample size.

############
#2) 
############

binom.1K.20.5 <- rbinom(n=1000,size = 20,prob = 0.5)
hist(binom.1K.20.5)

set.seed(3)
binom.1K.50.5 <- rbinom(n=1000,size = 50,prob = 0.5)
hist(binom.1K.50.5,freq = F)

# The normal distribution can be used to approximate the binomial by defining the mean as n*p and variance as n*p*(1-p), 
# where n is the number of trials and p the probability of success. 
# Do not confuse the number of trials with the sample size, which can also be represented by the letter n.
# Note: the normal distribution is continuous and the binomial is discrete. 
# Note: the normal distribution can be used as an approximation when n*p > 5 and n*p* (1-p) > 5.

# binom.1K.50.5 distribution will be used for the approximation

binmo.1K.50.5.mean <- 50*0.5
binmo.1K.50.5.sd <- sqrt(50*0.5*0.5)

xseq<-seq(0,50,0.1)
norm.approx <-  dnorm(xseq,binmo.1K.50.5.mean,binmo.1K.50.5.sd)

lines(xseq,norm.approx,col="red")

############
#3)
############

# The lambda parameter of the Poisson distribution is both the expected value and the variance
# Note: the normal distribution is continuous and the Poisson is discrete. 
# Note: the normal distribution can be used as an approximation when lambda is bigger than 10.

# Overlay histograms
hist(rpois(1000,12),xlim=c(0,30),ylim=c(0,250),col=rgb(0,0,0.5,0.5))
hist(rnorm(1000,12,sqrt(12)),add=T,col=rgb(0.5,0,0,0.5))

############
#4)
############


pnorm(1.96,0,1)

pt(1.96,2)
pt(1.96,5)
pt(1.96,10)
pt(1.96,30)
pt(1.96,100)
pt(1.96,1000000)

#5)
MyTableBio<-read.csv("~/Dropbox/Doctorado/CoursesAndWorkshops/MarMic_Statistics/bioenv-2.csv",header=T,sep=",")


# case a vs temperature
#x<-MyTableBio$Temperature
#y <-  4 * x^3 + rnorm(150, 300, 300)
#y<-abs(y)
#x.df<-as.data.frame(round(x,1))
#y.df<-as.data.frame(round(abs(y),0))
#plot(x,y)
#plot(lm(y~x))

plot(MyTableBio$a,MyTableBio$Temperature)

#case b vs temperature


#MyTableBio$Temperature
#x<-MyTableBio$Temperature
#y<- 4*x + 3 +rnorm(150,0,5)
#y.df<-as.data.frame(round(y,0))
#plot(x,y)
#plot(lm(y~x))

plot(MyTableBio$b,MyTableBio$Temperature)

#case c vs temperature

#y<-x
#x<-MyTableBio$Temperature
#quantile(x)
#sum(x<=8.9)
#y[x<=9.3]=rnorm(sum(x<=9.3),10,3)
#y[x>9.3]=rnorm(sum(!x<=9.3),100,10)
#y.df<-as.data.frame(round(y,0))

plot(MyTableBio$Temperature,MyTableBio$c)

#case d vs temperature

#x<-MyTableBio$Temperature
#y <- 5*x + rnorm(150,3,2)
#y.df<-as.data.frame(round(y,0))
#y[x>9.9] <- rnorm(sum(x>9.9),120,4)
#plot(x,y)

plot(MyTableBio$Temperature,MyTableBio$d)

smlc<-lm(MyTableBio$d~MyTableBio$Temperature)
sum(var(cbind(MyTableBio$c,MyTableBio$Temperature)))
var(residuals(smlc))
var(fitted(smlc))

####################

#6)

#### a
plot(MyTableBio$Temperature,MyTableBio$a)
slm.a_vs_t <- lm(MyTableBio$a~MyTableBio$Temperature)
abline(slm.a_vs_t,col="red",lwd=2)

hist(residuals(slm.a_vs_t))
plot(fitted(slm.a_vs_t),residuals(slm.a_vs_t))

nq <- quantile(dnorm(seq(-3.5,3.5,by=0.1),0,1),probs=seq(0,1,by=0.01))
eq <- quantile(residuals(slm.a_vs_t),probs=seq(0,1,by=0.01))
plot(nq,eq)

#### b
plot(MyTableBio$Temperature,MyTableBio$b)
slm.b_vs_t <- lm(MyTableBio$b~MyTableBio$Temperature)
abline(slm.b_vs_t,col="red",lwd=2)

hist(residuals(slm.b_vs_t))
plot(fitted(slm.b_vs_t),residuals(slm.b_vs_t))
text()

nq <- quantile(dnorm(seq(-3.5,3.5,by=0.1),0,1),probs=seq(0,1,by=0.01))
eq <- quantile(residuals(slm.b_vs_t),probs=seq(0,1,by=0.01))
plot(nq,eq)

##### c
plot(MyTableBio$Temperature,MyTableBio$c)
slm.c_vs_t <- lm(MyTableBio$c~MyTableBio$Temperature)
abline(slm.c_vs_t,col="red",lwd=2)

hist(residuals(slm.c_vs_t))
plot(fitted(slm.c_vs_t),residuals(slm.c_vs_t))

nq <- quantile(dnorm(seq(-3.5,3.5,by=0.1),0,1),probs=seq(0,1,by=0.01))
eq <- quantile(residuals(slm.c_vs_t),probs=seq(0,1,by=0.01))
plot(nq,eq)


##### d
plot(MyTableBio$Temperature,MyTableBio$d)
slm.d_vs_t <- lm(MyTableBio$d~MyTableBio$Temperature)
abline(slm.d_vs_t,col="red",lwd=2)

hist(residuals(slm.d_vs_t))
plot(fitted(slm.d_vs_t),residuals(slm.d_vs_t))

nq <- quantile(dnorm(seq(-3.5,3.5,by=0.1),0,1),probs=seq(0,1,by=0.01))
eq <- quantile(residuals(slm.d_vs_t),probs=seq(0,1,by=0.01))
plot(nq,eq)


#7)
slm<-lm(MyTableBio$a~MyTableBio$Temperature)
summary(slm)

r <- var(residuals(slm))
f <- var(fitted(slm))
t <- var(MyTableBio$d)

(f/(r +f))

var(residuals(slm))*(150-1) + var(fitted(slm))*(150-1)

