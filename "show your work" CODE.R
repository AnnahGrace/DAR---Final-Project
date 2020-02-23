#========================================================================
#===============================INDEX====================================
#========================================================================
#1.) Working Directory and File Storage
#2.) Data Simulation
  #a.) numeric continuous - rnorm()
  #b.) numeric continuous - runif()
  #c.) numeric discrete - rpois()
  #d.) charecter - rep() and letter[]
  #e.) generating data sets
  #f.) repeatedly generate data sets - replicate()
#3.) Playing with t-tests
#4.) Playing with ANOVA
#5.) Calculating effect size
#6.) Making More Efficent Code

#============================WD and File Storage=========================

#get working directory
wd <- getwd()
wd

# folders for storing data outputs and figures
# store names of the folders in an object
output.folder.names <- c("figures", "data.output")
# and make the folders if they don't exist yet. 
for(i in 1:length(output.folder.names)) 
  if(file.exists(output.folder.names[i])== FALSE) 
    dir.create(output.folder.names[i])

#path to figures folder
path.figures <- paste(wd,"/",output.folder.names[1],"/", sep= "")

#path to data output folder
path.data.output <- paste(wd,"/",output.folder.names[2],"/", sep= "")


#===============================Data Simulation=============================
#--------------rnorm()-----------------

#numeric continous

#rnorm generates random numbers in a normal distribution
#rnorm(n, mean= x, sd= y)
#rnor(n) assumes a standard normal distribution (mu=0, SD=1)
rnorm(5)
rnorm(5, mean= 50, sd= 5)

#set.seed() for creating a reproducible, randomly generated vector
#example with 2 seeds
set.seed(16)
rnorm(5)

set.seed(7)
rnorm(5)

set.seed(16)
rnorm(5)

set.seed(7)
rnorm(5)

#pull numbers from 3 distribution with different means (and place in 1 vector)
rnorm(n= 10, mean= c(0, 5, 20), sd= 1)

#now vary the sd as well
rnorm(n= 10, mean= c(0, 5, 20), sd= c(1, 5, 20) )

#use rnorm() to run a test
#check for coorilations between two randomly generated vectors
#(run whole sequence several times, just for fun)
x= rnorm(n= 10, mean= 0, sd= 1)
y= rnorm(n= 10, mean= 0, sd= 1)
plot(y ~ x)
cor(y, x)


#--------------runif()-----------------

#numeric continous

#runif() generates possitive, continuous numbers within a range
#runif(n, min= x, max= y)
#runif(n) assumes min= 0 and max= 1
runif(5)
runif(5, min= 50, max= 70)

#use runif() to run a test
#response variable= y, explanitory variable= x1
y= rnorm(n= 100, mean= 0, sd= 1)
x1= runif(n= 100, min= 1, max= 2)
head(x1)

#create a second explanitory variable (x2)
x2= runif(n= 100, min= 200, max= 300)
head(x2)

#fit to linear model
lm(y ~ x1 + x2)
#note that the coefficent for x2, which has a larger magnitude, is smaller
#the change in y for a "1-unit-increase" in x is reletive to the units/
#magnitude of x


#--------------rpois()-----------------

#numeric discrete

#rpois() generates random discrete integers from a poisson distribution
#rpois(n, lambda) (where lambda is the mean and cannot be left blacnk)
rpois(5, lambda= 2.5)

#explore the poisson distribution
y= rpois(100, lambda= 5)
summary(y)
hist(y)
#note that it is right skewed (in this case), not normal
#note also that the skew will be different everytime I/anyone runs this

#more exploration
y= rpois(100, lambda= 100)
summary(y)
hist(y)
#note that this disttribution is more symentrical (normal)


#--------------rep() and letters[]-----------------

#charecter

#letters[x:y] (or LETTERS[x:y] for capittals)
#print groups or levels labled with the letters of the alphabet
letters[1:26]
LETTERS[3:17]
#note that if your range is larger than 26, you get NA's after 26

#repete each element of a vector n times with rep()
#when the second input is unspecified, it defaults to "times" (see below)
rep(1:10, 2)

#each= repeat each element n times in order of elements
rep(letters[1:2], each= 3)

#times= repeat the whole vector in order n times
rep(letters[1:2], times= 3)

#you can also use times to print each element a different (defined) n times
rep(letters[1:2], times= c(2, 4) )

#length.out= repeat each element (in order) until vector= length n
rep(letters[1:2], length.out= 5)

#combine each and times to print each element n times and then-
#repeat that vector n2 times
rep(letters[1:2], each= 2, times= 3)

#combine each and length.out to print each element n times and then-
#repete that pattern until a vector length of n2 is reached
rep(letters[1:2], each= 2, length.out= 7)


#--------------generating data sets-------------------------

#___________________________________________________________________________
###create a data frame with catagorical and numeric (continous) variables###
#in this data frame, there are no differences between groups (same mean and SD)
data.frame(group= rep(letters[1:2], each= 3),
           response= rnorm(6, mean= 0, sd= 1) )

#alternitive notation of the above (I switch to this as difficulty increases)
group <- rep(letters[1:2], each= 3)
response <- rnorm(6, mean= 0, sd= 1)
data.frame(group, response)

#add another catagorical variable to the dataset
#we will have a single observation for every pair of observations
#(the catagiries are "crossed")
data.frame(group= rep(letters[1:2], each= 3),
           factor= rep(LETTERS[3:5], times= 2),
           response= rnorm(6, mean= 0, sd= 1) )

#____________________________________________________________________________
###create a data frame with differnces between groups###
#(different mean and or SD)
response <- rnorm(6, mean= c(5, 10), sd= 1)
group <- rep(letters[1:2], length.out= 6)
data.frame(group, response)

#create a data frame with multiple numeric variables
data.frame(group <- rep(LETTERS[3:4], each= 10),
           x <- runif(20, min= 10, max= 15),
           y <- runif(20, min= 100, max= 150))
#wow, that is weird to look at in the consoul

#lets try to correct with = rather than <-
data.frame(group = rep(LETTERS[3:4], each= 10),
           x = runif(20, min= 10, max= 15),
           y = runif(20, min= 100, max= 150))


#-------------replicate()------------------------------

#repetedly generate data sets
#replicate(n, expr, simplify)
  #n= number of replications
  #expr= the expression to repeat
  #simplify= controls the type of output the resaults of expr are saved into
    #simplify= FALSE saves output into a list rather than an array
    #if you do not specify simplify, you will get a matrix

#simple example with list output (generate randon, normal values)
replicate(3, 
          expr= rnorm(5, mean= 0, sd= 1), 
          simplify= FALSE )

#simple example with matric output (generate randon, normal values)
replicate(3, expr= rnorm(5, mean= 0, sd= 1))
#I don't like this output

#example with list output (with a numeric and a catagorical variable)
simlist <- replicate(3, 
                    expr= data.frame(group = rep(letters[1:2], each = 3),
                                      response= rnorm(6, mean= 0, sd= 1) ),
                    simplify= FALSE)

simlist
str(simlist)

#here is the fist sim
simlist[[1]]



#============================Playing with t-tests=========================
#------------2 random samples with the same mean and SD-------------

#Create two random samples with the same means and- 
#variance
a <- rnorm(10)
b <- rnorm(10)

#null: a and b are from the same population/their means are not significantly-
#different
#alt: a and b are not from the same population/their means are significantly-
#different

t.test(a, b)
#t-score= -0.024
#df= 13.456
#p-value= 0.981

#with a p-value of 0.98, we fail to reject the null


#------------2 random samples with different mean and SD-------------

#create two random samples with different means and-
#the same SD
c <- rnorm(10, 10)
d <- rnorm(10, 15)

#hypothisies are the same as above
?rnorm
t.test(c, d)
#t= -11.919
#df= 13.508
#p-value= 1.505e-08

#how close can I get before I get a lot of error?
#I would guess larger than a 1 point difference (while variance is 1)
c <- rnorm(10, 10)
d <- rnorm(10, 13)
t.test(c, d)

c <- rnorm(10, 10)
d <- rnorm(10, 12)
t.test(c, d)

c <- rnorm(10, 10)
d <- rnorm(10, 11)
t.test(c, d)
summary(t.test(c, d))
#Still recognizing a difference most times (run multiple times)

#test how many times out of 100 we get a significant resault
for (i in 1:100) {
  c <- rnorm(10, 10)
  d <- rnorm(10, 11)
  guiness <- t.test(c, d)
  print(guiness$p.value)
}
#I cannot for the life of me get R to give me

#trying again
p.guiness <- data.frame(NA)
for (i in 1:100) {
  c <- rnorm(10, 10)
  d <- rnorm(10, 11)
  guiness <- t.test(c, d)
  print(guiness$p.value)
  p.guiness[i, 1] <- guiness$p.value
}
#let the record reflect that this took me 2 hours
#I spent a long time trying to learn how to use if/else and append()

#check how many are significant
sig <- p.guiness <= 0.05
length(which(sig == TRUE))
#NOTE: this spits out the number of cases wher the p-value is equal to or-
#less than 0.05 (indicating significance)
#In other words, it tells you how many cases out of 100 it reads as significant

#double checking
length(which(sig == FALSE))

#so, it recognizes a significant difference in means about half of the time

#let's try with some different difference in means
p.guiness <- data.frame(NA)
for (i in 1:100) {
  c <- rnorm(10, 10)
  d <- rnorm(10, 10)
  guiness <- t.test(c, d)
  print(guiness$p.value)
  p.guiness[i, 1] <- guiness$p.value
}

sig <- p.guiness <= 0.05
length(which(sig == TRUE))
#reads a significant differnce about 5% of the time (as we would expect-
#with a default CI of 95%)

#lets change the CI a few times

#CI= 90%
p.guiness <- data.frame(NA)
for (i in 1:100) {
  c <- rnorm(10, 10)
  d <- rnorm(10, 10)
  guiness <- t.test(c, d, conf.level = 0.9 )
  print(guiness$p.value)
  p.guiness[i, 1] <- guiness$p.value
}

sig <- p.guiness <= 0.05
length(which(sig == TRUE))

#CI= 50%
p.guiness <- data.frame(NA)
for (i in 1:100) {
  c <- rnorm(10, 10)
  d <- rnorm(10, 10)
  guiness <- t.test(c, d, conf.level = 0.5 )
  print(guiness$p.value)
  p.guiness[i, 1] <- guiness$p.value
}

sig <- p.guiness <= 0.05
length(which(sig == TRUE))
#why is this not changing the read of TRUE?

#let's make a function for space efficency
rapid <- function(mean1, mean2, CI){
  p.guiness <- data.frame(NA)
  for (i in 1:100) {
    c <- rnorm(10, mean1)
    d <- rnorm(10, mean2)
    guiness <- t.test(c, d, conf.level = CI )
    p.guiness[i, 1] <- guiness$p.value
  }
  sig <- p.guiness <= 0.05
  print(length(which(sig == TRUE)))
}

#more testing
rapid(10, 10, 0.5)
#why is this not changing the read of TRUE?

#testing what changing the CI does
#change the CI repetedly to test)
set.seed(1)
c <- rnorm(10, 10)

set.seed(2)
d <- rnorm (10, 10)
t.test(c, d, conf.level = 0.001)
#Changing the CI does nothing to the p-value when the means are the same

#testing with different means
rapid(10, 12, 0.8)
#changing the CI seems to have no effect here either

#expand the above
set.seed(1)
c <- rnorm(10, 10)

set.seed(2)
d <- rnorm (10, 12)
t.test(c, d, conf.level = 0.1)

#lets look at the t-test formula to understand this (check online)
#CI's have nothing to do with the formula...
#I just realized that they only effect how you interpret your p-value
#I kind of knew that but now I fully understand how arbitrary it is

#lets make another fuction so we can play with SD
rapid.SD <- function(mean1, mean2, SD1, SD2){
  p.guiness <- data.frame(NA)
  for (i in 1:100) {
    c <- rnorm(10, mean1, SD1)
    d <- rnorm(10, mean2, SD2)
    guiness <- t.test(c, d)
    p.guiness[i, 1] <- guiness$p.value
  }
  sig <- p.guiness <= 0.05
  print(length(which(sig == TRUE)))
}

#lets play with SD
rapid.SD(10, 10, 1, 15)  #5 sigs
rapid.SD(10, 10, 1, 20)  #8 sigs
rapid.SD(10, 10, 1, 100)  #5 sigs
#very little effect

#what about when our means are unequal
rapid.SD(10, 12, 1, 15)  #8 sigs
rapid.SD(10, 12, 1, 20)  #11 sigs
rapid.SD(10, 12, 1, 100)  #5 sigs
#still very little effect
#upon looking at the formula again, I would expect SD to have a noticible- 
#effect on significance only when my sample size is very large

#lets test that theory
rapid.SD(100, 100, 1, 1)  #8 sigs
rapid.SD(100, 100, 1, 15)  #8 sigs
rapid.SD(100, 100, 1, 20)  #4 sigs
rapid.SD(100, 100, 1, 100)  #2 sigs
rapid.SD(100, 100, 1, 200)  #2 sigs
#This is still not having a very significant effect

#Upon looking at the formula again, I feel like sample size and means are the-
#only things that will have a large effect on how many sigificant resaults-
#this thing pumps out

#lets test that
#function that alows me to choose my sample sizes and means
rapid.n <- function(n1, n2, mean1, mean2){
  p.guiness <- data.frame(NA)
  for (i in 1:100) {
    c <- rnorm(n1, mean1)
    d <- rnorm(n2, mean2)
    guiness <- t.test(c, d)
    p.guiness[i, 1] <- guiness$p.value
  }
  sig <- p.guiness <= 0.05
  print(length(which(sig == TRUE)))
}

#comense experimentation
rapid.n(10, 10, 10, 10) #10 sig
rapid.n(100, 100, 10, 10) #1 sig
#these are false possitives

rapid.n(10, 10, 10, 11) #61 sig
rapid.n(100, 100, 10, 11) #100 sig

rapid.n(10, 10, 10, 12) #99 sig
rapid.n(100, 100, 10, 12) #100 sig

rapid.n(10, 10, 10, 10.1) #9 sig
rapid.n(100, 100, 10, 10.1) #11 sig

rapid.n(10, 10, 10, 10.2) #9 sig
rapid.n(100, 100, 10, 10.2) #28 sig

rapid.n(10, 10, 10, 10.3) #12 sig
rapid.n(100, 100, 10, 10.3) #57 sig

rapid.n(10, 10, 10, 10.4) #14 sig
rapid.n(100, 100, 10, 10.4) #74 sig

rapid.n(10, 10, 10, 10.5) #26 sig
rapid.n(100, 100, 10, 10.5) #92 sig

#well that is pretty cool
#This shows that t-tests can detect smaller effect sizes with confidencewhen n-
#is larger



#============================Playing with ANOVA=========================
#------------------Try out one way ANOVA--------------------------------------

#create a data frame with groups a, b, and c where half of the values for each-
#fall between 1 and 50, and half fall within 50 and 100
groups <- rep(letters[1:3], length= 10)
e<- runif(15, 1, 50)
f<- runif(15, 50, 100)

#null: groups do not have the same mean
#alt: groups have the same mean

#create and check a data frame
aov <- data.frame(groups, c(e, f))
names(aov)[2]<-paste("numbers")
levels(aov$groups)

#visualize data
boxplot(numbers ~ groups, data = aov)

#compute an ANOVA
res.aov <- aov(numbers ~ groups, data = aov)
summary(res.aov)

#let's make a function thats spits out a count of how many resaults out of 100-
#trials have a significant p-value
rapid.aov <- function(min1, max1, min2, max2){
  p.aov <- data.frame(matrix(NA, ncol=1, nrow=30))
  names(p.aov)[1]<-paste("f-value")
  groups <- rep(letters[1:3], length= 10)
  for (i in 1:100) {
    e<- runif(15, min1, max1)
    f<- runif(15, min2, max2)
    aov <- data.frame(groups, c(e, f))
    names(aov)[2]<-paste("numbers")
    res.aov <- aov(numbers ~ groups, data = aov)
    p.aov[i, 1:2] <- summary(res.aov)[[1]][["Pr(>F)"]]
  }
  sig <- p.aov <= 0.05
  print(length(which(sig == TRUE)))
}

#how many sigs will I get in a one-way ANOVA where half of the values for each-
#falls between 1 and 50, and half fall within 50 and 100?
rapid.aov(1, 50, 50, 100) #7 sigs

#lets play with range size
rapid.aov(1, 500, 1, 500) #7 sigs
rapid.aov(1, 50, 1, 50) #6 sigs
rapid.aov(1, 10, 1, 10) #3 sigs
rapid.aov(1, 5, 1, 5) #3 sigs
#when range1 (min1-max1) and range 2 (min2-max2) are the same, it reads-
#significance a mean of about 5 time out of 100 trials (what we would expect-
#with a 95% CI)

#lets play with different ranges
rapid.aov(1, 5, 5, 10) #0 sigs
rapid.aov(1, 5, 10, 15) #0 sigs
rapid.aov(1, 10, 1, 20) #1 sigs
rapid.aov(1, 10, 15, 35) #0 sigs
rapid.aov(400, 500, 500, 600) #0 sigs
#this is reading as no significant differnce... lets look at the data-
#tto figure out why
rapid.aov.box <- function(min1, max1, min2, max2){
  p.aov <- data.frame(matrix(NA, ncol=2, nrow=30))
  names(p.aov)[1]<-paste("p-value")
  groups <- rep(letters[1:3], length= 10)
  for (i in 1:100) {
    e<- runif(15, min1, max1)
    f<- runif(15, min2, max2)
    aov <- data.frame(groups, c(e, f))
    names(aov)[2]<-paste("numbers")
    res.aov <- aov(numbers ~ groups, data = aov)
    p.aov[i, 1:2] <- summary(res.aov)[[1]][["Pr(>F)"]]
    #add boxplot
    boxplot(numbers ~ groups, data = aov)
  }
  sig <- p.aov <= 0.05
  print(length(which(sig == TRUE)))
}
rapid.aov.box(400, 500, 500, 600) #5 sigs
#this is not having an effect because the numbers being pulled from each range-
#are being spread to each group evenly

#lest change that
rapid.aov.r <- function(min1, max1, min2, max2, min3, max3){
  p.aov <- data.frame(matrix(NA, ncol=2, nrow=100))
  names(p.aov)[1]<-paste("p-value")
  #now the letters print one at a time, 10 times
  groups <- rep(letters[1:3], each= 10)
  for (i in 1:100) {
    #add a third range so that I can control the range assigned to each group
    e<- runif(10, min1, max1)
    f<- runif(10, min2, max2)
    g<- runif(10, min3, max3)
    aov <- data.frame(groups, c(e, f, g))
    names(aov)[2]<-paste("numbers")
    res.aov <- aov(numbers ~ groups, data = aov)
    p.aov[i, 1:2] <- summary(res.aov)[[1]][["Pr(>F)"]]
  }
  sig <- p.aov <= 0.05
  print(length(which(sig == TRUE)))
}

#If I give it differnt ranges, I should get many sig counts
#let's try it
rapid.aov.r(1, 10, 20, 30, 40, 50) #100 sigs

#How close can I get before it starts making errors?
rapid.aov.r(1, 10, 10, 20, 20, 30) #100 sigs
rapid.aov.r(1, 10, 9, 19, 18, 28) #100 sigs
rapid.aov.r(1, 10, 5, 15, 10, 20) #100 sigs
#the medians here ^ are 5, 10, and 15

rapid.aov.r(1, 10, 2, 11, 3, 12) #28 sigs
#the medians here ^ are 5, 6, and 7

rapid.aov.r(1, 10, 3, 12, 5, 14) #87 sigs
#the medians here ^ are 5, 7, and 9

rapid.aov.r(1, 10, 2.5, 11.5, 4, 13) #46 sigs
#the medians here ^ are 5, 6.5, and 8

#so this can detect an effect size/difference of means of about 2 with-
#reasonible accuracy when n=10

#let now use normally distributed data
rapid.aov.norm <- function(n1, n2, n3, mu1, mu2, mu3, sd1, sd2, sd3){
  p.aov <- data.frame(matrix(NA, ncol=2, nrow=100))
  names(p.aov)[1]<-paste("p-value")
  #now the letters print one at a time, 10 times
  groups <- rep(letters[1:3], each= 10)
  for (i in 1:100) {
    #add a third range so that I can control the range assigned to each group
    e<- rnorm(n1, mu1, sd1)
    f<- rnorm(n2, mu2, sd2)
    g<- rnorm(n3, mu3, sd3)
    aov <- data.frame(groups, c(e, f, g))
    names(aov)[2]<-paste("numbers")
    res.aov <- aov(numbers ~ groups, data = aov)
    p.aov[i, 1:2] <- summary(res.aov)[[1]][["Pr(>F)"]]
  }
  sig <- p.aov <= 0.05
  print(length(which(sig == TRUE)))
}

#how big does the mean differnce have to be for R to reliably detect it?
#let's start with n= 10 and SD= 2
rapid.aov.norm(10, 10, 10, 5, 10, 15, 2, 2, 2) # 100 sigs
rapid.aov.norm(10, 10, 10, 5, 7, 9, 2, 2, 2) # 99 sigs
rapid.aov.norm(10, 10, 10, 5, 6, 7, 2, 2, 2) # 47 sigs
rapid.aov.norm(10, 10, 10, 5, 6.5, 8, 2, 2, 2) # 81 sigs
#again, it looks like a mean difference of two is the minimum when n= 10

#lets trty with SD=1
rapid.aov.norm(10, 10, 10, 5, 10, 15, 1, 1, 1) # 100 sigs
rapid.aov.norm(10, 10, 10, 5, 7, 9, 1, 1, 1) # 100 sigs
rapid.aov.norm(10, 10, 10, 5, 6, 7, 1, 1, 1) # 99 sigs
rapid.aov.norm(10, 10, 10, 5, 5.5, 6, 1, 1, 1) # 53 sigs
#and when SD is one, it can detect a mean difference of 1

#what if we increase sample size?
rapid.aov.norm(100, 100, 100, 5, 10, 15, 2, 2, 2) # 2 sigs
rapid.aov.norm(100, 100, 100, 5, 7, 9, 2, 2, 2) # 3 sigs
rapid.aov.norm(100, 100, 100, 5, 6, 7, 2, 2, 2) # 9 sigs
rapid.aov.norm(100, 100, 100, 5, 6.5, 8, 2, 2, 2) # 5 sigs
#hmmmmm

#lets change the difference of meas
rapid.aov.norm(100, 100, 100, 1, 100, 200, 2, 2, 2) # 0 sigs
rapid.aov.norm(100, 100, 100, 1, 50, 100, 2, 2, 2) # 0 sigs
rapid.aov.norm(100, 100, 100, 1, 25, 50, 2, 2, 2) # 0 sigs
rapid.aov.norm(100, 100, 100, 1, 10, 20, 2, 2, 2) # 0 sigs
#that is really weird

#lets make the functtion print use the numbers
rapid.aov.wtf <- function(n1, n2, n3, mu1, mu2, mu3, sd1, sd2, sd3){
  p.aov <- data.frame(matrix(NA, ncol=2, nrow=100))
  names(p.aov)[1]<-paste("p-value")
  groups <- rep(letters[1:3], each= 10)
  for (i in 1:100) {
    e<- rnorm(n1, mu1, sd1)
    f<- rnorm(n2, mu2, sd2)
    g<- rnorm(n3, mu3, sd3)
    aov <- data.frame(groups, c(e, f, g))
    names(aov)[2]<-paste("numbers")
    res.aov <- aov(numbers ~ groups, data = aov)
    p.aov[i, 1:2] <- summary(res.aov)[[1]][["Pr(>F)"]]
    #print the first few generated numbers
    print((aov))
    #print p values
    print(summary(res.aov)[[1]][["Pr(>F)"]])
  }
  sig <- p.aov <= 0.05
  print(length(which(sig == TRUE)))
}

rapid.aov.wtf(100, 100, 100, 1, 100, 200, 2, 2, 2)
#okay, so this is happening because I have it set up to print 30 letters but-
#300 numbers...

#Let's adjust the code
rapid.aov.ndep <- function(n, mu1, mu2, mu3, sd){
  p.aov <- data.frame(matrix(NA, ncol=2, nrow=100))
  names(p.aov)[1]<-paste("p-value")
  groups <- rep(letters[1:3], each= n)
  for (i in 1:100) {
    e<- rnorm(n, mu1, sd)
    f<- rnorm(n, mu2, sd)
    g<- rnorm(n, mu3, sd)
    aov <- data.frame(groups, c(e, f, g))
    names(aov)[2]<-paste("numbers")
    res.aov <- aov(numbers ~ groups, data = aov)
    p.aov[i, 1:2] <- summary(res.aov)[[1]][["Pr(>F)"]]
  }
  sig <- p.aov <= 0.05
  print(length(which(sig == TRUE)))
}

rapid.aov.ndep(10, 1, 10, 20, 2) # 100 sigs

#How close can my means get before R starts to get many errors?
rapid.aov.ndep(10, 1, 5, 9, 2) # 100 sigs
rapid.aov.ndep(10, 1, 4, 7, 2) # 100 sigs
rapid.aov.ndep(10, 1, 3, 5, 2) # 98 sigs
rapid.aov.ndep(10, 1, 2, 3, 2) # 40 sigs
#when SD= 2, it is again about 2

#let's test with SD= 1
rapid.aov.ndep(10, 1, 4, 7, 1) # 100 sigs
rapid.aov.ndep(10, 1, 3, 5, 1) # 100 sigs
rapid.aov.ndep(10, 1, 2, 3, 1) # 96 sigs
rapid.aov.ndep(10, 1, 1.5, 2, 1) # 40 sigs
#and when SD=1, it is about 1

#what about when we up the n?
rapid.aov.ndep(100, 1, 4, 7, 1) # 100 sigs
rapid.aov.ndep(100, 1, 3, 5, 1) # 100 sigs
rapid.aov.ndep(100, 1, 2, 3, 1) # 100 sigs
rapid.aov.ndep(100, 1, 1.5, 2, 1) # 100 sigs
rapid.aov.ndep(100, 1, 1.4, 1.8, 1) # 100 sigs
rapid.aov.ndep(100, 1, 1.3, 1.6, 1) # 98 sigs
rapid.aov.ndep(100, 1, 1.2, 1.4, 1) # 74 sigs
rapid.aov.ndep(100, 1, 1.1, 1.2, 1) # 20 sigs
#it looks like when your n increases by one order of magnitude, your percision- 
#in detecting a differnce of means also increases by one order of magnitude
#A little bit less, actually (0.2 is more reliable that 0.1)

#test with 1000
rapid.aov.ndep(1000, 1, 1.09, 1.18, 1) # 97 sigs
rapid.aov.ndep(1000, 1, 1.08, 1.16, 1) # 86 sigs
rapid.aov.ndep(1000, 1, 1.07, 1.14, 1) # 78 sigs
rapid.aov.ndep(1000, 1, 1.06, 1.12, 1) # 63 sigs
rapid.aov.ndep(1000, 1, 1.05, 1.1, 1) # 46 sigs
rapid.aov.ndep(1000, 1, 1.04, 1.08, 1) # 35 sigs
rapid.aov.ndep(1000, 1, 1.03, 1.06, 1) # 16 sigs
rapid.aov.ndep(1000, 1, 1.02, 1.04, 1) # 8 sigs
rapid.aov.ndep(1000, 1, 1.01, 1.02, 1) # 8 sigs
#nevermind, it is definatly not a linear increase
#my phisics roomate and I discussed and we think it is logarithmic, which is-
#very facinating

#stop

#------------------Try out two way ANOVA--------------------------------------

#make a fake data set that is compaible with two way ANOVA
dose <- rep(rep(c(1, 2, 3), each= 10), length= 90)
condition <- rep(c("no sick", "sick", "extra sick"), each= 30)
#lets do n= 10 per condition and dose 
change.in.level.of.sick <- rnorm(90, 0, 2)
sick.test <- data.frame(condition, dose, change.in.level.of.sick)

#visualize data
boxplot(change.in.level.of.sick ~ dose, data = sick.test)
boxplot(change.in.level.of.sick ~ condition, data = sick.test)

#visualize both level groups at one
cols <- rainbow(3, s = 0.5)
boxplot(change.in.level.of.sick ~ dose + condition, data = sick.test,
        at = c(1:3, 5:7, 9:11), col = cols,
        names = c(NA, "No Sick", NA, NA, "Sick", NA, NA, "Exrta sick", NA), 
        xaxs = FALSE,
        las=1,
        main= "Effects of Dose by Initial Level of Sick",
        ylab= "Change in Level of Sick",
        xlab= "Inittial level of Sick")
legend("topleft", fill = cols, legend = c(1,2,3), horiz = T)
#As we might expect given that I used random number generation, there doesn't-
#look like there is a lot of significant differences going on here

#compute an ANOVA
res.aov <- aov(change.in.level.of.sick ~ dose + condition, data = sick.test)
summary(res.aov)

#how to get two p values out
p.aov.2 <- data.frame(matrix(NA, ncol=3, nrow=100))
p.aov.2[i, 1:3] <- summary(res.aov)[[1]][["Pr(>F)"]]


#------------------------------stops working----------------------------------

#lets make a function
rapid.2.aov.r <- function(mean, variance){
  p.aov.2 <- data.frame(matrix(NA, ncol=3, nrow=100))
  names(p.aov.2)[1]<-paste("p-value dose")
  names(p.aov.2)[2]<-paste("p-value condition")
  dose <- rep(rep(c(1, 2, 3), each= 10), length= 90)
  condition <- rep(c("no sick", "sick", "extra sick"), each= 30)
  for (i in 1:100) {
    change.in.level.of.sick <- rnorm(90, mean, variance)
    aov <- data.frame(change.in.level.of.sick, condition, dose)
    res.aov <- aov(change.in.level.of.sick ~ dose + condition, data = sick.test)
    p.aov.2[i, 1:3] <- summary(res.aov)[[1]][["Pr(>F)"]]
  }
  sig <- p.aov.2[, 1] <= 0.05
  sig2 <- p.aov.2[, 2] <= 0.05
  print(length(which(sig == TRUE)))
  print(length(which(sig2 == TRUE)))
}

rapid.2.aov.r(10, 1) #0, 0 sigs
rapid.2.aov.r(100, 1) #0, 0 sigs
rapid.2.aov.r(1000, 1) #0, 0 sigs
#... I think I need to split up the change.in.level.of.sick variable so that-
#I can have differnt means (etc) for each initial level of sickness

#let's try that
rapid.2.aov <- function(m1, m2, m3, v1, v2, v3){
  p.aov.2 <- data.frame(matrix(NA, ncol=3, nrow=100))
  names(p.aov.2)[1]<-paste("p-value dose")
  names(p.aov.2)[2]<-paste("p-value condition")
  dose <- rep(rep(c(1, 2, 3), each= 10), length= 90)
  condition <- rep(c("no sick", "sick", "extra sick"), each= 30)
  for (i in 1:100) {
    change.in.level.of.sick[1:30] <- rnorm(30, m1, v1)
    change.in.level.of.sick[31:60] <- rnorm(30, m2, v2)
    change.in.level.of.sick[61:90] <- rnorm(30, m3, v3)
    aov <- data.frame(change.in.level.of.sick, condition, dose)
    res.aov <- aov(change.in.level.of.sick ~ dose + condition, data = sick.test)
    p.aov.2[i, 1:3] <- summary(res.aov)[[1]][["Pr(>F)"]]
  }
  sig <- p.aov.2[, 1] <= 0.05
  sig2 <- p.aov.2[, 2] <= 0.05
  print(length(which(sig == TRUE)))
  print(length(which(sig2 == TRUE)))
}

rapid.2.aov(1, 10, 20, 2, 2, 2) #0, 0 sigs
rapid.2.aov(1, 10, 20, 2, 2, 2) #0, 0 sigs
rapid.2.aov(1, 10, 20, 10, 10, 10) #0, 0 sigs

#I do not know what's happening so lets through a boxplot into my function
rapid.2.aov.b <- function(m1, m2, m3, v1, v2, v3){
  p.aov.2 <- data.frame(matrix(NA, ncol=3, nrow=100))
  names(p.aov.2)[1]<-paste("p-value dose")
  names(p.aov.2)[2]<-paste("p-value condition")
  dose <- rep(rep(c(1, 2, 3), each= 10), length= 90)
  condition <- rep(c("no sick", "sick", "extra sick"), each= 30)
  for (i in 1:100) {
    change.in.level.of.sick[1:30] <- rnorm(30, m1, v1)
    change.in.level.of.sick[31:60] <- rnorm(30, m2, v2)
    change.in.level.of.sick[61:90] <- rnorm(30, m3, v3)
    aov <- data.frame(change.in.level.of.sick, condition, dose)
    res.aov <- aov(change.in.level.of.sick ~ dose + condition, data = sick.test)
    p.aov.2[i, 1:3] <- summary(res.aov)[[1]][["Pr(>F)"]]
    cols <- rainbow(3, s = 0.5)
    boxplot(change.in.level.of.sick ~ dose + condition, data = sick.test,
            at = c(1:3, 5:7, 9:11), col = cols,
            names = c(NA, "No Sick", NA, NA, "Sick", NA, NA, "Exrta sick", NA), 
            xaxs = FALSE,
            las=1,
            main= "Effects of Dose by Initial Level of Sick",
            ylab= "Change in Level of Sick",
            xlab= "Inittial level of Sick")
    legend("topleft", fill = cols, legend = c(1,2,3), horiz = T)
  }
  sig <- p.aov.2[, 1] <= 0.05
  sig2 <- p.aov.2[, 2] <= 0.05
  print(length(which(sig == TRUE)))
  print(length(which(sig2 == TRUE)))
  cols <- rainbow(3, s = 0.5)
}

#testing
rapid.2.aov.b(10, 20, 30, 2, 2, 2)
#If you flip through the box plot, you will see that they are all the same

#First function boxplot test
rapid.2.aov.r <- function(mean, variance){
  p.aov.2 <- data.frame(matrix(NA, ncol=3, nrow=100))
  names(p.aov.2)[1]<-paste("p-value dose")
  names(p.aov.2)[2]<-paste("p-value condition")
  dose <- rep(rep(c(1, 2, 3), each= 10), length= 90)
  condition <- rep(c("no sick", "sick", "extra sick"), each= 30)
  for (i in 1:100) {
    change.in.level.of.sick <- rnorm(90, mean, variance)
    aov <- data.frame(change.in.level.of.sick, condition, dose)
    res.aov <- aov(change.in.level.of.sick ~ dose + condition, data = sick.test)
    p.aov.2[i, 1:3] <- summary(res.aov)[[1]][["Pr(>F)"]]
    boxplot(change.in.level.of.sick ~ dose + condition, data = sick.test,
            at = c(1:3, 5:7, 9:11), col = cols,
            names = c(NA, "No Sick", NA, NA, "Sick", NA, NA, "Exrta sick", NA), 
            xaxs = FALSE,
            las=1,
            main= "Effects of Dose by Initial Level of Sick",
            ylab= "Change in Level of Sick",
            xlab= "Inittial level of Sick")
    legend("topleft", fill = cols, legend = c(1,2,3), horiz = T)
  }
  sig <- p.aov.2[, 1] <= 0.05
  sig2 <- p.aov.2[, 2] <= 0.05
  print(length(which(sig == TRUE)))
  print(length(which(sig2 == TRUE)))
}

rapid.2.aov.r(10, 2)
#hmm... It is also printing the same box plots

#Print rnorm to see if problem is there
#prin p-value to see if problem is there
rapid.2.aov.r.testing <- function(mean, variance){
  p.aov.2 <- data.frame(matrix(NA, ncol=3, nrow=100))
  names(p.aov.2)[1]<-paste("p-value dose")
  names(p.aov.2)[2]<-paste("p-value condition")
  dose <- rep(rep(c(1, 2, 3), each= 10), length= 90)
  condition <- rep(c("no sick", "sick", "extra sick"), each= 30)
  for (i in 1:100) {
    change.in.level.of.sick <- rnorm(90, mean, variance)
    print(head(change.in.level.of.sick))
    aov <- data.frame(change.in.level.of.sick, condition, dose)
    res.aov <- aov(change.in.level.of.sick ~ dose + condition, data = sick.test)
    p.aov.2[i, 1:3] <- summary(res.aov)[[1]][["Pr(>F)"]]
    print( p.aov.2[i, 1:3])
  }
}

rapid.2.aov.r.testing(10, 5)
#problem is with p-value
#why, then, am I getting the same box plot

#let's test the boxplos again
rapid.2.aov.r.testing.2 <- function(mean, variance){
  p.aov.2 <- data.frame(matrix(NA, ncol=3, nrow=100))
  names(p.aov.2)[1]<-paste("p-value dose")
  names(p.aov.2)[2]<-paste("p-value condition")
  dose <- rep(rep(c(1, 2, 3), each= 10), length= 90)
  condition <- rep(c("no sick", "sick", "extra sick"), each= 30)
  for (i in 1:100) {
    change.in.level.of.sick <- rnorm(90, mean, variance)
    print(head(change.in.level.of.sick))
    boxplot(change.in.level.of.sick ~ dose + condition, data = sick.test,
            at = c(1:3, 5:7, 9:11), col = cols,
            names = c(NA, "No Sick", NA, NA, "Sick", NA, NA, "Exrta sick", NA), 
            xaxs = FALSE,
            las=1,
            main= "Effects of Dose by Initial Level of Sick",
            ylab= "Change in Level of Sick",
            xlab= "Inittial level of Sick")
    legend("topleft", fill = cols, legend = c(1,2,3), horiz = T)
    aov <- data.frame(change.in.level.of.sick, condition, dose)
    res.aov <- aov(change.in.level.of.sick ~ dose + condition, data = sick.test)
    p.aov.2[i, 1:3] <- summary(res.aov)[[1]][["Pr(>F)"]]
    print( p.aov.2[i, 1:3])
  }
}

#Testing
rapid.2.aov.r.testing.2(10, 5)
#So, even though I have different numbers, it is giving me the exact same-
#box plots

#trying somthing else
rapid.2.aov.r.testing <- function(mean, variance){
  p.aov.2 <- data.frame(matrix(NA, ncol=3, nrow=100))
  names(p.aov.2)[1]<-paste("p-value dose")
  names(p.aov.2)[2]<-paste("p-value condition")
  dose <- rep(rep(c(1, 2, 3), each= 10), length= 90)
  condition <- rep(c("no sick", "sick", "extra sick"), each= 30)
  for (i in 1:100) {
    change.in.level.of.sick <- rnorm(90, mean, variance)
    print(change.in.level.of.sick)
    aov <- data.frame(change.in.level.of.sick, condition, dose)
    res.aov <- aov(change.in.level.of.sick ~ dose + condition, data = sick.test)
    p.aov.2[i, 1:3] <- summary(res.aov)[[1]][["Pr(>F)"]]
    print(summary(res.aov)[[1]][["Pr(>F)"]])
  }
}

rapid.2.aov.r.testing(10, 2)
#the issue is that my one cahnge in level of sickness spans the entire length-
#of the data frame

#Lets change that
rapid.2.aov.w <- function(m1, m2, m3, sd1, sd2, sd3){
  p.aov.2 <- data.frame(matrix(NA, ncol=3, nrow=100))
  names(p.aov.2)[1]<-paste("p-value dose")
  names(p.aov.2)[2]<-paste("p-value condition")
  dose <- rep(rep(c(1, 2, 3), each= 10), length= 90)
  condition <- rep(c("no sick", "sick", "extra sick"), each= 30)
  for (i in 1:100) {
    change.in.level.of.sick1 <- rnorm(30, m1, sd1)
    change.in.level.of.sick2 <- rnorm(30, m2, sd2)
    change.in.level.of.sick3 <- rnorm(30, m3, sd3)
    aov <- data.frame(c(change.in.level.of.sick1, change.in.level.of.sick2,
                        change.in.level.of.sick3), condition, dose)
    res.aov <- aov(change.in.level.of.sick ~ dose + condition, data = sick.test)
    p.aov.2[i, 1:3] <- summary(res.aov)[[1]][["Pr(>F)"]]
  }
  sig <- p.aov.2[, 1] <= 0.05
  sig2 <- p.aov.2[, 2] <= 0.05
  print(length(which(sig == TRUE)))
  print(length(which(sig2 == TRUE)))
}

#testing
rapid.2.aov.working(1, 10, 100, 2, 2, 2)
#still not working

#let's print againrapid.2.aov.working <- function(m1, m2, m3, sd1, sd2, sd3){
rapid.2.aov.w <- function(m1, m2, m3, sd1, sd2, sd3){
  p.aov.2 <- data.frame(matrix(NA, ncol=3, nrow=100))
  names(p.aov.2)[1]<-paste("p-value dose")
  names(p.aov.2)[2]<-paste("p-value condition")
  dose <- rep(rep(c(1, 2, 3), each= 10), length= 90)
  condition <- rep(c("no sick", "sick", "extra sick"), each= 30)
  for (i in 1:100) {
    change.in.level.of.sick1 <- rnorm(30, m1, sd1)
    change.in.level.of.sick2 <- rnorm(30, m2, sd2)
    change.in.level.of.sick3 <- rnorm(30, m3, sd3)
    aov <- data.frame(c(change.in.level.of.sick1, change.in.level.of.sick2,
                        change.in.level.of.sick3), condition, dose)
    print(aov[, 1])
    res.aov <- aov(change.in.level.of.sick ~ dose + condition, data = sick.test)
    p.aov.2[i, 1:3] <- summary(res.aov)[[1]][["Pr(>F)"]]
    print(summary(res.aov)[[1]][["Pr(>F)"]])
  }
  sig <- p.aov.2[, 1] <= 0.05
  sig2 <- p.aov.2[, 2] <= 0.05
  print(length(which(sig == TRUE)))
  print(length(which(sig2 == TRUE)))
}

rapid.2.aov.w(1, 10, 100, 2, 2, 2)
#so even when the difference in means is there, I see no significance

#let's give a mean difference to every dose of every condition
rapid.2.aov.x <- function(m1, m2, m3, m4, m5, m6, m7, m8, m9){
  p.aov.2 <- data.frame(matrix(NA, ncol=3, nrow=100))
  names(p.aov.2)[1]<-paste("p-value dose")
  names(p.aov.2)[2]<-paste("p-value condition")
  dose <- rep(rep(c(1, 2, 3), each= 10), length= 90)
  condition <- rep(c("no sick", "sick", "extra sick"), each= 30)
  for (i in 1:100) {
    change.in.level.of.sick1 <- rnorm(10, m1, 2)
    change.in.level.of.sick2 <- rnorm(10, m2, 2)
    change.in.level.of.sick3 <- rnorm(10, m3, 2)
    change.in.level.of.sick4 <- rnorm(10, m4, 2)
    change.in.level.of.sick5 <- rnorm(10, m5, 2)
    change.in.level.of.sick6 <- rnorm(10, m6, 2)
    change.in.level.of.sick7 <- rnorm(10, m7, 2)
    change.in.level.of.sick8 <- rnorm(10, m8, 2)
    change.in.level.of.sick9 <- rnorm(10, m9, 2)
    aov <- data.frame(c(change.in.level.of.sick1, change.in.level.of.sick2,
                        change.in.level.of.sick3, change.in.level.of.sick4,
                        change.in.level.of.sick5, change.in.level.of.sick6,
                        change.in.level.of.sick7, change.in.level.of.sick8,
                        change.in.level.of.sick9), 
                      condition, dose)
    print(aov[, 1])
    res.aov <- aov(change.in.level.of.sick ~ dose + condition, data = aov)
    p.aov.2[i, 1:3] <- summary(res.aov)[[1]][["Pr(>F)"]]
    print(summary(res.aov)[[1]][["Pr(>F)"]])
  }
  sig <- p.aov.2[, 1] <= 0.05
  sig2 <- p.aov.2[, 2] <= 0.05
  print(length(which(sig == TRUE)))
  print(length(which(sig2 == TRUE)))
}

rapid.2.aov.x(10, 10, 10, 10, 10, 10, 10, 10, 100)
#WTF!!!

#lets try in not the function
change.in.level.of.sick1 <- rnorm(10, 10, 2)
change.in.level.of.sick2 <- rnorm(10, 10, 2)
change.in.level.of.sick3 <- rnorm(10, 10, 2)
change.in.level.of.sick4 <- rnorm(10, 10, 2)
change.in.level.of.sick5 <- rnorm(10, 10, 2)
change.in.level.of.sick6 <- rnorm(10, 10, 2)
change.in.level.of.sick7 <- rnorm(10, 10, 2)
change.in.level.of.sick8 <- rnorm(10, 10, 2)
change.in.level.of.sick9 <- rnorm(10, 100, 2)
aov <- data.frame(c(change.in.level.of.sick1, change.in.level.of.sick2,
                    change.in.level.of.sick3, change.in.level.of.sick4,
                    change.in.level.of.sick5, change.in.level.of.sick6,
                    change.in.level.of.sick7, change.in.level.of.sick8,
                    change.in.level.of.sick9), 
                  condition, dose)

res.aov <- aov(change.in.level.of.sick ~ dose + condition, data = aov)
summary(res.aov)

change.in.level.of.sick1 <- rnorm(10, 10, 2)
change.in.level.of.sick2 <- rnorm(10, 20, 2)
change.in.level.of.sick3 <- rnorm(10, 30, 2)
change.in.level.of.sick4 <- rnorm(10, 40, 2)
change.in.level.of.sick5 <- rnorm(10, 50, 2)
change.in.level.of.sick6 <- rnorm(10, 60, 2)
change.in.level.of.sick7 <- rnorm(10, 70, 2)
change.in.level.of.sick8 <- rnorm(10, 80, 2)
change.in.level.of.sick9 <- rnorm(10, 90, 2)
aov <- data.frame(c(change.in.level.of.sick1, change.in.level.of.sick2,
                    change.in.level.of.sick3, change.in.level.of.sick4,
                    change.in.level.of.sick5, change.in.level.of.sick6,
                    change.in.level.of.sick7, change.in.level.of.sick8,
                    change.in.level.of.sick9), 
                  condition, dose)

res.aov <- aov(change.in.level.of.sick ~ dose + condition, data = aov)
summary(res.aov)

change.in.level.of.sick1 <- rnorm(10, 10, 2)
change.in.level.of.sick2 <- rnorm(10, 200, 2)
change.in.level.of.sick3 <- rnorm(10, 3000, 2)
change.in.level.of.sick4 <- rnorm(10, 40000, 2)
change.in.level.of.sick5 <- rnorm(10, 500000, 2)
change.in.level.of.sick6 <- rnorm(10, 6000000, 2)
change.in.level.of.sick7 <- rnorm(10, 70000000, 2)
change.in.level.of.sick8 <- rnorm(10, 800000000, 2)
change.in.level.of.sick9 <- rnorm(10, 9000000000, 2)
aov <- data.frame(c(change.in.level.of.sick1, change.in.level.of.sick2,
                    change.in.level.of.sick3, change.in.level.of.sick4,
                    change.in.level.of.sick5, change.in.level.of.sick6,
                    change.in.level.of.sick7, change.in.level.of.sick8,
                    change.in.level.of.sick9), 
                  condition, dose)

res.aov <- aov(change.in.level.of.sick ~ dose + condition, data =aov)
summary(res.aov)

#---------------------------lets simplify-------------------------------

#use oroginal data
aov(change.in.level.of.sick ~ dose + condition, data =sick.test)
summary(aov(change.in.level.of.sick ~ dose + condition, data =sick.test))

#change original data
dose <- rep(rep(c(1, 2, 3), each= 10), length= 90)
condition <- rep(c("no sick", "sick", "extra sick"), each= 30)
change.in.level.of.sick <- rnorm(90, 15, 30)
sick.test <- data.frame(condition, dose, change.in.level.of.sick)

#test
summary(aov(change.in.level.of.sick ~ dose + condition, data =sick.test))
#so those are different

change.in.level.of.sick <- rnorm(90, 30, 90)
sick.test <- data.frame(condition, dose, change.in.level.of.sick)
summary(aov(change.in.level.of.sick ~ dose + condition, data =sick.test))
#okay, that progress...

#let's see if I can get a significant read
cls1 <- rnorm(30, 10, 2)
cls2 <- rnorm(30, 50, 2)
cls3 <- rnorm(30, 100, 2)
change.in.level.of.sick <- c(cls1, cls2, cls3)
sick.test <- data.frame(condition, dose, change.in.level.of.sick)
summary(aov(change.in.level.of.sick ~ dose + condition, data =sick.test))
#yes I can

#let's make a new function
take.2 <- function(m1, m2, m3){
  p.aov <- data.frame(matrix(NA, ncol=3, nrow=100))
  names(p.aov.2)[1]<-paste("p-value dose")
  names(p.aov.2)[2]<-paste("p-value condition")
  dose <- rep(rep(c(1, 2, 3), each= 10), length= 90)
  condition <- rep(c("no sick", "sick", "extra sick"), each= 30)
  for (i in 1:100) {
    cls1 <- rnorm(30, m1, 2)
    cls2 <- rnorm(30, m2, 2)
    cls3 <- rnorm(30, m3, 2)
    change.in.level.of.sick <- c(cls1, cls2, cls3)
    sick.test <- data.frame(condition, dose, change.in.level.of.sick)
    res.aov <- aov(change.in.level.of.sick ~ dose + condition, data =sick.test)
    p.aov[i, 1:3] <- summary(res.aov)[[1]][["Pr(>F)"]]
  }
  sig <- p.aov[, 1] <= 0.05
  sig2 <- p.aov[, 2] <= 0.05
  print(length(which(sig == TRUE)))
  print(length(which(sig2 == TRUE)))
}

take.2(10, 50, 100)
#YAY!!!

#------------------------------working-------------------------------------

#lets go ahead and change the name of that
aov.2w.3w <- function(m1, m2, m3){
  p.aov <- data.frame(matrix(NA, ncol=3, nrow=100))
  names(p.aov.2)[1]<-paste("p-value dose")
  names(p.aov.2)[2]<-paste("p-value condition")
  dose <- rep(rep(c(1, 2, 3), each= 10), length= 90)
  condition <- rep(c("no sick", "sick", "extra sick"), each= 30)
  for (i in 1:100) {
    cls1 <- rnorm(30, m1, 2)
    cls2 <- rnorm(30, m2, 2)
    cls3 <- rnorm(30, m3, 2)
    change.in.level.of.sick <- c(cls1, cls2, cls3)
    sick.test <- data.frame(condition, dose, change.in.level.of.sick)
    res.aov <- aov(change.in.level.of.sick ~ dose + condition, data =sick.test)
    p.aov[i, 1:3] <- summary(res.aov)[[1]][["Pr(>F)"]]
  }
  sig <- p.aov[, 1] <= 0.05
  sig2 <- p.aov[, 2] <= 0.05
  print(length(which(sig == TRUE)))
  print(length(which(sig2 == TRUE)))
}

#now lets make a nine way function
aov.2w.9w <- function(m1, m2, m3, m4, m5, m6, m7, m8, m9){
  p.aov <- data.frame(matrix(NA, ncol=3, nrow=100))
  names(p.aov.2)[1]<-paste("p-value dose")
  names(p.aov.2)[2]<-paste("p-value condition")
  dose <- rep(rep(c(1, 2, 3), each= 10), length= 90)
  condition <- rep(c("no sick", "sick", "extra sick"), each= 30)
  for (i in 1:100) {
    cls1 <- rnorm(10, m1, 2)
    cls2 <- rnorm(10, m2, 2)
    cls3 <- rnorm(10, m3, 2)
    cls4 <- rnorm(10, m4, 2)
    cls5 <- rnorm(10, m5, 2)
    cls6 <- rnorm(10, m6, 2)
    cls7 <- rnorm(10, m7, 2)
    cls8 <- rnorm(10, m8, 2)
    cls9 <- rnorm(10, m9, 2)
    change.in.level.of.sick <- c(cls1, cls2, cls3, cls4, cls5, cls6,
                                 cls7, cls8, cls9)
    sick.test <- data.frame(condition, dose, change.in.level.of.sick)
    res.aov <- aov(change.in.level.of.sick ~ dose + condition, data =sick.test)
    p.aov[i, 1:3] <- summary(res.aov)[[1]][["Pr(>F)"]]
  }
  sig <- p.aov[, 1] <= 0.05
  sig2 <- p.aov[, 2] <= 0.05
  print(length(which(sig == TRUE)))
  print(length(which(sig2 == TRUE)))
}

#lets get sigs for just conditions, not dose
aov.2w.3w(10, 50, 100) # 8, 100 sigs (it prints the dose count first)

#lets get sigs for both
aov.2w.9w(10, 20, 30, 40, 50, 60, 70, 80, 90) # 100, 100 sigs

#what if there is a difference in dose in one group but no difference at all-
#in the other two groups (wih differen sizes of mean differnce)?
aov.2w.9w(10, 20, 30, 20, 20, 20, 20, 20, 20) # 100, 0 sigs (0= condition)
aov.2w.9w(1, 2, 3, 2, 2, 2, 2, 2, 2) # 23, 7 sigs
aov.2w.9w(1, 3, 5, 3, 3, 3, 3, 3, 3) # 70, 0 sigs
aov.2w.9w(1, 4, 7, 4, 4, 4, 4, 4, 4) # 93, 3 sigs
#so, when n=90m (each does only exists 30 times, and only 10 times per- 
#condition), can detect a difference in means outcome in dosage down to about- 
#a mean difference of 3

#what about when does has no effect but condition does?
aov.2w.3w(10, 20, 30) # 7, 100 sigs (expected)
aov.2w.3w(1, 5, 10) # 3, 100 sigs (also expected, because SD is 2 and each-
#condition exisst 30 times in this function)
aov.2w.3w(1, 4, 7) # 1, 100 sigs
aov.2w.3w(1, 3, 5) # 3, 100 sigs
aov.2w.3w(1, 2, 3) # 5, 94 sigs
#we would expec o gett errors if we go any smaller since our condition n= 30-
#and we have seen previously that when n=10, we can detect a difference of-
#means only as large as (or slightly smaller than) the SD
aov.2w.3w(1, 1.5, 2) # 3, 45 sigs
aov.2w.3w(1, 1.3, 1.5) # 2, 12 sigs
#so when sample size (of condittion) is 30 and SD is 2, we can detect a-
#difference of means as small as 1 reliably (vs. mean dif=2 when n=10 and SD=2)




#============================Calculating effect size=========================
#---------------------------package instalaion-------------------------------

install.packages("effsize")
library(effects)

#activate/call function
cohen.d <- effsize::cohen.d


#-----------------------------t-tests-------------------------------------

#Cohen's d compairs the mean  between two conditions (two levels) and spists-
#out a standardized effect size (where 0.2 is considered small, 0.5 is medium-
#ond 0.8 is large)

#t-test can only look at a difference of means, no levels
#lets make two seperate vectors (each representing one level) so that we-
#can run a t-test and then merge them to run Cohen's d

#make the data
CBD <- rnorm(5, 5)
Control <- rnorm(5, 10)

#run the t-test
t.test(CBD, CCT)

#create equivilent data set
treatment <- rep(c("Control", "CBD"), each= 5)
change.in.anxiety.index <- c(rnorm(5, 5), rnorm(5, 10))
anxiety.test <- data.frame(treatment, change.in.anxiety.index)

#Run cohen's d
cohen.d(change.in.anxiety.index, treatment)
#that tis rediculously large...

#try again with a larger n
treatment <- rep(c("Control", "CBD"), each= 50)
change.in.anxiety.index <- c(rnorm(50, 5), rnorm(50, 10))
anxiety.test <- data.frame(treatment, change.in.anxiety.index)
#Run cohen's d
cohen.d(change.in.anxiety.index, treatment)
#Still unplausibly large

#try again with a smaller mean difference
treatment <- rep(c("Control", "CBD"), each= 5)
change.in.anxiety.index <- c(rnorm(5, 5), rnorm(5, 6))
anxiety.test <- data.frame(treatment, change.in.anxiety.index)
#Run cohen's d
cohen.d(change.in.anxiety.index, treatment)
#that is more reasonible

#since variance is default 1, lets close the mean difference gap a bit more
treatment <- rep(c("Control", "CBD"), each= 5)
change.in.anxiety.index <- c(rnorm(5, 5), rnorm(5, 5.5))
anxiety.test <- data.frame(treatment, change.in.anxiety.index)
#Run cohen's d
cohen.d(change.in.anxiety.index, treatment)
#0.7 seems pretty large still...

#lets close that gap some more
treatment <- rep(c("Control", "CBD"), each= 5)
change.in.anxiety.index <- c(rnorm(5, 5), rnorm(5, 5.2))
anxiety.test <- data.frame(treatment, change.in.anxiety.index)
#Run cohen's d
cohen.d(change.in.anxiety.index, treatment)
#well, if you run this a few times it changes between "large" and "negligible"

#lets just up SD a lot (because it seems to be most sensitive to SD)
treatment <- rep(c("Control", "CBD"), each= 5)
change.in.anxiety.index <- c(rnorm(5, 5, 10), rnorm(5, 6, 10))
anxiety.test <- data.frame(treatment, change.in.anxiety.index)
#Run cohen's d
cohen.d(change.in.anxiety.index, treatment)
#thats more like it
#so the only thing that has a big effect is SD...

#what if SD is different between groups?
treatment <- rep(c("Control", "CBD"), each= 5)
change.in.anxiety.index <- c(rnorm(5, 5, 10), rnorm(5, 6, 5))
anxiety.test <- data.frame(treatment, change.in.anxiety.index)
#Run cohen's d
cohen.d(change.in.anxiety.index, treatment)
#If you run this multiple times, it seems that having unequal variance makes-
#it unstable (it depends a lot on the rnorm and therefor is just not robust)

#Lets make SD even less equal
treatment <- rep(c("Control", "CBD"), each= 5)
change.in.anxiety.index <- c(rnorm(5, 5, 1), rnorm(5, 6, 10))
anxiety.test <- data.frame(treatment, change.in.anxiety.index)
#Run cohen's d
cohen.d(change.in.anxiety.index, treatment)
#even larger range
#it is very sensitive to SD

#what if I have large mean difference and large variance (checking if it is an-
#absolute thing or a proportional thing)
treatment <- rep(c("Control", "CBD"), each= 50)
change.in.anxiety.index <- c(rnorm(50, 5, 15), rnorm(50, 6, 15))
anxiety.test <- data.frame(treatment, change.in.anxiety.index)
#Run cohen's d
cohen.d(change.in.anxiety.index, treatment)
#small range

#now small mean difference and small variance
treatment <- rep(c("Control", "CBD"), each= 50)
change.in.anxiety.index <- c(rnorm(50, 5, 1), rnorm(50, 5.2, 1))
anxiety.test <- data.frame(treatment, change.in.anxiety.index)
#Run cohen's d
cohen.d(change.in.anxiety.index, treatment)
#small range
#it seems to be proportional

#What if we up the actual size of the means (small mean dif)
treatment <- rep(c("Control", "CBD"), each= 50)
change.in.anxiety.index <- c(rnorm(50, 50, 15), rnorm(50, 51, 15))
anxiety.test <- data.frame(treatment, change.in.anxiety.index)
#Run cohen's d
cohen.d(change.in.anxiety.index, treatment)
#small range

#What if we up the actual size of the means (big mean dif)
treatment <- rep(c("Control", "CBD"), each= 50)
change.in.anxiety.index <- c(rnorm(50, 50, 15), rnorm(50, 60, 15))
anxiety.test <- data.frame(treatment, change.in.anxiety.index)
#Run cohen's d
cohen.d(change.in.anxiety.index, treatment)
#medium range


#what about when SD is different
treatment <- rep(c("Control", "CBD"), each= 50)
change.in.anxiety.index <- c(rnorm(50, 5, 10), rnorm(50, 6, 20))
anxiety.test <- data.frame(treatment, change.in.anxiety.index)
#Run cohen's d
cohen.d(change.in.anxiety.index, treatment)
#small range

#what about when mean difference is large and SD is the same
treatment <- rep(c("Control", "CBD"), each= 50)
change.in.anxiety.index <- c(rnorm(50, 5, 15), rnorm(50, 10, 15))
anxiety.test <- data.frame(treatment, change.in.anxiety.index)
#Run cohen's d
cohen.d(change.in.anxiety.index, treatment)
#medium range

#what about when meand difference is large and SD is different
treatment <- rep(c("Control", "CBD"), each= 50)
change.in.anxiety.index <- c(rnorm(50, 5, 10), rnorm(50, 10, 20))
anxiety.test <- data.frame(treatment, change.in.anxiety.index)
#Run cohen's d
cohen.d(change.in.anxiety.index, treatment)
#small range

#in conclution, n has a proportional effect, mean difference has some effect-
#and SD has the most significant effect


#--------------with hodge's g correction--------------------------------

#there is an optional correction you can place in this that corrects for-
#upward bias(default is FALSE)
#lets turn it on

#large n, large MD, dif SD
treatment <- rep(c("Control", "CBD"), each= 50)
change.in.anxiety.index <- c(rnorm(50, 5, 10), rnorm(50, 10, 20))
anxiety.test <- data.frame(treatment, change.in.anxiety.index)
#Run cohen's d
cohen.d(change.in.anxiety.index, treatment, hodges.correction= TRUE)
#small range (same as without correction)

#large n, large MD, same SD
treatment <- rep(c("Control", "CBD"), each= 50)
change.in.anxiety.index <- c(rnorm(50, 5, 15), rnorm(50, 10, 15))
anxiety.test <- data.frame(treatment, change.in.anxiety.index)
#Run cohen's d
cohen.d(change.in.anxiety.index, treatment)
#small range (smaller than before)

#small n, small MD, dif SD
treatment <- rep(c("Control", "CBD"), each= 5)
change.in.anxiety.index <- c(rnorm(5, 5, 1), rnorm(5, 6, 10))
anxiety.test <- data.frame(treatment, change.in.anxiety.index)
#Run cohen's d
cohen.d(change.in.anxiety.index, treatment)
#large range (same as before)

#it seems that the correction does very little


#----------let's just pretend my simulated data is real and unchangible-------

#lets make a few data sets as though they are testing the same thig

#study 1
treatment <- rep(c("Control", "CBD"), each= 100)
change.in.anxiety.index <- c(rnorm(100, 5, 2), rnorm(100, 20, 8))
study1 <- data.frame(treatment, change.in.anxiety.index)

#study 2
treatment <- rep(c("Control", "CBD"), each= 100)
change.in.anxiety.index <- c(rnorm(100, 5, 2), rnorm(100, 25, 6))
study2 <- data.frame(treatment, change.in.anxiety.index)

#study 3
treatment <- rep(c("Control", "CBD"), each= 100)
change.in.anxiety.index <- c(rnorm(100, 10, 2), rnorm(100, 15, 2.5))
study3 <- data.frame(treatment, change.in.anxiety.index)

#study 4
treatment <- rep(c("Control", "CBD"), each= 100)
change.in.anxiety.index <- c(rnorm(100, 50, 4), rnorm(100, 51, 4))
study4 <- data.frame(treatment, change.in.anxiety.index)

#study 5
treatment <- rep(c("Control", "CBD"), each= 100)
change.in.anxiety.index <- c(rnorm(100, 20, 2), rnorm(100, 15, 1.5))
study5 <- data.frame(treatment, change.in.anxiety.index)

#Cohens d for each sudy

#study 1
cohen.d(study1$change.in.anxiety.index, study1$treatment)
#d= 2.4

#study 2
cohen.d(study2$change.in.anxiety.index, study2$treatment)
#d= 4.3

#study 3
cohen.d(study3$change.in.anxiety.index, study3$treatment)
#d= 2.3

#study 4
cohen.d(study4$change.in.anxiety.index, study4$treatment)
#d= 0.1
#the only real difference here is that my mean difference is pretty small
#since an effect size of 0.8 is considered large (with 0.5 being medium and-
#and 0.2 being small), this tells me that mean difference I have been feeding-
#R have been unreasonible given the implicit assumptions of this test's-
#interpritation
#by this I mean that if small, medium, and large differences are 0.2, 0.5, and-
#0.8 respectivly, that just means that in most cases, we will see pretty small
#mean differences in reality (at least in the social sciences)
#the "rule of thumb interpretations don't have anything to do with the- 
#calculation, it is just based on what we are used to seing in reality

#study 5
cohen.d(study5$change.in.anxiety.index, study5$treatment)
#d= -3 (I gave the control a higher mean in this one, hense the negetive value)


#let's do this again but feed R more expected mean differences


#------------more simulated tests with smaller mean difference----------------

#study 1
treatment <- rep(c("Control", "CBD"), each= 100)
change.in.anxiety.index <- c(rnorm(100, 5, 2), rnorm(100, 6, 2))
study1 <- data.frame(treatment, change.in.anxiety.index)

#study 2
treatment <- rep(c("Control", "CBD"), each= 100)
change.in.anxiety.index <- c(rnorm(100, 5, 2), rnorm(100, 7, 3))
study2 <- data.frame(treatment, change.in.anxiety.index)

#study 3
treatment <- rep(c("Control", "CBD"), each= 100)
change.in.anxiety.index <- c(rnorm(100, 10, 2), rnorm(100, 12, 2.5))
study3 <- data.frame(treatment, change.in.anxiety.index)

#study 4
treatment <- rep(c("Control", "CBD"), each= 100)
change.in.anxiety.index <- c(rnorm(100, 50, 4), rnorm(100, 51, 4))
study4 <- data.frame(treatment, change.in.anxiety.index)

#study 5
treatment <- rep(c("Control", "CBD"), each= 100)
change.in.anxiety.index <- c(rnorm(100, 20, 2), rnorm(100, 19, 1.5))
study5 <- data.frame(treatment, change.in.anxiety.index)

#Cohens d for each sudy

#study 1
cohen.d(study1$change.in.anxiety.index, study1$treatment)
#d= 0.4

#study 2
cohen.d(study2$change.in.anxiety.index, study2$treatment)
#d= 0.8

#study 3
cohen.d(study3$change.in.anxiety.index, study3$treatment)
#d= 0.6

#study 4
cohen.d(study4$change.in.anxiety.index, study4$treatment)
#d= 0.3


#study 5
cohen.d(study5$change.in.anxiety.index, study5$treatment)
#d= -0.5 (I gave the control a higher mean in this one, hense the negetive-
#value)

#as we can see, our d-estimates are more closer to the expeced range (0.0-1.0)-
#than before. This "rule of thumb" intterpretation is based on the mean-
#difference being pretty small (evidently)

#in conclution, cohen's d is very unrelible when SD is large, it is very-
#sensitive to mean difference when SD is not that large, and sample size has-
#an effect but not that much