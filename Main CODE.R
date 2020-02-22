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

#------------2 random samples with the same mean and variance-------------

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


#------------2 random samples with the same mean and variance-------------

#create two random samples with different means and-
#the same variance
c <- rnorm(10, 10)
d <- rnorm(10, 15)

#hypothisies are the same as above

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


aov <- data.frame(groups, c(e, f))
names(aov)[2]<-paste("numbers")
levels(aov$groups)

#visualize data
boxplot(numbers ~ groups, data = aov)

#compute an ANOVA
res.aov <- aov(numbers ~ groups, data = aov)
summary(res.aov)

#let's make a function thats stips out a count of how many resaults out of 100-
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
#reasonible accuracy when n=100


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




