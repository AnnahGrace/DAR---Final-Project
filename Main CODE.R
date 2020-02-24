#========================================================================
#===============================INDEX====================================
#========================================================================
#1.) Working Directory, File Storage, and Package Instalation
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
#6.) Forest Plots

#=====================WD, File Storage, and Package Instalattion================
#------------------------------WD and File Storage-------------------------

#get working directory
wd <- getwd()
wd

#folders for storing data outputs and figures
#store names of the folders in an object
output.folder.names <- c("figures", "data.output")
#make the folders (if they don't exist already) 
for(i in 1:length(output.folder.names)) 
  if(file.exists(output.folder.names[i])== FALSE) 
    dir.create(output.folder.names[i])

#path to figures folder
path.figures <- paste(wd,"/",output.folder.names[1],"/", sep= "")

#path to data output folder
path.data.output <- paste(wd,"/",output.folder.names[2],"/", sep= "")

#----------------------------Package Insalation--------------------------------

#effsize for calculating effect size
install.packages("effsize")
library(effects)
#activate/call function for cohen's d
cohen.d <- effsize::cohen.d

#forestplot for creating forest plots
install.packages("forestplot")
library(forestplot)


#===============================Data Simulation=============================
#--------------rnorm()-----------------

#rnorm generates numeric continous vectors

#rnorm generates random numbers in a normal distribution
#rnorm(n, mean= x, sd= y)
#rnor(n) assumes a standard normal distribution (mu=0, SD=1)

#example of default
rnorm(5)
#examle with defined mean and SD
rnorm(5, mean= 50, sd= 5)

#set.seed() for creating a reproducible, randomly generated vector
#example with 2 seeds (run each in order and when you run the seed a second-
#time, you will get the same numbers as you did the first time)
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
#this pulls numbers witth mean 0, then 5, then 20, and then 0, 5, and 20 again

#now vary the sd as well
rnorm(n= 10, mean= c(0, 5, 20), sd= c(1, 5, 20))
#same pattern as above (so in this case, mu=0 has SD= 1, mu= 5 has SD= 5, etc.)

#use rnorm() to run a test
#check for coorilations between two randomly generated vectors
#(highlight and run whole sequence several times, just for fun)
x= rnorm(n= 10, mean= 0, sd= 1)
y= rnorm(n= 10, mean= 0, sd= 1)
plot(y ~ x)
cor(y, x)


#--------------runif()-----------------

#runif() generates numeric continous vectors

#runif() generates possitive, continuous numbers within a range
#runif(n, min= x, max= y)
#runif(n) assumes min= 0 and max= 1

#example of defaut
runif(5)
#example with defined range
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
#unsurprisingly, we do not see much of a corilation here


#--------------rpois()-----------------

#rpois() generates numeric discrete vectors

#rpois() generates random discrete integers from a poisson distribution
#rpois(n, lambda) (where lambda is the mean and cannot be left blacnk)

#example
rpois(5, lambda= 2.5)

#explore the poisson distribution
y= rpois(100, lambda= 5)
summary(y)
hist(y)
#note that it is right skewed (in this case), not normal
#note also that the skew will be different everytime I/anyone runs this-
#section of code

#more exploration
y= rpois(100, lambda= 100)
summary(y)
hist(y)
#note that this disttribution is more symentrical but still not normal


#--------------rep() and letters[]-----------------

#rep() repeats number and letter generation sequences
#letters[] generates charecter vectors

#letters[x:y] (or LETTERS[x:y] for capittals)
#print groups or levels labled with the letters of the alphabet
letters[1:26]
LETTERS[3:17]
#note that if your range is larger than 26, you get NA's after 26

#repete each element of a vector n times with rep()
rep(thing to repete, n)
#there are several calsses of "n" which dictate how rep() replicates
#when the second input's class is unspecified, it defaults to "times" 
rep(1:10, 2)

#times= repeat the whole vector in order n times
rep(letters[1:2], times= 3)

#each= repeat each element n times in order of elements
rep(letters[1:2], each= 3)

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

#add another catagorical variable to the dataset
#we will have a single observation for every pair of the first and secon-
#catagirical variables
data.frame(group= rep(letters[1:2], each= 3),
           factor= rep(LETTERS[3:5], times= 2),
           response= rnorm(6, mean= 0, sd= 1))

#____________________________________________________________________________
###create a data frame with differnces between groups###
#(different mean and or SD)
response <- rnorm(6, mean= c(5, 10), sd= 1)
group <- rep(letters[1:2], length.out= 6)
data.frame(group, response)
#this gives "a" a mean of 5 and "b" a mean of 10

#create a data frame with multiple numeric variables
data.frame(group <- rep(LETTERS[3:4], each= 10),
           x <- runif(20, min= 10, max= 15),
           y <- runif(20, min= 100, max= 150))
#it is spread out in the console like that because the rown names are huge

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

#null hypohisis: a and b are from the same population/their means are not- 
#significantly different
#alternitive hypothisis: a and b are not from the same population/their means- 
#are significantly different

#run a t.test
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

#run t.test
t.test(c, d)
#t= -11.919
#df= 13.508
#p-value= 1.505e-08
#with this p-value, we can reject the null

#how close can my means get before I get a lot of error (false posiives)?

#make a funcion to automate rapid trial and error
rapid.t.mu <- function(mean1, mean2){
  #create an empty data frame to collect outputs
  p.guiness <- data.frame(NA)
  #create a loop to run 100 t-tests, all with different randomly generated-
  #numbers
  for (i in 1:100) {
    c <- rnorm(10, mean1)
    d <- rnorm(10, mean2)
    guiness <- t.test(c, d)
    #save the p-value of each into my empty data frame
    p.guiness[i, 1] <- guiness$p.value
  }
  #look and p.guiness (contains p-values) and place all significat values into-
  #a seperate vector
  sig <- p.guiness <= 0.05
  #print the number of p-values (out of 100) that are significant (<0.05)
  print(length(which(sig == TRUE)))
}

#test out a bunch of different means to see when the sig count drops below-
#~95 (this would indicate that it is reading a difference in means as not-
#significantly different 95% of the time)

rapid.t.mu(10, 14) # 100 sigs
rapid.t.mu(10, 13) # 100 sigs
rapid.t.mu(10, 12) # 98 sigs
rapid.t.mu(10, 11) # 43 sigs
rapid.t.mu(10, 10.5) # 18 sigs
#note that the presise sig count will be each time you run these but they will-
#be pretty close the the sigs recorded next to the functions in this code
#when SD is 1, it looks like t-tests can reliably detect a difference in means-
#when the difference is ~2

#if we give it the same means, it should read a difference ~5% of the time
#test that theory
rapid.t.mu(10, 10) # 6 sigs
#yes it does

#lets make adapt my first fuction so we can play with SD as well
rapid.t.mu.SD <- function(mean1, mean2, SD1, SD2){
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

#lets play with SD when means are equal
rapid.t.mu.SD(10, 10, 1, 15) #5 sigs
rapid.t.mu.SD(10, 10, 1, 20)#8 sigs
rapid.t.mu.SD(10, 10, 1, 100) #5 sigs
#very little effect

#what about when our means are unequal
rapid.t.mu.SD(10, 12, 1, 15) #8 sigs
rapid.t.mu.SD(10, 12, 1, 20) #4 sigs
rapid.t.mu.SD(10, 12, 1, 100) #6 sigs
#still very little effect
#upon looking at the formula for t-tests online again, I would expect SD to-
#have a noticible effect on significance only when my sample size is very large

#lets test that theory
rapid.t.mu.SD(100, 100, 1, 1) #8 sigs
rapid.t.mu.SD(100, 100, 1, 15) #8 sigs
rapid.t.mu.SD(100, 100, 1, 20) #4 sigs
rapid.t.mu.SD(100, 100, 1, 100) #2 sigs
rapid.t.mu.SD(100, 100, 1, 200) #2 sigs
#This is still not having much of an effect

#Upon looking at the formula again, I feel like sample size and means are the-
#only things that will have a large effect on how many sigificant resaults-
#this thing pumps out

#lets test that
#faddapt my function to allow me to chose sample size and means
rapid.t.n.mu <- function(n1, n2, mean1, mean2){
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

#lets test how many times it reads significance when n and mu are the same
rapid.t.n.mu(10, 10, 10, 10) #4 sig
rapid.t.n.mu(100, 100, 10, 10) #6 sig
#these are false possitives (we would expect 5/100)

#lets check with different means
rapid.t.n.mu(10, 10, 10, 11) #58 sig
rapid.t.n.mu(100, 100, 10, 11) #100 sig
#it can detect a smaller mean difference with larger sample size
rapid.n(10, 10, 10, 12) #99 sig
rapid.n(100, 100, 10, 12) #100 sig
#but when SD= 1 and the mean difference is 2, sample size does not have much-
#of an effect


#what does my sig count look like as I get closer to a mean difference of 0-
#(check when n= 10 and when n= 100)

rapid.t.n.mu(10, 10, 10, 10.6) #25 sig
rapid.t.n.mu(100, 100, 10, 10.6) #100 sig

rapid.t.n.mu(10, 10, 10, 10.5) #26 sig
rapid.t.n.mu(100, 100, 10, 10.5) #92 sig

rapid.t.n.mu(10, 10, 10, 10.4) #14 sig
rapid.t.n.mu(100, 100, 10, 10.4) #74 sig

rapid.t.n.mu(10, 10, 10, 10.3) #12 sig
rapid.t.n.mu(100, 100, 10, 10.3) #57 sig

rapid.t.n.mu(10, 10, 10, 10.2) #9 sig
rapid.t.n.mu(100, 100, 10, 10.2) #28 sig

rapid.t.n.mu(10, 10, 10, 10.1) #9 sig
rapid.t.n.mu(100, 100, 10, 10.1) #11 sig

#This shows that t-tests can detect smaller effect sizes with confidencewhen n-
#is larger



















#============================Playing with ANOVA=========================
#-------------------------one way ANOVA using rnorm-----------------------------

#play with the basics:

#create a vectors with groups a, b, and c where half of the values for each-
#fall between 1 and 50, and half fall within 50 and 100
groups <- rep(letters[1:3], length= 10)
e<- runif(15, 1, 50)
f<- runif(15, 50, 100)

#null hypothisis: groups have the same mean
#alternitive hypothisis: groups do not have the same mean

#make a data frame from the above vectors and check the levels
aov <- data.frame(groups, c(e, f))
names(aov)[2]<-paste("numbers")
levels(aov$groups)

#save data as a CSV
write.csv(aov, paste(path.data.output,'ANOVA-OneWay',sep = ''))

#visualize and save the data

#start saving
pdf(paste(path.figures, paste("ANOVA-BoxPlot-OneWay"), sep = ""),
    width = 5, height = 5)
#construct boxplot
boxplot(numbers ~ groups, data = aov)
#stop saving
dev.off()

#compute an ANOVA
res.aov <- aov(numbers ~ groups, data = aov)
summary(res.aov)


#get into functions

#let's make a function thats spits out a count of how many resaults out of 100-
#trials have a significant p-value (same as in the t-test section, essentially)
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
#when SD= 2 and n= 10, it is about 2 (same as what we saw from our t-tests)

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

#in conclution, you can detect smaller and smaller mean differences as n-
#increases but this change in sensitivity is not linear. It gets-
#proportionally less sensitive as n increases
#------------------------one way ANOVA using runif-----------------------------

#lets now run the same tests as in the last section (one way ANOVA using rnom)-
#but lets use runif this time
#runif generates non-normal data which violates the basic assumptoion of ANOVA-
#but some authors suggest that this shouldn't matter if your sample size is-
#larger than ~30

#make the function
rapid.aov.r <- function(min1, max1, min2, max2, min3, max3){
  p.aov <- data.frame(matrix(NA, ncol=2, nrow=100))
  names(p.aov)[1]<-paste("p-value")
  #now the letters print one at a time, 10 times (a*10, b*10, c*10)
  groups <- rep(letters[1:3], each= 10)
  for (i in 1:100) {
    #create three groups of runif so that I can vary the values for each-
    #leter group
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

rapid.aov.r(1, 10, 3, 12, 5, 14) #87 sigs
#the medians here ^ are 5, 7, and 9

rapid.aov.r(1, 10, 2.5, 11.5, 4, 13) #46 sigs
#the medians here ^ are 5, 6.5, and 8

rapid.aov.r(1, 10, 2, 11, 3, 12) #28 sigs
#the medians here ^ are 5, 6, and 7

#so this can detect an effect size/difference of means of about 2 with-
#reasonible accuracy when n=10
#there is no SD here but this is pretty similar to our previous tests where-
#n= 10 and SD= 2



#------------------Try out two way ANOVA--------------------------------------

#make a fake data set that is compaible with two way ANOVA (2 charecter-
#vectors with three levels each and one numeric vector)
dose <- rep(rep(c(1, 2, 3), each= 10), length= 90)
condition <- rep(c("no sick", "sick", "extra sick"), each= 30)
#lets do n= 10 per condition and dose (90 total because 3 levels*10n*3 more-
#levels)
change.in.level.of.sick <- rnorm(90, 0, 2)
sick.test <- data.frame(condition, dose, change.in.level.of.sick)

#save data as a CSV
write.csv(sick.test, paste(path.data.output,'ANOVA-TwoWay',sep = ''))

#visualize data
boxplot(change.in.level.of.sick ~ dose, data = sick.test)
boxplot(change.in.level.of.sick ~ condition, data = sick.test)

#visualize both level groups at one
pdf(paste(path.figures, paste("ANOVA-BoxPlot-TwoWay"), sep = ""),
    width = 5, height = 5)
cols <- rainbow(3, s = 0.5)
BoxPlot <- boxplot(change.in.level.of.sick ~ dose + condition, data = sick.test,
                   at = c(1:3, 5:7, 9:11), col = cols,
                   names = c(NA, "No Sick", NA, NA, "Sick", NA, NA, "Exrta sick", NA), 
                   xaxs = FALSE,
                   las=1,
                   main= "Effects of Dose by Initial Level of Sick",
                   ylab= "Change in Level of Sick",
                   xlab= "Inittial level of Sick")
legend("topleft", fill = cols, legend = c(1,2,3), horiz = T)
dev.off()
#As we might expect given that I used random number generation, there doesn't-
#look like there is a lot of significant differences going on here

#compute an ANOVA
res.aov <- aov(change.in.level.of.sick ~ dose + condition, data = sick.test)
summary(res.aov)

#how to get two p values out
p.aov.2 <- data.frame(matrix(NA, ncol=3, nrow=100))
p.aov.2[i, 1:3] <- summary(res.aov)[[1]][["Pr(>F)"]]


#lets go ahead and make a function (runs 100 two-way ANOVA's and sptis out-
#a sig count)
#this version of my two-way function allows me to change the means for each-
#"level of sick" but not for each dose
aov.2w.3w <- function(m1, m2, m3){
  p.aov <- data.frame(matrix(NA, ncol=3, nrow=100))
  names(p.aov)[1]<-paste("p-value dose")
  names(p.aov)[2]<-paste("p-value condition")
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
  #print the sigs for dose
  print(length(which(sig == TRUE)))
  #print the sigs for condition
  print(length(which(sig2 == TRUE)))
}

#now lets make a nine way function
#this function lets me change the mean for every dose in every level of sick
aov.2w.9w <- function(m1, m2, m3, m4, m5, m6, m7, m8, m9){
  p.aov <- data.frame(matrix(NA, ncol=3, nrow=100))
  names(p.aov)[1]<-paste("p-value dose")
  names(p.aov)[2]<-paste("p-value condition")
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

#lets get sigs for just conditions, not dose (3 way function)
aov.2w.3w(10, 50, 100) # 2, 100 sigs (it prints the dose count first)
#with a CI of 95%, we expect this function to countt ~5 sigs in dose dispite-
#there being no mean difference

#lets get sigs for both (using the 9 way)
aov.2w.9w(10, 20, 30, 40, 50, 60, 70, 80, 90) # 100, 100 sigs

#what if there is a difference in dose in one group but no difference at all-
#in the other two groups (wih differen sizes of mean differnce)?
aov.2w.9w(10, 20, 30, 20, 20, 20, 20, 20, 20) # 100, 0 sigs (0= condition)
aov.2w.9w(1, 2, 3, 2, 2, 2, 2, 2, 2) # 23, 7 sigs
aov.2w.9w(1, 3, 5, 3, 3, 3, 3, 3, 3) # 70, 0 sigs
aov.2w.9w(1, 4, 7, 4, 4, 4, 4, 4, 4) # 93, 3 sigs
#so, when n= 90 (each does only exists 30 times, and only 10 times per- 
#condition), we can reliably detect a difference in means outcome in dosag- 
#down to about a mean difference of 3

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
t.test(CBD, Control)
#p-value= 0.0003

#create equivilent data set for EF calculation
treatment <- rep(c("Control", "CBD"), each= 5)
change.in.anxiety.index <- c(rnorm(5, 5), rnorm(5, 10))
anxiety.test <- data.frame(treatment, change.in.anxiety.index)

#Run cohen's d
cohen.d(change.in.anxiety.index, treatment)
#that is rediculously large... lets try to figure out what is causing that

#try again with a larger n
treatment <- rep(c("Control", "CBD"), each= 50)
change.in.anxiety.index <- c(rnorm(50, 5), rnorm(50, 10))
anxiety.test <- data.frame(treatment, change.in.anxiety.index)
#Run cohen's d
cohen.d(change.in.anxiety.index, treatment)
#Still implausibly large

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
#so we are seing a lot of variattion in effect size based on the random-
#normal generations (rnorm)

#lets just up SD a lot (because cohen's d seems to be most sensitive to SD)
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
#this is giving me d estimates that range from large to negligible

#Lets make SD even less equal
treatment <- rep(c("Control", "CBD"), each= 5)
change.in.anxiety.index <- c(rnorm(5, 5, 1), rnorm(5, 6, 10))
anxiety.test <- data.frame(treatment, change.in.anxiety.index)
#Run cohen's d
cohen.d(change.in.anxiety.index, treatment)
#large range
#it is very sensitive to SD

#what if I have large mean difference and large variance (checking if it is an-
#absolute thing or a proportional thing)
treatment <- rep(c("Control", "CBD"), each= 50)
change.in.anxiety.index <- c(rnorm(50, 5, 15), rnorm(50, 6, 15))
anxiety.test <- data.frame(treatment, change.in.anxiety.index)
#Run cohen's d
cohen.d(change.in.anxiety.index, treatment)
#small range (negligible to small reads)

#now small mean difference and small variance
treatment <- rep(c("Control", "CBD"), each= 50)
change.in.anxiety.index <- c(rnorm(50, 5, 1), rnorm(50, 5.2, 1))
anxiety.test <- data.frame(treatment, change.in.anxiety.index)
#Run cohen's d
cohen.d(change.in.anxiety.index, treatment)
#small range (negligible to small reads)
#it seems to be proportional

#What if we up the actual size of the means (small mean dif)
treatment <- rep(c("Control", "CBD"), each= 50)
change.in.anxiety.index <- c(rnorm(50, 50, 15), rnorm(50, 51, 15))
anxiety.test <- data.frame(treatment, change.in.anxiety.index)
#Run cohen's d
cohen.d(change.in.anxiety.index, treatment)
#small range (negligible to small reads)

#What if we up the actual size of the means (big mean dif)
treatment <- rep(c("Control", "CBD"), each= 50)
change.in.anxiety.index <- c(rnorm(50, 50, 15), rnorm(50, 60, 15))
anxiety.test <- data.frame(treatment, change.in.anxiety.index)
#Run cohen's d
cohen.d(change.in.anxiety.index, treatment)
#medium range (small to large reads)


#what about when SD is different
treatment <- rep(c("Control", "CBD"), each= 50)
change.in.anxiety.index <- c(rnorm(50, 5, 10), rnorm(50, 6, 20))
anxiety.test <- data.frame(treatment, change.in.anxiety.index)
#Run cohen's d
cohen.d(change.in.anxiety.index, treatment)
#small range (negligible to small reads)

#what about when mean difference is large and SD is the same
treatment <- rep(c("Control", "CBD"), each= 50)
change.in.anxiety.index <- c(rnorm(50, 5, 15), rnorm(50, 10, 15))
anxiety.test <- data.frame(treatment, change.in.anxiety.index)
#Run cohen's d
cohen.d(change.in.anxiety.index, treatment)
#medium range (negligible to small reads)

#what about when meand difference is large and SD is different
treatment <- rep(c("Control", "CBD"), each= 50)
change.in.anxiety.index <- c(rnorm(50, 5, 10), rnorm(50, 10, 20))
anxiety.test <- data.frame(treatment, change.in.anxiety.index)
#Run cohen's d
cohen.d(change.in.anxiety.index, treatment)
#small range (small to medium reads)

#NOTE: I am geting these "ranges" by higlighting the section of code and-
#running it repeatedly (since the number are random, if you do this 5-10 times-
#for each one, it will not matter much that I am not using set.seed())

#in conclution, n has a proportional effect, mean difference has some effect-
#and SD has the most significant effect on the read of effect size


#--------------with hodge's g correction--------------------------------

#there is an optional correction you can place in cohens.d() that corrects for-
#upward bias(default is FALSE)
#lets turn it on for some t-test-esq daa

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

#as we can see, our d-estimates are closer to the expeced range (0.0-1.0)-
#than before. This "rule of thumb" interpretation is based on the mean-
#difference being pretty small (evidently)

#in conclution, cohen's d is very unrelible when SD is large, it is very-
#sensitive to mean difference when SD is not that large, and sample size has-
#an effect but not that much
#also, the hodge's g correction has a very small effect (at leastt here)


#=============================Forest Plots====================================
#---------------------------createt a matrix-------------------------------------

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
change.in.anxiety.index <- c(rnorm(100, 50, 4), rnorm(100, 52, 4))
study4 <- data.frame(treatment, change.in.anxiety.index)

#study 5
treatment <- rep(c("Control", "CBD"), each= 100)
change.in.anxiety.index <- c(rnorm(100, 19, 2), rnorm(100, 20, 1.5))
study5 <- data.frame(treatment, change.in.anxiety.index)

#save data for each study
write.csv(study1, paste(path.data.output,'FP-Study1',sep = ''))
write.csv(study2, paste(path.data.output,'FP-Study2',sep = ''))
write.csv(study3, paste(path.data.output,'FP-Study3',sep = ''))
write.csv(study4, paste(path.data.output,'FP-Study4',sep = ''))
write.csv(study5, paste(path.data.output,'FP-Study5',sep = ''))


#Cohens d for each sudy 
#place every d-estimate, upper, and lower limits in a data frame

#study 1
cd1 <- cohen.d(study1$change.in.anxiety.index, study1$treatment)
cde <- data.frame(cd1$estimate)
cde[, 2] <- cd1$conf.int[1]
cde[, 3] <- cd1$conf.int[2]
names(cde)[1] <- "d.est"
names(cde)[2] <- "lower"
names(cde)[3] <- "upper"

#study 2
cd2 <- cohen.d(study2$change.in.anxiety.index, study2$treatment)
cde[2, 1] <- cd2$estimate
cde[2, 2] <- cd2$conf.int[1]
cde[2, 3] <- cd2$conf.int[2]

#study 3
cd3 <- cohen.d(study3$change.in.anxiety.index, study3$treatment)
cde[3, 1] <- cd3$estimate
cde[3, 2] <- cd3$conf.int[1]
cde[3, 3] <- cd3$conf.int[2]

#study 4
cd4 <- cohen.d(study4$change.in.anxiety.index, study4$treatment)
cde[4, 1] <- cd4$estimate
cde[4, 2] <- cd3$conf.int[1]
cde[4, 3] <- cd3$conf.int[2]

#study 5
cd5 <- cohen.d(study5$change.in.anxiety.index, study5$treatment)
cde[5, 1] <- cd5$estimate
cde[5, 2] <- cd3$conf.int[1]
cde[5, 3] <- cd3$conf.int[2]

#-------------------------------make a forest plot--------------------------

#see "show your work" CODE if you want to see how I got here


#adding row names
#row names
cde[1, 4] <- "Study 1"
cde[2, 4] <- "Study 2"
cde[3, 4] <- "Study 3"
cde[4, 4] <- "Study 4"
cde[5, 4] <- "Study 5"

#add in an "overal" row to my studies data frame
cde[6, 1] <- mean(cde$d.est)
cde[6, 2] <- mean(cde$lower[1:5])
cde[6, 3] <- mean(cde$upper[1:5])
cde[6, 4] <- "Overal"

#create forest plot

#save range of x-axis ticks in a vector (for ease of use in the function)
xticks <- seq(from= 0, to= 1, by= 0.1)
#start saving as PDF
pdf(paste(path.figures, paste("FP-Forest Plot"), sep = ""))
#forest plot where the 4th column of the df sets the study names, the 1st-
#column lists the effect sizes (in cohen's d), the 2nd column lists the lower-
#limits of each ES, and the 3rd lists with upper limits of each ES
forestplot(cde[, 4], cde[, 1], cde[, 2], cde[, 3],
           #add a summary showing the average mean and range of all studies
           is.summary= c(rep(FALSE, 5), TRUE),
           #play with the colours
           col= fpColors(box= "black",line= "black", summary= "royalblue"),
           #set the end of the range lines to be sideways T's
           ci.vertices = TRUE,
           #set a line to run verticly from the mean ES
           grid = structure(mean(cde$d.est), 
                            #make that line dashed and red
                            gp = gpar(lty = 2, col = "firebrick")),
           #add an x-axis lable
           xlab= "Effect Size (est. d)",
           #set the x-axis ticks to "xicks" vector created earlier
           xticks= xticks,
           #this puts arrows on the ends of the range lines if the range-
           #extends beyond the length of the x-axis
           clip= 1)
#stop saving
dev.off()