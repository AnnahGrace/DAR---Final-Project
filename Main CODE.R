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
















