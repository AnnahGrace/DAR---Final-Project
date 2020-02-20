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