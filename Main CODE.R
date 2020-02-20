#===============================File Storage=============================

#get working directory
wd <- getwd()
wd

# folders for storing data outputs and figures
# store names of the folders in an object
output.folder.names <- c("figures", "data.output")
# and make the folders if they don't exist yet. 
for(i in 1:length(output.folder.names)) 
  if(file.exists(output.folder.names[i]) == FALSE) 
    dir.create(output.folder.names[i])

#path to figures folder
path.figures <- paste(wd,"/",output.folder.names[1],"/", sep = "")

#path to data output folder
path.data.output <- paste(wd,"/",output.folder.names[2],"/", sep = "")


#===============================Data Simulation=============================

#--------------rnorm()-----------------

#rnorm generates random numbers in a normal distribution
#rnorm(n, mean = x, sd = y)
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
rnorm(n = 10, mean = c(0, 5, 20), sd = 1)

#now vary the sd as well
rnorm(n = 10, mean = c(0, 5, 20), sd = c(1, 5, 20) )

#use rnorm() to run a test
#check for coorilations between two randomly generated vectors
#(run whole sequence several times, just for fun)
x = rnorm(n = 10, mean = 0, sd = 1)
y = rnorm(n = 10, mean = 0, sd = 1)
plot(y ~ x)
cor(y, x)


#--------------runif()-----------------

#runif() generates possitive, continuous numbers within a range
#runif(n, min= x, max= y)
#runif(n) assumes min= 0 and max= y
runif(5)
runif(5, min= 50, max= 70)

#use runif() to run a test
#response variable= y, explanitory variable= x1
y = rnorm(n = 100, mean = 0, sd = 1)
x1 = runif(n = 100, min = 1, max = 2)
head(x1)

#create a second explanitory variable (x2)
x2 = runif(n = 100, min = 200, max = 300)
head(x2)

#fit to linear model
lm(y ~ x1 + x2)
#note that the coefficent for x2, which has a larger magnitude, is smaller
#the change in y for a "1-unit-increase" in x is reletive to the units/
#magnitude of x


#--------------rpois()-----------------

#rpois() generates random discrete integers from a poisson distribution


