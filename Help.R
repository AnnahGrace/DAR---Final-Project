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

#------------------everything stops working but I don't know it yet-----------

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


#----I've noticed to boxplots are all the same--------------------------

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


#--------------------teting where the issue is---------------------

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




