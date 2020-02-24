#NOTE: I am giving myself an input for all n's, mu's, and sd's for maximum-
#customization and to avoid needing to make multiple functions
rapid.aov.norm <- function(n1, n2, n3, mu1, mu2, mu3, sd1, sd2, sd3){
  p.aov <- data.frame(matrix(NA, ncol=2, nrow=100))
  names(p.aov)[1]<-paste("p-value")
  #now the letters print one at a time, 10 times (a*10, b*10, c*10)
  groups <- rep(letters[1:3], each= 10)
  for (i in 1:100) {
    #create three groups of rnorm so that I can varry the values for each-
    #leter group
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
#again, it looks like a mean difference of two is the minimum when n= 10 and-
#SD= 2

#lets trty with SD=1
rapid.aov.norm(10, 10, 10, 5, 10, 15, 1, 1, 1) # 100 sigs
rapid.aov.norm(10, 10, 10, 5, 7, 9, 1, 1, 1) # 100 sigs
rapid.aov.norm(10, 10, 10, 5, 6, 7, 1, 1, 1) # 99 sigs
rapid.aov.norm(10, 10, 10, 5, 5.5, 6, 1, 1, 1) # 53 sigs
#and when SD is one, it can detect a mean difference of 1

#This code does not work for a sample size above 10 because of how I have the-
#letter generation set up so I am making a new function to correct for this
#(see "show your work" code to see how I figured this out)

#(I am making a new function becasue I do not have the time to rewrite all-
#of those trials)
#adjust previous function