#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

install.packages("car")
install.packages("tidyverse")
library("car")
library(tidyverse)
# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

# set working directory
setwd("C:/Users/doire/Desktop/ASDS/QM1/StatsI_Fall2021-main/problemSets/PS01")


#####################
# Problem 1
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

# n < 30 therefore use t-distribution for df = 24. CI will be wider than normal dist,
# which compensates for uncertainty arising from small n.

n <- length(y)                             # Sample size
mean_y <- mean(y)                          # Mean of sample
sd_y <- sd(y)                              # Standard dev of sample
se_y <- (sd_y/sqrt(n))                     # Estimated SE of sample
t90 <- qt(0.05, 24, lower.tail = FALSE)    # t-0.05 =  1.711 from tables 
lower_90 <- mean_y - (t90*se_y)            # 1.711 se's below mean
upper_90 <- mean_y + (t90*se_y)            # 1.711 se's above mean
CI90_for_students <- c(lower_90, upper_90)

# For a quick sense-check/comparison, we can run a t test from R, this is 95% CI.
# I expect CI90 to be marginally more narrow than CI95.

t.test(y)

# My un-rounded CI90 fits narrowly within CI95, sense checked.

# Test how average IQ in school compares to national average.
# National average has mean = 100.
# Small sample significance test.
# H_0: mean <= 100, H_alpha: mean > 100
# One-sided test

test_stat <- (mean_y - 100)/se_y                 #Calculate test statistic
P_value <- pt(abs(test_stat), 24, lower.tail = TRUE)

# P_value = 0.7 therefore we do not reject the null hypothesis
# Let's plot the probability distribution of sample means

IQ <- seq(0, 150, by=0.001)                       # Input vector
plot(IQ, dnorm(x=x.range, mean=mean_y, sd=sd_y),  # I have used dnorm
     type="l",                                    # Specify line type     
     main="Probability density for sample means", # Title
     ylab="density",                              # Label y-axis
     lwd=2,                                       # Specify type of line
     xaxt="n")                                    
axis(1, at=seq(0,150,by=20), labels=seq(0,150,by=20)) # x-axis up in 20's 
abline(v=mean_y, col="blue")    # Add a blue line at the sample mean




#####################
# Problem 2
#####################

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2021/main/datasets/expenditure.txt", header=T)

pairs(expenditure[,2:5], lower.panel=NULL,
      main = "Correlation matrix for data in 'expenditure' data set") #Matrix of correlations within 2nd-5th variables

scatter.smooth(expenditure$Y, expenditure$X1, ylab="Per capita income", xlab = "Per capita expenditure on housing assistance")
cor(expenditure$Y, expenditure$X1)
scatter.smooth(expenditure$Y, expenditure$X2)
cor(expenditure$Y, expenditure$X2)
scatter.smooth(expenditure$Y, expenditure$X3)
cor(expenditure$Y, expenditure$X3)
scatter.smooth(expenditure$X1, expenditure$X2)
cor(expenditure$X1, expenditure$X2)
scatter.smooth(expenditure$X1, expenditure$X3, ylab="Urban (per thousand)", xlab="Income per capita")
cor(expenditure$X1, expenditure$X3)
scatter.smooth(expenditure$X2, expenditure$X3)
cor(expenditure$Y, expenditure$X1)

Y_means_by_region <- aggregate(expenditure$Y, by = list(expenditure$Region), FUN = mean)
barplot(Y_means_by_region$x,
        main = "Mean of per capita spending by region",
        names.arg = Y_means_by_region$Group.1,
        ylab = "Mean per capita spending",
        xlab = "Region")

scatter.smooth(expenditure$Y, expenditure$X1, ylab="Per capita income", xlab = "Per capita expenditure on housing assistance")


scatterplot(Y ~ X1|Region, 
            data = expenditure,
            smooth = FALSE, grid = FALSE, regLine=FALSE,
            ylab= "Per capita expenditure on housing assistance",
            xlab = "Personal income per capita")


