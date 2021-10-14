#########################################################################

f0 <- matrix(c(14, 6, 7, 7, 7, 1), nrow=2, byrow=T) 
                  #Set up the data in a matrix of two rows

                  #First, calculate expected values

rs<-rowSums(f0)   #This method gives the sum of each row as vector
cs<-colSums(f0)   #And sum of each column, needed for expected values
total=sum(f0)     #Also need total of all elements in the data

rsxcs<-matrix(c(rs[1]*cs[1], rs[1]*cs[2], rs[1]*cs[3],
                rs[2]*cs[1], rs[2]*cs[2], rs[2]*cs[3]), 
              nrow=2, byrow=T)
                  #Set up a vector multiplying the row total by column
                  #total for each element in the data 
fe=rsxcs/total    #Next step is to divide by the total of all elements 

#I tried this in a for loop, but I couldn't make it to work:

   #for (i in 1:2){
    #    for (j in 1:3){    
    #        fe<-matrix(c(rs[i]*cs[j]/total))
    #    }
    #}

#Now use expected values to calculate chi-squared stat:

step1 <- f0 - fe        #Difference in f0 and expected values
step2 <- (step1**2)/fe  #Squared then divided by expected values
css <- sum(step2)       #Finally summed to give chi-squared stat
css

chisq<-chisq.test(f0) #And finally use R to check if all is correct
chisq


                        #Use R to generate p-value
pv<-pchisq(css, df = 2, lower.tail=FALSE)
                        
qchisq(0.1, df=2, lower.tail=FALSE) #Check which chi-squared value would
                        #correspond to alpha = 0.1

#########################################################################



chisq
#Standardised residual for f0[1,1]:

a <- 1-(rs[1]/total)     # 1 - row prop
b <- 1-(cs[1]/total)     # 1 - column prop
c<-(fe[1,1]*a*b)**0.5    # Denominator
stres1<- step1[1,1]/c    # f0 - fe from previous calculation

a <- 1-(rs[2]/total)     # 1 - row prop
b <- 1-(cs[3]/total)     # 1 - column prop
c<-(fe[2,3]*a*b)**0.5    # Denominator
stres6<- step1[2,3]/c    # f0 - fe from previous calculation

#########################################################################

women <- read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv")

lm_water_women <-lm(water~reserved, data = women)
summary(lm_water_women)

# Test statistic = (estimate of slope - zero)/standard error of slope
# Summary of linear regression gives slope = 9.252 with se = 3.948

ts <- 9.252/3.948

# 2-sided test, t-distribution
p_value_w <- 2*pt(ts, df = 322-2 , lower.tail = FALSE) 






























#105    
#########################################################################
fruitflies <- read.csv("http://stat2.org/datasets/FruitFlies.csv")

install.packages("tidyverse")
library(ggplot2)

boxplot(Longevity ~ Treatment, data = fruitflies, frame = FALSE)

ggplot(fruitflies, aes(x=Treatment, y=Longevity, color=Treatment)) + 
    geom_boxplot()+
    coord_flip()+
    labs(title="Plot of longevity by treatment",
         x="Treatment", y = "Longevity in days")+
    scale_x_discrete(limits=c("8 pregnant", 
                              "1 pregnant", 
                              "none", 
                              "1 virgin",
                              "8 virgin"))+
    theme(legend.position="none")

summary(fruitflies)

qplot(Sleep, Longevity, data = fruitflies, color = factor(Treatment))+
          labs(title="Plot of longevity against sleep by treatment")

qplot(Thorax, Longevity, data = fruitflies, color = factor(Treatment))+
    labs(title="Plot of longevity against length of thorax by treatment")

#########################################################################

# r = correlation coefficient 
# r = (covariance of x and y) divided by (sd of x * sd of y)

r <- cov(fruitflies$Longevity, fruitflies$Thorax)/
    (sd(fruitflies$Longevity)*sd(fruitflies$Thorax))

n <- 125                                 # Number of observations

test_stat <- (r*sqrt(n-2))/sqrt(1-r^2)   # Formula for test statistic

p_value <- 2*pt(test_stat, n-2, lower.tail = FALSE) 
                                         # 2-sided test, t-distribution

                                         # And now the quick way:
cor.test(fruitflies$Longevity, fruitflies$Thorax)

#########################################################################

# Regression on Longevity and Thorax

longevity_thorax <- lm(Longevity ~ Thorax, data = fruitflies)
summary(longevity_thorax)

ggplot(aes(Thorax, Longevity), data = fruitflies) +
    geom_point() +
    geom_smooth(method = "lm", formula = y ~ x)+
    labs(title ="Linear regression of longevity on thorax length",
         x = "Thorax length in mm",
         y = "Longevity in days")

#########################################################################

# Test statistic = (estimate of slope - zero)/standard error of slope

p_value_f <- 2*pt((144.33/15.77), df = 125-2 , lower.tail = FALSE) 
# 2-sided test, t-distribution

#########################################################################

# CI = estimated slope +/- tscore x standard error
# Calculate t-90 for df = 123

t90 <- qt(0.05, 123, lower.tail=FALSE)      # t90 = 1.66

lower_90 <- 144.33 - (t90*15.77)            # 1.66 se's below mean
upper_90 <- 144.33 + (t90*15.77)            # 1.66 se's above mean

# Check using confint:

confint(longevity_thorax, level = 0.9)

#########################################################################

#The newdata argument in predict() requires data a dataframe
nd <- data.frame(Thorax = 0.8)

#Find the mean of lifespans that the model associates with Thorax = 0.8mm 
predict(longevity_thorax, newdata= nd, se.fit=TRUE)

pred_int <- predict(longevity_thorax, newdata=nd, interval="prediction", level = 0.9)

conf_int <- predict(longevity_thorax, newdata=nd, interval="confidence", level = 0.9)

#########################################################################

# Generate thorax lengths and corresponding longevity, and combine

Thorax <- runif(125, 0.65, 0.94)      
Longevity <- -61.05 + Thorax*144.33    
nd2 <- as.data.frame(cbind(Thorax, Longevity))

# Generate predictions from nd2 

pred_int_2 <- predict(longevity_thorax, newdata=nd2, 
                      interval="prediction", level = 0.9)

# Will need one data frame to plot from, including a column for new
# thorax values. It cannot have the same column title though:

Thorax2 = Thorax
mydata <- cbind(fruitflies, pred_int_2, Thorax2)

# Regression line + confidence intervals from original data:

p <- ggplot(mydata, aes(Thorax, Longevity)) +
    geom_point() +
    stat_smooth(method = lm)

# Add prediction intervals based on new data:

p + geom_line(aes(x=Thorax2, y = lwr), color = "red", linetype = "dashed")+
    geom_line(aes(x =Thorax2, y = upr), color = "red", linetype = "dashed")+
    labs(title = "Confidence and prediction intervals", 
         x = "Thorax length in mm", y = "Longevity in days")
