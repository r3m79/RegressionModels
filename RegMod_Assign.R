####################################################
# Regression Models Assignment
####################################################
####################################################

## Regression Models Course Project
#
# In this project the assignment will be as follows:
# You work for Motor Trend, a magazine about the automobile industry. Looking at a data set of a collection of cars, 
#  they are interested in exploring the relationship between a set of variables and miles per gallon (MPG) (outcome). 
#  They are particularly interested in the following two questions:
#     - Is an automatic or manual transmission better for MPG?
#     - Quantify the MPG difference between automatic and manual transmissions?
#
# The following review criteria will apply
# 
# Did the student interpret the coefficients correctly?
# Did the student do some exploratory data analyses?
# Did the student fit multiple models and detail their strategy for model selection?
# Did the student answer the questions of interest or detail why the question(s) is (are) not answerable?
# Did the student do a residual plot and some diagnostics?
# Did the student quantify the uncertainty in their conclusions and/or perform an inference correctly?
# Was the report brief (about 2 pages long) for the main body of the report and no longer than 5 with supporting appendix of figures?
# Did the report include an executive summary?
# Was the report done in Rmd (knitr)?

#load libraries
library(dplyr)
library(ggplot2)

#Define Variables for simulation
set.seed(100) # set the seed value for reproducibility


#prepare data
mydata<-mtcars %>%
    mutate(transmission = ifelse(am=="0","A","M"))%>%
    mutate(am=NULL)

#plot data
plotdata <- ggplot(aes(x=as.factor(transmission), y=mpg,fill=transmission), data=mydata) + geom_boxplot()
plotdata <- plotdata + labs(title = "Automatic vs Manual Transmission")
plotdata <- plotdata + xlab("Transmission")
plotdata <- plotdata + ylab("MPG")
plotdata

#lets check the correlation between all variables
#Display Data Correlation
pairs(mpg~ . ,data=mtcars)

#we can see that cyl, disp, hp and wt seem to be the most correlated
#lets create linear model to verify exactly

#Linear Model MPG ~ transmission
lmd1<-lm(mpg~transmission,mydata)
summary(lmd1)

#Linear Model MPG ~ transmission + cyl + disp + hp + wt
lmd2<-lm(mpg~transmission + cyl + disp + hp + wt,mydata)
summary(lmd2)

#Linear Model MPG ~ All variables
lmd3<-lm(mpg~.,mydata)
summary(lmd3)

#lets now compare the 3 models
anova(lmd1,lmd2,lmd3)

#display residuals
par(mfrow=c(2,2))
plot(lmd2)
