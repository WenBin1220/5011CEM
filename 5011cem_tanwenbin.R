df <-read.csv ("C:/Users/Tan Wen Bin/Documents/5011/dataset/Apr_borough_grocery.csv")

# S1 - Read csv with data.table
library(data.table)
library(tidyverse)
library(microbenchmark)

df <- function(i){
  library(data.table)
  library(tidyverse)
  library(microbenchmark)
  list.files(path = "C:/Users/Tan Wen Bin/Documents/5011/", pattern = "*.csv") %>%
    map_df(~fread(.))
}

# Sequential time taken using micro benchmark
mbm1 <- microbenchmark(lapply(1:100, df))
mbm1
library(ggplot2)
autoplot(mbm1)
library(dplyr)


# Parallel time taken using micro benchmark===========================================================
df2 <- function(i){
  library(data.table)
  library(tidyverse)
  library(microbenchmark)
  list.files(path = "C:/Users/Tan Wen Bin/Documents/5011/", pattern = "*.csv") %>%
    map_df(~fread(.))
}

df3 <- function(i){
  library(parallel)
  cl <- makeCluster(detectCores())
  clusterEvalQ(cl, library(lme4))
  parLapply(cl, 1:100, df2)
  stopCluster(cl)
}

mbm2 <- microbenchmark(lapply(1:100, df2), df3())

mbm2
library(ggplot2)
autoplot(mbm2)

# S2 - Read csv with data.table======================================================================
#Import Dataset
#Grocery
year_osward_grocery<-read.csv("C:/Users/Tan Wen Bin/Documents/5011/dataset/year_osward_grocery.csv")

#Health Data
diabetes_estimates_osward_2016<-read.csv("C:/Users/Tan Wen Bin/Documents/5011/diabetes_estimates_osward_2016.csv")

#Cleanse (Select required variables)
a1 <- merge(diabetes_estimates_osward_2016, year_osward_grocery, by.x = "area_id")


# Correlation between the numerical data 
mosthighlycorrelated <- function(mydataframe,numtoreport)
{
  cormatrix <- cor(mydataframe)
  diag(cormatrix) <- 0
  cormatrix[lower.tri(cormatrix)] <- 0
  fm <- as.data.frame(as.table(cormatrix))
  names(fm) <- c("First.Variable", "Second.Variable","Correlation")
  head(fm[order(abs(fm$Correlation),decreasing=T),],n=numtoreport)
}
model2 <- select(a1, 'male', 'female', 'estimated_diabetes_prevalence')
mosthighlycorrelated(model2, 20)


# Correlation Visualization===========================================================================
library ("ggpubr")
ggscatter(a1, x = "male", y = "estimated_diabetes_prevalence", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "male", ylab = "prevalence of diabetes")


library ("ggpubr")
ggscatter(a1, x = "female", y = "estimated_diabetes_prevalence", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "female", ylab = "prevalence of diabetes")


# Hypothesis testing code==============================================================================
diabetesMale <- lm(estimated_diabetes_prevalence ~ male, data = a1)
print(summary(diabetesMale))

diabetesFemale <- lm(estimated_diabetes_prevalence ~ female, data = a1)
print(summary(diabetesFemale))

#Regression Analysis===================================================================================

#Comsumer age between 0 to 17
# Create the predictor and response variable.
x <- c(a1$male)
y <- c(a1$estimated_diabetes_prevalence)
relation <- lm(y~x)

# Plot the chart.
plot(y,x,col = "blue",main = "Prevalence of diabetes for Male  ",
     abline(lm(x~y)),cex = 1.3,pch = 16,xlab = "Number of male",ylab = "Prevalence of diabetes")

# Save the file.
dev.off()

#Comsumer age between 18 to 64
# Create the predictor and response variable.
x <- c(a1$female)
y <- c(a1$estimated_diabetes_prevalence)
relation <- lm(y~x)

# Plot the chart.
plot(y,x,col = "blue",main = "Prevalence of diabetes for female  ",
     abline(lm(x~y)),cex = 1.3,pch = 16,xlab = "Number of female",ylab = "Prevalence of diabetes")

# Save the file.
dev.off()

