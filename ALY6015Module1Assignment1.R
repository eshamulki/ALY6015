install.packages("dplyr")
install.packages("ggplot2")
install.packages("corrplot")
install.packages("RcolorBrewer")
install.packages("car")
install.packages("psych")
install.packages("Hmisc")
install.packages("tidyverse")
install.packages("corrplot")
install.packages("caret")
install.packages("VIM")
install.packages("mice")
install.packages("olsrr")
install.packages("caret")
install.packages("leaps")
install.packages("MASS")
install.packages("broom")

library(dplyr)
library(ggplot2)
library(corrplot)
library(RColorBrewer)
library(car)
library(psych)
library(Hmisc)
library(tidyverse)
library(corrplot)    
library(caret)
library(VIM)           
library(mice)          
library(olsrr)
library(caret)
library(leaps)
library(MASS)
library(broom)


housing <- read.csv("AmesHousing.csv")
summary(housing)


# Checking if there is any missing values
sapply(housing, function(x)sum(is.na(x)))
str(housing)
summary(housing$SalePrice)

num = unlist(lapply(housing,function(x) is.numeric(x)))

names(housing[ , num])

data <- housing[c("Lot.Frontage","Lot.Area","Overall.Qual","Overall.Cond","Mas.Vnr.Area","Total.Bsmt.SF","X1st.Flr.SF",
                     "Gr.Liv.Area","TotRms.AbvGrd","Garage.Area", "SalePrice")]

summary(data)

data$Lot.Area <- as.numeric(data$Lot.Area)
data$Overall.Qual <- as.numeric(data$Overall.Qual)
data$X1st.Flr.SF <- as.numeric(data$X1st.Flr.SF)
data$Gr.Liv.Area <- as.numeric(data$Gr.Liv.Area)
data$TotRms.AbvGrd <- as.numeric(data$TotRms.AbvGrd)
data$SalePrice <- as.numeric(data$SalePrice)


sd(data)
var(data)

ggplot(mydata,aes(SalePrice)) +
  geom_histogram(bins = 100)

#To remove the null values in the 

lapply(data,function(x) sum(is.na(x)))

for(col in names(data)){
  
  if(sum(is.na(data[,col]))>0 ){
    
    data[is.na(data[,col]),col]=mean(data[,col],na.rm=T)
  }
}

view(data)

#copy of the dataset
mydata <- data

#Correlation Plot and correlation matrix

cor1 = cor(data)
corrplot.mixed(cor1, lower.col = 'black', number.cex = .7)


corPlot(data,method = c("square"), type = c("full"))

cor(data, method = "spearman")

corPlot(data, type="upper", order="hclust", method = 'number')

corrplot(cor(data), main = "Correlations for Ames Housing")

model <- lm(SalePrice ~ Overall.Qual+Gr.Liv.Area+Garage.Area+Total.Bsmt.SF+X1st.Flr.SF, data = mydata)
summary(model)


ols_vif_tol(model)
ols_coll_diag(model)

ols_plot_obs_fit(model)


#Highest Correlation

ggplot(data, aes(x=Overall.Qual, y=SalePrice)) +
  geom_point(color="Orange",
            # fill="yellow",
             shape=21,
             alpha=0.5,
             stroke=2)

#Lowest Correlation

ggplot(data, aes(x=Overall.Cond, y=SalePrice)) +
  geom_point(color="red",
             fill="Yellow",
             shape=21,
             alpha=0.5,
             stroke=2)

#50 percent Correlation

ggplot(data, aes(x=TotRms.AbvGrd, y=SalePrice)) +
  geom_point(color="green",
             fill="#69b3a2",
             shape=21,
             alpha=0.5,
             stroke=2)

ggplot(data, aes(x=Total.Bsmt.SF, y=SalePrice)) +
  geom_point(color="green",
             fill="#69b3a2",
             shape=21,
             alpha=0.5,
             stroke=2)

ggplot(data, aes(x=Garage.Area, y=SalePrice)) +
  geom_point(color="blue",
             fill="#69b3a2",
             shape=21,
             alpha=0.5,
             stroke=2)


# Boxplot for checking the outliers

data %>% 
  ggplot(aes(x=Overall.Qual)) +
  geom_boxplot(outlier.colour="red", outlier.shape=16,
               outlier.size=2)

data %>% 
  ggplot(aes(x=Gr.Liv.Area)) +
  geom_boxplot(outlier.colour="red", outlier.shape=16,
               outlier.size=2)

data %>% 
  ggplot(aes(x=Garage.Area)) +
  geom_boxplot(outlier.colour="red", outlier.shape=16,
               outlier.size=2)

data %>% 
  ggplot(aes(x=Total.Bsmt.SF)) +
  geom_boxplot(outlier.colour="red", outlier.shape=16,
               outlier.size=2)

data %>% 
  ggplot(aes(x=X1st.Flr.SF)) +
  geom_boxplot(outlier.colour="red", outlier.shape=16,
               outlier.size=2)

data %>% 
  ggplot(aes(x=SalePrice)) +
  geom_boxplot(outlier.colour="red", outlier.shape=16,
               outlier.size=2)



#Removing the outlier for sales Price
boxplot.stats(data$SalePrice)$out
out <- boxplot.stats(data$SalePrice)$out
out_ind <- which(data$SalePrice %in% c(out))

data[out_ind,]

data <- data[-out_ind, ]

#Removing the outlier for Gr.Liv.Area column

boxplot.stats(data$Gr.Liv.Area)$out

out <- boxplot.stats(data$Gr.Liv.Area)$out
out_ind <- which(data$Gr.Liv.Area %in% c(out))

data[out_ind,"Gr.Liv.Area"]

data <- data[-out_ind, ]

#Removing the outlier for Garage Area column

boxplot.stats(data$Garage.Area)$out

out <- boxplot.stats(data$Garage.Area)$out
out_ind <- which(data$Garage.Area %in% c(out))

data[out_ind, ]
data <- data[-out_ind, ]

# #Removing the outlier for Total.Bsmt.SF column
# 
# boxplot.stats(data$Total.Bsmt.SF)$out
# 
# out <- boxplot.stats(data$Total.Bsmt.SF)$out
# out_ind <- which(data$Total.Bsmt.SF %in% c(out))
# 
# data[out_ind,"Total.Bsmt.SF" ]
# data <- data[-out_ind, ]

#Removing the outlier for X1st.Flr.SF column

boxplot.stats(data$X1st.Flr.SF)$out

out <- boxplot.stats(data$X1st.Flr.SF)$out
out_ind <- which(data$X1st.Flr.SF %in% c(out))

data[out_ind, "X1st.Flr.SF"]
data <- data[-out_ind, ]

model1 <- lm(SalePrice ~ Overall.Qual+Gr.Liv.Area+Garage.Area+Total.Bsmt.SF+X1st.Flr.SF, data = data)
summary(model1)
plot(model1)
broom::glance(model1)

#Testing by removing one variable at a time

model2 <- lm(SalePrice ~ Overall.Qual+Gr.Liv.Area+Garage.Area+Total.Bsmt.SF, data = data)
summary(model2)
broom::glance(model2)

model3 <- lm(SalePrice ~ Overall.Qual+Gr.Liv.Area+Garage.Area, data = data)
summary(model3)
broom::glance(model3)

model4 <- lm(SalePrice ~ Overall.Qual+Gr.Liv.Area, data = data)
summary(model4)
broom::glance(model4)

model5 <- lm(SalePrice ~ Overall.Qual+Gr.Liv.Area+Garage.Area+Total.Bsmt.SF, data = data)
summary(model5)
broom::glance(model5)


#Using the pairs function

model6 <- pairs(data = data, ~SalePrice+Overall.Qual+Gr.Liv.Area+Garage.Area+Total.Bsmt.SF+X1st.Flr.SF)
summary(model6)

# Collinearity test

ols_vif_tol(model1)
ols_coll_diag(model1)

ols_plot_obs_fit(model1)

#Regression plot for data

my.formula <- y ~ x
ggplot(mydata, aes(x = Overall.Qual+Gr.Liv.Area+Garage.Area+Total.Bsmt.SF+X1st.Flr.SF, y = SalePrice)) +
  geom_point(alpha = .5,color="blue") +
  geom_smooth(method = "lm", se = FALSE, color = "red", formula = my.formula) +
  labs(title = "Regression plot before removing outliers")

my.formula <- y ~ x
ggplot(data, aes(x = Overall.Qual+Gr.Liv.Area+Garage.Area+Total.Bsmt.SF+X1st.Flr.SF, y = SalePrice)) +
  geom_point(alpha = .5,color="green") +
  geom_smooth(method = "lm", se = FALSE, color = "red", formula = my.formula)+
  labs(title = "Regression plot after removing outliers")
  

#Stepwise regression in R

# Fit the full model 
full.model <- lm(SalePrice ~.-X1st.Flr.SF, data = data)
summary(full.model)
# Stepwise regression model
step.model <- stepAIC(full.model, direction = "forward", trace = FALSE)
summary(step.model)



models <- regsubsets(SalePrice~., data = data, nvmax = 5)
summary(models)

res.sum <- summary(models)
data.frame(
  Adj.R2 = which.max(res.sum$adjr2),
  CP = which.min(res.sum$cp),
  BIC = which.min(res.sum$bic)
)

my.formula <- y ~ x
ggplot(data, aes(x = Lot.Frontage+Lot.Area+Overall.Qual+Overall.Cond+Mas.Vnr.Area+Total.Bsmt.SF+X1st.Flr.SF+Gr.Liv.Area+TotRms.AbvGrd+Garage.Area, 
                   y = SalePrice)) +
  geom_point(alpha = .5,color="orange") +
  geom_smooth(method = "lm", se = FALSE, color = "red", formula = my.formula) +
  labs(title = "Regression plot for stepwise regression")







