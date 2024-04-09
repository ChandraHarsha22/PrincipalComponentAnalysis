#R Finals, Clean Code

library(tidyverse)
library(factoextra)
library(readxl)
library(lmtest)
library(ggplot2)
library(haven)
library(sandwich)
library(car)
library(plm)
library(AER)

data <- read_excel("C:/Users/harsh/OneDrive/Desktop/FALL 2022/Data Mining with R/Homework/Finals/AssetProxy_Data.xlsx")

data_desc <- read_excel("C:/Users/harsh/OneDrive/Desktop/FALL 2022/Data Mining with R/Homework/Finals/AssetProxy_Data.xlsx", sheet = "Variables")

View(data)

mydroplist <- c("HHoldCode","PerEqGCMln_Real","LogPerEqGCMln_Real")

mydata <- data[,!(names(data) %in% mydroplist)]
View(mydata)

#mydata <- subset(data, select=-c(HHoldCode,PerEqGCMIn_Real,LogPerEqGCMIn_Real))

summary(mydata)

mean(mydata$Area)#mean of area

table(mydata$Sex_0M1F)#returns counts of men and women

x <- mydata%>%filter(Urb1Rur0==1)%>%select(Area)
mean(x$Area)

table(mydata$CapitalUrban)

mypca <- prcomp(mydata,scale=T) #dataframe with results for pca

#scree plot to visualize percentage of variance in the data from different components

fviz_screeplot(mypca,labels=TRUE)

#to get the contribution of each factor within the different dimensions
mypca_PCs <- get_pca_var(mypca)

#x <- as.data.frame(mypca_PCs$contrib)#weight contributions of each variable

fviz_pca_var(mypca,labels=T)
fviz_contrib(mypca, choice = "var", axes = 1)
fviz_contrib(mypca, choice = "var", axes = 2)

mypca_PCs_coord <- data.frame(mypca_PCs$coord)

View(mypca_PCs_coord)# to see the 31 x 31 matrix of PCA coordinates

mypca_PC1_3_coord <- data.frame(mypca_PCs_coord[,1:3])#to extract only first three PCs from the matrix above

View(mypca_PC1_3_coord)

mypca_Indiv <- get_pca_ind(mypca)#coordinates at individual level 

mypca_Indiv_coord <- data.frame(mypca_Indiv$coord)

View(mypca_Indiv_coord)

#getting only the first column

myHholds_PCs <- data.frame(PC1=mypca_Indiv_coord$Dim.1,PC2 =mypca_Indiv_coord$Dim.2,PC3=mypca_Indiv_coord$Dim.3)#create a dataframe with first 3 principal components.

View(myHholds_PCs)

data1_3 <-data.frame(data[1:3])#Take the first three rows of our original data

finaldata <- cbind(data1_3,myHholds_PCs)#column bind the two dataframes to get one clean dataframe with first three variables, and our first three PCs.
View(finaldata)

#Regressions to generate model for predictions

reg1 <- lm(data=finaldata, LogPerEqGCMln_Real ~ PC1) #using just PC1 - Adjusted R-squared - 40.7%
summary(reg1)

finaldata$PredIncome_1 <- fitted(reg1)

reg2 <- lm(data=finaldata, LogPerEqGCMln_Real ~ PC1 + PC2 + PC3) #using just PC1-3: Adjusted R-squared - 42.05%
summary(reg2)

finaldata$PredIncome_2 <- fitted(reg2)

View(finaldata)

corr1 <- cor.test(finaldata$PredIncome_1,finaldata$LogPerEqGCMln_Real,method="pearson")
corr1 #63.8% correlation between predicted income 1 and log of income

corr2 <- cor.test(finaldata$PredIncome_2,finaldata$LogPerEqGCMln_Real,method="pearson")
corr2 #64.9% correlation between predicted income 2 and log of income

corr3 <-cor.test(finaldata$PredIncome_1,finaldata$PredIncome_2,method="pearson")
corr3 # 98.4% correlation between the two predicted incomes

#Plots of Histograms

#Real Income
plot_income <- ggplot(finaldata, aes(x=PerEqGCMln_Real))+
  geom_histogram(color='green', bins = 70,fill='orange')+
  xlab("Income")+
  ggtitle("Distribution of real income")
plot_income

#log of income
plot_log_income <- ggplot(finaldata, aes(x=LogPerEqGCMln_Real))+
  geom_histogram(color='green', bins = 70,fill='orange')+
  xlab("LogIncome")+
  ggtitle("Distribution of log of income")
plot_log_income

#Distribution of PC1
#PC1
plot_PC1 <- ggplot(finaldata, aes(x=PC1))+
  geom_histogram(color='green', bins = 70,fill='orange')+
  xlab("PC1")+
  ggtitle("Distribution of PC1")
plot_PC1

#Distribution of PC1 - PC3
#PC1 + PC2 + PC3
plot_PC1_3 <- ggplot(finaldata, aes(x=PC1+PC2+PC3))+
  geom_histogram(color='green', bins = 70,fill='orange')+
  xlab("PC1")+
  ggtitle("Distribution of PC1+PC2+PC3")
plot_PC1_3

#Scatter plot of logincome vs. predicted income
plot_x <- ggplot(finaldata, aes(y=LogPerEqGCMln_Real,x=PredIncome_1))+
  geom_point(color='green')+
  xlab("LogIncome")+
  ylab("PredIncome")+
  ggtitle("Plot of LogIncome vs. Predicted Income")
  
plot_x

write.csv(finaldata,file="C:/Users/harsh/OneDrive/Desktop/FALL 2022/Data Mining with R/Homework/Finals/finaldata.csv")

write.csv(mypca_PC1_3_coord,file="C:/Users/harsh/OneDrive/Desktop/FALL 2022/Data Mining with R/Homework/Finals/pca_weights.csv")






