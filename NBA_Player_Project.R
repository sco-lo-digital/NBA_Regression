library(dplyr)
library(caret)
#Remove unneeded columns
x<-NBA_Salaries[,-9:-11]
x<-x[-1]
#Remove $ from data to make numberic
x$X2015.16<-gsub("[$]","",x$X2015.16)
x$X2016.17<-gsub("[$]","",x$X2016.17)
x$X2017.18<-gsub("[$]","",x$X2017.18)
x$X2018.19<-gsub("[$]","",x$X2018.19)
x$X2019.20<-gsub("[$]","",x$X2019.20)
#Coerce to numeric
x$X2015.16<-as.numeric(x$X2015.16)
x$X2016.17<-as.numeric(x$X2016.17)
x$X2017.18<-as.numeric(x$X2017.18)
x$X2018.19<-as.numeric(x$X2018.19)
x$X2019.20<-as.numeric(x$X2019.20)
#Subset those observations with salary in more than one year
y<-subset(x, X2016.17>0)
#Remove unneeded columns
salaries<-y[-1:-2]
#Create variable avgSalaries
avgSalaries<-rowMeans(salaries, na.rm=T)
#add avgSalaries to 
newSalaries<-cbind(y,avgSalaries)

#merge data sets and clean data
df<-merge(newSalaries, NBA_Per_GM_2014, by = "Player")
#Factor Team variable
df$Tm.x <- factor(df$Tm.x)
#remove the extra Team variable
df <- subset(df, select = -c(Tm.y) )
#remove observations with only 1 Salary value
newdf<-subset(df, X2016.17>0)
#Create SF data set
sfDf <- filter(newdf, Pos == "SF")
#Create PF data set
pfDf <- filter(newdf, Pos == "PF")
#Create SF & PF data set
spfDf <- filter(newdf, Pos == "SF" | Pos == "PF")
#SF tend to get paid more
boxplot(avgSalaries~Pos, data=spfDf)
#plot ppg 
with(spfDf, plot(sfDf$Age, sfDf$avgSalaries, col = as.factor(Pos)))
#plot age vs salary
plot(spfDf$Age, spfDf$avgSalaries)
#histogram of age
hist(sfDf$Age, main = "Histogram of Age")
#observe number of players per team
table(sfDf$Tm.x)
lmDf<- subset(spfDf, select = -c(Player,Rk, Tm.x,X2015.16, X2016.17, X2017.18, X2018.19, X2019.20) )
#plot pairs exploration
pairs(lmDf[3:8], main = "Data Exploration",col = as.factor(lmDf$Pos))
pairs(lmDf[9:15], main = "Data Exploration",col = as.factor(lmDf$Pos))
pairs(lmDf[16:22], main = "Data Exploration",col = as.factor(lmDf$Pos))

train_control <- trainControl(method="cv", number=10)
lmFit<-train(avgSalaries~., data = lmDf, method = 'lmStepAIC')
lmcvFit<-train(avgSalaries~., data = lmDf, trControl=train_control, method = 'lmStepAIC')
residuals<-resid(lmcvFit)
predictions <- predict(lmFit)
plot(varImp(lmcvFit))