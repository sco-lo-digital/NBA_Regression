library(dplyr)
library(caret)
library(stringr)
library(corrplot)

#write.csv(playerSalaryDf, file="finalNBA.csv")
finalNBA <- read.csv("~/Data_Science/NBA_Project/finalNBA2.csv", stringsAsFactors=FALSE)
finalNBA <- filter(finalNBA, Gms.Played>60)
finalNBA <- filter(finalNBA, Games.Started>30)
#Remove observatiosn with NA's
filteredNBA <-  finalNBA[complete.cases(finalNBA),]
filteredNBA <- filteredNBA[-1:-2]
head(filteredNBA)
summary(filteredNBA)

###################################Exploration###############################
#Multivariate Comparison
featurePlot(x = finalNBA[, 4:10],
            y = as.factor(finalNBA$Pos),
            plot = "pairs",
            auto.key = list(columns = 2))
featurePlot(x = finalNBA[, 11:17],
            y = as.factor(finalNBA$Pos),
            plot = "pairs",
            auto.key = list(columns = 2))
featurePlot(x = finalNBA[, 18:24],
            y = as.factor(finalNBA$Pos),
            plot = "pairs",
            auto.key = list(columns = 2))
featurePlot(x = finalNBA[, 32:36],
            y = as.factor(finalNBA$Pos),
            plot = "pairs",
            auto.key = list(columns = 2))
#Univariate Comparison
regVar <- c("PER", "True.Shoot.Pct", "FTr", "ORB.", "DRB.", "AST.", "STL.","BLK.","TOV.")
featurePlot(x = filteredNBA[, regVar],
            y = filteredNBA$Average_Salary,
            plot = "scatter",
            type = c("p", "smooth"),
            span = .5,
            layout = c(3, 3), main="Univariate Plots vs Average Salary")

regVar1 <- c("USG.", "OWS", "DWS", "WS.48", "OBPM", "DBPM", "VORP","Gms.Played","Games.Started")
featurePlot(x = filteredNBA[, regVar1],
            y = filteredNBA$Average_Salary,
            plot = "scatter",
            type = c("p", "smooth"),
            span = .5,
            layout = c(3, 3), main="Univariate Plots vs Average Salary")

regVar2 <- c("MP.y", "FG.", "X3P.1", "X2P.1", "eFG.", "FT.", "ORB","DRB","AST")
featurePlot(x = filteredNBA[, regVar2],
            y = filteredNBA$Average_Salary,
            plot = "scatter",
            type = c("p", "smooth"),
            span = .5,
            layout = c(3, 3), main="Univariate Plots vs Average Salary")

regVar3 <- c("STL", "BLK", "TOV", "PF", "PTS", "Age")
featurePlot(x = filteredNBA[, regVar3],
            y = filteredNBA$Average_Salary,
            plot = "scatter",
            type = c("p", "smooth"),
            span = .5,
            layout = c(2, 3), main="Univariate Plots vs Average Salary")

#######
#Plot TSP vs Salary
#library(car)
#scatterplot(Average_Salary~PER | Pos, smoother=gamLine, data=finalNBA)
#scatterplot(Average_Salary~WS.48 | Pos, smoother=gamLine, data=finalNBA)

#Check for non-zero variance
nzv <- nearZeroVar(filteredNBA, saveMetrics= TRUE)#None observed
nzv#non-zero variance not a problem
#create correlation matrix
descrCor <-  cor(filteredNBA)
#identify high correlation column
highCorr <- sum(abs(descrCor[upper.tri(descrCor)]) > .8)
#identify high correlation at lower cutoff
highlyCorDescr <- findCorrelation(descrCor, cutoff = .90)
identifyPlot1 <- corrplot(descrCor, type="lower", tl.pos ="ld", tl.cex = .58)


# #Create New data set without the highly correlated variables
# filteredNBA2 <- filteredNBA[,-highlyCorDescr]
# #create a correlation matrix to verify
# descrCor2 <- cor(filteredNBA2)
# #display new summary of correlations
# summary(descrCor2[upper.tri(descrCor2)])
# #plot correlations
# identifyPlot2 <- corrplot(descrCor2, type="lower", tl.pos ="ld", tl.cex = .58)

#identify co-linearity
comboInfo <- findLinearCombos(filteredNBA)
comboInfo#No co-linearities


###################MODEL BUILDING###################
#create cross validation parameter
train_control <- trainControl(method="repeatedcv", number=20, repeats=2 )
#fit with cross validation
lmcvFit<-train(Average_Salary~., data = filteredNBA, trControl=train_control, method = 'lmStepAIC')
#Traditional NBA Metrix
TradFit<-train(Average_Salary~PTS+BLK+TOV+PF+Age+ORB+DRB, data = filteredNBA, trControl=train_control, method = 'lmStepAIC')
summary(TradFit)
#Advanced NBA Metrics
PERFit <- lm(Average_Salary~PER+USG.+VORP+OWS+Age, data=filteredNBA)
PERFitCV <- train(Average_Salary~PER+USG.+VORP+OWS+Age, data=filteredNBA, trControl=train_control, method = 'lmStepAIC')
summary(PERFitCV)

#Compare across models
results <- resamples(list(AdvMetrics=PERFitCV, TradMetrics=TradFit, KitchenSink=lmcvFit))
summary(results)
bwplot(results)
dotplot(results)

#plot important variables (based on t scores)
plot(varImp(PERFitCV))
#residuals appear to be normally distributed
residuals<-resid(PERFitCV)
plot(residuals)

#Harrison PER FIT
HB_PER <- data.frame(PER=13.4,USG.= 14.9, VORP=1.8, OWS=3.7, Age=22)
HB_Trad <- data.frame(PTS=827, BLK=19,TOV=71,PF=146, Age=22, ORB=117,DRB=136)


#create predictions of all salaries for chosen model
predictions <- predict(PERFitCV)
actual<-filteredNBA$Average_Salary
accuracyPlot<-data.frame(cbind(actual, predictions))
plot(accuracyPlot, main="Accuracy Plot")

#Create predictions for each model
PER_Salary <- predict(PERFitCV, newdata = HB_PER)
Sink_Salary <- predict(lmcvFit, newdata = HB_PER)
Trad_Salary <- predict(TradFit, newdata=HB_Trad)
#summary(predict(PERFitCV$finalModel, filteredNBA, interval = "confidence"))
