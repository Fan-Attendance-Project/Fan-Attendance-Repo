setwd("C:/Users/stanma02/Desktop/Final Project/WebScraper and Data/Capstone Data")
mydata <- read.csv(file="StadiumWinRate.csv",head=TRUE,sep=",")
str(mydata)

summary(mydata)

require(corrplot)

subdat <- subset(mydata, select= -c(X, League, Season, Venue, VAR, HomeGoal, AwayGoal, 
                                    HomePassesCompleted, HomePassesAttempts,HomeShotsonTarget, HomeShots,
                                    AwayPassesCompleted, AwayPassesAttempts, AwayShotsonTarget, AwayShots))

summary(subdat)
mcor<-cor(subdat)
corrplot(mcor, method="shade",addCoef.col = "black",shade.col=NA, tl.col="black",tl.srt = 45, tl.cex = 0.75,)

library(lessR)

hist(subdat$HomeResult)
subdat <- subset(mydata, select= -c(X, Season, Venue, VAR, HomeGoal, AwayGoal, 
                                    HomePassesCompleted, HomePassesAttempts,HomeShotsonTarget, HomeShots,
                                    AwayPassesCompleted, AwayPassesAttempts, AwayShotsonTarget, AwayShots))

table(subdat$League)
subdat$dummyBundesliga<-ifelse(subdat$League == "Bundesliga", 1,0)
subdat$dummyEPL<-ifelse(subdat$League == "EPL", 1,0)
subdat$dummyLaLiga=ifelse(subdat$League == "LaLiga", 1,0)
subdat$dummyLigue1=ifelse(subdat$League == "Ligue1", 1,0)
subdat$dummySerieA=ifelse(subdat$League == "SerieA", 1,0)

#subdat$Attendance <- as.numeric(scale(subdat$Attendance))
#subdat$Stadium_Capacity <- as.numeric(scale(subdat$Stadium_Capacity))

str(subdat)

subdat2<-subset(subdat, select= -c(League))
mcor<-cor(subdat2)
corrplot(mcor, method="shade",addCoef.col = "black",shade.col=NA, tl.col="black",tl.srt = 45, tl.cex = 0.75,)

#########################################Test/Train Split#######################################

my.data <- subdat2
set.seed(123)
my.data$u <- runif(n=dim(my.data)[1],min=0,max=1)

# Create train/test split;
train.df <- subset(my.data, u<0.80);
test.df  <- subset(my.data, u>=0.80);

# Check your data split. The sum of the parts should equal the whole.
# Do your totals add up?
dim(my.data)[1]
dim(train.df)[1]
dim(test.df)[1]
dim(train.df)[1]+dim(test.df)[1]

drop.list<-c('u');
#drop.list<-c('u');
train.clean <- train.df[,!(names(my.data) %in% drop.list)]
test.clean <- test.df[,!(names(my.data) %in% drop.list)]

####################################################Training##################################################
model1<-lm(HomeResult~.,data=train.clean)
model1
summary(model1)
par(mfrow=c(2,2))
plot(model1)
anova(model1)

model1RMSE<-sqrt((c(crossprod(model1$residuals)))/length(model1$residuals))

model2<-lm(HomeResult~StadiumUtilization+ Stadium_Capacity + AwayFouls + AwayTackles + AwayInterceptions+ AwayYellow 
           +GoalsScored + HomePassAccuracy + HomeShotAccuracy + AwayPassAccuracy+ AwayShotAccuracy+ dummyBundesliga 
           + dummyEPL + dummyLaLiga + dummyLigue1,data=train.clean)
model2
summary(model2)
par(mfrow=c(2,2))
plot(model2)
anova(model2)

model2RMSE<-sqrt((c(crossprod(model2$residuals)))/length(model2$residuals))

model3<-lm(HomeResult~StadiumUtilization+ Stadium_Capacity + AwayFouls + AwayTackles + AwayYellow 
           +GoalsScored + HomePassAccuracy + HomeShotAccuracy + AwayPassAccuracy+ AwayShotAccuracy+ dummyBundesliga 
           + dummyEPL + dummyLaLiga + dummyLigue1,data=train.clean)
model3
summary(model3)
par(mfrow=c(2,2))
plot(model3)
anova(model3)

model3RMSE<-sqrt((c(crossprod(model3$residuals)))/length(model3$residuals))

model4<-lm(HomeResult~StadiumUtilization+ Stadium_Capacity + AwayFouls + AwayTackles + AwayYellow 
           +GoalsScored + HomePassAccuracy + HomeShotAccuracy + AwayPassAccuracy+ AwayShotAccuracy ,data=train.clean)
model4
summary(model4)
par(mfrow=c(2,2))
plot(model4)
anova(model4)

model4RMSE<-sqrt((c(crossprod(model4$residuals)))/length(model4$residuals))

model5<-lm(HomeResult~StadiumUtilization+ Stadium_Capacity + AwayFouls + AwayTackles + AwayYellow 
           + HomePassAccuracy + HomeShotAccuracy + AwayPassAccuracy+ AwayShotAccuracy ,data=train.clean)
model5
summary(model5)
par(mfrow=c(2,2))
plot(model5)
anova(model5)

model5RMSE<-sqrt((c(crossprod(model5$residuals)))/length(model5$residuals))

model6<-lm(HomeResult~StadiumUtilization + AwayFouls + AwayTackles + AwayYellow 
           + HomePassAccuracy + HomeShotAccuracy + AwayPassAccuracy+ AwayShotAccuracy ,data=train.clean)
model6
summary(model6)
par(mfrow=c(2,2))
plot(model6)
anova(model6)

model6RMSE<-sqrt((c(crossprod(model6$residuals)))/length(model6$residuals))

model7<-lm(HomeResult~StadiumUtilization + AwayYellow 
           + HomePassAccuracy + HomeShotAccuracy + AwayPassAccuracy+ AwayShotAccuracy ,data=train.clean)
model7
summary(model7)
par(mfrow=c(2,2))
plot(model7)
anova(model7)

model7RMSE<-sqrt((c(crossprod(model7$residuals)))/length(model7$residuals))

#########################################Model Testing#######################################
# Model 1 on Training    
pdata <- predict(model1, newdata = train.clean, type = "response")
HomeResult<-train.clean$HomeResult
residuals<- HomeResult - pdata
hist(residuals)
plot(x=pdata,y=residuals)
M1RMSE<-sqrt((sum(residuals))^2/length(residuals))

# Model 1 on Testing    
pdata <- predict(model1, newdata = test.clean, type = "response")
HomeResult<-test.clean$HomeResult
residuals<- HomeResult - pdata
hist(residuals)
plot(x=pdata,y=residuals)
M1TestRMSE<-sqrt((sum(residuals))^2/length(residuals))

# Model 2 on Training    
pdata <- predict(model2, newdata = train.clean, type = "response")
HomeResult<-train.clean$HomeResult
residuals<- HomeResult - pdata
hist(residuals)
plot(x=pdata,y=residuals)
M2RMSE<-sqrt((sum(residuals))^2/length(residuals))

# Model 2 on Testing    
pdata <- predict(model2, newdata = test.clean, type = "response")
HomeResult<-test.clean$HomeResult
residuals<- HomeResult - pdata
hist(residuals)
plot(x=pdata,y=residuals)
M2TestRMSE<-sqrt((sum(residuals))^2/length(residuals))

# Model 3 on Training    
pdata <- predict(model3, newdata = train.clean, type = "response")
HomeResult<-train.clean$HomeResult
residuals<- HomeResult - pdata
hist(residuals)
plot(x=pdata,y=residuals)
M3RMSE<-sqrt((sum(residuals))^2/length(residuals))

# Model 3 on Testing    
pdata <- predict(model3, newdata = test.clean, type = "response")
HomeResult<-test.clean$HomeResult
residuals<- HomeResult - pdata
hist(residuals)
plot(x=pdata,y=residuals)
M3TestRMSE<-sqrt((sum(residuals))^2/length(residuals))

# Model 4 on Training    
pdata <- predict(model4, newdata = train.clean, type = "response")
HomeResult<-train.clean$HomeResult
residuals<- HomeResult - pdata
hist(residuals)
plot(x=pdata,y=residuals)
M4RMSE<-sqrt((sum(residuals))^2/length(residuals))

# Model 4 on Testing    
pdata <- predict(model4, newdata = test.clean, type = "response")
HomeResult<-test.clean$HomeResult
residuals<- HomeResult - pdata
hist(residuals)
plot(x=pdata,y=residuals)
M4TestRMSE<-sqrt((sum(residuals))^2/length(residuals))

# Model 5 on Training    
pdata <- predict(model5, newdata = train.clean, type = "response")
HomeResult<-train.clean$HomeResult
residuals<- HomeResult - pdata
hist(residuals)
plot(x=pdata,y=residuals)
M5RMSE<-sqrt((sum(residuals))^2/length(residuals))

# Model 5 on Testing    
pdata <- predict(model5, newdata = test.clean, type = "response")
HomeResult<-test.clean$HomeResult
residuals<- HomeResult - pdata
hist(residuals)
plot(x=pdata,y=residuals)
M5TestRMSE<-sqrt((sum(residuals))^2/length(residuals))

# Model 6 on Training    
pdata <- predict(model6, newdata = train.clean, type = "response")
HomeResult<-train.clean$HomeResult
residuals<- HomeResult - pdata
hist(residuals)
plot(x=pdata,y=residuals)
M6RMSE<-sqrt((sum(residuals))^2/length(residuals))

# Model 6 on Testing    
pdata <- predict(model6, newdata = test.clean, type = "response")
HomeResult<-test.clean$HomeResult
residuals<- HomeResult - pdata
hist(residuals)
plot(x=pdata,y=residuals)
M6TestRMSE<-sqrt((sum(residuals))^2/length(residuals))

# Model 7 on Training    
pdata <- predict(model7, newdata = train.clean, type = "response")
HomeResult<-train.clean$HomeResult
residuals<- HomeResult - pdata
hist(residuals)
plot(x=pdata,y=residuals)
M7RMSE<-sqrt((sum(residuals))^2/length(residuals))

# Model 7 on Testing    
pdata <- predict(model7, newdata = test.clean, type = "response")
HomeResult<-test.clean$HomeResult
residuals<- HomeResult - pdata
hist(residuals)
plot(x=pdata,y=residuals)
M7TestRMSE<-sqrt((sum(residuals))^2/length(residuals))


########### Save best model#################
finalmodel<-model5
summary(finalmodel)

saveRDS(finalmodel, file="finalmodel.rds")
