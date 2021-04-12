library(rpart)
library(rpart.plot)
library('scales')
library(tree)
library(dplyr)
library(Rmisc)
library(corrplot)
library(naniar)
library(nlme)


setwd("C:/Project/Data")
mytypes <- c('character','factor',rep('character',2),'factor',rep('character',2),rep('factor',7),'character',rep('numeric',18),rep('factor',2),rep('numeric',6))
mynames <- c('CCN','state','name','address','city','Zip','phone','type','off.nursing','off.physical','off.occupational','off.speech','off.medical','off.hha','date','rating','ER','taughtdrugs','checkfall','checkdepression','flushot','pnumococcal','taughtfootcare','betterwalking','betterbed','betterbathing','lesspain','betterbreathing','betterheal','betterdrug','admitted','ER','episode','star','year','season','timeindex','median','mean','pop','ruca')
df <- read.csv("completedata.csv",sep=',')
dim(df)
names(df)

table(df$ruca)
df$ruca[df$ruca==1] <- "Urban"
df$ruca[df$ruca!='Urban'] <- "Rural"

table(df$ruca)

dummyruca <- as.factor(df$ruca)
df$ruca <- dummyruca

df[df==199|df==201]<-NA
df[df==5030353|df==16665138]<-NA
sum(is.na(df))
names(df)

str(df)

df$off.nursing[df$off.nursing == 'Y'] <- 1
df$off.nursing[df$off.nursing == 'Yes'] <- 1
df$off.physical[df$off.physical == 'Y'] <- 1
df$off.physical[df$off.physical == 'Yes'] <- 1
df$off.physical[df$off.physical == 'N'] <- 0
df$off.physical[df$off.physical == 'No'] <- 0

df$off.occupational[df$off.occupational == 'Y'] <- 1
df$off.occupational[df$off.occupational == 'Yes'] <- 1
df$off.occupational[df$off.occupational == 'N'] <- 0
df$off.occupational[df$off.occupational == 'No'] <- 0

df$off.speech[df$off.speech == 'Y'] <- 1
df$off.speech[df$off.speech == 'Yes'] <- 1
df$off.speech[df$off.speech == 'N'] <- 0
df$off.speech[df$off.speech == 'No'] <- 0

df$off.medical[df$off.medical == 'Y'] <- 1
df$off.medical[df$off.medical == 'Yes'] <- 1
df$off.medical[df$off.medical == 'N'] <- 0
df$off.medical[df$off.medical == 'No'] <- 0

df$off.hha[df$off.hha == 'Y'] <- 1
df$off.hha[df$off.hha == 'Yes'] <- 1
df$off.hha[df$off.hha == 'N'] <- 0
df$off.hha[df$off.hha == 'No'] <- 0


df$off.physical <- as.factor(df$off.physical)
df$off.occupational <- as.factor(df$off.occupational)
df$off.speech <- as.factor(df$off.speech)
df$off.medical <- as.factor(df$off.medical)
df$off.hha <- as.factor(df$off.hha)
df$type <- as.factor(df$type)
df$CCN <- as.factor(df$CCN)
df$star <- as.factor(df$star)
df$year <- as.factor(df$year)

#The datatypes of columns currently present in the dataframe 
data_types <- function(frame) {
  res <- lapply(frame, class)
  res_frame <- data.frame(unlist(res))
  barplot(table(res_frame), main="Data Types", col="steelblue", ylab="Number of Features")
}

#This displays a plot of the datatype of features 
#Reference - https://stackoverflow.com/questions/21125222/determine-the-data-types-of-a-data-frames-columns
data_types(df)


str(df)
names(df)
vis_miss(df, warn_large_data = FALSE)

df <- data.frame(df[,c(1:8,10:15)],df[,c(17:28)],df[,c(30:41)])
names(df)
df_withoutnull <- na.omit(df)
res <- cor(data.frame(df_withoutnull[,c(15:30)],df_withoutnull[,c(35:37)]))
round(res, 2)


#Dropping highly correlated features
df <- data.frame(df[,c(1:18)],df[,c(20:21)], df[,c(24:35,38)])

names(df)
vis_miss(df, warn_large_data = FALSE)

dim(df)

data_admitted <- data.frame(df[,c(9:13)],df[,c(15:25)],df[,c(27,30,31,32,33)])
names(data_admitted)
data_ER <- data.frame(df[,c(9:13)],df[,c(15:24)],df[,c(26:27)], df[,c(30:33)])
names(data_ER)


# Correlation panel
panel.cor <- function(x, y){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- round(cor(x, y), digits=2)
  txt <- paste0("R = ", r)
  cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt)
}
# Customize upper panel
upper.panel<-function(x, y){
  points(x,y, pch = 19)
}
# Create the plots
pairs(data.frame(df[,c(15:30)],df[,c(35:36)]), 
      lower.panel = panel.cor,
      upper.panel = upper.panel)

str(df)


#corrplot(cor(data.frame(df[,c(15:32)],df[,c(35:39)])), method="color")

#res <- cor(data.frame(df[,c(15:30)],df[,c(33:35)]))
#round(res, 2)


data_admitted <- na.omit(data_admitted)
dim(data_admitted)
data_ER <- na.omit(data_ER)
dim(data_ER)



lm.fit <- lm(admitted ~ ., data=data_admitted)

summary(lm.fit)


#lm.fit <- lm(ER ~ ., data=data_ER)

#summary(lm.fit)


model1 <- rpart(formula= admitted ~ ., 
                model=TRUE,
                data =data_admitted,cp=0.001
)
#0.002
#0.00019852
summary(model1)
rpart.plot(model1,digits=10,fallen.leaves=TRUE,type=4,extra=1)
printcp(model1)
rsq.rpart(model1)


data_admitted <- subset(data_admitted, timeindex < 40)
dim(data_admitted)

lm.fit2019 <- lm(admitted ~ ., data=data_admitted)

summary(lm.fit2019)

model <- rpart(formula= admitted ~ .
                , 
                model=TRUE,
                data =data_admitted,cp=0.001
)
summary(model)
rpart.plot(model,digits=10,fallen.leaves=TRUE,type=4,extra=1)
printcp(model)
rsq.rpart(model)

head(data_admitted)



#lm.fit <- lm(log1p(admitted) ~ log1p(timely) + log1p(taughtdrugs) + log1p(checkfall) + log1p(checkdepression) + log1p(taughtfootcare) + log1p(episode) + year + season + timeindex+ log1p(median) + log1p(mean) + log1p(pop) + ruca, data=data_admitted)

#summary(lm.fit)

head(df)

length(unique(df$CCN))

getmode <- function(v) {
  levels(v)[which.max(table(v))]
}

#my_summary <- function(x, id, ...){
#  if (is.numeric(x)) {
#    return(tapply(x, id, mean))
#  }  
#  if (is.factor(x)) {
#    return(tapply(x, id, getmode))

#  }  
#}

my_summary <- function(x, id, na.rm = FALSE){   
  if (is.numeric(x)) {     
    return(tapply(x, id, mean, na.rm = na.rm))   
  }     
  if (is.factor(x)) {     
    return(tapply(x, id, getmode))   
  }   
  }

data_admitted_CCN <- data.frame(df[,c(2,9:13)],df[,c(15:25)],df[,c(27,30,31,32,33)])
data_admitted_CCN <- na.omit(data_admitted_CCN)
data_test <- data.frame(lapply(data_admitted_CCN, my_summary, id=data_admitted_CCN$CCN, na.rm = TRUE))
length(unique(data_admitted_CCN$CCN))
data_test <- na.omit(data_test)
dim(data_test)
names(data_test)

aggregatedModel <- lm(admitted ~ ., data= data.frame(data_test[,c(2:22)]))
summary(aggregatedModel)







mixedEffectDf <- df
mixedEffectDf$CCN <- as.factor(mixedEffectDf$CCN)
mixedEffectDf$year <- as.factor(mixedEffectDf$year)
mixedEffectDf$season <- as.factor(mixedEffectDf$season)

str(mixedEffectDf)
mixedEffectAdmitted <-data.frame(mixedEffectDf[,c(2,9:13)],mixedEffectDf[,c(15:25)],mixedEffectDf[,c(27,29,30,31,32,33)])
mixedEffectAdmitted <- na.omit(mixedEffectAdmitted)
names(mixedEffectAdmitted)

memodel <- lme(admitted~off.physical+off.occupational+off.speech+off.medical+off.hha+timely+taughtdrugs+checkfall+checkdepression+taughtfootcare+betterbathing+lesspain+betterbreathing+betterdrug+ruca, data=mixedEffectAdmitted, random = ~1|CCN/year/season, control = lmeControl(opt = "optim"))
summary(memodel)





