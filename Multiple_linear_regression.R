df <- read.csv("ABCData.csv",TRUE,",")
df <- na.omit(df)
splt <- sample(2,nrow(df),replace=TRUE,prob= c(0.7,0.3))
trainingdata<-df[splt==1,]
testingdata <- df[splt==2,]

modelresult <- lm(satisf~reliab+time+av_br+av_spec+price+credit+return+warranty+num_emp+size, data= trainingdata)
summary(modelresult)
prediction<-predict(modelresult,testingdata)
head(prediction)

modelresult2 <- lm(satisf~reliab+time+av_br+av_spec+price+credit+return+warranty+num_emp+size, data= trainingdata)
summary(modelresult2)
prediction2<-predict(modelresult,testingdata)
head(prediction2)

modelresult3 <- lm(satisf~reliab+time+av_br+credit+return, data= trainingdata)
summary(modelresult3)
prediction3<-predict(modelresult,testingdata)
head(prediction3)


modelresult4<- lm(satisf~reliab+time+av_spec, data= trainingdata)
summary(modelresult4)
prediction4<-predict(modelresult4,testingdata)
head(prediction4)


install.packages(party)
library(party)
cf1 <- cforest(satisf~ . , data= df, control=cforest_unbiased(mtry=2,ntree=50))
varimp(cf1, conditional=TRUE)
varimpAUC(cf1)

library(relaimpo)
lmMod <- lm(satisf ~ . , data = df)  # fit lm() model
relImportance <- calc.relimp(lmMod, type = "lmg", rela = TRUE)
sort(relImportance$lmg, decreasing=TRUE)