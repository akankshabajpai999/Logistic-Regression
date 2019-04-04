songs= read.csv("songs.csv")
rm(list=ls())
ms = subset(songs,songs$artistname=="Michael Jackson")
nrow(ms)
ms$songtitle[table(ms$Top10)==1]
str(ms$Top10)
ms[c("songtitle","Top10")]
table(songs$timesignature)
which.max(songs$tempo)
songs$songtitle[6206]
train = subset(songs, songs$year<=2009)
test=subset(songs, songs$year==2010)
nonvars = c("year", "songtitle", "artistname", "songID", "artistID")
train = train[,!names(songs)%in%nonvars]
test = test[,!names(songs)%in%nonvars]
model= glm(Top10~.,data=train,family = binomial)
summary(model)
cor(train$energy,train$loudness)
SongsLog2 = glm(Top10 ~ . - loudness, data=train, family=binomial)
summary(SongsLog2)

SongsLog3 = glm(Top10 ~ . - energy, data=train, family=binomial)
summary(SongsLog3)
pred = predict(SongsLog3,newdata=test,type ="response")
table(test$Top10, pred >= 0.45)
str(pred)
table(test$Top10)
length(test$Top10)
