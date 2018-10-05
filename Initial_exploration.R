library(ggplot2)
library(caret)
library(dplyr)	

initialDataset <- read.csv("UCI_Credit_Card.csv",header = T, sep = ",")
dim(initialDataset)

#Read data
initialDataset$SEX <- as.factor(initialDataset$SEX)
levels(initialDataset$SEX)<-c("Male", "Female")
#Change the data type
initialDataset$EDUCATION<-as.factor(initialDataset$EDUCATION)
levels(initialDataset$EDUCATION)<-c("Unknown", "Graduate School", "University", "High school", "Others", "Unknown", "Unknown")
initialDataset$MARRIAGE<-as.factor(initialDataset$MARRIAGE)
levels(initialDataset$MARRIAGE)<-c("Unknown" , "Married" , "Single" ,"Others")

initialDataset$PAY_0 <- as.factor(initialDataset$PAY_0)
initialDataset$PAY_2 <- as.factor(initialDataset$PAY_2)
initialDataset$PAY_3 <- as.factor(initialDataset$PAY_3)
initialDataset$PAY_4 <- as.factor(initialDataset$PAY_4)
initialDataset$PAY_5 <- as.factor(initialDataset$PAY_5)
initialDataset$PAY_6 <- as.factor(initialDataset$PAY_6)
initialDataset$default.payment.next.month<-as.factor(initialDataset$default.payment.next.month)

#
#initialDataset$ID<-NULL
#Initial exploration
summary(initialDataset)
sum(is.na(initialDataset))

################
#Age
ggplot(data = initialDataset, aes(AGE)) + geom_histogram(stat="count",color='red',fill='orange')
#Limited balance
ggplot(data = initialDataset, aes(x = 'Limit Balance',LIMIT_BAL)) + geom_boxplot()

#Marriage
ggplot(data = initialDataset, aes(x = MARRIAGE, fill = MARRIAGE)) + geom_bar() + geom_text(stat='count', aes(label=..count..), vjust=-1)
#Sex
ggplot(data = initialDataset, aes(x = SEX, fill = SEX)) + geom_bar() + geom_text(stat='count', aes(label=..count..), vjust=-1)
#Education
ggplot(data = initialDataset, aes(x = EDUCATION, fill = EDUCATION)) + geom_bar() + geom_text(stat='count', aes(label=..count..), vjust=-1)


#limited balance, marriage, sex
ggplot(initialDataset, aes(MARRIAGE, LIMIT_BAL , fill = SEX)) + geom_boxplot()
#limited balance, education, sex
ggplot(initialDataset, aes(EDUCATION, LIMIT_BAL , fill = SEX)) + geom_boxplot()
#age, marriage, sex
ggplot(initialDataset, aes(EDUCATION, AGE , fill = SEX)) + geom_boxplot()
#age, education, sex
ggplot(initialDataset, aes(MARRIAGE, AGE , fill = SEX)) + geom_boxplot()

#Default payment
predict_value <- as.data.frame(table(initialDataset$default.payment.next.month))

pie(predict_value$Freq, labels = paste0(predict_value$Var1,round(predict_value$Freq/ sum(predict_value$Freq) * 100, 2), "%"))

#default payment in sex distribution
ggplot(initialDataset, aes(default.payment.next.month, fill = SEX)) + geom_bar(position="dodge")+
  geom_text(aes(label=..count..),stat='count',position=position_dodge(0.9),vjust=-0.2)

#default payment in education distribution
ggplot(initialDataset, aes(EDUCATION, fill = default.payment.next.month)) + geom_bar(position="dodge")+
  geom_text(aes(label=..count..),stat='count',position=position_dodge(0.9),vjust=-0.2)
