#################################
#�����������
# Lecture Note 9 and 10 
# 2020-11-26
##################################
setwd("c:/stata")

# 2차원 공간?��?�� SVM (x1, x2=> y(0 or 1): classification )
set.seed(1234)
x1<-rnorm(20)  # normal distribution ���� 20���� ���� ����´�
x2<-rnorm(20)  # normal distribution 
x<-cbind(x1, x2)
View(x)
y<-c(rep(-1,10), rep(1,10)) # -1�� ���� �ݺ�, 1�� ���� �ݺ��ض�
y 
x[y==1,]<-x[y==1,]+1     # y�� 1�� x�� ���� 1�� �������
data1<-data.frame(x,y)  # classification ���� 

data1$y<- as.factor(data1$y)
plot(data1$x2~data1$x1, col=3-y)# y=-1, 1, col=4(blue), 2(red)

# SVM (y: y, x: x1, x2(two-dimension) )
install.packages("kernlab")
library(kernlab) 
svm1 <- ksvm(y ~ . , data=data1, C=1,              
          type="C-svc", kernel="vanilladot")      #C=1 ���� ���, c-scv = cost classification , 4���� kernel �� linear kernel�� �̸� 
svm1
yhat <- predict(svm1, newdata=data1)
yhat
table(yhat, data1$y)     #���⼭ svm1�������� �� ���� training error ���� �� ����

plot(svm1, data=data1) # ������ ������ ������ support vector, ���� �� �� ���� - ���� ����

svm2 <- ksvm(y ~ . , data=data1, C=3,  
             type="C-svc", kernel="vanilladot")     #C=3 ? ����ġ�� Ŀ�� ��. �� slack�� �۰� ��������. support vector �� �پ��� �� 
svm2

#######################################################
data2 <-read.csv(file="letterdata.csv", stringsAsFactors = F)
set.seed(1234)   #svm�� �� seed ���صΰ� �ؾ� ���� ���� ����
data2$letter <-as.factor(data2$letter)
svm3 <- ksvm(letter ~ . , data=data2, C=1, 
             type="C-svc", kernel="vanilladot")
svm3 
yhat3 <- predict(svm3, newdata=data2) # training error ���� ���ϴ� ����
table(yhat3, data2$letter)
1-mean(yhat3==data2$letter)  # in-sample prediction (overfittings���ɼ��� ����)

# train and test 
train <- data2[1:16000,]
test <-data2[16001:nrow(data2),]

svm4 <- ksvm(letter ~ . , data=train, C=1, 
             type="C-svc", kernel="vanilladot")
svm4 
yhat4 <- predict(svm4, newdata=test)
table(yhat4, test$letter)
1-mean(yhat4==test$letter)

# how to reduce out-sample prediction error 

svm5 <- ksvm(letter ~ . , data=train, C=1,type="C-svc", kernel="rbfdot") #����þ� ���ñ� 
svm5 
yhat5 <- predict(svm5, newdata=test) #�ð� �� �ɸ�
table(yhat5, test$letter) # table�� ���� �� ���� ȥ���ϴ��� �˾Ƹ����� �� 
1-mean(yhat5==test$letter)

# kernlab library : ksvm 
# e1071 library: svm # �̷� �͵� ���� ���� library �ڴ� svm�� ���ɾ�
