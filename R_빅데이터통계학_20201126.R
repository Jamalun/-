#################################
# 鍮낅뜲?씠?꽣 ?넻怨꾪븰 
# Lecture Note 9 and 10 
# 2020-11-26
##################################
setwd("c:/stata")

# 2李⑥썝 怨듦컙?뿉?꽌 SVM (x1, x2=> y(0 or 1): classification )
set.seed(1234)
x1<-rnorm(20)  # normal distribution 에서 20개의 값을 들고온다
x2<-rnorm(20)  # normal distribution 
x<-cbind(x1, x2)
View(x)
y<-c(rep(-1,10), rep(1,10)) # -1을 열번 반복, 1을 열번 반복해라
y 
x[y==1,]<-x[y==1,]+1     # y가 1인 x에 전부 1을 더해줘라
data1<-data.frame(x,y)  # classification 위해 

data1$y<- as.factor(data1$y)
plot(data1$x2~data1$x1, col=3-y)# y=-1, 1, col=4(blue), 2(red)

# SVM (y: y, x: x1, x2(two-dimension) )
install.packages("kernlab")
library(kernlab) 
svm1 <- ksvm(y ~ . , data=data1, C=1,              
          type="C-svc", kernel="vanilladot")      #C=1 슬랙 허용, c-scv = cost classification , 4가지 kernel 중 linear kernel의 이름 
svm1
yhat <- predict(svm1, newdata=data1)
yhat
table(yhat, data1$y)     #여기서 svm1실행했을 때 나온 training error 구할 수 있음

plot(svm1, data=data1) # 색깔이 검정인 점들이 support vector, 직접 셀 수 있음 - 출제 가능

svm2 <- ksvm(y ~ . , data=data1, C=3,  
             type="C-svc", kernel="vanilladot")     #C=3 ? 가중치가 커진 것. 즉 slack을 작게 만들어야함. support vector 도 줄어들게 됨 
svm2

#######################################################
data2 <-read.csv(file="letterdata.csv", stringsAsFactors = F)
set.seed(1234)   #svm할 때 seed 정해두고 해야 같은 값이 나옴
data2$letter <-as.factor(data2$letter)
svm3 <- ksvm(letter ~ . , data=data2, C=1, 
             type="C-svc", kernel="vanilladot")
svm3 
yhat3 <- predict(svm3, newdata=data2) # training error 직접 구하는 과정
table(yhat3, data2$letter)
1-mean(yhat3==data2$letter)  # in-sample prediction (overfittings가능성이 있음)

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

svm5 <- ksvm(letter ~ . , data=train, C=1,type="C-svc", kernel="rbfdot") #가우시안 뭐시기 
svm5 
yhat5 <- predict(svm5, newdata=test) #시간 좀 걸림
table(yhat5, test$letter) # table을 보고 뭘 많이 혼동하는지 알아맞혀야 함 
1-mean(yhat5==test$letter)

# kernlab library : ksvm 
# e1071 library: svm # 이런 것도 가능 앞은 library 뒤는 svm은 명령어
