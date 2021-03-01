###############################
# 鍮낅뜲?씠?꽣 ?넻怨꾪븰 
# Lecture Note 4
# 2020-10-08
##############################
setwd("c:/stata")
data1 <- read.csv(file="credit_LN4.csv", stringsAsFactors = F)

# student : dummy variable (0 또는 1의 값을 갖는 x변수)

data1$default <- as.factor(data1$default)    #일단 factor로 변환하고 사용 ㄱ as.factor도 같은거 
data1$student <- as.factor(data1$student)          #종속변수도 factor variable로 변환
str(data1)

glm2 <- glm(default ~ student, data=data1, family="binomial")   #LN4 8페이지
summary(glm2)

# x*beta=10 
exp(10)/(1+exp(10))   # Pr(Y=1) 

install.packages("gtools")
library(gtools)
inv.logit(10)      #inv.logit 이용하려면 라이브러리 불러와야함

# Student=Yes : Pr(Y=1) ??
xb <- (-3.50413 + 0.40489*1)  # 학생이니까 x =1 대입 (더미변수)
inv.logit(xb)

# Student=No : Pr(Y=1) ??
xb <- (-3.50413 + 0.40489*0) #학생이 아니니까 x=0 대입 
inv.logit(xb)

# Multiple logistic regression 
glm3 <- glm(default ~ balance + income + student , data=data1, family="binomial")
summary(glm3)


boxplot(balance ~ student, data=data1) # LN4 9페이지 상단 그림

# Student=Yes  #LN4 9페이지 3.4계산
xb1 <- (-1.087e+01+ 5.737e-03*1500+3.033e-06 *40000+-6.468e-01*1)
inv.logit(xb1)
# Student=No 
xb2 <- (-1.087e+01+ 5.737e-03*1500+3.033e-06 *40000+-6.468e-01*0)
inv.logit(xb2)


xx <-data.frame(balance=c(1500,1500), income=c(40000,40000), student=c("Yes", "No"))  #위에 두개식 요약
predict(glm3,newdata=xx, type="response")        #newdata 체크체크 !

# Multinomial logistic regression 
# nnet 
# multinom : 
library(nnet)

###################################################################################
# Prediction Measure 
# ROC curve
# CCR 
# Compare prediction performance 


glm3 <- glm(default ~ balance + income + student , data=data1, family="binomial")
glm4 <- glm(default ~ balance  , data=data1, family="binomial")

# glm3 
# Confusion matrix 
# threshold=0.5 

pr3 <- predict(glm3, type="response")  #glm3에서 나온 확률, 예측값이 나옴
pr3
yhat3 <- ifelse(pr3>0.5, "Yes", "No")
yhat3

# yhat3(=predicted), data1$default (=observed)
table(yhat3, data1$default) # confusion matrix 
# 9627 : n00, 228: n10, 40: n01, 105:n11 

# CCR=?
(9624+105)/10000

# Sensitivity : Pr(yhat=1 | y=1) # ROC 로 예측성과 평가 바로 위에는 confusion matrix (ccr)로 평가
105/(105+228)

# 1-specificity  : 1-Pr(yhat=0 | y=0)
1- 9627/(9627+40)

install.packages("pROC")
library(pROC)     #ROC 커브 그리기 전에 library 실행하기 

roccurve3 <- roc(data1$default~pr3)  #위에서 확률을 pr3로 정의했으니까 ㅇㅇ 왼쪽에는 실제값 오른쪽엔 확률
plot(roccurve3)         #x축 좌우가 바껴있음 ㅇㅇ부호 반대 
auc(roccurve3)

# glm4 
pr4 <- predict(glm4, type="response") 
pr4
yhat4 <- ifelse(pr4>0.5, "Yes", "No")
yhat4

# yhat3(=predicted), data1$default (=observed)
table(yhat4, data1$default) # confusion matrix 
# 9627 : n00, 228: n10, 40: n01, 105:n11 

# CCR=?
(9625+100)/10000

# Sensitivity : Pr(yhat=1 | y=1)
100/(100+233)

# 1-specificity  : 1-Pr(yhat=0 | y=0)
1- 9625/(9625+42)

#install.packages("pROC")
#library(pROC)

roccurve4 <- roc(data1$default~pr4)
plot(roccurve4)
auc(roccurve4)

##################################################################
#################################################################
# out-of-sample prediction 
# train:7000, test:3000

train <- data1[1:7000,] # 콤마 주의해 !!
test <- data1[7001:10000,]

glm5 <- glm(default ~ balance + income + student , data=train, family="binomial")
summary(glm5)

# out-of-sample prediction  # train에서 파라미터 구하고 out of sampled은 test 셋으로 newdata라 꼭 써주기
pr5 <- predict(glm5, newdata=test, type="response")  # probabilities for 3000 observations 
 
# threshold=0.5       # 여기까진 roc랑  ccr공통적으로 하는거 
yhat5 <- ifelse(pr5>0.5 , "Yes", "No")

# confusion matrix
table(yhat5, test$default)

# out-of-sample CCR 
(2896+32)/3000

# out-of-sample sensitivity 
32/(61+32)
# out-of-sample specificity 
2896/(2896+11)

roccurve5 <- roc(test$default ~ pr5)
plot(roccurve5)
auc(roccurve5)

