#################################
### 빅데이터통계학
# Lecture Note 4
# 2020-09-24
#################################
setwd("c:/stata")

# credit_LN4.csv 
data1 <- read.csv(file="credit_LN4.csv", stringsAsFactors = F)
plot(income~balance, data=data1)

# overlaid scatter plot 
plot(income[default=="Yes"]~balance[default=="Yes"], data=data1, pch=1) #pch는 점의 모양을 지정
points(income[default=="No"]~balance[default=="No"], data=data1, pch=2, col="red")

boxplot(data1$balance)
boxplot(balance~default, data=data1)
boxplot(income~default, data=data1)

par(mfrow=c(1,2))  #design your layout  # 1행 2열짜리 행렬을 만들고 순서대로 집어넣기
boxplot(balance~default, data=data1)
boxplot(income~default, data=data1)
dev.off()                        #이미지 자동저장

# CDF from a logistic distribution 
curve(1/(1+exp(-x)), from=-5, to=5)  #curve - 함수 그리는 명령어
curve(exp(x)/(1+exp(x)), from=-5, to=5)

# Estimation of Logistic Regression
# y: default (0 and 1) 
# x: balance : beta_1 (to be positive)

str(data1) # 문자였넹

data1$default <-factor(data1$default) #chr(문자)니까 숫자로 바꿔줘야 함. 그래야 추정 가능. fac가 범주형 변수를 숫자로 바꿔주니까 !
logit1 <- glm(default ~ balance, data=data1, family="binomial") #binomial 사건의 결과 두개
summary(logit1)

# 5.499 x 10^-3 (0.001)   =  5.529e-03   #e뒤에 나오는건 10의 지수 ! 부호는 숫자 앞에 나옴
5.499*0.001
# e+01= 10^1 : scientific notation 
# beta0_hat, beta1_hat 

#함수에 대입해서 직접 구하는 방법 (0.5가 기준)
# balance=$1000 => Pr(Y=1)         #balance가 1000일 때 가령 ?  물어보는 것이얌 
# Pr(y=1)= exp(beta0+beta1*x)/(1+exp(beta0+beta1*x))
exp(-1.065e+01+5.499e-03*1000)/(1+exp(-1.065e+01+5.499e-03*1000))  #0.0057이 나옴 (0.5보다 작으니까)
# 0.57% => predict 0 for this credit card user

# balance=$2000 => Pr(Y=1)
exp(-1.065e+01+5.499e-03*2000)/(1+exp(-1.065e+01+5.499e-03*2000)) #0.58이 나옴. 0.5넘기니까 default 할거라고 본다 ㅇㅇ
 

#함수를 안쓰고 predict 사용하기 
# predict(logit1, balance=2000) # 이렇게 하면 안됨 !
predict(logit1, data.frame(balance=2000), type="response")  # type=response란  probability를 계산하라 ! , 0.58이 나옴

