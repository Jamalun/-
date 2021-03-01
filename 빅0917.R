##############################
### 빅데이터통계학
# Lecture Note 3
# 2020-09-17
##############################
setwd("c:/stata")

# function graph : y=2x+1 , y=x^2+2*x -3 
curve(x+1, from=-5 , to =5)
curve(x^2+2*x-3, from=-5 , to =5)

# B_data2_1.dta (stata data file), *.csv (excel file)
install.packages("haven")
library(haven)
data1 <- read_dta(file="B_data2_1.dta")

reg1 <- lm(price~crime, data=data1)  # linear model , ols 추정량 얻는 방식
summary(reg1)

#y = alpha + beta_1*crime + beta_2*nox + e 
reg2 <- lm(price~crime+nox, data=data1)
summary(reg2)

# scatter plot 
plot(price ~ crime, data=data1, col="red") # col은 마커의 색깔 지정.
abline(reg1)  

# 예측손실함수 : MSE (sum of squared residuals)
# [sum(price - predicted price)^2] / n      fitted value = predicted value
# reg1 의 예측손실함수를 계산한다.
# predicted y=alpha_hat + beta_hat * crime 
str(reg1)

# residual = (y-y_hat)
mean(reg1$residuals^2)    #MSE 계산방법
mean((data1$price - reg1$fitted.values)^2)

# reg2에서 예측손실함수를 계산하라
mean(reg2$residuals^2)
mean((data1$price-reg2$fitted.values)^2)

# log(y)=1+x 
# y=exp(1+x)
curve(exp(1+x), from=-5, to=5 )

#여기부터 로그리니어임 ~~
# reg3 : log(price)=alpha+beta*crime+ e 
reg3 <- lm(log(price)~crime, data=data1)
summary(reg3)

# how to obtain the predicted value of y : y_hat  
# exp(alpha_hat + beta_hat*crime + sigma^2/2)
# exp(reg3$fitted.value+ sigma^2/2)
# residual s.e=sigma       , s.e는 standard error
# exp(reg3$fitted.value + 0.348^2/2) = y_hat 
data1$yhat <- exp(reg3$fitted.value + 0.348^2/2) #data1 프레임의 y란 변수로 만들어라

# 예측손실함수를 계산한다.   
# (1/n) sum(y-yhat)^2 : MSE
mean((data1$price - data1$yhat)^2)  # 68.65 from log-linear model , 그냥 linear 보다 log linear에서 예측손실함수가 더 작기 때문에
                                    #log linear model 이 더 잘 예측하는 거구나. 다시 말해서 y와 x변수 간의 관계까 비선형 관계구나로 예상 가능 


# reg4 예측손실함수를 계산하라.   reg2와 비교하려는 것
# reg4 : log(price)=alpha+beta1*crime + beta2*nox + e 
reg4 <- lm(log(price)~crime+nox, data = data1)
summary(reg4)

data1$yhat_2 <- exp(reg4$fitted.values+0.3234^2/2)
mean((data1$price - data1$yhat_2)^2) # 잔차의 제곱인 거 잊지 말기 .. 제곱이다 !!

# Out-of-sample prediction performance   # LN3예제 맨마지막 ㅇㅇ
# Model 1: price=alpha+ beta*crime + e
# 506 (354 train, 152 test)
set.seed(1234)
bs <- sample(1:504, size=354, replace=F) #replace = F 비복원추출 #sample이란 명령어로 무작위 뽑는 것 !, 그러나 seed정하면 같은 결과가 됨 !
bs   #train set에 해당하는 number

train <- data1[bs, ]  #row중에 즉 observation 중에 bs로 뽑힌 것들
test <- data1[-bs, ] #bs에 해당하지 않는 모든 애들

reg5 <- lm(price~crime, data=train)
summary(reg5)

# how to obtain the predicted values from test set 
yhat5_test <- predict(reg5, test )  # 152개의 값이 나온다. 
yhat5_test

# out-of-sample prediction 
mean((test$price-yhat5_test)^2)  # test와 train구분 ! 그리고 여기에다가 log를 끼울 수도 있겠다 !고난도
