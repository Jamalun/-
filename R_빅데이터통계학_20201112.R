############################
# 빅데이터통계학##
# Lecture 6 , 7, 8 
# 2020-11-12
############################
setwd("c:/stata")
credit <-read.csv(file="credit.csv", stringsAsFactors = F)
 
# y: factor variable (default : Yes or No)    y 변수는 지금 줄이랑 밑에줄 , 그 둘 중 하나
# y: numeric variable( continuous variable )
# Classification tree : y(factor variable)

str(credit$default)
credit$default<-as.factor(credit$default)

library(C50)
credit_model <- C5.0(default ~ ., data=credit ) # .은 y변수를 제외한 나머지 모든 변수를 x변수로 하라는 의미
credit_model      #tree size = terminal node의 수 에욤
summary(credit_model)   # 해석 - checking balance in 457+ 60 c총 517명인데  457명이 1이란 값을 가지고 있따. 즉 디폴트하지 않았다는 뜻  60명은 디폴트 했다. 
                       # 그 밑에 2는 디폴트를 말함. 32 +4 중 총 32명이 디폴트함 //끝에 있는 숫자들이 뭘 의미하는지 알아야함. 
                         #  출력 중 decision tree 부분 중요 size 가 72 , error rate 는 전체 1000명 중 122명 잘못 예측 했으므로 12.2 % 나오는거 
                           # 이는 또 a밑에꺼랑 b위에꺼 합해서 error 개수 구할 수 있담 / 또 error rate가 너무 낮으면 overfitting 문제 발생 즉 in sample 에선 잘 맞는데 out of 
                           #attribute usage
plot(credit_model)     # tree 구조 보게 해줘욤
# how to decrease the tree size 
credit_model1 <- C5.0(default ~ ., data=credit, 
                      control=C5.0Control(minCases = 20)) # terminal node에 들어가는 표본의 수가 최소 20개는 되게해라 
plot(credit_model1)

# out-of-sample prediction 
train <- credit[1:700,]  
test <- credit[701:nrow(credit),] #nrow = number of row 즉 전체 숫자 !

credit_model1 <-C5.0(default~., data=train)
summary(credit_model1)

pred1 <- predict(credit_model1, newdata=test)
pred1
table(test$default, pred1)
(41+51)/300  # out-of sample error rate

# Adaptive Boosting 
credit_model3 <-C5.0(default~., data=train, trials=10) #trials = 10 은 열번 만들어라 ! 란 의미 tree를 열개 만들어라 !
summary(credit_model3)
pred3 <- predict(credit_model3, newdata=test)
pred3
table(test$default, pred3)
(57+23)/300     # 한번 했을 때에 비해 10번 하니까 error rate 줄어들었음 !
plot(credit_model3, trials=5) # 너 tree 10개 만들었을텐데 5번째 tree 보여줘 

############################################
# Regression Tree LN8
install.packages("MASS") # 이 안에 예제데이터가 있음
library(MASS)
data(Boston)

#C50 : Classification tree 에서만 사용할 수 있다.

install.packages("tree")  # tree는  classification이랑 regression 에서도 가능
library(tree)
str(Boston$medv) #tree 란 놈은 y변수가 factor variable이면 classification으로 numeric이면 regression으로 자동으로 넘어감 

single <-tree(medv ~ . , data=Boston)
single
summary(single) # 해석 2) 부분 rm이 6.941보다 작은게 430개 있고 잔차의 제곱합이 17320 예측값은 19.93(순서대로)
                    # tree construction 뭐시기란? 사용가능한 variables 중에 y변수 한개 뺀 것 중에 실제로 쓰인것 여기선 5개 !
              #residual mean deviance = 6734는 전체 잔차제곱합 합친 거 , 전체 데이터 506개에서 tree size 9개를 빼줌. 여기서 tree size를 왜 빼주냐? 그래야 tree size가 클수록 deviance가 크게 나오기 때문 일종의 penalty
plot(single) # 이름이 안나타남
text(single, pretty=0) # pretty = 0 의 뜻은 변수 이름이 그대로 나타나게 하라 !  왼쪽이 예스 오른쪽이 노우다 . 이걸 찾는 문제 나올 수 있다. 

# Create train and test data sets
# train : 1:400 
# test : 401 ~ nrow(Boston)
#여기부터는 전부 내 뇌피셜

train <- Boston[1:400,]
test <- Boston[401:nrow(Boston), ]
Boston_model <- tree(medv ~ . , data = train)
summary(Boston_model)

pred2 <- predict(Boston_model, newdata = test)
pred2
table(test$medv, pred2)
prop.table(table(test$medv,pred2))


# Out-of-sample prediction : Residual Mean deviance (이게바로 error rate말하는 거)
