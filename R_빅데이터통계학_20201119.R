###########################
# 鍮낅뜲?씠?꽣 ?넻怨꾪븰 
# Lecture Note 8 and 9 
# 2020-11-19
###########################
setwd("c:/stata")

# Bootstrapping 
set.seed(1234)
sample(1:10,replace=T )  #replace = T 복원추출
sample(1:10,size=5, replace=T )

# Bagging 
install.packages("ipred")
library(ipred)
set.seed(300)

credit <-read.csv(file="credit.csv",
                  stringsAsFactors = F)
credit$default <- as.factor(credit$default) #numeric이니까 factor variable로 변경

mybag <- bagging(default ~ . ,data=credit, nbagg=25 )
mybag
summary(mybag)

credit_pred <- predict(mybag, newdata=credit)
table(credit_pred)
table(credit$default, credit_pred) #대각선이 정확하게 맞춘것

train <- credit[1:900,]
test <- credit[901:nrow(credit),]

mybag <- bagging(default ~ . ,data=train, nbagg=25 )
credit_pred <- predict(mybag, newdata=test)
table(credit_pred)
table(test$default, credit_pred)
(59+15)/100 #이게 바로 ccr

# Random forest 
install.packages("randomForest")
library(randomForest)

set.seed(1234)
rf <- randomForest(default ~ ., data=credit)  
rf    # 루트 20이니까 split에 사용된 변수는 4개
    # OOB 는 out of bag  매번 트리만들 때 사용하지 않는 사이즈 decorrelated ㅇㅇ


train <- credit[1:900,]
test <- credit[901:nrow(credit),]  # out-of-sample 
set.seed(1234)
rf <- randomForest(default ~ ., data=train)
rf_pred <-predict(rf, newdata=test, type="response")
rf_pred
table(test$default, rf_pred)

rf_pred <-predict(rf, newdata=test, type="prob") # 다수결 투표 ..! 500개 tree 중에 비율 !
rf_pred


rf <- randomForest(default ~ ., data=train, ntree=100) #만들고 싶은 tree의 개수 = ntree .. tree의 숫자를 줄이곳피다면 
rf

rf <- randomForest(default ~ ., data=train, mtry=5) # 사용되는 변수의 개수 딱 정해주기 ! # mtry = 20? bagging과 RF같아짐 
rf

# Q) bagging vs. rf : the number of predictors at each split 

rf <- randomForest(default ~ ., data=train)
plot(rf) # tree 50개 이상 만들 때부터 error rate 별 차이 없음 , 2*2 table에서 잘못 추정하는 걸 보여줌. 맨위는 합
rf                 #위에 이어서 빨간색이 첫번째 초록색이 두번째 , oob 는 검정색 

varImpPlot(rf) # variable importance # 중요한 변수가 뭔지 보여줘 # amount 가 제일 중요한 변수
varImpPlot(rf, n=5) # 가장 중요한 변수 5개만 보여줌..!

