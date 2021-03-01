
# Lecture Note 6 
# 2020-11-05
###############################빅데이터
setwd("c:/stata")
install.packages("rpart")  
install.packages("rpart.plot")
install.packages("rattle")

library(rpart)
library(rpart.plot)
library(rattle)

titanic <- read.csv(file="Titanic.csv", stringsAsFactors = F)   # survive가 y변수
titanic$Survived <- as.factor(titanic$Survived)   #y변수 factor variable로 바꿔줘야 함

#recursive  partitioning  # rpart의 줄임말
DT <- rpart(Survived ~ Sex+PClass , data=titanic, method="class")  #method는 y변수 알려주는 것, 범주형이니까 class
summary(DT)
rpart.plot(DT)       # tree 구조로 한번 보여줘봐

table(titanic$Survived)             # 0.5넘기면 산거고 1로 표기, 못 넘기면 죽은거고 0으로 표기
450/1313
fancyRpartPlot(DT)

# rpart, tree, C50         #전부 tree 만들어주는 library ㅇㅇ
-0.6*log2(0.6)-0.4*log2(0.4)
curve(-x*log2(x)-(1-x)*log2(1-x), from=0, to=1, col="red")

########################################
# data file: credit.csv 
credit <- read.csv(file="credit.csv",stringsAsFactors = F)

# 1: No default, 2: Default(YES)  , 디폴트가 y변수
class(credit$default)  #integer이 출력되는데 이는 정수를 말함
credit$default<-as.factor(credit$default)
class(credit$default)

prop.table(table(credit$default)) #root node 

# Entropy at the root node   
-0.7*log2(0.7)-0.3*log2(0.3)  

# train (900) and test(100) for out-of-sample prediction 
set.seed(1234)
bs <- sample(1:1000, size=900, replace=F)
train <- credit[bs, ]
test <- credit[-bs, ]

# Using train set, construct a tree structure 
# age, housing  #X변수들 ㅇㅇ

tree1 <-rpart(default ~ age + housing, data=train, method="class")        # 분기하지 않음 ㅇㅇ information gain  rule 때문 
rpart.plot(tree1)

# C5.0
install.packages("C50")
library(C50) 

# C5.0 command  
tree2 <- C5.0(x=train[,-17], y = train[,17])  # x변수 너무 많으니까 이렇게 표현해주는거지 !! 일일히 표기하는게 아님!
tree2           # 또 C5.0에서는 종속변수를 범주형변수로 가정하기 때문에 class를 써줄 필요가 없음, tree size = 72란 말은 노드가 72개란 말 !
summary(tree2)



