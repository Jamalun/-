############################### 
# 빅데이터통계학
# Lecture Note 10 
# 2020-12-03
###############################
setwd("c:/stata")
library(haven)
psysed <- read_dta(file="psysed.dta")

xx<-psysed[,2:3]

# k=4  # kmeans 는 무조건 범주가 연속형
set.seed(1234)
k4_fit <-kmeans(xx, center=4) # 4개의 중심점을 써라 . kmeans는 library 설치할 필요 X
k4_fit   #여기서 어떤 그룹이 운동 제일 못할지 알아맞혀야함.
str(k4_fit) #여기서 tot ss. tot withtin ss, between ss 나옴

k3_fit <-kmeans(xx, center=3)
k3_fit

k5_fit <-kmeans(xx, center=5)
k5_fit

k6_fit <-kmeans(xx, center=6)
k6_fit

# elbow plot:y axis=(tot.within SS/total SS)
wss3=k3_fit$tot.withinss/k3_fit$totss # k=3 
wss4=k4_fit$tot.withinss/k4_fit$totss # k=4 
wss5=k5_fit$tot.withinss/k5_fit$totss # k=5 
wss6=k6_fit$tot.withinss/k6_fit$totss # k=6 
wss <-c(wss3, wss4, wss5, wss6) # y-axis 
kk<-c(3,4,5,6) # x-axis
plot(wss~kk,type="l") # k=4구나 !! elbow 가 나오넴

# Exercise : x: flex, speed, strength 
# Find optimal k and optimal centers 
xx1<-psysed[,2:4]       #직접 해보기 ..!
set.seed(1234)
k4_fit <-kmeans(xx1, center=4)
k4_fit

k3_fit <-kmeans(xx1, center=3)
k3_fit

k5_fit <-kmeans(xx1, center=5)
k5_fit

k6_fit <-kmeans(xx1, center=6)
k6_fit

wss3=k3_fit$tot.withinss/k3_fit$totss # k=3 
wss4=k4_fit$tot.withinss/k4_fit$totss # k=4 
wss5=k5_fit$tot.withinss/k5_fit$totss # k=5 
wss6=k6_fit$tot.withinss/k6_fit$totss

wss <-c(wss3, wss4, wss5, wss6) # y-axis 
kk<-c(3,4,5,6) # x-axis
plot(wss~kk,type="l")

################################################
# kmeans는 기본패키지 ㅇㅇ 단순함. 따라서 cluster를 새로 깔아야함...!
install.packages("cluster")
library(cluster)           #kmeans는 x변수가 무조건 연속형이라 생각함. cluster와 다름 !
kk4 <- clara(xx,4,metric="euclidean") # x변수가 연속형이니까 euclidean! //jaccard metric 밑에 나옴..!(범주형일 때)
kk4
clusplot(xx, kk4$clustering, lines=0) # kk$clustering은 1번부터 4번에 해당하는 어떤 집단에 속하는지 나타냄

#################################################
wclub<-read_dta(file="wclub.dta") #LN10 8페이지부터 ..!,binary 형태
wk4 <- clara(wclub,4,metric="jaccard") # wclub 모두가x변수가 되니까 그대로 wclub이라고 써주기 !
wk4 # 출력되는 것 중에 'cluster size'란 게 있는데 이는 각 table에 몇명씩 앉을지 보여준다.!

