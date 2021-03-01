############################
# 鍮낅뜲?씠?꽣 ?넻怨꾪븰 
# Lecture Note 10 and 11
# 2020-12-10
##############################
setwd("c:/stata")
xx <- matrix(c(0,1,1,0,0,1,1,1,0,0), nrow=5,ncol=2 ) #LN10 9페이지 예제
xx<-t(xx)  # 그대로 입력하면 안되고 transpose해서 행과 열을 바꿔줘야 함 !
xx
dist(xx, method="binary")

library(haven)
wclub<-read_dta(file="wclub.dta")

library(cluster)
set.seed(1234)
k3_fit <- clara(wclub, k=3, metric="jaccard")
k3_fit
wclub$cluster <- k3_fit$clustering
table(wclub$cluster)

# 2번 그룹에 속한 여성들을 어떤걸 제일 좋아하나요?
wclub2 <-wclub[wclub$cluster==2,]
wc <-colMeans(wclub2[,1:35])
wc
sort(wc, decreasing = T) #내림차순 !!
#######################################################
# Leccture Note 11 
# Hierarchical Clustering 
set.seed(1234)
x <-matrix(rnorm(50*2), ncol=2)  # normal random number를 generate // ncol는 column number
x
plot(x[,2]~x[,1])   #이렇게 두면 분류하기 애매함 ..!

x_df<-data.frame(x)
x_df
km.out <-kmeans(x_df, centers=2, nstart=25)# 두개의 그룹으로 분류하라, nstart 중심점 무작위로 25번 정도 찍어봐라 ! 따로 안주면  nstart =1 
km.out

# HC : Draw a dendrogram  #libary 부를 필요 X 
hc.complete <- hclust(dist(x_df), method="complete")  #(dist(x_df) 뒤에 method = 'ucledian 생략된거 x_df가 이항변수라면 method = binary라고 써주기 )
hc.complete
plot(hc.complete) #plot 써줘야 dendrogram 나옴 40번과 가장 가까운 사람은 누구일까? 그림 빨리 만들고 맞히기

hc.single <- hclust(dist(x_df), method="single")
hc.single
plot(hc.single)

hc.average <- hclust(dist(x_df), method="average") 
hc.average
plot(hc.average)

x_df$D1 <-cutree(hc.complete, k=2)
rect.hclust(hc.complete, k=3, border="red")
x_df$D2 <-cutree(hc.complete, k=3)

# Wclub 데이터를 이용해서 30명의 여성이 있다.
# 1번 여성과 가장 가까운 여성의 번호는?
