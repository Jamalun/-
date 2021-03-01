####빅데이터 통계학
setwd("c:/stata") #working directory를 설정한다. 모든 작업을 진행할 폴더 설정.
aa <- c(1,2,3) # "=" assign  c : concatenate
aa
temp <- c(98.1,98.6,101.4)
temp[2]

subject_name <- c("John", "Jane", "Steve")


#logical value : True or False
flu_status <- c(FALSE, TRUE, FALSE)
flu_status

#subject of the value 
temp[2] #second element
temp[2:3] # 두번째부터 세번째까지
temp[-2] # 두번째 빼고 다 
temp[c(1,3)] # 첫번째 세번째 보여줘
gender <- c("MALE","FEMALE","MALE")
gender1 <- factor(gender)
gender1

#List 
list1 <- list(subject_name, temp, flu_status)
list1
list1[1]
list1[2:3]

list2 <- list(aa=subject_name, bb = temp, cc=flu_status) # aa,bb등 이름지어주기 가능쓰
list2
list2$aa    #지목해서 호출
list2$bb

#data frame
pt_data <- data.frame(subject_name, temp, flu_status)
pt_data
pt_data$subject_name
pt_data[1,2] # row 먼저
pt_data[,1:2]
pt_data[1:2,]

pt_data[,1] # 1st column (변수:variable)
pt_data[1,] # 1st row (관측치 : observation)

save(aa, flu_status,temp, file = "mydata.RData")
save.image(file="mydata1.Rdata")

load(file="mydata.Rdata")
load(file="mydata1.Rdata")

ls()

pt_data <- read.csv("pt_data.csv", header=T, stringsAsFactors = F)
pt_data
View(pt_data)
write.csv(pt_data, file= "pt_data1.csv", row.names=F)

usedcars <- read.csv(file= "usedcars.csv", stringsAsFactors = F)
str(usedcars)
class(usedcars)

View(usedcars)
head(usedcars)
tail(usedcars)
head(usedcars, n=10)

summary(usedcars$year)
summary(usedcars[c("price","mileage")])
summary(usedcars[,2:3])

mean(c(36000,40000,56000))
mean(usedcars$price)
median(usedcars$price)

range(usedcars$price)
diff(range(usedcars$price))

IQR(usedcars$price)

quantile(usedcars$price)
quantile(usedcars$price, prob=c(0.1,0.9))
quantile(usedcars$price, prob=seq(from=0, to =1, by=0.1))

boxplot(usedcars$price, main = "Boxplot of Used car prices", ylab="Price($)")

hist(usedcars$price, main="Histogram of Used car prices", xlab="Price($)", freq=F)
hist(usedcars$price, main="Histogram of Used car prices", xlab="Price($)")
hist(usedcars$price, main="Histogram of Used car prices", xlab="Price($)", breaks=c(0,5000,10000,15000,20000,25000))

table(usedcars$year)
table(usedcars$model)
table(usedcars$color)

model_table <- table(usedcars$model)
prop.table(model_table)
color_pct <- table(usedcars$color)
color_pct <- prop.table(color_pct)*100 
color_pct

#two way table

tt1 <-table(usedcars$model, usedcars$transmission)
prop.table(tt1)
prop.table(tt1, margin=1)
prop.table(tt1, margin=2)

plot(usedcars$price ~ usedcars$mileage, main = "산포도", ylab="Price", xlab="마일리지")
cor(usedcars$price, usedcars$mileage)
cor(usedcars[c("price","mileage","year")])

usedcars$conservative <- usedcars$color %in% c("Black", "Gray", "Silver", "White")
table(usedcars$conservative)
table(usedcars$model, usedcars$conservative)

tt2<- table(usedcars$model, usedcars$conservative)
summary(tt2)



