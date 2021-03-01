#################################
##빅데이터 통계학
# Lecture note 5
# 2020-10-22 
#################################
setwd("c:/stata")
sms_spam<- read.csv(file="sms_spam.csv", stringsAsFactors = F)

# y: variable (type)     #spam이랑 ham을 factor로 바꾸자
sms_spam$type<-as.factor(sms_spam$type)

# 1단계 : text cleaning 
# Corpus : 말뭉치
#install.packages("tm") # text mining 
library(tm)
sms_corpus <- VCorpus(VectorSource(sms_spam$text)) #text를 소스로해서 콜퍼스로 만들어라 단 library이용해야함
inspect(sms_corpus[[2]]) #대체corpus로 어떻게 만든건데? 두번째 text  결과를 보자

sms_corpus_clean <- tm_map(sms_corpus, content_transformer(tolower)) #모든 대문자를 소문자로 바꿔라 . 통일시켜
inspect(sms_corpus_clean[[1]])

sms_corpus_clean <- tm_map(sms_corpus_clean, removeNumbers)  #앞에서 clean한거를 똑같은 이름으로 다시 clean
sms_corpus_clean <- tm_map(sms_corpus_clean, removePunctuation) #콤마, 마침표 다 없애
sms_corpus_clean <- tm_map(sms_corpus_clean, removeWords, stopwords()) # 불용어 전부 없애. I He she and but 없애 이미 174개 코딩돼있음 

stopwords()         # 시험에 있는지 없는지 물을 수 있음 

sms_corpus_clean <- tm_map(sms_corpus_clean, stripWhitespace) # whitespace 공백. 한칸만 띄게 만들어. 10칸 띄우면 1칸으로 ㅇㅇ 

# 2단계: Sparse matrix (1단계 했으니까 2단계 고고고) 
sms_dtm <- DocumentTermMatrix(sms_corpus_clean)
dim(sms_dtm)  # 아하 5571 text에서 8298개의 단어를 찾아냈구나 ! 불필요한거 뺀거얌 디멘션이 뭐냐 ? 시험)

sms_freq_words <- findFreqTerms(sms_dtm,5) # 합이 5이상인 애들만 남겨라 column다 합쳤을 때 말야 
sms_freq_words

sms_dtm1 <- sms_dtm[,sms_freq_words]  # final sparse matrix   # 5이상인 것만 빼온거로 새로 만들어 sms를 !
dim(sms_dtm1)
convert_counts <- function(x) {
    x <- ifelse(x>0, "Yes","No")
}
sms_dtm2 <- apply(sms_dtm1, MARGIN=2 , FUN=convert_counts) #margin = 2는 column방향 

table(sms_spam$type, sms_dtm2[,7]) # 7번째 칼럼

install.packages("e1071")
library(e1071)
sms_classifier <- naiveBayes(sms_dtm2,sms_spam$type )  #naive로 테이블 만들어준거
sms_classifier                          
sms_classifier$tables$abiola
summary(sms_classifier$abiola)

sms_pred <- predict(sms_classifier, newdata=sms_dtm2)
sms_pred

# confusion matrix   이걸 만들어야 시험을 칠 수 있음 ㅇㅇ ccr sensitivity 계산
table(sms_pred, sms_spam$type)
(4813+667)/5571  #ccr
