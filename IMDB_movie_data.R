

install.packages(c("SnowballC","wordcloud","RColorBrewer"))

library("SnowballC")
library("wordcloud")
library("RColorBrewer")


install.packages("tm")
library(tm)



imDB = read.csv("C:\\workspace\\R\\data_sample\\IMDB_movie_data\\IMDB-Movie-Data.csv")


str(imDB)
variable.names(imDB)
summary(imDB)

# 결측치의 확인
is.na(imDB$Metascore)
sum(is.na(imDB$Metascore))
colSums(is.na(imDB))

# 결측치의 삭제 ; 전체 삭제
imDB2 = na.omit(imDB)
colSums(is.na(imDB2))

# 특정 변수에 결측치가 존재하는 경우에만 삭제
imDB3 = imDB[complete.cases(imDB[ , 12]), ]
colSums(is.na(imDB3))

# 결측치가 true이면 58.99입력
imDB$Metascore2 = imDB$Metascore
imDB$Metascore2 [is.na(imDB$Metascore2)] = 58.99

# 결측치를 생략하고 계산
mean(imDB$Revenue..Millions., na.rm = TRUE)

# 일반적으로, 연속형 변수는 평균으로 대체
# 이산형 변수는 최빈값으로 대체

# 결측치를 대체할때는 아래 사항으 확인
summary(imDB$Revenue..Millions.)
plot(x = imDB$Revenue..Millions.)
library(ggplot2)
ggplot(imDB, aes(x= imDB$Revenue..Millions.)) + 
  geom_histogram()

# 극단치(outlier)
ggplot(imDB, aes(x = as.factor(Year), y = Revenue..Millions.)) + 
  geom_boxplot(aes(fill = as.factor(Year)), outlier.colour = 'red', alpha = I(0.4))

variable.names(imDB)



# Outlier인 데이터 제거하기

# 1분위수 계산
Q1 = quantile(imDB$Revenue..Millions.,probs = c(0.25),na.rm = TRUE) 
# 3분위수 계산
Q3 = quantile(imDB$Revenue..Millions.,probs = c(0.75),na.rm = TRUE)

LC = Q1 - 1.5 * (Q3 - Q1) # 아래 울타리
UC = Q3 + 1.5 * (Q3 - Q1) # 위 울타리

IMDB2 = subset(imDB,
               Revenue..Millions. >  LC & Revenue..Millions. < UC)


strA = substr(imDB$Actors[1], 1, 5)
strB = substr(imDB$Actors[2], 1, 5)
strP = paste(strA, "_", strB, sep = "")

strC = strsplit(as.character(imDB$Actors[1]), split = ",")

imDB$Genre2 = gsub(",", " ", imDB$Genre)




## 텍스트 마이닝


CORPUS = Corpus(VectorSource(imDB$Genre2)) # 코퍼스 생성
CORPUS_TM = tm_map(CORPUS, removePunctuation) # 특수문자 제거
CORPUS_TM = tm_map(CORPUS_TM, removeNumbers) # 숫자 제거
CORPUS_TM = tm_map(CORPUS_TM, tolower) # 소문자로 변경


TDM = DocumentTermMatrix(CORPUS_TM)
inspect(TDM)
TDM = as.data.frame(as.matrix(TDM))
head(TDM)


IMDB_GENRE = cbind(imDB, TDM)

CORPUS=Corpus(VectorSource(IMDB$Description))
CORPUS_TM = tm_map(CORPUS,stripWhitespace)
CORPUS_TM = tm_map(CORPUS_TM,removePunctuation)
CORPUS_TM = tm_map(CORPUS_TM, removeNumbers)
CORPUS_TM = tm_map(CORPUS_TM, tolower)

DTM = DocumentTermMatrix(CORPUS_TM)
inspect(DTM)

CORPUS_TM = tm_map(CORPUS_TM,removeWords, 
                   c(stopwords("english"),"my","custom","words"))


convert_count = function(x) {
  y <- ifelse(x > 0,1,0)
  y = as.numeric(y)
  y
}


# Temr Document Matrix 생성

TDM = TermDocumentMatrix(CORPUS_TM)


# 워드클라우드 생성
m = as.matrix(TDM) 
v = sort(rowSums(m),decreasing=TRUE) # 빈도수를 기준으로 내림차순 정렬
d = data.frame(word = names(v),freq=v)  # 데이터 프레임 생성




# min.freq -> 최소 5번 이상 쓰인 단어만 띄우기
# max.words -> 최대 200개만 띄우기
# random.order -> 단어 위치 랜덤 여부

wordcloud(words = d$word, freq = d$freq, min.freq = 5,  
          max.words=200, random.order=FALSE,
          colors=brewer.pal(8, "Dark2"))



barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
        col ="lightblue", main ="Most frequent words",
        ylab = "Word frequencies")


