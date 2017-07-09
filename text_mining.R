install.packages("rJava")
install.packages("Rwordseg")  
install.packages("tm")
install.packages("tmcn")
install.packages("wordcloud")
install.packages("XML")
install.packages("RCurl")

library(XML)
library(RCurl)

data <- list()

#抓取ptt文章的url
for( i in 4000:4300){
  tmp <- paste(i, '.html', sep='')
  url <- paste('https://www.ptt.cc/bbs/movie/index', tmp, sep='')
  html <- htmlParse(getURL(url,ssl.verifypeer = FALSE))
  url.list <- xpathSApply(html, "//div[@class='title']/a[@href]", xmlAttrs)
  data <- rbind(data, paste('https://www.ptt.cc', url.list, sep=''))
}
data <- unlist(data)

#所有文章的url連結去抓所有文章的html網頁
getdoc <- function(line){
  start <- regexpr('https:', line)[1]
  end <- regexpr('html', line)[1]
  
  if(start != -1 & end != -1){
    url <- substr(line, start, end+3)
    html <- htmlParse(getURL(url,ssl.verifypeer = FALSE), encoding='UTF-8')
    doc <- xpathSApply(html, "//div[@id='main-content']", xmlValue)
    name <- strsplit(url, '/')[[1]][6]
    write(doc, gsub('html', 'txt', name))
  }      
}
sapply(data, getdoc)

#處理文字
library(tm)
library(tmcn)
library(Rwordseg)
d.corpus <- Corpus(DirSource("C:/Users/AA018231/Documents"), list(language = NA))

#清除標點符號、數字、大小寫英文
d.corpus <- tm_map(d.corpus, removePunctuation)
d.corpus <- tm_map(d.corpus, removeNumbers)
d.corpus <- tm_map(d.corpus, function(word) {
  gsub("[A-Za-z0-9]", "", word)
})

#進行中文斷詞
words <- readLines("http://wubi.sogou.com/dict/download_txt.php?id=9182")
words <- toTrad(words)
insertWords(words)

#只挑出名詞來進行分析
d.corpus <- tm_map(d.corpus[1:100], segmentCN, nature = TRUE)
d.corpus <- tm_map(d.corpus, function(sentence) {
  noun <- lapply(sentence, function(w) {
    w[names(w) == "n"]
  })
  unlist(noun)
})
d.corpus <- Corpus(VectorSource(d.corpus))

#清除停用字符
myStopWords <- c(stopwordsCN(), "編輯", "時間", "標題", "發信", "實業", "作者")
d.corpus <- tm_map(d.corpus, removeWords, myStopWords)

head(myStopWords, 20)

#建立 TermDocumentMatrix
tdm <- TermDocumentMatrix(d.corpus, control = list(wordLengths = c(2, Inf)))
inspect(tdm[1:10, 1:2])

#畫出關鍵字詞雲
library(wordcloud)

m1 <- as.matrix(tdm)
v <- sort(rowSums(m1), decreasing = TRUE)
d <- data.frame(word = names(v), freq = v)
wordcloud(d$word, d$freq, min.freq = 10, random.order = F, ordered.colors = F, 
          colors = rainbow(length(row.names(m1))))
