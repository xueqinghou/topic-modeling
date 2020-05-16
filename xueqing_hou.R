rm(list=ls())
library(tm)
library(topicmodels)
library(text2vec)

#### read documents in one variable #####
docs = Corpus(DirSource("/Users/kouyukihare/Desktop/social media/food/fb"))
mystop = c('US', 'recipe','today', 'http', 'https', '“', '”', 'we', 'what', 'friend', 'day','great','good',
            'will','just','new','love','make','know','time','birthday','food','free','get','can','thank','thanks','many','made','now','see','please') 

#### create DTM ####
dtm = DocumentTermMatrix(docs, control=list(tolower=T, removePunctuation=T, removeNumbers=T, stripWhitespace=T, stopwords=c(stopwords("english"),mystop)))
dtm = removeSparseTerms(dtm,0.97)
dtm = as.matrix(dtm)

#### create time serise ####
x = as.Date("2010-12-01") + months(1:60)

#### plot the trend of ingredients ####
plot(type = 'l',x,dtm[,colnames(dtm)=="cauliflower"],  main = "Num of Posts of Cauliflower",xlab = 'Month',ylab = 'Number')
plot(type = 'l',x,dtm[,colnames(dtm)=="zucchini"], main = "Num of Posts of Zucchini",,xlab = 'Month',ylab = 'Number')
plot(type = 'l',x,dtm[,colnames(dtm)=="pumpkin"], main = "Num of Posts of pumpkin",,xlab = 'Month',ylab = 'Number')

#### detect food frequency ####
freq = c()
term_frequency = function(food){
  for (y in 2011:2015){
    for (m in 1:12) {
      file = paste("fpost-",y,"-", m, ".csv",sep='')
      post = readLines(paste('/Users/kouyukihare/Desktop/social media/food/fbfood/fb',y,'/',file,sep = ''))  
      post = tolower(post)
      frequency = length(post[grepl(as.character(food), post,ignore.case=TRUE, perl=TRUE)])  
      totalfreq = length(post)
      percentf =frequency/totalfreq
      freq = c(freq,percentf)  
    }
  }
  freq
}

############# Cauliflower Rice ################
frequency1 = term_frequency("cauliflower rice") 
df1 = data.frame(x,frequency1)
plot(df1,type = "l",main="Frequency of posts Cauliflower Rice",xlab = 'Month',ylab = 'Frequency')
timeseries1 = ts(data = df1$freq,frequency=12)    
ggseasonplot(timeseries1)

############# Vegetable Noodle  ################
frequency2 = term_frequency("vegetable noodle|zucchini noodle") 
df2 = data.frame(x,frequency2)
plot(df2,type = "l",main="Frequency of posts Vegetable Noodle",xlab = 'Month',ylab = 'Frequency')
timeseries2 = ts(data = df2$freq,frequency=12)    
ggseasonplot(timeseries2)

############# Pumpkin Pie (Validation) ################
frequency3 = term_frequency("pumpkin pie") 
df3 = data.frame(x,frequency3)
plot(df3,type = "l",main="Frequency of posts Pumpkin Pie",xlab = 'Month',ylab = 'Frequency')
timeseries3 = ts(data = df3$freq,frequency=12)    
ggseasonplot(timeseries3)

