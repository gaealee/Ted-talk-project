getwd()
ted <- read.csv("Desktop/L/framework 2/project/ted_main.csv")

ted <- read.csv("ted_main.csv",header = T,check.names = F)
library(ggplot2)
#install.packages('gridExtra')
library(gridExtra)
#install.packages('gridextra')
library(tidyverse)
#install.packages('tidyverse')
library(tidytext)
#install.packages('tidytext')
library(reshape2)
library(scales)
library(RColorBrewer)
#install.packages('anytime')
library(anytime)
library(qdap)
library(ggthemes)
library(gtools)
library(corrplot)
library(tm)
library(tidyr) 
library(dplyr)
library(ggplot2)
library(ggthemes)
library(rpart); 
library(rpart.plot)
library(qdap)
library(qdapTools)
library(wordcloud)
library(tidyr)
library(data.table)
library(ggrepel)
library(treemap)
library(viridis)
library(jsonlite)
library(stringr)

#a. Find out the data structure of analysisdata
res <- sapply(ted, class)
table(res)

str(res)
?table
#b. Find missing values and remove them
res <- sapply(ted, function(x) sum(is.na(x)))
miss <- sort(res, decreasing = T)
miss[miss>0]
ted <- na.omit(ted)

miss
anyNA(ted)
#c. Change the UNIX timestamps to human readablle data
#install.packages('anytime')
ted$film_date <- anydate(ted$film_date)
ted$published_date <- anydate(ted$published_date)

#d.	Create a new variable that has a scaled figure of views 
#(showing baseline rating of where the view number of each video is). 
ted <- ted %>%
  mutate(viewscaled = scale(ted$views))

#Putting a quantile variable for views
quartVIEW <- as.numeric(quantcut(ted$views, q = 4))
ted <- cbind(ted, quartVIEW)

#e.	Modify the ratings variable to have the counts of emotion ratings 
#more interpretable

ls = list()

for (i in (1:nrow(ted))) {
  data = ted[i,"ratings"]
  to.remove = c("[[:punct:]]","id","name","count")
  for (r in to.remove) {
    
    data = gsub(r,"",data)
  }
  data = strsplit(data," ")[[1]]
  index1 = seq(from = 4, to = 100,by = 6)
  index2 = index1 + 2
  rating = as.data.frame(data[index1])
  count = as.data.frame(as.numeric(as.character(data[index2])))
  
  data = cbind(rating,count)
  colnames(data) = c("rating","count")
  data = data[complete.cases(data),]
  data = data[order(data$count,decreasing = T),]
  data$title = ted[i,"title"]
  
  ls[[i]] = data
  
}
ratings = do.call(rbind,ls)
ratings.cols = dcast(ratings,title ~ rating, value.var = "count")
ted = merge(ted,ratings.cols,by.x = "title",by.y = "title",all.x = T)

#Extracting each tag word and counting them, creating a new column just like ratings

ted$tags = as.character(ted$tags)
ls = list()
for (i in 1:nrow(ted)){
  data = ted$tags[i]
  data = gsub("'","",data)
  data = substr(data,2,nchar(data)-1)
  data = as.data.frame(data)
  data = unnest_tokens(data,tag,data, token = "regex", pattern = ",")
  data$tag = trimws(data$tag,which = "left")
  data$tag = gsub("[[:punct:]]","",data$tag)
  data$title = ted[i,"title"]
  colnames(data) = c("tag","title")
  
  ls[[i]] = data
  
}
tag.data = do.call(rbind,ls)
tag.summary = summarise(group_by(tag.data,tag), count = n())
tag.summary = tag.summary[order(tag.summary$count,decreasing = T),]
tag.summary

#putting a quartile value for Funny
quarFUNNY <- as.numeric(quantcut(ratings.cols$Funny, q = 4))
ted <- cbind(ted,quarFUNNY)
#Putting jaw dropping a quartile value
quarJD <- as.numeric(quantcut(ratings.cols$Jawdropping, q = 4))
ted <- cbind(ted, quarJD)
#Putting inspiring in a quartile value (highest rating)
quarINS <- as.numeric(quantcut(ratings.cols$Inspiring, q = 4))
ted <- cbind(ted,quarINS)
#Putting informative
quarINF <- as.numeric(quantcut(ratings.cols$Informative, q = 4))
ted <- cbind(ted,quarINF)
#Combining the bad rating values- combining Longwinded, Confusing, Unconvincing, and Obnoxious
combNEG <- ratings.cols$Obnoxious + ratings.cols$Longwinded + ratings.cols$Confusing + ratings.cols$Unconvincing
quarNEG <- as.numeric(quantcut(combNEG, q = 4))
ted <- cbind(ted, quarNEG)

#f. Tokenize the variable 'description'. For this dataset, it is 
#important to do this in the begging in order to have description text 
#values be available in the preliminary exploratory analysis. 

#create a corpus from the variable description
ted$description[12]
#install.packages('tm')
corpus = Corpus(VectorSource(ted$description))
#convert to lower case
corpus = tm_map(corpus, FUN = content_transformer(tolower))
#remove punctuation
corpus = tm_map(corpus, FUN = removePunctuation)
#remove stopwords
corpus = tm_map(corpus, FUN = removeWords, c(stopwords('english')))
#strip whitespace
corpus = tm_map(corpus, FUN = stripWhitespace)
#create a dictionary
dict = findFreqTerms(DocumentTermMatrix(Corpus(VectorSource(ted$description))), lowfreq=0)
dict_corpus = Corpus(VectorSource(dict))
#use tm_map to stem words
corpus = tm_map(corpus,FUN = stemDocument)
#Create a document term matrix
dtm = DocumentTermMatrix(corpus)
#Remove Sparse Terms
xdtm = removeSparseTerms(dtm,sparse = 0.95)
#complete stems
xdtm = as.data.frame(as.matrix(xdtm))
colnames(xdtm) = stemCompletion(x = colnames(xdtm),dictionary = dict_corpus,type='prevalent')
colnames(xdtm) = make.names(colnames(xdtm))

ted1 <- ted[c("comments","duration","languages","main_speaker","num_speaker","published_date","ratings","speaker_occupation","tags","views","title")]
#checking the colnames and dimeantions of the new table
colnames(ted1)

#Converting the Published date to a normal date format and creating Month and Year 
#column to be used later in the analysis
#install.packages('lubridate')
library(lubridate)
ted1$published_date <- as.Date(as.character(ymd_hms(as.POSIXct(as.numeric(
  ted1$published_date),origin = '2014-04-09', tz = "GMT"))),format = "%Y-%m-%d")
ted1$published_month <- factor(month.abb[month(ted$published_date)])
ted1$published_year <- year(ted$published_date)

#Adding a ‘sno’ column in the datse
library(dplyr)
len <- ted1 %>% 
  summarise(sno = n())
ted1$sno <- seq(1,as.numeric(len))
ted1$sno

#  Cleaning the Ratings column and transforming it to number of positive,
#Negative and Neutral ratings
#reading the values as json to get the values in rows
df1 <- c()
for (i in 1:2550)
{
  df <- fromJSON(str_replace_all(ted1$rating[i],"'",'"'))
  df$sno <- i 
  df1 <- rbind(df,df1)
}

#Creating a table with the ratings
ted_ratings <- df1

#Checking the distinct rating types available
df %>% distinct(name)

#Classified the distinct rating types to positive, negative and neutral ratings
negative_words <- c('Unconvincing','Confusing','Obnoxious','Longwinded')
positive_words <- c('Informative','Inspiring','Fascinating','Ingenious','Beautiful','Persuasive','Jaw-dropping','Courageous','Funny')

df1$ratings_type <- ifelse(df1$name %in% unlist(negative_words),'negative_ratings',ifelse(df1$name %in% unlist(positive_words),'positive_ratings',ifelse(df1$name == 'OK','neutral_ratings',' ')))

ted2 <- df1 %>% group_by(sno,ratings_type) %>% 
  summarise(count_rating_type = sum(count)) %>% spread(ratings_type,count_rating_type) %>% ungroup() %>%
  inner_join(ted1,by = "sno")

#Cleaning the Speaker occupation field
ted1$speaker_occupation[1:5]
#replacing all the ;,/ to blanks
ted2$speaker_occupation <- ted2$speaker_occupation %>% str_replace_all('/',' ') %>% str_replace_all(',',' ')   %>% str_replace_all(';',' ') %>% str_replace_all('\\+',' ') %>% tolower()

#Unnesting each occupation
df2 <- unnest_tokens(ted2,occupation1,speaker_occupation) %>% select(sno,occupation1)

#stop word list to be removed
stop_words <-  c('and','of','in','expert','social','the','for')

#removing stop words and renaming similar words 
df2 <- df2 %>% subset(!occupation1 %in% stop_words) %>% mutate(occupation1 = str_replace_all(occupation1, 
                                                                                             c("writer" = "author","scientists" = "scientist","researcher" = "scientist","neuroscientist" = "scientist", 
                                                                                               "professor" = "educator", "scholar" = "educator", "education" = "educator", "teacher" = "educator", 
                                                                                               "songauthor" = "author","editor" = "author","data" = "data related","analyst" = "data related",
                                                                                               "statistician" = "data related", "musician" = "artist","singer" = "artist","sing" = "artist","poet" = "artist","actor" = "artist", "comedian" = "artist","playwright" = "artist","media" = "artist","performance" = "artist","guitarist" = "artist", "dancer" = " artist","humorist" = "artist","pianist" = "artist", "violinist" = "artist","magician" = "artist","artists" = "artist","band" = "artist", "director" = "filmmaker", "producer" = "filmmaker", "entrepreneur" = "business","ceo" = "business", "founder" = "business", "psychology" = "psychologist", "physician" = "health", "medical" = "health", "doctor" = "health", "design" = "designer", "designerer" = "designer", "reporter" = "journalist"))) 

#creating a list of top 20 words
occupation_by_rank <- df2 %>% group_by(occupation1) %>% summarise(n = n_distinct(sno)) %>% arrange(desc(n))
top_20_occ <- occupation_by_rank[1:20,1]
data.table(head(occupation_by_rank,20))

ted3 <- df2 %>%  mutate(rank = ifelse(occupation1 %in% unlist(top_20_occ),1,0)) %>% arrange(sno,desc(rank)) %>%
  subset(!duplicated(sno)) %>% right_join(ted2,by = "sno") %>% 
  mutate(speaker_occupation = ifelse(is.na(occupation1),"others",occupation1)) %>% 
  select(-(occupation1))


#Cleaning the tags field
ted3$tags[1:2]
#unnesting individual tags from the field
ted3$tags <- ted3$tags %>% str_replace_all('\\[','') %>% str_replace_all('\\]','')   %>% str_replace_all("\\'",' ') %>% str_replace_all(',',' ') %>% tolower()

talk_tags <- unnest_tokens(ted3,tags1,tags) %>% select(sno,tags1)
data.table(head(talk_tags,10))

#Creating the final dataset
ted_final <- ted3 %>%
  select(c("sno","main_speaker","title","num_speaker","comments","positive_ratings","negative_ratings","neutral_ratings","duration","languages","speaker_occupation","views","published_month","published_year","published_date")) %>%
  mutate(ratings = positive_ratings + negative_ratings + neutral_ratings)

#Combine tokenized words to views in main dataset to see correlation
tednum <- select(ted, views)
tedtok <- cbind(tednum, xdtm)
corrplot::corrplot(cor(tedtok))
cor(tedtok)
desc(cor(tedtok))

#Ratings based dataframe

tedrat <- select(ted, name, speaker_occupation, views, event, Beautiful, Courageous,
                 Fascinating, Funny, Informative, Ingenious, Inspiring, Jawdropping,
                 Longwinded, Obnoxious, OK, Persuasive, Unconvincing)

#Making a correlation matrix:
tedratnum <- as.matrix(select(ted, views, Beautiful, Courageous,
                              Fascinating, Funny, Informative, Ingenious, Inspiring, Jawdropping,
                              Longwinded, Obnoxious, OK, Persuasive, Unconvincing))

anyNA(tedratnum)
corrplot(cor(tedratnum))

(cor(tedratnum))

#Most common ratings
summarise(group_by(ratings,rating))

rating.summary = summarise(group_by(ratings,rating),sum(count))
colnames(rating.summary) = c("word","n")
rating.summary$word = as.character(rating.summary$word)
rating.summary$n = as.integer(rating.summary$n)
rating.summary = rating.summary[order(rating.summary$n,decreasing = T),]

p = ggplot(rating.summary,aes(x = reorder(word,n),y=n)) + geom_bar(stat = "identity") +
  coord_flip() +   labs(title = "Count by Rating",
                        x = "Count",
                        y = "Rating") + scale_y_continuous(label = comma)
p = p + theme(panel.background = element_blank(),axis.line = element_line("black"),axis.ticks = element_blank())
p

#Ratings by Views

tedratnum2 <- as.data.frame(tedratnum)

#Beautiful
ggplot(data = tedratnum2) + aes(x = Beautiful, y = views) +
  geom_point()

#Funny
ggplot(data = tedratnum2) + aes(x = Funny, y = views) +
  geom_point()

#Longwinded
ggplot(data = tedratnum2) + aes(x = Longwinded, y = views) +
  geom_point()

#Inspiring
ggplot(data = tedratnum2) + aes(x = Inspiring, y = views) +
  geom_point()

#Making a function to plot a chart: From Kaggle Kernels
#Settings: 
col1 = brewer.pal(8,"Set2")[3]
col2 = brewer.pal(8,"Set2")[4]
mycols = c(col1,col2)
plot.bar = function(plot.data,xlab,ylab,title)
{
  #make basic plot
  plot = ggplot(plot.data,aes(x = x, y = y, fill = y)) + geom_bar(stat = "identity",fill = col1)
  #add colour, remove legend, rename axis, add title
  plot = plot + xlab(xlab) + ylab(ylab) + ggtitle(title) + scale_y_continuous(labels = comma)
  #remove borders, grid line and background
  plot = plot + theme_bw() + theme(panel.grid = element_blank(),panel.border = element_blank(),
                                   axis.line = element_line(colour = "grey"),axis.ticks = element_blank(), 
                                   plot.title = element_text(size = 20,face = "bold",hjust = 0.5))
  print(plot)
}

#Additional modifications for charts to work

#Break up groups into classes

unique(sapply(ted,class))

int = colnames(ted)[as.vector(sapply(ted,class))== "integer"]
fac = colnames(ted)[as.vector(sapply(ted,class)) == "factor"]

ted$film_date = as.POSIXlt(ted$film_date,origin = "1970-01-01")
ted$published_date = as.POSIXlt(ted$published_date,origin = "1970-01-01")

#Above, using POSIXltto extract month and year from them. 
ted$mm = ted$published_date$mon
ted$yy = ted$published_date$year + 1900
ted$mmyy = paste(ted$published_date$mon,ted$published_date$year + 1900,sep = "-")

#How many talks are published each year
plot.data = summarise(group_by(ted[,c("yy","title")],yy),count = n())
colnames(plot.data) = c("x","y")
plot.data$x = as.character(plot.data$x)

plot.bar(plot.data,"year","number of talks","Ted Talks published by year")

#Which years had the highest views 
plot.data = summarise(group_by(ted[,c("yy","title","views")],yy),sum(views)/1000000)
colnames(plot.data) = c("x","y")
plot.data$x = as.character(plot.data$x)

plot.bar(plot.data,"year","total views, millions","Ted Talks total views by year")

#Plotting the number of views and published year
plot.data = summarise(group_by(ted[,c("views","yy","title")],yy),count = n(),sum(views))
colnames(plot.data) = c("year","count","views")


#plot.data$year = as.factor(plot.data$year)
plot.data$views = plot.data$views/1000000
plot.data = melt(plot.data,id = "year")

plot = ggplot(plot.data,aes(x = year,y = value,group = variable,colour = variable)) + 
  geom_line(size = 2)

plot = plot + scale_x_continuous(breaks = plot.data$year) + 
  theme(panel.grid = element_blank(), 
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_line(colour = "grey")) + 
  scale_colour_manual(values = c(col1,col2),guide = F)+ 
  ylab("") 

#Below is how you put a label on the actual chart
plot = plot + ggplot2::annotate("text",x = 2008, y = 80, 
                                label = "Number of talks\n published",colour = col1) + 
  ggplot2::annotate("text",x = 2006.7, y = 250, label = "Views \n(millions)",colour = col2) +
  labs(title='                                Total Views and Published Ted Talks')
plot


#Chart total count by tag name
p = ggplot(tag.summary[1:20,], aes(x = reorder(tag,count), y = count)) + 
  geom_bar(stat = "identity", fill = col1) + coord_flip() + scale_y_continuous(label = comma) + 
  xlab("tag") + labs(title='Count of Tag types')
p

#Text Analysis approaches

#DESCRIPTION VARIABLE

#Browsing tokens created through corpus created
plot(sort(colSums(xdtm), decreasing = T))

#Term Matrix with Term Frequency - Inverse Document Frequency Weighting
dtm_tfidf = DocumentTermMatrix(x=corpus,control = list(weighting=function(x) weightTfIdf(x,normalize=F)))
xdtm_tfidf = removeSparseTerms(dtm_tfidf,sparse = 0.95)
xdtm_tfidf = as.data.frame(as.matrix(xdtm_tfidf))
colnames(xdtm_tfidf) = stemCompletion(x = colnames(xdtm_tfidf),
                                      dictionary = dict_corpus,type='prevalent')
colnames(xdtm_tfidf) = make.names(colnames(xdtm_tfidf))
sort(colSums(xdtm_tfidf),decreasing = T)

xdtm[1:10, 2:10]
xdtm_tfidf[1:10, 2:10]

#Contrasting weights of term frequency and term frequency inverse document weighting for top 20 terms. 

#install.packages('ggthemes')
data.frame(term = colnames(xdtm),tf = colMeans(xdtm), tfidf = colMeans(xdtm_tfidf))%>%
  arrange(desc(tf))%>%
  top_n(20)%>%
  gather(key=weighting_method,value=weight,2:3)%>%
  ggplot(aes(x=term,y=weight,fill=weighting_method))+geom_col(position='dodge')+
  coord_flip()+theme_minimal()

#Predicting Views from Data
views_desc <- cbind(Views = ted$views, xdtm)
Views_desc_tfidf <- cbind(Views = ted$views,xdtm_tfidf)

set.seed(617)
split = sample(1:nrow(views_desc),size = 0.7*nrow(views_desc))
train = views_desc[split,]
test = views_desc[-split,]

set.seed(617)
split2 = sample(1:nrow(Views_desc_tfidf),size = 0.7*nrow(Views_desc_tfidf))
train2 = Views_desc_tfidf[split2,]
test2 = Views_desc_tfidf[-split2,]

#MODEL ON DATA TERM MATRIX TERMS

#Tree based predictive model
#install.packages('rpart.plot')
tree <- rpart(Views~.,train)
rpart.plot(tree)

pred_tree <- predict(tree, newdata = test)
rmse_tree <- sqrt(mean((pred_tree - test$Views)^2))
rmse_tree

tree2 <- rpart(Views~.,train2)
rpart.plot(tree)

pred_tree2 <- predict(tree2, newdata = test2)
rmse_tree2 <- sqrt(mean((pred_tree2 - test$Views)^2))
rmse_tree

#Linear Regression based predictive model
reg <- lm(Views~.,train)
summary(reg)

pred_reg <- predict(reg, newdata = test)
rmse_reg <- sqrt(mean((pred_reg-test$Views)^2))
rmse_reg

reg2 <- lm(Views~.,train2)
summary(reg)

median(ted$views)
mean(ted$views)

pred_reg2 <- predict(reg2, newdata = test2)
rmse_reg2 <- sqrt(mean((pred_reg2-test$Views)^2))
rmse_reg2
rmse_reg
#Predictions on using description corpuses with Ratings

#Inspiring rating
ratinsp <- cbind(Insp = ted$Inspiring, xdtm)
set.seed(617)
split = sample(1:nrow(ratinsp),size = 0.7*nrow(ratinsp))
train = ratinsp[split,]
test = ratinsp[-split,]

tree <- rpart(Insp~.,train)
rpart.plot(tree)

pred_tree <- predict(tree, newdata = test)
rmse_tree <- sqrt(mean((pred_tree - test$Insp)^2))
rmse_tree

#Linear Regression based predictive model
reg <- lm(Insp~.,train)
summary(reg)

pred_reg <- predict(reg, newdata = test)
rmse_reg <- sqrt(mean((pred_reg-test$Insp)^2))
rmse_reg

#Funny
ratfun <- cbind(Funn = ted$Funny, xdtm)
set.seed(617)
split = sample(1:nrow(ratfun),size = 0.7*nrow(ratfun))
train = ratfun[split,]
test = ratfun[-split,]
tree <- rpart(Funn~.,train)
rpart.plot(tree)

pred_tree <- predict(tree, newdata = test)
rmse_tree <- sqrt(mean((pred_tree - test$Funn)^2))
rmse_tree

#Linear Regression based predictive model
reg <- lm(Funn~.,train)
summary(reg)

pred_reg <- predict(reg, newdata = test)
rmse_reg <- sqrt(mean((pred_reg-test$Funn)^2))
rmse_reg

#Sentiment Analysis

#Most common words in description-
#install.packages('qdap')

freq_terms(text.var = ted$description, top = 25)
plot(freq_terms(text.var = ted$description, top = 25))

#Removing Stopwords
freq_terms(text.var = ted$description,top = 25, stopwords = Top200Words)

#Which Words are most tied to views
description_words_by_views = word_list(ted$description,
                                       grouping.var=tolower(ted$views),
                                       stopwords = Top200Words,
                                       cut.n = 25,
                                       apostrophe.remove = T)
plot(freq_terms(description_words_by_views))
freq_terms(description_words_by_views, top = 25, stopwords = c(Top200Words, 'c', '?','says','s','cthe', 'ted', 'ca', 'freq', 
                                                               'listword',
                                                               'talk','talk',"talks","ted","it's","says"))

plot(freq_terms(description_words_by_views, top = 25, stopwords = c(Top200Words, 'c', '?','says','s','cthe', 'ted', 'ca', 'freq', 'listword',
                                                                    'talk','talk',"talks","ted","it's","says")))
#Which description words are most tied to videos rated Funny
description_words_by_Funny = word_list(ted$description,
                                       grouping.var=tolower(ted$Funny),
                                       stopwords = Top200Words,
                                       cut.n = 25,
                                       apostrophe.remove = T)
plot(freq_terms(description_words_by_Funny, top = 25, stopwords = c(Top200Words, 'c', '?','says','s','cthe', 'ted', 'ca', 'freq', 'listword',
                                                                    'talk','talk',"talks","ted","it's","says")))

#Using word lexicons Bing 
as.data.frame(get_sentiments('bing'))[1:50,]

#Valence of words in description by name
ted$description <- as.character(ted$description)
ted$name <- as.character(ted$name)
ted%>%
  group_by(main_speaker)%>%
  unnest_tokens(output = word, input = description)%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(sentiment)


#Total number of positive and negative words in description grouped by name
ted%>%
  group_by(main_speaker)%>%
  unnest_tokens(output = word, input = description)%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(sentiment)%>%
  count()

#Chart of positive v negative
ted%>%
  group_by(main_speaker)%>%
  unnest_tokens(output = word, input = description)%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(sentiment)%>%
  count()%>%
  ggplot(aes(x=sentiment,y=n,fill=sentiment))+geom_col()+theme_economist()+guides(fill=F)

#Do positive words in descripption correspond to more views

ted %>%
  select(main_speaker,description,quartVIEW)%>%
  group_by(main_speaker)%>%
  unnest_tokens(output=word,input=description)%>%
  ungroup()%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(quartVIEW, sentiment)%>%
  summarize(n = n())%>%
  mutate(proportion = n/sum(n))

str(ted$views)
quantile(ted$views)

#Visualizing Views with negative or positive:
ted %>%
  select(main_speaker,description,quartVIEW)%>%
  group_by(main_speaker)%>%
  unnest_tokens(output=word,input=description)%>%
  ungroup()%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(quartVIEW,sentiment)%>%
  summarize(n = n())%>%
  mutate(proportion = n/sum(n))%>%
  ggplot(aes(x=quartVIEW,y=proportion,fill=sentiment))+geom_col()+theme_economist()

#Do positive words in descripption correspond to more Funny Videos

ted %>%
  select(main_speaker,description,quarFUNNY)%>%
  group_by(main_speaker)%>%
  unnest_tokens(output=word,input=description)%>%
  ungroup()%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(quarFUNNY, sentiment)%>%
  summarize(n = n())%>%
  mutate(proportion = n/sum(n))

#Visualize
ted %>%
  select(main_speaker,description,quarFUNNY)%>%
  group_by(main_speaker)%>%
  unnest_tokens(output=word,input=description)%>%
  ungroup()%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(quarFUNNY,sentiment)%>%
  summarize(n = n())%>%
  mutate(proportion = n/sum(n))%>%
  ggplot(aes(x=quarFUNNY,y=proportion,fill=sentiment))+geom_col()+theme_economist()

#Count Using NRC for varying emotions
ted%>%
  group_by(main_speaker)%>%
  unnest_tokens(output = word, input = description)%>%
  inner_join(get_sentiments('nrc'))%>%
  group_by(sentiment)%>%
  count()

#Visualize by emotion
ted%>%
  group_by(main_speaker)%>%
  unnest_tokens(output = word, input = description)%>%
  inner_join(get_sentiments('nrc'))%>%
  group_by(sentiment)%>%
  count()%>%
  ggplot(aes(x=reorder(sentiment,X = n),y=n,fill=sentiment))+geom_col()+guides(fill=F)+coord_flip()+theme_wsj()

#Visualizing emotion in words in FUNNY rated videos
ted%>%
  group_by(quarFUNNY)%>%
  unnest_tokens(output = word, input = description)%>%
  inner_join(get_sentiments('nrc'))%>%
  group_by(quarFUNNY, sentiment)%>%
  count()%>%
  ggplot(aes(x=reorder(sentiment,X = n),y=n,fill=sentiment))+geom_col()+guides(fill=F)+coord_flip()+theme_wsj()

#Emotions based on quartile views
ted%>%
  group_by(main_speaker)%>%
  unnest_tokens(output = word, input = description)%>%
  inner_join(get_sentiments('nrc'))%>%
  group_by(main_speaker,sentiment,quartVIEW)%>%
  count()%>%
  group_by(sentiment, quartVIEW)%>%
  summarize(n = mean(n))%>%
  ungroup()%>%
  ggplot(aes(x=quartVIEW,y=n,fill=quartVIEW))+
  geom_col()+
  facet_wrap(~sentiment)+
  guides(fill=F)+coord_flip()

#Emotions based on quartile videos FUNNY
ted%>%
  group_by(main_speaker)%>%
  unnest_tokens(output = word, input = description)%>%
  inner_join(get_sentiments('nrc'))%>%
  group_by(main_speaker,sentiment,quarFUNNY)%>%
  count()%>%
  group_by(sentiment, quarFUNNY)%>%
  summarize(n = mean(n))%>%
  ungroup()%>%
  ggplot(aes(x=quarFUNNY,y=n,fill=quarFUNNY))+
  geom_col()+
  facet_wrap(~sentiment)+
  guides(fill=F)+coord_flip()

#Emotions based on quartile Negative words
ted%>%
  group_by(main_speaker)%>%
  unnest_tokens(output = word, input = description)%>%
  inner_join(get_sentiments('nrc'))%>%
  group_by(main_speaker,sentiment,quarNEG)%>%
  count()%>%
  group_by(sentiment, quarNEG)%>%
  summarize(n = mean(n))%>%
  ungroup()%>%
  ggplot(aes(x=quarNEG,y=n,fill=quarNEG))+
  geom_col()+
  facet_wrap(~sentiment)+
  guides(fill=F)+coord_flip()

#Name variable

#Emotions based on quartile with Funny rating
ted%>%
  group_by(main_speaker)%>%
  unnest_tokens(output = word, input = name)%>%
  inner_join(get_sentiments('nrc'))%>%
  group_by(main_speaker,sentiment,quarFUNNY)%>%
  count()%>%
  group_by(sentiment, quarFUNNY)%>%
  summarize(n = mean(n))%>%
  ungroup()%>%
  ggplot(aes(x=quarFUNNY,y=n,fill=quarFUNNY))+
  geom_col()+
  facet_wrap(~sentiment)+
  guides(fill=F)+coord_flip()

#Do positive words in the name correspond to more views
ted %>%
  select(main_speaker,name,quartVIEW)%>%
  group_by(main_speaker)%>%
  unnest_tokens(output=word,input=name)%>%
  ungroup()%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(quartVIEW, sentiment)%>%
  summarize(n = n())%>%
  mutate(proportion = n/sum(n))

#Visualizing
ted %>%
  select(main_speaker,name,quartVIEW)%>%
  group_by(main_speaker)%>%
  unnest_tokens(output=word,input=name)%>%
  ungroup()%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(quartVIEW,sentiment)%>%
  summarize(n = n())%>%
  mutate(proportion = n/sum(n))%>%
  ggplot(aes(x=quartVIEW,y=proportion,fill=sentiment))+geom_col()+theme_economist()

#Wordclouds

#Wordcloud on Description Data

#adding talks to stopwords:
talk <- c('talk','SMART')
stop_words <- rbind(stop_words, talk)
stop_words$word

wordcloudData = 
  ted%>%
  group_by(main_speaker)%>%
  unnest_tokens(output=word,input=description)%>%
  anti_join(stop_words)%>%
  group_by(word)%>%
  summarize(freq = n())%>%
  arrange(desc(freq))%>%
  ungroup()%>%
  data.frame()

#wordcloudData
set.seed(617)
wordcloud(words = wordcloudData$word,wordcloudData$freq,scale=c(2,0.5),max.words = 100,colors=brewer.pal(9,"Spectral"))

#Compare and Contrast positive and negative words from Description Variable 
wordcloudData = 
  ted%>%
  group_by(main_speaker)%>%
  unnest_tokens(output=word,input=description)%>%
  anti_join(stop_words)%>%
  inner_join(get_sentiments('bing'))%>%
  ungroup()%>%
  count(sentiment,word,sort=T)%>%
  spread(key=sentiment,value = n,fill=0)%>%
  data.frame()
rownames(wordcloudData) = wordcloudData[,'word']
wordcloudData = wordcloudData[,c('positive','negative')]
set.seed(617)
comparison.cloud(term.matrix = wordcloudData,scale = c(2,0.5),max.words = 200, rot.per=0)

#Charts for other analysis (mainly refernced from kernels)
themefn=theme(legend.position="none", axis.text.x=element_text(angle=90,hjust=1),
              plot.title=element_text(size=12,hjust=0.5,face="bold",margin=margin(b=20)))
ggplot(ted,aes(Fascinating,views,size=Fascinating,col=views))+geom_point(alpha=0.8)+geom_smooth(method=lm)+
  themefn+labs(x="Fascinating",y="Views",title="No of views Vs Fascinating")+
  scale_color_gradientn(colors=colorRampPalette(brewer.pal(9,'OrRd'))(100))+
  scale_x_continuous(labels=scales::comma)

#From Kernel
legend.position="none"
themefn=theme(legend.position="none", axis.text.x=element_text(angle=90,hjust=1),
              plot.title=element_text(size=12,hjust=0.5,face="bold",margin=margin(b=20)))
ggplot(ted,aes(views,duration,size=views,col=duration))+geom_point(alpha=0.8)+geom_smooth(method=lm)+
  themefn+labs(x="No.of Views",y="Duration",title="No of views Vs Duration")+
  scale_color_gradientn(colors=colorRampPalette(brewer.pal(9,'OrRd'))(100))+
  scale_x_continuous(labels=scales::comma)


#Top 10 Most Viewed Talks

tedview <- select(ted, views, event, main_speaker, tags)
temp=data.table(tedview) #Data.table
temp=temp[,head(.SD,5),by=event] %>% 
  select(event,views, main_speaker) %>% 
  group_by(event) #Select top 5 talks in each event by views

temp$viewsinMl=round(temp$views/1000000,2) #convert views to millions
pattern="TED[:digit:]" #Select only TEDYYYY events.
tedevent=temp[str_detect(temp$event,pattern)==TRUE,] #Subset the pattern to separate DF

#Most viewed talk by event
temp %>% arrange(desc(viewsinMl)) %>% head(10) %>% ggplot(aes(reorder(event,viewsinMl),viewsinMl,fill=main_speaker))+
  geom_bar(stat="identity",position=position_dodge())+theme(legend.position="none",axis.text.x = element_text(angle=90),
                                                            plot.title = element_text(size=15,hjust=0.5))+
  labs(x="Event",y="Views in Million",title="Top 10 Most Viewed talks")+coord_flip()+
  geom_label_repel(aes(label=main_speaker,fill=factor(event)),fontface="bold",color="white", box.padding=.3)

#Treemap of speakers around ted events
#install.packages('treemap')
treemap(tedevent,index=c("event","main_speaker"),vSize ="viewsinMl",vColor = "event",palette="Set3",title="TED Events",
        sortID ="-viewsinMl",border.col = "#FFFFFF",type="categorical",fontsize.legend = 0,
        fontsize.title = 17,bg.labels = "#FFFFFF") #Visualize using treemap

#Which events had the highest number of talks
temp2 <- ted %>% group_by(event) %>% tally() %>% arrange(desc(n))
ggplot(head(temp2,20),aes(factor(event,levels=event),n,fill=event))+
  geom_bar(stat="identity")+scale_fill_viridis(discrete=TRUE,option="C")+
  themefn+labs(x="Event",y="Number of Talks",title="Top 20 Events by number of talks")+
  scale_x_discrete(labels=function(x) str_wrap(x,width=10))

#Treemap of most viewed speakers from talk with highest number of talks (TED2014)
pattern="TED2014"
TED2014=temp[str_detect(temp$event,pattern)==TRUE,]
treemap(TED2014,index=c("event","main_speaker"),vSize ="viewsinMl",vColor = "main_speaker",
        palette="Set3",title="TED2014",sortID ="-viewsinMl",border.col = "#FFFFFF",
        type="categorical",fontsize.legend = 0,fontsize.title = 17,bg.labels = "#FFFFFF")


#Treemap of speakers around ted events
#install.packages('treemap')
treemap(tedevent,index=c("event","main_speaker"),vSize ="viewsinMl",vColor = "event",palette="Set3",title="TED2014",
        sortID ="-viewsinMl",border.col = "#FFFFFF",type="categorical",fontsize.legend = 0,
        fontsize.title = 17,bg.labels = "#FFFFFF") #Visualize using treemap

#Wordcloud using the speakers occupation
temp=ted %>% group_by(speaker_occupation) %>% tally(sort=TRUE)
attach(temp)
wordcloud(speaker_occupation,n,scale=c(3,0.008),min.freq=5,random.order = TRUE,
          random.color = TRUE,rot.per = 0.3,colors=c("#d7dd35","#465c8b","#3f3250","#ccdfcb"))

#Wordcloud using speakers that were rated as funny
temp=ted %>% group_by(speaker_occupation, Funny) %>% tally(sort=TRUE)
attach(temp)
wordcloud(speaker_occupation,n,scale=c(3,0.008),min.freq=5,random.order = TRUE,
          random.color = TRUE,rot.per = 0.3,colors=c("#d7dd35","#465c8b","#3f3250","#ccdfcb"))

#########ted talks over time#########################
#getting a dot for number of comments by published year
ted_final %>%
  mutate(published_year1 = as.factor(published_year)) %>%
  group_by(published_year1) %>%
  summarise(avg_comments = mean(comments)) %>%
  ggplot(aes(x = published_year1, y = avg_comments)) +
  geom_point(col = "tomato2", size = 3) +
  geom_segment(aes(x = published_year1,xend = published_year1,y = min(avg_comments),yend = max(avg_comments)),linetype = "dashed",size = 0.05) +
  coord_flip() +
  labs(title = "Number of Comments by Published Year", x = "Published year", y = "Average # of Comments") +
  theme_minimal()


#Getting a dot chart for number of ratings by published year
ted_final %>%
  mutate(published_year1 = as.factor(published_year)) %>%
  group_by(published_year1) %>%
  summarise(avg_ratings = mean(ratings)) %>%
  ggplot(aes(x = published_year1, y = avg_ratings)) +
  geom_point(col = "tomato2", size = 3) +
  geom_segment(aes(x = published_year1,xend = published_year1,y = min(avg_ratings),yend = max(avg_ratings)),linetype = "dashed",size = 0.05) +
  coord_flip() +
  labs(title = "Number of ratings by Published Year", x = "Published year", y = "Average # of Ratings")+
  theme_minimal()


#creating a stacked bar chart showing percentag eof positive, neutral and negative ratings by the published year
ted_final %>%
  group_by(published_year) %>%
  summarise(Perc_Positive_Ratings= sum(positive_ratings)/sum(ratings), Perc_Negative_Ratings = sum(negative_ratings)/sum(ratings), Perc_Neutral_Ratings = sum(neutral_ratings)/sum(ratings)) %>%
  gather(Type, Perc_rating ,-published_year) %>%
  ggplot(aes(x = published_year, y = Perc_rating, fill = Type)) + geom_bar(stat = "identity") +
  labs(title = "Percentage of Positive, Negative and Neutral Ratings by Published Year", x = "Published year", y = "% of Ratings") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()



###########what make the best ted talks##########################
### i. Do publishing months impact views?
# Adding levels to published month
month_order <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
ted_final$published_month <- factor(ted_final$published_month, levels = month_order)


#Plotting Views by published month facetted over last 7 years
ted_final %>%
  filter(published_year >= 2010) %>%
  group_by(published_year,published_month) %>%
  summarise(m_views = sum(views)) %>%
  inner_join(ted_final %>%
               filter(published_year >= 2010) %>%
               group_by(published_year) %>%
               summarise(y_views = sum(views)),by = "published_year") %>%
  mutate(perc_views = m_views/y_views) %>%
  ggplot(aes(x = published_month,y = perc_views,group = 1, color = published_year)) +
  geom_point() + geom_line() + facet_wrap(~published_year,ncol = 1) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Published Month", y = "Percent COntribution in Yearly views", title = "Monthly percentage views over years 2010-2017 - Seasonality") +
  theme_minimal()



### ii. does speaker occupation affect the TED Talks

#Defining the view category
ted_final$view_category <-
  ifelse(between(ted_final$views,quantile(ted_final$views,0),quantile(ted_final$views,0.20)),'Worst',
         ifelse(between(ted_final$views,quantile(ted_final$views,0.20),quantile(ted_final$views,0.40)),'Bad',
                ifelse(between(ted_final$views,quantile(ted_final$views,0.40),quantile(ted_final$views,0.60)),'Ok',
                       ifelse(between(ted_final$views,quantile(ted_final$views,0.60),quantile(ted_final$views,0.80)),'Good',
                              ifelse(ted_final$views > quantile(ted_final$views,0.80),'Best','NA')))))
#creating a function to create a wordcloud and the frequency chart by the view category used in the function call
generate_cloud_grph <- function(v_cat){
  df_wc <- as.data.frame(ted_final %>%
                           subset(view_category == v_cat,select = c(speaker_occupation,view_category)) %>%
                           count(speaker_occupation, sort = TRUE))
  
  wordcloud(words = df_wc$speaker_occupation, freq = df_wc$n, min.freq = 1,
            max.words = 100, random.order = FALSE, rot.per = 0.35,
            colors = brewer.pal(8, "Dark2"))
  
  ted_final %>%
    filter(view_category == v_cat) %>%
    group_by(speaker_occupation) %>%
    summarise(n = n()) %>%
    arrange(desc(n)) %>%
    head(10) %>%
    ggplot(aes(x = reorder(speaker_occupation,n), y = n, label = n)) +
    geom_point(size = 6) +
    geom_segment(aes(x = speaker_occupation,
                     xend = speaker_occupation,
                     y = 0,
                     yend = n)) +
    geom_text(color = "white", size = 3) + coord_flip() +
    labs(x = "Frequency",y = "Speaker Occupation") +
    theme_classic()
}

# best ted talks
generate_cloud_grph("Best")

#worst ted talks
generate_cloud_grph("Worst")

#Generating the comparison clouds for Best and Worst Ted Talks
set.seed(1234)
ted_final %>% select(speaker_occupation,view_category) %>%
  subset(view_category %in% c('Best','Worst')) %>%
  group_by(speaker_occupation,view_category) %>%
  summarise(n = n()) %>%
  acast(speaker_occupation ~ view_category, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"),
                   max.words = 100)



### iii. are longer ted talks less viewed?
#creating the interactive box plot of duration by category of TED talk
update.packages("ggplot2")
install.packages("plotly")
library(plotly)
ted_final %>%
  plot_ly(y = ~duration, color = ~view_category, type = "box")

cor(ted_final$views,ted_final$duration) #0.04874043



### iv. do viewers prefer co-presenters?
#creating a variable to club  more than one speaker as co-speaker talks
datatable(ted_final %>%
            mutate(No_of_Speakers = ifelse(num_speaker == 1 , '1','>1')) %>%
            group_by(No_of_Speakers) %>%
            summarise(count = n()))

#creating a boxplot of views by number of speakers category
ted_final %>%
  mutate(No_of_Speakers = ifelse(num_speaker == 1 , '1','>1')) %>%
  ggplot(aes(x = No_of_Speakers, y = views, fill = No_of_Speakers)) +
  geom_boxplot() +
  scale_y_log10(labels = scales::comma) +
  theme_minimal()



### v. do views increase with more languages?
#Creating a boxplot of number of languages by view catgory
ted_final %>%
  ggplot(aes(x = view_category, y = languages)) +
  geom_boxplot(width = 0.3, fill = "plum") + coord_flip() +
  labs( x = "View Category", y = "# of Languages", title = "Languages vs View_category") +
  theme_minimal()

cor(ted_final$views,ted_final$languages) #0.3776231



### vi. do tags associated to a ted talk affect views?

#generating a comparison word cloud for Best and Worst TED talks
set.seed(1234)
talk_tags %>%
  inner_join(ted_final, by = "sno") %>%
  select(view_category, tags1) %>%
  filter(!(tags1 %in% c('global','tedx'))) %>%
  subset(view_category %in% c('Best','Worst')) %>%
  group_by(tags1,view_category) %>%
  summarise(n = n())  %>%
  acast(tags1 ~ view_category, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#F8766D", "#00BFC4"), max.words = 100)




#########assign number to ratings####################
cols <- ratings.cols[,-1];head(cols)
ratingcol <- ratings.cols
ratings.cols$sum<- rowSums(cols)
head(ratings.cols)

# assign numeric ratings, the positive words should have the highest
# rating and the negative words should assign the lowest rating
ratings.cols$Beautiful <- ratings.cols$Beautiful * 4.6
ratings.cols$Confusing <-ratings.cols$Confusing * 2.5
ratings.cols$Courageous <-ratings.cols$Courageous * 4.7
ratings.cols$Fascinating <-ratings.cols$Fascinating * 4.9
ratings.cols$Funny <-ratings.cols$Funny * 4.8
ratings.cols$Informative <-ratings.cols$Informative * 4.8
ratings.cols$Ingenious <-ratings.cols$Ingenious * 4.7
ratings.cols$Inspiring <-ratings.cols$Inspiring * 4.9
ratings.cols$Jawdropping <-ratings.cols$Jawdropping * 4.8
ratings.cols$Longwinded <-ratings.cols$Longwinded * 2.2
ratings.cols$Obnoxious <-ratings.cols$Obnoxious * 2
ratings.cols$OK <-ratings.cols$OK * 3
ratings.cols$Persuasive <-ratings.cols$Persuasive * 4.8
ratings.cols$Unconvincing <-ratings.cols$Unconvincing * 2.5

head(ratings.cols)

# overall_ranking top 10 talks
aa <- ratings.cols[,-1]
aa<- aa[,-15]
head(aa)
ratings.cols$avg <- rowSums(aa)/ratings.cols$sum

head(ratings.cols)

avg_title <- data.frame(ratings.cols$title,ratings.cols$avg)
avg_title <-as.vector(avg_title)
ranked<-avg_title[order(avg_title$ratings.cols.avg, decreasing = T),]
head(ranked,10)
#                                        ratings.cols.title        ratings.cols.avg
#2481       Why we all need to practice emotional first aid         4.828267
#2456                    Why our screens make us less happy         4.823867
#2160                          Transplant cells, not organs         4.820353
#1233             Lifesaving scientific tools made of paper         4.812024
#866            How I help free innocent people from prison         4.811852
#2535        Your brain hallucinates your conscious reality         4.802251
#2305           What reality are you creating for yourself?         4.799495
#1821                       The happy secret to better work         4.798017
#2309     What rivers can tell us about the earth's history         4.794898
#2443 Why journalists have an obligation to challenge power         4.794648

# only negative ratings included(longwinded, unconvincing, obnoxious,confusing)
# - top 10 talks
negative <- data.frame(ratings.cols$Longwinded,ratings.cols$Unconvincing,ratings.cols$Obnoxious,
                       ratings.cols$Confusing)
negative$total <- rowSums(negative)
bb <- data.frame(ratingcol$Longwinded,ratingcol$Unconvincing,ratingcol$Obnoxious,
                 ratingcol$Confusing)
negative$sum <- rowSums(bb)
negative$avg <- negative$total/negative$sum
ne_avg_title <- data.frame(ratings.cols$title, negative$avg)
ranked2<-ne_avg_title[order(ne_avg_title$negative.avg, decreasing = F),]
head(ranked2,10)
#                                ratings.cols.title        ratings.cols.avg
#2456                        Why our screens make us less happy     2.000000
#1389 No one should die because they live too far from a doctor     2.050000
#2128        This tennis icon paved the way for women in sports     2.077778
#1349                                 My trek to the South Pole     2.083000
#407              Asking for help is a strength, not a weakness     2.100000
#1529                         Sanitation is a basic human right     2.109091
#2438       Why I take the piano on the road ... and in the air     2.109091
#864                                   How I harnessed the wind     2.122642
#824           How data from a crisis text line is saving lives     2.125000
#1584                          Songs that bring history to life     2.125000



#############clustering#################
str(ted)
str(ted_final)

# choose variable duration, languages, num_speaker and avg_rating to do clustering
cluster_data <- cbind(ted$duration, ted$languages, ted$num_speaker, avg_title$ratings.cols.avg,
                      ted_final$published_year)
colnames(cluster_data) = c("duration", "languages", "num_speaker", "avg_ratings", "published_year")
cluster_data<- as.data.frame(cluster_data)

#split data
set.seed(1706)
split = createDataPartition(y=cluster_data$avg_ratings, p = 0.7, list = F, groups = 100)
train = cluster_data[split,]
test = cluster_data[-split,]

# linear regression predicitive Model
linear = lm(avg_ratings~., train)
summary(linear)


sseLinear = sum(linear$residuals^2); sseLinear #93.48971
predLinear = predict(linear,newdata=test)
sseLinear = sum((predLinear-test$avg_ratings)^2); sseLinear #36.6233


####cluster then predict (cluster first and then run a model to see if there is an improvement)

# before cluster, remove the outcome variable
trainMinusDV = subset(train,select=-c(avg_ratings))
testMinusDV = subset(test,select=-c(avg_ratings))

## prepare for clustering

# normalize the data
library(caret)
preproc = preProcess(trainMinusDV)
trainNorm = predict(preproc,trainMinusDV)
testNorm = predict(preproc,testMinusDV)

# hierarchical cluster analysis
distances = dist(trainNorm,method = 'euclidean')
clusters = hclust(d = distances,method = 'ward.D2')
cor(cophenetic(clusters),distances) #0.4441152
plot(color_branches(as.dendrogram(clusters),k = 4,groupLabels = F))
clusterGroups = cutree(clusters,k=4)

#visualize
temp = data.frame(cluster = factor(clusterGroups),
                  factor1 = fa(trainNorm,nfactors = 2,rotate = 'varimax')$scores[,1],
                  factor2 = fa(trainNorm,nfactors = 2,rotate = 'varimax')$scores[,2])
ggplot(temp,aes(x=factor1,y=factor2,col=cluster))+
  geom_point()

library(cluster)
clusplot(trainNorm,
         clusterGroups,
         color=T,shade=T,labels=4,lines=0,main='Hierarchical Cluster Plot')

# K-means clustering
set.seed(1706)
km = kmeans(x = trainNorm,centers = 4,iter.max=10000,nstart=100)
#km$centers ##0.3193323
mean(km$cluster==clusterGroups) # %match between results of hclust and kmeans

#total within sum of squares plot
within_ss = sapply(1:10,FUN = function(x) kmeans(x = trainNorm,centers = x,iter.max = 1000,nstart = 25)$tot.withinss)
ggplot(data=data.frame(cluster = 1:10,within_ss),aes(x=cluster,y=within_ss))+
  geom_line(col='steelblue',size=1.2)+
  geom_point()+
  scale_x_continuous(breaks=seq(1,10,1))

# ratio plot
ratio_ss = sapply(1:10,FUN = function(x) {km = kmeans(x = trainNorm,centers = x,iter.max = 1000,nstart = 25)
km$betweenss/km$totss} )
ggplot(data=data.frame(cluster = 1:10,ratio_ss),aes(x=cluster,y=ratio_ss))+
  geom_line(col='steelblue',size=1.2)+
  geom_point()+
  scale_x_continuous(breaks=seq(1,10,1))

# Silhouette Plot
silhoette_width = sapply(2:10,FUN = function(x) pam(x = trainNorm,k = x)$silinfo$avg.width)
ggplot(data=data.frame(cluster = 2:10,silhoette_width),aes(x=cluster,y=silhoette_width))+
  geom_line(col='steelblue',size=1.2)+
  geom_point()+
  scale_x_continuous(breaks=seq(2,10,1))
##The elbow plots support a three cluster solution while silhoette plot recommends( im not sure of this, seems 3 or 5?)
##We are going to go with three clusters as dividing the data into many groups would not leave enough data to fit a model.


#apply clustering solution from train set to test set
km_kcca = as.kcca(km,trainNorm) # flexclust uses objects of the classes kcca
clusterTrain = predict(km_kcca)
clusterTest = predict(km_kcca,newdata=testNorm)

table(clusterTrain)
## clusterTrain
##    1    2    3    4
##   48   572 602  635

table(clusterTest)
## clusterTrain
## 1    2   3   4
## 10  221 223  239

#split train and test based on cluster membership
train1 = subset(train,clusterTrain==1)
train2 = subset(train,clusterTrain==2)
train3 = subset(train,clusterTrain==3)
train4 = subset(train,clusterTrain==4)

test1 = subset(test,clusterTest==1)
test2 = subset(test,clusterTest==2)
test3 = subset(test,clusterTest==3)
test4 = subset(test,clusterTest==4)


########### linear regression predict for each cluster then combine
lm1 = lm(avg_ratings~.,train1)
lm2 = lm(avg_ratings~.,train2)
lm3 = lm(avg_ratings~.,train3)
lm4 = lm(avg_ratings~.,train4)

pred1 = predict(lm1,newdata=test1)
pred2 = predict(lm2,newdata=test2)
pred3 = predict(lm3,newdata=test3)
pred4 = predict(lm4,newdata=test4)

sse1 = sum((test1$avg_ratings-pred1)^2); sse1 #1.150848

sse2 = sum((test2$avg_ratings-pred2)^2); sse2 #12.39138

sse3 = sum((test3$avg_ratings-pred3)^2); sse3 #14.90106

sse4 = sum((test4$avg_ratings-pred4)^2); sse4 #7.669469

predOverall = c(pred1,pred2,pred3, pred4)
avg_ratingsOverall = c(test1$avg_ratings,test2$avg_ratings,test3$avg_ratings, test4$avg_ratings)
sseOverall = sum((predOverall - avg_ratingsOverall)^2); sseOverall #36.11276

#comparison( compare the sse for model on the entire data to the sse for models on clusters.)
paste('sse for model on entire data',sseLinear)
# "sse for model on entire data 36.6232972358463"

paste('sse for model on clusters',sseOverall)
# "sse for model on clusters 36.1127563376862"



############predict using tree
tree = rpart(avg_ratings~.,train,minbucket=10)
predTree = predict(tree,newdata=test)
sseTree = sum((predTree - test$avg_ratings)^2); sseTree #38.04129

#clustering then predict using tree
tree1 = rpart(avg_ratings~.,train1,minbucket=10)
tree2 = rpart(avg_ratings~.,train2,minbucket=10)
tree3 = rpart(avg_ratings~.,train3,minbucket=10)
tree4 = rpart(avg_ratings~.,train4,minbucket=10)

pred1 = predict(tree1,newdata=test1)
pred2 = predict(tree2,newdata=test2)
pred3 = predict(tree3,newdata=test3)
pred4 = predict(tree4,newdata=test4)

sse1 = sum((test1$avg_ratings-pred1)^2); sse1 # 0.8364697

sse2 = sum((test2$avg_ratings-pred2)^2); sse2 #  13.26816

sse3 = sum((test3$avg_ratings-pred3)^2); sse3 # 16.97868

sse4 = sum((test4$avg_ratings-pred4)^2); sse4 # 8.762765

predTreeCombine = c(pred1,pred2,pred3, pred4)
avg_ratingsOverall = c(test1$avg_ratings,test2$avg_ratings,test3$avg_ratings, test4$avg_ratings)
sseTreeCombine = sum((predTreeCombine - avg_ratingsOverall)^2); sseTreeCombine #39.84608

paste('sse for model on entire data',sseTree)
##"sse for model on entire data 38.041291776803"

paste('sse for model on clusters',sseTreeCombine)
##"sse for model on clusters 39.846080403789"
