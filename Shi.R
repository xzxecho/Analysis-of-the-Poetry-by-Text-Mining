

library(tm)
library(tidyverse)
library(showtext)
library(webshot)
webshot::install_phantomjs() #首次使用需要执行
library(htmlwidgets)



library(rjson)
library(stringr)
result <- fromJSON(file = "shijing.json")

for (i in 1:length(result)){
  l=length(result[[i]]$content)
  result[[i]]$content <- paste(result[[i]]$content[1:l],collapse="")
  result[[i]]$len <- str_length(result[[i]]$content)
}

df <- data.frame(do.call('rbind',result))
df$chapter<-as.character(df$chapter)
df$chapter<-as.factor(df$chapter)
df$len <- unlist(df$len)



library(dplyr)
library(ggplot2)
library(echarts4r)
guofeng <- subset(df,chapter == "国风")
daya <- subset(df,chapter == "大雅")
xiaoya <- subset(df,chapter == "小雅")
song <- subset(df,chapter == "周颂"|chapter == "鲁颂"|chapter == "商颂")

#ggplot(df, aes(x=chapter,fill=chapter))+ 
#  geom_bar(stat = 'count')+
#  theme_classic()+
#  xlab('章节')+
#  ylab('频次')+
#  theme(legend.title=element_blank())+
#  theme(text = element_text(family = "Songti SC")) 
  
p_chapter<-df %>%
  count(chapter, name='count') %>%
  mutate(percent = count / sum(count) * 100)

p_chapter %>%
  e_charts(chapter) %>%
  e_pie(count, radius = c("40%", "75%"))%>%
  e_labels(alignTo = "none",
           formatter = "{b}({c}首,{d}%) ")

#library(gridExtra)
#library(grid)
#library(png)
#library(graphics)
#img1 <- readPNG("chapter_bar.png")
#img2 <- readPNG("chapter_pie.png")
#grid.arrange(rasterGrob(img1), rasterGrob(img2),ncol=2)

#国风饼图
guofeng_section<-guofeng %>%
  count(section, name='count') %>%
  mutate(percent = count / sum(count) * 100)

e1 <- guofeng_section %>%
  e_charts(section,height = 250) %>%
  e_pie(count, radius = c("40%", "75%"),legend = FALSE)%>%
  e_title(text="国风")%>%
  e_labels(alignTo = "none", formatter = "{b}({c}首,{d}%)")


#大雅饼图
daya_section<-daya %>%
  count(section, name='count') %>%
  mutate(percent = count / sum(count) * 100)

e2 <- daya_section %>%
  e_charts(section, height = 250) %>%
  e_pie(count, radius = c("40%", "75%"),legend = FALSE)%>%
  e_title(text="大雅")%>%
  e_labels(alignTo = "none",formatter = "{b}\n({c}首,{d}%)")


#小雅饼图
xiaoya_section<-xiaoya %>%
  count(section, name='count') %>%
  mutate(percent = count / sum(count) * 100)

e3 <- xiaoya_section %>%
  e_charts(section, height = 250) %>%
  e_pie(count, radius = c("40%", "75%"),legend = FALSE)%>%
  e_title(text="小雅")%>%
  e_labels(alignTo = "none",formatter = "{b}\n({c}首,{d}%)")

#颂饼图
song_section<-song %>%
  count(section, name='count') %>%
  mutate(percent = count / sum(count) * 100)

e4 <- song_section %>%
  e_charts(section, height = 250) %>%
  e_pie(count, radius = c("40%", "75%"),legend = FALSE)%>%
  e_title(text="颂")%>%
  e_labels(alignTo = "none",formatter = "{b}\n({c}首,{d}%)")

e_arrange(e1,e2,e3, e4, cols = 2, rows = 2)

#length
pbox <- ggplot(data = df, aes(x = chapter, y = len)) + 
  geom_boxplot(aes(fill = chapter))+
  theme_classic()+
  xlab('章节')+
  ylab('字数')+
  theme(legend.title=element_blank())+
  theme(text = element_text(family = "Songti SC")) 
ggsave(pbox,filename = "pbox.png")

#violin+boxplot
ggplot(data = df, aes(x = chapter, y = len)) + 
  geom_violin(aes(fill = chapter), trim = FALSE)+
  geom_boxplot(aes(fill = chapter),width=0.1)+
  theme_classic()+
  xlab('章节')+
  ylab('字数')+
  theme(legend.title=element_blank())+
  theme(text = element_text(family = "Songti SC")) 


#piechart
#ggplot(data=df, mapping=aes(x="chapter",fill=chapter))+
#  geom_bar(stat="count",width=0.5,position='stack')+
#  coord_polar("y", start=0)+
#  labs(x = '', y = '', title = '')+
#  theme_classic()+
#  theme(axis.ticks = element_blank()) +  # 将左上角边框的刻度去掉
#  theme(axis.text = element_blank())+
#  theme(text = element_text(family = "Songti SC"))+
#  theme(panel.border=element_blank())





library(jiebaR)
library(wordcloud2)
words_all = paste(df$content,collapse="")
engine <- worker( ) #初始化分词引擎
segment <- segment(words_all, engine) #分词
allstopwords <- c("而","何","乎","乃","其","且","若","所","为","焉","以","因","与","于",
                  "也","则","者","之","不","得","可","是","已","此","的","兮","中","矣",
                  "在","彼","又", "亦","有","无","既","或","之")
stopword_wo <- c("而","何","乎","乃","其","且","若","所","为","焉","以","因","与","于",
                 "也","则","者","之","不","得","可","是","已","此","的","兮","中","矣",
                 "在","彼","又","亦","有","无","既","或","之","我")
segment <- filter_segment(segment,allstopwords)
wordfreqs <- freq(segment)#计算词频
wordfreqs <- arrange(wordfreqs, -freq)
my_graph <- wordcloud2(wordfreqs,size=2,fontFamily = "STKaiti",rotateRatio = 0)
my_graph
#saveWidget(my_graph,"tmp.html",selfcontained = F) #先保存为网页格式
#webshot("tmp.html","wordcloud.jpg", delay = 3,vwidth = 1000, vheight=1000) #在依据网页格式生成jpg图片格式


segment1 <-segment[  str_count(segment, pattern = "") ==1]
segment1 <- filter_segment(segment1,stopword_wo )
wordfreqs1 <- freq(segment1)#计算词频
wordfreqs1 <- arrange(wordfreqs1, -freq)
my_graph1 <- wordcloud2(wordfreqs1,size=1,fontFamily = "STKaiti",rotateRatio = 0)
my_graph1


segment2 <-segment[  str_count(segment, pattern = "") ==2]
wordfreqs2 <- freq(segment2)#计算词频
wordfreqs2 <- arrange(wordfreqs2, -freq)
my_graph2 <- wordcloud2(wordfreqs2,fontFamily = "STKaiti",size=2,rotateRatio = 0)
my_graph2
#saveWidget(my_graph2,"tmp.html",selfcontained = F) #先保存为网页格式
#webshot("tmp.html","wordcloud.jpg", delay = 3,vwidth = 1000, vheight=1000) #在依据网页格式生成jpg图片格式


segment4 <-segment[  str_count(segment, pattern = "") ==4]
wordfreqs4 <- freq(segment4)#计算词频
wordfreqs4 <- arrange(wordfreqs4, -freq)
my_graph4 <- wordcloud2(wordfreqs4,fontFamily = "STKaiti",size=1,rotateRatio = 0)
my_graph4



title <- paste(df$title,sep="")
title_freqs <- freq(title)#计算词频
title_freqs <- arrange(title_freqs, -freq)
title_graph <-wordcloud2(title_freqs ,fontFamily = "STKaiti",rotateRatio = 0)
#saveWidget(title_graph,"title.html",selfcontained = F) #先保存为网页格式
title_graph




#国风
engine <- worker() #初始化分词引擎
words_guofeng = paste(guofeng$content,collapse="")
segment <- segment(words_guofeng, engine) #分词
segment <- filter_segment(segment,stopword_wo)
wordfreqs <- freq(segment)#计算词频
wordfreqs <- arrange(wordfreqs, -freq)
guofeng_graph <- wordcloud2(wordfreqs,fontFamily = "STKaiti",size=1,rotateRatio = 0)
guofeng_graph
#saveWidget(guofeng_graph,"guofeng.html",selfcontained = F) #先保存为网页格式


#大雅
engine <- worker() #初始化分词引擎
words_daya = paste(daya$content,collapse="")
segment <- segment(words_daya, engine) #分词
segment <- filter_segment(segment,stopword_wo)
wordfreqs <- freq(segment)#计算词频
wordfreqs <- arrange(wordfreqs, -freq)
wordcloud2(wordfreqs,fontFamily = "STKaiti",size=1,rotateRatio = 0)
daya_graph <- wordcloud2(wordfreqs,fontFamily = "STKaiti",size=1,rotateRatio = 0)
daya_graph
#saveWidget(daya_graph,"daya.html",selfcontained = F) #先保存为网页格式

#小雅
engine <- worker() #初始化分词引擎
words_xiaoya = paste(xiaoya$content,collapse="")
segment <- segment(words_xiaoya, engine) #分词
segment <- filter_segment(segment,stopword_wo)
wordfreqs <- freq(segment)#计算词频
wordfreqs <- arrange(wordfreqs, -freq)
xiaoya_graph <- wordcloud2(wordfreqs,fontFamily = "STKaiti",size=2,rotateRatio = 0)
xiaoya_graph
#saveWidget(xiaoya_graph,"xiaoya.html",selfcontained = F) #先保存为网页格式

#颂
engine <- worker() #初始化分词引擎
words_song = paste(song$content,collapse="")
segment <- segment(words_song, engine) #分词
segment <- filter_segment(segment,stopword_wo)
wordfreqs <- freq(segment)#计算词频
wordfreqs <- arrange(wordfreqs, -freq)
wordcloud2(wordfreqs,fontFamily = "STKaiti",size=1,rotateRatio = 0)
song_graph <- wordcloud2(wordfreqs,fontFamily = "STKaiti",size=1,rotateRatio = 0)
song_graph
#saveWidget(song_graph,"song.html",selfcontained = F) #先保存为网页格式


#correlation
library(ggcorrplot)
c = paste(df$content,sep="")
it = itoken(c, progressbar = FALSE)
vectorizer = hash_vectorizer(2 ^ 18, c(1L, 2L))
dtm1 = create_dtm(it, vectorizer)
dtm2 = create_dtm(it, vectorizer)
#d1_d2_jac_sim = sim2(dtm1, dtm2, method = "jaccard", norm = "none")
d1_d2_cos_sim = sim2(dtm1, dtm2, method = "jaccard", norm = "none")

d1_d2_jac_sim = as.matrix(d1_d2_jac_sim)
which(d1_d2_jac_sim != 0, arr.ind = T)
ggcorrplot(d1_d2_jac_sim)



#RWMD
library(text2vec)
library(rsparse)
tokens = word_tokenizer(c)
for (i in 1:length(tokens)){
  tokens[[i]]<-tokens[[i]][!tokens[[i]] %in% allstopwords]
}
v = create_vocabulary(itoken(tokens))
v = prune_vocabulary(v, term_count_min = 5, doc_proportion_max = 0.5)
it = itoken(tokens)
vectorizer = vocab_vectorizer(v)
dtm = create_dtm(it, vectorizer)
tcm = create_tcm(it, vectorizer, skip_grams_window = 5)
glove_model = GloVe$new(rank = 50, x_max = 10)
wv = glove_model$fit_transform(tcm, n_iter = 5)
# get average of main and context vectors as proposed in GloVe paper
wv = wv + t(glove_model$components)
rwmd_model = RelaxedWordMoversDistance$new(dtm, wv)
rwms = rwmd_model$sim2(dtm[1:305, ])
head(sort(rwms[1, ], decreasing = T))



