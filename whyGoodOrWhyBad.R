#microwave
Bad<-microwave[microwave$star_rating==1,]
bad.text.mic<-tolower(Bad$review_body)
bad.text.mic=gsub("\\.","",bad.text.mic)
bad.text.mic=gsub("\\,","",bad.text.mic)
bad.text.mic<-gsub("[\n.* ]"," ", bad.text.mic);
engine<-worker(type="mix",stop_word = 'stopwords.txt')

bad.text.seg.mic<-segment(bad.text.mic,engine)

bad.freq.mc<-freq(bad.text.seg.mic)
bad.segfreq.mc <- bad.freq.mc[bad.freq.mc[,2]>=10,]

Bad<-microwave[microwave$star_rating==1,]
bad.product.mic<-tolower(Bad$product_title)
bad.product.mic=gsub("\\.","",bad.product.mic)
bad.product.mic=gsub("\\,","",bad.product.mic)
bad.product.mic<-gsub("[\n.* ]"," ", bad.product.mic);
engine<-worker(type="mix",stop_word = 'stopwords.txt')

bad.product.seg.mic<-segment(bad.product.mic,engine)

bad.freq.pro.mc<-freq(bad.product.seg.mic)
bad.segfreq.pro.mc <- bad.freq.pro.mc[bad.freq.pro.mc[,2]>=10,]

#-----
good<-microwave[microwave$star_rating==5,]
good.text.mic<-tolower(good$review_body)
good.text.mic=gsub("\\.","",good.text.mic)
good.text.mic=gsub("\\,","",good.text.mic)
good.text.mic<-gsub("[\n.* ]"," ", good.text.mic);
engine<-worker(type="mix",stop_word = 'stopwords.txt')

good.text.seg.mic<-segment(good.text.mic,engine)

good.freq.mc<-freq(good.text.seg.mic)
good.segfreq.mc <- good.freq.mc[good.freq.mc[,2]>=10,]


#hair dryer
Bad<-hair_dryer[hair_dryer$star_rating==1,]
bad.text.dry<-tolower(Bad$review_body)
bad.text.dry=gsub("\\.","",bad.text.dry)
bad.text.dry=gsub("\\,","",bad.text.dry)
bad.text.dry<-gsub("[\n.* ]"," ", bad.text.dry);
engine<-worker(type="mix",stop_word = 'stopwords.txt')

bad.text.seg.dry<-segment(bad.text.dry,engine)

bad.freq.dry<-freq(bad.text.seg.dry)
bad.segfreq.dry <- bad.freq.dry[bad.freq.dry[,2]>=10,]

good<-hair_dryer[hair_dryer$star_rating==5,]
good.text.dry<-tolower(good$review_body)
good.text.dry=gsub("\\.","",good.text.dry)
good.text.dry=gsub("\\,","",good.text.dry)
good.text.dry<-gsub("[\n.* ]"," ", good.text.dry);
engine<-worker(type="mix",stop_word = 'stopwords.txt')

good.text.seg.dry<-segment(good.text.dry,engine)

good.freq.dry<-freq(good.text.seg.dry)
good.segfreq.dry <- good.freq.mc[good.freq.mc[,2]>=10,]

#-----bad product
Bad<-hair_dryer[hair_dryer$star_rating==1,]
bad.product.dry<-tolower(Bad$product_title)
bad.product.dry=gsub("\\.","",bad.product.dry)
bad.product.dry=gsub("\\,","",bad.product.dry)
bad.product.dry<-gsub("[\n.* ]"," ", bad.product.dry);
engine<-worker(type="mix",stop_word = 'stopwords.txt')

bad.product.seg.dry<-segment(bad.product.dry,engine)

bad.freq.pro.dry<-freq(bad.product.seg.dry)
bad.segfreq.pro.dry <- bad.freq.pro.dry[bad.freq.pro.dry[,2]>=10,]



#pacifier
Bad<-pacifier[pacifier$star_rating==1,]
bad.text.pac<-tolower(Bad$review_body)
bad.text.pac=gsub("\\.","",bad.text.pac)
bad.text.pac=gsub("\\,","",bad.text.pac)
bad.text.pac<-gsub("[\n.* ]"," ", bad.text.pac);
engine<-worker(type="mix",stop_word = 'stopwords.txt')

bad.text.seg.pac<-segment(bad.text.pac,engine)

bad.freq.pac<-freq(bad.text.seg.pac)
bad.segfreq.pac <- bad.freq.pac[bad.freq.pac[,2]>=10,]

good<-pacifier[pacifier$star_rating==5,]
good.text.pac<-tolower(good$review_body)
good.text.pac=gsub("\\.","",good.text.pac)
good.text.pac=gsub("\\,","",good.text.pac)
good.text.pac<-gsub("[\n.* ]"," ", good.text.pac);
engine<-worker(type="mix",stop_word = 'stopwords.txt')

good.text.seg.pac<-segment(good.text.pac,engine)

good.freq.pac<-freq(good.text.seg.pac)
good.segfreq.pac <- good.freq.mc[good.freq.mc[,2]>=10,]
#----bad product
Bad<-pacifier[pacifier$star_rating==1,]
bad.product.pac<-tolower(Bad$product_title)
bad.product.pac=gsub("\\.","",bad.product.pac)
bad.product.pac=gsub("\\,","",bad.product.pac)
bad.product.pac<-gsub("[\n.* ]"," ", bad.product.pac);
engine<-worker(type="mix",stop_word = 'stopwords.txt')

bad.product.seg.pac<-segment(bad.product.pac,engine)

bad.freq.pro.pac<-freq(bad.product.seg.pac)
bad.segfreq.pro.pac <- bad.freq.pro.pac[bad.freq.pro.pac[,2]>=10,]




##----bad idf mic-----
library(tidytext)
bad.freq.mc$group<-"microwave"
bad.freq.dry$group<-"hair_dryer"
bad.freq.pac$group<-"pacifier"
bad.text.product<-rbind(bad.freq.mc,bad.freq.dry,bad.freq.pac)
tf_idf <- bad.text.product %>%
  bind_tf_idf (char, group, freq) %>%
  arrange (desc (tf_idf))

tf_idf %>%
  group_by(group) %>%
  top_n(10, tf_idf) %>%
  ungroup() %>%
  mutate(word = reorder(freq, tf_idf)) %>%
  ggplot(aes(char, tf_idf, fill = group)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ group, scales = "free") +
  ylab("tf-idf") +
  coord_flip()+theme_minimal()

seg.mc$group<-"microwave"
seg.dry$group<-"hair dryer"
seg.pacifier$group<-"seg.pacifier"
seg.group.title<-rbind(seg.mc,seg.dry,seg.pacifier)
library(dplyr)
tf_idf_title <- seg.group.title%>%
  bind_tf_idf (char, group, freq) %>%
  arrange (desc (tf_idf))
tf_idf_title %>%
  group_by(group) %>%
  top_n(10, tf_idf) %>%
  ungroup() %>%
  mutate(word = reorder(freq, tf_idf)) %>%
  ggplot(aes(char, tf_idf, fill = group)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ group, scales = "free") +
  ylab("tf-idf") +
  xlab("Keywords")+
  coord_flip()+theme_minimal()

##----good idf mic-----
good.freq.mc$group<-"microwave"
good.freq.dry$group<-"hair_dryer"
good.freq.pac$group<-"pacifier"
good.text.product<-rbind(good.freq.mc,good.freq.dry,good.freq.pac)
good_tf_idf <- good.text.product %>%
  bind_tf_idf (char, group, freq) %>%
  arrange (desc (tf_idf))

good_tf_idf %>%
  group_by(group) %>%
  top_n(10, tf_idf) %>%
  ungroup() %>%
  mutate(word = reorder(freq, tf_idf)) %>%
  ggplot(aes(char, tf_idf, fill = group)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ group, scales = "free") +
  ylab("tf-idf") +
  coord_flip()+theme_minimal()




#-----tf-idf bad product title-----
bad.freq.pro.mc$group<-"microwave"
bad.freq.pro.dry$group<-"hair_dryer"
bad.freq.pro.pac$group<-"pacifier"
bad.pro.product<-rbind(bad.freq.pro.mc,bad.freq.pro.dry,bad.freq.pro.pac)
tf_idf_pro <- bad.pro.product %>%
  bind_tf_idf (char, group, freq) %>%
  arrange (desc (tf_idf))
tf_idf_pro



tf_idf_pro %>%
  group_by(group) %>%
  top_n(10, tf_idf) %>%
  ungroup() %>%
  mutate(word = reorder(freq, tf_idf)) %>%
  ggplot(aes(char, tf_idf, fill = group)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ group, scales = "free") +
  ylab("tf-idf") +
  coord_flip()+theme_minimal()


#词关联
library(widyr)
library(igraph)
library(ggraph)
head(bad.segfreq.mc)#词频大于10
head(bad.segfreq.pro.mc)#词频大于10
top50bad.mic<-bad.segfreq.mc%>%
  top_n(50,freq)
top50bad.pro.mic<-bad.segfreq.pro.mc%>%
  top_n(50,freq)
bad



newsgroup_cors <- bad.text.product %>%
  pairwise_cor(group, char,freq, sort = TRUE)



#microwave
onestar.mic<-microwave[microwave$star_rating==1,]
onestar.mic<-tolower(onestar.mic$review_body)
onestar.mic=gsub("\\.","",onestar.mic)
onestar.mic=gsub("\\,","",onestar.mic)
onestar.mic<-gsub("[\n.* ]"," ", onestar.mic);

twostar.mic<-microwave[microwave$star_rating==2,]
twostar.mic<-tolower(twostar.mic$review_body)
twostar.mic=gsub("\\.","",twostar.mic)
twostar.mic=gsub("\\,","",twostar.mic)
twostar.mic<-gsub("[\n.* ]"," ", twostar.mic);

threestar.mic<-microwave[microwave$star_rating==3,]
threestar.mic<-tolower(threestar.mic$review_body)
threestar.mic=gsub("\\.","",threestar.mic)
threestar.mic=gsub("\\,","",threestar.mic)
threestar.mic<-gsub("[\n.* ]"," ", threestar.mic);

fourstar.mic<-microwave[microwave$star_rating==4,]
fourstar.mic<-tolower(fourstar.mic$review_body)
fourstar.mic=gsub("\\.","",fourstar.mic)
fourstar.mic=gsub("\\,","",fourstar.mic)
fourstar.mic<-gsub("[\n.* ]"," ", fourstar.mic);

fivestar.mic<-microwave[microwave$star_rating==5,]
fivestar.mic<-tolower(fivestar.mic$review_body)
fivestar.mic=gsub("\\.","",fivestar.mic)
fivestar.mic=gsub("\\,","",fivestar.mic)
fivestar.mic<-gsub("[\n.* ]"," ", fivestar.mic);
engine<-worker(type="mix",stop_word = 'stopwords.txt')

onestar.mic<-segment(onestar.mic,engine)
twostar.mic<-segment(twostar.mic,engine)
threestar.mic<-segment(threestar.mic,engine)
fourstar.mic<-segment(fourstar.mic,engine)
fivestar.mic<-segment(fivestar.mic,engine)

onestar.freq.mc<-freq(onestar.mic)
twostar.freq.mc<-freq(twostar.mic)
threestar.freq.mc<-freq(threestar.mic)
fourstar.freq.mc<-freq(fourstar.mic)
fivestar.freq.mc<-freq(fivestar.mic)

top100onestar.freq.mc<-onestar.freq.mc%>%
  top_n(100,freq)
top100twostar.freq.mc<-twostar.freq.mc%>%
  top_n(100,freq)
top100threestar.freq.mc<-threestar.freq.mc%>%
  top_n(100,freq)
top100fourstar.freq.mc<-fourstar.freq.mc%>%
  top_n(100,freq)
top100fivestar.freq.mc<-fivestar.freq.mc%>%
  top_n(100,freq)

top100onestar.freq.mc$stars<-1
top100twostar.freq.mc$stars<-2
top100threestar.freq.mc$stars<-3
top100fourstar.freq.mc$stars<-4
top100fivestar.freq.mc$stars<-5

top100onestar.freq.mc<-top100onestar.freq.mc[1:100,]
top100twostar.freq.mc<-top100twostar.freq.mc[1:100,]
top100threestar.freq.mc<-top100threestar.freq.mc[1:100,]
top100fourstar.freq.mc<-top100fourstar.freq.mc[1:100,]
top100fivestar.freq.mc<-top100fivestar.freq.mc[1:100,]
top100.mic<-rbind(top100onestar.freq.mc,top100twostar.freq.mc,top100threestar.freq.mc,
                  top100fourstar.freq.mc,top100fivestar.freq.mc)
head(top100.mic)

dim(top100onestar.freq.mc)
dim(top100twostar.freq.mc)
dim(top100threestar.freq.mc)
dim(top100fourstar.freq.mc)
dim(top100fivestar.freq.mc)
library(widyr)

starsgroup_cors <- top100.mic %>%
  pairwise_cor(stars, char, freq, sort = TRUE)

library(ggraph)
library(igraph)
set.seed(2019)

starsgroup_cors %>%
  filter(correlation > .4) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(alpha = correlation, width = correlation)) +
  geom_node_point(size = 10, color = "lightblue") +
  geom_node_text(aes(label = name),size=8,col=2, repel = TRUE) +
  theme_void()

top100tf_idf<-tf_idf%>%
  top_n(100,tf_idf)

top100tf_idf_pro<-tf_idf_pro%>%
  top_n(100,tf_idf)
top100tf_idf_pro<-top100tf_idf_pro[1:100,]

top100tf_idf_pro<-top100tf_idf_pro[,c("char","freq")]
top100tf_idf<-top100tf_idf[,c("char","freq")]
names(top100tf_idf)<-c("review","freq1")
names(top100tf_idf_pro)<-c("product","freq2")
head(top100tf_idf)

top100idf<-cbind(top100tf_idf,top100tf_idf_pro)
top100idf[1,10]
idfgroup_cors <- top100idf[1:10,] %>%
  pairwise_cor(product, reviews, freq, sort = TRUE)

#-----------
Bad<-microwave[microwave$star_rating==1,]
bad.product.mic<-tolower(Bad$product_title)
bad.product.mic=gsub("\\.","",bad.product.mic)
bad.product.mic=gsub("\\,","",bad.product.mic)
bad.product.mic<-gsub("[\n.* ]"," ", bad.product.mic);
engine<-worker(type="mix",stop_word = 'stopwords.txt')

bad.product.seg.mic<-segment(bad.product.mic,engine)

bad.freq.pro.mc<-freq(bad.product.seg.mic)
bad.segfreq.pro.mc <- bad.freq.pro.mc[bad.freq.pro.mc[,2]>=10,]

#samsung michelle lg  Frigidaire appliance
IDT<-c()
for(i in 1:nrow(microwave)){
  Idt<-ifelse("lg" %in% segment(tolower(microwave$review_headline),engine)[i],1,0)
  IDT<-c(Idt,IDT)
}
microwave$identify<-IDT
sum(IDT)
microwave[microwave$identify==1,]$score

#write.csv(microwave,"microwave.csv")
nrow(microwave)
nrow(hair_dryer)
nrow(pacifier)/nrow(microwave)
nrow(pacifier)/nrow(hair_dryer)
mean(microwave$star_rating)
