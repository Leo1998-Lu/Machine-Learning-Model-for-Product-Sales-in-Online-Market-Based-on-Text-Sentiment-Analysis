#review body

text.body.mic<-gsub("[\n.* ]"," ",tolower(microwave$review_body))
cut.engine<-worker()
new_user_word(cut.engine,mydic)
seg<-segment(text.body.mic,cut.engine)
segfreq<-table(seg)
segfreq<-sort(segfreq,decreasing = T)[1:100]

seg.mic<-sapply(text.body.mic,segment,engine)
score.mic.body <- getEmotionalType(seg.mic, pos, neg)
score.mic.body
mic<-cbind(cor.data,score.mic.body$total)
colnames(mic)[5]<-"body.score"
#Random forest----------
forest.fit<-randomForest(rating_level~.-body.score,mic)
pred<-forest.fit$predicted
forest.perf<-table(mic$rating_level,pred,dnn=c("Actual","Predicted"))
sum(diag(prop.table(forest.perf)))###Accuracy
head(mic)
cor(data.frame(lapply(mic,as.numeric)))



######(c)######
#microwave
text.body.mic<-gsub("[\n.* ]"," ",tolower(microwave$review_body))
cut.engine<-worker(type="mix",stop_word = 'stopwords.txt')
new_user_word(cut.engine,mydic)
seg<-segment(text.body.mic,cut.engine)
segfreq<-freq(seg)
segsort<-sort(table(seg),decreasing = T)[1:15]
seg.mc<-segfreq[segfreq[,2]>=10,]


#hair dryer
text.body.dryer<-gsub("[\n.* ]"," ",tolower(hair_dryer$review_body))
cut.engine<-worker(type="mix",stop_word = 'stopwords.txt')
new_user_word(cut.engine,mydic)
seg2<-segment(text.body.dryer,cut.engine)
segfreq2<-freq(seg2)
segsort2<-sort(table(seg2),decreasing = T)[1:15]
seg.dry<-segfreq2[segfreq2[,2]>=10,]


#pacifier
text.body.pacifier<-gsub("[\n.* ]"," ",tolower(pacifier$review_body))
cut.engine<-worker(type="mix",stop_word = 'stopwords.txt')
new_user_word(cut.engine,mydic)
seg3<-segment(text.body.pacifier,cut.engine)
segfreq3<-freq(seg3)
segsort3<-sort(table(seg3),decreasing = T)[1:15]
seg.pacifier<-segfreq3[segfreq3[,2]>=10,]

#microwave
tf_idf_mc<-segfreq$freq/sum(c(segfreq$freq,segfreq2$freq,segfreq3$freq))
segfreq$tf_idf<-tf_idf_mc
summary(segfreq$tf_idf)

tf.idf.mc.table<-c()
for(i in 1:10){
  tfidf.mc<-segfreq[which(segfreq$tf_idf==sort(segfreq$tf_idf,decreasing = T)[i]),]
  tf.idf.mc.table<-rbind(tfidf.mc,tf.idf.mc.table)
}
head(tf.idf.mc.table)

#hair dryer
tf_idf_dry<-segfreq2$freq/sum(c(segfreq$freq,segfreq2$freq,segfreq3$freq))
segfreq2$tf_idf<-tf_idf_dry
summary(segfreq2$tf_idf)

tf.idf.dry.table<-c()
for(i in 1:10){
  tfidf.dry<-segfreq2[which(segfreq2$tf_idf==sort(segfreq2$tf_idf,decreasing = T)[i]),]
  tf.idf.dry.table<-rbind(tfidf.dry,tf.idf.dry.table)
}
tf.idf.dry.table

#pacifier
tf_idf_pac<-segfreq3$freq/sum(c(segfreq$freq,segfreq2$freq,segfreq3$freq))
segfreq3$tf_idf<-tf_idf_pac
summary(segfreq3$tf_idf)

tf.idf.pac.table<-c()
for(i in 1:10){
  tfidf.pac<-segfreq3[which(segfreq3$tf_idf==sort(segfreq3$tf_idf,decreasing = T)[i]),]
  tf.idf.pac.table<-rbind(tfidf.pac,tf.idf.pac.table)
}
tf.idf.pac.table

highfreq<-data.frame(t(t(segsort)))

colnames(highfreq)
ggplot(highfreq,aes(x=seg,y=Freq,fill=seg))+geom_bar(stat="identity",position="dodge")+coord_flip()

ggplot(tf.idf.mc.table,aes(x=char,y=tf_idf,fill=tf_idf))+geom_bar(stat="identity",position="dodge")+theme_bw()+
  coord_flip()+theme(legend.position="none")+xlab("Words")+ylab("Tf-idf")
ggplot(tf.idf.dry.table,aes(x=char,y=tf_idf,fill=tf_idf))+geom_bar(stat="identity",position="dodge")+theme_bw()+
  coord_flip()+theme(legend.position="none")+xlab("Words")+ylab("Tf-idf")





###emotion judgement
head(seg.mc)
seg.mc$word=seg.mc$char

contributions <- seg.mc %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(word) %>%
  summarize(occurences = n(),
            contribution = sum(score))
contributions$word

contributions %>%
  top_n(25, abs(contribution)) %>%
  mutate(word = reorder(word, contribution)) %>%
  ggplot(aes(word, contribution, fill = contribution > 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip()

##################---------------
seg.mic<-sapply(text.body.mic,segment,cut.engine)
score.mic.body <- getEmotionalType(seg.mic, pos, neg)
score.mic.body
mic<-cbind(cor.data,score.mic.body)
head(mic)
#Random forest----------
names(mic)
forest.fit<-randomForest(rating_level~.-pos.weight-neg.weight,mic)
pred<-forest.fit$predicted
forest.perf<-table(mic$rating_level,pred,dnn=c("Actual","Predicted"))
sum(diag(prop.table(forest.perf)))###Accuracy
head(mic)
cor(data.frame(lapply(mic,as.numeric)))
head(cor.data)
#Decison tree--------
library(rpart)
set.seed(2019)
dtree<-rpart(rating_level~.,data=cor.data,method="class",
                   parms=list(split="information"))
dtree$cptable
plotcp(dtree)#复杂度参数与交叉验证误差。虚线是基于一个标准差准则得到的上限1.054054+0.1123214
summary(dtree)

#剪枝
dtree.pruned<-prune(dtree,cp=.0021)


library(rpart.plot) #用剪枝后的传统决策树预测方向。从树的顶端开始。
prp(dtree.pruned, type=2,extra=104,
    fallen.leaves = TRUE,main="Decision Tree",split.font=2,split.cex=2,space=2,under.font=2,under.cex=2,legend.cex=2)

#对训练集外样本单元分类
dtree.pred<-predict(dtree.pruned,mic$rating_level,type = "class")
dtree.perf<-table(cor.data$rating_level,dtree.pred,dnn = c("Actual","Predicted"))
dtree.perf
sum(diag(prop.table(dtree.perf)))


#2.条件推断树------------------
library(party)
fit.ctree<-ctree(rating_level~.,data=cor.data)
plot(fit.ctree,main="Conditional Inference Tree")

library(partykit)
ctree.pred<-predict(fit.ctree,mic$rating_level,type="response")
ctree.perf<-table(cor.data$rating_level,ctree.pred,dnn=c("Actual","Predicted"))
ctree.perf
sum(diag(prop.table(ctree.perf)))
(294+633)/((294+108)+(633+34))


#dryer Random forest----------
names(hair_dryer)
hair_dryer$review_score<-hair_dryer$score$total
hair_dryer$rating_level<-factor(hair_dryer$star_rating)



forest.fit.dry<-randomForest(rating_level~review_score+verified_purchase+helpful_votes,data=hair_dryer)
pred.dryer<-forest.fit$predicted
forest.perf<-table(mic$rating_level,pred,dnn=c("Actual","Predicted"))
sum(diag(prop.table(forest.perf)))###Accuracy
head(mic)
cor(data.frame(lapply(mic,as.numeric)))

#Decison tree--------
library(rpart)
set.seed(2019)
dtree<-rpart(rating_level~review_score+verified_purchase+helpful_votes,data=hair_dryer,method="class",
             parms=list(split="information"))
dtree$cptable
plotcp(dtree)#复杂度参数与交叉验证误差。虚线是基于一个标准差准则得到的上限1.054054+0.1123214
summary(dtree)

#剪枝
dtree.pruned<-prune(dtree,cp=.0021)


library(rpart.plot) #用剪枝后的传统决策树预测方向。从树的顶端开始。
prp(dtree.pruned, type=2,extra=104,
    fallen.leaves = TRUE,main="Decision Tree",split.font=2,split.cex=2,space=2,under.font=2,under.cex=2,legend.cex=2)

#对训练集外样本单元分类
dtree.pred<-predict(dtree.pruned,mic$rating_level,type = "class")
dtree.perf<-table(cor.data$rating_level,dtree.pred,dnn = c("Actual","Predicted"))
dtree.perf
sum(diag(prop.table(dtree.perf)))


#2.条件推断树------------------
library(party)
hair_dryer$Verified.purchase<-ifelse(hair_dryer$verified_purchase=="Y",1,0)

names(mic)
dryer.data<-select(hair_dryer,c("rating_level","review_score","Verified.purchase","helpful_votes"))
names(dryer.data)[3]<-"verified_purchase"
mic.data<-mic[,1:4]
names(mic.data)


pac.data<-select(pacifier,c("star_rating","score","verified_purchase","helpful_votes"))
pac.data$review_score<-pac.data$score$total
pac.data$rating_level<-pac.data$star_rating
names(pac.data)
pac.data<-select(pac.data,c("rating_level","review_score","verified_purchase","helpful_votes"))

pac.data$verified_purchase<-ifelse(pac.data$verified_purchase=="Y",1,0)
pac.data$rating_level<-factor(pac.data$rating_level)


fit.ctree.pac<-randomForest(rating_level~.,data=dryer.data)
plot(fit.ctree.pac,main="Conditional Inference Tree")

length(pac.data$rating_level)
library(partykit)
ctree.pred.pac<-predict(fit.ctree.pac,pac.data$rating_level,type="response")
length(ctree.pred.pac)
ctree.perf.pac<-table(ctree.pred.pac,dnn=c("Actual","Predicted"))
ctree.perf
sum(diag(prop.table(ctree.perf)))
(294+633)/((294+108)+(633+34))

#----------
total.data<-rbind(mic.data,dryer.data,pac.data)
fit.ctree.total<-ctree(rating_level~.,data=total.data)
plot(fit.ctree.total,main="Conditional Inference Tree")

nrow(total.data)
library(partykit)
SAMPLES<-sample(total.data$rating_level,1615)
length(SAMPLES)
ctree.pred.total<-predict(fit.ctree.total,SAMPLES,type="response")
length(ctree.pred.total)
length(SAMPLES)

ctree.perf.total<-table(SAMPLES,ctree.pred.total,dnn=c("Actual","Predicted"))
ctree.perf
sum(diag(prop.table(ctree.perf)))
(294+633)/((294+108)+(633+34))

forest.fit.total<-randomForest(rating_level~.,total.data)
pred<-forest.fit$predicted
forest.perf<-table(mic$rating_level,pred,dnn=c("Actual","Predicted"))
sum(diag(prop.table(forest.perf)))###Accuracy
names(total.data)
#write.csv(total.data,"total.data.csv")
ggplot(dryer.data,aes(x=rating_level,y=review_score,col=rating_level))+geom_jitter()+theme_minimal()+
  theme(legend.position="none")+ylab("review  score")+xlab("rating  level")
ggplot(pac.data,aes(x=rating_level,y=review_score,fill=rating_level))+geom_violin()+theme_minimal()+
  theme(legend.position="none")+ylab("review  score")+xlab("rating  level")

library(ggthemes)
library(ggplot2)
theme_set(theme_minimal())  # from ggthemes

# plot
g <- ggplot(pac.data, aes(x=rating_level,y=review_score))
g + geom_tufteboxplot(stat = "fivenumber",
                       position = "dodge", outlier.colour = "black", outlier.shape = 59,
                       outlier.size = 205, outlier.stroke = 10, voffset = 0.21,
                       hoffset = 0.5, na.rm = FALSE, show.legend = NA,
                       inherit.aes = TRUE, median.type = "point", whisker.type = "line")+
  ylab("review  score")+xlab("rating  level")

