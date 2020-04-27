setwd("C:\\Users\\19406\\Desktop\\美赛\\2020美赛\\Problem_C_Data")
library(readxl)#read data
library(jiebaR)#Word segmentation and other operations
library(plyr)#Text processing
library(randomForest)#randomforest
library(readr)
library(wordcloud2)

microwave<-read_xlsx("microwave.xlsx")
hair_dryer<-read_xlsx("hair_dryer.xlsx")
pacifier<-read_xlsx("pacifier.xlsx")

sum(is.na(microwave))#0
sum(is.na(hair_dryer))#8
sum(is.na(pacifier))#93
hair_dryer<-na.omit(hair_dryer)
pacifier<-na.omit(pacifier)

head(microwave)
summary(microwave)
str(microwave)

##microwave review_headline
text.mic<-microwave$review_headline
text.mic<-tolower(text.mic)
head(microwave$review_body)
#begin segment
engine<-worker(type="mix",stop_word = 'stopwords.txt')

text3<-segment(text.mic,engine)
print(text3)
text4<-freq(text3)# frequence of the word
text5<-text4[text4[,2]>=10,]
nrow(text5)
print(text5)
#write.csv(text5,"freq.microwave.csv")

##hair dryer review_headline
text.dry<-hair_dryer$review_headline
text.dry<-tolower(text.dry)
head(hair_dryer$review_body)
#begin segment
engine<-worker(type="mix",stop_word = 'stopwords.txt')
text3.dry<-segment(text.dry,engine)
print(text3.dry)
text4.dry<-freq(text3.dry)# frequence of the word
text5.dry<-text4.dry[text4.dry[,2]>=10,]
nrow(text5.dry)
print(text5.dry)
#write.csv(text5.dry,"freq.hair_dryer.csv")

##pacifier review_headline
text.pacifier<-pacifier$review_headline
text.pacifier<-tolower(text.pacifier)
text.pacifier=gsub("\\.","",text.pacifier)
#begin segment
engine<-worker(type="mix",stop_word = 'stopwords.txt')

text3.pacifier<-segment(text.pacifier,engine)
print(text3.pacifier)
text4.pacifier<-freq(text3.pacifier)# frequence of the word
text5.pacifier<-text4.pacifier[text4.pacifier[,2]>=10,]
nrow(text5.pacifier)
print(text5.pacifier)
#write.csv(text5.pacifier,"freq.pacifier.csv")



# Create my own lexicon of positive and negative words used in reviews

pos = rbind(
  c('enthusiastic', 'positive'),
  
  c('love', 'positive'),
  
  c('amazing', 'positive'),
  
  c('good', 'positive'),
  
  c('great', 'positive'),
  
  c('best', 'positive'),
  
  c('wonderful', 'positive'),
  
  c('nice', 'positive'),
  
  c('cool', 'positive'),
  
  c('perfect', 'positive'),
  
  c('happy', 'positive'),
  
  c('well', 'positive'),
  
  c('pretty', 'positive'),
  
  c('excellent', 'positive'),
  
  c('easy', 'positive'),
  
  c('fine', 'positive'),

  c('professional', 'positive'), 
  
  c('powerful', 'positive'),
  
  c('well', 'positive'),
  
  c('awesome', 'positive'),

  c('cute', 'positive'),
  
  c('loves', 'positive'),
  
  c('likes', 'positive'),
  
  c('like', 'positive'),

  c('loved', 'positive'),
  
  c('recommend', 'positive'),
  
  c('five stars', 'positive'),

  c('four stars', 'positive'),
  
  c("adequate", 'positive'),
  
  c("agreed" , 'positive'),
  
  c("allow", 'positive'),
  
  c("beautiful", 'positive'),
  
  c("better", 'positive'),
  
  c("big", 'positive'),
  
  c("clean", 'positive'),
  
  c("clear", 'positive'),
  
  c("ease", 'positive'),
  
  c("easy", 'positive'),
  
  c("fantastic", 'positive'),
  
  c("fit", 'positive'),
  
  c("free", 'positive'),
  
  c("helpful", 'positive'),
  
  c("important", 'positive'),
  
  c("kind" , 'positive'),
  
  c("luckily", 'positive')
)                    
           



neg = rbind(          
  c("lost", 'negative'), 
  
  c("noisy", 'negative'),
  
  c('not buy', 'negative'),
  
  c("broke", 'negative'),
  
  c("confusing", 'negative'),
  
  c("damage", 'negative'),
  
  c("difficult", 'negative'),
  
  c("disappointing", 'negative'),
  
  c("drop", 'negative'),
  
  c("error", 'negative'),
  
  c("fail", 'negative'),
  
  c("failed", 'negative'),
  
  c("failure", 'negative'),
  
  c("falling", 'negative'),
  
  c("limited", 'negative'),
  
  c("lack", 'negative'),
  
  c("hard", 'negative'),
  
  c("annoying", 'negative'),
  
  c("alone", 'negative'),
  
  c('disappointed', 'negative'),
  
  c('hate', 'negative'),
  
  c('horrible', 'negative'),
  
  c('bad', 'negative'),
  
  c('disgust', 'negative'),
  
  c('poor', 'negative'),
  
  c('worse', 'negative'),
  
  c('died', 'negative'),
  
  c('broken', 'negative'),
  
  c('junk', 'negative'),
  
  c('terrible', 'negative'),
  
  c('worst', 'negative'),
  
  c('problem', 'negative'),
  
  c('dead', 'negative'),
  
  c('stopped', 'negative'),
  
  c('one star', 'positive'),
  
  c('two stars', 'positive')
  
)








#Merge positive and negative words and add positive and negative words to a custom lexicon
engine=worker()
mydic = rbind(pos, neg)
new_user_word(engine,mydic)



#Cut words for each review
segwords.mic<-sapply(text.mic,segment,engine)

segwords.dry<-sapply(text.dry,segment,engine)

segwords.pacifier<-sapply(text.pacifier,segment,engine)


#Custom emotion type score function


fun <- function( x, y) x%in% y

getEmotionalType <- function( x,pwords,nwords){
  
  pos.weight = sapply(llply( x,fun,pwords),sum)
  
  neg.weight = sapply(llply( x,fun,nwords),sum)
  
  total = pos.weight - neg.weight
  
  return(data.frame( pos.weight, neg.weight, total))
  
}
score.mic <- getEmotionalType(segwords.mic, pos, neg)
score.dry <- getEmotionalType(segwords.dry, pos, neg)
score.pac <- getEmotionalType(segwords.pacifier, pos, neg)

microwave$score<-score.mic
hair_dryer$score<-score.dry
pacifier$score<-score.pac
head(microwave$score)

negative<-c(sum(microwave$score$total<0),sum(hair_dryer$score$total<0),sum(pacifier$score$total<0))#negative
neutral<-c(sum(microwave$score$total==0),sum(hair_dryer$score$total==0),sum(pacifier$score$total==0))#neutrality
positive<-c(sum(microwave$score$total>0),sum(hair_dryer$score$total>0),sum(pacifier$score$total>0))#positive
attitude<-c(rep("Negative",3),rep("Neutral",3),rep("Positive",3))
review.rate<-data.frame(Product,negative,neutral,positive)
count<-c(negative,neutral,positive)
Review.Rate<-data.frame(cbind(Product,attitude,count))
Review.Rate$count<-c(negative,neutral,positive)
library(ggplot2)
ggplot(Review.Rate,aes(Product,count,fill=attitude))+geom_bar(stat="identity",position="stack")+
  labs(fill="Attitude")+theme(panel.grid.major =element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(),axis.line = element_line(colour = "white"))


####(a)####
summary(microwave$star_rating)
barplot(table(microwave$star_rating))
table(pacifier$star_rating)

table(hair_dryer$star_rating)


summary(pacifier$star_rating)
sum(is.na(pacifier))
sum(is.na(hair_dryer))

Product<-c("microwave","hair_dryer","pacifier")
Star.1<-c(sum(microwave$star_rating==1),sum(hair_dryer$star_rating==1),sum(na.omit(pacifier$star_rating==1)))
Star.2<-c(sum(microwave$star_rating==2),sum(hair_dryer$star_rating==2),sum(na.omit(pacifier$star_rating==2)))
Star.3<-c(sum(microwave$star_rating==3),sum(hair_dryer$star_rating==3),sum(na.omit(pacifier$star_rating==3)))
Star.4<-c(sum(microwave$star_rating==4),sum(hair_dryer$star_rating==4),sum(na.omit(pacifier$star_rating==4)))
Star.5<-c(sum(microwave$star_rating==5),sum(hair_dryer$star_rating==5),sum(na.omit(pacifier$star_rating==5)))
Type<-c(rep("★",3),rep("★★",3),rep("★★★",3),rep("★★★★",3),rep("★★★★★",3))
Count<-c(Star.1,Star.2,Star.3,Star.4,Star.5)
Star.rate<-data.frame(cbind(Product,Type,Count))
Star.Rate<-data.frame(cbind(Product,Star.1,Star.2,Star.3,Star.4,Star.5))
Star.Rate
my.star.rate<-data.frame(Product,Type,Count=c(Star.1,Star.2,Star.3,Star.4,Star.5))
library(ggplot2)
ggplot(my.star.rate,aes(Product,Count,fill=Type))+geom_bar(stat="identity",position="dodge")+theme_minimal()+labs(fill="Star rate")
ggplot(my.star.rate,aes(Product,Count,fill=Type))+geom_bar(stat="identity",position="dodge")+theme_minimal()+labs(fill="Star rate")+coord_flip()
ggplot(my.star.rate,aes(Product,Count,fill=Type))+geom_bar(stat="identity",position="fill")+theme_bw()+labs(fill="Star rate")+ylab("Proportion")+coord_flip()







########(b)#########
library(dplyr)

#microwave
star_date<-microwave%>%
  group_by(review_date)%>%
  summarise(Mean=mean(star_rating))
star_date
library(xts)
date_star_line<-xts(star_date$Mean,as.Date(star_date$review_date,"%m/%d/%Y"))
library(lubridate)
plot(date_star_line)

timeline_star<-data.frame(Year=year(date_star_line),Month=month(date_star_line),Day=day(date_star_line),Value=as.numeric(date_star_line[,1]))
timeline_star

year_mean<-timeline_star%>%
  group_by(Year)%>%
  summarise(Mean_star=mean(Value))
plot(ts(year_mean$Mean_star,start = 2004,frequency = 1),ylab="Mean star of per year",xlab="Timeline of microwave")
points(ts(year_mean$Mean_star,start = 2004,frequency = 1),pch=5,cex=1.3,col=2)

month_mean<-timeline_star%>%
  group_by(Year,Month)%>%
  summarise(Mean_star=mean(Value))
View(month_mean)
plot(ts(month_mean$Mean_star),ylab="Mean star of per month",xlab="Timeline of microwave")

library(forecast)
plot(ts(month_mean$Mean_star),ylab="Mean star of per month",xlab="Timeline of microwave",ylim=c(0.8,5.65))
mic.model<-auto.arima(month_mean$Mean_star)#MA(1)
mic.fore<-forecast(mic.model,h=5)
lines(mic.fore$fitted,col=2,lwd=2)
L1<-mic.fore$fitted-1.96*sqrt(mic.model$sigma2)
U1<-mic.fore$fitted+1.96*sqrt(mic.model$sigma2)
lines(L1,col=4,lty=2)
lines(U1,col=4,lty=2)

#hair dryer
star_date2<-hair_dryer%>%
  group_by(review_date)%>%
  summarise(Mean=mean(star_rating))
star_date2
date_star_line2<-xts(star_date2$Mean,as.Date(star_date2$review_date,"%m/%d/%Y"))
plot(date_star_line2)

timeline_star2<-data.frame(Year=year(date_star_line2),Month=month(date_star_line2),Day=day(date_star_line2),Value=as.numeric(date_star_line2[,1]))
timeline_star2

year_mean2<-timeline_star2%>%
  group_by(Year)%>%
  summarise(Mean_star=mean(Value))
plot(ts(year_mean2$Mean_star,start = 2002,frequency = 1),ylab="Mean star of per year",xlab="Timeline of hair dryer")
points(ts(year_mean2$Mean_star,start = 2002,frequency = 1),pch=5,cex=1.3,col=2)

month_mean2<-timeline_star2%>%
  group_by(Year,Month)%>%
  summarise(Mean_star=mean(Value))
month_mean2

plot(ts(month_mean2$Mean_star),ylab="Mean star of per month",xlab="Timeline of hair dryer")

plot(ts(month_mean2$Mean_star),ylab="Mean star of per month",xlab="Timeline of hair dryer",ylim=c(0.4,6.16))
dry.model<-auto.arima(month_mean2$Mean_star)#ARI(2,1)
dry.fore<-forecast(dry.model,h=5)
lines(dry.fore$fitted,col=2,lwd=2)
L2<-dry.fore$fitted-1.96*sqrt(dry.model$sigma2)
U2<-dry.fore$fitted+1.96*sqrt(dry.model$sigma2)
lines(L2,col=4,lty=2)
lines(U2,col=4,lty=2)


#pacifier
star_date3<-pacifier%>%
  group_by(review_date)%>%
  summarise(Mean=mean(star_rating))
star_date3
date_star_line3<-xts(star_date3$Mean,as.Date(star_date3$review_date,"%m/%d/%Y"))
plot(date_star_line3)

timeline_star3<-data.frame(Year=year(date_star_line3),Month=month(date_star_line3),Day=day(date_star_line3),Value=as.numeric(date_star_line3[,1]))
timeline_star3

year_mean3<-timeline_star3%>%
  group_by(Year)%>%
  summarise(Mean_star=mean(Value))
plot(ts(year_mean3$Mean_star,start = 2003,frequency = 1),ylab="Mean star of per year",xlab="Timeline of pacifier")
points(ts(year_mean3$Mean_star,start = 2003,frequency = 1),pch=5,cex=1.3,col=2)

month_mean3<-timeline_star3%>%
  group_by(Year,Month)%>%
  summarise(Mean_star=mean(Value))
month_mean3


plot(ts(month_mean3$Mean_star),ylab="Mean star of per month",xlab="Timeline of pacifier",ylim=c(0.5,6))
pac.model<-auto.arima(month_mean3$Mean_star)#ARI(2,1)
pac.fore<-forecast(pac.model,h=5)
lines(pac.fore$fitted,col=2,lwd=2)
L3<-pac.fore$fitted-1.96*sqrt(pac.model$sigma2)
U3<-pac.fore$fitted+1.96*sqrt(pac.model$sigma2)
lines(L2,col=4,lty=2)
lines(U2,col=4,lty=2)
min(L3)
max(U3)


######(e)#######
verified_purchase<-ifelse(microwave$verified_purchase=="Y",1,0)
review_score<-microwave$score$total
rating_level<-microwave$star_rating
helpful_votes<-microwave$helpful_votes
cor.data<-data.frame(rating_level,review_score,verified_purchase,helpful_votes)
cor(cor.data,method = "spearman")

##Correlation coefficient significance test
library(psych)
corr.test(cor.data,method = "spearman",alpha=.05,ci=TRUE)

library(ggcorrplot)
p.dat<-cor_pmat(cor.data,method = "spearman")
ggcorrplot(cor(cor.data,method = "spearman"),p.mat=p.dat)


ggplot(cor.data,aes(rating_level,review_score,col=factor(rating_level)))+geom_jitter()+theme_minimal()+
  theme(legend.position="none")+ylab("review  score")+xlab("rating  level")

ggplot(cor.data,aes(rating_level,review_score,fill=factor(rating_level)))+geom_violin()+theme_minimal()+
  theme(legend.position="none")+ylab("review  score")+xlab("rating  level")



############a-random forest###############
head(cor.data)
cor.data$rating_level<-factor(cor.data$rating_level)


#Random forest----------
names(cor.data)
forest.fit<-randomForest(rating_level~.,cor.data)
pred<-forest.fit$predicted
forest.perf<-table(cor.data$rating_level,pred,dnn=c("Actual","Predicted"))
sum(diag(prop.table(forest.perf)))###Accuracy




##########(d)#########
plot(mic$total,type="l")
review_date<-data.frame(date=microwave$review_date,review_score=mic[,"total"],star_rate=microwave[,"star_rating"])
head(review_date)

par(mfrow=c(2,3))
for(i in 1:5) {
  plot(review_date$review_score[review_date$star_rating==i][length(review_date$review_score[review_date$star_rating==i]):1],
       type="l",xlab="",ylab="review  score")
  text(length(review_date$review_score[review_date$star_rating==i])*0.2,max(review_date$review_score[review_date$star_rating==i]),paste(rep("★",i),collapse=""))}

plot(review_date$review_score,type="l",xlab="",ylab="review  score")
text(length(review_date$review_score)*0.8,max(review_date$review_score),"General")

#--------Star one--------
n=length(review_date$review_score[review_date$star_rating==1])
STAR.1<-review_date$review_score[review_date$star_rating==1][n:1]

#5 sample
line_5<-rep(NA,5-1)

for(i in 1:n-5){
  samp5<-(STAR.1[i]+STAR.1
        [i+1]+STAR.1[i+2]+STAR.1
        [i+3]+STAR.1[i+4])/5
  line_5<-append(line_5,samp5)
}
plot(line_5,type="l",xlab="",ylab="review  score")


#10 sample
line_10<-rep(NA,10-1)
for(i in 1:n-10){
  samp10<-(STAR.1[i]+STAR.1
         [i+1]+STAR.1[i+2]+STAR.1
         [i+3]+STAR.1[i+4]+STAR.1
         [i+5]+STAR.1[i+6]+STAR.1
         [i+7]+STAR.1[i+8]+STAR.1[i+9])/10
  line_10<-append(line_10,samp10)
}
lines(line_10,type="l",xlab="",ylab="review  score",col=2,lwd=2)

#30 sample⽇
line_30<-rep(NA,30-1)
for(i in 1:n-30){
  samp30<-(STAR.1[i]+STAR.1
         [i+1]+STAR.1[i+2]+STAR.1
         [i+3]+STAR.1[i+4]+STAR.1
         [i+5]+STAR.1[i+6]+STAR.1
         [i+7]+STAR.1[i+8]+STAR.1
         [i+9]+STAR.1[i+10]+STAR.1
         [i+11]+STAR.1[i+12]+STAR.1
         [i+13]+STAR.1[i+14]+STAR.1
         [i+15]+STAR.1[i+16]+STAR.1
         [i+17]+STAR.1[i+18]+STAR.1
         [i+19]+STAR.1[i+20]+STAR.1
         [i+21]+STAR.1[i+22]+STAR.1
         [i+23]+STAR.1[i+24]+STAR.1
         [i+25]+STAR.1[i+26]+STAR.1
         [i+27]+STAR.1[i+28]+STAR.1[i+29])/30
  line_30<-append(line_30,samp30)
}
lines(line_30,type="l",xlab="",ylab="review  score",col=4,lwd=2)
abline(h=0,lty=5)
legend("topright",legend = c("5-sample moving average","10-sample moving average","30-sample moving average"),
       lwd=c(1,2,2),col=c(1,2,4),cex=0.8,title = "One star")


#--------Star two--------
n=length(review_date$review_score[review_date$star_rating==2])
STAR.2<-review_date$review_score[review_date$star_rating==2][n:1]
#5 sample
line_5<-rep(NA,5-1)

for(i in 1:n-5){
  samp5<-(STAR.2[i]+STAR.2
          [i+1]+STAR.2[i+2]+STAR.2
          [i+3]+STAR.2[i+4])/5
  line_5<-append(line_5,samp5)
}
plot(line_5,type="l",xlab="",ylab="review  score")


#10 sample
line_10<-rep(NA,10-1)
for(i in 1:n-10){
  samp10<-(STAR.2[i]+STAR.2
           [i+1]+STAR.2[i+2]+STAR.2
           [i+3]+STAR.2[i+4]+STAR.2
           [i+5]+STAR.2[i+6]+STAR.2
           [i+7]+STAR.2[i+8]+STAR.2[i+9])/10
  line_10<-append(line_10,samp10)
}
lines(line_10,type="l",xlab="",ylab="review  score",col=2,lwd=2)

#30 sample⽇
line_30<-rep(NA,30-1)
for(i in 1:n-30){
  samp30<-(STAR.2[i]+STAR.2
           [i+1]+STAR.2[i+2]+STAR.2
           [i+3]+STAR.2[i+4]+STAR.2
           [i+5]+STAR.2[i+6]+STAR.2
           [i+7]+STAR.2[i+8]+STAR.2
           [i+9]+STAR.2[i+10]+STAR.2
           [i+11]+STAR.2[i+12]+STAR.2
           [i+13]+STAR.2[i+14]+STAR.2
           [i+15]+STAR.2[i+16]+STAR.2
           [i+17]+STAR.2[i+18]+STAR.2
           [i+19]+STAR.2[i+20]+STAR.2
           [i+21]+STAR.2[i+22]+STAR.2
           [i+23]+STAR.2[i+24]+STAR.2
           [i+25]+STAR.2[i+26]+STAR.2
           [i+27]+STAR.2[i+28]+STAR.2[i+29])/30
  line_30<-append(line_30,samp30)
}
lines(line_30,type="l",xlab="",ylab="review  score",col=4,lwd=2)
abline(h=0,lty=5)
legend("topright",legend = c("5-sample moving average","10-sample moving average","30-sample moving average"),
       lwd=c(1,2,2),col=c(1,2,4),cex=0.8,title = "Two star")


#--------Star three--------
n=length(review_date$review_score[review_date$star_rating==3])
STAR.3<-review_date$review_score[review_date$star_rating==3][n:1]
#5 sample
line_5<-rep(NA,5-1)

for(i in 1:n-5){
  samp5<-(STAR.3[i]+STAR.3
          [i+1]+STAR.3[i+2]+STAR.3
          [i+3]+STAR.3[i+4])/5
  line_5<-append(line_5,samp5)
}
plot(line_5,type="l",xlab="",ylab="review  score")


#10 sample
line_10<-rep(NA,10-1)
for(i in 1:n-10){
  samp10<-(STAR.3[i]+STAR.3
           [i+1]+STAR.3[i+2]+STAR.3
           [i+3]+STAR.3[i+4]+STAR.3
           [i+5]+STAR.3[i+6]+STAR.3
           [i+7]+STAR.3[i+8]+STAR.3[i+9])/10
  line_10<-append(line_10,samp10)
}
lines(line_10,type="l",xlab="",ylab="review  score",col=2,lwd=2)

#30 sample⽇
line_30<-rep(NA,30-1)
for(i in 1:n-30){
  samp30<-(STAR.3[i]+STAR.3
           [i+1]+STAR.3[i+2]+STAR.3
           [i+3]+STAR.3[i+4]+STAR.3
           [i+5]+STAR.3[i+6]+STAR.3
           [i+7]+STAR.3[i+8]+STAR.3
           [i+9]+STAR.3[i+10]+STAR.3
           [i+11]+STAR.3[i+12]+STAR.3
           [i+13]+STAR.3[i+14]+STAR.3
           [i+15]+STAR.3[i+16]+STAR.3
           [i+17]+STAR.3[i+18]+STAR.3
           [i+19]+STAR.3[i+20]+STAR.3
           [i+21]+STAR.3[i+22]+STAR.3
           [i+23]+STAR.3[i+24]+STAR.3
           [i+25]+STAR.3[i+26]+STAR.3
           [i+27]+STAR.3[i+28]+STAR.3[i+29])/30
  line_30<-append(line_30,samp30)
}
lines(line_30,type="l",xlab="",ylab="review  score",col=4,lwd=2)
abline(h=0,lty=5)
legend("topleft",legend = c("5-sample moving average","10-sample moving average","30-sample moving average"),
       lwd=c(1,2,2),col=c(1,2,4),cex=0.8,title = "Three star")

#--------Star four--------
n=length(review_date$review_score[review_date$star_rating==4])
STAR.4<-review_date$review_score[review_date$star_rating==4][n:1]
#5 sample
line_5<-rep(NA,5-1)

for(i in 1:n-5){
  samp5<-(STAR.4[i]+STAR.4
          [i+1]+STAR.4[i+2]+STAR.4
          [i+3]+STAR.4[i+4])/5
  line_5<-append(line_5,samp5)
}
plot(line_5,type="l",xlab="",ylab="review  score")


#10 sample
line_10<-rep(NA,10-1)
for(i in 1:n-10){
  samp10<-(STAR.4[i]+STAR.4
           [i+1]+STAR.4[i+2]+STAR.4
           [i+3]+STAR.4[i+4]+STAR.4
           [i+5]+STAR.4[i+6]+STAR.4
           [i+7]+STAR.4[i+8]+STAR.4[i+9])/10
  line_10<-append(line_10,samp10)
}
lines(line_10,type="l",xlab="",ylab="review  score",col=2,lwd=2)

#30 sample⽇
line_30<-rep(NA,30-1)
for(i in 1:n-30){
  samp30<-(STAR.4[i]+STAR.4
           [i+1]+STAR.4[i+2]+STAR.4
           [i+3]+STAR.4[i+4]+STAR.4
           [i+5]+STAR.4[i+6]+STAR.4
           [i+7]+STAR.4[i+8]+STAR.4
           [i+9]+STAR.4[i+10]+STAR.4
           [i+11]+STAR.4[i+12]+STAR.4
           [i+13]+STAR.4[i+14]+STAR.4
           [i+15]+STAR.4[i+16]+STAR.4
           [i+17]+STAR.4[i+18]+STAR.4
           [i+19]+STAR.4[i+20]+STAR.4
           [i+21]+STAR.4[i+22]+STAR.4
           [i+23]+STAR.4[i+24]+STAR.4
           [i+25]+STAR.4[i+26]+STAR.4
           [i+27]+STAR.4[i+28]+STAR.4[i+29])/30
  line_30<-append(line_30,samp30)
}
lines(line_30,type="l",xlab="",ylab="review  score",col=4,lwd=2)
abline(h=0,lty=5)
legend("topleft",legend = c("5-sample moving average","10-sample moving average","30-sample moving average"),
       lwd=c(1,2,2),col=c(1,2,4),cex=0.8,title = "Four star")

#--------Star five--------
n=length(review_date$review_score[review_date$star_rating==5])
STAR.5<-review_date$review_score[review_date$star_rating==5][n:1]
#5 sample
line_5<-rep(NA,5-1)

for(i in 1:n-5){
  samp5<-(STAR.5[i]+STAR.5
          [i+1]+STAR.5[i+2]+STAR.5
          [i+3]+STAR.5[i+4])/5
  line_5<-append(line_5,samp5)
}
plot(line_5,type="l",xlab="",ylab="review  score")


#10 sample
line_10<-rep(NA,10-1)
for(i in 1:n-10){
  samp10<-(STAR.5[i]+STAR.5
           [i+1]+STAR.5[i+2]+STAR.5
           [i+3]+STAR.5[i+4]+STAR.5
           [i+5]+STAR.5[i+6]+STAR.5
           [i+7]+STAR.5[i+8]+STAR.5[i+9])/10
  line_10<-append(line_10,samp10)
}
lines(line_10,type="l",xlab="",ylab="review  score",col=2,lwd=2)

#30 sample⽇
line_30<-rep(NA,30-1)
for(i in 1:n-30){
  samp30<-(STAR.5[i]+STAR.5
           [i+1]+STAR.5[i+2]+STAR.5
           [i+3]+STAR.5[i+4]+STAR.5
           [i+5]+STAR.5[i+6]+STAR.5
           [i+7]+STAR.5[i+8]+STAR.5
           [i+9]+STAR.5[i+10]+STAR.5
           [i+11]+STAR.5[i+12]+STAR.5
           [i+13]+STAR.5[i+14]+STAR.5
           [i+15]+STAR.5[i+16]+STAR.5
           [i+17]+STAR.5[i+18]+STAR.5
           [i+19]+STAR.5[i+20]+STAR.5
           [i+21]+STAR.5[i+22]+STAR.5
           [i+23]+STAR.5[i+24]+STAR.5
           [i+25]+STAR.5[i+26]+STAR.5
           [i+27]+STAR.5[i+28]+STAR.5[i+29])/30
  line_30<-append(line_30,samp30)
}
lines(line_30,type="l",xlab="",ylab="review  score",col=4,lwd=2)
abline(h=0,lty=5)
legend("topright",legend = c("5-sample moving average","10-sample moving average","30-sample moving average"),
       lwd=c(1,2,2),col=c(1,2,4),cex=0.8,title = "Five star")


#--hair dryer
n=nrow(pacifier[pacifier$star_rating==1,c("review_date","star_rating","score")])
STAR.5.dry<-pacifier[pacifier$star_rating==1,c("score")][n:1,]
STAR.5.dry<-STAR.5.dry$score$total
#5 sample
line_5<-rep(NA,5-1)

for(i in 1:n-5){
  samp5<-(STAR.5.dry[i]+STAR.5.dry
          [i+1]+STAR.5.dry[i+2]+STAR.5.dry
          [i+3]+STAR.5.dry[i+4])/5
  line_5<-append(line_5,samp5)
}
plot(line_5,type="l",xlab="",ylab="review  score")


#10 sample
line_10<-rep(NA,10-1)
for(i in 1:n-10){
  samp10<-(STAR.5.dry[i]+STAR.5.dry
           [i+1]+STAR.5.dry[i+2]+STAR.5.dry
           [i+3]+STAR.5.dry[i+4]+STAR.5.dry
           [i+5]+STAR.5.dry[i+6]+STAR.5.dry
           [i+7]+STAR.5.dry[i+8]+STAR.5.dry[i+9])/10
  line_10<-append(line_10,samp10)
}
lines(line_10,type="l",xlab="",ylab="review  score",col=2,lwd=2)

#30 sample⽇
line_30<-rep(NA,30-1)
for(i in 1:n-30){
  samp30<-(STAR.5.dry[i]+STAR.5.dry
           [i+1]+STAR.5.dry[i+2]+STAR.5.dry
           [i+3]+STAR.5.dry[i+4]+STAR.5.dry
           [i+5]+STAR.5.dry[i+6]+STAR.5.dry
           [i+7]+STAR.5.dry[i+8]+STAR.5.dry
           [i+9]+STAR.5.dry[i+10]+STAR.5.dry
           [i+11]+STAR.5.dry[i+12]+STAR.5.dry
           [i+13]+STAR.5.dry[i+14]+STAR.5.dry
           [i+15]+STAR.5.dry[i+16]+STAR.5.dry
           [i+17]+STAR.5.dry[i+18]+STAR.5.dry
           [i+19]+STAR.5.dry[i+20]+STAR.5.dry
           [i+21]+STAR.5.dry[i+22]+STAR.5.dry
           [i+23]+STAR.5.dry[i+24]+STAR.5.dry
           [i+25]+STAR.5.dry[i+26]+STAR.5.dry
           [i+27]+STAR.5.dry[i+28]+STAR.5.dry[i+29])/30
  line_30<-append(line_30,samp30)
}
lines(line_30,type="l",xlab="",ylab="review  score",col=4,lwd=2)
abline(h=0,lty=5)
legend("topright",legend = c("5-sample moving average","10-sample moving average","30-sample moving average"),
       lwd=c(1,2,2),col=c(1,2,4),cex=0.8,title = "One star")
mean(na.omit(line_30))
     
######(c)######
#Brand differentiation
brand.mic<-substr(tolower(microwave$product_title),1,12)

brand.mic<-ifelse(brand.mic=="amana 1.5 cu","amana",brand.mic)
brand.mic<-ifelse(brand.mic=="haier hmv163","haier",brand.mic)
brand.mic<-ifelse(brand.mic=="frigidaire f","frigidaire",brand.mic)
brand.mic<-ifelse(brand.mic=="lg  over the"|
                    brand.mic=="lg over-the-","amana",brand.mic)
brand.mic<-ifelse(brand.mic=="danby 0.7 cu"|brand.mic=="danby dwc283","danby",brand.mic)
brand.mic<-ifelse(brand.mic=="whirlpool wm"|
                    brand.mic=="whirlpool st"|
                    brand.mic=="whirlpool gh"|
                    brand.mic=="whirlpool gm","whirlpool",brand.mic)
brand.mic<-ifelse(brand.mic=="samsung smh1"|
                    brand.mic=="samsung bask"|
                    brand.mic=="samsung coun"|
                    brand.mic=="samsung mc11","samsung",brand.mic)
brand.mic<-ifelse(brand.mic=="sharp microw"|
                    brand.mic=="sharp 1-1/2-"|
                    brand.mic=="sharp 1.1-cu"|
                    brand.mic=="sharp 950-wa"|
                    brand.mic=="sharp r-520k"|
                    brand.mic=="sharp r1874t"|
                    brand.mic=="sharp rmotda"|
                    brand.mic=="sharp kb6524","sharp",brand.mic)
brand.mic<-ifelse(brand.mic=="profile 2.2 "|
                    brand.mic=="ge profile p"|
                    brand.mic=="ge pvm9179sf","profile",brand.mic)
brand.mic<-ifelse(brand.mic=="spacemaker j"|
                    brand.mic=="ge jvm1540sm"|
                    brand.mic=="pem31dmww%2d"|
                    brand.mic=="pem31smss ge","spacemaker",brand.mic)
brand.mic<-ifelse(brand.mic=="hmv3051u 300","hmv3051u",brand.mic)  
brand.mic<-ifelse(brand.mic=="arksen porta","arksen",brand.mic)
brand.mic<-ifelse(brand.mic=="magic chef","magic",brand.mic)
brand.mic<-ifelse(brand.mic=="broan 412402"|
                    brand.mic=="broan 639 wa","magic",brand.mic)
table(brand.mic)

brand.mic<-ifelse(brand.mic=="haier"|
         brand.mic=="spacemaker"|
         brand.mic=="frigidaire"|
         brand.mic=="profile"|
         brand.mic=="amana"|
         brand.mic=="samsung"|
         brand.mic=="sharp"|
         brand.mic=="whirlpool"|
         brand.mic=="danby",brand.mic,"others")

microwave$brand<-brand.mic

barplot(sort(table(microwave$brand)),ylab="count")
ggplot(microwave,aes(brand,star_rating,col=brand))+geom_jitter()+theme_minimal()+
  theme(legend.position="none",axis.title.x.bottom = element_text(size=16),
        axis.title.y.left = element_text(size=16),
        axis.text.y = element_text(size=14),
        axis.text.x = element_text(size=14))+ylab("Rating  level")+xlab("Brand")
mic$brand<-brand.mic
ggplot(mic,aes(brand,body.score,fill=brand))+geom_point(position = "jitter", alpha = 0.5,colour="grey")+
theme_minimal()+theme(legend.position="none")+ylab("rating  level")+
geom_boxplot(fill = NA, outlier.colour = NA, colour = "black") +
  theme(axis.title.x.bottom = element_text(size=16),
        axis.title.y.left = element_text(size=16),
        axis.text.y = element_text(size=14),
        axis.text.x = element_text(size=14))+ylab("Review  Score")+xlab("Brand")

mic.brand.score<-microwave%>%
  group_by(brand)%>%
  summarise(Mean.Score=mean(star_rating))
mic.brand.score

mic.bran.review.score<-mic%>%
  group_by(brand)%>%
  summarise(Mean.Score=mean(body.score))
mic.bran.review.score

hair.dryer.brand.score<-hair_dryer%>%
  group_by(product_title)%>%
  summarise(Mean.Score=mean(star_rating))
hair.dryer.brand.score
#write.csv(hair.dryer.brand.score,"hair.dryer.brand.score.csv")


pacifier.brand.score<-pacifier%>%
  group_by(product_title)%>%
  summarise(Mean.Score=mean(star_rating))
pacifier.brand.score
#write.csv(pacifier.brand.score,"pacifier.brand.score.csv")

summary(microwave$score)
summary(hair_dryer$score)
sum(pacifier$score$total<=4)/nrow(pacifier)
class(pacifier$score$total)
