#Load the packages
library(tidyverse)
library(data.table)
library(readxl)
library(sentimentr)
library(wordcloud)
library(ggthemes)
library(tm)
library(stopwords)
library(packHV)

#load the data

knhs <- as.data.table(read_excel("KNHS Learning Experience Review_Feedback (Responses).xlsx"))

table(knhs$grade)

# Overall Polarity

summary(sentiment(knhs$comments))
total_knhs_sent<-sentiment_by(knhs$comments, by=knhs$id)
total_knhs_sent|>summarize(polarity=mean(ave_sentiment, na.rm=TRUE), words=sum(word_count), sd=sd(ave_sentiment, na.rm = TRUE))

## Visualize the distribution of polarity scores
hist_boxplot(total_knhs_sent$ave_sentiment, freq = F, 
             density = T,
             col="#bada55", breaks=18, 
             xlim=c(-1, 2.3), 
             main = " ")


## polarity per grade level
knhs_per_grade<-sentiment_by(knhs$comments, by=knhs$grade)
knhs_per_grade


# tagging of sentiment
total_knhs_sent<-total_knhs_sent%>%mutate(sentiment=case_when(ave_sentiment==0~"neutral",
                                                              ave_sentiment>0~"positive",
                                                              ave_sentiment<0~"negative"))
## Visualize the positive, negative and netural
total_knhs_sent%>%ggplot(aes(fct_rev(fct_infreq(sentiment)), fill=sentiment))+
  geom_bar()+
  coord_flip()+
  stat_count(geom = "text", colour = "black", size = 6,
             aes(label = ..count..), position=position_nudge(y=25))+
  theme_gdocs()

knhs<-knhs%>%
  mutate(polarity=total_knhs_sent$ave_sentiment)


# word cloud for Original words

knhs_pos<-subset(knhs$comments_fil, knhs$polarity>0)
knhs_neg<-subset(knhs$comments_fil, knhs$polarity<0)


# Paste and collapse the positive comments
pos_words <- paste(knhs_pos, collapse = " ")

# Paste and collapse the negative comments
neg_words <- paste(knhs_neg, collapse = " ")

# Organize and Vcorpus
all_corpus <- c(pos_words, neg_words) %>% 
  VectorSource() %>% 
  VCorpus()


# Define a the corpus cleaning function
clean_corpus <- function(corpus) {
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removeWords, words = c(stopwords("en"),stopwords("tl", source = "stopwords-iso")))
  corpus <- tm_map(corpus, stripWhitespace)
  return(corpus)
}


# Clean the corpus

all_corpus<-clean_corpus(all_corpus)
all_tdm<-TermDocumentMatrix(
  all_corpus,control=list(weighting = weightTf
))

# Column names

all_tdm_m <- as.matrix(all_tdm)
colnames(all_tdm_m) <- c("positive", "negative")


# Comparison cloud
comparison.cloud(
  all_tdm_m,
  random.order = FALSE,
  max.words = 30,title.bg.colors = "white",title.size = 3,
  colors = c("navyblue", "red")
)


# Top pos words
order_by_pos <- order(all_tdm_m[, 1], decreasing = T)

# Review top 10 pos words

all_tdm_m[order_by_pos, ] %>% head(n=10)

# Top neg words
order_by_neg <- order(all_tdm_m[,2], decreasing = T)

# Review top 10 neg words
all_tdm_m[order_by_neg, ] %>% head(n=10)


# Emotional Intent
emotion_nrc<-get_nrc_sentiment(knhs$comments)%>%select(1:8)
total_emotion<-emotion_nrc%>%pivot_longer(cols = 1:8, names_to = "emotion", values_to = "value")%>%
  group_by(emotion)%>%
  summarize(total=sum(value))

total_emotion<-total_emotion|>mutate(prop=prop.table(total))

total_emotion
ggplot(total_emotion, aes(fct_reorder(emotion, -prop), prop, fill=emotion, 
                          label = scales::percent(prop)))+
  geom_text(position = position_dodge(width = 0.9),    # move to center of bars
          vjust = -0.4,    # nudge above top of bar
            size = 6) + 
  scale_y_continuous(labels = scales::percent)+
  geom_col()+
  theme(panel.background = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title = element_blank(), 
        axis.line =element_line(color="grey"), 
        legend.position = "none", 
        axis.text = element_text(size=18, color = "black"),
        axis.text.y = element_blank())

