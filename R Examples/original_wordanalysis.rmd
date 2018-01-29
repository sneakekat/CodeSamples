---
title: "Word Count and Sentiment Analysis of BBT Content"
author: "Kat"
output: 
  html_document:
    keep_md: true
---
>
## In this analysis I will look the messages that have the highest and lowest Click-Through Rates to see if there are any specific words that stand out in these messages.
```{r ,message=FALSE, echo=FALSE}
# First, I loaded in the Data and took out empty rows at the bottom of the spreadsheet. (see .rmd file for code)


library(xlsx)
library(dplyr)
library(tidytext)
library(ggplot2)


bbt <- read.xlsx("BBT Content Analysis 9.22.2017.2.xlsx", sheetIndex=1)

bbt2 <- bbt[1:571,]   # subset without empty rows
bbt2$Text.Message <- iconv(bbt2$Text.Message, "UTF-8", "UTF-8")  # convert to UTF-8 to fix apostrophe encoding

#bbt2$Text.Message[5:62] # these messages are clean with coding
#5, 15, 20, 22, 23, 24, 38, 62
#nrow(bbt2)
```

```{r, echo=FALSE, message=FALSE}

##### Then, I checked for missing values in the Month column since that seemed like a good way to determine the onboarding and stop-like messages. There were 35 message in total that I omitted from the analysis.
# check for na values
na <- bbt2[is.na(bbt2$Month),]
#nrow(na)
```


```{r, echo=FALSE}

messages <- na$Text.Message


#messages
```
# MESSAGES
```{r, echo=FALSE}
##### I removed "BBT: " from all messages. I also fixed apostrophe error that occurred when reading .xlsx file
# I changed the following words so that I could group certain words together and avoid plural errors.

bbt3 <- bbt2[!is.na(bbt2$Month),]

nrow(bbt3)

bbt3$Text.Message <- gsub("BBT: ", "", bbt3$Text.Message)#subset out ignoring messages above & remove BBT: in text.message

milestoneCHK <- bbt3[grep("milestone.+", bbt3$Text.Message),]
#milestoneCHK


#milestoneCHK$Text.Message
#summary(milestoneCHK$CTR) # median is 0.175, mean is 0.17


#### BABY/BABIES/BABYS is messing up

bbt3$Text.Message <- gsub("milestones", "milestone", bbt3$Text.Message) # milestone & milestones

bbt3$Text.Message <- gsub("toddlers", "toddler", bbt3$Text.Message)  # fix toddler & toddler's to toddler

bbt3$Text.Message <- gsub("toddler's", "toddler", bbt3$Text.Message)

bbt3$Text.Message <- gsub("misbehavior", "behavior", bbt3$Text.Message) # misbehavior to behavior

bbt3$Text.Message <- gsub("behaviors", "behavior", bbt3$Text.Message) # behaviors to behavior

bbt3$Text.Message <- gsub("baby's", "baby", bbt3$Text.Message) # baby's to baby


bbt3$Text.Message <- gsub("babys", "baby", bbt3$Text.Message) # baby's to baby

bbt3$Text.Message <- gsub("babies", "baby", bbt3$Text.Message) # baby & babies

bbt3$Text.Message <- gsub("babys", "baby", bbt3$Text.Message)  # baby & baby's


bbt3$Text.Message <- gsub("toys", "toy", bbt3$Text.Message)  # toy & toys

bbt3$Text.Message <- gsub("symptoms", "symptom", bbt3$Text.Message)  # toy & toys

bbt3$Text.Message <- gsub("feelings", "feeling", bbt3$Text.Message)  # feeling & feelings

bbt3$Text.Message <- gsub("child's", "child", bbt3$Text.Message) # child & child's

bbt3$Text.Message <- gsub("childs", "child", bbt3$Text.Message) # child & child's

bbt3$Text.Message <- gsub("tips", "tips", bbt3$Text.Message) # tips & tip

bbt3$Text.Message <- gsub("children", "child", bbt3$Text.Message) # childs & childs

bbt3$Text.Message <- gsub("skills", "skill", bbt3$Text.Message) # skill & skills

bbt3$Text.Message <- gsub("crying", "cry", bbt3$Text.Message) # cry & crying

bbt3$Text.Message <- gsub("cries", "cry", bbt3$Text.Message) # cries & cry

bbt3$Text.Message <- gsub("games", "game", bbt3$Text.Message) # games & game

bbt3$Text.Message <- gsub("grows", "grow", bbt3$Text.Message) # grow & grows

bbt3$Text.Message <- gsub("fears", "fear", bbt3$Text.Message) # fear & fear

bbt3$Text.Message <- gsub("strategies", "strategy", bbt3$Text.Message) # strategy & strategies

bbt3$Text.Message <- gsub("emotional", "emotion", bbt3$Text.Message) # emotional to emotions

bbt3$Text.Message <- gsub("emotions", "emotion", bbt3$Text.Message) # emotions to emotion

bbt3$Text.Message <- gsub("likes", "like", bbt3$Text.Message) # like & likes

bbt3$Text.Message <- gsub("smells", "smell", bbt3$Text.Message) # semlls & smell

bbt3$Text.Message <- gsub("weeks", "week", bbt3$Text.Message) # weeks to week

bbt3$Text.Message <- gsub("tantrums", "trantrum", bbt3$Text.Message) # trantrums to tantrum

bbt3$Text.Message <- gsub("consequences", "consequence", bbt3$Text.Message) # consequence to consequences

bbt3$Text.Message <- gsub("techniques", "technique", bbt3$Text.Message) # techniques to technique

bbt3$Text.Message <- gsub("tummies", "tummy", bbt3$Text.Message) # tummies to tummy


babyMSG <- bbt3[grep("baby|baby's|babies",  bbt3$Text.Message),]
#babyMSG



milestoneMSG <- bbt3[grep("milestone.+", bbt3$Text.Message),]
#milestoneMSG$Text.Message
#length(milestoneMSG$Text.Message)
#summary(milestoneMSG$CTR) 


############# bbt3 is the name of the completed replacement of words!!!
```

```{r, echo=FALSE}
##### These are the Unique CTR Rates and a basic summary
#unique(bbt3$CTR)
#summary(bbt3$CTR)
```

>
### Word Count Analysis for High CTR Rates ( > 15%)
#### For this analysis I used CTR that were above the 3rd quartile which had a CTR of 15% or more to determine if there are any words that consistently show up in those messages that are getting more views.
#### The package I used to do the analysis splits up the sentences into individual words and counts up how many times they appear in all the messages. It also allows you to omit "stop" words which are the most commonly used words in the English language and therefore not relevant.

```{r, echo=FALSE, message=FALSE}
##### I used the upper quantile (CTR >= 15%) to do analysis. There were 148 messages that produced 15% or higher CTR.

bbt3High <- subset(bbt3, CTR >= 0.15)
#nrow(bbt3High)
#summary(bbt3High$CTR)
```


```{r message=FALSE, echo=FALSE}
library(dplyr)
bbt_df <- data_frame(line=nrow(bbt3High), text=bbt3High$Text.Message, CTR=bbt3High$CTR)

## use tidytext to unnest all Text messages per line,  unest words, and unjoin stop words (INCLUDES ALL)

bbt_df <- bbt_df %>% unnest_tokens(word, text) %>% anti_join(stop_words)


```

>
### Below you can see that the most used words in the most clicked messages are "child", followed by "learn", "baby, "tips, and "toddler". Below that is a word cloud of this visualization.
```{r, echo=FALSE, message=FALSE}
bbt_df %>% 
  count(word, sort=TRUE) %>% 
  filter(n>10) %>%  
  mutate(word=reorder(word, n)) %>% 
  ggplot(aes(word, n)) + 
    geom_col() + xlab(NULL) + coord_flip()+
   labs(title="Words Most Used in BBT Messages, CTR >= 15%", subtitle="Stop Words Omitted")

##### Word cloud with CTR >= 15%
library(wordcloud)
bbt_df %>%
  count(word) %>%
  with(wordcloud(word, n, max.words=100, rot.per=0))

```

>
### Word Count Analysis for Low CTR Rates ( < 7%)
#### Next I looked at the messages with the lowest CTR, and you can see that the top 5 words used in these messages  are "child", "pbs", "talk", "laundry", and "video. "Child" shows up in the messages with high CTR as well. 
```{r, echo=FALSE, message=FALSE}
bbt3Low <- subset(bbt3, CTR <= 0.07)

bbt_dfL <- data_frame(line=nrow(bbt3Low), text=bbt3Low$Text.Message, CTR=bbt3Low$CTR)

bbt_dfL <- bbt_dfL %>% unnest_tokens(word, text) %>% anti_join(stop_words)

# creating chart
bbt_dfL %>% 
  count(word, sort=TRUE) %>% 
  filter(n>10) %>% 
  mutate(word=reorder(word, n)) %>% 
  ggplot(aes(word, n)) + 
  geom_col() + xlab(NULL) + coord_flip()+
  labs(title="Words Most Used in BBT Messages, CTR <= 7%", subtitle="Stop Words Omitted")
```



```{r, echo=FALSE, eval=FALSE}

#####What happens if I keep all the stop words for CTR >= 15%,
#bbt3High sorts CTR >=15 above

bbt_dfALL <- data_frame(line=nrow(bbt3High), text=bbt3High$Text.Message, CTR=bbt3High$CTR)


bbt_dfALL <- bbt_dfALL %>% unnest_tokens(word, text)

bbt_dfALL %>% 
  count(word, sort=TRUE) %>% 
  filter(n>5) %>% 
  mutate(word=reorder(word, n)) %>% 
  ggplot(aes(word, n)) + 
    geom_col() + xlab(NULL) + coord_flip()+
   labs(title="Words Most Used in BBT Messages, CTR >= 15%", subtitle="Stop Words KEPT")

```

```{r, echo=FALSE}

#####What are Common words from CTR <= 7% and CTR >=15 %?
wordStop15H<- bbt_df$word # words from high CTR
wordStop15L <- bbt_dfL$word # words from low CTR

JointWORD_stop <- wordStop15H[wordStop15H %in% wordStop15L] # what high words are in low words?

#length(JointWORD_stop) #how many similar words
#JointWORD_stop # all similar words

SIM_df <- as.data.frame(JointWORD_stop)

# SIM_df %>%
#     count(JointWORD_stop, sort=TRUE) %>%
#    filter(n>2) %>%
#    mutate(JointWORD_stop=reorder(JointWORD_stop, n)) %>% 
#    ggplot(aes(JointWORD_stop, n)) + 
#    geom_col() + xlab(NULL) + coord_flip()+
#    labs(title="Words in CTR<7% &  CTR>15%")

```

>
### Words Unique to High Click-Through-Rates (>15%)
#### I separated out the words that are unique to messages whose CTRs are 15% or greater. I did this by comparing the words in High CTR to Low CTR and took out the words that were in both lists to leave the unique High CTR words. 
#### Below is a bar chart of unique words and a word cloud below that.
#### You can see that the most used words here are behavior, months, developmental, milestone, and intellectual. 
```{r, echo=FALSE, message=FALSE}

uniqueWORD_stop <- wordStop15H[!(wordStop15H %in% wordStop15L)]
#uniqueWORD_stop
uniqueWORD_stopdf <- as.data.frame(uniqueWORD_stop)

uniqueWORD_stopdf %>%
    count(uniqueWORD_stop, sort=TRUE) %>%
   filter(n>2) %>%
   mutate(uniqueWORD_stop=reorder(uniqueWORD_stop, n)) %>% 
   ggplot(aes(uniqueWORD_stop, n)) + 
   geom_col() + xlab(NULL) + coord_flip()+
   labs(title="Words Unique to CTR>15")

##### Word cloud with UNIQUE WORDS to CTR >= 15%
uniqueWORD_stopdf %>%
  count(uniqueWORD_stop) %>%
  with(wordcloud(uniqueWORD_stop, n, min.freq=2, max.words=50, rot.per=0))

```

>
### Sentiment Analysis of CTR >= 15%, Use Bing Sentiment 
#### This chart shows that the sentiment according to Bing for the highest CTR messages tends to be more positive than negative.
```{r, echo=FALSE, message=FALSE}

bbt_dfALL <- data_frame(line=1:nrow(bbt3), text=bbt3$Text.Message, CTR=bbt3$CTR)

# filter out CTR > = 15%
 bing15 <- bbt_dfALL %>% 
   unnest_tokens(word, text) %>%  # separate all words out
   filter(CTR>=.15) %>%          # only CTR >=.15
  inner_join(get_sentiments("bing")) %>%   # add bing sentiment
  count(word, sentiment, sort=TRUE) 

 bing15 %>%
   group_by(sentiment) %>%
   filter(n>1)%>%
   #top_n(5) %>%
   ungroup() %>%
   mutate(word=reorder(word, n)) %>%
   # graph
    ggplot(aes(word, n, fill=sentiment)) + 
    geom_col(show.legend=FALSE) + 
    facet_wrap(~sentiment, scales="free_y") + 
    labs(y="Contribution to Sentiment", x=NULL) + 
    coord_flip()
  
```

>
### Sentiment Analysis of CTR <= 7%, Use Bing Sentiment, all
#### Here, it seems that messages with low Click-Through rates are also generally positive, which suggests that BBT messages as a whole have a positive sentiment and that is not necessarily for why people are clicking or not.

```{r, echo=FALSE}

# filter out CTR <= 7%
 bing07 <- bbt_dfALL %>% 
   unnest_tokens(word, text) %>%  # separate all words out
   filter(CTR<=0.07) %>%          # only CTR < 0.07
  inner_join(get_sentiments("bing")) %>%   # add bing sentiment
  count(word, sentiment, sort=TRUE) 

 bing07 %>%
   group_by(sentiment) %>%
   #filter(n>1) %>%
   #top_n(10) %>%
   ungroup() %>%
   mutate(word=reorder(word, n)) %>%
   # graph
    ggplot(aes(word, n, fill=sentiment)) + 
    geom_col(show.legend=FALSE) + 
    facet_wrap(~sentiment, scales="free_y") + 
    labs(y="Contribution to Sentiment", x=NULL) + 
    coord_flip()

  
```



```{r, echo=FALSE, message=FALSE, eval=FALSE}

##### Sentiment Analysis (NRC) of All BBT Messages
bbt_dfALL %>% 
   unnest_tokens(word, text) %>%  # separate all words out
  inner_join(get_sentiments("nrc")) %>%   # add bing sentiment
  count(word, sentiment, sort=TRUE) %>%
   group_by(sentiment) %>%
   #filter(n>1) %>%
   top_n(8) %>%
   ungroup() %>%
   mutate(word=reorder(word, n)) %>%
   # graph
    ggplot(aes(word, n, fill=sentiment)) + 
    geom_col(show.legend=FALSE) + 
    facet_wrap(~sentiment, scales="free_y") + 
    labs(y="Number of Words", x=NULL) + 
    ggtitle("Top 10 Words by NRC Sentiment")+
    coord_flip()

```


> 
### Sentiment Analysis (BING) of All BBT Messages
#### Again, here is the sentiment analysis of All BBT messages, not separated by CTR. It again shows that the sentiment is mostly positive, reiterating the point made above. One thing to consider is that the Bing Sentiment Analysis does not consider sarcasm and negation. So something like "that is not funny" would still attribute funny to a positive sentiment.

```{r, echo=FALSE, message=FALSE}
bbt_dfALL %>% 
   unnest_tokens(word, text) %>%  # separate all words out
  inner_join(get_sentiments("bing")) %>%   # add bing sentiment
  count(word, sentiment, sort=TRUE) %>%
   group_by(sentiment) %>%
   #filter(n>1) %>%
   top_n(8) %>%
   ungroup() %>%
   mutate(word=reorder(word, n)) %>%
   # graph
    ggplot(aes(word, n, fill=sentiment)) + 
    geom_col(show.legend=FALSE) + 
    facet_wrap(~sentiment, scales="free_y") + 
    labs(y="Number of Words", x=NULL) + 
    ggtitle("Top 10 Words by BING Sentiment")+
    coord_flip()

```

>
### Further Mean & Median Analysis of Keyword "Behavior"
####  The Median/Mean CTR rates for all BBT messages are 11% and 11.86%, respectively. Let's check the Median/Mean for messages with the word "behavior"" in them. *The median/mean CTR for BBT messages that have the word behavior in them is 16% and 17% respectively. The messages with the word behavior are below.*

```{r, echo=FALSE, results='asis'}
library(knitr)
#summary(bbt3$CTR)  # mean is .11, median is .1186

behaviorMSG <- bbt3[grep("behavior.+", bbt3$Text.Message),]
behaviorMSG$Text.Message
#kable(behaviorMSG$Text.Message)
#summary(behaviorMSG$CTR) # median is 0.175, mean is 0.17

```

>
### Further Mean & Median Analysis of Keyword "Milestone(s)"
#### The Median/Mean CTR rates for all BBT messages are 11% and 11.86%, respectively. *The median/mean CTR for BBT messages that have the word "milestone(s)" in them is 28%, 28.8%. The messages with the word milestone are below.*
```{r, echo=FALSE, results='asis'}

milestoneMSG <- bbt3[grep("milestone.+", bbt3$Text.Message),]
milestoneMSG$Text.Message
#length(milestoneMSG$Text.Message)
#summary(milestoneMSG$CTR) 


```

>
### Further Mean & Median Analysis of Keyword "Toddler(s)"

#### The Median/Mean CTR rates for all BBT messages are 11% and 11.86%, respectively. *The median/mean CTR for BBT messages that have the word "toddler(s)" in them is 15.8%, 20%. The messages with the word toddler in them are below.*

```{r, echo=FALSE, results='asis'}

toddlerMSG <- bbt3[grep("toddler.+", bbt3$Text.Message),]
toddlerMSG$Text.Message
#length(toddlerMSG$Text.Message)
#summary(toddlerMSG$CTR) 


```
