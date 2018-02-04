---
title: "Making a Prediction Model"
author: "kat"
date: "January 5, 2018"
output: html_document
---

```{r, message=FALSE, echo=FALSE}
# clear workspace & remove variables
#knitr::opts_chunk$set(echo=FALSE)
rm(list = ls())

library(quanteda)
library(ggplot2)
library(wordcloud)
library(stringi)
library(data.table)
library(splitstackshape)
```

# Reading in small set of data
```{r, echo=FALSE}
### Reading in US Blogs
file <- ("./Coursera-SwiftKey/final/en_US/en_US.blogs.txt")
con <- file(description=file, open="r") 
line <- readLines(con, encoding="UTF-8")   # ADDING ENCODING HERE FIXED MY ISSUE!!!!!!!!!!!! FOR TEXT.... :(  now do to fix 
close(con)

#Reading in US News
file2 <- ('./Coursera-SwiftKey/final/en_US/en_US.news.txt')
con2 <- file(description=file2, "rb") 
line2 <- readLines(con2, n=-1, warn=TRUE, skipNul = TRUE, encoding="UTF-8")
close(con2)
```
## 10% Sample - Will do this normally, but not with my test set

```{r, echo=FALSE}
set.seed(2525)
### Taking 10% sample of each data##until I get a working model for speed sake...then increase up to 25%?
# .10% sample of BLOGS 90,000
#0.10 * length(line)
blogA <- line
blog <- sample(line, 90000)  #10% sample
# .10% sample of NEWS is 101000
#0.10 * length(line2)
news <-  sample(line2, 101000)
# 10% sample of TWITTER is 236000
# #0.10 * length(line3)
# twitterS <-  sample(line3, 236000)

```
  
# Just testing to see what it looks like

```{r}
library(stringi)
#blog <- line
#news <- line2
#blog
#news
news <- gsub("[^[:alnum:]]", " ",news)  # removes all elmeents which are not either alphabets or numbers!!!!
#stri_unescape_unicode(news)
#news
```

```{r}
#Creating corpus Document, labels did not "stick" or I haven't figured out how to correctly subset
myCorpus <- corpus(blog)
docvars(myCorpus, "Source") <- "blog"
summary(myCorpus)


```
myCorpus2 <- corpus(news)
docvars(myCorpus2, "Source") <- "news"
#summary(myCorpus2)

allCorpus <- myCorpus + myCorpus2 
summary(allCorpus)
#tail(summary(allCorpus))

#bbt3$English.Message <- gsub("[^[:alpha:]]", " ", bbt3$English.Message)

# Kat understanding process:
1. read in data
2. take sample for processing sake
3. create Corpus for each set
4. add all Corpus together (why corpus object?)
5. turn Corpus into dfm (document frequency matrix) with dfm() - while removing punctuation, spacing, swear words, tokenizing (do I need to tokenize to sentence first, then strip punctuation to tokenize by word?)
6. probabilities?
7. n-grams 2, 3

STEP 5
# 5-grams to 3-grams
```{r, echo=FALSE}
#don't know how to remove swear words!!!!!!!!
swearwords <- readLines("swearWords.txt", encoding="UTF-8")

# turns each text into tokens
blogtoks <- tokens(myCorpus, remove_punct=TRUE, remove_separators=TRUE, remove_numbers=TRUE, what="word")
#turns each tokenized text to lowercase
blogtoks <-tokens_tolower(blogtoks)
#turns tokenized texts into n-grams 1-5
blogunigrams <- tokens_ngrams(blogtoks, n=1, concatenator=" ")
blogbigrams <- tokens_ngrams(blogtoks, n=2, concatenator=" ")
blog3grams <- tokens_ngrams(blogtoks, n=3, concatenator=" ")
blog4grams <- tokens_ngrams(blogtoks, n=4, concatenator=" ")
blog5grams <- tokens_ngrams(blogtoks, n=5, concatenator=" ")

#head(blogtrigrams)
```

# working with 3grams to build some code- need to export as rd file later
# creating datatable for trigrams
```{r}
#unlist & count, then split into individual columns
blog3grams_ul <- unlist(blog3grams)  # use unlist
blog3grams_ul <- data.table(blog3grams_ul) # save as data.table
colnames(blog3grams_ul) <- c("trigram")
trigram_count_dt <- blog3grams_ul[, .(count=.N), by=trigram][order(-count)]
#trigram_count_dt

#SPLIT UP TRI GRAMS into 2 & 1 col
#References: https://gist.github.com/mrdwab/11380733
#https://cran.r-project.org/web/packages/splitstackshape/splitstackshape.pdf
library(splitstackshape)
#trigram_split_df<- cSplit(trigram_count_dt, "trigram", " ")

# 3 columns - for Tri-grams
trigram_split_df<- cSplit(trigram_count_dt, "trigram", " ")
trigram_split_df$previous <- paste(trigram_split_df$trigram_1, trigram_split_df$trigram_2, sep=" ")
neworder <- c("previous", "trigram_3", "count", "trigram_1", "trigram_2")
setcolorder(trigram_split_df, neworder)
trigram_split_df <- trigram_split_df[, setdiff(colnames(trigram_split_df), c("trigram_1", "trigram_2")), with=FALSE]
trigram_split_df$trigram_3 <- as.character(trigram_split_df$trigram_3)
trigram_split_df  # end result that I will store when app loads
saveRDS(trigram_split_df, file="triblogsamp.rds", ascii=FALSE)
```

blogunigrams <- tokens_ngrams(blogtoks, n=1, concatenator=" ")
blogbigrams <- tokens_ngrams(blogtoks, n=2, concatenator=" ")
blog3grams <- tokens_ngrams(blogtoks, n=3, concatenator=" ")
blog4grams <- tokens_ngrams(blogtoks, n=4, concatenator=" ")
blog5grams <- tokens_ngrams(blogtoks, n=5, concatenator=" ")

## Creating 5-grams - need to export as rd file when I do this with everything!!
```{r}
blog5grams_ul <- unlist(blog5grams)  # use unlist
blog5grams_ul <- data.table(blog5grams_ul) # save as data.table
colnames(blog5grams_ul) <- c("fivegram")
fivegram_count_dt <- blog5grams_ul[, .(count=.N), by=fivegram][order(-count)]
#fivegram_count_dt
#SPLIT UP TRI GRAMS into 2 & 1 col
#References: https://gist.github.com/mrdwab/11380733
#https://cran.r-project.org/web/packages/splitstackshape/splitstackshape.pdf
library(splitstackshape)
#trigram_split_df<- cSplit(trigram_count_dt, "trigram", " ")

# 3 columns
fivegram_split_df<- cSplit(fivegram_count_dt, "fivegram", " ")
fivegram_split_df$previous <- paste(fivegram_split_df$fivegram_1, fivegram_split_df$fivegram_2,fivegram_split_df$fivegram_3, fivegram_split_df$fivegram_4, sep=" ")
fivegram_split_df
neworder <- c("previous", "fivegram_5", "count", "fivegram_1", "fivegram_2", "fivegram_3", "fivegram_4")
setcolorder(fivegram_split_df, neworder) # reorder columns per above so I can subset
fivegram_split_df <- fivegram_split_df[, setdiff(colnames(fivegram_split_df), c("fivegram_1", "fivegram_2", "fivegram_3", "fivegram_4")), with=FALSE] #subsets only the first 3 cols
fivegram_split_df$fivegram_5 <- as.character(fivegram_split_df$fivegram_5)
fivegram_split_df  # end result that I will store
saveRDS(fivegram_split_df, file="fiveblogsamp.rds", ascii=FALSE)
```

#Creating 4-grams - need to export as rd file when I do this with everything!!
```{r}
blog4grams_ul <- unlist(blog4grams)  # use unlist
blog4grams_ul <- data.table(blog4grams_ul) # save as data.table
colnames(blog4grams_ul) <- c("fourgram")
fourgram_count_dt <- blog4grams_ul[, .(count=.N), by=fourgram][order(-count)]
#fivegram_count_dt
#SPLIT UP TRI GRAMS into 2 & 1 col
#References: https://gist.github.com/mrdwab/11380733
#https://cran.r-project.org/web/packages/splitstackshape/splitstackshape.pdf
library(splitstackshape)
#trigram_split_df<- cSplit(trigram_count_dt, "trigram", " ")

# reorder columns, combine first 3, and last 1 as predicted
fourgram_split_df<- cSplit(fourgram_count_dt, "fourgram", " ")
fourgram_split_df$previous <- paste(fourgram_split_df$fourgram_1, fourgram_split_df$fourgram_2,fourgram_split_df$fourgram_3 ,sep=" ")
fourgram_split_df

# reorder columns & subset for final dt 
neworder <- c("previous", "fourgram_4", "count", "fourgram_1", "fourgram_2", "fourgram_3")
setcolorder(fourgram_split_df, neworder) # reorder columns per above so I can subset
fourgram_split_df <- fourgram_split_df[, setdiff(colnames(fourgram_split_df), c("fourgram_1", "fourgram_2", "fourgram_3")), with=FALSE] #subsets only the first 3 cols
fourgram_split_df$fourgram_4 <- as.character(fourgram_split_df$fourgram_4)
fourgram_split_df  # end result that I will store
saveRDS(fourgram_split_df, file="fourblogsamp.rds", ascii=FALSE)
```

# 3-grams above

# 2-gram creation
```{r}
blogbigrams_ul <- unlist(blogbigrams)  # use unlist
blogbigrams_ul <- data.table(blogbigrams_ul) # save as data.table
colnames(blogbigrams_ul) <- c("bigram")
bigram_count_dt <- blogbigrams_ul[, .(count=.N), by=bigram][order(-count)]
#bigram_count_dt

#split into rows
#References: https://gist.github.com/mrdwab/11380733
#https://cran.r-project.org/web/packages/splitstackshape/splitstackshape.pdf
library(splitstackshape)
#trigram_split_df<- cSplit(trigram_count_dt, "trigram", " ")

# reorder columns, combine first 3, and last 1 as predicted
bigram_split_df<- cSplit(bigram_count_dt, "bigram", " ")
#bigram_split_df$previous <- paste(bigram_split_df$bigram_1, fourgram_split_df$fourgram_2,fourgram_split_df$fourgram_3 ,sep=" ") don't need this step for bigrams

names(bigram_split_df)[names(bigram_split_df)=="bigram_1"] ="previous"   #renamed bigram_1 previous
#bigram_split_df

# reorder columns & subset for final dt 

neworder <- c("previous", "bigram_2", "count")
setcolorder(bigram_split_df, neworder) # reorder columns per above so I can subset
#fourgram_split_df <- fourgram_split_df[, setdiff(colnames(fourgram_split_df), c("fourgram_1", "fourgram_2", "fourgram_3")), with=FALSE] #subsets only the first 3 cols
bigram_split_df$bigram_2 <- as.character(bigram_split_df$bigram_2)
bigram_split_df$previous <- as.character(bigram_split_df$previous)
bigram_split_df  # end result that I will store
saveRDS(bigram_split_df, file="biblogsamp.rds", ascii=FALSE)
```
# Unigram set

```{r}
blogunigrams_ul <- unlist(blogunigrams)  # use unlist
blogunigrams_ul <- data.table(blogunigrams_ul) # save as data.table
colnames(blogunigrams_ul) <- c("unigram")
unigram_count_dt <- blogunigrams_ul[, .(count=.N), by=unigram][order(-count)]
unigram_count_dt  # final dt I will use
saveRDS(unigram_count_dt, file="uniblogsamp.rds", ascii=FALSE)
```



TEST EXPRESSION FOR REFERENCE LATER
```{r}
output <- grepl("^kat rules$", trigram_split_df$previous)
#sum(output)
if(sum(output) == 0) {
  print("nothing")
} # when grep gives nothing, value=TRUE produces character(0), without value=TRUE, integer(0) 

```

# this function works for 6 only
```{r}
user_input <- c("and this is one of the")
user_input <- as.data.table(user_input)
user_split <- cSplit(user_input,"user_input", " ")
# want 5 words first 
user_split
n <- ncol(user_split)  # tells me how many words were entered
user_split[,(n-3):n]  # cuts the words to n-3 (max 4 words)
# this is specifically for 4-gram, if I did 6-gram, then this would be up to 5 words
#user_pred<- with(user_split, paste("^",user_input_3," ",user_input_4," ",user_input_5," ",user_input_6 ,"$", sep="")) #expression to input into grep

# for tri-gram input only (search last 2words)
user_pred<- with(user_split, paste("^",user_input_5," ",user_input_6 ,"$", sep=""))
output <- grepl(user_pred, trigram_split_df$previous)
#words <- grep(user_pred, trigram_split_df$previous, value=TRUE)
words <- trigram_split_df[like(previous, user_pred)]
#sum(output)
if(sum(output) == 0) {
  print("nothing")
} else {
top3 <- words[1:3,] #subsets grep used above to find top 3
total <- words[, sum(count)]  # gets the total occurances of grepped word by summing the count column
top3$prop <- (top3$count/total)*100 #creates a new column that gives the proportion of top 3 words as percentage
print(top3[1:3,c(2,4)]) #displays only the predicted word and percentage

 # faster to use words here or just have R run the grep line here
}

# when grep gives nothing, value=TRUE produces character(0), without value=TRUE, integer(0) 

```
failed attempt to use n to determine which strings to concatenate
```{r}
#using paste with variables 
#http://www.cookbook-r.com/Strings/Creating_strings_from_variables/

#words
#top3
#total
#ncol(user_split)
n <- ncol(user_split)
r<- user_split[1,n, with=FALSE]
s<- user_split[1,(n-1), with=FALSE]
t<- user_split[1,(n-2), with=FALSE]
u<-user_split[1,(n-3), with=FALSE]
paste("^",r[1,1]," ",s[1,1]," ",t[1,1]," ",u[1,1]," ","$", sep="")  # for some reason paste is reading these as 1's
#user_split[,1:(n-1)]
#user_split[,3:6]
#user_split[,5:]
#user_split[,(n-3):n]

```
hard code function for sentence lengths up to 7 (put i )

## These are the dts (soon to be rd's) that I will load in my ap
unigram_count_dt
bigram_split_df
trigram_split_df
fourgram_split_df
fivegram_split_df

# nestled if else 
https://www.r-bloggers.com/a-wrapper-around-nested-ifelse/

# Enter any number of words...FUNCTION WILL WORK!!..is slow for 5-gram table :( n=4)

```{r}
words=0 #for testing

#test words
#user_input <- c("and this is one of the")
#user_input <- c("and this is one of the") #for 6 test
#user_input <- c("is one of the")  # user enters a 4-gram that I know works
user_input <- c("kat is one of the") 


user_input <- as.data.table(user_input)
user_split <- cSplit(user_input,"user_input", " ")
n <- ncol(user_split)

#check to see if n>=5, if so cut things down
if(n>=5){
user_split<-user_split[,(n-3):n]
colnames(user_split) <- c("user_input_1", "user_input_2", "user_input_3", "user_input_4")
n=4
}
user_pred=FALSE # needed for condition

p=FALSE
y=FALSE
while(y==FALSE & n==4) {
  #case1 n=4, match
  if(n==4){   #combine string, grepl if else (output is not = 0; y=FALSE Else, y=TRUe, spit out)
    # very specific to n=4 because of user_inputs
    user_pred<- with(user_split, paste("^",user_input_1," ",user_input_2," ",user_input_3," ",user_input_4 ,"$", sep=""))
    # out put is specific to five_gram b/c this is 4 words search
    output <- sum(grepl(user_pred, fivegram_split_df$previous))
      if (output ==0) {
        # sets up case 2, where n=4, but no match, so I have to go to n=3
        #colnames(user_split) <- c("gone1", "user_input1", "user_input2", "user_input3")
        user_pred<- with(user_split, paste("^",user_input_2," ",user_input_3," ",user_input_4,"$",sep=""))
        print("no match1") #just for now, take this out later
        n=3
        y=TRUE # need this to get out of loop
      } else {
         # this will set up words for the output below
        # THIS TAKES THE LONGEST RIGHT NOW!!!!!!! #finds me all rows that match user input with predicted word & count
        words <- fivegram_split_df[like(previous, user_pred)]
        print("came from 5-gram table")  #just for testing
        # y=TRUE to get me out of this loop
        y=TRUE
        p=TRUE
        }      
  } #n==4, case 1 if end
  else {y==TRUE} #to jump out in case n=!3 ?? no working
} # end of while condition to see if 4 words finds a match

# Now go through loop to look for last 3 words in 4-gram table

m=FALSE
k=FALSE
if(p==FALSE){ #if I got a match, don't do this!
while(m==FALSE & n==3) {
  #case2 n=3, match
  if(n==3 & user_pred==FALSE){  #for case where n=3, not where it was cut from above #combine string, grepl if else (output is not = 0; y=FALSE Else, y=TRUe, spit out)
     user_pred<- with(user_split, paste("^",user_input_1," ",user_input_2," ",user_input_3,"$", sep=""))
     output <- sum(grepl(user_pred, fourgram_split_df$previous))
      if (output==0){
        user_pred<- with(user_split, paste("^",user_input_2," ",user_input_3,"$",sep=""))
        print("no match2") #just for now, take this out later
        n=2
        m=TRUE # need this to get out of loop
      } else {
        words <- fourgram_split_df[like(previous, user_pred)]
        print("came from 4-gram table")  #just for testing
        # y=TRUE to get me out of this loop
        p=TRUE
        m=TRUE
      }
  } else { # case where I started with n=4, now I have n=3, so user_pred==NULL
      output <- sum(grepl(user_pred, fourgram_split_df$previous))
      if (output==0){
        user_pred<- with(user_split, paste("^",user_input_3," ",user_input_4,"$",sep="")) #cut to 2-gram
        print("no match3") #just for now, take this out later
        n=2
        k=TRUE
        m=TRUE # need this to get out of loop
      } else {
        words <- fourgram_split_df[like(previous, user_pred)]
        print("came from 4-gram table started with n=4")  #just for testing
        # y=TRUE to get me out of this loop
        p=TRUE
        m=TRUE
      }
  }  
}  
} #p==true bracket


if(p==FALSE){
# Now go through loop to look for last 2 words in 3 gram table
q=FALSE
while(q==FALSE & n==2) {
  #case5 n=2, match
  if(n==2 & user_pred==FALSE){  # case where user only inputs 2 words
     user_pred<- with(user_split, paste("^",user_input_1," ",user_input_2,"$", sep=""))
     output <- sum(grepl(user_pred, trigram_split_df$previous))
      if (output==0){
        user_pred<- with(user_split, paste("^",user_input_2,"$",sep="")) # cut to 2nd word
        print("no match4") #just for now, take this out later
        q=TRUE # need this to get out of loop
        n=1
      } else {
        words <- trigram_split_df[like(previous, user_pred)]
        print("came from tri-gram table, n==2")  #just for testing
        # y=TRUE to get me out of this loop
        p=TRUE
        q=TRUE
      }
  } else if(k==TRUE) { # case where I started with n=4, now I have n=3, so k==TRUE
      output <- sum(grepl(user_pred, trigram_split_df$previous))
      if (output==0){
        user_pred<- with(user_split, paste("^",user_input_4,"$",sep="")) #cut to 1-gram
        print("no match5") #just for now, take this out later
        n=1
        q=TRUE # need this to get out of loop
      } else {
        words <- trigram_split_df[like(previous, user_pred)]
        print("came from 3-gram table, n=4")  #just for testing
        # y=TRUE to get me out of this loop
        p=TRUE
        q=TRUE
      }
  } else{
      output <- sum(grepl(user_pred, trigram_split_df$previous))
      if (output==0){
        user_pred<- with(user_split, paste("^",user_input_3,"$",sep="")) #cut to 1-gram
        print("no match6") #just for now, take this out later
        n=1
        q=TRUE # need this to get out of loop
      } else {
        words <- trigram_split_df[like(previous, user_pred)]
        print("came from 3-gram table, n=3")  #just for testing
        # y=TRUE to get me out of this loop
        p=TRUE
        q=TRUE
      }
    
    }
}  
} #p==false bracket


#for unigram search

if(p==FALSE){  #if no match was found yet...keep going
t=FALSE
i=FALSE
while(t==FALSE & n==1) {
  #cases 8/9...+++
  if(n==1 & user_pred==FALSE){  # case where user only inputs 1 word
     user_pred<- with(user_split, paste("^",user_input_1,"$", sep=""))
     output <- sum(grepl(user_pred, bigram_split_df$previous))
      if (output==0){ # this mean no bigrams with this word, go to unigrams
        output <- sum(grepl(user_pred, unigram_count_dt$previous))     # output might be 0 or 1, see condition below(i)
        print("no match7") #just for now, take this out later
        i=TRUE
        t=TRUE # need this to get out of loop
      } else {
        words <- bigram_split_df[like(previous, user_pred)]
        print("came from bi-gram table, n==1")  #just for testing
        # y=TRUE to get me out of this loop
        p=TRUE
        t=TRUE
      }
  } else { # all other cases, user_pred is defined from case n=4, n=3, n=2
      output <- sum(grepl(user_pred, bigram_split_df$previous))
      if (output==0){
        output <- sum(grepl(user_pred, unigram_count_dt$previous)) #output might be 0 or 1(see condition below (i))
        #user_pred<- with(user_split, paste("^",user_input_4,"$",sep="")) #cut to 1-gram
        print("no match7") #just for now, take this out later
        n=1
        i=TRUE
        t=TRUE # need this to get out of loop
      } else {
        words <- bigram_split_df[like(previous, user_pred)]
        print("came from b-gram table, n=4")  #just for testing
        # y=TRUE to get me out of this loop
        p=TRUE
        t=TRUE
      }
  } 
}  
} #p==false bracket

if(i==TRUE & output!=0){
  words <- unigram_count_dt[like(previous, user_pred)]
}
  


if(words!=0) {
    top3 <- words[1:3,] #subsets grep used above to find top 3
    total <- words[, sum(count)]
    top3$percent <- (top3$count/total)*100
    print(top3[1:3,c(2,4)])
 } else {
    print("this is where unigram top 3 goes")
  }

```   

```{r}
man = 2
if(man==FALSE){print("blah")}

```
  # gets the total occurances of grepped word by summing the count column
top3$prop <- (top3$count/total)*100 #creates a new column that gives the proportion of top 3 words as percentage
print(top3[1:3,c(2,4)])

```

     # very specific to n=4 because of user_inputs
    user_pred<- with(user_split, paste("^",user_input_1," ",user_input_2," ",user_input_3," ",user_input_4 ,"$", sep=""))
    # out put is specific to five_gram b/c this is 4 words search
    output <- sum(grepl(user_pred, fivegram_split_df$previous))
      if (output ==0) {
        # sets up case 3, where n=3, but no match, so I have to go to n=2
        user_pred<- with(user_split, paste("^",user_input_2," ",user_input_3," ",user_input_4,"$",sep=""))
        print("no match") #just for now, take this out later
        y=TRUE # need this to get out of loop
      } else {
         # this will set up words for the output below
        # THIS TAKES THE LONGEST RIGHT NOW!!!!!!! #finds me all rows that match user input with predicted word & count
        words <- fivegram_split_df[like(previous, user_pred)]
        # y=TRUE to get me out of this loop
        y=TRUE
        }      
  } #n==4, case 1 if end
} # end of while condition to see if 4 words finds a match




```
# spits output no matter what the number of words is
if(y==TRUE){
top3 <- words[1:3,] #subsets grep used above to find top 3
total <- words[, sum(count)]  # gets the total occurances of grepped word by summing the count column
top3$prop <- (top3$count/total)*100 #creates a new column that gives the proportion of top 3 words as percentage
print(top3[1:3,c(2,4)])#displays only the predicted word and percentage
} else {
  print("no match")
}

```
if(n==4) {
user_pred<- with(user_split, paste("^",user_input_1," ",user_input_2," ",user_input_3," ",user_input_4 ,"$", sep=""))
} else {
  if(n==3){
    user_pred<- with(user_split, paste("^",user_input_1," ",user_input_2," ",user_input_3," ",user_input_4 ,"$", sep=""))
  }
  
}

output <- sum(grepl(user_pred, fivegram_split_df$previous)) #gives me T/F value that allows me to sum in my function
}
if(output==0){
  
}


} else { #nothing happens, will hopefully just keep going
  }

# CAN THIS BE THE LAST PART OF THE THING? MAKE ABOVE....recursive?? or just keep doing else if statements (at bigrams output can be 0...)


if( output == 0) {  # maybe get rid of 4? in theory it doesn't have to be 4...
   # do nothing, hopefully will keep going
} else {
top3 <- words[1:3,] #subsets grep used above to find top 3
total <- words[, sum(count)]  # gets the total occurances of grepped word by summing the count column
top3$prop <- (top3$count/total)*100 #creates a new column that gives the proportion of top 3 words as percentage
print(top3[1:3,c(2,4)])#displays only the predicted word and percentage
}

print("it broke out")  # this is not working right now
```


```{r}
paste(r, s, sep=" ")
r[1,1]
```
#user_pred
as.matrix(r[1,])
class(r)
r
#r<- user_split[1,n]
#s
#t
#u
```

# grep a string the user inputs - only last 3 words
```{r}
user_input <- c("and this is one of the")  ## user can input whatever they want, i need to:
user_input <- as.data.table(user_input)
user_split <- cSplit(user_input,"user_input", " ") # splits string into parts
#need 1, 2, 4, 5, 6? grams dt's here..but right now just try with 3-gram
#check 3-gram table
```
#1. determine the length of the string
  # a - if it's more than 4 words, cut it down to 4 words (do I even need to do this? just account for length using n?), find the length
  # b - if it's not more than 4, find the length ### do this later?
#2. then my function needs to search 4-gram previous column - if there is a match --> continue to calculating % of top 3 below
#3 if grep finds nothing, then cut string to last 3 words (or if I use cSplit,I could juse take n-2:n) n=length of string
#4 use grep if grep finds nothing, then cut string to 2 last word (cSplit, (n-1:n))
#5 use grep if grep finds nothing, then take last word and predict otp 3 words (the, of, something) --> could just have this stores so I don't have to do the last part "lastresort" variable


###### AT THIS STEP - figure out how to split the input string into 2 words, 1 word
#grep("^one of$", trigram_split_df$previous, value = TRUE)   # probably to split with this trigram_split_df<- cSplit(trigram_count_dt, "trigram", " ")
#trigram_split_df$previous <- paste(trigram_split_df$trigram_1, trigram_split_df$trigram_2, sep=" ")
length(user_input)



oneof<- trigram_split_df[like(previous, "^one of$")] 
top3 <- oneof[1:3,] #subsets grep used above to find top 3
total <- oneof[, sum(count)]  # gets the total occurances of grepped word by summing the count column
top3$prop <- (top3$count/total)*100 #creates a new column that gives the proportion of top 3 words as percentage
top3[1:3,c(2,4)] #displays only the predicted word and percentage

```


REPEAT CODE FROM ABOVE?
```{r}
colnames(test) <- c("trigram")
trigram_count_dt <- test[, .(count=.N), by=trigram][order(-count)]
trigram_count_dt_zero <- trigram_count_dt[count == 1,]
sum2<- trigram_count_dt_zero[1:2000000,]
sum2[, sum(count)]
```

```{r}

######## FIGURE OUT HOW TO FINISH MY LOOP!!!!!!! even though I don't need it

library(data.table)
#head(blogbigrams)

# Loop to create data.table from n-gram lists

trigrams_dt=NULL #initialize data.table #length(blogbigrams[[1]]) # if I can figure out total length of trigrams - can initialize dt that way
y <- 2  # how many texts (lines) do I have?  change 2 for : length(blogbigrams)
for (i in y){x <- length(blogbigrams[[i]])
for (j in x) {
q <- blogbigrams[[i]][[j]]

trigrams_dt[i,] <- q
}
}
```

```{r}
trigrams_dt
#blogbigrams[[5]][[1:2]]
```
data.table(blogbigrams[[]][[]]
```{r}
trigrams_dt=data.table(trigrams=character()) # data table object 
#data.table(NULL)
data.table(blogbigrams)
d<- rbind(d, blogbigrams[[5]][[1]], blogbigrams[[5]][[2]])


saveRDS(d, file="d.rds", ascii=FALSE)
#blogbigrams[[5]][[1]]
```

```{r}
d2 <- readRDS("d.rds")
colnames(d2)<-c("6-gram")
d1<- c("the best")
#d2
d3<- rbind(d2, d1)
ncol(d3)

```

```{r}

```


```{r}

head(blogbigrams)
```
```{r}
library(dplyr)
dfm_Blog <- dfm(myCorpus, remove=swearwords, ngrams=2, tolower = TRUE, remove_punct = TRUE) #what = "fasterword", verbose = FALSE)
blogbigrams <- textstat_frequency(dfm_Blog)  # bigrams dataframe
blogbigrams<- blogbigrams %>% mutate(prop=frequency/sum(frequency))
```

```{r}
head(blogbigrams)
grep("^of_*", blogbigrams, value=TRUE)
```

dfm_Blog <- dfm(myCorpus, tolower = TRUE, remove_punct=TRUE, remove=swearwords) # this actually removes swearwords
dfm_Blogtrim <- dfm_trim(dfm_Blog, min_count=4)
#textstat_frequency(dfm_Blogtrim)

```{r}
features_blog_dfm <- textstat_frequency(dfm_Blog)
nrow(subset(features_blog_dfm, frequency==1))
subset(features_blog_dfm, feature == "bitches") # allows me to search for a particular word
```

~ 50% of the frequency of word representation is by first 80 or so words
```{r}
sum(features_blog_dfm$frequency)
nrow(features_blog_dfm)
library(dplyr)
b <-features_blog_dfm %>% mutate(prop=frequency/sum(frequency)) # to figure out % that is shows up...added up to get 50 above, it was about 80 words = 50% of the data
b<-  (b[1:20000,])  # play around with this number to figure out % of words that give you 95 of the frequency, here I can leave out 1944-844 words POTENTIALLY because they don't appear that often? MB...
 sum(b$prop)

```
# calculate 2-gram & 3-grams  for blog
```{r}
# this gives me the sum of how many words TOTAL (repeats or not - NOT unique words)
# make sure text_stat_frequency(dfm, n=____)  if n=25, only top 25...etc
sum(features_blog_dfm$frequency)  

#features_blog_dfm %>% mutate(prop=frequency/sum(frequency)) #Q: did I calculate prop correctly?

#Creating n-grams size 2 for prediction model, top 20

dat.dfm2 <- dfm(myCorpus, remove=swearwords, ngrams=2, tolower = TRUE, remove_punct = TRUE) #what = "fasterword", verbose = FALSE)
blogbigrams <- textstat_frequency(dat.dfm2)  # bigrams dataframe

saveRDS(blogbigrams, "blogbi.rds")

dat.dfm3 <- dfm(allCorpus, remove=swearwords, ngrams=3, tolower = TRUE, remove_punct = TRUE)
trigrams <- textstat_frequency(dat.dfm3)  %>% #trigrams dataframe
 mutate(prop=frequency/sum(frequency))

dat.dfm4 <- dfm(allCorpus, remove=swearwords, ngrams=4, tolower = TRUE, remove_punct = TRUE)
quadgrams <- textstat_frequency(dat.dfm4)  %>% #trigrams dataframe
 mutate(prop=frequency/sum(frequency))

trigrams
quadgrams

```

#attempting quiz
```{r}
subset(quadgrams, feature =="of_cheese")
```
```{r}
q <- as.character("a_case_of_beer_paste")
r <-as.character("a_case_of_cheese4")
t <- as.character("a_case_of_poop")
m <- as.character("a_case_of_5aaa")

all <- as.data.frame(rbind(q, r, t, m))
all$V1 <- as.character(all$V1)
all

grep("a_case_of_*", all$V1, value=TRUE)  # how to search a string!!!!!

```

```{r}
toks <- tokens(c(text1 = "the quick brown fox jumped over the lazy dog"))
tokens_ngrams(toks, n = 1:3)
tokens_ngrams(toks, n=2)
tokens_ngrams(toks, n = c(2,4), concatenator = " ")
tokens_ngrams(toks, n = c(2,4), skip = 1, concatenator = " ")

```



#summary(topfeatures(dat.dfm, 20))
#topfeatures(dat.dfm)

dat.dfm2 <- textstat_frequency(dat.dfm, n=25)
dat.dfm2
dat.dfm2$feature <- with(dat.dfm2, reorder(feature, -frequency))
ggplot(dat.dfm2, aes(x=feature, y=frequency))+
  geom_point() + 
  theme(axis.text.x=element_text(angle = 90, hjust=1))+
  ggtitle("Top 25 N-grams, size 2")

```

tokenInfoB <- summary(myCorpus)
b<- tokenInfoB[which.max(tokenInfoB$Tokens),]
b
#blogS[92]




## Plot to see most frequent words in Blogs, News & Twitter
#### I see there is what I'm assuming is a space as the most frequent character. I haven't looked into how to remove this. I didn't see this affect the n_grams, but maybe it will later.
```{r, echo=FALSE}
library(ggplot2)
swearwords <- readLines("swearWords.txt", encoding="UTF-8")



#blogs
dfm_Blog <- dfm(myCorpus, tolower = TRUE, remove_punct=TRUE, remove=swearwords)
features_blog_dfm <- textstat_frequency(dfm_Blog, n=25)
features_blog_dfm$feature <- with(features_blog_dfm, reorder(feature, -frequency))
ggplot(features_blog_dfm, aes(x=feature, y=frequency))+
  geom_point() + 
  theme(axis.text.x=element_text(angle = 90, hjust=1))+
  ggtitle("Top 25 Most Frequent Words in Blogs Document")

```
#news
dfm_Blog2 <- dfm(myCorpus2, tolower = TRUE, remove_punct=TRUE, remove=swearwords)
features_blog_dfm2 <- textstat_frequency(dfm_Blog2, n=25)
features_blog_dfm2$feature <- with(features_blog_dfm2, reorder(feature, -frequency))
ggplot(features_blog_dfm2, aes(x=feature, y=frequency))+
  geom_point() + 
  theme(axis.text.x=element_text(angle = 90, hjust=1))+
  ggtitle("Top 25 Most Frequent Words in News Document")

#twitter
dfm_Blog3 <- dfm(myCorpus3, tolower = TRUE, remove_punct=TRUE, remove=swearwords)
features_blog_dfm3 <- textstat_frequency(dfm_Blog3, n=25)
features_blog_dfm3$feature <- with(features_blog_dfm3, reorder(feature, -frequency))
ggplot(features_blog_dfm3, aes(x=feature, y=frequency))+
  geom_point() + 
  theme(axis.text.x=element_text(angle = 90, hjust=1))+
  ggtitle("Top 25 Most Frequent Words in Twitter Document")
```

```{r, echo=FALSE}
# playing with top features
#topfeatures(dfm_Blog)
```


## Which 2 words (N-grams) appear the most in all documents (Blogs, News, & Twitter combined) 
```{r, echo=FALSE}
#Creating n-grams size 2 for prediction model, top 20

dat.dfm <- dfm(allCorpus, remove=swearwords, ngrams=2, tolower = TRUE, remove_punct = TRUE, what = "fasterword", verbose = FALSE)
#summary(topfeatures(dat.dfm, 20))
#topfeatures(dat.dfm)

dat.dfm2 <- textstat_frequency(dat.dfm, n=25)
dat.dfm2
dat.dfm2$feature <- with(dat.dfm2, reorder(feature, -frequency))
ggplot(dat.dfm2, aes(x=feature, y=frequency))+
  geom_point() + 
  theme(axis.text.x=element_text(angle = 90, hjust=1))+
  ggtitle("Top 25 N-grams, size 2")
```

## Which 3 words (N-grams) appear the most in all documents (Blogs, News, & Twitter combined)
```{r, echo=FALSE}
dat.dfm3 <- dfm(allCorpus, remove=swearwords, ngrams=3, tolower = TRUE, remove_punct = TRUE, what = "fasterword", verbose = FALSE)
#summary(topfeatures(dat.dfm3, 20))
#topfeatures(dat.dfm3)

dat.dfm3 <- textstat_frequency(dat.dfm, n=25)
dat.dfm3
dat.dfm3$feature <- with(dat.dfm3, reorder(feature, -frequency))
ggplot(dat.dfm3, aes(x=feature, y=frequency))+
  geom_point() + 
  theme(axis.text.x=element_text(angle = 90, hjust=1))+
  ggtitle("Top 25 N-grams, size 3")
```

## Next Steps - I realize not every non-data scientist will understand, but maybe there's a linguist with some thoughts out there?
#### I need to read up on Katz's Backoff Model for NLP and figure out how to use the n-grams I created for a prediction model. Once I have that, I need to create a shiny app that allows the user to input a set of words and spits out a prediction of the next word (or words???) based on the model I create. Basically, a lot of magic (aka hard word) needs to happen. Other questions I haven't answered that were posed are :
1) How many unique words do you need in a frequency sorted dictionary to cover 50% of all word instances in the language? 90%? *I don't have the answer to this and am not sure I truly understand the question right now * 
2) How will I deal with foreign languages *No idea right now* 
3) How will I increase coverage to include words not in the corpora? *I could add another document, but why and what words? How do I know what's missing?*



# Appendix
## Preprocessing & EDA
#### The following was done for pre-processing and exploratory data analysis
- Use readLines to open connection to txt file, *encoding="UTF-8"* to avoid encoding issues with Windows *Appendix A*
- Remove profanity words, used http://www.bannedwordlist.com/ *(see Appendix E (Plots) or Appendix F (N-grams) for code)*
- Sampling : 10% of each document was taken for now. I will increase later when I have a working model. *Appendix B*
- quanteda package was used to create one corpus for all three sets of documents (was not able to keep the labels created (blog, news, twitter) to "stay"" when I added the corpuses together, meaning when I tried to subset, I can't filter for news corpus because the label for this doesn't seem to be there or maybe I didn't actually add them together? - *Appendix C* *any help with this is appreciated*
- Longest text by document type *Appendix D* 
- Basic plot of most frequent words per document type  *Appendix E*
-punctuation was removed for all three documents *(see Appendix E (Plots) or Appendix F (N-grams) for code)*
- N-grams of size 2 & 3 were created *Note: This took a while so I scaled back the sample size from 25% to 10% until I get a working model*  *Appendix F* 



>
A
```{r}
# clear workspace & remove variables
#knitr::opts_chunk$set(echo=TRUE)
#rm(list = ls())

library(quanteda)
library(ggplot2)
library(wordcloud)
```

```{r, eval=FALSE, echo=TRUE}
### Reading in US Blogs
file <- ("./Coursera-SwiftKey/final/en_US/en_US.blogs.txt")
con <- file(description=file, open="r") 
line <- readLines(con, encoding="UTF-8")   # ADDING ENCODING HERE FIXED MY ISSUE!!!!!!!!!!!! FOR TEXT.... :(  now do to fix 
close(con)

#Reading in US News
file2 <- ('./Coursera-SwiftKey/final/en_US/en_US.news.txt')
con2 <- file(description=file2, "rb") 
line2 <- readLines(con2, n=-1, warn=TRUE, skipNul = TRUE, encoding="UTF-8")
close(con2)

#Reading in US Twitter 
file3 <- ('./Coursera-SwiftKey/final/en_US/en_US.twitter.txt')
con3 <- file(description=file3) 
line3 <- readLines(con3, n=-1, warn=TRUE, skipNul = TRUE, encoding="UTF-8")
close(con3)

```

>
B

```{r, eval=FALSE, echo=TRUE}
### Taking 10% sample of each data##until I get a working model for speed sake...then increase up to 25%?
# .10% sample of BLOGS 90,000
0.10 * length(line)
blogS <- sample(line, 90000)
# .10% sample of NEWS is 101000
0.10 * length(line2)
newsS <-  sample(line2, 101000)
# 10% sample of TWITTER is 236000
0.10 * length(line3)
twitterS <-  sample(line3, 236000)

```

>
C
```{r, echo==TRUE, eval=FALSE}
#Creating corpus Document, labels did not "stick" or I haven't figured out how to correctly subset. I might not need this in the end, but I wanted it for EDA
myCorpus <- corpus(blogS)
docvars(myCorpus, "Source") <- "blog"
#summary(myCorpus)

myCorpus2 <- corpus(newsS)
docvars(myCorpus2, "Source") <- "news"
#summary(myCorpus2)

myCorpus3 <- corpus(twitterS)
docvars(myCorpus3, "Source") <- "twitter"
#summary(myCorpus3)

allCorpus <- myCorpus + myCorpus2 + myCorpus3
tail(summary(allCorpus))
```

>
D

```{r, echo=TRUE, eval=FALSE}
tokenInfoB <- summary(myCorpus)
b <- tokenInfoB[which.max(tokenInfoB$Tokens),]
texts(blogS)[b$Tokens] # longest length text in my blog sample

tokenInfoN <- summary(myCorpus2)
c <- tokenInfoN[which.max(tokenInfoN$Tokens),]
texts(newsS)[c$Tokens] # longest length text in my news sample

tokenInfoT <- summary(myCorpus3)
d<- tokenInfoT[which.max(tokenInfoT$Tokens),]
texts(twitterS)[d$Tokens] # longest length text in my twitter sample

```

>
E

```{r, echo=TRUE, eval=FALSE}
## Plot of most frequent words in Blogs, News & Twitter
#blogs
dfm_Blog <- dfm(myCorpus, tolower = TRUE, remove_punct=TRUE, remove=swearwords)
features_blog_dfm <- textstat_frequency(dfm_Blog, n=100)
features_blog_dfm$feature <- with(features_blog_dfm, reorder(feature, -frequency))
ggplot(features_blog_dfm, aes(x=feature, y=frequency))+
  geom_point() + 
  theme(axis.text.x=element_text(angle = 90, hjust=1))+
  ggtitle("Top 100 Most Frequenct Words in Blogs Document")

#news
dfm_Blog2 <- dfm(myCorpus2, tolower = TRUE, remove_punct=TRUE, remove=swearwords)
features_blog_dfm2 <- textstat_frequency(dfm_Blog2, n=100)
features_blog_dfm2$feature <- with(features_blog_dfm2, reorder(feature, -frequency))
ggplot(features_blog_dfm2, aes(x=feature, y=frequency))+
  geom_point() + 
  theme(axis.text.x=element_text(angle = 90, hjust=1))+
  ggtitle("Top 100 Most Frequenct Words in News Document")

#twitter
dfm_Blog3 <- dfm(myCorpus3, tolower = TRUE, remove_punct=TRUE, remove=swearwords)
features_blog_dfm3 <- textstat_frequency(dfm_Blog3, n=100)
features_blog_dfm3$feature <- with(features_blog_dfm3, reorder(feature, -frequency))
ggplot(features_blog_dfm3, aes(x=feature, y=frequency))+
  geom_point() + 
  theme(axis.text.x=element_text(angle = 90, hjust=1))+
  ggtitle("Top 100 Most Frequenct Words in Twitter Document")
```

>
F


```{r, echo=TRUE, eval=FALSE}
### Creating n-grams size 2 for prediction model
dat.dfm <- dfm(allCorpus, remove=swearwords, ngrams=2, tolower = TRUE, remove_punct = TRUE, what = "fasterword", verbose = FALSE)
summary(topfeatures(dat.dfm, 20))
topfeatures(dat.dfm)

#Creating n-grams size 3 for prediction model
dat.dfm3 <- dfm(allCorpus, remove=swearwords, ngrams=3, tolower = TRUE, remove_punct = TRUE, what = "fasterword", verbose = FALSE)
summary(topfeatures(dat.dfm3, 20))
topfeatures(dat.dfm3)
```


