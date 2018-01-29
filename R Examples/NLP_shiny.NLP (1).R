#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

rm(list = ls())
library(shiny)
library(data.table)
library(splitstackshape)
#Data2<- readRDS("sample.rds")
unigram_count_dt <- readRDS("uniblogsamp.rds")
bigram_split_df <- readRDS("biblogsamp.rds")
trigram_split_df  <- readRDS("triblogsamp.rds")
fourgram_split_df <- readRDS("fourblogsamp.rds")
fivegram_split_df<- readRDS("fiveblogsamp.rds")
print("pre-loaded")

shinyServer(function(input, output) {
   
# hopefully makes reactive   
   userInput <- reactive({
     input$user_input
    })  
  
   output$changed <- renderText({
     paste(userInput(), "Kat added this")
   })
   
    output$parrot <- renderText({
      input$user_input
    })
    
  output$view <- renderTable({ 
   
    
     
   # rm(list=setdiff(ls(), c("unigram_count_dt", "bigram_split_df", "trigram_split_df", "fourgram_split_df", "fivegram_split_df")))
    #test words
    rm(list=setdiff(ls(), c("fivegram_split_df")))
   # user_input <- c("grizzle is one of the")
    user_input <- userInput()
    user_input <- as.data.table(user_input)
    user_split <- cSplit(user_input,"user_input", " ")
    n <- ncol(user_split)
    
    #check to see if n>=5, if so cut things down
    if(n>=5){
      user_split<-user_split[,(n-3):n]
      colnames(user_split) <- c("user_input_1", "user_input_2", "user_input_3", "user_input_4")
      n=4
    }
    
    y=FALSE # use this to determine if I need to do discounting at the end for unobserved?
    
    #n=4
    if(n==4){ 
      user_pred<- with(user_split, paste("^",user_input_1," ",user_input_2," ",user_input_3," ",user_input_4 ,"$", sep=""))
      words <- fivegram_split_df[like(previous, user_pred)]  
      if (nrow(words) ==0) {
        #user_pred<- with(user_split, paste("^",user_input_2," ",user_input_3," ",user_input_4,"$",sep="")) # don't need this?
        user_split <- user_split[,2:4]  
        colnames(user_split) <- c("user_input_1", "user_input_2", "user_input_3")
        print("no match1") #just for now, take this out later
        n=3
        y=TRUE # use this for discounting at the end???
      } else {
        print("came from 5-gram table")  #just for testing
      }      
    }
    
    # CASE n=3
    if(n==3){
      user_pred<- with(user_split, paste("^",user_input_1," ",user_input_2," ",user_input_3,"$", sep=""))
      words <- fourgram_split_df[like(previous, user_pred)]
      if (nrow(words)==0){
        user_split <- user_split[,2:3]
        colnames(user_split) <- c("user_input_1", "user_input_2")
        print("no match2") #just for now, take this out later
        n=2
        ### come up with another variable to decide if I should apply discount
      } else {
        print("came from 4-gram table")  #just for testing
      }
    }
    
    ## n=2 
    if(n==2){  
      user_pred<- with(user_split, paste("^",user_input_1," ",user_input_2,"$", sep=""))
      words <- trigram_split_df[like(previous, user_pred)]
      if (nrow(words)==0){
        user_split <- user_split[,2]
        colnames(user_split) <- c("user_input_1")
        print("no match4") #just for now, take this out later
        n=1
        # come up with another variable to decide if I should apply discount
      } else {
        print("came from tri-gram table, n==2")  #just for testing
      }
    } 
    
    ## n=1 
    if(n==1){  # might have to change nrow condition for words
      user_pred<- with(user_split, paste("^",user_input_1,"$", sep=""))
      words <- bigram_split_df[like(previous, user_pred)]
      if (nrow(words)==0){
        words <- unigram_count_dt[like(unigram, user_pred)] # search the unigram table
      }
      else {
        print("came from bi-gram table, n==1")  #just for testing
      }
    }
    
    # find the % - don't know if I'm doing this correctly
    if(nrow(words) !=0) {
      top3 <- words[1:3,] #subsets grep used above to find top 3
      total <- words[, sum(count)]
      top3$percent <- (top3$count/total)*100
      print(as.data.frame(top3[1:3,c(2,4)]))
    } else {
      print(unigram_count_dt[1:3,1])
    }
    
    
  
    
    
    })  # end of Render table brackets 
    
    
    



})
