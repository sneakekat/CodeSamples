#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Coursera Data Science Specialization Capstone"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      h3("Enter a set of words"),
        textInput(inputId="user_input", 
                  label="")
      #can also set #value=
         # numericInput(inputId="testnumber", 
          #             label="Type in any number", value=10),
         # submitButton("Submit") # need this to make the function not run until they submit?
            
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(  # panel 1
        tabPanel("Main",
         textOutput("parrot", container=span),
         textOutput("changed"),
         #dataTableOutput('view')),
         tableOutput("view")),
         #textOutput("view")),
        
      tabPanel("Explanation")  #panel 2
        
      # tableOutput("view")
       #textOutput("predictions3"),
       #DT::dataTableOutput("predictions2")
       #dataTableOutput("predictions2")
      )
    )
  )
))
