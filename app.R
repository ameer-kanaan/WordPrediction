library(shiny)
library(quanteda)
library(stringr)
library(data.table)
library(stringi)

ui <- navbarPage("Amer Kanaan - Interactive Word Prediction Model",
                 tabPanel("Predictor",
                          verticalLayout(
                                p("This is an interactive model interface for next word prediction, it aims to simulate a mobile phone prediction interface. Please enter a text in the indicated field and see the predicted results. The interface should work fast and quick."),
                                br(),
                                fluidRow(column(2, offset =  5,
                                                textInput("strings", "Enter Your Text Here")
                                )
                                )
                                ,
                                hr(),
                                
                                fluidRow( 
                                      column(2, offset = 3,
                                             wellPanel(
                                                   textOutput("pr1")
                                             )
                                      ),
                                      column(2,
                                             wellPanel(
                                                   textOutput("pr2")
                                             )
                                      ),
                                      column(2,
                                             wellPanel(
                                                   textOutput("pr3")
                                             )
                                      )
                                      
                                ),
                                br(),
                                
                                fluidRow( column(2, offset = 5,
                                                 htmlOutput("prediction1"),
                                                 br(),
                                                 htmlOutput("prediction2"),
                                                 br(),
                                                 htmlOutput("prediction3")
                                                 
                                )
                                ) 
                          )
                 ),
                 tabPanel("About",
                          p("This model is done if fulfilling the last project for John Hopkin's Coursera Capstone of Data Science Specialization.", tags$br(),
"It utilizes an efficient (i.e. fast and memory-friendly) algorithm for interactively predicting the next word. The algorithm used the Stupid-Backoff method", a( "(Brants et al., 2007)", href= "https://www.aclweb.org/anthology/D07-1090/"), ".The interface should be intuitive and easy to use.", tags$br(),
                            "According to the benchmarking script provided by the course instructors, the expected precision of this model is just above 20% for the top-3 predictions.", tags$br(), tags$br(),
                            "For code and scripts, please visit my", a("github repository." , href = "https://github.com/ameer-kanaan/WordPrediction")
                            
                            )
                 )
)
                 
# )

server <- function(input, output) {
      
      
      output$pr1 <- renderText({find_next_word(input$strings)[1]})
      output$pr2 <- renderText({find_next_word(input$strings)[2]})
      output$pr3 <- renderText({find_next_word(input$strings)[3]})
      
      observe({ 
            
            pred <- {find_next_word(input$strings)}
            
            
            output$prediction1 <- renderUI({
                  
                  HTML(paste("First Prediction is: ",strong(pred [1])))
            })
            
            output$prediction2 <- renderUI({
                  
                  paste("Second Prediction is: ",pred [2])
            })
            
            output$prediction3 <- renderUI({
                  
                  paste("Third Prediction is: ",pred [3])
            })
            
            
      }
      )
      
      
}

shinyApp(ui = ui, server = server)

