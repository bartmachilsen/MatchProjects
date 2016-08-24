library(shiny)
library(shinythemes)
matches <- read.csv("data/output.csv",header=FALSE)


ui <- fluidPage(theme=shinytheme("cerulean"),
  wellPanel(
  tags$h1("Matching PhD-projects within the Humanities"),
  tags$p("This Shiny app takes your KU Leuven u-number as input, and returns the 3 best matching PhD-projects within the Group of Humanities based on a lexical analyis of the provided project descriptions."),
  tags$p("The quality of the matches depends on the quality of your and others' project descriptions."),
  tags$p("The ", tags$code("Python"), " notebook for collecting the project descriptions and for computing the lexical similarities can be found here: ",tags$a(href= "www.someurl.com","code")),
  textInput(inputId="unum",
              label="Enter your u-number",
              value="your u-number",
              width = '50%',
              placeholder = NULL
            ),
  submitButton("Submit")
  ),
  wellPanel(
   textOutput("text1"),
   tags$br(),
   tags$b(textOutput("match1_name")),
   textOutput("match1_title"),
   tags$a(textOutput("match1_url")),
   tags$br(),
   tags$b(textOutput("match2_name")),
   textOutput("match2_title"),
   tags$a(textOutput("match2_url")),
   tags$br(),
   tags$b(textOutput("match3_name")),
   textOutput("match3_title"),
   tags$a(textOutput("match3_url"))
  )
)


server <- function(input,output) {
  # Strip u/U from number
  unum <- reactive({
    if(tolower(substring(input$unum,1,1))=="u"){
      substring(input$unum,2)
    }else{
      input$unum
      }
  })
  

  match_result <- reactive({
    match_found <- matches[grepl(unum(),matches$V1),]
    if(nrow(match_found)==1){paste0("Matches for  ", match_found$V2, " :")}else{"u number not found"}
    })

  match1_name <- reactive({
    match_found <- matches[grepl(unum(),matches$V1),]
    if(nrow(match_found)==1){paste0("1. ", match_found$V4)}else{" "}
  })
  match1_title <- reactive({
    match_found <- matches[grepl(unum(),matches$V1),]
    if(nrow(match_found)==1){paste0(match_found$V6)}else{" "}
  })
  match1_url <- reactive({
    match_found <- matches[grepl(unum(),matches$V1),]
    if(nrow(match_found)==1){paste0(match_found$V5)}else{" "}
  })
  
  match2_name <- reactive({
    match_found <- matches[grepl(unum(),matches$V1),]
    if(nrow(match_found)==1){paste0("2. ", match_found$V7)}else{" "}
  })
  match2_title <- reactive({
    match_found <- matches[grepl(unum(),matches$V1),]
    if(nrow(match_found)==1){paste0(match_found$V9)}else{" "}
  })
  match2_url <- reactive({
    match_found <- matches[grepl(unum(),matches$V1),]
    if(nrow(match_found)==1){paste0(match_found$V8)}else{" "}
  })
  
  match3_name <- reactive({
    match_found <- matches[grepl(unum(),matches$V1),]
    if(nrow(match_found)==1){paste0("3. ", match_found$V10)}else{" "}
  })
  match3_title <- reactive({
    match_found <- matches[grepl(unum(),matches$V1),]
    if(nrow(match_found)==1){paste0(match_found$V12)}else{" "}
  })
  match3_url <- reactive({
    match_found <- matches[grepl(unum(),matches$V1),]
    if(nrow(match_found)==1){paste0(match_found$V11)}else{" "}
  })
    
  output$text1 <- renderText({match_result()})
  
  output$match1_name <- renderText({match1_name()})
  output$match1_title <- renderText({match1_title()})
  output$match1_url <- renderText({match1_url()})

  output$match2_name <- renderText({match2_name()})
  output$match2_title <- renderText({match2_title()})
  output$match2_url <- renderText({match2_url()})
  
  output$match3_name <- renderText({match3_name()})
  output$match3_title <- renderText({match3_title()})
  output$match3_url <- renderText({match3_url()})
  
}
shinyApp(ui=ui, server=server)