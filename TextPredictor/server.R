library(shiny)

source("./utilities.R")

# Define server logic required to run prediction algorithm and render text
shinyServer(function(input, output) {
        predict <- reactive({
                input$textinput %>% nextword()
        })
        output$textoutput <- renderText(predict())
})
