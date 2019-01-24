library(shiny)

# Define UI for application 
shinyUI(fluidPage(
        # Application title
        titlePanel("Text Predictor"),
        # Sidebar: text-box for input & submit button to make prediction
        sidebarLayout(
                sidebarPanel(
                        textInput(
                                inputId = "textinput",
                                label = "Enter text here:",
                                value = ""
                        ),
                submitButton("Predict")
                ),
    
    # Show a plot of the generated distribution
    mainPanel(
            h3("Predicted text:"),
            textOutput(outputId = "textoutput")
    )
  )
))
