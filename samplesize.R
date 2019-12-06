###################################################################
# This file contains the Shiny app for the sample size calculator.
# 
# Author: Ning Li (ning.li2@ge.com)
# Updated: 2019-08-13
###################################################################


library(shiny)

# USER INTERFACE
ui <- fluidPage(
    navbarPage("Medical Device Sample Size Calculator",
               tabPanel("Information",
                        titlePanel("Information"),
                        br(),
                        "This calculator can be used to calculate the target sample size for single-arm MR and non-inferority Ultrasound trial",
                        
                        br(),
                        br(),
                        "Created by: Ning Li(ning.li2@ge.com)",
                        
                        br(),
                        br(),
                        "Last updated: 13th August, 2019"),
               tabPanel("Calculator",
                        # Give the page a title
                        titlePanel("Sample Size Calculator"),
                        helpText("Sample size calculator for MR and Ultrasound Trial"),
                        # left input 
                        sidebarPanel(h3("User Inputs"),
                                     helpText("If output is NA, check that all inputs are feasible values."),
                                     br(),
                                     
                                     radioButtons(inputId = "device_type", label = "Type of Device", choices = c("MR","Ultrasound"), inline = TRUE),
                                     
                                     
                                     numericInput(inputId = "expected_rate", label = "Expected rates", value = 0.9, min = 0, max = 1, width=validateCssUnit("50%")),
                                     uiOutput("expected_rate"),
                                     helpText("Enter the expected rate, e.g., 0.9"),
                                     
                                     numericInput(inputId = "target_rate", label = "Target or Control group rates", value = 0.75, min = 0, max = 1, width=validateCssUnit("50%")),
                                     uiOutput("target_rate"),
                                     helpText("Enter the target or control group rate, e.g., 0.75"),
                                     
                                     numericInput(inputId = "alpha", label = "Type I Error", value = 0.05, min = 0, max = 1, width=validateCssUnit("50%")),
                                     uiOutput("alpha"),
                                     helpText("Enter the type I error, e.g., 0.05"),
                                     
                                     numericInput(inputId = "power", label = "Power", value = 0.80, min = 0, max = 1, width=validateCssUnit("50%")),
                                     uiOutput("power"),
                                     helpText("Enter the power, e.g., 0.80"),
                                     
                                     conditionalPanel(condition = "input.device_type=='Ultrasound'",
                                                      numericInput(inputId = "margin",label = "Non-inferority Margin", value = 0.1, min = 0, max = 1, step = 1, width=validateCssUnit("50%")),
                                                      helpText("Enter the non-inferority Margin")
                                     ),
                                     width=6),
                       # right outputs
                       mainPanel(br(),
                         # h3("Results"),
                         br(),
                         dataTableOutput("results"), width=6 ))
    )
)




# SERVER function

server <- function(input, output, session) {
    
    
    # Evaluate Design Effect for selected options, and output table with values
    results.data.table <- reactive({
        
        ifelse(input$device_type=="MR", 
               {
                   samplesize<-( qnorm(1-input$alpha/2)*sqrt(input$target_rate*(1-input$target_rate))+ qnorm(input$power)*sqrt(input$expected_rate*(1-input$expected_rate)) )**2 / (input$expected_rate-input$target_rate)**2
               },
               {
                   samplesize<-(qnorm(1-input$alpha/2) + qnorm(input$power))**2 *(input$expected_rate*(1-input$expected_rate)+input$target_rate*(1-input$target_rate)) / (abs(input$expected_rate-input$target_rate)-input$margin)**2    
               })
    
    row.names<-c("Sample Size")
    
    finalresult<-data.frame("Result"=row.names, "Value"=samplesize)
    
    })


# Output data table results without any data table options (page no., row number, etc..)
output$results <- renderDataTable({results.data.table()}, options = list(lengthChange= F, paging = F, searching = F, ordering= F, info=F))


}

shinyApp(ui = ui, server = server)
