library(shiny)
library(shinythemes)
library(data.table)
library(RCurl)
library(readr)




data <- read.csv("df.train.csv")
head(data)
m1 <- readRDS("m3.rds")









ui <- fluidPage(theme = shinytheme("superhero"),
    
    # Page header
    headerPanel('Stroke Risk Predictor'),
    
    # Input values
    sidebarPanel(
        #HTML("<h3>Input parameters</h3>"),
        tags$label(h3('Input parameters')),
        radioButtons("gender", 
                     label = "Gender", 
                     choices = unique(data$gender), 
                     select = 1),
        numericInput("age", 
                     label = "Age (in years)", 
                     value = 36),
        radioButtons("hypertension", 
                           label = "Diagnosis of Hypertension?",
                           choiceNames = list("No", "Yes"),
                           choiceValues = list("0", "1"),
                           select = 1),
        radioButtons("heart_disease", 
                           label = "Diagnosis of Heart Disease?",
                           choiceNames = list("No", "Yes"),
                           choiceValues = list("0", "1"),
                           select = 1),
        radioButtons("ever_married", 
                           label = "Ever Married?",
                           choices = unique(data$ever_married),
                           select = 1),
        radioButtons("work_type", 
                           label = "Type of Work?",
                           choices = unique(data$work_type),
                           select = 1),
        radioButtons("Residence_type", 
                           label = "Residence Type",
                           choices = unique(data$Residence_type),
                           select = 1),
        numericInput("avg_glucose_level", 
                     label = "Average Glucose Level", 
                     value = 80),
        numericInput("bmi", 
                     label = "BMI", 
                     value = 28.00),
        radioButtons("smoking_status", 
                           label = "Smoking Status",
                           choiceNames = list("Never Smoked", "Formerly Smoked", "Smokes", "Uknown"),
                           choiceValues = list("never smoked", "formerly smoked", "smokes", "Uknown"),
                           select = 1),
        actionButton("submitbutton", "Submit", 
                     class = "btn btn-primary")
    ),
    
    mainPanel(
        tags$label(h3('Stroke Risk Prediction')),
        verbatimTextOutput('contents'),
        tableOutput('tabledata'),
        
        
        tags$img(src="TabPlot.png", height="800px", width="1200px", alt="Something Went Wrong", deleteFile=F)
    )
        
)






server <- function(input, output, session) {
    
    # Input Data
    datasetInput <- reactive({  
    
        df <- data.frame(
            Name = c("gender",
                     "age",
                     "hypertension",
                     "heart_disease",
                     "ever_married",
                     "work_type",
                     "Residence_type",
                     "avg_glucose_level",
                     "bmi",
                     "smoking_status"),
            Value = (c(input$gender,
                       input$age,
                       input$hypertension,
                       input$heart_disease,
                       input$ever_married,
                       input$work_type,
                       input$Residence_type,
                       input$avg_glucose_level,
                       input$bmi,
                       input$smoking_status)),
            stringsAsFactors = FALSE)
        
        stroke <- "stroke"
        df <- rbind(df, stroke)
        input <- transpose(df)
        write.table(input,"input.csv", sep = ",", quote = FALSE, row.names = FALSE, col.names = FALSE)
        
        test <- read.csv(paste("input", ".csv", sep=""), header = TRUE)
        
        test$gender <- as.character(test$gender, levels = c("male", "female", "Other"))
        test$hypertension <- as.character(test$hypertension, levels = c("0", "1"))
        test$heart_disease <- as.character(test$heart_disease, levels = c("0", "1"))
        test$ever_married <- as.character(test$ever_married, levels = c("No", "Yes"))
        test$work_type <- as.character(test$work_type, levels = c("Private", "Govt_job", "Self-employed", "children", "Never_worked"))
        test$Residence_type <- as.character(test$Residence_type, levels = c("Urban", "Rural"))
        test$smoking_status <- as.character(test$smoking_status, levels = c("never smoked", "formerly smoked", "smokes", "Unknown"))
        test$stroke <- as.character(test$stroke, levels = c("0", "1"))
        write.csv(test, "test.csv")
        
        
        Output <- data.frame('Stroke Risk' = predict(m1,test, type = "response"))
        print(Output)
        
    })
    
    # Status/Output Text Box
    output$contents <- renderPrint({
        if (input$submitbutton > 0) { 
            isolate("Range of 0-1, 0 = lower risk, 1 = higher risk") 
        } else {
            return("Server is ready for calculation.")
        }
    })
    
    # Prediction results table
    output$tabledata <- renderTable({
        if (input$submitbutton > 0) { 
            isolate(datasetInput()) 
        } 
    })
    
}


# Run the application 
shinyApp(ui = ui, server = server)



