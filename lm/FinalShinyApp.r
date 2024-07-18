#This code loads in the shiny package

library(shiny)


ui <- fluidPage(

    #This code creates the layout of the ui. First by adding a title.
    
    titlePanel("Linear Modeling App"),

    #Next, we add a side bar were the inputs for the program will reside.
    
    sidebarLayout(
        sidebarPanel(

            #Here, we load the file.
            
            fileInput("file1", "Choose CSV File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            
            # Horizontal line ----
            tags$hr(),
            
            #This code Checks the file for a header.
            
            checkboxInput("header", "Header", TRUE),

            # Horizontal line ----
            tags$hr(),
            
            #This code adds the button to change the delimiter the fileInput reads for.
            
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ","),
            
            #Here, we establish what quatation marks the fileInput should read for.
            
            radioButtons("quote", "Quote",
                         choices = c(None = "",
                                     "Double Quote" = '"',
                                     "Single Quote" = "'"),
                         selected = '"'),
            
            # Horizontal line ----
            tags$hr(),
            
            #This code adjusts how much of the table should be shown.
            
            radioButtons("disp", "Display",
                         choices = c(Head = "head",
                                     All = "all"),
                         selected = "head"),

            #Here we create the button that will generate the linear model.

            actionButton("Push", "Run Linear Model")
        ),

        #Here we establish the main panel where outputs will be displayed.
        
        mainPanel(
            plotOutput("distPlot"),
            plotOutput("lmPlot"),
            tableOutput("contents"),
            verbatimTextOutput("txt"),
            verbatimTextOutput("txt1")

        )
    )
)


server <- function(input, output) {

    #Here, we generate the code that generates the displays. First, by loading the csv file for the server to read.
    
    dataInput <- reactive({
        req(input$file1)
        
        df <- read.csv(input$file1$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
        return(df)
    })
    
    #Here, we generate the initial scatter plot
    
    output$distPlot <- renderPlot({
        plot(dataInput()$x,dataInput()$y, xlab = "X", ylab = "Y")
    })

    #Here we create an empty list of reactive values.
    
    lmdata <- reactiveValues()

    #Here we create a function that creates reactive values based on the loaded csv file
    #The first creates the linear model, the second pulls the r squared value, and the third
    #pulls the intercept and slope.
    
    update_lm <- function() {
        lmdata$model <- lm(y ~ x, data = dataInput())
        lmdata$rsq <- summary(lmdata$model)$r.squared
        lmdata$coef <- summary(lmdata$model)$coefficients[,1]
    }

    #Here we assign the function of making the linear model to the action button.
    
    observeEvent(input$Push, {update_lm()})
    
    #Here we create a scatter plot that will have the linear model when the action button is pushed.
    
    output$lmPlot <- renderPlot({
        plot(dataInput()$x,dataInput()$y, , xlab = "X", ylab = "Y")
        abline(lmdata$model)
    })

    #Here we output the r squared value and the model coefficients as text.
    
    output$txt <- renderText({paste("R Squared Value:", lmdata$rsq)})
    output$txt1 <- renderText({paste("Coefficient Values (Intercept, Slope):", lmdata$coef)})
    
    #Here, We generate the table for the data set we input.
    
    output$contents <- renderTable({
        
       #This code checks the file for a header, if there is one, it loads the file as a table,
        #otherwise the table returns null.
        
        
        if(input$disp == "head") {
            return(head(dataInput()))
        }
        else {
            return(dataInput())
        }
        
    })
        
}

# Run the application 
shinyApp(ui = ui, server = server)
