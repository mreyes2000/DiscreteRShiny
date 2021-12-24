# modular multiplication

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)

source("modCalc.R")

# The user interface
stylesheet <- tags$head(tags$style(HTML('
    .alert {
  padding: 20px;
  background-color: #f44336; /* Red */
  color: white;
  margin-bottom: 15px;
}

/* The close button */
.closebtn {
  margin-left: 15px;
  color: white;
  font-weight: bold;
  float: right;
  font-size: 22px;
  line-height: 20px;
  cursor: pointer;
  transition: 0.3s;
}

#label {
    text-align: center;
}

/* When moving the mouse over the close button */
.closebtn:hover {
  color: black;
}
  ')
))

header <- dashboardHeader(title = span(HTML("Modular Multiplication"), 
                                       style = "font-weight: bold;
                                                font-size: 22px;"),
                          titleWidth = 310)

sidebar <- dashboardSidebar(
  width = "300px",
  numericInput("Zn", label = h3("Finite Field Z_n"), value = 2, min=2),
  uiOutput("Valid"),
  selectInput("BoxWidth", "Box Width in Bar Chart", c("Extra Narrow" = 10, "Narrow" = 5, "Medium" = 2, "Wide" = 1, "Extra Wide" = 0), selected=2),
  radioButtons("BarOrientation", "Bar Chart Orientation", c("Vertical", "Horizontal"))
)

body <- dashboardBody(
  stylesheet,
  fluidRow(
    column(
      width = 12,
      plotOutput("chart", height = 500)
    )
  ),
  fluidRow(
    column(
      width = 12,
      h3("Table of Modular Multiplication"),
      dataTableOutput("table"), style="height:500px; overflow-y: scroll;overflow-x: scroll; "
    )
  )
)
      
ui <- dashboardPage(title = "Modular Multiplication", header, sidebar, body)

# Functions that read the input and modify the output and input
server <- function(session, input, output) {
  myReactives <- reactiveValues()
  #You need observe() to create a reactive context
  observe({
    myReactives$Zn <-input$Zn
  })
  output$Zn <- renderUI({helpText(myReactives$Zn)})

  observeEvent(input$Zn, {
    if(input$Zn < 2 || is.na(input$Zn) || !is.integer(input$Zn)){
      number <- 'Nope'
      output$Valid <- renderUI({
        tags$div(class = "alert", 
                 tags$span(class='closebtn',
                           onclick="this.parentElement.style.display='none';",
                           'x'),
                 'Specify an integer greater than 2'
        )
      })
    }
    else 
    {
      output$Valid <- renderUI(h1(''))
      coprimes <- ModMult.makeList(input$Zn)
      tbl <- outer(coprimes, coprimes, function(a, b) {return ((a * b) %% input$Zn)})
      colnames(tbl) <- coprimes
      rownames(tbl) <- coprimes
      output$table <- renderDataTable(datatable(tbl, options = list(pageLength = length(coprimes), dom= "t"),
                                                escape = FALSE)
                                      %>% formatStyle(columns = c(0:length(coprimes)),
                                                      width=paste0(str(100/length(coprimes)), "%")))
    }
  })
  
  observeEvent(c(input$Zn, input$BoxWidth, input$BarOrientation), {
    if(input$Zn < 2 || is.na(input$Zn) || !is.integer(input$Zn)){
      number <- 'Nope'
      output$alert <- renderUI({
        tags$div(class = "alert", 
                 tags$span(class='closebtn',
                           onclick="this.parentElement.style.display='none';",
                           'x'),
                 'Specify a number > 2'
        )
      })
    }
    else 
    {
      coprimes <- ModMult.makeList(input$Zn)
      orders <- ModMult.vOrder(coprimes, input$Zn)
      x <- sort(unique(orders))
      y <- tabulate(orders)[x]
      labs <- c("Orders", "Count")
      vec <- c(1,2)
      if (input$BarOrientation == "Horizontal") vec <- c(2, 1)
      output$chart <- renderPlot(barplot(y, main= "Distribution of Orders", xlab = (labs[vec])[1] , ylab =  (labs[vec])[2] , names.arg = x, space=strtoi(input$BoxWidth), horiz=(input$BarOrientation == "Horizontal")))
    }
  })
}

# Run the app.
shinyApp(ui = ui, server = server)
