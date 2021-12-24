library(shiny)
library(shinydashboard)
library(shinyWidgets)

#Functions that implement the mathematics
source("cayleys3calc.R")
source("cayleya5calc.R")

#The user interface
header <- dashboardHeader(title = "Cayley Graph for Dihedral Groups",
                          titleWidth = 600)

sidebar <- dashboardSidebar(disable = TRUE)

body <- dashboardBody(
  fluidRow(align="center",
           h3("Dihedral Group", style=kahua.s),
           selectInput("Dn", label= NULL, choices = c("D3 (Triangle)" = 3, "D4 (Square)" = 4, "D5 (Pentagon)" = 5, "D6 (Hexagon)" = 6))
  ),
  column(
    width = 3,
    h3("Multiplication", style=kahua.s),
    h4("Defining relations:", style=kahua),

    div(HTML(paste(uiOutput("r_def_relation_1"), CA5.manifestWord("ff")," = I<br>", CA5.manifestWord("frfr")," = I",sep="")),style=kahua.s),
    h4("Rewrite rules:", style=kahua),                 
    div(HTML(paste(  uiOutput("r_def_relation_1_imp"),
                     CA5.manifestWord("ff")," &#8658; I<br>",
                     uiOutput("r_def_relation_3_imp"),
                     sep="")),style=kahua.s),
    actionBttn("btnright","Select right operand (red)",style="jelly",color="danger"),
    uiOutput("right"),
    actionBttn("btnleft","Select left operand (green)",style="jelly",color="success"),
    uiOutput("left"),
    actionBttn("btncalc","Calculate the product (purple)",style="jelly",color="royal"),
    uiOutput("prod"),
    #uiOutput("message")
  ),
  column(
    width = 6,
    h2("The Cayley Graph", align="center", style = kahua.s),
    div(HTML("Labeling rule: no ",CA5.manifestWord("r")," to the left of ",CA5.manifestWord("f"),"<br>"),style=kahua, align= "center"),
    plotOutput("cayley",height = 400, click = "plot_click"),
    uiOutput("message")
  ),
  column(
    width = 3,
    h3("Permutations", align="center", style = kahua.s),
    selectInput("chooser",helpText("Choose",span("r", style = "color:royalblue; font-weight: bold"),span(textOutput("order_choose_perm"),style="font-size:initial"),style=kahua),c("(123)","(132)")),
    selectInput("choosef",helpText("Choose",span("f", style = "color:darkorange; font-weight: bold"),span(": Order 2",style="font-size:initial"),style=kahua),c("(12)","(13)","(23)")),
    h3("Vertex Labels", align="center", style = kahua.s),
    radioButtons("radio", label = NULL,
                 choices = list("Numbers" = 1, "Permutations" = 2, "Words" = 3), 
                 selected = 1),
    actionBttn("btnreset","Reset",style="jelly",color="warning"),
  )
   
)
ui <- dashboardPage(header, sidebar, body)


#Functions that read the input and modify the output and input
server <- function(session, input, output) {
  #Variables that are shared among server functions
  
  rightop <- ""
  leftop <- ""
  rperm <- "(123)"
  fperm <- "(12)"
  chooseRight <- FALSE
  chooseLeft <- FALSE
    #Initialization
  observeEvent(input$Dn,{
    output$r_def_relation_1 <- renderText(paste(CA5.manifestWord(paste(rep("r", input$Dn), collapse = "")), " = I <br>"))
    output$r_def_relation_1_imp <- renderText(paste(CA5.manifestWord(paste(rep("r", input$Dn), collapse = "")), " &#8658; I <br>"))
    output$r_def_relation_3_imp <- renderText(paste(CA5.manifestWord("rf"), " &#8658; ",CA5.manifestWord("f"),  CA5.manifestWord(paste(rep("r", input$Dn), collapse = ""))))
    output$order_choose_perm <- renderText(paste(": Order ", toString(input$Dn)))
    r_choices <- list(c("(123)", "(132)"), c("(1234)", "(1432)"), c("(12345)", "(15432)"), c("(123456)", "(165432)"))
    f_choices <- list(c("(12)", "(13)", "(23)"), c("(12)(34)", "(14)(23)", "(13)", "(24)"), c("(12)(35)", "(25)(34)", "(15)(24)", "(14)(23)", "(13)(45)"), c("(12)(36)(45)", "(26)(35)", "(16)(25)(34)", "(15)(24)", "(14)(23)(56)", "(13)(469"))
    names(r_choices) <- c("3", "4", "5", "6")
    names(f_choices) <- c("3", "4", "5", "6")
    
    print(r_choices[strtoi(input$Dn)-2])
    updateSelectInput(
      session = session,
      inputId = 'chooser',
      choices = r_choices[strtoi(input$Dn)-2]
    )
    updateSelectInput(
      session = session,
      inputId = 'choosef',
      choices = f_choices[strtoi(input$Dn)-2]
    )
    
    S3DF <<- CS3.makeDataFrame(input$Dn)
    output$cayley <- renderPlot(CS3.drawGraph(S3DF, input$radio))
  })
  
  relate <- 'Defining relations: <br/>
  rrr=I,ff=I,frfr=I<br/>Rewrite rules:<br/>
  "rrr" -> "", "ff" -> ""<br/>
  "rf" -> "frr"'
  output$defrel <- renderUI(h3(HTML(relate)))
#Set a flag so we know how to use the next mouse click
  observeEvent(input$radio,{
    S3DF <<- CS3.makePerms(S3DF,rStr=input$chooser,fStr=input$choosef)
    output$cayley <- renderPlot(CS3.drawGraph(S3DF, input$radio))
  })
  
  observeEvent(input$chooser,{
    S3DF <<- CS3.makePerms(S3DF,rStr=input$chooser,fStr=input$choosef)
    output$cayley <- renderPlot(CS3.drawGraph(S3DF, input$radio))
  })
  
  observeEvent(input$choosef,{
    S3DF <<- CS3.makePerms(S3DF,rStr=input$chooser,fStr=input$choosef)
    output$cayley <- renderPlot(CS3.drawGraph(S3DF, input$radio))
  })
  
  
  observeEvent(input$btnleft,{
    output$message <- renderUI(h3("Click on a vertex"))
    chooseLeft <<- TRUE
  })
  observeEvent(input$btnright,{
    output$message <- renderUI(h3("Click on a vertex"))
    chooseRight <<- TRUE
  })
#Use the mouse click to select a vertex
  observeEvent(input$plot_click,{
    i <- CS3.findClosestVertex(input$plot_click$x,input$plot_click$y, input$Dn)
    if (chooseRight){
      rightop <<- S3DF[i,6]
      output$right <- renderUI(h3(rightop))
      S3DF <<- CS3.markVertex(S3DF,i,"orangered")
      output$message <- renderUI(h3(""))
      chooseRight <<- FALSE
    }
    if (chooseLeft){
      leftop <<- S3DF[i,6]
      output$left <- renderUI(h3(leftop))
      S3DF <<- CS3.markVertex(S3DF,i,"darkturquoise")
      output$message <- renderUI(h3(""))
      chooseLeft <<- FALSE
    }
    #Redraw the graph to show the selected vertex
    output$cayley <- renderPlot(CS3.drawGraph(S3DF, input$radio))
  })

#Multiply the selected group elements
  observeEvent(input$btncalc,{
    output$message <- renderUI("")
    product <<- CS3.multiply(leftop,rightop, input$Dn)
    msg <- paste0("The product is ",leftop,rightop," which simplifies to "       ,product,".")
    output$prod <- renderUI(h3(msg))
    S3DF <<- CS3.markVertex(S3DF,product,"slateblue")
    #Redraw the graph to show the result
    output$cayley <- renderPlot(CS3.drawGraph(S3DF, input$radio))
  })
  
  observeEvent(input$btnreset,{
    output$message <- renderUI("")
    product <<- ""
    msg <- "Click the buttons above to choose elements to multiply."
    output$prod <- renderUI(h3(msg))
    output$left <- renderUI(h3(""))
    output$right <- renderUI(h3(""))
    S3DF <<- CS3.makeDataFrame(input$Dn)
    #Redraw the graph to show the result
    output$cayley <- renderPlot(CS3.drawGraph(S3DF, input$radio))
  })
}

#Run the app
shinyApp(ui = ui, server = server)