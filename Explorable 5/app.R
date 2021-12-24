# Assignment 2: permute

################################################################################
#
# Link to Published App:
#
# Link to Demo App: https://jannawithrow2.shinyapps.io/permute/
#
################################################################################

library(shiny)
library(shinydashboard)
library(shinyWidgets)
# for alert messages
library(shinyalert)

source("permutecalc.R")

stylesheet <- tags$head(tags$link(rel = "stylesheet", type = "text/css",
                                  href = "styles.css"))

header <- dashboardHeader(title = "Permutations", titleWidth = 240)

sidebar <- dashboardSidebar(disable = TRUE)

body <- dashboardBody(
  tags$div(class = "body_font",
           # It is conventional to put most style settings in a separate file,
           # which should then be placed in a subfolder titled "www"
           fluidRow(stylesheet,
                    column(
                      width = 4,
                      box(
                        width = NULL, height = NULL,
                        h3("Input"),
                        textInput("atext", em("a"), "(12)"),
                        textInput("btext", em("b"), "(13)"),
                      ),
                      useShinyalert(),
                      box(
                        width = NULL, height = 165,
                        h3("Products"),
                        uiOutput("prodab"),
                        uiOutput("prodba")
                      ),
                      box(
                        width = NULL, height = 165,
                        h3("Inverses"),
                        uiOutput("inva"),
                        uiOutput("invb")
                      ),
                      box(
                        width = NULL, height = 165,
                        h3("Conjugates"),
                        uiOutput("conja"),
                        uiOutput("conjb")
                      ),
                    ),
                    
                    column(
                      width = 4,
                      box(
                        width = NULL, height = 350,
                        h3("Powers of ", em("a")),
                        uiOutput("powersa")
                      ),
                      box(
                        width = NULL, height = 350,
                        h3("Powers of ", em("ab")),
                        uiOutput("powersab")
                      )
                    ),
                    
                    column(
                      width = 4,
                      box(
                        width = NULL, height = 350,
                        h3("Powers of ", em("b")),
                        uiOutput("powersb")
                      ),
                      box(
                        width = NULL, height = 350,
                        h3("Powers of ", em("ba")),
                        uiOutput("powersba")
                      ),
                      actionBttn(inputId = "btncalc", label = "Calculate",
                                 style = "jelly", size = "lg")
                    )
           )
  ),
  tags$head(tags$style("#atext, #btext, #prodab, #prodba, #inva, #invb,
  #conja, #conjb{color: DodgerBlue; font-family: Computer Modern; font-size:
                       18px; text-align: center;}")),
  tags$head(tags$style("#powersa, #powersab, #powersb, #powersba{overflow-y:
                       scroll; max-height: 250px; color: DodgerBlue;}"))
)

ui <- dashboardPage(header, sidebar, body)


server <- function(input, output) {
  observeEvent(input$btncalc, {
    a <- input$atext
    b <- input$btext
    error <- F
    
    p_check <- function(x) {str_detect(x, regex("^([(]\\d+[)])+$"))}
    repeat_int <- function(x) {str_detect(x, regex("(\\d).*\\1+"))}
    
    input_error <- function (x) {
      if (!p_check(x) & x != "I") {
        shinyalert(title = "Invalid Entry",
                   text = paste0("Please input your permutation, \"", x, ",\" in
                                 cycle notation."),
                   type = "error", confirmButtonCol = "#94DBFF")
        error <<- T
      } else if (repeat_int(x)) {
        shinyalert(title = "Invalid Entry",
                   text = paste0("Please input your permutation, \"", x, ",\" as
                                 a product of disjoint cycles."),
                   type = "error", confirmButtonCol = "#94DBFF")
        error <<- T
      }
    }
    
    input_error(a)
    input_error(b)
    
    if (!error) {
      ab <- Perm.multiply(a, b)
      output$prodab <- renderUI(withMathJax(sprintf("$$ab = %s$$", ab)))
      ba <- Perm.multiply(b, a)
      output$prodba <- renderUI(withMathJax(sprintf("$$ba = %s$$", ba)))
      output$powersa <- renderUI(HTML(Perm.powerString(a)))
      output$powersb <- renderUI(HTML(Perm.powerString(b)))
      output$powersab <- renderUI(HTML(Perm.powerString(ab)))
      output$powersba <- renderUI(HTML(Perm.powerString(ba)))
      aInv <- Perm.inverse(a)
      output$inva <- renderUI(withMathJax(sprintf("$$a^{-1} = %s$$", aInv)))
      bInv <- Perm.inverse(b)
      output$invb <- renderUI(withMathJax(sprintf("$$b^{-1} = %s$$", bInv)))
      aConj <- Perm.conjugate(a, b)
      output$conja <- renderUI(withMathJax(sprintf("$$aba^{-1} = %s$$", aConj)))
      bConj <- Perm.conjugate(b, a)
      output$conjb <- renderUI(withMathJax(sprintf("$$bab^{-1} = %s$$", bConj)))
    }
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
