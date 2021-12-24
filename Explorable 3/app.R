# GroupA4

################################################################################
#                                                                              #
# Link to Published A4 App: https://jannawithrow3.shinyapps.io/group_a4/       #
#                                                                              #
# Link to Demo D6 App: https://jannawithrow.shinyapps.io/group_d6/             #
#                                                                              #
################################################################################

library(shiny)
library(shinydashboard)
library(shinyWidgets)

source("a4calc.R")
source("buttonrows.R")
source("permutecalc.R")

header <- dashboardHeader(
  title = span(HTML("A<sub>&thinsp;&thinsp;4&thinsp;</sub>&thinsp;: Symmetry
                    Group of the Tetrahedron"), style = "font-weight: bold"),
  titleWidth = 420
)

sidebar <- dashboardSidebar(disable = TRUE)

body <- dashboardBody(
  fluidRow(
    column(
      width = 12,
      box(
        width = NULL,
        height = 412,
        fluidRow(
          column(
            width = 8,
            h4(span("Inputs and Products", style = "font-weight: bold")),
            h5("Click on elements of the group to find their product."),
            div(id = "results2",
                withTags(
                  table(
                    tr(width = "150px",
                       th(HTML("&nbsp;&nbsp;&thinsp; Inputs")),
                       width = "150px", th("Products")),
                    th(width = "150px", htmlOutput("line.out")),
                    th(width = "150px", htmlOutput("results")),
                  )
                ),
            ),
            tags$head(tags$style("#results2{color:white; font-size:20px;
                  font-style:italic; overflow:scroll; max-height: 275px;
                  background: lightskyblue}"))
          ),
          column(
            width = 4,
            h4(""),
            div(align = "center", withTags(table(
              tr(actionBttn("reset", "Clear Inputs and Products"))
            )))
          )
        )
      ),
      
      fluidRow(
        column(width = 4,
               box(
                 width = NULL,
                 h4(span("Elements of the Group", style = "font-weight: bold")),
                 h1(""),
                 div(h5("The Identity"), style = "text-align: center"),
                 controlRow1("ctrlI"),
                 h1(""),
                 div(h5("Order 6 Elements (60° and 300° Rotations)"),
                     style = "text-align: center"),
                 controlRow2(
                   c("ctrl123456", "ctrl165432")
                 ),
                 h1(""),
                 div(h5("Order 3 Elements (120º and 240º Rotations)"), style = "text-align: center"),
                 controlRow2(
                   c("ctrl135_246", "ctrl153_264")
                 ),
                 h1(""),
                 div(h5("Order 2 Element (180º Rotation)"),
                     style = "text-align: center"),
                 controlRow1(
                   c("ctrl14_25_36")
                 ),
                 h1(""),
                 div(h5("Order 2 Elements (Edge-Edge Flips)"),
                     style = "text-align: center"),
                 controlRow3(
                   c("ctrl12_36_45", "ctrl16_25_34", "ctrl14_23_56")
                 ),
                 h1(""),
                 div(h5("Order 2 Elements (Vertex-Vertex Flips)"),
                     style = "text-align: center"),
                 controlRow3(
                   c("ctrl26_35", "ctrl13_46", "ctrl15_24")
                 ),
                 h3(""),
               ),
        ),
        
        column(width = 4,
               box(
                 width = NULL,
                 h4(span("Subgroups", style = "font-weight: bold")),
                 h1(""),
                 div(h5(htmlOutput("subsc1")), style = "text-align: center"),
                 buttonRow1("btnC1", labels = list(htmlOutput("showc1")),
                            btn_style = "padding:4px"
                 ),
                 h1(""),
                 div(h5(htmlOutput("subsc2")), style = "text-align: center"),
                 buttonRow1("btnC2_1", labels = list(htmlOutput("showc21")),
                            btn_style = "padding:4px"
                 ),
                 h5(""),
                 buttonRow3(
                   input_ids = c("btnC2_2", "btnC2_3", "btnC2_4"),
                   labels = list(htmlOutput("showc22"),
                                 htmlOutput("showc23"),
                                 htmlOutput("showc24")),
                   btn_style = "padding:4px"
                 ),
                 h5(""),
                 buttonRow3(
                   input_ids = c("btnC2_5", "btnC2_6", "btnC2_7"),
                   labels = list(htmlOutput("showc25"),
                                 htmlOutput("showc26"),
                                 htmlOutput("showc27")),
                   btn_style = "padding:4px"
                 ),
                 h1(""),
                 div(h5(htmlOutput("subsc3")), style = "text-align: center"),
                 buttonRow1("btnC3_1", labels = list(htmlOutput("showc31")),
                            btn_style = "padding:4px"
                 ),
                 h1(""),
                 div(h5(htmlOutput("subsc6")), style = "text-align: center"),
                 buttonRow1("btnC6_1", labels = list(htmlOutput("showc6")),
                            btn_style = "padding:4px"
                 ),
                 h1(""),
                 div(h5(htmlOutput("subss3")), style = "text-align: center"),
                 buttonRow2(
                   input_ids = c("btnS3_1", "btnS3_2"),
                   labels = list(htmlOutput("shows31"),
                                 htmlOutput("shows32")),
                   btn_style = "padding:4px"
                 ),
                 h1(""),
                 div(h5(htmlOutput("subsv4")), style = "text-align: center"),
                 buttonRow3(
                   input_ids = c("btnV4_1", "btnV4_2", "btnV4_3"),
                   labels = list(htmlOutput("showv41"),
                                 htmlOutput("showv42"),
                                 htmlOutput("showv43")),
                   btn_style = "padding:4px"
                 ),
                 h1(""),
                 div(h5(htmlOutput("subsd6")), style = "text-align: center"),
                 buttonRow1("btnD6", labels = list(htmlOutput("showd6")),
                            btn_style = "padding:4px"
                 ),
                 h1(""),
                 hr(),
                 div(align = "center",
                     radioButtons("formatting", "Select Formatting Preference",
                                  choices = c("Use Subscripts",
                                              "Forgo Subscripts"),
                                  selected = "Use Subscripts"))
               ),
        ),
        
        column(width = 4,
               box(
                 width = NULL,
                 h4(span("Cosets", style = "font-weight: bold")),
                 h1(""),
                 h5("Select a subgroup, then click \"Left Cosets\" or
                    \"Right Cosets.\""),
                 buttonRow2(
                   input_ids = c("btnLC", "btnRC"),
                   labels = list("Left Cosets", "Right Cosets"),
                   btn_style = "padding:4px"
                 ),
                 h1("")
               ),
               box(
                 width = NULL,
                 h4(span("Conjugate Subgroup", style = "font-weight: bold")),
                 h1(""),
                 h5("To start, select a subgroup, then click \"Select a.\""),
                 buttonRow2(
                   input_ids = c("btnmark", "btnconj"),
                   labels = list("Select a", "Generate Subgroup"),
                   btn_style = "padding:4px"
                 ),  
                 h5(uiOutput("conjmsg")),
                 h1("")
               ),
               box(
                 width = NULL,
                 h4(span("Generate a Subgroup", style = "font-weight: bold")),
                 h1(""),
                 h5("To start, click \"Generator a.\""),
                 buttonRow3(
                   input_ids = c("btnmarkgena", "btnmarkgenb", "btngen"),
                   labels = list("Generator a", "Generator b", "Generate"),
                   btn_style = "padding:4px"
                 ), 
                 h5(uiOutput("genmsg")),
                 h1("")
               ),
               box(
                 width = NULL,
                 h4(span("Reset", style = "font-weight: bold")),
                 h1(""),
                 buttonRow1("btnclear", "Clear", btn_style = "padding:4px"),
                 h1("")
               )
        )
      ),
      
      column(
        width = 12,
        box(
          width = NULL,
          align = "center",
          h4(span("Multiplication Table", style = "font-weight: bold;")),
          tableOutput("multable"),
          tags$style("#multable {overflow: scroll; white-space: nowrap}"),
          tags$style("#multable td:first-child {font-weight: bold}")
        )
      ))
  ),
  tags$style("::-webkit-scrollbar {width: 6px; height: 6px; border-radius: 12px}
                   ::-webkit-scrollbar-thumb
                   {background-color: rgba(0, 0, 0, .2); border-radius: 12px;
                   -webkit-box-shadow: inset 0 0 6px rgba(0, 0, 0, .3)}"
  )
)

ui <- dashboardPage(title = "D6: Symmetry Group of the Hexagon", header,
                    sidebar, body)


server <- function(input, output, session) {
  # Global variables accessible to server()
  N <- 12 # Number of elements in the group
  A4DF <- makeA4data()
  
  # Colors for cosets
  color.list <- c("paleturquoise", "palegreen", "lavender", "lavenderblush",
                  "peachpuff", "lightyellow", "lightcyan", "mintcream",
                  "lightsalmon", "lightblue", "thistle")
  
  # Output to display in the text box
  result.list <- ""
  line.out <- ""
  
  # Result of all multiplications so far
  product <- "I"
  subgroup <- numeric(0)
  subgroup_c <<- numeric(0)
  conjugating <- FALSE
  generating <- 0
  a <- "I"
  gena <- "I"
  genb <- "I"
  
  # Computes a product as specified by "a" and "b" in vector v
  evaluate <- function(v, a, b) {
    result <- "I"
    for (i in 1:length(v)){
      result <- Perm.multiply(result, ifelse(v[i] == "a", a, b))
    }
    return (result)
  }
  
  # Elements in the chosen subgroup
  displayButton <- function(i) {
    renderUI({actionButton(A4DF[i, 1], A4DF[i, 2],
                           style = paste("padding:4px; background:",
                                         A4DF[i, 3]))}) 
  }
  
  # Show all the buttons
  showButtons <- function() {
    output$ctrlI <- displayButton(1)
    output$ctrl123456 <- displayButton(2)
    output$ctrl165432 <- displayButton(3)
    output$ctrl135_246 <- displayButton(4)
    output$ctrl153_264 <- displayButton(5)
    output$ctrl14_25_36 <- displayButton(6)                                     
    output$ctrl12_36_45 <- displayButton(7)
    output$ctrl16_25_34 <- displayButton(9)
    output$ctrl14_23_56<- displayButton(8)
    output$ctrl26_35 <- displayButton(10)                                     
    output$ctrl13_46 <- displayButton(11)
    output$ctrl15_24 <- displayButton(12)
  }
  showButtons()
  
  # Display the multiplication table
  tbl <- outer(A4DF[, 2], A4DF[, 2], Vectorize(Perm.multiply, c("a", "b")))
  colnames(tbl) <- A4DF[, 2]
  rownames(tbl) <- A4DF[, 2]
  output$multable <- renderTable(tbl, rownames = TRUE)
  
  # Multiplies by a specified permutation and displays all calculations so far
  compute.and.show <- function(perm) {
    if (conjugating) {
      a <<- perm
      output$conjmsg <- renderUI(paste0("To conjugate your chosen subgroup by
                                        the element ", perm, 
                                        ", select \"Generate Subgroup.\"",
                                        collapse = ""))
      conjugating <<- FALSE
      return()
    }
    if (generating == 1) {
      gena <<- perm
      output$genmsg <- renderUI(paste0("If you would like see the subgroup
      generated by only ", gena, ", then click \"Generate.\" Otherwise, click
      \"Generator b,\" and select another element of the group.",
                                       collapse = ""))
      return()
    }
    if (generating == 2) {
      genb <<- perm
      output$genmsg <- 
        renderUI(paste0("To generate a subgroup with elements ", gena, " and ",
                        genb, ", select \"Generate.\"",
                        collapse = ""))
      return()
    }
    product <<- Perm.multiply(perm,product)
    line.out <<- paste(line.out, "&nbsp;&nbsp;", perm, "<br/>")
    result.list <<- paste(result.list, product, "<br/>")
    output$line.out <- renderUI(HTML(line.out))
    output$results <- renderUI(HTML(result.list))
  }
  
  # Marks all elements in a subgroup with a color
  mark.subgroup <- function() {
    for (i in 1:N) {
      A4DF$color[i] <<- 
        if ((i %in% subgroup) & !(A4DF$color[i] == "pink")) {
          "yellow"
        } else if (i %in% subgroup) {
          "pink"
        } else {
          "gray90"
        }
    }
  }
  
  # Event handlers for all the element buttons 
  observeEvent(input$btnI, {
    compute.and.show("I")
  })
  observeEvent(input$btn123456, {
    compute.and.show("(123456)")
  })
  observeEvent(input$btn165432, {
    compute.and.show("(165432)")
  })
  observeEvent(input$btn135_246, {
    compute.and.show("(135)(246)")
  })
  observeEvent(input$btn153_264, {
    compute.and.show("(153)(264)")
  })
  observeEvent(input$btn14_25_36, {
    compute.and.show("(14)(25)(36)")
  })
  observeEvent(input$btn12_36_45, {
    compute.and.show("(12)(36)(45)")
  })
  observeEvent(input$btn16_25_34, {
    compute.and.show("(16)(25)(34)")
  })
  observeEvent(input$btn14_23_56, {
    compute.and.show("(14)(23)(56)")
  })
  observeEvent(input$btn26_35, {
    compute.and.show("(26)(35)")
  })
  observeEvent(input$btn13_46, {
    compute.and.show("(13)(46)")
  })
  observeEvent(input$btn15_24, {
    compute.and.show("(15)(24)")
  })
  
  # The reset button clears the output and reinitializes the product
  observeEvent(input$reset, {
    line.out <<- ""
    result.list <<- ""
    product <<- "I"
    output$line.out <- renderUI(HTML(line.out))
    output$results<-renderUI(HTML(result.list))
  })
  
  # Event handlers for the subgroup buttons
  mark_btns <- function() {
    A4DF[, 3] <<- rep("gray90", N)
    mark.subgroup()
    showButtons()
  }
  observeEvent(input$btnC1, {
    subgroup <<- c(1)
    mark_btns()
  })
  observeEvent(input$btnC2_1, {
    subgroup <<- c(1, 6)
    mark_btns()
  })
  observeEvent(input$btnC2_2, {
    subgroup <<- c(1, 7)
    mark_btns()
  })
  observeEvent(input$btnC2_3, {
    subgroup <<- c(1, 9)
    mark_btns()
  })
  observeEvent(input$btnC2_4, {
    subgroup <<- c(1, 8)
    mark_btns()
  })
  observeEvent(input$btnC2_5, {
    subgroup <<- c(1, 10)
    mark_btns()
  })
  observeEvent(input$btnC2_6, {
    subgroup <<- c(1, 11)
    mark_btns()
  })
  observeEvent(input$btnC2_7, {
    subgroup <<- c(1, 12)
    mark_btns()
  })
  observeEvent(input$btnC3_1, {
    subgroup <<- c(1, 4, 5)
    mark_btns()
  })
  observeEvent(input$btnC6_1, {
    subgroup <<- c(1, 2, 3, 4, 5, 6)
    mark_btns()
  })
  observeEvent(input$btnS3_1, {
    subgroup <<- c(1, 4, 5, 7, 8, 9)
    mark_btns()
  })
  observeEvent(input$btnS3_2, {
    subgroup <<- c(1, 4, 5, 10, 11, 12)
    mark_btns()
  })
  observeEvent(input$btnV4_1, {
    subgroup <<- c(1, 6, 7, 12)
    mark_btns()
  })
  observeEvent(input$btnV4_2, {
    subgroup <<- c(1, 6, 8, 11)
    mark_btns()
  })
  observeEvent(input$btnV4_3, {
    subgroup <<- c(1, 6, 9, 10)
    mark_btns()
  })
  observeEvent(input$btnD6, {
    subgroup <<- c(1:12)
    mark_btns()
  })
  
  # Function for color coding cosets
  coset <- function (left_right) {
    if (identical(subgroup, numeric(0))) {
      showButtons()
    } else {
      mark.subgroup()
      idx = 1   # Index into the color list -- one for each coset
      # Keep creating cosets as long as there are elements that are still gray
      while(length(which(A4DF$color == "gray90") > 0)) {
        # Find the first unassigned group element
        in.coset <- which(A4DF$color == "gray90")[1]
        # Generate its left coset and put a new color on the buttons
        for (j in 1:N) {
          if(j %in% subgroup) {
            if (left_right == "L") {
              element <- Perm.multiply(A4DF[in.coset, 2], A4DF[j, 2])
            } else {
              element <- Perm.multiply(A4DF[j, 2], A4DF[in.coset, 2])
            }
            k <- which(A4DF[, 2] == element)
            A4DF[k, 3] <<- color.list[idx]
          }
        }
        idx <- idx + 1
      }
      showButtons()
    }
  }
  
  # Event handler for left cosets
  observeEvent(input$btnLC, {
    coset("L")
  })
  
  # Right cosets work the same way
  observeEvent(input$btnRC, {
    coset("R")
  })
  
  observeEvent(input$btnmark, {
    conjugating <<- TRUE
    output$conjmsg <- renderUI("Select an element of the group.")
  })
  
  # Instructions after user clicks Generator a and Generator b
  generate_ab <- function(ab) {
    if (ab == "a") generating <<- 1 else generating <<- 2
    A4DF[, 3] <<- rep("gray90", N)
    showButtons()
    output$genmsg <- renderUI(paste0("Select an element of the group for
                                     generator ",
                                     if (ab == "a") "a." else "b."))
  }
  
  observeEvent(input$btnmarkgena, {
    generate_ab("a")
  })
  
  observeEvent(input$btnmarkgenb, {
    generate_ab("b")
  })
  
  # Generate random sequences of generators.
  # If we generate more than half the group, then it's the entire group.
  observeEvent(input$btngen, {
    subgroup <<- numeric(0)
    for (j in 1:(4 * N)) {
      v <- sample(c("a", "b"), sample(7:12, 1), replace = TRUE)
      element <- evaluate(v, gena, genb)
      k <- which(A4DF[, 2] == element)[1]
      if(!(k %in% subgroup)) {
        subgroup <<- c(subgroup, k)
        A4DF[k, 3] <<- "yellow"
      }
      # If subgroup has more than N/2 elements, it's the entire group
      if (length(subgroup) > N/2) {
        subgroup <<- 1:N
        break
      } 
    }
    mark_btns()
    output$genmsg <- 
      renderUI(paste0("The subgroup generated by ", gena, " and ", genb,
                      " is now yellow."))
  })
  
  # Clear button
  observeEvent(input$btnclear, {
    subgroup <<- numeric(0)
    generating <<- 0
    gena <<- "I"
    genb <<- "I"
    mark_btns()
    output$genmsg <- renderUI("")
    output$conjmsg <- renderUI("")
  })
  
  # Conjugate Subgroup
  observeEvent(input$btnconj, {
    subgroup_c <<- numeric(0)
    aInv <- Perm.inverse(a)
    A4DF[, 3] <<- rep("gray90", N)
    for (j in 1:N) {
      if (j %in% subgroup) {
        element <- Perm.conjugate(a, A4DF[j, 2])
        k <- which(A4DF[, 2] == element)[1]
        A4DF[k, 3] <<- "pink"
        subgroup_c <<- c(subgroup_c, k)
      }
    }
    showButtons()
    output$conjmsg <- renderUI(paste0("The subgroup ", a, "H", aInv,
                                      " is now pink."))
    subgroup <<- subgroup_c
  })
  
  # Formatting for subscripts
  observeEvent(input$formatting, {
    subs_title <- function(subgroup_type, num, singular_pl = "s") {
      renderUI(HTML(paste0(subgroup_type, "<sub>&thinsp;", num,
                           "</sub> Subgroup", if (singular_pl == "p") "s")))
    }
    
    subs_btn <- function(subgroup_type, num) {
      renderUI(HTML(paste0("Show ", subgroup_type, "<sub>&thinsp;", num,
                           "</sub>")))
    }
    
    subs <- input$formatting == "Use Subscripts"
    
    output$subsc1 <-
      if (subs) subs_title("C", "1") 
    else renderUI(HTML("C1 Subgroup"))
    output$showc1 <-
      if (subs) subs_btn("C", "1")
    else renderUI(HTML("Show C1"))
    
    output$subsc2 <-
      if (subs) subs_title("C", "2", "p") 
    else renderUI(HTML("C2 Subgroups"))
    output$showc21 <- output$showc22 <- output$showc23 <- output$showc24 <- output$showc25 <- output$showc26 <- output$showc27 <-
      if (subs) subs_btn("C", "2")
    else renderUI(HTML("Show C2"))
    
    output$subsc3 <-
      if (subs) subs_title("C", "3", "p")
    else renderUI(HTML("C3 Subgroups"))
    output$showc31 <-
      if (subs) subs_btn("C", "3")
    else renderUI(HTML("Show C3"))

    output$subsc6 <-
      if (subs) subs_title("C", "6") 
    else renderUI(HTML("C6 Subgroup"))
    output$showc6 <-
      if (subs) subs_btn("C", "6")
    else renderUI(HTML("Show C6"))
    
    output$subss3 <-
      if (subs) subs_title("S", "3") 
    else renderUI(HTML("S3 Subgroup"))
    output$shows31 <- output$shows32 <-
      if (subs) subs_btn("S", "3")
    else renderUI(HTML("Show S3"))
    
    output$subsv4 <-
      if (subs) subs_title("V", "4") 
    else renderUI(HTML("V4 Subgroup"))
    output$showv41 <- output$showv42 <-output$showv43 <-
      if (subs) subs_btn("V", "4")
    else renderUI(HTML("Show V4"))
    
    output$subsd6 <-
      if (subs) subs_title("D", "6") 
    else renderUI(HTML("D6 Subgroup"))
    output$showd6 <-
    if (subs) subs_btn("D", "6")
    else renderUI(HTML("Show D6"))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
