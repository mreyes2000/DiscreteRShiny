# buttonrows.R

# Functions to create neatly-spaced rows of buttons
buttonRow1 <- function(input_ids, labels, btn_style) {
  div(
    align = "center",
    withTags(
      table(
        tr(actionButton(input_ids[1], labels[1], style = btn_style)),
      )
    )
  )
}

buttonRow2 <- function(input_ids, labels, btn_style) {
  div(
    align = "center",
    withTags(
      table(
        width = "100%",
        td(width = "50%", align = "center",
           actionButton(input_ids[1], labels[1], style = btn_style)),
        td(width = "50%", align = "center",
           actionButton(input_ids[2], labels[2], style = btn_style))
      )
    )
  )
}

buttonRow3 <- function(input_ids, labels, btn_style) {
  div(
    align = "center",
    withTags(
      table(width = "100%",
            td(width = "33.33%", align = "center",
               actionButton(input_ids[1], labels[1], style = btn_style)),
            td(width = "33.33%", align = "center",
               actionButton(input_ids[2], labels[2], style = btn_style)),
            td(width = "33.33%", align = "center",
               actionButton(input_ids[3], labels[3], style = btn_style))
      )
    )
  )
}

# Functions to create neatly-spaced rows of placeholders for buttons
controlRow1 <- function(ctrl_id) {
  div(align = "center", withTags(table(tr(uiOutput(ctrl_id)))))
}

controlRow2 <- function(ctrl_ids) {
  div(
    align = "center",
    withTags(
      table(
        width = "100%",
        td(width = "50%", align = "center", uiOutput(ctrl_ids[1])),
        td(width = "50%", align = "center", uiOutput(ctrl_ids[2]))
      )
    )
  )
}

controlRow3 <- function(ctrl_ids) {
  div(
    align = "center",
    withTags(
      table(
        width = "100%",
        td(width = "33.33%", align = "center", uiOutput(ctrl_ids[1])),
        td(width = "33.33%", align = "center", uiOutput(ctrl_ids[2])),
        td(width = "33.33%", align = "center", uiOutput(ctrl_ids[3])),
      )
    )
  )
}

controlRow4 <- function(ctrl_ids) {
  div(
    align = "center",
    withTags(
      table(
        width = "100%",
        td(width = "25%", align = "center", uiOutput(ctrl_ids[1])),
        td(width = "25%", align = "center", uiOutput(ctrl_ids[2])),
        td(width = "25%", align = "center", uiOutput(ctrl_ids[3])),
        td(width = "25%", align = "center", uiOutput(ctrl_ids[4])),
      )
    )
  )
}
