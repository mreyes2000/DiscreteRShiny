# a4calc.R

makeA4data <- function(output) {
  N <- 12 # Number of elements in the group
  DF <- data.frame(button = character(N), perm = character(N),
                   color = character(N), stringsAsFactors = FALSE)
  DF[1, ] <- c("btnI", "I", "gray90")
  DF[2, ] <- c("btn123456", "(123456)", "gray90")
  DF[3, ] <- c("btn165432", "(165432)", "gray90")
  DF[4, ] <- c("btn135_246", "(135)(246)", "gray90")
  DF[5, ] <- c("btn153_264", "(153)(264)", "gray90")
  DF[6, ] <- c("btn14_25_36", "(14)(25)(36)", "gray90")
  DF[7, ] <- c("btn12_36_45", "(12)(36)(45)", "gray90")
  DF[8, ] <- c("btn14_23_56", "(14)(23)(56)", "gray90")
  DF[9, ] <- c("btn16_25_34", "(16)(25)(34)", "gray90")
  DF[10, ] <- c("btn26_35", "(26)(35)", "gray90")
  DF[11, ] <- c("btn13_46", "(13)(46)", "gray90")
  DF[12, ] <- c("btn15_24", "(15)(24)", "gray90")
  return(DF)
}

# DF <- makeA4data()