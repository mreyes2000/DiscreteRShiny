# d3calc.R - Symmetries of the Equilateral Triangle

btn_sidebar <- function(input_id, lbl_str) {
  actionButton(input_id, span(HTML(lbl_str), style = "text-align:center;"),
               style = "width:80px;")
}

D4.makeDataFrame <- function() {
  DF <- data.frame(name = rep("", 8), cfg = rep("", 8),
                   stringsAsFactors = FALSE)
  DF[1, ] <- c("I", "ABCD")
  DF[2, ] <- c("r", "DABC")
  DF[3, ] <- c("r<sup>2</sup>", "CDAB")
  DF[4, ] <- c("r<sup>3</sup>", "BCDA")
  DF[5, ] <- c("s", "BADC")
  DF[6, ] <- c("rs", "CBAD")
  DF[7, ] <- c("r<sup>2</sup>s", "DCBA")
  DF[8, ] <- c("r<sup>3</sup>s", "ADCB")
  return(DF)
}
# BiggsDF <- D3.makeDataFrame()
# BiggsDF

D4.showConfigs <- function(DF) {
  par(mar = c(1, 1, 1, 1))
  plot(NULL, xlim = c(-0.35, 17.65), ylim = c(-1, 3), asp = 1, axes = FALSE)
  for (i in 0:7) {
    # points() determines the positioning of the triangles' vertices.
    # Positive values increase positioning in the right/up direction, and
    # negative values increase positioning in the left/down direction.
    # For instance, the first value in the first vector is used to set the
    # left/right position of the bottom left vertex of each triangle. The first
    # value in the second vector determines the up/down position of the bottom
    # left vertex of each triangle.
    points(c(0, 2, 2, 0, 0) + 3 * i, c(2, 2, 0, 0, 2), type = "l")
    lbl <- strsplit(DF[i + 1, 2], "")[[1]]
    # The text() function below determines the position of the As, Bs, and Cs
    # within the smaller triangles.
    text(c(0, 2, 2, 0) + 0.17 * c(1, -1,-1,1) + 3 * i, c(2, 2, 0, 0)+ 0.17 * c(-1, -1,1,1), lbl)
    # The text() function below determines the position of the i, r, s, x, y,
    # and z labels under the triangles.
    lblz <- c("I", "r", "r^2", "r^3", "s", "rs", "r^2", "r^3s")
    text(1 + 3 * i, -0.5, lblz[i+1])
    # segments() graphs the dotted lines
    # Positive values increase positioning in the right/up direction, and
    # negative values increase positioning in the left/down direction.
    # For example, the first value in each vector is used to determine the
    # display of x's vertical axis of reflection. Respectively, the entries
    # describe the position of that axis' bottom vertex (right/left), bottom
    # vertex (up/down), top vertex (right/left), and top vertex (up/down).
    segments(c(13, 15 - 0.15, 18 - 0.3, 23 + 0.15),
             c(-0.3, -0.15, 1, -0.15),
             c(13, 17 + 0.15, 20+0.3, 21 - 0.15),
             c(2.3, 2.15 , 1, 2 + 0.15), lty = 2)
  }
}
# D3.showConfigs(BiggsDF)

# cfg is a string of symbols, reading clockwise from the top of the triangle
# D3.showTriangle() determines the positioning of the large triangle
D4.showSquare <- function(cfg) {
  par(mar = c(1, 1, 1, 1))
  plot(NULL, xlim = c(-0.35, 2.65), ylim = c(-1, 2), asp = 1, axes = FALSE)
  points(c(0, 2, 2, 0, 0), c(0, 0, 2, 2, 0), type = "l", lwd = 2)
  lbl <- strsplit(cfg, "")[[1]]
  # Here you shift text
  text(c(0, 2, 2, 0) + 0.17 * c(1, -1,-1,1), c(2, 2, 0, 0)+ 0.17 * c(-1, -1,1,1), lbl)
}
# D3.showTriangle("ABC")

# a is one of the Biggs symbols for an operation.

# The return value is the new configuration
D4.apply <- function(a, cfg) {
  v <- strsplit(cfg, "")[[1]]   # select first component of list
  w <- switch(a,
              "I" = v,
              "r" = c(v[4], v[1], v[2], v[3]),
              "r<sup>2</sup>" = c(v[3], v[4], v[1], v[2]),
              "r<sup>3</sup>" = c(v[2], v[3], v[4], v[1]),
              "s" = c(v[2], v[1], v[4], v[3]),
              "rs" = c(v[3], v[2], v[1], v[4]),
              "r<sup>2</sup>s" = c(v[4], v[3], v[2], v[1]),
              "r<sup>3</sup>s" = c(v[1], v[4], v[3], v[2])
  )
  s <- paste(w, collapse = "") 
  return(s)
}
# D3.apply("r", "BCA")


D4.multiply <- function(DF, b, a) {
  # Look up the name, which occurs once and only once
  idx <- which.max(DF$name == a)
  # Find the corresponding configuration
  cfg <- DF$cfg[idx]
  # Apply the group operation to it
  newcfg <- D4.apply(b, cfg)
  # Look up the configuration
  idx <- which.max(DF$cfg == newcfg)
  return (DF$name[idx])
}
# D3.multiply(BiggsDF, "r", "r")

# To use D3.multiply() with outer(), we must vectorize it.
vD4.multiply <- Vectorize(D4.multiply, c("a", "b"))
