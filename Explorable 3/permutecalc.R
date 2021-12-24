# permute
# 
# Library for a few string-related functions:
library(stringr)

# Returns individual mappings, perm(x)
Perm.apply <- function(x, perm) {
  # If x is not in the permutation, return x
  if(!str_detect(perm, x)) return(x)
  # Otherwise locate it and find the location of the next symbol in the
  # permutation
  pos <- str_locate(perm, x)
  # To extract a character, get the next one-character substring
  nextch <- str_sub(perm, pos + 1, pos + 1)
  # If it is not a special character like "(" or ")", return the string
  if(nextch != ")") return(nextch)
  # Otherwise back up to find the number at the start of the cycle and return
  # that instead
  while(str_sub(perm, pos - 1, pos - 1) != "(")  pos <- pos - 1
  return(str_sub(perm, pos, pos))
}

# Converts individual mappings to cycle notation
Perm.cycle.convert <- function(fval) {
  # Build the answer as a vector of characters
  ans <- c("")
  least_elt <- FALSE
  for (i in 1:length(fval)) {
    # If the symbol is unchanged or is already in a cycle, do nothing
    # Start a new cycle if you reach a number that you have already added to
    # your cycle
    # Otherwise keep tracing through the cycle until you find the end
    if (!least_elt & i != as.numeric(fval[i]) & (!(fval[i] %in% ans))) {
      least_elt <- TRUE; ans <- c(ans, "(", i)
    }
    if (least_elt) {
      while (!(fval[i] %in% ans)) {
        ans <- c(ans, fval[i]); i <- as.numeric(fval[i])
      }
      least_elt <- FALSE
      # Close the cycle when it returns to its starting character
      ans <- c(ans, ")")
    }
  }
  # Collapse the vector of cycles into a string and return it
  if(identical(ans, "")) return("I") else return(paste(ans, collapse = ""))
}

# Helper function that covers the special case for the identity "I"
# aI = Ia = a, bI = Ib = b
# If one permutation is the identity, just return the other
I_test <- function (a, b) {
  if(a == "I") return(b); if(b == "I") return(a)
}

# Computes the product ab of two permutations of the symbols "1" through "9"
Perm.multiply <- function(a,b){
  I_test(a, b)
  # Make a vector of the function outputs
  ans <- c("")
  for (i in 1:9) {
    ans[i] <- Perm.apply(Perm.apply(as.character(i), b), a)
  }
  # Generate cycle notation and return the permutation
  return(Perm.cycle.convert(ans))
}

# Helper function that does most of Perm.powerString() and Perm.inverse()
power <- function(perm) {
  if (perm == "I" | str_detect(perm, regex("[(][1-9][)]"))) {
    return(list("I", "I"))
  }
  ans <- pow <- pow_old <- perm
  while (pow != "I") {
    pow_old <- pow
    pow <- Perm.multiply(perm, pow)
    ans <- c(ans, pow)
  }
  return(list(ans, pow_old))
}

# Makes a list of powers separated by HTML line breaks
Perm.powerString <- function(perm) {
  return(paste(power(perm)[[1]], collapse = "<br/>"))
}

# Finds the inverse of perm
Perm.inverse <- function(perm) {
  # One way to finds the inverse is by taking powers of a permutation, then
  # stopping when the next power is the identity permutation
  return(power(perm)[[2]])
  
}

#Forms the conjugate aba^(-1)
Perm.conjugate <- function(a, b) {
  return(Perm.multiply(a, Perm.multiply(b, Perm.inverse(a))))
}
