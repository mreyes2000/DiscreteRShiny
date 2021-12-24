# Assignment 2: permute
#
# <- This hash (#) symbol precedes comments in R. Your computer will ignore
# lines that begin with a hash.
#
# Collaboration is encouraged, but your code should be your own. Please provide
# links your sources (including stack overflow) and make note of any Math S-152
# collaborators.
#
################################################################################
# 
# Collaborators: 
# 
# Sources: 
# 
################################################################################
#
# This file should be located in the Dashboard folder from Canvas that you
# recently redownloaded.
#
#  Dashboard
#  |
#  +-- Permute
#      |
#      +-- app.R
#      +-- permutecalc.R
#
# Below, the places where you will write code have been marked like this:
# ** your code here **
#
# You may find the following functions useful as you program your functions:
# print() # (for testing and debugging)
# identical()
# as.character()
# paste()
# Perm.apply()
# Perm.cycle.convert()
# any function that you have already written
# 
# RDocumentation demonstrates how to use R functions. You can find the
# RDocumentation entries for these functions by searching them on the following
# site: https://www.rdocumentation.org
# 
# In addition, you may find it useful to use the following types of loops:
# if
# if-else
# for
# while
# 
# If you would like more information on loops in R, you may find it worthwhile
# to read the following excerpt from "Mastering Shiny" by Hadley Wickham:
# https://bookdown.org/rdpeng/rprogdatascience/control-structures.html
#
# 
# This assignment has six sub-parts. You should do them in order. If you would
# like to compare your app to a completed version of it, here is a link to a
# demo app: https://jannawithrow2.shinyapps.io/permute/
#
# 1) Read through the code for Perm.apply() and Perm.cycle.convert() until you
# have an solid understanding of how they work.
#
# 2) Write and test code for the function Perm.multiply. (see below)
#
# 3) Write and test code for the function Perm.powerString. (see below)
#
# 4) Write and test code for the function Perm.inverse. (see below)
#
# 5) Write and test code for the function Perm.conjugate. (see below)
#
# 6) When you have completed parts 1-5, run app.R as a final test to make sure
# all of the functions are working and celebrate your creation of a personal
# library of permutation functions. Afterwards, publish your app and create a
# comment at the top of app.R that includes this app's published URL. Then
# submit that URL on Canvas. The only file you will need to edit during parts
# 1-5 is permutecalc.R. 
# 
# If you get stuck at any point, spend at least 10-20 minutes trying to get
# un-stuck before walking away for a bit and coming back later to look at your
# code with fresh eyes. Consult friends and the internet. Also feel free to stop
# by my office hours or message me on Slack. I'll almost always respond within
# 24 hours. If I can't easily answer your question over Slack, I'll ask you to
# join my office hours or offer to set up a Zoom call with you.
#
# Here's a library you'll likely use for a few string-related functions:
library(stringr)




# 1)
# Perm.apply has two inputs and one output.
# x - a single character, passed as a string like "1" or "4"
# perm - a permutation written in cycle notation like "(13)(245)"
# output - a single character, returned as a string like "3" or "5"
#
# Perm.apply(x,perm) returns the result of the permutation perm applied to x.
#
# Example usage:
#
# Perm.apply("5", ("(13)(245)"))
# [1] "2"
#
# Returns perm(x)
Perm.apply <- function(x, perm) {
  
  # If x is not in the permutation, return x.
  if (str_detect(perm, pattern = x, negate = TRUE)) return (x)
  else {
    
    # Otherwise locate x in the permutation, and find the location of the next
    # symbol in the permutation.
    pos_x <- str_locate(perm, pattern = x)
    pos_next <- pos_x[, 1] + 1
    
    # To extract a character, get the next one-character substring.
    char_next <- str_sub(perm, start = pos_next, end = pos_next)
    
    # If it is not a special character like "(" or ")", return the string.
    if (char_next != "(" & char_next != ")")  return (char_next)
    else {
      
      # Otherwise back up to find the number at the start of the cycle
      # and return that instead.
      y <- 0
      while (char_next != "(") {
        y <- y + 1
        char_first <- str_sub(perm, start = pos_next, end = pos_next)
        pos_next <- pos_x - y
        char_next <- str_sub(perm, start = pos_next, end = pos_next)
      }
      return (char_first)
    }
  }
}

# You may find the following lines useful for testing this code. First uncomment
# the lines beginning with "Perm.apply" below. Then place your curser above the
# first line of code (i.e. "library(stringr)"). Click "Run" until your curser is
# below the last test function, "Perm.apply("5",("(13245)"))." Lastly, look at
# the output printed in your Console below and check that the output is correct.
#
# Perm.apply("5", ("(13)(245)"))
# Perm.apply("5", ("(13)(24)"))
# Perm.apply("5", ("(13245)"))
# debug(Perm.apply)
# undebug(Perm.apply)




# Perm.cycle.convert has one input and one output.
# fval - a vector consisting only of non-repeating digits corresponding to a
# permutation, like c("2", "4", "3", "1"), which corresponds to (124)
# output - a permutation written in cycle notation like "(124)"
#
# Perm.cycle.convert(fval) converts the vector fval to cycle notation. The fval
# argument corresponds to the "table" representation of a permutation. That is,
# fval = c("2", "4", "3", "1") means a(1) = 2, a(2) = 4, a(3) = 3, and a(4) = 1.
# This is specific to permutations of the digits 1 through 9, and this function
# will be reused in other apps.
#
# Example usage:
#
# Perm.cycle.convert(c("2", "4", "3", "1"))
# [1] "(124)"
#
# Converts individual mappings to cycle notation
Perm.cycle.convert <- function(fval) {
  
  # Build the answer as a vector of characters.
  ans <- c("")
  
  # Loop through the digits 1:9 in order.
  # If the symbol is unchanged or is already in a cycle, there is nothing to do.
  least_elt <- FALSE
  for (i in 1:length(fval)) {
    if(!least_elt & i != as.numeric(fval[i]) & (!(fval[i] %in% ans))) {
      least_elt <- TRUE
      ans <- c(ans, "(", i)
    }
    # Keep tracing through the cycle until you find the end.
    if (least_elt) {
      while (!(fval[i] %in% ans)) {
        ans <- c(ans, fval[i])
        i <- as.numeric(fval[i])
      }
      # End the cycle and potentially start a new one if you reach a number that
      # you have already added to your cycle.
      least_elt <- FALSE
      ans <- c(ans, ")")
    }
  }
  
  # Collapse the vector of cycles into a string and return it.
  if (identical(ans, "")) return ("I")
  else return (paste(ans, collapse = ""))
}

# You may find the following lines useful for testing this code:
# Perm.cycle.convert(c("2", "4", "3", "1"))
# Perm.cycle.convert(c("2", "1", "3", "4"))
# Perm.cycle.convert(c("2", "1", "4", "3"))
# debug(Perm.cycle.convert)
# undebug(Perm.cycle.convert)





# The remaining functions in permutecalc.R are significantly shorter and simpler
# than the preceding functions.
#
# CHALLENGE: If you have some coding experience, you might find it more
# stylistically elegant to create a couple of helper functions that you can
# reuse later on. For instance, you could create a short function that checks
# whether one of two permutations is the identity. You might also try to code a
# generalized helper function that can be used to do almost all of
# Perm.powerString(), Perm.inverse(), and Perm.conjugate(). Feel free to circle
# back to this idea later on.




# 2)
# Perm.multiply has two inputs and one output.
# a - a permutation written in cycle notation like "(24)(567)"
# b - a permutation written in cycle notation like "(123)(4689)"
# output - a permutation written in cycle notation like "(134)"
#
# Perm.multiply(a,b) returns the result of the permutation product ab (i.e. "b
# followed by a").
#
# Example usage:
#
# Perm.multiply("(123)", "(12)(34)")
# [1] "(134)"
#
# Computes the product ab of two permutations of the symbols "1" through "9"
Perm.multiply <- function(a, b) {
  # Default return value
  # Delete the following lines when you start writing your version of this
  # function:
  
  # Don't forget to include a special case for the identity "I."
  # aI = Ia = a, bI = Ib = b
  # If one permutation is the identity, just return the other.
  # ** your code here **
  
  if (a == "I") return (b)
  if (b == "I") return (a)
  
  # If neither permutation is the identity, make a vector of the function
  # outputs.
  # You may choose to save space/code by using the permutation's order.
  # ** your code here **
  
  n <- c(1:9)
  n_prime_prime <- as.character(c(1:9))
  for (i in n) {
    n_prime_prime[i] <- Perm.apply(Perm.apply(as.character(i), b), a)
  }
  
  # If input and output are equal, return the identity.
  # ** your code here **
  
  if (all(as.character(n) == strtoi(n_prime_prime))) return ("I")

  # Otherwise generate cycle notation, and return that result.
  # ** your code here **
  
  output <- Perm.cycle.convert(n_prime_prime)
  return (output)
}

# You may find the following lines useful for testing your code:
# Perm.multiply("(123)", "(12)(34)")
# Perm.multiply("(12)(34)", "(123)")
# Perm.multiply("(12)(34)", "(15)(23)")
# debug(Perm.multiply)
# undebug(Perm.multiply)
#
# If you programmed Perm.multiply correctly, your code will be vectorizable as
# well:
# vPerm.multiply <- Vectorize(Perm.multiply,c("a","b"))




# 3)
# Perm.powerString has two inputs and one output.
# perm - a permutation written in cycle notation like "(135)"
# output - a list of permutations like "(135)<br/>(153)<br/>I"
#
# Perm.powerString(perm) makes a list of powers or perm separated by HTML line
# breaks ("<br/>").
#
# Example usage:
#
# Perm.powerString("(135)")
# [1] "(135)<br/>(153)<br/>I"
#
# Makes a list of powers separated by HTML line breaks
Perm.powerString <- function(perm) {
  
  # Default return value
  # Delete the following lines when you start writing your version of this
  # function:
  
  # ** your code here **
  
  mutable <- perm
  powers <- perm
  while (mutable != "I") {
    mutable <- Perm.multiply(mutable, perm)
    powers <- paste0(powers, "<br/>", mutable)
  }
  return (powers)
  
}

# You may find the following lines useful for testing your code:
# Perm.powerString("(135)")
# Perm.powerString("(123)(4689)")
# Perm.powerString("I")



# 4)
# Perm.inverse has one input and one output
# perm - a permutation written in cycle notation like "(123)(4689)"
# output - a permutation written in cycle notation like "(132)(4986)"
# 
# Perm.inverse(perm) returns the inverse permutation of perm, denoted perm^(-1).
# You can check your code by confirming that perm*perm^(-1) == I, the identity
# permutation.
#
# Example usage:
#
# Perm.inverse("(123)(4689)")
# [1] "(132)(4986)"
#
# Finds the inverse of perm
Perm.inverse <- function(perm) {
  
  # Default return value
  # Delete the following lines when you start writing your version of this
  # function:
  
  # One way to finds the inverse is by taking powers of a permutation, then
  # stopping when the next power is the identity permutation. You may find the
  # while-loop construction helpful.
  # ** your code here **
  n <- c(1:9)
  n_prime <- as.character(c(1:9))
  
  for (i in n) {
    n_prime[i] <- Perm.apply(as.character(i), perm)
  }
    
  result <- as.character(c(1:9))
  result[strtoi(n_prime)] <- result
  
  output <- Perm.cycle.convert(result)
  return(output)
  
}

# You may find the following lines useful for testing your code:
# Perm.inverse("(123)(4689)")




# 5)
# Perm.conjugate has two inputs and one output.
# a - a permutation written in cycle notation like "(24)(567)"
# b - a permutation written in cycle notation like "(123)(4689)"
# output - a permutation written in cycle notation like "(143)(2789)"
#
# Perm.conjugate(a,b) returns the conjugate aba^(-1), where a^(-1) is the
# inverse of a. In other words, aa^(-1) == I, the identity permutation.
# 
# Example usage:
#
# Perm.conjugate("(24)(567)", "(123)(4689)")
# [1] "(143)(2789)" 
Perm.conjugate <- function(a, b) {
  
  # Default return value
  # Delete the following lines when you start writing your version of this
  # function:
  # ** your code here **
  a_inv <- Perm.inverse(a)
  ab <- Perm.multiply(a, b)
  aba_inv <- Perm.multiply(ab, a_inv)
  
  return (aba_inv)
  
}

# You may find the following lines useful for testing your code:
# Perm.conjugate("(24)(567)", "(123)(4689)")




# 6)
# Run and publish these files to further test out your new functions and see
# them in action! Afterwards, create a comment at the top of app.R that includes
# this app's published URL, then submit that URL on Canvas. Congrats on a job
# well done!
