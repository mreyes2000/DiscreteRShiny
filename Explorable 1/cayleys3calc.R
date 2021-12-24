#S3 in terms of generators and relations



CS3.makeDataFrame <- function(num) {
  if (num == 3) {
    lol <- CS3.makeTriaFrame()
  }
  if (num == 4){
    lol <- CS3.makeSquareFrame()
  }
  if (num == 5){
    lol <- CS3.makePentaFrame()
  }
  if (num == 6){
    lol <- CS3.makeHexaFrame()
  }
  return(lol)
}

CS3.makeSquareFrame <- function() {
  A <- c(0.5, 3.5)
  B <- c(1, 3)
  N <- 8
  DF <- data.frame(x=numeric(N),y=numeric(N),rdest=numeric(N),
                   fdest=numeric(N),label=rep("",N),str=rep("",N),
                   color =rep("",N),perm=rep("",N),
                   stringsAsFactors = FALSE)
  DF[1,]<-list(A[1],A[1],2,5,"1","I","lightgray","")
  DF[2,]<-list(A[2],A[1],3,6,"2","r","lightgray","")
  DF[3,]<-list(A[2],A[2],4,7,"3","rr","lightgray","")
  DF[4,]<-list(A[1],A[2],1,8,"4","rrr","lightgray","")
  DF[5,]<-list(B[1],B[1],8,1,"5","f","lightgray","")
  DF[6,]<-list(B[2],B[1],5,2,"6","fr","lightgray","")
  DF[7,]<-list(B[2],B[2],6,3,"7","frr","lightgray","")
  DF[8,]<-list(B[1],B[2],7,4,"8","frrr","lightgray","")
  
  return(DF)
}

CS3.makeTriaFrame <- function() {
  #Calculate the coordinates, placing the centroids of both triangles at the origin
  #The large outside triangle
  A <- c(0.5,0.5)
  B <- c(2,0.5+1.5*sqrt(3))
  F <- c(3.5,0.5)
  #The small inside triangle
  C <- c(1,0.75)
  D <- c(2, 0.75 + sqrt(3))
  E <- c(3,0.75)
  N <- 6
  DF <- data.frame(x=numeric(N),y=numeric(N),rdest=numeric(N),
                   fdest=numeric(N),label=rep("",N),str=rep("",N),
                   color =rep("",N),perm=rep("",N),
                   stringsAsFactors = FALSE)
  DF[1,]<-list(A[1],A[2],2,4,"1","I","lightgray","")
  DF[2,]<-list(F[1],F[2],3,5,"2","r","lightgray","")
  DF[3,]<-list(B[1],B[2],1,6,"3","rr","lightgray","")
  DF[4,]<-list(C[1],C[2],6,1,"4","f","lightgray","")
  DF[5,]<-list(E[1],E[2],4,2,"5","fr","lightgray","")
  DF[6,]<-list(D[1],D[2],5,3,"6","frr","lightgray","")
  
  
  
  return(DF)
}

CS3.makePentaFrame <- function() {
  A <- c(0.8, 0.2)
  B <- c(3.2, 0.2)
  C <- c(3.2+2.4*cos(2*pi/5), 0.2+2.4*sin(2*pi/5))
  D <- c(3.2+2.4*cos(2*pi/5) - 2.4*cos(pi/5), 0.2+2.4*sin(2*pi/5) + 2.4*sin(pi/5))
  E <- c(0.8-2.4*cos(2*pi/5), 0.2+2.4*sin(2*pi/5))
  F <- c(1.2, 0.7)
  G <- c(2.8, 0.7)
  H <- c(2.8 + 1.6*cos(2*pi/5), 0.7+1.6*sin(2*pi/5))
  I <- c(2.8 + 1.6*cos(2*pi/5) - 1.6*cos(pi/5), 0.7+1.6*sin(2*pi/5) + 1.6*sin(pi/5))
  J <- c(1.2 -1.6*cos(2*pi/5), 0.7+1.6*sin(2*pi/5))
  
  N <- 10
  DF <- data.frame(x=numeric(N),y=numeric(N),rdest=numeric(N),
                   fdest=numeric(N),label=rep("",N),str=rep("",N),
                   color =rep("",N),perm=rep("",N),
                   stringsAsFactors = FALSE)
  DF[1,]<-list(A[1],A[2],2,6,"1","I","lightgray","")
  DF[2,]<-list(B[1],B[2],3,7,"2","r","lightgray","")
  DF[3,]<-list(C[1],C[2],4,8,"3","rr","lightgray","")
  DF[4,]<-list(D[1],D[2],5,9,"4","rrr","lightgray","")
  DF[5,]<-list(E[1],E[2],1,10,"5","rrrr","lightgray","")
  DF[6,]<-list(F[1],F[2],10,1,"6","f","lightgray","")
  DF[7,]<-list(G[1],G[2],6,2,"7","fr","lightgray","")
  DF[8,]<-list(H[1],H[2],7,3,"8","frr","lightgray","")
  DF[9,]<-list(I[1],I[2],8,4,"9","frrr","lightgray","")
  DF[10,]<-list(J[1],J[2],9,5,"10","frrrr","lightgray","")
  
  return(DF)
}

CS3.makeHexaFrame <- function() {
  A <- c(1, 3)
  B <- c(0.2, 0.2+2*sqrt(3))
  C <- c(0, 4, 0.2+sqrt(3))
  D <- c(1.3, 2.7)
  E <- c(0.2+sqrt(3) - 0.7*sqrt(3), 0.2+sqrt(3) + 0.7*sqrt(3))
  F <- c(0.6, 3.4, 0.2+sqrt(3))
  
  N <- 12
  DF <- data.frame(x=numeric(N),y=numeric(N),rdest=numeric(N),
                   fdest=numeric(N),label=rep("",N),str=rep("",N),
                   color =rep("",N),perm=rep("",N),
                   stringsAsFactors = FALSE)
  DF[1,]<-list(A[1],B[1],2,7,"1","I","lightgray","")
  DF[2,]<-list(A[2],B[1],3,8,"2","r","lightgray","")
  DF[3,]<-list(C[2],C[3],4,9,"3","rr","lightgray","")
  DF[4,]<-list(A[2],B[2],5,10,"4","rrr","lightgray","")
  DF[5,]<-list(A[1],B[2],6,11,"5","rrrr","lightgray","")
  DF[6,]<-list(C[1],C[3],1,12,"6","rrrrr","lightgray","")
  DF[7,]<-list(D[1],E[1],12,1,"7","f","lightgray","")
  DF[8,]<-list(D[2],E[1],7,2,"8","fr","lightgray","")
  DF[9,]<-list(F[2],F[3],8,3,"9","frr","lightgray","")
  DF[10,]<-list(D[2],E[2],9,4,"10","frrr","lightgray","")
  DF[11,]<-list(D[1],E[2],10,5,"11","frrrr","lightgray","")
  DF[12,]<-list(F[1],F[3],11,6,"12","frrrrr","lightgray","")
  
  return(DF)
}

#Hace arrows a medias
myArrow <- function(x0, y0, x1, y1, cut = 1, ...){
  x.new <- (1 - cut) * x0 + cut * x1
  y.new <- (1 - cut) * y0 + cut * y1
  # segments(x0, y0, x1, y1, ...)
  arrows(x0, y0, x.new, y.new, ...)
}

CS3.drawGraph <- function(DF, state) {
  par(mar=c(.1,.1,.1,.1))  #narrow margins
  plot(NULL,xlim = c(0,4),ylim = c(0,4),axes = FALSE, asp = 1)
  
  labels <- list(DF$label, DF$perm, DF$str)

  x1 <- DF[DF$rdest,1]
  y1 <- DF[DF$rdest,2]

  for (i in 1:length(DF$x)){
    myArrow(DF$x[i], DF$y[i], x1[i], y1[i], cut = 0.5, col = 'navy', length = 0.15)
  }
  
  arrows(DF$x,DF$y,x1,y1,col='navy',length = 0)
  x1 <- DF[DF$fdest,1]
  y1 <- DF[DF$fdest,2]
  segments(DF$x,DF$y,x1,y1,col = "orange", lwd = 2)
  points(DF$x,DF$y, pch = 21, bg = DF$color, col = 'black', cex=2)
  text(DF$x+0.2,DF$y+0.1, unlist(labels[as.integer(state)]),cex = 1.5)
}

a <- c(1,2,3)
a[2]

CS3.multiply <- function(a,b, dn) {
  x <- paste(a,b,sep="")
  repeat{
    y <- x
    x <- sub(paste(rep("r", dn), collapse= ""),"",x)
    x <- sub("ff","",x)
    x <- sub("rf",paste("f", paste(rep("r", dn), collapse= ""), sep= ""),x)
    if (y == x)
      break
  }
  return(x)
}
#CS3.multiply("fr","frr")



CS3.power <- function(a,n){
  x <-a
  while (n>1){
    x <- CS3.multiply(x,a)
    n <- n-1
  }
  return(x)
}

#This function is overloaded. x can be an index or a string like "rrfr"
CS3.markVertex <- function(DF,x,color) {
  if (is.character(x)){
    x <- CS3.multiply(x,"")
    x <- which(DF$str == x)
  }
  DF[x,7] <- color
  return(DF)
} 

source("permutecalc.R")

CS3.makePerm <- function(DF,str, rStr = "(123)", fStr = "(12)"){
  product = "I"
  n <- nchar(str)
  if (n > 0) {
    for (i in (1:nchar(str))){
      g <- substr(str,i,i)
      product <- Perm.multiply(product,ifelse(g=="f",fStr,rStr))
    }
  }
  return (product)
}

CS3.makePerms <- function(DF,rStr = "(123)", fStr = "(12)"){
    for (i in (1:nrow(DF))){
#      DF$color[i] <- "white"
      DF$perm[i] <- CS3.makePerm(DF,DF$str[i],rStr,fStr)
      print(DF$str[i])
      print(DF$perm[i])
    }
  return (DF)
}

CS3.findClosestVertex <- function(x,y,n){
  DF <- CS3.makeDataFrame(n)
  m <- as.integer(n)*2
  distsq <- numeric(m)
  for (i in 1:m){
    distsq[i] <- (DF[i,1]-x)^2+(DF[i,2]-y)^2
  }
  k <- which(distsq == min(distsq))
  stopifnot(length(k)==1)
  return(k)
}


