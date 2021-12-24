#A5 in terms of generators and relations


# If I knew what CSS was this part might be less ugly.

# Bold serif fonts for headings, like "Cayley Graph" or "Permutations"
kahua.s <- "font-weight: bold; font-family: 'Liberation Serif', 'Nimbus Roman No 9 L', 'Hoefler Text', 'Times', 'Times New Roman', serif"

# Decent-sized sans serif fonts for subheadings like "Rewrite Rules" or "Defining Relations"
kahua   <- "font-size:large; font-family: 'Liberation Sans', 'Nimbus Sans L', 'Lucida Sans Unicode', 'Lucida Grande', 'FreeSans', 'Helvetica Neue', 'Helvetica', sans-serif"

# Formatting on the generators; 'r' gets royalblue, 'f' gets darkorange, both fixed-width fonts
kahuaf <- "<span style = \"color:darkorange; font-weight: bold; font-family: 'Liberation Mono', 'Nimbus Mono L', 'DejaVu Mono', 'Bitstream Sans Mono', 'Bitstream Vera Mono', 'Lucida Console', 'Andale Mono', 'Courier New', monospace\">f</span>"
kahuar <- "<span style = \"color:royalblue; font-weight: bold; font-family: 'Liberation Mono', 'Nimbus Mono L', 'DejaVu Mono', 'Bitstream Sans Mono', 'Bitstream Vera Mono', 'Lucida Console', 'Andale Mono', 'Courier New', monospace\">r</span>"
identiteit <- "<span style = \"font-weight: bold; font-family: 'Liberation Serif', 'Nimbus Roman No 9 L', 'Hoefler Text', 'Times', 'Times New Roman', serif\">I</span>"
# here's some fixed-width whitespace so that we can kludge in some text alignment between lines
whitespace <- "<span style = \"font-weight: bold; font-family: 'Liberation Mono', 'Nimbus Mono L', 'DejaVu Mono', 'Bitstream Sans Mono', 'Bitstream Vera Mono', 'Lucida Console', 'Andale Mono', 'Courier New', monospace\">&nbsp;</span>"

# sadly rep(whitespace,n) does not do what I want so here is a silly function
# If I ever had delusions of being a programmer this should put those notions to rest
# If I remembered how sprintf() works this could be much cleaner
Leerraum <- function(n){ #it takes an integer n and returns a string of n non-breaking spaces
    peaCrabs <- ""
    for (i in 1:n){
        peaCrabs <- paste0(peaCrabs,whitespace)
    }
    return(peaCrabs)
}

# Let's see if we can make a function that takes a word made out of r and f
# and returns some HTML we can render for a prettier word display
# 'sana' is Finnish for 'group element expressed in terms of generato elements'

CA5.manifestWord <- function(sana){ # we get ourselves a string of 'r's and 'f's
    separo <- unlist(strsplit(sana, split="")) # we explode the string 'sana' into a vector of single characters 'separo'
    slovo <- ""
    if (length(separo)==0) return(identiteit) # in the dataframe the identity element is "" but when we display it we call it "I"
    for (i in 1:length(separo)){
        slovo <- paste0(slovo,ifelse(separo[i]=="f",kahuaf,kahuar)) # and we paste in the html over and over
    }
    return(slovo)
}

# here we start doing math

CA5.makeDataFrame <- function() {
    r1 <- 3.7 #distance to outer pentagon
    V <- matrix(nrow = 60, ncol = 2)
    V[1:5,1] <- r1*(cos((0:4)*0.4*pi-0.5*pi))
    V[1:5,2] <- r1*(sin((0:4)*0.4*pi-0.5*pi))
    r2 <- 3 #distance to inner pentagon
    V[6:10,1] <- r2*(cos((0:4)*0.4*pi-0.5*pi))
    V[6:10,2] <- r2*(sin((0:4)*0.4*pi-0.5*pi))
    #Interpolate
    V[11:15,] <- (5*V[6:10,]+V[c((7:10),6),])/6
    V[16:20,] <- (V[6:10,]+5*V[c((7:10),6),])/6
    s <- V[11:15,]-V[6:10,]
    V[21:25,] <- V[c(17:20,16),]-s
    V[26:30,] <- V[c(15,11:14),]+s
    r3 <- 0.5 #distance to central pentagon
    V[31:35,1] <- -r3*(cos((0:4)*0.4*pi-0.5*pi))
    V[31:35,2] <- -r3*(sin((0:4)*0.4*pi-0.5*pi))
    r4 <- 1 #distance to next pentagon
    V[36:40,1] <- -r4*(cos((0:4)*0.4*pi-0.5*pi))
    V[36:40,2] <- -r4*(sin((0:4)*0.4*pi-0.5*pi))
    V[41:45,] <- V[c(37:40,36),]-s
    V[46:50,] <- V[c(36:40),]+s
    V[51:55,] <- V[c(50,46:49),]+s
    V[56:60,] <- V[c(42:45,41),]-s

    # magic numbers; the function tables for r and f
    rdest <- c(2,3,4,5,1,20,16,17,18,19,6,7,8,9,10,25,21,22,23,24,29,30,26,27,28,15,11,12,13,14,35,31,32,33,34,45,41,42,43,44,60,56,57,58,59,36,37,38,39,40,50,46,47,48,49,54,55,51,52,53)
    fdest <- c(6,7,8,9,10,1,2,3,4,5,16,17,18,19,20,11,12,13,14,15,58,59,60,56,57,54,55,51,52,53,36,37,38,39,40,31,32,33,34,35,48,49,50,46,47,44,45,41,42,43,28,29,30,26,27,24,25,21,22,23)
    
    # 60
    N <- 60
    
    # Initialize the dataframe
    # Each row is a node. x and y are the x-y coordinates on the diagram.
    # If you apply r to the node, you go to rdest, and if you apply f to the node, you go to fdest
    # rsrc is going to be filled with the vertex from which an r operation will take you to this vertex
    # label is just writing the number
    # sana is going to be filled with the standard r-f word for each vertex
    # I'm using 'node' and 'vertex' interchangably
    # color is hopefully self-explanatory. It will default to light gray, but may get colored differently
        # when we start multiplying words or permutations
    # perm is going to be filled with the cycle-form permutation for the five edge colors
    # later I might add columns to indicate the best place to put labels for each node
    # xoffset1, yoffset1 for a spot in the middle of the biggest angle,
    # xoffset2, yoffset2 for a spot in the next biggest angle
    # and I suppose a third x-y pair in case we want to have 'label', 'sana', and 'perm' all
        # displayed on the diagram at once, although that would be pretty busy
    DF <- data.frame(x=V[1:N,1],
                     y=V[1:N,2],
                     rdest=rdest,
                     fdest=fdest,
                     rsrc=integer(60),
                     label=as.character(1:N),
                     sana=rep("",N),
                     color =rep("lightgray",N),
                     perm=rep("",N),
                     stringsAsFactors=FALSE)
    
    
    # insane code for creating the canonical sequence of r and f to label each vertex of the graph.
    # Should probably come up with a more elegant way to do this
    while (min(nchar(DF$sana)) == 0){
        for (indeksi in 1:60){
            if (indeksi == 1 | nchar(DF$sana[indeksi])>0){
                rtarget <- DF$rdest[indeksi]
                rsana <- paste("r",DF$sana[indeksi],sep="")
                ftarget <- DF$fdest[indeksi]
                fsana <- paste("f",DF$sana[indeksi],sep="")
                if (nchar(DF$sana[rtarget])==0){
                    DF$sana[rtarget] <- rsana
                }
                if (nchar(DF$sana[ftarget])==0){
                    DF$sana[ftarget] <- fsana
                }
            }
        }
    }
    
    # and now that we have some sort of sequence of r and f that gets to each node, multiply by I
    # because that will put everything into our standard form.
    for (xarisxis in 1:60){
        DF$sana[xarisxis] <- CA5.multiply(DF$sana[xarisxis],"")
    }
    
    # Time to fill in the rsrc column
    for (xarisxis in 1:60){
        DF$rsrc[xarisxis]<-which(DF$rdest==xarisxis)
    }
    
    
    
#For the sake of nostalgia, here's the old code for building the dataframe
    # DF <- data.frame(x=numeric(6),y=numeric(6),rdest=numeric(6),
    #                  fdest=numeric(6),label=rep("",6),sana=rep("",6),
    #                  stringsAsFactors = FALSE)
    # DF[1,]<-c(V[1,1],V[1,2],2,6,"1","")
    # DF[2,]<-c(V[2,1],V[2,2],3,7,"2","")
    # DF[3,]<-c(V[3,1],V[3,2],4,8,"3","")
    # DF[4,]<-c(V[4,1],V[4,2],5,9,"4","")
    # DF[5,]<-c(V[5,1],V[5,2],1,10,"5","")
    # DF[6,]<-c(V[6,1],V[6,2],20,1,"6","")
    # DF[7,]<-c(V[7,1],V[7,2],16,2,"7","")
    # DF[8,]<-c(V[8,1],V[8,2],17,3,"8","")
    # DF[9,]<-c(V[9,1],V[9,2],18,4,"9","")
    # DF[10,]<-c(V[10,1],V[10,2],19,5,"10","")
    # DF[11,]<-c(V[11,1],V[11,2],6,16,"11","")
    # DF[12,]<-c(V[12,1],V[12,2],7,17,"12","")
    # DF[13,]<-c(V[13,1],V[13,2],8,18,"13","")
    # DF[14,]<-c(V[14,1],V[14,2],9,19,"14","")
    # DF[15,]<-c(V[15,1],V[15,2],10,20,"15","")
    # DF[16,]<-c(V[16,1],V[16,2],25,11,"16","")
    # DF[17,]<-c(V[17,1],V[17,2],21,12,"17","")
    # DF[18,]<-c(V[18,1],V[18,2],22,13,"18","")
    # DF[19,]<-c(V[19,1],V[19,2],23,14,"19","")
    # DF[20,]<-c(V[20,1],V[20,2],24,15,"20","")
    # DF[21,]<-c(V[21,1],V[21,2],29,58,"21","")
    # DF[22,]<-c(V[22,1],V[22,2],30,59,"22","")
    # DF[23,]<-c(V[23,1],V[23,2],26,60,"23","")
    # DF[24,]<-c(V[24,1],V[24,2],27,56,"24","")
    # DF[25,]<-c(V[25,1],V[25,2],28,57,"25","")
    # DF[26,]<-c(V[26,1],V[26,2],15,54,"26","")
    # DF[27,]<-c(V[27,1],V[27,2],11,55,"27","")
    # DF[28,]<-c(V[28,1],V[28,2],12,51,"28","")
    # DF[29,]<-c(V[29,1],V[29,2],13,52,"29","")
    # DF[30,]<-c(V[30,1],V[30,2],14,53,"30","")
    # DF[31,]<-c(V[31,1],V[31,2],35,36,"31","")
    # DF[32,]<-c(V[32,1],V[32,2],31,37,"32","")
    # DF[33,]<-c(V[33,1],V[33,2],32,38,"33","")
    # DF[34,]<-c(V[34,1],V[34,2],33,39,"34","")
    # DF[35,]<-c(V[35,1],V[35,2],34,40,"35","")
    # DF[36,]<-c(V[36,1],V[36,2],45,31,"36","")
    # DF[37,]<-c(V[37,1],V[37,2],41,32,"37","")
    # DF[38,]<-c(V[38,1],V[38,2],42,33,"38","")
    # DF[39,]<-c(V[39,1],V[39,2],43,34,"39","")
    # DF[40,]<-c(V[40,1],V[40,2],44,35,"40","")
    # DF[41,]<-c(V[41,1],V[41,2],60,48,"41","")
    # DF[42,]<-c(V[42,1],V[42,2],56,49,"42","")
    # DF[43,]<-c(V[43,1],V[43,2],57,50,"43","")
    # DF[44,]<-c(V[44,1],V[44,2],58,46,"44","")
    # DF[45,]<-c(V[45,1],V[45,2],59,47,"45","")
    # DF[46,]<-c(V[46,1],V[46,2],36,44,"46","")
    # DF[47,]<-c(V[47,1],V[47,2],37,45,"47","")
    # DF[48,]<-c(V[48,1],V[48,2],38,41,"48","")
    # DF[49,]<-c(V[49,1],V[49,2],39,42,"49","")
    # DF[50,]<-c(V[50,1],V[50,2],40,43,"50","")
    # DF[51,]<-c(V[51,1],V[51,2],50,28,"51","")
    # DF[52,]<-c(V[52,1],V[52,2],46,29,"52","")
    # DF[53,]<-c(V[53,1],V[53,2],47,30,"53","")
    # DF[54,]<-c(V[54,1],V[54,2],48,26,"54","")
    # DF[55,]<-c(V[55,1],V[55,2],49,27,"55","")
    # DF[56,]<-c(V[56,1],V[56,2],54,24,"56","")
    # DF[57,]<-c(V[57,1],V[57,2],55,25,"57","")
    # DF[58,]<-c(V[58,1],V[58,2],51,21,"58","")
    # DF[59,]<-c(V[59,1],V[59,2],52,22,"59","")
    # DF[60,]<-c(V[60,1],V[60,2],53,23,"60","")
    # 
    # 
    # 
    # DF <-transform(DF,x=as.numeric(x),y=as.numeric(y),
    #                rdest=as.numeric(rdest),fdest=as.numeric(fdest))
    return(DF)
}

uqqw<-integrate(function(x) 1/log(x)+1/(1-x),0,1);xvc<-uqqw$value;cvx<-1-xvc # yup
i_to_the_i <- exp(-pi/2) # i to the i is real and equals sqrt(exp(-pi))


# Let's try to identify the best place to put a label.
# Every vertex has three line segments, one for f,
# and two for r, with one representing V as target and the other V as source.
# We can just find the angles between all three segments
# and declare the biggest angle the best place to put a label.
# Now, for each vertex we find the three cosines
# This is crude and inelegant, and if we were doing more than three vectors we would make it pretty by using matrices

# OK time to draw some graph
CA5.drawGraph <- function(DF, permlabel=FALSE) { # not using the permlabel argument but leaving it in because hope springs eternal
    xr0 <- DF[DF$rsrc,1]; yr0 <- DF[DF$rsrc,2]  # the coordinates for where an r is coming from
    xr1 <- DF[DF$rdest,1];yr1 <- DF[DF$rdest,2] # the coordinates for where r takes us
    xf1 <- DF[DF$fdest,1];yf1 <- DF[DF$fdest,2] # the coordinates for where f takes us
    xv1 <- xr0 - DF$x; yv1 <- yr0 - DF$y # vector components for incoming r vector
    xv2 <- xr1 - DF$x; yv2 <- yr1 - DF$y # vector components for outgoing r vector
    xv3 <- xf1 - DF$x; yv3 <- yf1 - DF$y # vector components for f vector
    norme1 <- sqrt(xv1^2+yv1^2); norme2 <- sqrt(xv2^2+yv2^2);norme3 <- sqrt(xv3^2+yv3^2) # some Euclidean norms
    xv1 <- xv1/norme1; yv1 <- yv1/norme1 # normalize the vectors now
    xv2 <- xv2/norme2; yv2 <- yv2/norme2 # I hope no one but me ever has to debug this
    xv3 <- xv3/norme3; yv3 <- yv3/norme3 # because this is bad even for me
    ks12 <- xv1*xv2+yv1*yv2; ks13<- xv1*xv3+yv1*yv3; ks23 <- xv2*xv3+yv2*yv3 # ksab is the product of vector a with vector b
    for (i in 1:60){ # I should really figure out how to vectorize this
        smks <- min(ks12[i],ks13[i],ks23[i]) # the widest angle has the smallest cosine (not true in general but true for our diagram)
        if (ks12[i]==smks){
            xv3[i] <- -2*xv3[i]
            yv3[i] <- -2*yv3[i]; next # I am going to programmer hell
        }
        if (ks13[i]==smks){
            xv2[i] <- -2*xv2[i]
            yv2[i] <- -2*yv2[i]; next
        }
            xv1[i] <- -2*xv1[i] 
            yv1[i] <- -2*yv1[i] 
    }
    offx <- xv1 + xv2 + xv3; offy <- yv1 + yv2 + yv3
    xoffset <- offx*i_to_the_i/sqrt(offx^2+offy^2);yoffset <- offy*i_to_the_i/sqrt(offx^2+offy^2) # this is going to be hard to debug
    par(mar=c(.1,.1,.1,.1))  #narrow margins
    # start an empty plot, just big enough, and make sure the aspect ratio is forced to 1
    plot(NULL,xlim = c(1.05*min(DF$x),1.05*max(DF$x)),ylim = c(1.05*min(DF$y),1.05*max(DF$y)),axes = FALSE, asp = 1)
    arrows(DF$x,DF$y,(xvc*xr1+cvx*DF$x),(xvc*yr1+cvx*DF$y),length = 0.1,col="DarkSlateBlue") # draw blue arrows
    segments((xvc*xr1+cvx*DF$x),(xvc*yr1+cvx*DF$y),xr1,yr1,col="DarkSlateBlue") # these represent multiplication by r
    segments(DF$x,DF$y,xf1,yf1,col = "lightsalmon", lwd = 2) # no arrowheads because f is order 2
    points(DF$x,DF$y,pch=21,bg = DF$color, col = "black", cex=2) # and now we draw the vertices, after the lines
    text(DF$x+xoffset,DF$y+yoffset,DF$label) # now we actually label
}

# super advanced debugging technique
# CA5.drawGraph(A5DF)

# how we multiply group elements represented in terms of their generators
CA5.multiply <- function(a,b) {
    x <- paste("rrrrr",a,b,sep="") # collapsing it down to standard form is messy so don't question me
    oldx <- "xyzzy" # initialize previous non-standard generator form with nonsense
    while (x!=oldx){ # I could have left it as a for loop but I thought it would be more elegant like this
        oldx <- x # before we mess with it, keep a copy for reference
        x <- sub("ff","",x) # two flips equals nothing
        x <- sub("frf","rrrrfrrrr",x) # flip,rotate,flip = counter-rotate,flip,counter-rotate
        x <- sub("frrrrf","rfr",x) # flip,counter-rotate,flip = rotate,flip,rotate
        x <- sub("rfrrf","frrrfrrrr",x) # yeah you get the idea
        x <- sub("frrrfrrrf","rfrrrfr",x) # there's lots of ways of getting from point A to point B
        x <- sub("rrrrr","",x) # five consecutive rotations is not a good use of your time
    }
    return(x)
}


# There's a function here to raise a generator-form element to an integer power
# I don't think we ever use it
CA5.power <- function(a,n){
    x <-a # it does pretty much what you'd expect
    while (n>1){
        x <- CA5.multiply(x,a) # it's multiplying
        n <- n-1 # n times
    }
    return(x)
}


# markVertex takes a dataframe, a way of representating the vertex, and a color
# it is 'overloaded' in that it can cope with the vertex being represented either
    # in generator form or just by its index number
        # If you try to input its permutation cycles this will fail
        # maybe I could add something to cope with that
# Important note: the only thing this function does is change the value of 'color' in the dataframe
# It does not redraw the diagram
CA5.markVertex <- function(DF,x,color) {
    if (is.character(x)){
        x <- CA5.multiply(x,"")
        x <- which(DF$sana == x)
    }
    DF$color[x] <- color # replaced hardcoded column number with column name
    return(DF)
} 

A5DF <- CA5.makeDataFrame() #this seems like a weird place to put this but whatever

# A5DF <- CA5.markVertex(A5DF,"frrrf","orangered")
# A5DF <- CA5.markVertex(A5DF,59,"cornflowerblue")
 CA5.drawGraph(A5DF)

# let's not waste our time rewriting permutecalc, it's perfect
source("permutecalc.R")


# Given a cycle form for r 'rSana' and a cycle form for f 'fSana'
# a dataframe, and a single word made up of generator elements
# convert the generator word into cycle notation
# returns a permutation like (13)(25)
CA5.makePerm <- function(DF,sana,IVert ="I", rSana = "(12345)", fSana = "(12)(34)"){
    product = IVert # start with the identity
    n <- nchar(sana) # how many elements are in the generator word
    if (n > 0) # as long as it's not stupid,
        for (i in (1:nchar(sana))){ # loop through the word
            g <- substr(sana,i,i) # we might be able to do this with strsplit()
            product <- Perm.multiply(product,ifelse(g=="f",fSana,rSana)) # use Perm.multiply to multiply perms
        }
    return (product)
}

# CA5.makePerm(A5DF,"frrrr")

# This function makePerms is TOTALLY DIFFERENT from the function makePerm
# This one takes a dataframe and the permutation cycle-form representations for the generators,
# and returns a dataframe completely filled in with permutation representations for all the vertices
CA5.makePerms <- function(DF,rSana = "(12345)", fSana = "(12)(34)"){
    for (i in (1:nrow(DF))){
        DF$perm[i] <- CA5.makePerm(DF,DF$sana[i],"I",rSana,fSana)
        DF$label[i] <- DF$perm[i] # This will work for now but once you make perms you can never go back
    }
    return (DF)
}

#############################################

 A5DF <- CA5.makePerms(A5DF)
# CA5.drawGraph(A5DF)


# findClosestVertex takes an x and y coordinate
# and unsurprisingly enough, finds the closest vertex to that position
# through the straightforward approach of calculating all the distances
CA5.findClosestVertex <- function(x,y){
    DF <- CA5.makeDataFrame()
    distsq <- numeric(6)
    for (i in 1:60){
        distsq[i] <- (DF[i,1]-x)^2+(DF[i,2]-y)^2
    }
    k <- which(distsq == min(distsq))
    stopifnot(length(k)==1)
    return(k)
}

# perms <- sapply(A5DF$sana,CA5.makePerm,DF=A5DF)
# perms

