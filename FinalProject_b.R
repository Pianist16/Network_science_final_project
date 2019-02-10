setwd("C:/Users/User/Desktop/Чина/CEU/2nd Year/1.Fall2015/Statistical Methods/Project")
library(igraph)
library(taRifx)

### This script is for the network of type 2nd -> winner, 3rd -> winner and 3rd -> 2nd ###
### i.e. it takes into account of the link beteen 3rd and 2nd players, contrary to the "FinalProject_a" ###

#### Preparing the data ####

a1 <- read.csv("data1.csv",stringsAsFactors = FALSE)
a2 <- read.csv("data2.csv",stringsAsFactors = FALSE)
b1 <- data.frame()
b2 <- data.frame()
c1 <- data.frame()
c2 <- data.frame()

for (i in seq(from = 1, to = 1112)){
  b1[i,1] <- a1[2*i-1,3]
  b1[i,2] <- a1[2*i-1,4]
  b1[i,3] <- a1[2*i-1,5]
  c1[i,1] <- a1[2*i,3]
  c1[i,2] <- a1[2*i,4]
  c1[i,3] <- a1[2*i,5]
}

b1 <- b1[-59,] # empty cell => DELETE IT!!! =)
c1 <- c1[-59,] # empty cell => DELETE IT!!! =)

for (i in seq(from = 1, to = 199)){
  b2[i,1] <- a2[2*i-1,3]
  b2[i,2] <- a2[2*i-1,4]
  b2[i,3] <- a2[2*i-1,5]
  c2[i,1] <- a2[2*i,3]
  c2[i,2] <- a2[2*i,4]
  c2[i,3] <- a2[2*i,5]
}

# m1 and m2 to see what we got in previous loops
m1 <- cbind(b1,c1)
m2 <- cbind(b2,c2)

t1.1 <- destring(c1[,1])
t2.1 <- destring(c1[,2])
t3.1 <- destring(c1[,3])
p1.1 <- as.matrix(t1.1)
p2.1 <- as.matrix(t2.1)
p3.1 <- as.matrix(t3.1)

t1.2 <- destring(c2[,1])
t2.2 <- destring(c2[,2])
t3.2 <- destring(c2[,3])
p1.2 <- as.matrix(t1.2)
p2.2 <- as.matrix(t2.2)
p3.2 <- as.matrix(t3.2)

d1 <- cbind(p1.1,p2.1,p3.1)
d2 <- cbind(p1.2,p2.2,p3.2)

### Games from 2001 to 2015

e1.1 <- data.frame()
e2.1 <- data.frame()
e3.1 <- data.frame()

f <- NA

for (i in seq(from = 1, to = 1111, by = 3)){
  if (d1[i,1] > d1[i,2] & d1[i,1] > d1[i,3] & d1[i,2] > d1[i,3]){
    e1.1[i,1] <- b1[i,1] #A
    e1.1[i,2] <- b1[i,2] #B
    e1.1[i,3] <- d1[i,1] - d1[i,2]
    e1.1[i+1,1] <- b1[i,1] #A
    e1.1[i+1,2] <- b1[i,3] #C
    e1.1[i+1,3] <- d1[i,1] - d1[i,3]
    e1.1[i+2,1] <- b1[i,2] #B
    e1.1[i+2,2] <- b1[i,3] #C
    e1.1[i+2,3] <- d1[i,2] - d1[i,3]
  } else {
    if (d1[i,1] > d1[i,2] & d1[i,1] > d1[i,3] & d1[i,3] > d1[i,2]){
      e1.1[i,1] <- b1[i,1] #A
      e1.1[i,2] <- b1[i,2] #B
      e1.1[i,3] <- d1[i,1] - d1[i,2]
      e1.1[i+1,1] <- b1[i,1] #A
      e1.1[i+1,2] <- b1[i,3] #C
      e1.1[i+1,3] <- d1[i,1] - d1[i,3]
      e1.1[i+2,1] <- b1[i,3] #C
      e1.1[i+2,2] <- b1[i,2] #B
      e1.1[i+2,3] <- d1[i,3] - d1[i,2]
    } else {
      if (d1[i,2] > d1[i,1] & d1[i,1] > d1[i,3] & d1[i,2] > d1[i,3]){
        e1.1[i,1] <- b1[i,2] #B
        e1.1[i,2] <- b1[i,1] #A
        e1.1[i,3] <- d1[i,2] - d1[i,1]
        e1.1[i+1,1] <- b1[i,2] #B
        e1.1[i+1,2] <- b1[i,3] #C
        e1.1[i+1,3] <- d1[i,2] - d1[i,3]
        e1.1[i+2,1] <- b1[i,1] #A
        e1.1[i+2,2] <- b1[i,3] #C
        e1.1[i+2,3] <- d1[i,1] - d1[i,3]
      } else {
        if (d1[i,2] > d1[i,1] & d1[i,1] > d1[i,3] & d1[i,3] > d1[i,2]){
          e1.1[i,1] <- b1[i,2] #B
          e1.1[i,2] <- b1[i,1] #A
          e1.1[i,3] <- d1[i,2] - d1[i,1]
          e1.1[i+1,1] <- b1[i,2] #B
          e1.1[i+1,2] <- b1[i,3] #C
          e1.1[i+1,3] <- d1[i,2] - d1[i,3]
          e1.1[i+2,1] <- b1[i,3] #C
          e1.1[i+2,2] <- b1[i,1] #A
          e1.1[i+2,3] <- d1[i,3] - d1[i,1]
        } else {
          if (d1[i,1] > d1[i,2] & d1[i,3] > d1[i,1] & d1[i,3] > d1[i,2]){
            e1.1[i,1] <- b1[i,3] #C
            e1.1[i,2] <- b1[i,1] #A
            e1.1[i,3] <- d1[i,3] - d1[i,1]
            e1.1[i+1,1] <- b1[i,3] #C
            e1.1[i+1,2] <- b1[i,2] #B
            e1.1[i+1,3] <- d1[i,3] - d1[i,2]
            e1.1[i+2,1] <- b1[i,1] #A
            e1.1[i+2,2] <- b1[i,2] #B
            e1.1[i+2,3] <- d1[i,1] - d1[i,2]
          } else {
            if (d1[i,2] > d1[i,1] & d1[i,3] > d1[i,1] & d1[i,3] > d1[i,2]){
              e1.1[i,1] <- b1[i,3] #C
              e1.1[i,2] <- b1[i,1] #A
              e1.1[i,3] <- d1[i,3] - d1[i,1]
              e1.1[i+1,1] <- b1[i,3] #C
              e1.1[i+1,2] <- b1[i,2] #B
              e1.1[i+1,3] <- d1[i,3] - d1[i,2]
              e1.1[i+2,1] <- b1[i,2] #B
              e1.1[i+2,2] <- b1[i,1] #A
              e1.1[i+2,3] <- d1[i,2] - d1[i,1]
            } else {
              if (d1[i,1] == d1[i,2] | d1[i,1] == d1[i,3] | d1[i,2] == d1[i,3]){
                e1.1[i,1] <- f #C
                e1.1[i,2] <- f #A
                e1.1[i,3] <- f
                e1.1[i+1,1] <- f #C
                e1.1[i+1,2] <- f #B
                e1.1[i+1,3] <- f
                e1.1[i+2,1] <- f #B
                e1.1[i+2,2] <- f #A
                e1.1[i+2,3] <- f
              }
            }
          }
        }
      }
    } 
  }
}

for (i in seq(from = 2, to = 1111, by = 3)){
  if (d1[i,1] > d1[i,2] & d1[i,1] > d1[i,3] & d1[i,2] > d1[i,3]){
    e2.1[i,1] <- b1[i,1] #A
    e2.1[i,2] <- b1[i,2] #B
    e2.1[i,3] <- d1[i,1] - d1[i,2]
    e2.1[i+1,1] <- b1[i,1] #A
    e2.1[i+1,2] <- b1[i,3] #C
    e2.1[i+1,3] <- d1[i,1] - d1[i,3]
    e2.1[i+2,1] <- b1[i,2] #B
    e2.1[i+2,2] <- b1[i,3] #C
    e2.1[i+2,3] <- d1[i,2] - d1[i,3]
  } else {
    if (d1[i,1] > d1[i,2] & d1[i,1] > d1[i,3] & d1[i,3] > d1[i,2]){
      e2.1[i,1] <- b1[i,1] #A
      e2.1[i,2] <- b1[i,2] #B
      e2.1[i,3] <- d1[i,1] - d1[i,2]
      e2.1[i+1,1] <- b1[i,1] #A
      e2.1[i+1,2] <- b1[i,3] #C
      e2.1[i+1,3] <- d1[i,1] - d1[i,3]
      e2.1[i+2,1] <- b1[i,3] #C
      e2.1[i+2,2] <- b1[i,2] #B
      e2.1[i+2,3] <- d1[i,3] - d1[i,2]
    } else {
      if (d1[i,2] > d1[i,1] & d1[i,1] > d1[i,3] & d1[i,2] > d1[i,3]){
        e2.1[i,1] <- b1[i,2] #B
        e2.1[i,2] <- b1[i,1] #A
        e2.1[i,3] <- d1[i,2] - d1[i,1]
        e2.1[i+1,1] <- b1[i,2] #B
        e2.1[i+1,2] <- b1[i,3] #C
        e2.1[i+1,3] <- d1[i,2] - d1[i,3]
        e2.1[i+2,1] <- b1[i,1] #A
        e2.1[i+2,2] <- b1[i,3] #C
        e2.1[i+2,3] <- d1[i,1] - d1[i,3]
      } else {
        if (d1[i,2] > d1[i,1] & d1[i,1] > d1[i,3] & d1[i,3] > d1[i,2]){
          e2.1[i,1] <- b1[i,2] #B
          e2.1[i,2] <- b1[i,1] #A
          e2.1[i,3] <- d1[i,2] - d1[i,1]
          e2.1[i+1,1] <- b1[i,2] #B
          e2.1[i+1,2] <- b1[i,3] #C
          e2.1[i+1,3] <- d1[i,2] - d1[i,3]
          e2.1[i+2,1] <- b1[i,3] #C
          e2.1[i+2,2] <- b1[i,1] #A
          e2.1[i+2,3] <- d1[i,3] - d1[i,1]
        } else {
          if (d1[i,1] > d1[i,2] & d1[i,3] > d1[i,1] & d1[i,3] > d1[i,2]){
            e2.1[i,1] <- b1[i,3] #C
            e2.1[i,2] <- b1[i,1] #A
            e2.1[i,3] <- d1[i,3] - d1[i,1]
            e2.1[i+1,1] <- b1[i,3] #C
            e2.1[i+1,2] <- b1[i,2] #B
            e2.1[i+1,3] <- d1[i,3] - d1[i,2]
            e2.1[i+2,1] <- b1[i,1] #A
            e2.1[i+2,2] <- b1[i,2] #B
            e2.1[i+2,3] <- d1[i,1] - d1[i,2]
          } else {
            if (d1[i,2] > d1[i,1] & d1[i,3] > d1[i,1] & d1[i,3] > d1[i,2]){
              e2.1[i,1] <- b1[i,3] #C
              e2.1[i,2] <- b1[i,1] #A
              e2.1[i,3] <- d1[i,3] - d1[i,1]
              e2.1[i+1,1] <- b1[i,3] #C
              e2.1[i+1,2] <- b1[i,2] #B
              e2.1[i+1,3] <- d1[i,3] - d1[i,2]
              e2.1[i+2,1] <- b1[i,2] #B
              e2.1[i+2,2] <- b1[i,1] #A
              e2.1[i+2,3] <- d1[i,2] - d1[i,1]
            } else {
              if (d1[i,1] == d1[i,2] | d1[i,1] == d1[i,3] | d1[i,2] == d1[i,3]){
                e2.1[i,1] <- f #C
                e2.1[i,2] <- f #A
                e2.1[i,3] <- f
                e2.1[i+1,1] <- f #C
                e2.1[i+1,2] <- f #B
                e2.1[i+1,3] <- f
                e2.1[i+2,1] <- f #B
                e2.1[i+2,2] <- f #A
                e2.1[i+2,3] <- f
              }
            }
          }
        }
      }
    } 
  }
}

for (i in seq(from = 3, to = 1111, by = 3)){
  if (d1[i,1] > d1[i,2] & d1[i,1] > d1[i,3] & d1[i,2] > d1[i,3]){
    e3.1[i,1] <- b1[i,1] #A
    e3.1[i,2] <- b1[i,2] #B
    e3.1[i,3] <- d1[i,1] - d1[i,2]
    e3.1[i+1,1] <- b1[i,1] #A
    e3.1[i+1,2] <- b1[i,3] #C
    e3.1[i+1,3] <- d1[i,1] - d1[i,3]
    e3.1[i+2,1] <- b1[i,2] #B
    e3.1[i+2,2] <- b1[i,3] #C
    e3.1[i+2,3] <- d1[i,2] - d1[i,3]
  } else {
    if (d1[i,1] > d1[i,2] & d1[i,1] > d1[i,3] & d1[i,3] > d1[i,2]){
      e3.1[i,1] <- b1[i,1] #A
      e3.1[i,2] <- b1[i,2] #B
      e3.1[i,3] <- d1[i,1] - d1[i,2]
      e3.1[i+1,1] <- b1[i,1] #A
      e3.1[i+1,2] <- b1[i,3] #C
      e3.1[i+1,3] <- d1[i,1] - d1[i,3]
      e3.1[i+2,1] <- b1[i,3] #C
      e3.1[i+2,2] <- b1[i,2] #B
      e3.1[i+2,3] <- d1[i,3] - d1[i,2]
    } else {
      if (d1[i,2] > d1[i,1] & d1[i,1] > d1[i,3] & d1[i,2] > d1[i,3]){
        e3.1[i,1] <- b1[i,2] #B
        e3.1[i,2] <- b1[i,1] #A
        e3.1[i,3] <- d1[i,2] - d1[i,1]
        e3.1[i+1,1] <- b1[i,2] #B
        e3.1[i+1,2] <- b1[i,3] #C
        e3.1[i+1,3] <- d1[i,2] - d1[i,3]
        e3.1[i+2,1] <- b1[i,1] #A
        e3.1[i+2,2] <- b1[i,3] #C
        e3.1[i+2,3] <- d1[i,1] - d1[i,3]
      } else {
        if (d1[i,2] > d1[i,1] & d1[i,1] > d1[i,3] & d1[i,3] > d1[i,2]){
          e3.1[i,1] <- b1[i,2] #B
          e3.1[i,2] <- b1[i,1] #A
          e3.1[i,3] <- d1[i,2] - d1[i,1]
          e3.1[i+1,1] <- b1[i,2] #B
          e3.1[i+1,2] <- b1[i,3] #C
          e3.1[i+1,3] <- d1[i,2] - d1[i,3]
          e3.1[i+2,1] <- b1[i,3] #C
          e3.1[i+2,2] <- b1[i,1] #A
          e3.1[i+2,3] <- d1[i,3] - d1[i,1]
        } else {
          if (d1[i,1] > d1[i,2] & d1[i,3] > d1[i,1] & d1[i,3] > d1[i,2]){
            e3.1[i,1] <- b1[i,3] #C
            e3.1[i,2] <- b1[i,1] #A
            e3.1[i,3] <- d1[i,3] - d1[i,1]
            e3.1[i+1,1] <- b1[i,3] #C
            e3.1[i+1,2] <- b1[i,2] #B
            e3.1[i+1,3] <- d1[i,3] - d1[i,2]
            e3.1[i+2,1] <- b1[i,1] #A
            e3.1[i+2,2] <- b1[i,2] #B
            e3.1[i+2,3] <- d1[i,1] - d1[i,2]
          } else {
            if (d1[i,2] > d1[i,1] & d1[i,3] > d1[i,1] & d1[i,3] > d1[i,2]){
              e3.1[i,1] <- b1[i,3] #C
              e3.1[i,2] <- b1[i,1] #A
              e3.1[i,3] <- d1[i,3] - d1[i,1]
              e3.1[i+1,1] <- b1[i,3] #C
              e3.1[i+1,2] <- b1[i,2] #B
              e3.1[i+1,3] <- d1[i,3] - d1[i,2]
              e3.1[i+2,1] <- b1[i,2] #B
              e3.1[i+2,2] <- b1[i,1] #A
              e3.1[i+2,3] <- d1[i,2] - d1[i,1]
            } else {
              if (d1[i,1] == d1[i,2] | d1[i,1] == d1[i,3] | d1[i,2] == d1[i,3]){
                e3.1[i,1] <- f #C
                e3.1[i,2] <- f #A
                e3.1[i,3] <- f
                e3.1[i+1,1] <- f #C
                e3.1[i+1,2] <- f #B
                e3.1[i+1,3] <- f
                e3.1[i+2,1] <- f #B
                e3.1[i+2,2] <- f #A
                e3.1[i+2,3] <- f
              }
            }
          }
        }
      }
    } 
  }
}

### Games from 1994 to 2000 ###

e1.2 <- data.frame()
e2.2 <- data.frame()
e3.2 <- data.frame()

for (i in seq(from = 1, to = 199, by = 3)){
  if (d2[i,1] > d2[i,2] & d2[i,1] > d2[i,3] & d2[i,2] > d2[i,3]){
    e1.2[i,1] <- b2[i,1] #A
    e1.2[i,2] <- b2[i,2] #B
    e1.2[i,3] <- d2[i,1] - d2[i,2]
    e1.2[i+1,1] <- b2[i,1] #A
    e1.2[i+1,2] <- b2[i,3] #C
    e1.2[i+1,3] <- d2[i,1] - d2[i,3]
    e1.2[i+2,1] <- b2[i,2] #B
    e1.2[i+2,2] <- b2[i,3] #C
    e1.2[i+2,3] <- d2[i,2] - d2[i,3]
  } else {
    if (d2[i,1] > d2[i,2] & d2[i,1] > d2[i,3] & d2[i,3] > d2[i,2]){
      e1.2[i,1] <- b2[i,1] #A
      e1.2[i,2] <- b2[i,2] #B
      e1.2[i,3] <- d2[i,1] - d2[i,2]
      e1.2[i+1,1] <- b2[i,1] #A
      e1.2[i+1,2] <- b2[i,3] #C
      e1.2[i+1,3] <- d2[i,1] - d2[i,3]
      e1.2[i+2,1] <- b2[i,3] #C
      e1.2[i+2,2] <- b2[i,2] #B
      e1.2[i+2,3] <- d2[i,3] - d2[i,2]
    } else {
      if (d2[i,2] > d2[i,1] & d2[i,1] > d2[i,3] & d2[i,2] > d2[i,3]){
        e1.2[i,1] <- b2[i,2] #B
        e1.2[i,2] <- b2[i,1] #A
        e1.2[i,3] <- d2[i,2] - d2[i,1]
        e1.2[i+1,1] <- b2[i,2] #B
        e1.2[i+1,2] <- b2[i,3] #C
        e1.2[i+1,3] <- d2[i,2] - d2[i,3]
        e1.2[i+2,1] <- b2[i,1] #A
        e1.2[i+2,2] <- b2[i,3] #C
        e1.2[i+2,3] <- d2[i,1] - d2[i,3]
      } else {
        if (d2[i,2] > d2[i,1] & d2[i,1] > d2[i,3] & d2[i,3] > d2[i,2]){
          e1.2[i,1] <- b2[i,2] #B
          e1.2[i,2] <- b2[i,1] #A
          e1.2[i,3] <- d2[i,2] - d2[i,1]
          e1.2[i+1,1] <- b2[i,2] #B
          e1.2[i+1,2] <- b2[i,3] #C
          e1.2[i+1,3] <- d2[i,2] - d2[i,3]
          e1.2[i+2,1] <- b2[i,3] #C
          e1.2[i+2,2] <- b2[i,1] #A
          e1.2[i+2,3] <- d2[i,3] - d2[i,1]
        } else {
          if (d2[i,1] > d2[i,2] & d2[i,3] > d2[i,1] & d2[i,3] > d2[i,2]){
            e1.2[i,1] <- b2[i,3] #C
            e1.2[i,2] <- b2[i,1] #A
            e1.2[i,3] <- d2[i,3] - d2[i,1]
            e1.2[i+1,1] <- b2[i,3] #C
            e1.2[i+1,2] <- b2[i,2] #B
            e1.2[i+1,3] <- d2[i,3] - d2[i,2]
            e1.2[i+2,1] <- b2[i,1] #A
            e1.2[i+2,2] <- b2[i,2] #B
            e1.2[i+2,3] <- d2[i,1] - d2[i,2]
          } else {
            if (d2[i,2] > d2[i,1] & d2[i,3] > d2[i,1] & d2[i,3] > d2[i,2]){
              e1.2[i,1] <- b2[i,3] #C
              e1.2[i,2] <- b2[i,1] #A
              e1.2[i,3] <- d2[i,3] - d2[i,1]
              e1.2[i+1,1] <- b2[i,3] #C
              e1.2[i+1,2] <- b2[i,2] #B
              e1.2[i+1,3] <- d2[i,3] - d2[i,2]
              e1.2[i+2,1] <- b2[i,2] #B
              e1.2[i+2,2] <- b2[i,1] #A
              e1.2[i+2,3] <- d2[i,2] - d2[i,1]
            } else {
              if (d2[i,1] == d2[i,2] | d2[i,1] == d2[i,3] | d2[i,2] == d2[i,3]){
                e1.2[i,1] <- f #C
                e1.2[i,2] <- f #A
                e1.2[i,3] <- f
                e1.2[i+1,1] <- f #C
                e1.2[i+1,2] <- f #B
                e1.2[i+1,3] <- f
                e1.2[i+2,1] <- f #B
                e1.2[i+2,2] <- f #A
                e1.2[i+2,3] <- f
              }
            }
          }
        }
      }
    } 
  }
}

for (i in seq(from = 2, to = 199, by = 3)){
  if (d2[i,1] > d2[i,2] & d2[i,1] > d2[i,3] & d2[i,2] > d2[i,3]){
    e2.2[i,1] <- b2[i,1] #A
    e2.2[i,2] <- b2[i,2] #B
    e2.2[i,3] <- d2[i,1] - d2[i,2]
    e2.2[i+1,1] <- b2[i,1] #A
    e2.2[i+1,2] <- b2[i,3] #C
    e2.2[i+1,3] <- d2[i,1] - d1[i,3]
    e2.2[i+2,1] <- b2[i,2] #B
    e2.2[i+2,2] <- b2[i,3] #C
    e2.2[i+2,3] <- d2[i,2] - d2[i,3]
  } else {
    if (d2[i,1] > d2[i,2] & d2[i,1] > d2[i,3] & d2[i,3] > d2[i,2]){
      e2.2[i,1] <- b2[i,1] #A
      e2.2[i,2] <- b2[i,2] #B
      e2.2[i,3] <- d2[i,1] - d1[i,2]
      e2.2[i+1,1] <- b2[i,1] #A
      e2.2[i+1,2] <- b2[i,3] #C
      e2.2[i+1,3] <- d2[i,1] - d2[i,3]
      e2.2[i+2,1] <- b2[i,3] #C
      e2.2[i+2,2] <- b2[i,2] #B
      e2.2[i+2,3] <- d2[i,3] - d2[i,2]
    } else {
      if (d2[i,2] > d2[i,1] & d2[i,1] > d2[i,3] & d2[i,2] > d2[i,3]){
        e2.2[i,1] <- b2[i,2] #B
        e2.2[i,2] <- b2[i,1] #A
        e2.2[i,3] <- d2[i,2] - d2[i,1]
        e2.2[i+1,1] <- b2[i,2] #B
        e2.2[i+1,2] <- b2[i,3] #C
        e2.2[i+1,3] <- d2[i,2] - d2[i,3]
        e2.2[i+2,1] <- b2[i,1] #A
        e2.2[i+2,2] <- b2[i,3] #C
        e2.2[i+2,3] <- d2[i,1] - d2[i,3]
      } else {
        if (d2[i,2] > d2[i,1] & d2[i,1] > d2[i,3] & d2[i,3] > d2[i,2]){
          e2.2[i,1] <- b2[i,2] #B
          e2.2[i,2] <- b2[i,1] #A
          e2.2[i,3] <- d2[i,2] - d2[i,1]
          e2.2[i+1,1] <- b2[i,2] #B
          e2.2[i+1,2] <- b2[i,3] #C
          e2.2[i+1,3] <- d2[i,2] - d2[i,3]
          e2.2[i+2,1] <- b2[i,3] #C
          e2.2[i+2,2] <- b2[i,1] #A
          e2.2[i+2,3] <- d2[i,3] - d2[i,1]
        } else {
          if (d2[i,1] > d2[i,2] & d2[i,3] > d2[i,1] & d2[i,3] > d2[i,2]){
            e2.2[i,1] <- b2[i,3] #C
            e2.2[i,2] <- b2[i,1] #A
            e2.2[i,3] <- d2[i,3] - d2[i,1]
            e2.2[i+1,1] <- b2[i,3] #C
            e2.2[i+1,2] <- b2[i,2] #B
            e2.2[i+1,3] <- d2[i,3] - d2[i,2]
            e2.2[i+2,1] <- b2[i,1] #A
            e2.2[i+2,2] <- b2[i,2] #B
            e2.2[i+2,3] <- d2[i,1] - d2[i,2]
          } else {
            if (d2[i,2] > d2[i,1] & d2[i,3] > d2[i,1] & d2[i,3] > d2[i,2]){
              e2.2[i,1] <- b2[i,3] #C
              e2.2[i,2] <- b2[i,1] #A
              e2.2[i,3] <- d2[i,3] - d2[i,1]
              e2.2[i+1,1] <- b2[i,3] #C
              e2.2[i+1,2] <- b2[i,2] #B
              e2.2[i+1,3] <- d2[i,3] - d2[i,2]
              e2.2[i+2,1] <- b2[i,2] #B
              e2.2[i+2,2] <- b2[i,1] #A
              e2.2[i+2,3] <- d2[i,2] - d2[i,1]
            } else {
              if (d2[i,1] == d2[i,2] | d2[i,1] == d2[i,3] | d2[i,2] == d2[i,3]){
                e2.2[i,1] <- f #C
                e2.2[i,2] <- f #A
                e2.2[i,3] <- f
                e2.2[i+1,1] <- f #C
                e2.2[i+1,2] <- f #B
                e2.2[i+1,3] <- f
                e2.2[i+2,1] <- f #B
                e2.2[i+2,2] <- f #A
                e2.2[i+2,3] <- f
              }
            }
          }
        }
      }
    } 
  }
}

for (i in seq(from = 3, to = 199, by = 3)){
  if (d2[i,1] > d2[i,2] & d2[i,1] > d2[i,3] & d2[i,2] > d2[i,3]){
    e3.2[i,1] <- b2[i,1] #A
    e3.2[i,2] <- b2[i,2] #B
    e3.2[i,3] <- d2[i,1] - d2[i,2]
    e3.2[i+1,1] <- b2[i,1] #A
    e3.2[i+1,2] <- b2[i,3] #C
    e3.2[i+1,3] <- d2[i,1] - d2[i,3]
    e3.2[i+2,1] <- b2[i,2] #B
    e3.2[i+2,2] <- b2[i,3] #C
    e3.2[i+2,3] <- d2[i,2] - d2[i,3]
  } else {
    if (d2[i,1] > d2[i,2] & d2[i,1] > d2[i,3] & d2[i,3] > d2[i,2]){
      e3.2[i,1] <- b2[i,1] #A
      e3.2[i,2] <- b2[i,2] #B
      e3.2[i,3] <- d2[i,1] - d2[i,2]
      e3.2[i+1,1] <- b2[i,1] #A
      e3.2[i+1,2] <- b2[i,3] #C
      e3.2[i+1,3] <- d2[i,1] - d2[i,3]
      e3.2[i+2,1] <- b2[i,3] #C
      e3.2[i+2,2] <- b2[i,2] #B
      e3.2[i+2,3] <- d2[i,3] - d2[i,2]
    } else {
      if (d2[i,2] > d2[i,1] & d2[i,1] > d2[i,3] & d2[i,2] > d2[i,3]){
        e3.2[i,1] <- b2[i,2] #B
        e3.2[i,2] <- b2[i,1] #A
        e3.2[i,3] <- d2[i,2] - d2[i,1]
        e3.2[i+1,1] <- b2[i,2] #B
        e3.2[i+1,2] <- b2[i,3] #C
        e3.2[i+1,3] <- d2[i,2] - d2[i,3]
        e3.2[i+2,1] <- b2[i,1] #A
        e3.2[i+2,2] <- b2[i,3] #C
        e3.2[i+2,3] <- d2[i,1] - d2[i,3]
      } else {
        if (d2[i,2] > d2[i,1] & d2[i,1] > d2[i,3] & d2[i,3] > d2[i,2]){
          e3.2[i,1] <- b2[i,2] #B
          e3.2[i,2] <- b2[i,1] #A
          e3.2[i,3] <- d2[i,2] - d2[i,1]
          e3.2[i+1,1] <- b2[i,2] #B
          e3.2[i+1,2] <- b2[i,3] #C
          e3.2[i+1,3] <- d2[i,2] - d2[i,3]
          e3.2[i+2,1] <- b2[i,3] #C
          e3.2[i+2,2] <- b2[i,1] #A
          e3.2[i+2,3] <- d2[i,3] - d2[i,1]
        } else {
          if (d2[i,1] > d2[i,2] & d2[i,3] > d2[i,1] & d2[i,3] > d2[i,2]){
            e3.2[i,1] <- b2[i,3] #C
            e3.2[i,2] <- b2[i,1] #A
            e3.2[i,3] <- d2[i,3] - d2[i,1]
            e3.2[i+1,1] <- b2[i,3] #C
            e3.2[i+1,2] <- b2[i,2] #B
            e3.2[i+1,3] <- d2[i,3] - d2[i,2]
            e3.2[i+2,1] <- b2[i,1] #A
            e3.2[i+2,2] <- b2[i,2] #B
            e3.2[i+2,3] <- d2[i,1] - d2[i,2]
          } else {
            if (d2[i,2] > d2[i,1] & d2[i,3] > d2[i,1] & d2[i,3] > d2[i,2]){
              e3.2[i,1] <- b2[i,3] #C
              e3.2[i,2] <- b2[i,1] #A
              e3.2[i,3] <- d2[i,3] - d2[i,1]
              e3.2[i+1,1] <- b2[i,3] #C
              e3.2[i+1,2] <- b2[i,2] #B
              e3.2[i+1,3] <- d2[i,3] - d2[i,2]
              e3.2[i+2,1] <- b2[i,2] #B
              e3.2[i+2,2] <- b2[i,1] #A
              e3.2[i+2,3] <- d2[i,2] - d2[i,1]
            } else {
              if (d2[i,1] == d2[i,2] | d2[i,1] == d2[i,3] | d2[i,2] == d2[i,3]){
                e3.2[i,1] <- f #C
                e3.2[i,2] <- f #A
                e3.2[i,3] <- f
                e3.2[i+1,1] <- f #C
                e3.2[i+1,2] <- f #B
                e3.2[i+1,3] <- f
                e3.2[i+2,1] <- f #B
                e3.2[i+2,2] <- f #A
                e3.2[i+2,3] <- f
              }
            }
          }
        }
      }
    } 
  }
}

r1.1 <- rbind(e1.1,e2.1,e3.1)
r2.1 <- na.omit(r1.1)
r3.1 <- data.frame(r2.1[,2],r2.1[,1])

r1.2 <- rbind(e1.2,e2.2,e3.2)
r2.2 <- na.omit(r1.2)
r3.2 <- data.frame(r2.2[,2],r2.2[,1])

g1 <- graph.data.frame(r3.1, directed = TRUE)
g1_ <- graph.data.frame(r3.1, directed = TRUE)
E(g1)$weight <- r2.1[,3]
E(g1_)$weight <- 1

g2 <- graph.data.frame(r3.2, directed = TRUE)
g2_ <- graph.data.frame(r3.2, directed = TRUE)
E(g2)$weight <- r2.2[,3]
E(g2_)$weight <- 1

g1.final <- simplify(g1)
g1_final <- simplify(g1_)
g2.final <- simplify(g2)
g2_final <- simplify(g2_)

#### Doing PageRank ####

pagerank.g1.final <- page.rank(g1.final, vids = V(g1.final), directed = TRUE) # with simplify() applied to g1; weights = difference in earnings for each row and are set so by default,because -> line 302
pagerank.g1_final <- page.rank(g1_final, vids = V(g1_final), directed = TRUE) # with simplify() applied to g1_; weights = 1 for each row and are set so by default,because -> line 303
pagerank.g2.final <- page.rank(g2.final, vids = V(g2.final), directed = TRUE) # with simplify() applied to g2; weights = difference in earnings for each row and are set so by default,because -> line 307
pagerank.g2_final <- page.rank(g2_final, vids = V(g2_final), directed = TRUE) # with simplify() applied to g2_; weights = 1 for each row and are set so by default,because -> line 308

head(sort(pagerank.g1.final$vector, decreasing=TRUE), 10)
head(sort(pagerank.g1_final$vector, decreasing=TRUE), 10)
head(sort(pagerank.g2.final$vector, decreasing=TRUE), 10)
head(sort(pagerank.g2_final$vector, decreasing=TRUE), 10)

#### Doing Motif Detection ####

q1 <- get.edgelist(g1.final,names = FALSE) # for period 2001-2015
q2 <- get.edgelist(g2.final,names = FALSE) # for period 1994-2000

w5 <- q1 - 1 # for subgraph size of 3; for period 2001-2015
w6 <- q2 - 1 # for subgraph size of 3; for period 1994-2000
w7 <- q1 - 1 # for subgraph size of 4; for period 2001-2015
w8 <- q2 - 1 # for subgraph size of 4; for period 1994-2000

write.table(w5, file = "w5.txt", sep = " ", row.names = FALSE, col.names = FALSE, append = FALSE)
write.table(w6, file = "w6.txt", sep = " ", row.names = FALSE, col.names = FALSE, append = FALSE)
write.table(w7, file = "w7.txt", sep = " ", row.names = FALSE, col.names = FALSE, append = FALSE)
write.table(w8, file = "w8.txt", sep = " ", row.names = FALSE, col.names = FALSE, append = FALSE)

### The rest is done using the "Fanmod" program ###
degree(g2.final)[order(degree(g2.final), decreasing=TRUE)[1:10]]