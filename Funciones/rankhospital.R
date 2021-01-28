outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
BD <- data.frame(sapply(outcome[,c(11,17,23)], as.numeric))
BD1 <- outcome[,c(2)]
BD2 <- as.factor(outcome[,c(7)])
BD1 <- data.frame(BD1, BD2)
BD <- cbind(BD1, BD)
names(BD) <- c("Hospital Name", "State", "Heart Attack", "Heart Failure", "Pneumonia" )


library(dplyr)
rankhospital <- function(state, outcome, rank = "mejor"){
  x <- c()
  for(i in 1:length(unique(BD$State))){
    if (state == unique(BD$State)[i]){
      x[i] <- 1
    }
    else{
      x[i] <- 0
    }
  }
  z <- sum(x)
  if (z == 0){
    stop("Estado inválido")
  }
  k <- c("Heart Attack", "Heart Failure", "Pneumonia")
  w <- c()
  for(i in 1:3){
    if (outcome == k[i]){
      w[i] <- 1
    }
    else{
      w[i] <- 0
    }
  }
  v <- sum(w)
  if (v == 0){
    stop("Resultado inválido")
  }
  b <- split(BD, BD$State)
  d <-  na.omit(as.data.frame(b[state]))
  rownames(d) <- NULL
  names(d) <- c("Hospital Name", "State", "Heart Attack", "Heart Failure", "Pneumonia")
  o <- function(d, outcome, rank){
    if (is.numeric(rank) == T){
      l <- d[,c("Hospital Name", "State", outcome)]
      r <- arrange(l, l[,3], l$'Hospital Name')
      a <- r[rank,]
    }else if(rank == "mejor"){
      l <- d[,c("Hospital Name", "State", outcome)]
      a <- arrange(l, l[,3], l$'Hospital Name')
    }else if(rank == "peor"){
      l <- d[,c("Hospital Name", "State", outcome)]
      a <- arrange(l, desc(l[,3]), l$'Hospital Name')
    }
    a
  }
  z <- o(d, outcome, rank)
  head(z,1)
}



