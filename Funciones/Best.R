###### Taller 4 ######

outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
BD <- data.frame(sapply(outcome[,c(11,17,23)], as.numeric))
BD1 <- outcome[,c(2,7)]
BD <- cbind(BD1, BD)
names(BD) <- c("Hospital Name", "State", "Heart Attack", "Heart Failure", "Pneumonia" )
# histogram of the 30-day death rates from heart attack

outcome[, 11] <- as.numeric(outcome[, 11])
hist(outcome[, 11])


# Función para encontrar el mejor hospital
library(dplyr)
best <- function(state, outcome) {
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
  c <-  na.omit(as.data.frame(b[state]))
  rownames(c) <- NULL
  names(c) <- c("Hospital Name", "State", "Heart Attack", "Heart Failure", "Pneumonia" )
  m <- arrange_at(c, outcome)
  l <- m[,c("Hospital Name",outcome)]
  head(l, n = 1)
}

