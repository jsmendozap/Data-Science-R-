outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
BD <- data.frame(sapply(outcome[,c(11,17,23)], as.numeric))
BD1 <- outcome[,c(2,7)]
BD <- cbind(BD1, BD)
names(BD) <- c("Hospital Name", "State", "Heart Attack", "Heart Failure", "Pneumonia" )


library(dplyr)
rankall <- function(outcome, rank = "mejor") {
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
  d <- data.frame()
  b <- split(BD, BD$State)
  if (outcome == "Heart Attack"){
    t <- 3
  }else if (outcome == "Heart Failure"){
    t <- 4
  }else if (outcome == "Pneumonia"){
    t <- 5
  }
  for(i in 1:length(b)){
    if(rank == "mejor"){
      s <- data.frame(b[[i]])
      names(s) <- c("Hospital Name", "State", "Heart Attack", "Heart Failure", "Pneumonia" )
      m <- arrange(s, s[,t], s[,1])
      l <- m[1,c("Hospital Name", "State", outcome)]
      d <- rbind(d,l)
    }else if (rank == "peor"){
      s <- data.frame(b[[i]])
      names(s) <- c("Hospital Name", "State", "Heart Attack", "Heart Failure", "Pneumonia" )
      m <- arrange(s, desc(s[,t]), s[,1])
      l <- m[1,c("Hospital Name", "State", outcome)]
      d <- rbind(d, l)
    }else{
      s <- data.frame(b[[i]])
      names(s) <- c("Hospital Name", "State", "Heart Attack", "Heart Failure", "Pneumonia" )
      m <- arrange(s, s[,t], s[,1])
      l <- m[rank,c("Hospital Name", "State", outcome)]
      d <- rbind(d, l)
    }
  }
  d
}
