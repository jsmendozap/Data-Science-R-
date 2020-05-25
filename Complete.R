#Escriba una función que lea un directorio lleno de archivos e informe el número de casos completamente
#observados en cada archivo de datos. La función debe devolver un marco de datos en el que la primera 
#columna sea el nombre del archivo y la segunda el número de casos completos. A continuación se presenta
#un prototipo de esta función


complete <- function(Directorio, id = 1:332){
  arc <- dir(Directorio)
  dat <- data.frame()
  for (i in id){
    dat <- rbind(dat,na.omit(read.csv2(arc[i], sep = ",")))
  }
  nobs <- tapply(dat$sulfate, dat$ID, length)
  ID <- id
  dat2 <- tibble(nobs,id)
  dat2
}
