# Escriba una función que tome un directorio de archivos de datos y un umbral para los casos completos 
#y calcule la correlación entre el sulfato y el nitrato para vigilar los lugares donde el número de casos
#completamente observados (en todas las variables) es mayor que el umbral. La función debe devolver un 
#vector de correlaciones para los monitores que cumplan el requisito del umbral. Si ningún monitor
#cumple el requisito del umbral, la función debe devolver un vector numérico de longitud 0. 

corr <- function(Directorio, id = 1:332, umbral = 0){
  arc <- dir(Directorio)
  dat <- data.frame()
  for (i in id){
    dat <- rbind(dat, na.omit(read.csv2(arc[i], sep = ",")))
  }
  b <- group_by(dat, dat$ID)%>%
    summarise(corre = cor(as.numeric(sulfate), as.numeric(nitrate)))
  c <- filter(b, b[,2] >= umbral)
  c
}

