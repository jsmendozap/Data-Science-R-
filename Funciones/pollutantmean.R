#Escriba una función llamada "contaminante-medio" que calcula la media de un contaminante (sulfato o nitrato) 
#a través de una lista específica de monitores. La función "significado de los contaminantes" toma tres
#argumentos: "directorio", "contaminante" e "id". Dados los números de identificación de un monitor de 
#vectores, 'pollutantmean' lee que monitorea los datos de material particulado del directorio especificado
#en el argumento 'directory' y devuelve la media del contaminante a través de todos los monitores, 
#ignorando cualquier valor faltante codificado como NA. Un prototipo de la función es el siguiente

ruta <- getwd()
id <- dir()
pollutantmean <- function(Directorio, Pollutant, ID = 1:332){
  arc <- dir(Directorio)
  dat <- data.frame()
  for(i in ID){
    dat <- rbind(dat,read.csv2(arc[i], sep = ","))
  }
  dat_subset <- subset(dat, select = Pollutant)
  dat_subset2 <- dat_subset[!is.na(dat_subset)]
  media <- mean(as.double(dat_subset2, na.rm = T))
  media
}




  