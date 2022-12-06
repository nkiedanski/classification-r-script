file.choose() #selecciona ruta, copio y agrego a variable "ruta"
library(readr) #instalar readr antes

#debe estar la ruta completa del archivo
ruta <-"/Users/nicole/Downloads/winequality-white.csv"

Vinos <- data.frame(read_csv2(ruta)) #si no tuviera titulos agrego ,colnames=FALSE
#read_csv cuando esta separado por , 
#read_csv2 cuando es separado por ;
Vinos
