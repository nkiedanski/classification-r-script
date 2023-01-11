music_africa
music_africa_var = music_africa[,-117] #saco la etiqueta que no aplica
music_africa_var

# PCA

#PARTE 1 ###################################################################### 
pr.out <- prcomp(music_africa_var) 
pr.out
pr.out$rotation # las columnas continen los vectores propios
summary(pr.out) # la importancia de cada componente
pve <- summary(pr.out)$importance[2,] # proporción de la varianza explicada por cada componente principal
pve

#plot de la proporcion de varianza explicada
plot(pve , xlab=" Principal Component ", ylab="Proportion of Variance Explained ", ylim=c(0,1),type='b') 

#plot de la proporcion de varianza acumulada
plot(cumsum(pve), xlab="Principal Component ", ylab="Cumulative Proportion of Variance Explained ", ylim=c(0,1),type='b')

# PARTE 2 #####################################################################

biplot(pr.out, scale=0) #biplot con todos los datos

music_africa_var_sub = music_africa_var[1:100, 1:10]
music_africa_var_sub
pr.out.submatriz <- prcomp(music_africa_var_sub) 
pr.out.submatriz
pve.submatriz <- summary(pr.out.submatriz)$importance[2,]
pve.submatriz
#plot de la proporcion de varianza explicada
plot(pve.submatriz , xlab=" Principal Component ", ylab="Proportion of Variance Explained ", ylim=c(0,1),type='b') 
#plot de la proporcion de varianza acumulada
plot(cumsum(pve.submatriz), xlab="Principal Component ", ylab="Cumulative Proportion of Variance Explained ", ylim=c(0,1),type='b')
biplot(pr.out.submatriz, scale=0) #biplot con todos los datos
summary(pr.out.submatriz)
