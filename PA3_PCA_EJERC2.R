#NOTAS OBTENIDAS POR ESTUDIANTES EN 5 PARCIALES

notes <- matrix(c(12,12,10, 11, 16,16, 16, 16, 16, 18, 12, 14, 22, 19, 22, 29, 
                  29, 31, 30, 16, 28, 28, 24, 24, 20, 22, 20, 11, 14, 26, 
                  11, 14, 28, 23, 20, 26, 25, 17, 19, 24, 18, 19, 25, 24, 36), 
                ncol=5, byrow = T)
rownames(notes) <-c("Juan", "Alina", "Ana", "Monica", "Daniel", "Andres", 
                     "Pedro", "Valentina", "Sandra")
colnames(notes) <-c("Matematica", "Fisica", "Musica", "Dibujo", "Id.Espanol")

notes

#perform principle components analysis, in this case, SCALE is not needed
pr.out <- prcomp(notes)

pr.out$rotation
# matrix whose columns contain the eigen vectors

summary(pr.out)

biplot (pr.out, scale=0)
#Analysis: al analizar la componente principal 2 se puede distinguir 2 agrupaciones, por un lado las variables Matematica 
# y Fisica que tienen un comportamiento aproximado (asignaturas cientificas) y por otro lados las variables Dibujo y Musica
# que tienen un comportamiento aproximado (asignaturas artisticas) y opuesto al de las asignaturas cientificias.
# Se podria decir que Andres y Valentina performan mejor en las asignaturas cientificas mientras que Pedro y Sandra 
# performan mejor en las asignaturas artisticas. Monica y Alina que se encuentran en el medio tienen un performance similar
# en todas las asignaturas. La PC2 podria entonces estar relacionada a las mejores notas de cada alumno
# En la CP1 todas las variables tienen comportamiento muy similar, siendo el de id. espanol un poco menor. Monica en un 
# extremo podria ser el que performo mejor en todas las asignaturas y Juan en el otro extremo el que performo peor
# en todas las asignaturas. En este sentido la CP1 relacionada con el overall performance. 

#compute the proportion of variance explained
pve <- summary(pr.out)$importance[2,]
pve

#plot the proportion of variance explained
plot(pve , xlab=" Principal Component ", ylab="Proportion of Variance Explained ", ylim=c(0,1),type='b') 

#plot the cumulative proportion of variance explained
plot(cumsum(pve), xlab="Principal Component ", ylab="Cumulative Proportion of Variance Explained ", ylim=c(0,1),type='b')

#El codo se muestra en la CP3, 