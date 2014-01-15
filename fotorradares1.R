##Limpiar workspace

rm(list=ls());

#Mensaje de Bienvenida

cat('Ingrese número de alumnos:')
n <- as.numeric(scan(file = "", what = "", nmax = 1, quiet=TRUE))
if (length(n)==0) { cat('Ingrese un valor correcto')} else { #Mensaje de error
cat('Los códigos de los cursos son los siguientes:') 
scan(file = "", what = "", nmax = 1, quiet=TRUE)
cat('[1] 6° Básico, [2] 7° Básico, [3] 8° Básico')
scan(file = "", what = "", nmax = 1, quiet=TRUE)
cat('[4] 1° Medio ,[5] 2° Medio, [6] 3° Medio Plan Común')
scan(file = "", what = "", nmax = 1, quiet=TRUE)
cat('[7] 3° Medio Electivo, [8] 4° Medio Plan Común, [9] 4° Medio Electivo')
scan(file = "", what = "", nmax = 1, quiet=TRUE)
cat('Ingrese el código del curso de los alumnos:')
curso <- as.numeric(scan(file = "", what = "", nmax = 1, quiet=TRUE))
if (length(curso)==0) {cat('Ingrese un valor correcto')} else if (curso %in% 1:9) { 

##Generación de variables (Actividad 1)

cedad <- as.numeric(c(11,12,13,14,15,16,17,18,18)) # vector de edades promedio para generar variable edades

for (i in seq(along=cedad)) { # establecer limite maximo y minimo de edad por curso
	if (curso==i) { lmin <- cedad[i]-1 ; lmax <-cedad[i]+1} }

if (curso <= 7) { lic <- rep(0,n)} else { lic <- round(rnorm(n,0,1))  #Posibilidad de tener licencia solo en cuarto medio.

for (i in seq(along=lic)) { 
	if (lic[i]>1|lic[i]<0 ) { 
		lic[i] <- 0} } }  # Arreglar muestra para dejar 1s y 0s.

edad <- round(runif(n,lmin,lmax)); sexo <- trunc(rnorm(n,1,0.5)); phogar <- round(rnorm(n,3,0.5)); lhogar <- round(rnorm(n,2,0.5)); chogar <- round(rnorm(n,2,0.5)); vhogar <- round(rnorm(n,2,0.5));

for (i in seq(along=sexo)){  ##Arreglando la variable sexo para que queden 1s y 0s
	if ( sexo[i]>1 | sexo[i]<0) {
		sexo[i] <- 0}
};
for (i in seq(along=lhogar)){  ##No permitir que existan más personas con licencia en el hogar que personas en el hogar
	if (lhogar[i]>phogar[i]) { 
		lhogar[i]=phogar[i]}
};
for (i in seq(along=vhogar)) { ##No permitir que existan personas que no conduzcan su propio auto.
	if (vhogar[i]==0) {
		chogar[i] <- 0}
};

sexonum <- sort(sexo); ##Ordenar por sexo
for (i in seq(along=sexonum))
if (identical(sexonum,rep(0,n))) { sexocat <- factor(sexonum, labels=c("Masculino"))} #Arreglar error cuando son todos iguales.
else if (identical(sexonum,rep(1,n))) { sexocat <- factor(sexonum, labels=c("Femenino"))} #Arreglar error cuando son todos iguales.
else {sexocat <- factor(sexonum, labels=c("Masculino", "Femenino"))}; #Creacion de variable categórica para la variable sexo
porsex <- c(round((length(which(sexonum==1))/n)*100),round((length(which(sexonum<1))/n)*100)); #Vector con frecuencia porcentual de variable sexo

##Creación de data frame (Actividad 2)
datos <- data.frame(EDAD=edad, SEXO=sexocat, LICENCIA=lic, HABITANTESHOGAR=phogar, LICENCIAHOGAR=lhogar, CONDUCTORESHOGAR=chogar, VEHICULOSHOGAR=vhogar);

##Display de datos
print(datos);

##Calculo de datos actividad 4.
porlhogar <- rep(0,n); porchogar <- rep(0,n)
for (i in seq(along=porlhogar)) { ##Calculo aproximado de vector de porcentajes de personas que tienen licencia de conducir por hogar
	porlhogar[i] <- round((lhogar[i]/phogar[i])*100)
}
for (i in seq(along=porchogar)) { ##Calculo aproximado de vector de porcentajes de personas que conducen por hogar
	porchogar[i] <- round((chogar[i]/phogar[i])*100)
}
#Calculo de medianas y medias.

edad4 <- c(round(mean(edad)),round(median(edad)));
phogar4 <- c(round(mean(phogar)),round(median(phogar)));
porlhogar4 <- c(round(mean(porlhogar)),round(median(porlhogar))); 
porchogar4 <- c(round(mean(porchogar)),round(median(porchogar)));
vhogar4 <- c(round(mean(vhogar)),round(median(vhogar))); 

##Display de resultados

print('La media y la mediana de las edades son:');
scan(file = "", what = "", nmax = 1, quiet=TRUE);
print(edad4);
scan(file = "", what = "", nmax = 1, quiet=TRUE);

print('La media y la mediana de la cantidad de habitantes por casa son:');
scan(file = "", what = "", nmax = 1, quiet=TRUE);
print(phogar4);
scan(file = "", what = "", nmax = 1, quiet=TRUE);

print('La media y la mediana de los porcentajes de personas por casa que tienen licencia de conducir son:');
scan(file = "", what = "", nmax = 1, quiet=TRUE);
print(porlhogar4);
scan(file = "", what = "", nmax = 1, quiet=TRUE);

print('La media y la mediana de los porcentajes de personas por casa que conducen a diario son:');
scan(file = "", what = "", nmax = 1, quiet=TRUE);
print(porchogar4);
scan(file = "", what = "", nmax = 1, quiet=TRUE);

print('La media y la mediana de la cantidad de vehiculos motorizados por hogar son:');
scan(file = "", what = "", nmax = 1, quiet=TRUE);
print(vhogar4);

##Exportar tabla de datos actividad 2

write.table(datos, file='simulacion.xls', sep="\t", row.names=FALSE)

##Gerenación de gráficos actividad 4

m <- matrix(1:8, 2, 4, byrow=TRUE);
pdf('graficos actividad 3.pdf')
layout(m);
hist(edad, main="Edad", col="red", xlab="edades", ylab="frecuencia");
names(porsex) <- c('Varones', 'Mujeres')
lbls <- paste(names(porsex), porsex)
lbls <- paste(lbls, "%", sep="")
pie(porsex, labels=lbls, main="Distribución alumnos", col=rainbow(length(lbls)))
hist(lic, main="Habitantes Hogar", col="blue", xlab="Alumnos con licencia", ylab="frecuencia")
hist(phogar, main="Personas por hogar", col="red", xlab="Personas por hogar", ylab="frecuencia")
hist(lhogar, main="Personas con licencia p/h", col="green", xlab="Personas con licencia p/h", ylab="frecuencia")
hist(chogar, main="Personas que conducen p/h", col="brown", xlab="Personas que conducen p/h", ylab="frecuencia")
hist(vhogar, main="Vehiculos motorizados p/h", col="yellow", xlab="Vehiculos motorizados p/h", ylab="frecuencia")


} else { cat('Por favor introduzca un valor correcto')}}#Mensaje de error 








