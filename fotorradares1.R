##Limpiar workspace
rm(list=ls());

#Mensaje de Bienvenida
cat('Ingrese n�mero de alumnos')
n <- as.numeric(scan(file = "", what = "", nmax = 1, quiet=TRUE))

##Generaci�n de variables (Actividad 1)
edad <- round(runif(n,14,15)); sexo <- trunc(rnorm(n,1,0.5)); lic <- rep(0,n); phogar <- round(rnorm(n,3,0.5)); lhogar <- round(rnorm(n,2,0.5)); chogar <- round(rnorm(n,2,0.5)); vhogar <- round(rnorm(n,2,0.5));
for (i in seq(along=edad)){  ##Arreglando la variable sexo para que queden 1s y 0s
	if ( sexo[i]>1 | sexo[i]<0) {
		sexo[i] <- 0}
};
for (i in seq(along=lhogar)){  ##No permitir que existan m�s personas con licencia en el hogar que personas en el hogar
	if (lhogar[i]>phogar[i]) { 
		lhogar[i]=phogar[i]}
};
for (i in seq(along=vhogar)) { ##No permitir que existan personas que no conduzcan su propio auto.
	if (vhogar[i]==0) {
		chogar[i] <- 0}
};

sexonum <- sort(sexo); ##Ordenar por sexo
sexocat <- factor(sexonum, labels=c("Masculino", "Femenino")); ##Creacion de variable categ�rica para la variable sexo
porsex <- c(round((length(which(sexonum==1))/n)*100),round((length(which(sexonum<1))/n)*100));

##Creaci�n de data frame (Actividad 2)
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
scan(file = "", what = "", nmax = 1, quiet=TRUE);

##Exportar tabla de datos actividad 2

write.table(datos, file='simulacion.xls', sep="\t", row.names=FALSE)

##Gerenaci�n de gr�ficos actividad 4
m <- matrix(1:8, 2, 4, byrow=TRUE);
pdf('graficos actividad 3.pdf')
layout(m);
hist(edad, main="Edad", col="red", xlab="edades", ylab="frecuencia");
names(porsex) <- c('Varones', 'Mujeres')
lbls <- paste(names(porsex), porsex)
lbls <- paste(lbls, "%", sep="")
pie(porsex, labels=lbls, main="Distribuci�n alumnos", col=rainbow(length(lbls)))
hist(lic, main="Habitantes Hogar", col="blue", xlab="Alumnos con licencia", ylab="frecuencia")
hist(phogar, main="Personas por hogar", col="red", xlab="Personas por hogar", ylab="frecuencia")
hist(lhogar, main="Personas con licencia p/h", col="green", xlab="Personas con licencia p/h", ylab="frecuencia")
hist(chogar, main="Personas que conducen p/h", col="brown", xlab="Personas que conducen p/h", ylab="frecuencia")
hist(vhogar, main="Vehiculos motorizados p/h", col="yellow", xlab="Vehiculos motorizados p/h", ylab="frecuencia")





