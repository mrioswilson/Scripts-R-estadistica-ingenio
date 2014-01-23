##Actividad 2

##Limpiar el espacio de trabajo
rm(list=ls());

##Creaci�n de variables y tablas de datos (Actividades 2.1 y 2.2)

col1 <- as.numeric(c('2000','1698')); col2 <- as.numeric(c('2001','1562')); col3 <- as.numeric(c('2002','1549'));
col4 <- as.numeric(c('2003','1703')); col5 <- as.numeric(c('2004','1757')); col6 <- as.numeric(c('2005','1626'));
col7 <- as.numeric(c('2006','1652')); col8 <- as.numeric(c('2007','1645')); col9 <- as.numeric(c('2008','1782')); col10 <- as.numeric(c('2009','1508'));
tabla1 <- data.frame(col1,col2,col3,col4,col5,col6,col7,col8,col9,col10); #Fallecidos acccidentes por a�o
datos1 <- as.numeric(c(col1[2],col2[2],col3[2],col4[2],col5[2],col6[2],col7[2],col8[2],col9[2],col10[2])) #Fallecidos accidentes

##Obtenci�n de media y desviaci�n est�ndar (Actividades 2.1 y 2.2) (Calculo y display)

n <- length(datos1) # numero total de datos
var1 <- 0 #Contador para la varianza
media1 <- sum(datos1)/n; #Calculando medida directamente
for (i in seq(along=datos1)) {  var1 <- var1 + ((1/n)*((datos1[i]-media1)^2)) } #Calculando desviaci�n est�ndar directamente
devs1 <- sqrt(var1); 
print('la media de la muestra es:');
scan(file = "", what = "", nmax = 1, quiet=TRUE);
print('suma (datos1) / N');
print(media1)
print('Aproximadamente:')
print(round(media1))
scan(file = "", what = "", nmax = 1, quiet=TRUE);
print('la desviaci�n est�ndar de la muestra es:');
scan(file = "", what = "", nmax = 1, quiet=TRUE);
print('raiz( (1/n)* suma_i=1_i=N(xi - promedio)^2 )');
print(devs1);
print('Aproximadamente:');
print(round(devs1));
scan(file = "", what = "", nmax = 1, quiet=TRUE);

##Creaci�n de variables y tablas de datos (Actividad 2.5)

col1 <- as.numeric(c('2000','2128855')); col2 <- as.numeric(c('2001','2176501')); col3 <- as.numeric(c('2002','2218062'));
col4 <- as.numeric(c('2003','2195878')); col5 <- as.numeric(c('2004','2298620')); col6 <- as.numeric(c('2005','2351662'));
col7 <- as.numeric(c('2006','2657892')); col8 <- as.numeric(c('2007','2824570')); col9 <- as.numeric(c('2008','3023050')); col10 <- as.numeric(c('2009','3139088'));

tabla2 <- data.frame(col1,col2,col3,col4,col5,col6,col7,col8,col9,col10); #Parque vehicular por a�o
datos2 <- as.numeric(c(col1[2],col2[2],col3[2],col4[2],col5[2],col6[2],col7[2],col8[2],col9[2],col10[2])) #Parque Vehicular


##Obtenci�n de media, desviaci�n est�ndar y correlaci�n (Actividades 2.5) (Calculo y display)

n <- length(datos1) # numero total de datos
var2 <- 0 #Contador para la varianza
media2 <- sum(datos2)/n; #Calculando medida directamente
for (i in seq(along=datos2)) {  var2 <- var2 + ((1/n)*((datos2[i]-media2)^2)) } #Calculando desviaci�n est�ndar directamente
devs2 <- sqrt(var2); 
print('la media de la muestra es:');
scan(file = "", what = "", nmax = 1, quiet=TRUE);
print('suma (xi) / N');
print(media2) 
print('Aproximadamente:')
print(round(media2))
scan(file = "", what = "", nmax = 1, quiet=TRUE);
print('la desviaci�n est�ndar de la muestra es:');
scan(file = "", what = "", nmax = 1, quiet=TRUE);
print('raiz( (1/n)* suma_i=1_i=N(xi - promedio)^2 )');
print(devs2);
print('Aproximadamente:');
print(round(devs2));
scan(file = "", what = "", nmax = 1, quiet=TRUE);
covar2 <- 0 #calcular covarianza directamente
for (i in seq(along=datos2)) { covar2 <- covar2 +((1/n)*((datos1[i]-media1))*((datos2[i]-media2)))}
print ('la covarianza entre las variables fallecidos en accidentes de tr�nsito y parque automotriz es:')
scan(file = "", what = "", nmax = 1, quiet=TRUE);
print( '(1/n)* suma_i=1_i=N(((xi - media(xi)))*(yi - media(yi)))');
print(covar2)
print('Aproximadamente:');
print(round(covar2));
scan(file = "", what = "", nmax = 1, quiet=TRUE);
print ('el coeficiente de correlaci�n lineal (R) entre las variables fallecidos en accidentes de tr�nsito y parque automotriz es:')
r2 <- covar2/(devs1*devs2); #calcular coeficiente de correlación lineal directamente
print(r2)
print('Aproximadamente:');
print(round(r2));
print('R^2 cercano a 0 implica que la correlaci�n lineal es nula');
modelo <- lm(datos1~datos2); #Ajustar modelo lineal

#Generar gr�fico en PDF
pdf('gr�fico actividad 2-5.pdf');
plot(datos1~datos2, xlab='Parque Automotriz 2000-2009 por a�o', ylab='Fallecidos en accidentes de tr�nsito 2000-2009 por a�o', main='Fallecidos accidentes de tr�nsito v/s Parque automotriz (2000-2009)');
abline(modelo);
grid();




