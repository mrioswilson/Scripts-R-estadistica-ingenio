##Actividad 2

#Limpiar el espacio de trabajo
rm(list=ls());

#Creación de variables y tablas de datos (revisar datos)

col1 <- c('2000','1698'); col2 <- c('2001','1562'); col3 <- c('2002','1549');
col4 <- c('2003','1703'); col5 <- c('2004','1757'); col6 <- c('2005','1626');
col7 <- c('2006','1652'); col8 <- c('2007','1645'); col9 <- c('2008','1782'); col10 <- c('2009','1508');
tabla <- data.frame(col1,col2,col3,col4,col5,col6,col7,col8,col9,col10);
datos <- as.numeric(c(col1[2],col2[2],col3[2],col4[2],col5[2],col6[2],col7[2],col8[2],col9[2],col10[2]))

#Obtención de media y desviación estándar

media <- mean(datos);
devs <- sd(datos); #(revisar valor)
print('la media de la muestra es:');
scan(file = "", what = "", nmax = 1, quiet=TRUE);
print('suma (datos) / N');
scan(file = "", what = "", nmax = 1, quiet=TRUE);
print(media)
scan(file = "", what = "", nmax = 1, quiet=TRUE);
print('Aproximadamente:')
print(round(media))
scan(file = "", what = "", nmax = 1, quiet=TRUE);
print('la desviación estándar de la muestra es:');
scan(file = "", what = "", nmax = 1, quiet=TRUE);
print('raiz( (1/n)* suma_i=1_i=N(xi - promedio)^2 )');
scan(file = "", what = "", nmax = 1, quiet=TRUE);
print(devs);
scan(file = "", what = "", nmax = 1, quiet=TRUE);
print('Aproximadamente:');
print(round(devs));
scan(file = "", what = "", nmax = 1, quiet=TRUE);

#