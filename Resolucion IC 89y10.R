#EJERCICIO 3
#pob infinita, se desconoce la distribucion
n <- 248
p_ <- 0.19 #corresponde al 19%= 47,12
alpha <- 1-0.92
e <- 1-alpha/2
z <- qnorm(e,0,1)
li <- p_ - (z*sqrt((p_*(1-p_))/n))
ls <- p_ + (z*sqrt((p_*(1-p_))/n))
c(li,ls)
#0.1463884 0.2336116

#<IC EJERCICIO 8
#854,8 -754,3 -698,2 -789,2 -401,9 -642,6
val <- c(854.8,754.3,698.2,789.2,401.9,642.6)
#Es normal, la varianza poblacional es desconocida, poblacion infinita
#uso una T student
Xraya <- mean(val)
desvio <- sd(val)
muestra <- 6
alpha <- 1-0.95
e <- 1-alpha/2
#tstudent
z <- qt(e,muestra-1)
li <- Xraya - (z*(desvio/sqrt(muestra)))
ls <- Xraya + (z*(desvio/sqrt(muestra)))
#Resultado: 523.2770 857.0564
c(li,ls)


#EJERCICIO 9
#distribucion normal con un desvio de 1 gr pb inf
sd <- 1
x_ <- 138
n <- 19
alpha <- 1-0.95
e <- 1-alpha/2
z <- qnorm(e,0,1)
li <- x_ - (z*(sd/sqrt(n)))
ls <- x_ + (z*(sd/sqrt(n)))
#Resultado: 137.5504 138.4496
c(li,ls)

#EJERCICIO 10
#pob finita, dist ?, uso factor de correction
N <- 250
n <- 60
x_ <- 950
sd <- 18
fc <- (N-n)/(N-1)
#punto a
alpha <- 1-0.95
e <- 1-alpha/2
z <- qnorm(e,0,1)
li <- x_ - (z*(sd/sqrt(n))*sqrt(fc))
ls <- x_ + (z*(sd/sqrt(n))*sqrt(fc))
c(li,ls)
# resultado a 946.0215 953.9785
#punto b
alpha <- 1-0.98
p_ <- 0.36 #(es 22/n)
e <- 1-alpha/2
z <- qnorm(e,0,1)
li <- p_ - (z*sqrt((p_*(1-p_))/n)*fc)
ls <- p_ + (z*sqrt((p_*(1-p_))/n)*fc)
c(li,ls)
# resultado b 0.2340734 0.4859266



