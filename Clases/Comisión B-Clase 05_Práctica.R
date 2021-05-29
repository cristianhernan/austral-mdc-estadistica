
## EJERCICIO 1

NC = 0.90
LI = 1250 - qnorm((1+NC)/2,mean = 0,sd = 1) * 5/sqrt(38)
LS = 1250 + qnorm((1+NC)/2,mean = 0,sd = 1) * 5/sqrt(38)
c(LI,LS)

(LS + LI)/2
(LS - LI)/2

qnorm(0.975,mean = 0,sd = 1)

qt(0.975,df = 12-1)

NC = 0.90
A = qchisq(1-(1+NC)/2,df = 11)
B = qchisq((1+NC)/2,df = 11)
n = 12
S = 135
LI = ((n-1)*S^2)/B
LS = ((n-1)*S^2)/A
c(LI,LS)

## EJERCICIO 7

# Parte A

NC = 0.90
a= (1+NC)/2
n=22
s=2
z=qnorm(a,mean = 0,sd = 1)
LI = 150 - z * s/sqrt(n)
LS = 150 + z * s/sqrt(n)
c(LI,LS)

# Parte B
LI = 148.41
LS = 150.85
(LS + LI)/2
(LS - LI)/2

z <- (LS - LI)/2 * sqrt(18)/2
pnorm(q = z,mean = 0,sd = 1)

alpha = (pnorm(q = z,mean = 0,sd = 1) - 1)* -2
1- alpha 


## CALCULAR CON CHI CUADRADO
pchisq(0.95,df = 20)

pnorm(q = 12,mean = 10,sd = 2) - pnorm(q = 8,mean = 10,sd = 2)
pnorm(q = 14,mean = 10,sd = 2) - pnorm(q = 6,mean = 10,sd = 2)
pnorm(q = 16,mean = 10,sd = 2) - pnorm(q = 4,mean = 10,sd = 2)
pnorm(q = 20,mean = 10,sd = 2) - pnorm(q = 0,mean = 10,sd = 2)

x_raya = 172
mu = 174
sigma = 20
n = 50

pnorm(x_raya,mean = mu, sd = sigma/sqrt(n))

z = (x_raya - mu)/(sigma/sqrt(n))
pnorm(q = z,mean = 0,sd = 1)

qnorm(0.20,mean = 0,sd = 1)
