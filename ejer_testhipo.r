# Se  pudo  comprobar  que  el  año  pasado,  los precios  de una  determinada  canasta 
# de productos se distribuye normalmente con media $1780, y un desvío estándar de $110. 
# Este año, una muestra de 40 ventas, proporcionó un precio promedio de $1900.  
# Con  un  nivel  de  significación  del  5%, 
# ¿se  puede  afirmar  que  el  precio promedio de estos productos, ha aumentado, 
# con respecto al precio promedio del año pasado?

mu <- 1780
sigma <- 110
n <- 40
Xraya <- 1900
a <- 0.05

#Ho -> Xraya < mu
#Ha -> Xraya > mu

#Poblacion infinita, varianza conocida.
Zt <- qnorm(1-a/2)
Zt
Ze <- ((Xraya-mu)/(sigma/sqrt(n)))
Ze
#Ze > Zt por lo tanto se rechaza Ho

############EJERCICIO 2#################
# El  encargado  de  personal  ha  informado  que  el  nivel  medio  de  ausencia
# de  los empleados  durante  el  trimestre  pasado  fue  inferior  a  15  días. 
# Para  ello  se seleccionó  una  muestra  de  50  empleados,  quienes  tuvieron
# un  promedio  de ausencias  de  12,2  días  y  un  desvío  de  6,2  días.  
# Se  supone  que  la  población  se distribuye normalmente. Con un nivel de
# significación del 5%, determine si el informe del encargado puede considerarse
# válido.

mu <- 15
n <- 50
Xraya <- 12.2
s <- 6.2
a <- 0.05

#Ho -> Xraya > mu
#Ha -> Xraya < mu

#poblacion infinita, varianza pob desconocida.
#se unsa una T student

Tt <- qt(1-a/2,n-1)
Te <- ((Xraya-mu)/(s/sqrt(n)))

#Te < Tt entonces se rechaza Ho

############EJERCICIO 3#################

# Del padrón nacional de una importante obra social, se toman al azar, 9 pacientes
# que  padecen  una  misma  enfermedad,  y  se  les  indica  que  realicen  una
# misma tarea,  cuyo  tiempo  de  realización  tiene  distribución  normal. 
# El  tiempo  promedio para la realización dela tarea fue de 17 minutos, 
# con desvío de 2 minutos. Con un nivel de significación del 1%,
# ¿estos datos permiten afirmar que la varianza es menor a 5?

n <- 9
Xraya <- 17
s <- 2
a <- 0.01
sigma2 <- 5

#Ho -> sigma2 > 5
#Ha -> sigma2 < 5

#Normal, mu desconocida uso chi cuadrado
x2t <- qchisq(a,df=n-1)
x2e <-((n-1)*s^2)/(sigma2) 

#como x2e > x2t, no rechazo Ho


