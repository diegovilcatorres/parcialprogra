parcialprogra
================
diego vilca torres
24/7/2021

\#parte 1 \#pregunta 1

\#tangente hiperbolica x&lt;- seq(-10,10,0.5) y&lt;-
(exp((x))<sup>2-1)/(exp((x))</sup>2+1) plot(x,y,“l”)

\#derivada de tangente hiperbolica x&lt;- seq(-10,10,0.5) y&lt;-
(4\*(exp((x))<sup>2))/((exp((x))</sup>2)+1)^2 plot(x,y,“l”)

\#pregunta 3 Peri&lt;-function(a,b,c){ resultado&lt;- a+b+c
return(resultado) } Peri(4,3,5)

Area&lt;-function(a,b,c){ resultado&lt;-
(((a+b+c)/2)*(((a+b+c)/2)-a)*(((a+b+c)/2)-b)\*(((a+b+c)/2)-c))^0.5
return(resultado) } Area(3,5,8)

\#pregunta 4 Tc&lt;-function(L,n,i,s){ resultado&lt;-
(7\*(L<sup>0.6)*(n<sup>0.6))/((i</sup>0.4)*(s</sup>0.3))
return(resultado) } Tc(3,4,6,8)

\#pregunta 5 a1=c(2,5,1) a2=c(1,-4,-1) a3=c(3,1,-4) cbind(a1,a2,a3)
A=cbind(a1,a2,a3) b1=c(7,-19,4) cbind(b1) B=cbind(b1) inv(A) inv(A)%\*%B

\#parte 2

\#pregunta 1 install.packages(“tidyverse”) library(tidyverse)
install.packages(“ggplot2”) library(ggplot2) install.packages(“sf”)
library(sf) install.packages(“raster”) library(raster) library(rgdal)

setwd(“C:/R/programacion/parcialprogra”) cuenca&lt;-
st\_read(“uh\_datos.shp”) plot(cuenca) datos&lt;- readOGR(dsn = “.”,
layer= “uh\_datos”) dataN&lt;-<datos@data> head(dataN) dataN %&gt;%
group\_by(AAA) %&gt;% summarise(promedio\_AAA=mean(pcp))

\#pregunta 2 library(sf) library(tidyverse) este&lt;- c(272841.7,
272893.6, 272892.5, 272913.8, 272911.2, 272837.5) norte&lt;-
c(8666459.9, 8666456.9, 8666446.1, 8666441.5, 8666399.9, 8666407.9) df
&lt;- cbind.data.frame(este, norte)

poligono &lt;-function(df){ pol &lt;- df %&gt;% st\_as\_sf(coords =
c(“este”, “norte”), crs = 32718) %&gt;% summarise(geometry =
st\_combine(geometry)) %&gt;% st\_cast(“POLY”) plot(pol) } poligono(df)
