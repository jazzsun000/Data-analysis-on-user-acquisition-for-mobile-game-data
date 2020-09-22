#Assume retention rate will follow the power function y=ax^b
y<-c(0.622540167615873,0.382692040561189,0.24980321340927,0.215770708894754,0.126637958975784,0.0826503681066815)
x<-c(1,3,7,14,30,60)
#log(y)=log(a*x^b)=log(a)+b*log(x)
lmResult<-lm(log(y)~log(x) )
#coef(lmResult)
coef(lmResult)["(Intercept)"]
coef(lmResult)["log(x)"]
#Coefficients:
#(Intercept)       log(x)  
#-0.4291      -0.4811 
#log(a) = 6.9316
exp(-0.4291)
#a = 0.6510948
#b = -0.4811 
#y = 0.6510948X^(-0.4811 )
#Calculate the area under power function, we can get the estimate lifetime
#To calculate, integration value of a function, we first define a function (with name f or some other name0 for the function as shown below.
f <-function(x) 0.6510948*(x^(-0.4811 ))
#Lifetime (180):
#Integral (1 -> 180):
integrate(f,1,180)
#It mean the lifetime y = 0.6510948X^(-0.4811 ) =>17.31565
#LTV(180)
17.31565*0.50
#LTV=8.65

f=expression(x^2+3*x)
#To calculate first derivative of f, we use D() function and ‘x’ to specify that derivation has to be carried out with respect to x (Of course, the above expression, f, contains only x as the independent variable).
D(f,'x')
#To calculate, integration value of a function, we first define a function (with name f or some other name0 for the function as shown below.
f<-function(x) x^2+3*x
integrate(f,0,1)
