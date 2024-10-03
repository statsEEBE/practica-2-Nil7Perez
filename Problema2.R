#Codigo para problema 2

mis_dades<- iris
mis_dades

x<-mis_dades$Petal.Length
y<-mis_dades$Sepal.Length
plot(x,y)


x_bar=mean(x)
y_bar=mean(y)
m<-sum((x-x_bar)*(y-y_bar))/sum((x-x_bar)^2)
b<- y_bar-m*x_bar
m*1.5+b

x_pred<-x
y_pred<- m*x_pred+b

plot(x,y)
lines(x_pred,y_pred)

r2<-sum((y_pred-y_bar)^2)/sum((y-y_bar)^2)
r<-sqrt(r2)

mod<-lm(y~x)#es posa primer eix y i despres eix x
cor.test(x,y)# coeficient de determinacio r= arrel(r^2)
summary(mod)
y_pred2<-predict(mod,data.frame(x=1.5))
