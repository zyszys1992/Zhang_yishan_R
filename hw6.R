str(diamonds)
diamonds
#question 2
#use library ggplot to draw a graph between price=y and carat=x
ggplot(diamonds, aes(carat, price)) + geom_point(aes(colour=factor(color)))+labs(title = "Diamonds-Weight to Price by color",x="weight")+
theme(plot.title = element_text(colour = 'blue', face = 'bold'))

#question 3
#use library ggplot to draw a graph between log(price)=y and log(carat)=x
ggplot(diamonds, aes(log(carat), log(price))) + geom_point(aes(colour=factor(color)))+labs(title = "Diamonds-Weight to Price by (linear)",x="weight",y="price")+
theme(plot.title = element_text(colour = 'blue', face = 'bold'))

#question 4
#give mod the value of linear module of log(price) and log(carat)
mod<-lm(log(price)~log(carat),data=diamonds)
#use library ggplot to draw a graph between log(price) and log(carat) after linear simulation
ggplot(diamonds, aes(x=log(carat),y=resid(mod),colour=factor(color))) +geom_point()+theme(
  legend.position = 'top',legend.box = 'horizontal',
  plot.title = element_text(colour = 'blue', face = 'bold')
)+ggtitle("Diamonds-Weight to Price by Color")+xlab("Weight") + ylab("Price Residuals")

#question 5
#give mod the value of linear module of log(price) and log(carat)
mod<-lm(log(price)~log(carat),data=diamonds)
#use library ggplot to draw a graph between log(price) and log(carat) after linear simulation
ggplot(diamonds, aes(x=log(carat),y=resid(mod),colour=factor(color))) +geom_point()+theme(
  legend.position = 'top',legend.box = 'horizontal',
  plot.title = element_text(colour = 'blue', face = 'bold')
)+ggtitle("Diamonds-Weight to Price by Color")+xlab("Weight") + ylab("Price Residuals")

#use grid from library and give p1 the density with histogram and p2 the count with histogram
p1<-ggplot(diamonds, aes(x=price,..density..,fill=color))+geom_histogram(binwidth=250)+theme(legend.position='none')

p2<-ggplot(diamonds, aes(x=carat,fill=color))+geom_histogram(binwidth=0.1)+theme(legend.position='none')

vie1=viewport(x=0.2, y=0.2, w=0.3, h=0.3)
vie2=viewport(x=0.8, y=0.6, w=0.3, h=0.3)
print(p1,vp=vie1)
print(p2,vp=vie2)




