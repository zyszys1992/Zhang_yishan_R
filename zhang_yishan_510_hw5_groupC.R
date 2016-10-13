title: "510hw_5"
output: html_document
---
  #I finished the homework of group C
  #I sent you an email that I wanted to move down to group C 
  data(diamonds)
attributes(diamonds)
data.frame(diamonds)

#question 1
#define a function as function1 which can give all attributes and methods and the number of
#columns of data
#parameter is x which is data's name
function1<-function(x){
  #make the data a string 
  print(str(x))
  #count how many columns of data 
  print(ncol(x))
  #print the methods of data.frame
  print(methods(class="data.frame"))
}




#question 2
#define a function as function2 which can give the number of rows in data
#parameter is x which means data you put in.
function2<-function(x)
{
  nrow(x)#how many rows of data
}



#question 3
#define a function as function3 which gives the columns' names of data 
#parameter is x which means data you put in.
function3<-function(x){
  #give columns names 
  colnames(x)
}



#question 4
#define a function as function4 which can give the type of each columns
#parameter is x which means data you put in.
function4<-function(x){
  #lapply function works with class to give types of each columns 
  lapply(x, class)
}



#question 5
#define a function as function5 which can loop through data to give means of each numeric 
#columns of data 
#parameter is x which means data you put in.
function5<-function(x)
{#for loop start from 1 to the number of columns of data 
  for( i in 1:ncol(x))
    #determine if the columns of i is numeric
    if (is.numeric(x[2,i]))
      #if yes, print the columns means of numeric columns as matrix format 
      print(as.matrix(colMeans(x[i])))
}




#question 6
#open the package "plyr"
#define a function as function6 which can count each numeber or letter's frequency 
#parameter is x which means data you put in.
function6<-function(x){
  #give number from 1 to columns' number 
  numbers<-c(1:ncol(x))
  #give a the value of columns names 
  a<-colnames(x)
  #for loop gives i start from the frist number in numbers 
  for (i in numbers)
    #count function to count each columns' variable's frequency
    #a[i]represent ith column
  {str(count(x,vars=a[i]))}
}


```{r}
#question 7
#define a function as function7 which can 	determine	the	number	
#of	rows	containing	NA	(missing	value)	in	each	column 
#parameter is x which means data you put in.
#because there is no missing value in diamonds data so I set 0 as NA
#count how many rows with NA with setting 0 as NA
function7<-function(x){
  #intial value of nacount1 as 0
  nacount1=0
  #give NA to each variable of value 0
  x[x==0]<-NA
  #for loop through data 
  for (i in 1:nrow(x))
   #determine if ith row, each columns are NA or not  
  {if (is.na(x[i,1:ncol(x)]))
    #if yes add 1 to nacount1
  {nacount1=nacount1+1
  }}
  print(nacount1)
  
  
  #initial value of nacount2 as 0
  nacount2=0
  #give NA to each variable of value 0
  x[x==0]<-NA
  #for loop through 1st row to the last and 1st column to the last 
  for (i in 1:nrow(x))
    for (j in 1:ncol(x))
      #determine if x[i,j] is NA, and each row with NA only counts 1
    {if (is.na(x[i,j]))
    {nacount2=nacount2+1
    break}}
  print(nacount2)
  #print the percentage of nacount2 
  print(nacount2/nrow(x))
}
```







#question 8
#define a function as function8 which can give a "-" between each columns names and 
#also connect two columns' variable by "-"
#parameter is x which means data you put in.
function8<-function(x){
  #for loop through i which start from the 1st to the ncol(x)-1
  for (i in 1:(ncol(x)-1)){
    #for loop through j which start from the i+1 to the ncol(x)
    for(j in (i+1):ncol(x) ){
      #paste function to connect two columns by "-"
      pairs=matrix(paste(colnames(x)[i],colnames(x)[j],sep="-"),ncol=1,nrow(x))
      pairs2=matrix(paste(matrix(x[,i],ncol=1,nrow=nrow(x)),matrix(x[,j],ncol=1,nrow=nrow(x)),sep="-"),ncol=1,nrow=nrow(x))
      print(data.frame(pairs,pairs2))
    }}
  #for loop through i which start from the 1st to the ncol(x)-1
  for (i in 1:(ncol(x)-1))
    #for loop through j which start from the i+1 to the ncol(x)
    for(j in (i+1):ncol(x)) 
      #determine if two columns are numeric, give the correlation of them 
      if (is.numeric(x[,i]) & is.numeric(x[,j]))
        print(paste(colnames(x)[i],colnames(x)[j],cor(x[,i],x[,j]),sep="-"))
}



