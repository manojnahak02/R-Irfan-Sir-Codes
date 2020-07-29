##Variable Assignment
# = or <- means assign
x=1
x
###Function and c is combining values iinto a vector or list 
c(1,2,3)
a=c(1,2,3)
##comment
 1 + 1      # this is a comment 


##to understand  what is c function
help(c)
 ##If you are not sure about the name of the function you are looking for, 
 #you can perform a fuzzy search with the apropos function.
 apropos("nova")  
 
Type of Data in r
1)numeric
2)integer
3)Complex
4)logical
5)Charater

##Numeric
x = 10.5       # assign a decimal value 
 x              # print the value of x 
[1] 10.5 
###To check the ClASS OF X
 class(x)       # print the class name of x 
[1] "numeric"

 k = 1 
  k              # print the value of k 
 [1] 1 
  class(k)       # print the class name of k 
 [1] "numeric"

  
####tO CONFIRM whether it is integer or not    
  is.integer(k)  # is k an integer? 
  
  
### Integer 
 # In order to create an integer variable in R, 
  #we invoke the as.integer function. We can be assured that 
  #y is indeed an integer by applying the is.integer function.
  
y = as.integer(3) 
y              # print the value of y 
[1] 3 
   
class(y)       # print the class name of y 
[1] "integer" 
is.integer(y)  # is y an integer?   
#Incidentally, we can convert a numeric value into an integer 
##with the same as.integer function.


as.integer(3.14)    # coerce a numeric value    

###complex Value
z = 1 + 2i     # create a complex number 
 z              # print the value of z 
[1] 1+2i 
 class(z)       # print the class name of z 
[1] "complex"
 
# Logical Value
##A logical value is often created via comparison between variables.
x = 1; y = 2   # sample values 
z = x > y      # is x larger than y? 
z              # print the logical value 
[1] FALSE 
class(z)       # print the class name of z 
 [1] "logical"
##Standard logical operations are "&" (and), "|" (or), and "!" (negation).
u = TRUE; v = FALSE 
u & v          # u AND v 
[1] FALSE 
u | v          # u OR v 
[1] TRUE 
!u             # negation of u 
[1] FALSE
##Charater
#A character object is used to represent string values in R. 
#We convert objects into character values with the as.character() function:
x = as.character(3.14) 
x              # print the character string 
[1] "3.14" 
class(x)       # print the class name of x 
[1] "character"
#Two character values can be concatenated with the paste function.

fname = "Joe"; lname ="Smith" 
paste(fname, lname) 
[1] "Joe Smith"

##To extract a substring, we apply the substr function.
#Here is an example showing how to extract the substring between 
#the third and twelfth positions in a string.

substr("Mary has a little lamb.", start=3, stop=12) 
[1] "ry has a l"

#And to replace the first occurrence of the word "little" by 
#another word "big" in the string, we apply the sub function.

sub("little", "big", "Mary has a little lamb.") 
[1] "Mary has a big lamb."

