#!/usr/bin/env python
# coding: utf-8

# In[1]:


a=10
b=20
print(a+b)


# hello python

# In[ ]:


#this is long comment
#and it extends
#to multiple lines
"""this is also a perfect example of multi-line comments""
""this is a multiline comment""


# In[9]:


a = 5; d=10
b, c=3.2, "hello"
x = y = z = "world"

print("value of a=\n",a) #"is string"
print("value of b=",b)
print()
print(b,c,sep=",")
print(c)
c
print(d)


# In[10]:


#division oeperation in python
print(1/3)
print(1//3) #floored division
print(7%3)


# In[12]:


a=5
b=3.2
print(type(b))
print(a+b)
a="5"
b="3.2"
print(a+" "+b)
a="5"
b="3.2"
print(a+b)


# In[15]:


#user input
name=input("enter your name:")
print("hello" ,name)


# In[20]:


x=3
y=5

if x < y:
    print("this is the first block")
    print('x is less than y')
elif x > y:
    print('x is greater than y')
else:
    print('x and y are equal')
print("done")


# In[21]:


if x==y:
    print('x and y are equal')
else:
    if x < y:
        print('x is less than y')
    else:
        print('x is greater than y')
print("done")


# In[31]:


choice=int(float(input("Enter 1/2/3:")))
#choice=round(choice,0)
print(choice)
if choice == 1:
    print('bad guess')
elif choice == 2:
    print('good guess')
elif choice == 3:
    print('close, but not correct')
else :
    print("Enter a correct number")
print("done")


# In[ ]:
x=5;y=8
if (x > 7 and y>7) or x!=y:
    print('x is a positive single-digit number.')
    
#%%
    #user defined function
    
    def squ(a):
        print ("value is = ",a)
        return a*a
        
value=float(input("enter a number:"))
square(value)
#%%

x = 'cake'
y = 'cookie'
x+ '&' +y

#%%
#checinking '=='
x=4
y=2
x==y

#%%

x>y

#%%

## using if esle statement
x=4
y=2
z =(x==y)
if z:
    print("cookie")
else:  print("No cookie")

#%%
# to check length
str1 = "cake & U"
str2 = "404"
len(str1)
# to check if numeric
print(str1.isdigit())
print(str2.isdigit())

#%%

## using if esle statement
x=4
y=2
print(x or y)
print(x and y)
z =(x or y)
if z:
    print("cookie")
else:  print("No cookie")

#%%
import numpy
##import array as arr
a = arr.array("1",[3,6,9])
type(a)

#%%
## Range used to give continuous numbers
numbers = range(1,20)
print(numbers[0])
print(numbers[2])

characters =("python", "scala","spark")
print(characters[0])
print(characters[2])

#%%
for i in range(-1,20,4):
    print(i)
    
#%%
# concatenated lists
estates = ["TN", "Karnataka", "AP"]
estatesUS = ["California","Florida","Texas","Alabama"]
print(estates + estatesUS)

estates.append("Kerala")
print(estates)
type(estates)