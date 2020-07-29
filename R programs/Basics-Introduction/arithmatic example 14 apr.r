a = c(1,3,5,7)
b = c(1,2,4,8)
a+b
a-b
a*b
a/b
a>b
a<b
a==b
a<=b
a>=b

u = c(10,20,30)
v = c(1,2,3,4,5,6,7,8,9)
u + v
#vector index
# we retrive a values u=in a vector by declaring the index inside a single
# square bracket [] operator.
s = c("aa","bb","cc","dd","ee")
s[3]

s[-3]
# out of range index
#if an index is out of range misssing value will be reported
s[10]

#numeric index vector
s = c("aa","bb","cc","dd","ee")
s[c(2,3)]

#duplicate
s[c(2,3,3)]

#out of order indexes
s[c(2,1,3)]

#range index
s[2:4]






