# this script will be used to do the code in chapter two of advanced R

#libs
library(lobstr)

# create a new column called 3, that is the sum of 1 and 2
df <- data.frame(runif(3), runif(3))
names(df) <- c(1, 2)

df
df$`3` <- df$`1` + df$`2`
df



# how much memory does Y occupy?
x <- runif(1e6)
y <- list(x, x, x)

# looks like 22.9mb, which is 3x the memory of X
obj_size(y)
# correct answer is about 8 mb

# on which line does a get copied in code below?
a <- c(1, 5, 3, 2)
b <- a
b[[1]] <- 10
# third line?
# correct answer is b[[1]] <- 10




# 2.2.2 exercises ===============

# describe relationship betweenn a, b, c, and d
a <- 1:10
b <- a
c <- b
d <- 1:10
obj_addr(a)
obj_addr(b)
obj_addr(d)
# a, b, and c all point to the same vector
# d points to a different vector,
#  even though the vector is the exact same

# the following code accesses the mean function
# do they reference the same object in memory?
mean
base::mean
get("mean")
evalq(mean)
match.fun("mean")
obj_addr(mean)
obj_addr(base::mean)
obj_addr(get("mean"))
obj_addr(match.fun("mean"))
# looks like they all point to the same object


# R read functions will convert non-synactic names 
# why is this a problem, and how do you prevent it?
# looks like you could prevent it by turning check.names = F
# I suppose it could be problematic if you don't know
# how the names are changed? or if the col names had a meaning?
read.csv()

# what rules does make.names() use?
make.names()
# x is pre-pended, if necessary
# reserved words have a . appended 
# NA is translated to "NA"
# invalid characters are translated to .
# duplicated values are altered by make.unique()








# 2.3.6 problems


# 2 why does traceback show two copies here?
x <- c(1L, 2L, 3L)
tracemem(x)

x[[3]] <- 4

# i have no idea
# maybe because it has to be copied to another location
# because its losing its reference (like is no longer points to the same thing)
# and then gets copied to get modified?