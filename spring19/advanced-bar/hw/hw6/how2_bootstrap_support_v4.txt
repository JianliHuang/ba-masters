###
#
# bootstrap index generation and use
#
# note: broom and dplyr are moving away from bootstrap() support
#   and they appear to be getting rid of do() [?]
#
#   Wickham wrote some VERY slick code which I encountered at 
#
#      https://github.com/tidyverse/dplyr/issues/269 
#
#   and experimented with (the line for "indicies" does most of the
#   heavy lifting and is quite subtle - or at least it was for me, exploring
#   and experimenting with code is always interesting and should be encouraged)
#
#   anyhow, this is some basic how-to code snippits for 2 kinds of bootstrap operations
#
#
###
#
#  History
#
#     20181024 wj Initial code
#     20190401    Update, clean up, and getting ready for data.table bootstrap
#     20190404    minor clean-up, code correction
#
###
#
# generally useful values (can be changed as desired)
#

seed  <- 1 # horrible seed value but serves our purpose here

###
# 
# multiple copies a sequence
#
#
my.seq <- 1:2  # keep it short so you can follow what is happening and get comfortable with the results
n      <- 3

rep(my.seq, n)

###
#
# generate "b" copies of random indicies for an original sample of size "n" (with replacement)
#
#  this is the basis for classic bootstrap operation: generate a collection of 
#       collections of indexs values
#
b <- 3 # number of bootstrap index sequences
n <- 4 # size of original sample

set.seed(seed)

(t <- replicate(b, sample(n, replace=T), simplify=F) )  # borrowed from tidyverse link w/ great reverence

class(t)

t1 <- t[[1]]  # first element of the list (un-named)
t1

# in pure vector form with separate grouping vector (for use with DT[] ) ( note "simplify=T" )
set.seed(seed)

(t <- as.vector(replicate(b, sample(n, replace=T), simplify=T) ) )
(g <- rep(1:b, each=n) )                                            # number of groups (b), rep(n) corrected (20190404)

###
#
# generate "b" copies of random indicies for an original sample size "n" (with replacement)
#
#  AND make sure each index value occurs the same number of times across the 
#      collection of "b" copies
#
# "balanced" boostrap (see: Davison, A. Schechtman, E., "Efficient bootstrap simulation",
#     Biometrika, 73, 3, 555-566 (1986) )
#
# COMMENT:  this code is VERY basic but it WORKS (first criteria)
#           clearer, faster code would be welcomed
#
#
###
#
# use of "i,g" style (used here) works easily with DT[] syntax for amazing speed
#

b <- 3  # number of bootstrap samples
n <- 4  # number of original observations

set.seed(seed)

# work these in parts if required to be sure you understand what is happening
(i <- sample(rep(1:n, b)) ) # generate and shuffle; this style repeats sequence 1:n "b" times
(g <- rep(1:b, each=n) )    # define the sample boundaries

table(i)

(x <- rnorm(n) )        # arbitrary data
# how to use it
m  <- rep(NA,max(g))    # pre-allocate space
for (j in 1:max(g) ) {
    k    <- i[ g == j ] # index values for sample
    m[j] <- mean(x[k])  # calculation on current bootstrap sample
    print (j)
    print (k)
    }
m

###
#
# other code to make this faster (cleaner and faster)
#

b <- 3 # number of bootstrap samples
n <- 4 # number of original observations

set.seed(seed)

i <- sample(rep(1:n, b)) # generate and shuffle, work this in parts if needed, to be sure you see it
g <- matrix(data=i, ncol=n, byrow=T)

# how to use it
m <- rep(NA,nrow(g))   # pre-allocate space
for (j in 1:nrow(g) ) {
    k    <- g[ j, ]    # index values for sample 
    m[j] <- mean(x[k]) # calculation on current bootstrap sample
    print (j)
    print (k)
    }
m

###
#
# with data.table (fastest way at this time)
#
# re-use "i" from matrix() approach
#
g   <- rep(1:b, each=n)            # as seen in the first "balanced" example 
t   <- data.table(i=i, g=g)
(t2 <- t[,.(m=mean(x[i])), by=g] ) # ".()" is alias for "list()" in data.table

###
#
# bootstrap: explicit "train" v. "test" although you can generate "test" for each sample
#
#
# check use of parameter "subset" in lm(), glm(), nls() 
#