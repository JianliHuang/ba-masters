setwd('c:/data/BUAN6357/HW_2'); source('prep.txt', echo=T)

#codeset from alookanalytics

# parameters
simulations <- 100000 # number of simulations
perimeter <- 1

# randomly generate a point and check if it is in circle
f_point_in_circle <- function(perimeter=1){
  x <- runif(n=1, min=-perimeter, max=perimeter)
  y <- runif(n=1, min=-perimeter, max=perimeter)
  return(list(x=x, 
              y=y,
              in_circle=x^2 + y^2 <= perimeter^2))
}

# Monte Carlo simulations
set.seed(123)
pi_df <- data.frame(x=rep(NA, simulations),
                    y=rep(NA, simulations),
                    in_circle=rep(NA, simulations))

system.time(
  for (i in seq(simulations)){
    my_simulation <- f_point_in_circle()
    pi_df$in_circle[i] <- my_simulation$in_circle
    pi_df$x[i] <- my_simulation$x
    pi_df$y[i] <- my_simulation$y
  }
)
#99.868 seconds run time

my_pi <- 4 * sum(pi_df$in_circle) / nrow(pi_df)

#setting proper variable names for run1
x1 <- pi_df$x
y1 <- pi_df$y
in_circle1 <- pi_df$in_circle
my_pi1 <- my_pi



#codeset 2
set.seed(123) #setting the same seed as prev exp
sims <- 100000  #no of sims
vec_lf <- runif(sims+sims,-1,1) #longform vector
vec_final <- matrix(vec_lf, ncol = 2, byrow = T) #converting to 2 col matrix
x2 <- vec_final[,1] #x coord gen
y2 <- vec_final[,2] #y coord gen
system.time(
  in_circle2 <- (x2^2 + y2^2 <= 1)
)
#.001s run time!

my_pi2 <- 4 * sum(in_circle2) / length(in_circle2)

#checks
all.equal(x1, x2)
all.equal(y1, y2)
all.equal(in_circle1, in_circle2)
all.equal(my_pi1, my_pi2)

source('validate.txt', echo=T)