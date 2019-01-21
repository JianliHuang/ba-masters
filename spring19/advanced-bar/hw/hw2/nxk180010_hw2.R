#setwd(“c:/data/BUAN6357/HW_2”); source(“prep.txt”, echo=T)

#codeset 1

# parameters
simulations <- 10000 # number of simulations
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

my_pi <- 4 * sum(pi_df$in_circle) / nrow(pi_df)


#codeset 2
set.seed(123)
s <- 10000
t <- runif(s+s,-1,1)
t_ <- matrix(t, ncol = 2, byrow = T)
x2 <- t_[,1]
y2 <- t_[,2]
system.time(
  in_circle2 <- (x2^2 + y2^2 <= 1)
)
my_pi2 <- 4 * sum(in_circle2) / length(in_circle2)

#checks
x1 <- pi_df$x
y1 <- pi_df$y
in_circle1 <- pi_df$in_circle
my_pi1 <- my_pi
all.equal(x1, x2)
all.equal(y1, y2)
all.equal(in_circle1, in_circle2)
all.equal(my_pi1, my_pi2)

#source(“validate.txt”, echo=T)