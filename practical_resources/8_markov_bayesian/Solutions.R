## ----setup,echo=TRUE,include=FALSE---------------------------------------
in_dir <- function(dir, code) {
  cur <- getwd()
  setwd(dir)
  on.exit(setwd(cur))

  force(code)
}
options(scipen=99)

### 8. 1

## ------------------------------------------------------------------------

p <- rbeta(1000, 16, 26) 


## ------------------------------------------------------------------------

x <- rbinom(1000, 40, (1-p)^2)


### 8.2

## ------------------------------------------------------------------------

a <- c(211, 61, 2, 2)
p_rep <- matrix(nrow = 1000, ncol = 4)

for (i in 1:4)
    p_rep[,i] <- rgamma(1000, a[i], 1)

p_rep <- p_rep / rowSums(p_rep)
p_rep[1:3, ]


## ------------------------------------------------------------------------

a <- c(211, 61, 2, 2)

# install.packages("VGAM") # if not already installed
library(VGAM)

p_rep <- rdiric(1000, a)
p_rep[1:3, ]


### 8.3

## ------------------------------------------------------------------------

a <- matrix(nrow = 4, ncol = 4) # or array(dim=c(4,4))
state_names <- c("STW","UTW","Ex","TF")
rownames(a) <- colnames(a) <- state_names
a["STW",] <- c(1,1,1,1)
a["UTW",] <- c(1,1,1,1)
a["Ex",] <- c(33,33,1,33)
a["TF",] <- c(0,0,0,1)


## ------------------------------------------------------------------------

N <- rbind(
    "STW" = c(210,60,1,1),
    "UTW" = c(88,641,4,13),
    "Ex" = c(1,0,0,1),
    "TF" = c(0,0,0,0))
a_post <- a + N 


## ------------------------------------------------------------------------

lambda_rep <- array(dim = c(1000, 4, 4))
dimnames(lambda_rep) <- list(NULL, state_names, state_names)

for (i in 1:4) {
    lambda_rep[,i,] <- rdiric(1000, a_post[i,])
}



## ------------------------------------------------------------------------
# posterior mean transition probability matrix
apply(lambda_rep, c(2,3), mean)


# c.
## ------------------------------------------------------------------------
# probabilities of occupancy

J <- 12
nsim <- 1000
S <- 4  
pi <- array(dim = c(nsim, S , J+1))

for (i in 1:nsim) {
    pi[i,,1] <- c(1,0,0,0)
    for (j in 2:(J+1)) {
        for (s in 1:S){
            pi[i,s,j] <- pi[i,,j-1] %*% lambda_rep[i,,s]
        }
    }
}


## ------------------------------------------------------------------------

for (i in 1:nsim) {
    pi[i,,1] <- c(1,0,0,0)
    for (j in 2:(J+1)){
        pi[i,,j] <- pi[i,,j-1] %*% lambda_rep[i,,]
    }
}


## ------------------------------------------------------------------------

apply(pi, c(2,3), mean)


# d.
## ------------------------------------------------------------------------

state_costs <- c(STW = 7.96,
                 UTW = 7.96,
                 Ex = 1000,
                 TF = NA)

state_util <- c(STW = 1,
                UTW = 0,
                Ex = 0,
                TF = 0)

cost <- eff <- numeric(nsim)

for (i in 1:nsim) {
    cts <- ets <- numeric(J)
    for (j in 1:J) {
        state_costs["TF"] <- state_costs[1:3] %*% pi[i,1:3,j]
        cts[j] <- sum(pi[i,,j] * state_costs) # costs for time J, summed over states
        ets[j] <- sum(pi[i,,j] * state_util) # effects for time J, summed over states
    }
    cost[i] <- sum(cts) # costs summed over all times and states, for this PSA sample
    eff[i] <- sum(ets)  # effects ...
}
summary(cost)
summary(eff)


## ------------------------------------------------------------------------

library(ggplot2)
qplot(cost, eff, 
      xlab = "Expected total cost over 12 weeks", 
      ylab = "Expected number of successfully treated weeks out of 12")
## alternatively using base R graphics, simply:
## plot(cost, eff)


## ------------------------------------------------------------------------


N <- rbind(
  "STW" = c(66, 32, 0, 2),
  "UTW" = c(42, 752, 5, 20),
  "Ex" = c(0, 4, 1, 0),
  "TF" = c(0, 0, 0, 0)
)

a_post <- a + N 
lambda_rep_fp <- array(dim=c(1000, 4, 4))
dimnames(lambda_rep_fp) <- list(NULL, state_names, state_names)

for (i in 1:4) {
    lambda_rep_fp[,i,] <- rdiric(1000, a_post[i,])
}


## ------------------------------------------------------------------------

pi_fp <- array(dim = c(nsim, S , J+1))

for (i in 1:nsim) {
    pi_fp[i,,1] <- c(1,0,0,0)
    for (j in 2:(J+1)) {
        for (s in 1:S){
            pi_fp[i,s,j] <- pi_fp[i,,j-1] %*% lambda_rep_fp[i,,s]
        }
    }
}
state_costs_fp <- c(STW = 2.38,
                    UTW = 2.38,
                    Ex = 1000,
                    TF = NA)

cost_fp <- eff_fp <- numeric(nsim)

for (i in 1:nsim) {
    cts <- ets <- numeric(J)
    for (j in 1:J) {
        state_costs_fp["TF"] <- state_costs_fp[1:3] %*% pi_fp[i,1:3,j]
        cts[j] <- sum(pi_fp[i,,j] * state_costs_fp) # costs for time J, summed over states
        ets[j] <- sum(pi_fp[i,,j] * state_util) # effects for time J, summed over states
    }
    cost_fp[i] <- sum(cts) # costs summed over all times and states, for this PSA sample
    eff_fp[i] <- sum(ets)  # effects ...
}


## ------------------------------------------------------------------------

library(BCEA)
bc <- bcea(e = cbind("FP"=eff_fp, "SFC"=eff),
           c = cbind("FP"=cost_fp, "SFC"=cost),
           wtp = seq(0, 150, 5))
contour2(bc, wtp=50)
ceac.plot(bc)

