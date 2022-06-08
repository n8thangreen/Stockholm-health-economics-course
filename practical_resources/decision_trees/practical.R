# Depression decision tree
# Deterministic version

# Number and names of treatments
n.treat <- 3
t.names <- c("No treatment", "CBT", "Antidepressant")

# Over a 30 year time horizon
# Costs for recovery, relapse, and no recovery
c.rec <- 1000
c.rel <- 2000
c.norec <- 2500

# Cost of treatment
c.treat <- c(0, 300, 30)

# Over a 30 year time horizon
# QALYs for recovery, relapse, and no recovery
q.rec <- 26
q.rel <- 23
q.norec <- 20

# Probabilities of recovery and relapse following recover
p.rec <- p.rel <- rep(NA, n.treat)

# Probabilities for no treatment
p.rec[1] <- 0.029
p.rel[1] <- 0.019

# Probabilities for CBT
# Higher recovery probability than no treatment
p.rec[2] <- 0.044
# Lowest probability of relapse
p.rel[2] <- 0.010

# Probabilities for antidepressant
# Highest probability of recovery
# Higher relapse probability than CBT
p.rec[3] <- 0.047
p.rel[3] <- 0.017

# Name the vectors
names(p.rec) <- names(p.rel) <- t.names

# Willingness to pay threshold
lambda.target <- 20000

# Create structures to save the incremental costs, effects, and net benefits,
# as well as # absolute costs, effects, and net benefits
incremental.costs <- incremental.effects <- incremental.nb <-
  costs <- effects <- net.benefit <- rep(NA, n.treat)
# Now name these vectors
incremental.costs <- incremental.effects <- incremental.nb <-
  names(c.treat) <- names(costs) <- names(effects) <- t.names

# Calculate the total costs and effects
(costs <-
    c.treat + p.rec * (1 - p.rel) * c.rec + p.rec * p.rel * c.rel + (1 - p.rec) * c.norec)

(effects <- p.rec * (1 - p.rel) * q.rec + p.rec * p.rel * q.rel + (1 - p.rec) * q.norec)


