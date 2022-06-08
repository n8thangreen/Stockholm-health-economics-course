# Depression decision tree
# Probabilistic version

# Bayesian cost-effectiveness analysis functions
library(BCEA)

# Number of PSA samples
n.samples <- 1000

# Number and names of treatments
n.treat <- 3
t.names <- c("No treatment", "CBT", "Antidepressant")

# Over a 30 year time horizon
# Costs for recovery, relapse, and no recovery
c.rec <- rnorm(n = n.samples, mean = 1000, sd = 50)
c.rel <- rnorm(n = n.samples, mean = 2000, sd = 100)
c.norec <- rnorm(n = n.samples, mean = 2500, sd = 125)

# Generate a histogram of costs of recovery for illustrative purposes
jpeg(file = "cost.recovery.histogram.jpg")
hist(c.rec,
     breaks = 20,
     xlab = "Lifetime cost of recovery (£)",
     main = "Histogram")
dev.off()

# Over a 30 year time horizon
# QALYs for recovery, relapse, and no recovery
q.rec <- rnorm(n = n.samples, mean = 26, sd = 2)
q.rel <- rnorm(n = n.samples, mean = 23, sd = 3)
q.norec <- rnorm(n = n.samples, mean = 20, sd = 4)

# Probabilities of recovery and relapse following recover
p.rec <- p.rel <- matrix(nrow = n.samples, ncol = n.treat)

# Probabilities for no treatment
p.rec[, 1] <- rbeta(n = n.samples, shape1 = 6, shape2 = 200)
p.rel[, 1] <- rbeta(n = n.samples, shape1 = 2, shape2 = 100)

# Probabilities for CBT
# Probability of recovery higher than on no treatment
p.rec[, 2] <- rbeta(n = n.samples, shape1 = 6, shape2 = 130)
# Probability of relapse lower than on no treatment
p.rel[, 2] <- rbeta(n = n.samples, shape1 = 2, shape2 = 200)

# Probabilities for antidepressant
# Probability of recovery lower than no treatment or CBT
p.rec[, 3] <- rbeta(n = n.samples, shape1 = 6, shape2 = 120)
# Probability relase lower than no treatment, higher than CBT
p.rel[, 3] <- rbeta(n = n.samples, shape1 = 2, shape2 = 120)

# Generate a histogram of probabilities following a beta-distribution for illustrative purposes
jpeg(width = 960,
     height = 480,
     file = "probabilities.beta.histogram.jpg")
par(mfrow = c(1, 2))
hist(p.rec[, 1],
     breaks = 20,
     xlab = "Probability of recovery, no treatment",
     main = "Histogram")
hist(p.rel[, 1],
     breaks = 20,
     xlab = "Probability of relapse, no treatment",
     main = "Histogram")
dev.off()

# Cost of treatment
c.treat <- t(matrix(rep(c(0, 300, 30), n.samples), ncol = n.samples, nrow =
                      3))

# Willingness to pay threshold
lambdas <- c(1:50) * 1000
lambda.target <- 20000

incremental.costs <-
  incremental.effects <-
  incremental.nb <-
  costs <- effects <- net.benefit <- matrix(nrow = n.samples, ncol = n.treat)
colnames(incremental.costs) <-
  colnames(incremental.effects) <-
  colnames(incremental.nb) <-
  colnames(p.rec) <-
  colnames(p.rel) <-
  colnames(effects) <- colnames(costs) <- colnames(net.benefit) <-
  t.names


# Total cost and effects
costs <-
  c.treat + p.rec * (1 - p.rel) * c.rec + p.rec * p.rel * c.rel + (1 - p.rec) *
  c.norec
effects <- p.rec * (1 - p.rel) * q.rec + p.rec * p.rel * q.rel + (1 - p.rec) *
  q.norec

net.benefit <- lambda.target * effects - costs

# Incremental outcomes
incremental.costs <- costs - costs[, 1]
incremental.effects <- effects - effects[, 1]
incremental.nb <- net.benefit - net.benefit[, 1]

# Overall summary
# Average costs and effects
colMeans(costs)
colMeans(effects)
colMeans(net.benefit)

# Build a general input.parameters matrix
input.parameters <-
  t(rbind(
    t(p.rec),
    t(p.rel),
    # t(lor.rec),
    # t(lor.rel),
    q.rec,
    q.rel,
    q.norec,
    c.rec,
    c.rel,
    c.norec))

colnames(input.parameters)[1:6] <-
  c(paste("p.rec", colnames(p.rec)), paste("p.rel", colnames(p.rel)))
colnames(input.parameters)[7:10] <-
  c(paste("lor.rec", colnames(p.rec)[2:3]), paste("lor.rel", colnames(p.rel)[2:3]))


format.results <- function(x, n.digits = 2) {
  paste(
    round(mean(x), digits = n.digits),
    " (",
    round(quantile(x, probs = 0.025), digits = n.digits),
    ", ",
    round(quantile(x, probs = 0.975), digits = n.digits),
    ")",
    sep = ""
  )
}

# For example for c.rec<-rnorm(n=n.samples, mean=1000, sd=50)
# format.results(c.rec)

results.matrix <- matrix(NA, nrow = 6, ncol = n.treat)
rownames(results.matrix) <-
  c(
    "Total costs",
    "Total QALYs",
    "Incremental costs",
    "Incremental QALYs",
    "Net Benefit",
    "Incremental NB"
  )
colnames(results.matrix) <- t.names
for (i.treat in 1:n.treat)
{
  results.matrix["Total costs", i.treat] <-
    format.results(x = costs[, i.treat])
  results.matrix["Total QALYs", i.treat] <-
    format.results(x = effects[, i.treat])
  results.matrix["Incremental costs", i.treat] <-
    format.results(x = incremental.costs[, i.treat])
  results.matrix["Incremental QALYs", i.treat] <-
    format.results(x = incremental.effects[, i.treat])
  results.matrix["Net Benefit", i.treat] <-
    format.results(x = net.benefit[, i.treat])
  results.matrix["Incremental NB", i.treat] <-
    format.results(x = incremental.nb[, i.treat])
}

# Export as a csv
write.csv(results.matrix, file = "depression.results.csv")

# Optimal treatment at £20,000
which.max.nb <- apply(net.benefit, c(1), which.max)
