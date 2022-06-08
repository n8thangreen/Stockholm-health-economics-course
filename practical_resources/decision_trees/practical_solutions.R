# Solution to decision trees practical
# Howard Thom 8-July-2019


##########
# part 1

# Run the file "practical.R"
# Can do this using the source() command
source("practical.R")

# The model input parameters are the vectors (e.g. c.norec) and matrices (e.g. p.rel, c.treat)
# going into the model equations:
# costs<-c.treat+p.rec*(1-p.rel)*c.rec+p.rec*p.rel*c.rel+(1-p.rec)*c.norec
# effects<-p.rec*(1-p.rel)*q.rec+p.rec*p.rel*q.rel+(1-p.rec)*q.norec

# Net benefit
(net.benefit <- lambda.target * effects - costs)

# Incremental results relative to no treamtent
(incremental.costs <- costs - costs[1])
(incremental.effects <- effects - effects[1])
(incremental.net.benefit <-
    lambda.target * incremental.effects - incremental.costs)

# Incremental cost effectiveness ratios relative to no treatment
(icer <- incremental.costs / incremental.effects)


##########
# part 2

# To ensure the input parameters make sense, look at the means of matrices using the colMeans() function
# and the means of vectors using the mean() function
# Matrices
# CBT has lowest probability of relapse
colMeans(p.rel)
# Antidepressant has the highest probability of recovery
colMeans(p.rec)
# Mean cost of treatment (these are actually constant!)
colMeans(c.treat)

# Vectors
# Cost of no recovery
mean(c.norec)
# Cost of relapse
mean(c.rel)
# Cost of recovery
mean(c.rec)
# The cost of no recovery is highest, as patients incur higher hospitalization and therapy costs
# QALYs of no recovery
mean(q.norec)
# QALYs of relapse
mean(q.rel)
# QALYs of recovery
mean(q.rec)
# The QALY for recovery is highest, as patients don't suffer a relapse and spend most of remaining 30 years if good mental health

# Summary of cost and effects
# Mean QALYs of each treatment over 30 years
# Very similar but antidepressants and CBT are higher than no treatment
colMeans(effects)
# Mean costs
# CBT is the most expensive while no treatment and antidepressant are similar
colMeans(costs)
# Calculate the net benefit at a willingess-to-pay of £20,000
net.benefit <- 20000 * effects - costs
# Net benefit is highest for antidepressant, but all options are very close.
colMeans(net.benefit)


#############################
# Using BCEA

# If BCEA not installed you'll have to install BCEA first
# install.packages("BCEA")
# Load the package
library(BCEA)


# a)
# Create a bcea object for the depression decision tree
# e are the effects, c are the costs
# Set the 'ref' reference treatment to be 1 (no treatment)
# Set the interventions, which is the names of the treatments, to t.names
depression.bcea <-
  bcea(
    e = effects,
    c = costs,
    ref = 1,
    interventions = t.names)


# b)
# The summary() gives comparisons of CBT and antidepressants to no treatment.
# wtp is the willingness-to-pay threshold. The detafult to 25000 so set this to 20000
# The "EIB" is expected incremental benefit at the wtp=20000, the "CEAC" is the
# probability that the reference of "no treatment" has highest net benefit (most cost-effective),
# and the ICER is the incremental cost-effectiveness ratio.
# We can see that EIB is negative for no treatment relative to both CBT and antidepresants, so the latter two are favoured
# The CEAC is less than 0.5 so there is a higher probability that CBT and antidepressant are most cost-effective
# The ICER is less than £20,000 for both CBT and antidepressant so both should be considered cost-effective
summary(depression.bcea, wtp = 20000)

# c)
# Now comparing CBT to antidepressant
# Use the bcea() function but with ref=2, so that CBT is the reference treatment.
# All other inputs remain the same
depression.refCBT.bcea <-
  bcea(
    e = effects,
    c = costs,
    ref = 2,
    interventions = t.names)

# The final matrix of EIB and CEAC are of interest.
# The first row are comparisons with no treatment which suggest CBT is more cost-effective as EIB is positive,
# CEAC>0.50. The ICER isn't useful for interpretation.
# The second row tells us that CBT has lower incremental net benefits (EIB is negative) and that the probability
# that CBT is more cost-effective than antidepressants is less than 50% (CEAC<0.50)
summary(depression.refCBT.bcea, wtp = 20000)

# d) Multiple treatment comparison

# For multiple treatment comparison pass the output of bcea() to multi.ce()
# This may take a moment
depression.multi.ce <- multi.ce(depression.bcea)

# Now generate the probability that each treatment has the highest net benefit at
# a range of willingness-to-pay thresholds.
# These are the cost-effectiveness acceptability curves (CEAC)
# At £20,000, antidepressants have the highest probability of having highest net benefit (being most cost-effective)
ceac.plot(depression.multi.ce,
          pos = c(1, 0.5))
