# Suppose the IRS will audit 20% of income tax returns reporting an annual gross income of over $80,000. What is the probability that of 15 such returns, at most 8 will be audited?
x <- pbinom(8,15,.2)

# A certain rare blood type can be found in only 0.05% of people. If the population of a randomly selected group is 3000, what is the probability that at least two persons in the group have this rare blood type?
#y <- 1 - pbinom(n-1,N,p)
y <- 1 - pbinom(1,3000,.0005)

# A particular telephone number is used to receive both voice calls and fax messages. Suppose that 25% of the incoming calls involve fax messages, and consider a sample of 25 incoming calls.
# What is the probability that At most 6 of the calls involve a fax message?
z <- pbinom(6,25,.25)
# What is the probability that exactly 6 of the calls involve a fax message?
z <- dbinom(6,25,.25)
# What is the probability that at least 6 of the calls involve a fax message?
z <- 1 - pbinom(6-1,25,.25)
# What is the probability that more than 6 of the calls involve a fax message?
z <- 1 - pbinom(6,25,.25)
# What is the expected number of calls that involve a fax message?
x <- 25*.25
# What is the standard deviation of the number of calls that involve a fax message?
y <- sqrt(25*.25*.75)
# What is the probability that the number of calls that involve a fax message is within 2 standard deviations of the mean?
z <- pbinom(25*.25+2*sqrt(25*.25*.75),25,.25) - pbinom(25*.25-2*sqrt(25*.25*.75),25,.25)

# 4. A particular concentration of a chemical found in polluted water has been found to be lethal to 20% of the fish that are exposed to the concentration for 24 hours. Twenty fish are placed in a tank containing this concentration of chemical in water.
# a Find the probability that exactly 14 survive.
x <- dbinom(14,20,1-.2)
# b Find the probability that at least 10 survive.
x <- pbinom(10,20,1-.2)
# c Find the probability that at most 16 survive.
x <- dbinom(16,20,1-.2)
# d Find the mean and variance of the number that survive.
mean <- 20*(1-.2)
var <- 20*(1-.2)*.2

# Suppose that 30% of the applicants for a certain industrial job possess advanced training in computer programming. Applicants are interviewed sequentially and are selected at random from the pool. Find the probability that the first applicant with advanced training in programming is found on the fifth interview.
x <- dgeom(5-1,.3)
# What is the expected number of applicants who need to be interviewed to find the first one with advanced training?
x <- 1/.3

# From an ordinary deck of 52 cards we draw cards at random, with replacement, and successively until an ace is drawn. What is the probability that at least 10 draws are needed?
x <- 1 - pgeom(10-1,.25)

# Suppose that 20% of a group of people have hazel eyes. What is the probability that the eighth person chosen at random from this group is the third one that has hazel eyes?
x <- dbinom(3,8,.2)

# The employees of a firm that manufactures insulation are being tested for indications of asbestos in their lungs. The firm is requested to send three employees who have positive indications of asbestos on to a medical center for further testing. If 40% of the employees have positive indications of asbestos in their lungs, find the probability that ten employees must be tested in order to find three positives.
x <- dhyper(3,10,10*.4,10)

# Ten percent of the engines manufactured on an assembly line are defective. If engines are randomly selected one at a time and tested, what is the probability that the first non-defective engine will be found
# a) on the second trial?
x <- dgeom(2-1,1-.1)
# b) on the fifth trial?
x <- dgeom(5-1,1-.1)
# c) on or before the fifth trial?
x <- pgeom(5,1-.1)
# Find the mean and variance of the number of the trial on which the first non-defective engine is found.
mean <- 1/(1-.1)
var <- mean^2

# A store has 50 light bulbs available for sale. Of these, five are defective. A customer buys eight light bulbs at random from this store. What is the probability that he finds exactly one defective light bulb among them?
x <- dbinom(1,8,5/50)

# Florence is moving and wishes to sell her package of 100 computer diskettes. Unknown to her, 10 of those diskettes are defective. Sharon will purchase them if a random sample of 10 contains no more than one defective disk. What is the probability that she will buy them?
x <- pbinom(1,10,10/100)

# An urn contains ten marbles, of which five are green two are blue, and three are red. Three marbles are to be drawn from the urn, one at a time without replacement. What is the probability that all three marbles drawn will be green?
x <- 5/10*4/9*3/8

# From an ordinary deck of 52 cards, 8 cards are drawn at random and with replacement. What is the probability that, of the eight cards drawn, four are spades?
x <- dbinom(4,8,13/52)

# Only 60% of certain kinds of seeds germinate when planted under normal conditions. Suppose that four such seeds are planted and X denotes the number of those that will germinate. Find the probability function of X and that of y = 2x + 1.
x <- dbinom(0:4,4,.6)

# If two fair dice are rolled ten times, what is the probability of at least one 6 (on either die) in exactly five of these rolls?
x <- dbinom(5,10,1/6)

# Let X be the number of births in a certain hospital until the first twins are born. (In the US the number of twin births is approximately 1 in 90)
#P(X = x) <- (89/90)^(x-1)*(1/90)
