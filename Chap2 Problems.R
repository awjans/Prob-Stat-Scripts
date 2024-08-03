# Number selected at Random from 1 to 1000 is divisible by 3
A <- seq(1,333)
B <- seq(1,1000)
PA <- max(A)/max(B)

# Person chosen at Random has Type A or AB Blood
PA <- 0.41
PB <- 0.10
PAB <- 0.04
PO <- 0.45
PAorAB <- PA + PAB

# Suspicious American on Government Conspiracies
PVL <- 0.24
PSL <- 0.24
PU <- 0.40
PO <- 0.12
PVLorSL <- PVL + PSL

# Person Needing Glasses
PS <- rbind(c(0.44, 0.14), c(0.02, 0.40)) # P(S) Sample Space
PNG <- sum(PS[1,]) # P(NG) Person Needs Glasses
PNGNU <- sum(PS[1,2]) # P(NGNU) Person Needs Glasses and Not Used
PGU <- sum(PS[,1]) # P(GU) Person uses Glasses even if not needed

# Oil Prospecting
PE1 <- 0.01
PE3 <- 0.09
PE4 <- 0.81
PE2 <- 1 - PE1 - PE3 - PE4
PH1 <- PE1 + PE2 + PE3

# Employment Opportunities
PS <- c(0,0,1/6,0,1/6,1/6)
PM <- sum(PS)

# Probability of Rolling a 5 given an Odd Number was Rolled
PAgivenB <- (1/6)/(1/2)

# Probability of living to 90 (0.63) if already lived to 80 (0.75)
PAgivenB <- (0.63)/(0.75)

# Probability if 2 5s are rolled if the Sum is divisible by 5
PAgivenB <- (1/36)/(13/36)

# Probability of 8 spades drawn from deck if first 3 are spades
PAgivenB <- choose(13, 3)*choose(39, 5)/choose(52, 8)

# Probability of 3 red then 5 blue marbles drawn from bag with replacement
# of 3 red and 5 blue marbles
PAgivenB <- (3/8)^3*(5/8)^5

# Probability of testing 4 good batteries of 13 batteries, 10 are good, 3 are bad, next one is bad
PAgivenB <- choose(10, 4)*choose(3, 1)/choose(13, 5)

# Suppose that 15% of the population of a country are unemployed women, and a total of 25% are unemployed. What percent of the unemployed are women?
PAgivenB <- 0.15/0.25

# If eight defective and 12 non-defective items are inspected one-by-one, at random and without replacement, what is the probability that the first four items inspected are defective
PAgivenB <- choose(8, 4)*choose(12, 0)/choose(20, 4)

# If eight defective and 12 non-defective items are inspected one-by-one, at random and without replacement, what is the probability that from the first 3 items at least 2 are defective
PAgivenB <- choose(8, 2)*choose(12, 1)/choose(20, 3)

# A smoke detector system uses two devices, A and B. If smoke is present, the probability that it will be detected by device A is .95; by device B, .90; and by both devices, .88. What is the probability that smoke is present if it is detected by device A or B or both?
# PAgivenB = P(A) + P(B) - P(A and B)
PAgivenB <- 0.95 + 0.90 - 0.88

# A smoke detector system uses two devices, A and B. If smoke is present, the probability that it will be detected by device A is .95; by device B, .90; and by both devices, .88. What is the probability that smoke is not detected
# P(A') = 1 - (P(A) + P(B) - P(A and B))
PAgivenB <- 1 - (0.95 + 0.90 - 0.88)

# An insurance company rents 35% of the cars for its customers from agency I and 65% from agency II. If 8% of the cars of agency I and 5% of the cars from agency II break down during the rental periods, what is the probability that a car rented by this insurance company breaks down?
PAgivenB <- 0.35*0.08 + 0.65*0.05

# If 5% of men and 0.25% of women are color blind, what is the probability that a randomly selected person is color blind?
PAgivenB <- 0.05*0.5 + 0.0025*0.5

# Suppose that 37% of a community are at least 45 years old. If 80% of the time a person who is 45 or older tells the truth, and 65% of the time a person below 45 tells the truth, what is the probability that a randomly selected person answers a question truthfully?
PAgivenB <- 0.37*0.80 + 0.63*0.65

# A factory produces its entire output with three machines. Machines I, II, and III produce 50%, 30%, and 20% of the output, but 4%, 2%, and 4% of their outputs are defective, respectively. What fraction of the total output is defective?
PAgivenB <- 0.50*0.04 + 0.30*0.02 + 0.20*0.04

