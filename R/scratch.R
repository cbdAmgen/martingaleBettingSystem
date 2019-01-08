pr <- function(expP, expQ){
  return(p^expP*q^expQ)
}

win <- function(multiple, pr){
  return(sum(multiple * pr))
}

goBustProb <- function(bustP, bustQ){
  return(sum(p^bustP*q^bustQ))
}

p <- 0.4748         # probability of winning each hand
q <- 1 - p          # probability of losing each hand

## (1,1)
expP <- c(1,0)
expQ <- c(0,1)
sum(pr(expP, expQ))
multiple <- c(1,-1)
win(multiple, pr(expP, expQ))
bustP <- c(0)
bustQ <- c(1)
goBustProb(bustP, bustQ)
q

## (2,1)
expP <- c(2,1,0)
expQ <- c(0,1,1)
sum(pr(expP, expQ))
multiple <- c(2,0,-1)
win(multiple, pr(expP, expQ))
bustP <- c(1,0)
bustQ <- c(1,1)
goBustProb(bustP, bustQ)
q*(1 + p)

## (2,2)
expP <- c(2,1,1,0)
expQ <- c(0,1,1,2)
sum(pr(expP, expQ))
multiple <- c(2,0,1,-3)
win(multiple, pr(expP, expQ))
bustP <- c(0)
bustQ <- c(2)
goBustProb(bustP, bustQ)
q^2

## (3,1)
expP <- c(3,2,1,0)
expQ <- c(0,1,1,1)
sum(pr(expP, expQ))
multiple <- c(3,1,0,-1)
win(multiple, pr(expP, expQ))
bustP <- c(2,1,0)
bustQ <- c(1,1,1)
goBustProb(bustP, bustQ)
q*(1 + p + p^2)

## (3,2)
expP <- c(3,2,2,1,2,1,0)
expQ <- c(0,1,1,2,1,2,2)
sum(pr(expP, expQ))
multiple <- c(3,1,2,-2,2,0,-3)
win(multiple, pr(expP, expQ))
bustP <- c(1,0)
bustQ <- c(2,2)
goBustProb(bustP, bustQ)
q^2*(1 + p)

## (3,3)
expP <- c(3,2,2,1,2,1,1,0)
expQ <- c(0,1,1,2,1,2,2,3)
sum(pr(expP, expQ))
multiple <- c(3,1,2,-2,2,0,1,-7)
win(multiple, pr(expP, expQ))
bustP <- c(0)
bustQ <- c(3)
goBustProb(bustP, bustQ)
q^3

## (4,1)
expP <- c(4,3,2,1,0)
expQ <- c(0,1,1,1,1)
sum(pr(expP, expQ))
multiple <- c(4,2,1,0,-1)
win(multiple, pr(expP, expQ))
bustP <- c(3,2,1,0)
bustQ <- c(1,1,1,1)
goBustProb(bustP, bustQ)
q*(1 + p + p^2 + p^3)

## (4,2)
expP <- c(4,3,3,2,3,2,1,3,2,2,1,0)
expQ <- c(0,1,1,2,1,2,2,1,2,2,3,2)
sum(pr(expP, expQ))
multiple <- c(4,2,3,-1,3,1,-2,3,1,2,-2,-3)
win(multiple, pr(expP, expQ))
bustP <- c(2,1,1,0)
bustQ <- c(2,2,3,2)
goBustProb(bustP, bustQ)
q^2*(1 + 2*p)

## (4,3)
expP <- c(4,3,3,2,3,2,2,1,3,2,2,1,2,1,0)
expQ <- c(0,1,1,2,1,2,2,3,1,2,2,3,2,3,3)
sum(pr(expP, expQ))
multiple <- c(4,2,3,-1,3,1,2,-6,3,1,2,-2,2,0,-7)
win(multiple, pr(expP, expQ))
bustP <- c(1,0)
bustQ <- c(3,3)
goBustProb(bustP, bustQ)
q^3*(1 + p)

## (4,4)
expP <- c(4,3,3,2,3,2,2,1,3,2,2,1,2,1,1,0)
expQ <- c(0,1,1,2,1,2,2,3,1,2,2,3,2,3,3,4)
sum(pr(expP, expQ))
multiple <- c(4,2,3,-1,3,1,2,-6,3,1,2,-2,2,0,1,-15)
win(multiple, pr(expP, expQ))
bustP <- c(0)
bustQ <- c(4)
goBustProb(bustP, bustQ)
q^4


## (5,1)
expP <- c(5:0)
expQ <- c(0,1,1,1,1,1)
sum(pr(expP, expQ))
multiple <- c(5,3,2,1,0,-1)
win(multiple, pr(expP, expQ))
bustP <- c(4,3,2,1,0)
bustQ <- c(1,1,1,1,1)
goBustProb(bustP, bustQ)
q*(1 + p + p^2 + p^3 + p^4)

###########################################################
###########################################################
###########################################################
###########################################################
###########################################################
## (5,2)
expP <- c(5,4,4,3,4,3,2,4,3,3,2,1,4,3,3,2,3,2,1,0)
expQ <- c(0,1,1,2,1,2,2,1,2,2,3,2,1,2,2,3,2,3,3,2)
sum(pr(expP, expQ))
multiple <- c(5,3,4,0,4,2,-1,4,2,3,-1,-2,4,2,3,-1,3,1,-2,-3)
win(multiple, pr(expP, expQ))


## (5,3)
expP <- c(5,4,4,3,4,3,3,2,4,3,3,2,3,2,1,4,3,3,2,3,2,2,1,3,2,2,1,0)
expQ <- c(0,1,1,2,1,2,2,3,1,2,2,3,2,3,3,1,2,2,3,2,3,3,4,2,3,3,4,3)
sum(pr(expP, expQ))
multiple <- c(5,3,4,0,4,2,3,-5,4,2,3,-1,3,1,-6,4,2,3,-1,3,1,2,-6,3,1,2,-2,-7)
win(multiple, pr(expP, expQ))