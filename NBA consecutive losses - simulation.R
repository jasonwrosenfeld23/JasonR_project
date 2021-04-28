#  Winning and Losing Streaks in the NBA 

# In July 2010, LeBron James and Chris Bosh moved to the Heat, forming a modern day NBA "superteam."  This lead former NBA head coach turned announcer Jeff Van Gundy to say: "They (the Heat) will never lose two games in a row this year."  # http://www.chicagonow.com/blogs/chicago-bulls-confidential/2010/08/jeff-van-gundy-predicts-heat-break-the-bulls-72-win-record.html#ixzz17A3rKvun

# This got me thinking - was this a wise statement?  What's the probability that even a very good need never suffers consecutive losses at any point in an 82-game NBA season?

# Simulation seems the most direct way, so that's what I'll start with

n <- 82   # 82 games in an NBA season
x <- rbinom(82,1,0.85)   # assume they have an 85% chance of winning each game, with each game independent of the others.
k <- 1:(n-1)
y <- x[k+1]+x[k]      # the vector of successive sums; an element of y is 0 if & only if there are two consecutive 0 elements in x (consecutive losses)

a <- min(c(which(y==0),Inf))   # is finite if & only if an element of y is 0

b <- ifelse(a < 83,1,0)  # 1 corresponds to team suffering back to back losses in that season; 0, not

# Putting it together for the simulation:

loss <- function(m,n)
{
  sapply(1:m, function(o)
  {
    x <- rbinom(n,1,0.85)
    k <- 1:(n-1)
    y <- x[k+1]+x[k]                     
    a <- min(c(which(y==0),Inf))   
    b <- ifelse(a < n+1,1,0)
    b
  })
}

summary(loss(10000,82))  # simulate 10,000 times

#mean = 0.8004.  Thus, the probability that, in an 82-game season, a team who wins 85% of their games suffers consecutive losses is about 80%.
# Note: in reality, the probability is likely higher, since game results are NOT completely independent.  If a team just lost a game, it's more likely to have had an injured player, be playing on the road without rest, etc, which is correlated to its performance in the following game.

##### Try another way to write the simulation to check my work

n <- 82	# 82 games in a regular season
x <- rbinom(82,1,0.85)	# imagine Heat have 85% chance of winning a game, assuming games are independent.
b <- cumsum(x) # running count on number of 1s (wins); wwe are looking for when b[j+2] - b[j] = 0.  When this occurs, then we know the team lost 2 consecutive games.

j <- c(1:80)
a <- b[j+2]-b[j] 
d <- min(a) # if min(a) = 0 then the team lost consecutive games in that season.
k <- ifelse(d==0,1,0)  # if k is 1, then the team lost consecutive games, if 0, then they did not.

## Writing the simulation.

losing <- function(m,n)
{
  sapply(1:m, function(o)
  {
    x <- rbinom(n,1,0.85)
    b <- cumsum(x) 
    j <- c(1:80)
    a <- b[j+2]-b[j] 
    d <- min(a)
    k <- ifelse(d==0,1,0) 
    k
  })
}

summary(losing(10000,82))  # simulate 10,000 times

# Approximately the same mean, confirming that a team who wins 85% of their games would have a consecutive game losing streak in about 80% of their seasons.

# Actual results?  They indeed lost two games in a row, and as early as in the second month of the season!

# Now let's say we want to know, well, how good does a team name to have a better than 50% chance of getting through the season without losing consecutive games?

noConsLosses = function(winProb) {
  numWins = 0:82
  probWins = dbinom(numWins, 82, winProb)
  probVector = choose(numWins + 1, (82 - numWins)) /
    choose(82, 82 - numWins)
  totalProb = probWins * probVector
  return(sum(totalProb))
}

probList = seq(from = 0.85, to = 1, by = 1e-04)
byProb = numeric(length(probList))
for (i in 1:length(probList)) {
     byProb[i] = noConsLosses(probList[i])
}

library(ggplot2)

qplot(probList, byProb, geom = "line")

# Looks like y is above .5 at around win probability of 91%.  Check exactly.

probList[min(which(byProb >= 0.5))]  # Assuming independence, a team must have a 90.4% chance or greater of winning each game for that team to have a greater than 50% chance of getting through and 82-game season without suffering consecutive losses.


#######  More simualations

# If we are curious if an 85% win% team would suffer a 3 game losing streak at some point in the season, we can modify the code pretty easily.

loss <- function(m,n)
{
  sapply(1:m, function(o)
  {
    x <- rbinom(n,1,0.85)
    k <- 1:(n-2)
    y <- x[k+2]+x[k+1]+x[k]      # the vector of successive sums, this time we look at 3 consecutive elements                
    a <- min(c(which(y==0),Inf))   # is finite if & only if an element of y is 0
    b <- ifelse(a < n+1,1,0)
    b
  })
}

summary(loss(10000,82))

#Only 20.9%  So in only about 1 season out of 5 will a team who wins 85% of their games suffer a 3-game losing streak.

#####################################################################

# Fun also to analyze winning streaks, and instructive.  If our team goes on a 3 game winning streak, for example, should we assume we are a really good team?  Or is that something we should expect to do at some point of the season, even if we're bad?

win <- function(m,n,p)
{
sapply(1:m, function(o)
{
x <- rbinom(n,1,p)      
k <- c(1:(n-2))
y <- x[k+2]+x[k+1]+x[k]      # summing wins for 3 consecutive games.                  
a <- min(c(which(y==3),Inf))   # is finite if & only if an element of y is 3
b <- ifelse(a < n+1,1,0)
b
})
}

summary(win(10000,82,0.20)) 

# 41%! Even a terrible team, a team that wins only 20% of its games, still has a 41% chance of going on a 3-game winning streak at some point in the season!

# Now let's find the probability of a 3 gm win streak in an 82 game season for teams with different win %s.
  
  P_20p_3w <- mean((win(10000,82,0.20)))
  P_30p_3w <- mean((win(10000,82,0.30)))
  P_40p_3w <- mean((win(10000,82,0.40)))
  P_50p_3w <- mean((win(10000,82,0.50)))
  P_60p_3w <- mean((win(10000,82,0.60)))
  P_70p_3w <- mean((win(10000,82,0.70)))
  P_80p_3w <- mean((win(10000,82,0.80)))
  P_90p_3w <- mean((win(10000,82,0.90)))
  
  vector_3w <- cbind(P_20p_3w,P_30p_3w,P_40p_3w,P_50p_3w,P_60p_3w,P_70p_3w,P_80p_3w,P_90p_3w)
  
  vector_3w
  
# That vector shows the probability that the team goes on a 3-gm winning streak at some point of the year, given a certain probability of them winning each game.
  
# Now let's check the probability of getting a 5 game winning streak at some point of the year.
  
  win <- function(m,n,p)
  {
    sapply(1:m, function(o)
    {
      x <- rbinom(n,1,p)      
      k <- c(1:(n-4))
      y <- x[k+4]+x[k+3]+x[k+2]+x[k+1]+x[k]                    
      a <- min(c(which(y==5),Inf))   # is finite if & only if an element of y is 5
      b <- ifelse(a < n+1,1,0)
      b
    })
  }
  
  summary(win(10000,82,0.20)) # a team who wins 20% of their games has a 2.17% chance of achieving a 5-game winning streak at some point.
  
  P_20p_5w <- mean((win(10000,82,0.20)))
  P_30p_5w <- mean((win(10000,82,0.30)))
  P_40p_5w <- mean((win(10000,82,0.40)))
  P_50p_5w <- mean((win(10000,82,0.50)))
  P_60p_5w <- mean((win(10000,82,0.60)))
  P_70p_5w <- mean((win(10000,82,0.70)))
  P_80p_5w <- mean((win(10000,82,0.80)))
  P_90p_5w <- mean((win(10000,82,0.90)))
  
  vector_5w <- cbind(P_20p_5w,P_30p_5w,P_40p_5w,P_50p_5w,P_60p_5w,P_70p_5w,P_80p_5w,P_90p_5w)
  
  # how about a 7 game winning streak?
  
  win <- function(m,n,p)
  {
    sapply(1:m, function(o)
    {
      x <- rbinom(n,1,p)      
      k <- c(1:(n-6))
      y <- x[k+6]+x[k+5]+x[k+4]+x[k+3]+x[k+2]+x[k+1]+x[k]      # summing wins for 7 consecutive games.                  
      a <- min(c(which(y==7),Inf))   # is finite if & only if an element of y is 7
      b <- ifelse(a < n+1,1,0)
      b
    })
  }
  
  P_20p_7w <- mean((win(10000,82,0.20)))
  P_30p_7w <- mean((win(10000,82,0.30)))
  P_40p_7w <- mean((win(10000,82,0.40)))
  P_50p_7w <- mean((win(10000,82,0.50)))
  P_60p_7w <- mean((win(10000,82,0.60)))
  P_70p_7w <- mean((win(10000,82,0.70)))
  P_80p_7w <- mean((win(10000,82,0.80)))
  P_90p_7w <- mean((win(10000,82,0.90)))
  
  vector_7w <- cbind(P_20p_7w,P_30p_7w,P_40p_7w,P_50p_7w,P_60p_7w,P_70p_7w,P_80p_7w,P_90p_7w)
  
# how about a 10 game winning streak?
  
  win <- function(m,n,p)
  {
    sapply(1:m, function(o)
    {
      x <- rbinom(n,1,p)      
      k <- c(1:(n-9))
      y <- x[k+9]+x[k+8]+x[k+7]+x[k+6]+x[k+5]+x[k+4]+x[k+3]+x[k+2]+x[k+1]+x[k]                       
      a <- min(c(which(y==10),Inf))   # is finite if & only if an element of y is 10
      b <- ifelse(a < n+1,1,0)
      b
    })
  }
  
  P_20p_10w <- mean((win(10000,82,0.20)))
  P_30p_10w <- mean((win(10000,82,0.30)))
  P_40p_10w <- mean((win(10000,82,0.40)))
  P_50p_10w <- mean((win(10000,82,0.50)))
  P_60p_10w <- mean((win(10000,82,0.60)))
  P_70p_10w <- mean((win(10000,82,0.70)))
  P_80p_10w <- mean((win(10000,82,0.80)))
  P_90p_10w <- mean((win(10000,82,0.90)))
  
  vector_10w <- cbind(P_20p_10w,P_30p_10w,P_40p_10w,P_50p_10w,P_60p_10w,P_70p_10w,P_80p_10w,P_90p_10w)
  
# Finally, how about a 15 game winning streak?
  
  win <- function(m,n,p)
  {
    sapply(1:m, function(o)
    {
      x <- rbinom(n,1,p)      
      k <- c(1:(n-14))
      y <- x[k+14]+x[k+13]+x[k+12]+x[k+11]+x[k+10]+x[k+9]+x[k+8]+x[k+7]+x[k+6]+x[k+5]+x[k+4]+x[k+3]+x[k+2]+x[k+1]+x[k]                        
      a <- min(c(which(y==15),Inf))   # is finite if & only if an element of y is 15
      b <- ifelse(a < n+1,1,0)
      b
    })
  }
  
  P_20p_15w <- mean((win(10000,82,0.20)))
  P_30p_15w <- mean((win(10000,82,0.30)))
  P_40p_15w <- mean((win(10000,82,0.40)))
  P_50p_15w <- mean((win(10000,82,0.50)))
  P_60p_15w <- mean((win(10000,82,0.60)))
  P_70p_15w <- mean((win(10000,82,0.70)))
  P_80p_15w <- mean((win(10000,82,0.80)))
  P_90p_15w <- mean((win(10000,82,0.90)))
  
  vector_15w <- cbind(P_20p_15w,P_30p_15w,P_40p_15w,P_50p_15w,P_60p_15w,P_70p_15w,P_80p_15w,P_90p_15w)
  
  
  #Plot our findings
  
  length <- c(3,5,7,10,15)
  
  winstreaks <- rbind(vector_3w,vector_5w,vector_7w,vector_10w,vector_15w) 
  
  # 3 plots, one over the other - for a 20%, 50% and 80% win team
  
  {
    
    par(mfrow=c(3,1))    # places three scatterplots in same plot, one over the other
    
    plot(length,winstreaks[,1],col="salmon1",lwd="5", main="chance of various streaks for a team who wins 20% of their games") 
    
    plot(length,winstreaks[,4],col="aquamarine",lwd="5",main="chance of various streaks for a team who wins 50% of their games")
    
    plot(length,winstreaks[,7],col="gold4",lwd="5",main="chance of various streaks for a team who wins 80% of their games")
    
    par(mfrow=c(1,1))    # reset
    
  }
  
  
  # plot all in one window and add labels
  
  plot(length, winstreaks[,1], ylim=c(0:1), col="salmon1", xlab= "Greatest # of Consecutive Wins in an 82 game season",
       ylab="Probabilities of Various Teams Achieving A Certain Win Streak", 
       main="Probability of Achieving Various Win Streaks", pch=16,lwd="5",cex.lab=1.1)
  points(length,winstreaks[,2],col="tan2",pch=17,lwd="5")
  points(length,winstreaks[,3],col="tomato",pch=18,lwd="5")
  points(length,winstreaks[,4],col="aquamarine",pch=19,lwd="5")
  points(length,winstreaks[,5],col="violetred4",pch=20,lwd="5")
  points(length,winstreaks[,6],col="darkblue",pch=21,lwd="5")
  points(length,winstreaks[,7],col="gold4",pch=22,lwd="5")
  points(length,winstreaks[,8],col="darkred",pch=23,lwd="5")
  
  legend(12,0.9, c("20%tm", "30%tm", "40%tm","50%tm","60%tm","70%tm","80%tm","90%tm"), col=c("salmon1", "tan2", "tomato","aquamarine","violetred4","darkblue", "gold4","darkred"), pch=c(16:23))
  



  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  