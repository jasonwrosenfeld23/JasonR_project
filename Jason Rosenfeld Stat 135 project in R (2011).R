##############################  # 4 Parts  #############################  Jason Rosenfeld


## PART 1, Creating a Theoretical Model For NBA In-Game Win Probability and Inspecting Its Results
## PART 2, Data Checking and Finalizing a Data Set (will compare data vs theory)
## Part 3, Preparing for Data Analysis: Prelim Analysis (data includes 16 seasons)
## Part 4, Comparing the Theoretical Model to Actual Results 

## PART 1, Creating a Theoretical Model and Inspecting Its Results
# In-Game Win Probability for an NBA Game 

# A theoretical model can be made to calculate a baseline in-game probability 
# that Team A beats team B.

#### Some background on the model.
# Take A to be a random variable, the # of points Team A scores in a game.
# Take B to be another random variable, the # of points Team B scores in a game.

# P ( A > B ) = P(A wins the game).

# P ( A - B > 0 ) = P ( ( A - B - E( A - B ) ) / stdev ( A - B)  >  -E( A - B ) / stdev ( A - B )) =

# P ( Z >  ( E(B) - E(A) ) / stdev ( A - B) = P ( Expected lead / Standard Deviation).

# EL = Expected lead , SD = Standard Deviation.

# And thus, the probability that team A beats team B, is pnorm(Z), with Z = EL / SD

# EL is how many points we expect the team to win by, given how the game has played out so far.

# SD is the standard deviation of difference in scores for the remaining part of the game.  Thus,
# as there is less time left in the game, standard deviation decreases.

# EL = current lead +- 0.5 + Las Vegas point spread proportional to how much time is left.

# SD = 13.5 * sqrt ( t / 48).

# we will do all calculations from the perspective of the home team.

# current lead is the current lead of the home team at game time of calculation.  Adding or subtracting 0.5 
# depends on if the home team has the ball (if they do, then add).  As for the Vegas point spread,
# if before the game, Las Vegas predicted the home team to win by 12, and we are making our prediction
# at the start of the second quarter (thus with 3/4 of the game left), we'd add on 12 * 3/4= 9 points
# for the Las Vegas proportional point spread.

# For SD, 13.5 is approx. the standard deviation for 2 equal teams for a full length game.  So we need 
# to adjust it depending on how much time is left in the game.  " t " is in minutes. 48 is the # of minutes 
# in a full length NBA game (non-overtime game, that is).

# An example:

# Lakers are favored by 16 points vs. the Clippers.  At the start of the 2nd quarter, the Lakers
# lead by 2 and have possession of the ball. (so there are 3 quarters, 36 minutes left in the game).

# "ps" is pointspread, "l" is lead, "pos" is possession, "t" = time remaining in game in minutes.

ps <- 16
l <- 2
pos <- 0.5
t <- 36

SD <- 13.5 * sqrt( t / 48  )
EL <- l + pos + ps*( t / 48 )

Z <- EL / SD

probAwins <- pnorm(Z)

probAwins

# And thus, in the above example, up by 2 with the ball at the start of the 2nd quarter,
# the Lakers have an 89% chance of winning the game.

# What may be interesting to look at is, in that Lakers vs Clippers example above, imagine that
# the Lakers always have a 2 point lead throughout the entire game, and the ball.  How does their 
# probability of winning vary throughout the game (ie with different values of " t " )?

# Note it might not be immediately obvious what should happen to their chance of winning.
# In a sense, one may think it should increase, since as the game is running out of time, the
# Lakers continue to lead, and thus their opponent has less and less time to make up the deficit.  
# But in a sense it should decrease, since the Lakers are maintaining
# only a small lead, whereas the point spread says the lead should be increasing.

t <- c(45:1) #time remaining will run from 45 mins to 1 min, just integers.

l <- 2

EL <- l + pos + ps*( t / 48 )
SD <- 13.5 * sqrt ( t / 48)

Z <- EL / SD

probwin <- pnorm(Z)
probwin

winprob <- cbind(t,probwin)

winprob

plot(probwin ~ t, xlab="time remaining in game in mins", ylab="win probability",
     main="With a Constant 2 Point Lead, In-game win probability vs how time is left", cex.lab=1.25)

#since it seems a little more natural to view the progression of in-game win probability from
#left to right (so as you move from left to right, there are fewer and fewer minutes left in the game),
#let's create a new variable, 'timepassed', which shows how many minutes have already been played in the game.

timepassed = 48-t

winprob1 <- cbind(timepassed,probwin)

winprob1

plot(probwin ~ timepassed,xlab="time passed in mins", ylab="win probability",
     main="With a Constant 2 Point Lead, In-game win probability vs How Much Time Has Passed", cex.lab=1.25)

#Interesting plot.  From the plot it looks like the Lakers probability of winning decreases slightly throughout
#the game, reaching its lowest point when there are approx 8 minutes left in the game,
#and reaching its highest point when there is only 1 minute left in the game.
#It seems to make sense that their win probability is lowest with 8 mins left because at this time, 
#both their expected lead and the standard deviation for the game are around 5 (actually, the standard
#deviation is higher than their expected lead).  But as time goes on, and the games goes into its 
#final minute, then it looks like their victory has gotten safer.  They are still leading by more than 
#2 points (as they do for the entire game), with the ball,
#and the opponent only has a minute left to overcome the deficit.

#lead is constant throughout (2), but expected lead decreases as there is less and less time left.

#SD changes throughout the game. 

#Finding numerical answers for when LA's lead is at its max and min.

sortedprob <- winprob1[order(probwin),] #a sorting of lowest to highest win probability during the game.

length(probwin) 	# 45 probabilities in the table

sortedprob[1,] 	# during the game, min. prob of winning is 82.6%
sortedprob[45,] 	# during the game, max. prob of winning is 92.7%

round(sortedprob[1,],2) 	#rounded
round(sortedprob[45,],2)	#rounded

#alternatively, could get this info. using "which()"; I will do so to confirm answer:

which(probwin==max(probwin))  #  row # of win probability at its max
which(probwin==min(probwin))  #  row # of win probability at its min

winprob1[38,]; winprob1[45,]

# max. win prob is 92.7%, with 1 min left in the game.
# min. win prob is 82.6%, with 8 mins left in the game.

#At the max and min of win probability,

Expected_lead_at_max <- EL[45] # 2.83 was the expected lead of the Lakers with 1 minute to go
Expected_stdev_at_max <- SD[45] # 1.95 was the st deviation of the diff of scores with 1 minute to go

Expected_lead_at_min <- EL[38] # 5.16 was the expected lead of the Lakers with 8 minutes to go
Expected_stdev_at_min <- SD[38]# 5.51 was the st deviation of the difference of scores with 8 minutes to go


#####################

# In addition to calculating each probability for each scenario, we can also try to make a table of 
# in-game win probabilities.  Then, at any given time in the game, we can just go to the table,
# find the spot that correctly reflects the current situation, and read off the baseline probability.

# " lead " will represent the size of the lead of the home team. 

lead <- c(1:10, 15, 20, 30)   #(of course it is possible that the home team can lead by other amounts, 
# but this seems like enough values for a table)

# Since it is of course possible that the home team can be trailing, we also want negative numbers
# (I suppose this can be thought of as a "negative lead" for the home team.)

lead <- c(-rev(lead), 0, lead)

# Would probably make more sense to have the home team leading be on the top half of the chart, so let's
# reverse again.

lead <- rev(lead)
lead

# OK, now we a vector composed of various amounts of the home team leading and trailing.  Now we need to create
# various times remaining in the game.

t <- c( 1:3/6, 1,2,5,10,12,15,24,30,36) # again, of course this doesn't capture every possible amount of time
# left in a game, but it is informative enough for this table.

# so 0.1667 (1/6 minute) corresponds to 10 seconds left in the game, 1 corresponds to 1 minute left in the game, etc.

lead;t

#to have the time decrease from left to right, need to reverse

t <- rev(t)

# let's take the case when there is 5 minutes remaining in the game,  and look at under various leads,
# what is the probability of the home team winning.

t

j <- 7 # 5  minutes remaining corresponds to seventh entry of "t".

pos <- 0.5 # if the home team has the ball, add 0.5.  If they don't, subtract -0.5.

# We'll assume the Las Vegas pointspread is 4 points, approx. the amount a home team is favored in the NBA.
# (that's because if two teams of equal ability compete, the home team is expected to win by ~ 4 points).

# We'll also assume the home team currently has the ball.  (If they don't, simply change the " + " in front
# of " pos " to a " - " .)

# 13.5 corresponds to the standard deviation in a full length game of average teams.

prob <- pnorm( ( lead + pos + 4 * ( t[j]/48) ) / (13.5 * sqrt( t[j]/48)) ) 

round(prob,2)

# now let's bind the probability column vector with the lead one so that we can see,
# with 5 mins remaining, how various leads correspond to different win probabilities.

results <- round(cbind(lead,prob),2)
results

#Looks like, if the home team has the ball with 5 minutes remaining, and they lead by 10
#or more, the game is nearly sealed.  Yet if their lead is less than 5 then the opponent
#has a greater than 10% chance of coming back and winning it.

#Note:

results[14,]

# Even if the game is tied, then just from the benefits of being the home team 
# and from having the ball, with 5 mins left, the home team still has an 58% chance to win,
# pretty significantly higher than the coin flip odds you might quickly think about when you 
# just see the score tied with 5 mins left.

# So the 27x2 matrix "results" shows a baseline probability of the home team winning a game 
# with 5 mins left, with various leads.  

# But would it be possible to create a larger matrix that shows baseline probabilities of the
# home team winning with various leads AND with various times left?  Let's try.

# Again, we use 4 as the pointspread for home court advantage, and assume home team has the ball.
# We are assuming two teams of equal ability are playing each other.
# If this is the case, the home court advantage makes the home team expected to win by approx. 4 points.
# As for possession of the ball, I simplify and assume home team has it at whatever point of the game
# we are studying, so that we always add 0.5 points to their expected lead.

t

# j <- 1 corresponds to our first entry of " t " (time), which is 36 mins left in the game.
# "prob" is the probability of home team winning with various leads

j <- 1; prob <- pnorm( ( lead + pos + 4 * ( t[j]/48) ) / (13.5 * sqrt( t[j]/48)) ) 

for (j in 2:12) ( prob <- cbind(prob, pnorm( ( lead + pos + 4 * ( t[j]/48) ) / (13.5 * sqrt( t[j]/48)) ))) 

prob

# need a for loop from j=2 to j=12 because in total we have 12 different time remaining values.
# we then use the cbind function so that we can achieve a matrix of win probabilities
# for various times left in the game and with various leads.

# finally, use round function to help ensure we don't have scientific notation.  Also use rbind and cbind
# so that we can see a listing of how much time is left and how much the home team leads by, respectively.

winprobmatrix <- round( rbind( c(0, t), cbind(lead,prob) ), 2) 
winprobmatrix

# entry [1,1] of the matrix is "0", which is just a placeholder...no value belongs in that cell.

# row 1 corresponds to how much time is left.

# column 1 corresponds to how great the home team's lead is.

# and then the entries in the matrix outside of row 1 and column 1 correspond to P(home team wins)
# at that point of the game with that lead.

# I took the liberty of bringing this table to Notepad and making it a little prettier.  
# This is the result:

f <- function(x) {
  
  "dummy function so I can show the table with no errors in R:

    home lead         time remaining (minutes) ------>                                                            
 [1,]    	    36.00 30.00 24.00 15.00 12.00 10.00 5.00 2.00 1.00 0.50 0.33 0.17

 [2,]   30    1.00  1.00  1.00  1.00  1.00  1.00 1.00 1.00 1.00 1.00 1.00 1.00
 [3,]   20    0.98  0.98  0.99  1.00  1.00  1.00 1.00 1.00 1.00 1.00 1.00 1.00
 [4,]   15    0.94  0.95  0.97  0.99  0.99  1.00 1.00 1.00 1.00 1.00 1.00 1.00
 [5,]   10    0.88  0.89  0.90  0.94  0.96  0.97 0.99 1.00 1.00 1.00 1.00 1.00
 [6,]    9    0.86  0.87  0.89  0.92  0.94  0.95 0.99 1.00 1.00 1.00 1.00 1.00
 [7,]    8    0.84  0.85  0.86  0.90  0.92  0.94 0.98 1.00 1.00 1.00 1.00 1.00
 [8,]    7    0.82  0.83  0.84  0.88  0.90  0.91 0.97 1.00 1.00 1.00 1.00 1.00
 [9,]    6    0.79  0.80  0.81  0.85  0.87  0.88 0.94 0.99 1.00 1.00 1.00 1.00
[10,]    5    0.77  0.77  0.78  0.81  0.83  0.85 0.91 0.98 1.00 1.00 1.00 1.00
[11,]    4    0.74  0.74  0.75  0.78  0.79  0.81 0.87 0.95 0.99 1.00 1.00 1.00
[12,]    3    0.71  0.71  0.72  0.74  0.75  0.76 0.82 0.91 0.97 0.99 1.00 1.00
[13,]    2    0.68  0.68  0.68  0.69  0.70  0.71 0.75 0.83 0.91 0.97 0.99 1.00
[14,]    1    0.65  0.65  0.64  0.64  0.64  0.65 0.67 0.73 0.79 0.87 0.91 0.97
[15,]    0    0.62  0.61  0.60  0.59  0.59  0.59 0.58 0.60 0.62 0.65 0.68 0.74
[16,]   -1    0.58  0.57  0.56  0.54  0.53  0.52 0.49 0.45 0.42 0.37 0.34 0.27
[17,]   -2    0.55  0.54  0.52  0.49  0.47  0.46 0.40 0.31 0.23 0.14 0.10 0.03
[18,]   -3    0.52  0.50  0.48  0.43  0.41  0.39 0.32 0.20 0.11 0.04 0.01 0.00
[19,]   -4    0.48  0.46  0.44  0.38  0.36  0.33 0.24 0.11 0.04 0.01 0.00 0.00
[20,]   -5    0.45  0.43  0.40  0.33  0.30  0.28 0.17 0.06 0.01 0.00 0.00 0.00
[21,]   -6    0.42  0.39  0.36  0.29  0.25  0.22 0.12 0.03 0.00 0.00 0.00 0.00
[22,]   -7    0.38  0.35  0.32  0.24  0.21  0.18 0.08 0.01 0.00 0.00 0.00 0.00
[23,]   -8    0.35  0.32  0.28  0.20  0.17  0.14 0.05 0.00 0.00 0.00 0.00 0.00
[24,]   -9    0.32  0.29  0.25  0.17  0.13  0.11 0.03 0.00 0.00 0.00 0.00 0.00
[25,]  -10    0.29  0.26  0.22  0.14  0.10  0.08 0.02 0.00 0.00 0.00 0.00 0.00
[26,]  -15    0.16  0.13  0.10  0.04  0.02  0.01 0.00 0.00 0.00 0.00 0.00 0.00
[27,]  -20    0.08  0.06  0.03  0.01  0.00  0.00 0.00 0.00 0.00 0.00 0.00 0.00
[28,]  -30    0.01  0.01  0.00  0.00  0.00  0.00 0.00 0.00 0.00 0.00 0.00 0.00

"
}

# Let's see a few examples.

# With 1 quarter left, how does the chance the home team wins vary with different size leads?

winprobmatrix[,1] # various leads
winprobmatrix[,6] # with 12 mins left, various win probabilities (ignore top entry "12.00")

Q4_theoretical <- cbind(winprobmatrix[,1],winprobmatrix[,6])
Q4_theoretical # calling it "Q4_theoretical" because there is only the 4th qtr left in this scenario,
# and the results are based on the above described theoretical model. 

# ignore top row, which is a dummy row.

# let's create simpler names so don't need to carry around uninformative and bulky ones

leads <- winprobmatrix[,1]
winprob_12mins <- winprobmatrix[,6]

leads
winprob_12mins

plot(winprob_12mins~leads,xlab="various leads", ylab="in-game win probability entering the 4th Quarter",
     xlim=range(-30,30),ylim=range(0,1), main="Probability of Home Team Winning With 1 Qtr Left, 
At Various Leads", cex.lab=1.25) #increase size of label fonts

# If the home team is leading or trailing by 30 to start the 4th quarter, the game is basically over.
# But if either team is within 10 points, then each team still has at least ~ 10% chance of winning.

abline(lm(winprob_12mins~leads)) # see what happens when a regression line is added

# TERRIBLE!  A simple linear regression model doesn't seem to be sufficient to describe 
# the movement of in-game win probability.

# Can try to transform the data, or a different method of regression,
# but need to keep in mind there are negative values (corresponding
# to when the home teams is losing), so some transformations of the data would not make sense.

plot(winprob_12mins~leads,xlab="various leads", ylab="in-game win probability entering the 4th Quarter",
     xlim=range(-30,30),ylim=range(0,1),
     main="Probability of Home Team Winning With 1 Qtr Left, At Various Leads")
abline(lm(winprob_12mins~0+leads))  # set y-intercept = 0. doesn't help.

summary(lm(winprob_12mins~leads))

leads2 <- leads^2

leadsrecip <- 1/leads

leads_and_constant <- leads+30

leadsc2 <- leads_and_constant^2

abline(lm(winprob_12mins~leads+(leads)^2))

summary(lm(winprob_12mins~leads))

summary(lm(winprob_12mins~leads+leads^2))

summary(lm(winprob_12mins~leads_and_constant+leadsc2))

# I try a few transformations and quick and dirty approaches here, but results at first glance
# are not promising and a little hard to interpret.  To avoid getting bogged down with this
# somewhat periphery topic (in this situation), I will proceed.  -----------

# Above I held the time constant (12 mins remaining), and checked how the probability of winning
# varied with different leads.
# Now, I will hold the lead constant (5), and check how the probability of winning
# varies with different time remaining.

plot(winprobmatrix[10,]~winprobmatrix[1,],xlab="various amounts of time left in the game",
     ylab="in-game win probability with a constant 5 point lead", xlim=range(0,36),ylim=range(0,1),
     main="With a Constant 5 point lead, Probability of Home Team Winning With Varying Time Left"
     ,cex.lab = 1.25)  # increase size of the font of the labels.

# Again, more natural to see time decreasing from left to right, so reverse:

timepassed = 48-winprobmatrix[1,]

plot(winprobmatrix[10,]~timepassed,xlab="varying amounts of time passed",
     ylab="in-game win probability with a constant 5 point lead", xlim=range(12,48),ylim=range(0,1),
     main="With a Constant 5 Point Lead, Probability of Home Team Winning With Varying Times Played",
     cex.lab = 1.25)

# Interesting, it looks like the benefit of being the home team, and the benefit of a somewhat small 
# 5 point lead, has a very big effect: At almost any point of the game, the home team has a 
# nearly 80% or greater chance of winning; on the other hand, their probability of winning 
# increases very slowly, very slowly all the way until the final few minutes.


######  Done ? ######

# If the above proposed theoretical model really does a good job of predicting in-game how likely
# the home team is to win the game, then we are all set!  Not much more work is needed here!
# But the problem is, the theoretical model is nothing more than a theoretical model.  To check
# how well it does, I will begin an analysis of data from actual NBA games, for 15 seasons. 




##################################################################################################################


## PART 2, Data Checking and Finalizing a Data Set


# Data set contains regular season games only (1993-2009, 16 seaasons).
# It is possible not all games are listed.  In the possible cases where a 
# game is not listed, it is due to a lack of a betting market on that game 
# (probably due to an uncertainty of a star player playing in the game, not revealed by the team until tipoff).
# Words like "betting market" and "spreads" and "line" are  essential here, since one of 
# the important inputs in the model is the estimate (pre-game) of how many points the home team 
# is expected to beat the road team by.

# OT (overtime) numbers are the sum of all Overtime periods.  
# If there were 2 or more OT periods, they are combined in the OT column.
# The Line is in the point of view of the Home team.  
# Some games did not have lines or totals, but are listed in this sheet. 
# This may be due to poor data collection or just no betting market for that 
# game for some reason or another. The more recent games are more likely 
# to have complete betting market data than the older games.

#######   DATA IS FROM 1993-2008 REGULAR SEASON   #######

# (1993 = 1993-94 season, 2008 = 2008-09 season. )
# (Thus, the year mentioned refers to year the season started.)

win <- read.table("win.txt", header = TRUE,sep="\t")
head(win,10)

attach(win)

#Since I didn't create this dataset (a friend did and he was kind enough to let me use it),
#before I even think about beginning analysis, I will try to check the data to the best of my abilities.
#First, I'll randomly select a bunch of rows (20), and then check their data against 
#data on the reliable site basketball-reference.com.
#In addition to this, I'll also search for any obvious errors by checking maximum and minimum values
#of all appropriate columns.

#Do random checking of rows. 

str(win)

#We have a data.frame of 18401 rows and 18 columns.

win[sample(1:nrow(win),20,replace=FALSE),]  # randomly select 20 rows for checking

#Now that we have a list of 20 rows, I will use
#http://www.basketball-reference.com/boxscores/index.cgi?month=11&day=29&year=2000
#to verify all data in those 20 rows, except for the "Line" and "Total" betting lines
#which I can't easily get my hands on.

#...The 20 rows I checked all match up exactly!  Now let's check for obvious errors by
#inspecting each column. (Note that which 20 rows are selected will be different each time
#a sample is taken, so it's possible if one again tries this checking exercise, he'll find an error).

#Before we do that, let's be sure that each variable is categorized correctly.

str(win)

#Season is an integer, but it would make more sense to be a Factor.

#Date is a Factor, but it would make more sense to be a Date.

#the others look fine, so let's just fix these two.

Season1 <- factor(Season)

Date1 <- as.Date(Date, format="%m/%d/%Y")

# (adding a " 1 " in the variable name to make the names different from the old names )

#Remove the incorrectly characterized "Date" and "Season" from data.frame "win".
#Create new data.frame "win1".

win1 <- subset(win,select = c(-Date,-Season))

#Now add in "Date1" and "Season1", which are characterized correctly.

win1 <- data.frame(win1,Date1,Season1)

str(win1) #check it.

#Yes.  Now "Date1" is characterized as a Date, and "Season1" as a Factor.  Let's use "win1" from now on.

detach(win)

attach(win1)

str(win1)

head(win1,20)

#But now "Date" and "Season" are all the way on the far right.  Let's bring them back to their original positions.

win1 <- win1[,c(17,18,1:16)]

head(win1,20)

#Looks good, looks like the original.

#Now let's check for obvious errors by inspecting each column.

str(win1)

#There are 16 levels of Seasons, which makes sense since our database should be from 1993-2008.

max(HFinal)

#Supposedly a home team scored 168 in a game. Is that possible??  Let's sort by scores and examine this.

order(HFinal)

SortedByHFinal <- win1[order(HFinal), ]

tail(SortedByHFinal,10)

#Supposedly the Denver Nuggets, on March 16, 2008, in a NON-Overtime game, scored 168 points.  Let's verify.
#Boxscore verifies these results; quarter totals match as well.  http://www.basketball-reference.com/boxscores/200803160DEN.html
#It was the 3rd most points scored in NBA history for a regulation game. http://hoopedia.nba.com/index.php?title=Denver_Nuggets

#I also see a 157, but this game went into what looks like multiple overtimes, so it is believable.
#152 and no overtime?  Verify.  http://www.basketball-reference.com/boxscores/199803130LAC.html
#Verified.  Now let's check the fewest points scored by a home team in a game.

head(SortedByHFinal,10)

#Uh oh, something looks wrong.  An NBA Team scored 17 points in an entire game?!?! 
#And scored 0 points in the 2nd, 3rd, AND 4th quarter?
#Let's examine Houston vs Toronto, March 5, 2002 (row 9616 in data frame 'win1')

#Toronto's results match perfectly, but Houston's are wrong for every quarter.
#http://www.basketball-reference.com/boxscores/200203050HOU.html
#Houston's scoring was 26, 21, 35, 30, for each quarter, and 112 total for the game.
#Our confidence in the data has decreased.  Let's make the changes in the dataset.
#I could do this either by using R's data editor ( win1 <- edit(win1 ) or going to the original
#file and editing there.  I'll use the latter approach throughout, unless otherwise stated.

detach(win1)

#new file called "win2".

win2 <- read.table("win2.txt", header = TRUE,sep="\t")

win2[9616,]

attach(win2)

#solved.

#Note-since I brought in a new dataset, the characterization of variables resets.  We'll worry
#about this later once we finish checking data, but only if it becomes an issue.

#Back to where we were, examining the fewest points scored in a game, looking for errors.

SortedByHFinal <- win2[order(HFinal), ]

head(SortedByHFinal,10)

#A home team scored 49 in a full game?  Possible, but let's investigate.
#Miami @ Chicago, April 10, 1999.  Row 6252.  Supposedly the Bulls only scored 49 points in the entire game.
#Looks correct! http://www.basketball-reference.com/boxscores/199904100CHI.html
#And indeed, this game wasn't as suspicious as the HOU vs. TOR one, since for this game the sum of 
#the quarter point totals equaled the total points for the game, which it should.
#As for the remaining games on this short list, nothing looks obviously wrong.

#Now let's examine away team's final score totals.

order(AFinal)

SortedByAFinal <- win2[order(AFinal, Date), ]

#Checking the most points scored in a game by road teams.

tail(SortedByAFinal,10)

#Nothing looks terribly outrageous.

#Checking the fewest points scored in a game by road teams.

head(SortedByAFinal,10)

#Here we go again.  Supposedly the Denver Nuggets scored 0 points in an entire game.
#March 30, 2002 (row 9790) Denver and Chicago.  Denver supposedly scored 0 in each qtr.
#Denver actually scored 100 points in this game.  http://www.basketball-reference.com/boxscores/200203300CHI.html
#Denver scored 26, 26, 27, 21, and thus had a total of 100.

detach(win2)

#Fixed errors, new dataset "win3" .

win3 <- read.table("win3.txt", header = TRUE,sep="\t")

attach(win3)

order(AFinal)

SortedByAFinal <- win3[order(AFinal), ]

head(SortedByAFinal,10)

#Now nothing looks suspicious with "AFinal", the score of the away team in the entire game.

#Now check if anything is suspicious in the result of "H1Q", the number of points the home team scored in the first quarter.

order(H1Q)

SortedByH1Q <- win3[order(H1Q, Date), ]

head(SortedByH1Q,10)

#2 points scored in Q1 by Charlotte (vs Cleveland) on Dec, 6, 2008 (row 17458)?
#total pts scored in the game are accurate, but it looks like there are errors for each quarter's totals!
#http://www.basketball-reference.com/boxscores/200812060CHA.html

detach(win3)

#fix, create "win4".  If spot many more errors, might need to declare dataset worthless.  Will have faith for now though...

win4 <- read.table("win4.txt", header = TRUE,sep="\t")

attach(win4)

tail(SortedByH1Q,10)

#Nothing obviously wrong.  Continue to look for obvious errors.

order(H2Q)

SortedByH2Q <- win4[order(H2Q, Date), ]

head(SortedByH2Q,10)

#OK.

tail(SortedByH2Q,10)

#OK.

order(H3Q)

SortedByH3Q <- win1[order(H3Q, Date), ]

head(SortedByH3Q,10)

#ERROR ALERT! Supposedly ORL (Magic) scored 0 points in the 3rd and 4th qtrs, at home vs Utah (11/29/2000, row 7863).
#http://www.basketball-reference.com/boxscores/200011290ORL.html
#They really scored 27, 20, 24, 15 (total 86, not 83).  
#Must go to dataset and make yet another change.

detach(win4)

#Fixing, creating "win5".  If I find a few more errors, I will abandon the data set.

win5 <- read.table("win5.txt", header = TRUE,sep="\t")

attach(win5)

tail(SortedByH3Q,10)

#OK

order(H4Q)

SortedByH4Q <- win5[order(H4Q, Date), ]

head(SortedByH4Q,10)

#OK

tail(SortedByH4Q,10)

#OK

order(A1Q)

SortedByA1Q <- win5[order(A1Q, Date), ]

head(SortedByA1Q,10)

#OK

tail(SortedByA1Q,10)

#OK

order(A2Q)

SortedByA2Q <- win5[order(A2Q, Date), ]

head(SortedByA2Q,10)

#OK

tail(SortedByA2Q,10)

#OK

order(A3Q)

SortedByA3Q <- win5[order(A3Q, Date), ]

head(SortedByA3Q,10)

#OK

tail(SortedByA3Q,10)

#OK

order(A4Q)

SortedByA4Q <- win5[order(A4Q, Date), ]

head(SortedByA4Q,10)

#OK

tail(SortedByA4Q,10)

#OK

order(HOT)

SortedByHOT <- win5[order(HOT, Date), ]

head(SortedByHOT,10)

#OK

tail(SortedByHOT,10)

#OK

order(AOT)

SortedByAOT <- win5[order(AOT, Date), ]

head(SortedByAOT,10)

#OK

tail(SortedByAOT,10)

#OK

order(Line)

SortedByLine <- win5[order(Line, Date), ]

head(SortedByLine,10)

#The DAL @ HOU game supposedly had a line of -107 (supposedly the Rockets were favored to win by 107) (row 17332).
#Since of course this is impossible, there was obviously an error.  There are two ways we can handle this.
#We can either delete out this entire row (win1 <- win1[-which(win1$Line == -107.0),])
#or we can simply get rid of the Line for this game, act as if it is missing.
#Since all of the other entries for the game are perfectly correct, I will do the latter.

detach(win5)

win6 <- read.table("win6.txt", header = TRUE,sep="\t")

attach(win6)

win6[17332,]

#So now there is an "NA" for the line for this game.

#Checking the other side of the pointspreads (how large are the positive ones? are there any outrageous ones?).

tail(SortedByLine,10)

#Not very helpful since the only thing I see in the "Line" column is "NA"...I want to check the highest positive values.
#Thus, I need to exclude or move the rows for which Line is "NA" .
#(There are NAs, and correctly so, since for some games, there was no line (for whatever reason).)

order(Line, na.last = FALSE)

SortedByLine <- win1[order(Line, na.last = FALSE), ]

tail(SortedByLine,10)

#OK

#Since I don't plan for "Total", aka the "over/under" of a game, to be used in this analysis, I won't inspect it.
#(Over/under is a betting line regarding how many combined points the teams are expected to score.  So if the over/under
#is 200, and you bet over, then you are betting that the teams will combine to score more than 200 points).

#OK! After checking for errors in the dataset, we did indeed find a few obvious ones.  
#With that said, since this is a pilot study and since the vast majority of what I checked was correct, 
#and since there are so many datapoints that even if there are a few uncorrected errors they probably
#shouldn't have too big of an impact, we will proceed to use this dataset in the analysis.

#The dataset to be used will be the most correct version, "win6".

#Before we proceed, I did a lot of cleaning above, so let's check what is stored in R:

ls.str()

# Since I definitely no longer need "win", "win1"...."win5", since they have errors,
# let's get rid of those data.frames.

rm(win,win1,win2,win3,win4,win5)  # remove those dataframes with errors.

ls.str()    # those 6 dataframes removed, but "win6", what we will be using, remains.

##################################################################################################################


## Part 3,  Preparing for Data Analysis: Prelim Analysis

# Recall, 4 pieces of info we need to calculate in-game win probability:
# Current lead, Las Vegas points spread, time remaining, and who has possession.
# Dataset ("win6") has the first 3 pieces of information, and not having the 4th one
# shouldn't be that important, especially when there is a lot of time remaining.

# But let's first do a very preliminary analysis comparing a result from 
# the theoretical model to a model derived from the data.

length(HFinal)  # side question, how often does the home team beat the road team in a full game?
i <- ifelse(HFinal > AFinal,1,0) # home team winning corresponds to " i " = 1
sum(i)
sum(i)/length(HFinal)  # about 60.3% of the time.  Home court advantage is important!

win6 <- cbind(win6,i) # add the indicator variable to the data.frame so we know outcome of each game.
head(win6,5)

sum(!is.na(HOT))  # of the 18401 games in our dataset, 1125 went into OT (6.1% of games)
length(HOT) # 18401

# Of the 1125 OT games, what % did the home team win?

sum(ifelse(!is.na(HOT),i,0))  # home team won 580 of the 1125 OT games, or 51.6%.

# Being at home for a full-length game is clearly much more important than in a game
# that's already gone into overtime! P(home team wins)=60.3% vs 51.5%.

# Very Prelim Analysis, analyzing the situation with 12 mins left in the game (so start of Q4).

D3Q <- H3Q+H2Q+H1Q-A3Q-A2Q-A1Q #Home team's total score - Away team's total score at the start of Q4.

summary(lm(i~D3Q)) # R^2 is 0.4123. # Linear model appropriate to model win prob based on size of lead?

D3Q2 <- D3Q^2

summary(lm(i~D3Q+D3Q2)) # R^2 is 0.4215--adding a squared term doesn't help much.

D3Q3 <- D3Q^3

summary(lm(i~D3Q+D3Q2+D3Q3)) # R^2 is 0.452---adding a cubed term helps a little.

d <- c(0,30,20,15,10,9:-10,-15,-20,-30) # We will check how various differences affect the win probability.

winprob_datacube <- 5.36*10^-1+3.54*10^-2*d-6.76*10^-5*d^2-1.84*10^-5*d^3 #with sq and cubed term

winprob_datacube <- ifelse(winprob_datacube < 1, winprob_datacube[], 1) # a probability cant be > 1.

winprob_datacube <- ifelse(winprob_datacube > 0, winprob_datacube[], 0) # make negative probability = 0.

# plot of win prob vs diff in lead, start of 4th qtr, sq and cubed term included, using data
plot(winprob_datacube ~ d,xlab="Increased Difference in Lead",ylab="Probability Home Team Wins, Start of Q4",
     main="How Much Increased Lead Affects Win Probability At Start of Q4, the cubic model, using data" ,cex.lab = 1.25) 

winprob_data <- 0.5292 + 0.0260*d  # using the simple linear regression model (just linear term).

# What this (simple model) is saying is that, at the start of the 4th quarter, if the game is
# tied, the the home team has an approx 53% chance of winning the game.  Naturally, this win 
# probability is affected by how great the home team's current lead is at the start of the 4th quarter.
# For each additional point that lead is, their probability of winning increases by 2.6%.

winprob_data #  but a probability can't be greater than 1! Force any prob > 1 to be 1.

winprob_data <- ifelse(winprob_data < 1, winprob_data[], 1)

winprob_data

plot(winprob_data ~ d, xlab="Increased Difference in Lead",ylab="At Start of Q4, Probability Home Team Wins",
     main="How Much Increased Leads Affect Win Probability At Start of Q4, Using Data, Simple Linear Model",cex.lab=1.25)
abline(lm(winprob_data~d),col="red") # add least squares lines

# The plot shows the linear increase (until P=1) of the home team winning with each 
# additional point lead at the start of the 4th quarter (so with 12 minutes left in the game,
# how much is each extra point lead going to affect the home team's chance of winning?).

# Based on the theoretical model, probabilities with various leads at the start of the 4th:

Q4_theoretical <- cbind(winprobmatrix[,1],winprobmatrix[,6])
Q4_theoretical

Q4_data <- cbind(d,winprob_data)  # based on data
Q4_data

Q4_theo_data <- cbind(Q4_theoretical,Q4_data)

Q4_theo_data

Q4_theo_data <- Q4_theo_data[,-3] # get rid of redunant time column, also ignore row 1 (dummy row).

Q4_theo_data

winprob_data <- ifelse(winprob_data > 0, winprob_data[], 0) # make negative probability = 0.

Q4_data <- cbind(d,winprob_data)

Q4_theo_data <- cbind(Q4_theoretical,Q4_data) 

Q4_theo_data <- Q4_theo_data[,-3] # get rid of redunant time column, also ignore row 1 (dummy row).

round(Q4_theo_data,2) # ignoring row 1, column 2 shows probability based on theory, column 3, based on data.
# column 1 shows various size leads of the home team.  Time is start of Q4.

# Results that this (very) simple linear regression model provide and those that
# our theoretical model provide are quite different. That could be somewhat expected since the 
# theoretical model is also incorporating point spread and possession, which would increase the 
# probability of the home team winning and thus mainly explain the difference we observe
# (and by incorporating more factors it is, we hope, doing a better job of predicting).
# With that said, the models seem in pretty good agreement as to when the game is "safe"- when one team is
# almost definitely going to win.  Going into the 4th, for the home team a lead of about 18 is very secure,
# and for the road team, a lead of about 20 is secure.

# Let's use graphics to compare the model based on theory vs data, and let's also specifically look at
# if the models are in accordance in saying when the lead is "safe":  P(home team wins) = 1 or 0 )

# Recall a plot I made earlier, showing how the in-game win probability at the start of Q4
# changes with various leads.  Let's compare that plot (based on the theoretical model) with the 
# plot I can make from the very simple data analysis above (using simple linear regression).

###### Two separate plots, next to each other.

plot(winprob_data ~ d, xlab="Increased Difference in Lead",ylab="Probability Home Team Wins",
     main="How Much Increased Leads Affect Win Probability (using data)") 

x11(width=5,height=5.5)  
plot(winprob_12mins~leads,xlab="various leads", ylab="in-game win probability",
     xlim=range(-25,25),ylim=range(0,1),  #changing the x-range a little to conform with above plot
     main="Probability of Home Team Winning With 1 Qtr Left, At Various Leads (using theory)")

# They look a little different.

###### Two plots one over the other.

{
  
  par(mfrow=c(2,1))    # places two scatterplots in same plot, one over the other
  
  plot(winprob_data ~ d, xlim=range(-25,25),ylim=range(0,1),xlab="Increased Difference in Lead",
       ylab="Probability Home Team Wins", main="How Much Increased Leads Affect 
Win Probability (using data)", col="green")
  
  plot(winprob_12mins~leads, xlim=range(-25,25),ylim=range(0,1), xlab="various leads", 
       ylab="in-game win probability",main="Probability of Home Team Winning With 1 Qtr Left, 
At Various Leads (using theory)",col="orange")
  
  par(mfrow=c(1,1))    # reset
  
}

# But since the model with the sq and cubic term had a higher R^2 than the simple linear model,
# let's see how that compares to the theoretical model--show the plots of all 3 models.

##### Three plots one over the other.

{
  
  par(mfrow=c(3,1))    # places three scatterplots in same plot, one over the other
  
  plot(winprob_data ~ d, xlim=range(-25,25),ylim=range(0,1),xlab="Increased Difference in Lead",
       ylab="Probability Home Team Wins", main="How Much Increased Leads Affect 
Win Probability (using data, linear model)", col="red",lwd="4",cex.lab=1.2)
  
  plot(winprob_datacube ~ d,xlab="Increased Difference in Lead",ylab="Probability Home Team Wins",
       main="How Much Increased Leads Affect Win Probability (using data, cubic model)", col="green",lwd="4",cex.lab=1.2)
  
  plot(winprob_12mins~leads, xlim=range(-25,25),ylim=range(0,1), xlab="various leads", 
       ylab="in-game win probability",main="Probability of Home Team Winning With 1 Qtr Left, 
At Various Leads (using theory)",col="orange",lwd="4",cex.lab=1.2)
  
  par(mfrow=c(1,1))    # reset
  
}

# Definitely looks like the model with the sq and cubed term (middle) more resembles the 
# theoretical model (bottom) than does the simple model (top).


##### Three plots plotted on the same axes.

plot(d,winprob_data, col="red",xlab="Difference in Lead", ylab="Home Team's Win Probability",
     main="Win Probability at the Start of Q4, Affected By Size of Lead",pch=23,lwd="3")

points(d,winprob_datacube, col="green",pch=18,lwd="5")
points(leads,winprob_12mins, col="orange",pch=21,lwd="3")

legend(15,0.4, c("data simple", "data cubed", "theory"), col=c("red", "green", 
                                                               "orange"), pch=c(23, 18, 21))

# as we saw, the cubic model using the data is somewhat similar to the theoretical model.

## Anyway, that was just a very preliminary look at the data, with a few graphical outputs.
## Now we will aim to more rigorously compare the theoretical model to what the data says.



##################################################################################################################


## Part 4, Comparing the Theoretical Model to Actual Results (based on 16 seasons of data)

sum(!is.na(Line)) # 18,108 of the 18,401 games in the dataset "win6" have a Line (pointspread).

##### PLAN: For every game in the database that has a Line (18,108 games), I will use the 
##### theoretical model to calculate, at the start of the 2nd, 3rd, and 4th quarters, 
##### the probability of winning that game.  A random example (using fake numbers):
##### I will have the following at my disposal: at the start of Q2, P(home team wins) = 65%,
##### for that same game, at the start of Q3, P(home team wins) = 71%, for that same game,
##### at the start of Q4, P(home team wins) = 55%.  And finally,  i = 1 or 0, depending on if
##### the home team actually won (1 if they won).

##### Then, to find out how well the model predicted the results, I will look at all P(home team wins)
##### at the start of Q2, and do a comparison with how frequently teams at different probabilities
##### actually won their games.

# create new variables so I can do the in-game win probability calculations.

Q1Lead <- H1Q - A1Q
Q2Lead <- H1Q+H2Q - (A1Q+A2Q)
Q3Lead <- H1Q+H2Q+H3Q - (A1Q+A2Q+A3Q)
FinalLead <- HFinal - AFinal

win6 <- cbind(win6,Q1Lead,Q2Lead,Q3Lead,FinalLead) # bind those variables

# First focus on calculating in-game win probability at the start of Q2.

# Recall that SD is the st dev. of difference in scores for the remaining part of the game.

# SD <- 13.5 * sqrt( t / 48  )  # "t" = time remaining in game.

SD36 <- 13.5 * sqrt( 36 / 48 ) # at the start of Q2, there are 
# 3 full quarters left, 12*3 = 36 mins left.

SD36 # at the start of the 2nd qtr, the st dev. of difference in scores
# for the rest of the game. is 11.7.

# the following is the Expected Lead formula from our theoretical model.

# EL <- l + pos + ps*( t / 48 )

# Since our dataset doesn't tell us who has possession, we will remove that input.

# EL <- l + ps*( t / 48 ) # "ps" is pointspread, which is called "Line" in the dataset.

ps <- -1*Line  #(multiply by -1 since a negative pointspread means that that team is better)
# in other words, if the Lakers are -10, it means they are expected to win by 10.

l <- Q1Lead  # "l" is lead.

EL36 <- l + ps*( 36 / 48 ) # 36 mins left at the start of Q2.

SD36  # with 36 mins. left, a constant for all games, 11.7
EL36  # based on each team's current lead and pointspread.

Z <- EL36 / SD36

probAwins_36 <- pnorm(Z)

head(probAwins_36,20) # home team's in-game win probability at start of Q2, predicted by theoretical model.

win6 <- cbind(win6, probAwins_36)

# I will compare the theoretical model to actual results by first creating bins, consisting
# of every 10% in win probability.  For example, if model says home team has 5% chance
# of winning the game, it will be in bin 1 (bin 1 is from 0%-10%, 10% < bin 2 <=20%, etc.

a <- probAwins_36  # use "a" to make writing more convenient.

mean(a, na.rm = TRUE) # at the start of the 2nd quarter, the avg P(home team wins) is 59.6% (theory)

f <- function(a)  	# use a function so I can create the 10 bins
{
  if(a <= .10){
    b <- 1
  } else if ( .10 < a & a <= .20) {
    b <- 2
  } else if ( .20 < a & a <= .30) {
    b <- 3
  } else if ( .30 < a & a <= .40) {
    b <- 4
  } else if ( .40 < a & a <= .50) {
    b <- 5
  } else if ( .50 < a & a <= .60) {
    b <- 6
  } else if ( .60 < a & a <= .70) {
    b <- 7
  } else if ( .70 < a & a <= .80) {
    b <- 8
  } else if ( .80 < a & a <= .90) {
    b <- 9
  } else if ( .90 < a & a <= 1) {
    b <- 10
  }
  b
}

bins <- ceiling(10*a) # multiply by 10 and use the ceiling function to make the 10 bins, ranging from 1 to 10.

head(bins,50) # the entries filled with "NA" are those for the games for which there was no line available, 
# and thus an in-game win probability could not be calculated using the theoretical model.

win6 <- cbind(win6,bins)

head(win6,5)

# how many games fall into each bin? 
# (remember, a bin indicates how large, measured at the start of Q2, is the team's probability of 
# winning that game, based on the theoretical model.  The higher the bin, the higher the probability.)

hist(bins,freq=TRUE, breaks = seq(1, 10, by= 1),main="Bins Describing at Start of Q2 Prob Home Team wins")

binsbreakdown <- hist(bins,freq=TRUE, breaks = seq(1, 10, by= 1))$counts
binsbreakdown

# Thankfully, we have many observations ( > 1300) in each bin.  

winQ1_data <- tapply(i,bins,mean) # how frequently do the home teams in each bin end up winning?

winQ1_theory <- c(.05,.15,.25,.35,.45,.55,.65,.75,.85,.95)

compareQ1 <-  cbind(winQ1_data,winQ1_theory)

diff_data_theory <- c(compareQ1[,1]-compareQ1[,2])

compareQ1 <- cbind(compareQ1, diff_data_theory)

compareQ1  # interesting indeed!  it looks like the model at start of Q2 does quite a good job in games when the game is close
# but it struggles a lot when it appears that the home team has little chance of coming back (it
# underestimates a comeback; the home team comes back from large deficits more than theory says it should).

##### Let's do the same thing for comparing theory to data results at the start of the 3rd quarter.
##### Since there are 12 less minutes left in the game (compared to at the start of the 2nd quarter),
##### perhaps the model does even better.

# Recall that SD is the st dev. of difference in scores for the remaining part of the game.

# SD <- 13.5 * sqrt( t / 48  )  # "t" = time remaining in game.

SD24 <- 13.5 * sqrt( 24 / 48 ) # at the start of Q3, there are 
# 2 full quarters left, 12*2 = 24 mins left.

SD24 # at the start of the 2nd qtr, the st dev. of difference in scores
# for the rest of the game. is 9.55.

# the following is the Expected Lead formula from our theoretical model.

# EL <- l + pos + ps*( t / 48 )

# Since our dataset doesn't tell us who has possession, we will remove that input.

# EL <- l + ps*( t / 48 ) # "ps" is pointspread, which is called "Line" in the dataset.

ps <- -1*Line  #(multiply by -1 since a negative pointspread means that that team is better)
# in other words, if the Lakers are -10, it means they are expected to win by 10.

l <- Q2Lead  # "l" is lead.

EL24 <- l + ps*( 24 / 48 ) # 24 mins left at the start of Q3.

SD24  # with 24 mins. left, a constant for all games, 9.55
EL24  # based on each team's current lead and pointspread.

Z <- EL24 / SD24

probAwins_24 <- pnorm(Z)

head(probAwins_24,20) # home team's in-game win probability at start of Q3, predicted by theoretical model.

win6 <- cbind(win6, probAwins_24)

# I will compare the theoretical model to actual results by first creating bins, consisting
# of every 10% in win probability.  For example, if model says home team has 5% chance
# of winning the game, it will be in bin 1 (bin 1 is from 0%-10%, 10% <bin 2 <=20%, etc.

a <- probAwins_24  # use "a" to make writing more convenient.

mean(a, na.rm = TRUE) # at the start of the 3nd quarter, the avg P(home team wins) is 60.0% (theory)

f <- function(a)  	# use a function so I can create the 10 bins
{
  if(a <= .10){
    b <- 1
  } else if ( .10 < a & a <= .20) {
    b <- 2
  } else if ( .20 < a & a <= .30) {
    b <- 3
  } else if ( .30 < a & a <= .40) {
    b <- 4
  } else if ( .40 < a & a <= .50) {
    b <- 5
  } else if ( .50 < a & a <= .60) {
    b <- 6
  } else if ( .60 < a & a <= .70) {
    b <- 7
  } else if ( .70 < a & a <= .80) {
    b <- 8
  } else if ( .80 < a & a <= .90) {
    b <- 9
  } else if ( .90 < a & a <= 1) {
    b <- 10
  }
  b
}

bins <- ceiling(10*a) # multiply by 10 and use the ceiling function to make the 10 bins, ranging from 1 to 10.

head(bins,50) # the entries filled with "NA" are those for the games for which their was no line available, 
# and thus an in-game win probability could not be calculated using the model.

win6 <- cbind(win6,bins)

head(win6,5)

# how many games fall into each bin? 
# (remember, a bin indicates how large, measured at the start of Q3, is the team's probability of 
# winning that game, based on the theoretical model.  The higher the bin, the higher the probability.)

hist(bins,freq=TRUE, breaks = seq(1, 10, by= 1),main="Bins Describing at Start of Q3 Prob Home Team wins")

binsbreakdown <- hist(bins,freq=TRUE, breaks = seq(1, 10, by= 1))$counts
binsbreakdown

# Thankfully, we still have many observations ( > 1000) in each bin, though now with only 2 qtrs left,
# the distribution has become bimodal (at each extreme).

winQ2_data <- tapply(i,bins,mean) # how frequently do the home teams in each bin end up winning?

winQ2_theory <- c(.05,.15,.25,.35,.45,.55,.65,.75,.85,.95)

compareQ2 <-  cbind(winQ2_data,winQ2_theory)

diff_data_theory <- c(compareQ2[,1]-compareQ2[,2])

compareQ2 <- cbind(compareQ2, diff_data_theory)

compareQ2  # the model looks like it has improved!  Most notably, it now does much better at the extremes, and
# looks like throughout the various win probabilities it does fairly decently.



##### Finally, let's see how well it does at the start of the 4th qtr, with only 12 minutes remaining in the game.


# Recall that SD is the st dev. of difference in scores for the remaining part of the game.

# SD <- 13.5 * sqrt( t / 48  )  # "t" = time remaining in game.

SD12 <- 13.5 * sqrt( 12 / 48 ) # at the start of Q4, there is 
# 1 full quarter left, 12*1 = 12 mins left.

SD12 # at the start of the 4th qtr, the st dev. of difference in scores
# for the rest of the game. is 6.75.

# the following is the Expected Lead formula from our theoretical model.

# EL <- l + pos + ps*( t / 48 )

# Since our dataset doesn't tell us who has possession, we will remove that input.

# EL <- l + ps*( t / 48 ) # "ps" is pointspread, which is called "Line" in the dataset.

ps <- -1*Line  #(multiply by -1 since a negative pointspread means that that team is better)
# in other words, if the Lakers are -10, it means they are expected to win by 10.

l <- Q3Lead  # "l" is lead.

EL12 <- l + ps*( 12 / 48 ) # 12 mins left at the start of Q4.

SD12  # with 12 mins. left, a constant for all games, 6.75
EL12  # based on each team's current lead and pointspread.

Z <- EL12 / SD12

probAwins_12 <- pnorm(Z)

head(probAwins_12,20) # home team's in-game win probability at start of Q4, predicted by theoretical model.

win6 <- cbind(win6, probAwins_12)

# I will compare the theoretical model to actual results by first creating bins, consisting
# of every 10% in win probability.  For example, if model says home team has 5% chance
# of winning the game, it will be in bin 1 (bin 1 is from 0%-10%, 10% < bin 2 <= 20%, etc.

a <- probAwins_12  # use "a" to make writing more convenient.

mean(a, na.rm = TRUE) # at the start of the 4th quarter, the avg P(home team wins) is 60.17% (theory)

f <- function(a)  	# use a function so I can create the 10 bins
{
  if(a <= .10){
    b <- 1
  } else if ( .10 < a & a <= .20) {
    b <- 2
  } else if ( .20 < a & a <= .30) {
    b <- 3
  } else if ( .30 < a & a <= .40) {
    b <- 4
  } else if ( .40 < a & a <= .50) {
    b <- 5
  } else if ( .50 < a & a <= .60) {
    b <- 6
  } else if ( .60 < a & a <= .70) {
    b <- 7
  } else if ( .70 < a & a <= .80) {
    b <- 8
  } else if ( .80 < a & a <= .90) {
    b <- 9
  } else if ( .90 < a & a <= 1) {
    b <- 10
  }
  b
}

bins <- ceiling(10*a) # multiply by 10 and use the ceiling function to make the 10 bins, ranging from 1 to 10.

head(bins,50) # the entries filled with "NA" are those for the games for which their was no line available, 
# and thus an in-game win probability could not be calculated using the model.

win6 <- cbind(win6,bins)

head(win6,5)

# how many games fall into each bin? 
# (remember, a bin indicates how large, measured at the start of Q4, is the team's probability of 
# winning that game, based on the theoretical model.  The higher the bin, the higher the probability.)

hist(bins,freq=TRUE, breaks = seq(1, 10, by= 1),main="Bins Describing at Start of Q4 Prob Home Team wins")

binsbreakdown <- hist(bins,freq=TRUE, breaks = seq(1, 10, by= 1))$counts
binsbreakdown

# Thankfully, we still have many observations ( > 900) in each bin, though now with only 1 qtr left,
# the distribution has become very clearly bimodal (at each extreme).

winQ3_data <- tapply(i,bins,mean) # how frequently do the home teams in each bin end up winning?

winQ3_theory <- c(.05,.15,.25,.35,.45,.55,.65,.75,.85,.95)

compareQ3 <-  cbind(winQ3_data,winQ3_theory)

diff_data_theory <- c(compareQ3[,1]-compareQ3[,2])

compareQ3 <- cbind(compareQ3, diff_data_theory)

compareQ3  # the model does terrifically at the extremes (probably due to such great sample sizes of data)
# and also does pretty well at the intermediaries.  The results are quite satisfying.


##############Summary-

# It looks like the model overall does a pretty good job of in-game predicting winners. However, it looks like it 
# doesn't value enough "momentum", or in a statistical sense, deviations.  It underestimates the chance of a home
# team coming back from a deficit, and overestimates the probability that the home team will hold onto a lead.  
# Subsequent work is necessary to test this formula more rigorously; by instead of just looking at 3 times
# in thegame (after Q1, Q2, and Q3, as I did above), looking at every part of the game (so with 1 min left,
# 2 mins left, 3 mins left, etc.).  My suspicion is that the formula will break down at the very end of the game
# (when the CLT no longer kicks in).  In addition, my "bin" method of analysis could almost certainly be improved upon,
# even if I'm not sure how.  Someone with more knowledge could probably do a better job with that, and he'd then
# be able to get a better reading of how well the model works.  Finally, new models could then be tried to see
# how one could improve the theoretical model.  My work, thus, was just a beginning.
























































































































































































































































































