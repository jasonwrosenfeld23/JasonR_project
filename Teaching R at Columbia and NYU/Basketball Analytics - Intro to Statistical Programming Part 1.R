###R Super-Basics

#Addition
1 + 2

#Multiplication
3 * 3

#Aliasing
a <- 3 * 3

a

b <- 2

#Basic Plotting
plot(a,b)

#Cleanup environment! (remove)
rm(a,b)


###R Basics

##Download 2018-19 NBA Stats from https://www.basketball-reference.com/leagues/NBA_2019.html
#On the Team Stats leaderboard, click to export as csv, then copy
#and paste it into a new text file. Save file as "bballref"

#Read and View Data
library(readr)
stats <- read_csv("NYU Basketball Analytics/Class Sessions/Session 2/bballref")
View(stats)

#View first 6 (or n) rows
head(stats)
tenteams <- head(stats, 10)
tail(stats, 3)

#Correct Column Name
names(stats)[1]<-"Rank"

#Remove unwanted rows from bottom
stats1 <- head(stats,30)
View(stats1)

#Basic Scatter Plot
plot(stats1$PTS,stats1$'FG%') #put FG% in single quotes because % sign

#Add Title and Axis Labels
plot(stats1$PTS,stats1$'FG%',  
     main="NBA Points vs FG% (2018-2019)", 
     xlab = "Points", 
     ylab = "FG%" )


