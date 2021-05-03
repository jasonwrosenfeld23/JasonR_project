# It's Monday, May 3, 2021 and the owner of Google's hometown NBA team, the Golden State Warriors, has come to me for help.  After several years at the top of the league, his team has struggled the past two seasons (currently ranked 9th in the Western Conference).  In his quest to get back to the top, he wants to know what is truly necessary to "win big" (his words) in the NBA so he can focus his franchise building plans around what matters most.

# Unfortunately, he told me about this project at 11:00 am today and he wants to meet crosstown at 1:00 pm, giving me only 75 minutes to 'throw something together' (also his words).  Oh, he also did not give me more color regarding what he wants than that, but he has mentioned a few hypothesis previously that would probably be good to test.  Let's get to it.

#############################################################################################################

# The owner often quotes NBA legend Pat Riley who says 'No rebounds, no rings.'  In other words, if you don't rebound well, you aren't going to win championships. 

# We can either download to CSVs the relevant data we need from basketball-reference.com, and then read those CSVs into R, or parse the comments of the webpage, motivated by this https://stackoverflow.com/questions/40665907/web-scraping-data-table-with-r-rvest (Note: NBA team personnel have data feeds and so rarely needs to get data from basketball-reference.com!).

adv <- "http://www.basketball-reference.com/leagues/NBA_2021.html?lid=header_seasons#all_misc_stats"

library(rvest)
library(readr)  

h <- adv %>% read_html()  

nba_2021 <- h %>% html_nodes(xpath = '//comment()') %>%   
  html_text() %>%  
  paste(collapse = '') %>%    
  read_html() %>%    
  html_node('table#misc_stats') %>%   
  html_table() %>%   
  { setNames(.[-1, ], paste0(names(.), .[1, ])) } %>%    
  type_convert()    

nba_2021[1:3, 1:10]

# This seems to work well, so let's do it for the 4 prior years, giving us 5 years of data total.  It also gives us data beyond the rebounding data, which we will leverage for additional analyses.


adv <- "http://www.basketball-reference.com/leagues/NBA_2020.html?lid=header_seasons#all_misc_stats"

h <- adv %>% read_html()  

nba_2020 <- h %>% html_nodes(xpath = '//comment()') %>%   
  html_text() %>%  
  paste(collapse = '') %>%    
  read_html() %>%    
  html_node('table#misc_stats') %>%   
  html_table() %>%   
  { setNames(.[-1, ], paste0(names(.), .[1, ])) } %>%    
  type_convert()    


adv <- "http://www.basketball-reference.com/leagues/NBA_2019.html?lid=header_seasons#all_misc_stats"

h <- adv %>% read_html()  

nba_2019 <- h %>% html_nodes(xpath = '//comment()') %>%   
  html_text() %>%  
  paste(collapse = '') %>%    
  read_html() %>%    
  html_node('table#misc_stats') %>%   
  html_table() %>%   
  { setNames(.[-1, ], paste0(names(.), .[1, ])) } %>%    
  type_convert()    


adv <- "http://www.basketball-reference.com/leagues/NBA_2018.html?lid=header_seasons#all_misc_stats"

h <- adv %>% read_html()  

nba_2018 <- h %>% html_nodes(xpath = '//comment()') %>%   
  html_text() %>%  
  paste(collapse = '') %>%    
  read_html() %>%    
  html_node('table#misc_stats') %>%   
  html_table() %>%   
  { setNames(.[-1, ], paste0(names(.), .[1, ])) } %>%    
  type_convert()    


adv <- "http://www.basketball-reference.com/leagues/NBA_2017.html?lid=header_seasons#all_misc_stats"

h <- adv %>% read_html()  

nba_2017 <- h %>% html_nodes(xpath = '//comment()') %>%   
  html_text() %>%  
  paste(collapse = '') %>%    
  read_html() %>%    
  html_node('table#misc_stats') %>%   
  html_table() %>%   
  { setNames(.[-1, ], paste0(names(.), .[1, ])) } %>%    
  type_convert()    

# Great, now we have five seasons of data.  For the purposes of this analysis, let's merge them.

nba_5s <- rbind(nba_2021, nba_2020, nba_2019, nba_2018, nba_2017)

str(nba_5s)

# Hm, 5 seasons x 30 teams = 150 rows, not 155.  Why five extra?

head(nba_5s$Team,40)

# Ah, there is a "League Average" row, presumably for each of the 5 seasons.  We need to remove them.

nba_5s <- nba_5s[nba_5s$Team != 'League Average', ]

str(nba_5s)

head(nba_5s$Team,40)  # Perfect, the five league average rows are gone.

View(nba_5s)  # Now what data do we want to look at to test 'no rebounds, no rings'?

# Well, there are two types of rebounding: offensive and defense.  And they are measured in several different ways, but a widely accepted way, and which we have data for, is offensive and defensive rebounding %, respectively.  How correlated are they to winning?

# Let's plot them first
  
attach(nba_5s)

# But there's no win % column, so we'll need to create one

winperc <- W / (W + L)

nba_5s <- cbind(nba_5s, winperc)

rm(winperc)

View(nba_5s) # great, now we have win percentage.  

str(nba_5s) # But a bunch of variables have spaces! 

#  Can clean up using 'stringr' or 'janitor' packages or just use backticks

ORebperc <- `Offense Four FactorsORB%`
DRebperc <- `Defense Four FactorsDRB%`

nba_5s <- cbind(nba_5s,ORebperc, DRebperc)  

rm(ORebperc)
rm(DRebperc)

View(nba_5s) # Looks good, now let's plot

plot(nba_5s$ORebperc, nba_5s$winperc , xlab = 'Offensive Rebounding Percentage', ylab = 'Regular Season Win %', main = 'No Rebounds, No Rings(???)')

##  Well, there is no information on here regarding who won the championship and how correlated offensive rebounding is with that, so relabel and analyze

plot(nba_5s$ORebperc, 100*nba_5s$winperc , xlab = 'Offensive Rebounding Percentage', ylab = 'Regular Season Win %', main = 'Offensive Rebounding Relationship with Regular Season Winning (2017-2021)', col="blue")

# Seems to be a weak relationship

cor.test(nba_5s$ORebperc, nba_5s$winperc) # no statistical significance between a team's offensive rebounding performance and winning in the regular season.  But maybe it matters to win the championship?  Let's first look defensively...

plot(nba_5s$DRebperc, 100*nba_5s$winperc , xlab = 'Defensive Rebounding Percentage', ylab = 'Regular Season Win %', main = 'Defensive Rebounding Relationship with Regular Season Winning (2017-2021)', col="darkorange")  # Looks like a positive correlation there!

cor.test(nba_5s$DRebperc, nba_5s$winperc) # Statistically significant correlation between defensive rebounding well and winning in the regular season.

cor.test(nba_5s$ORebperc,nba_5s$DRebperc) # Notice that there is no correlation between how well a team rebounds offensively and defensively

## Ok, so it doesn't look like offensive rebounding is that closely correlated to winning in the regular season, though defensive rebounding is correlated to winning games.  But maybe it matters when it comes to "winning big*"

# *Because the owner did not define for us what he meant when he said 'winning big', we'll have to define that for ourselves.  Since there are 30 NBA teams and only one can win the championship each year, let's define 'winning big' as finishing top 4 for the season - ie, making the Eastern or Western Conference Championship.  

write.csv(nba_5s, "nba5s.csv")  # In the interest of time, writing to CSV this dataset, marking the teams who 'won big', and bringing that dataset back into R.  Since this current season isn't complete and thus we don't know who will 'win big', we'll just use the other four seasons.

nba5s <- read.csv("nba5s.csv", header = TRUE)

head(nba5s,3)

detach(nba_5s)

attach(nba5s)

sum(winbig) # 4 teams have 'won big' each of the last 4 years, giving us 16 big winners.  Now how strong were they at offensive and defensive rebounding?

plot(ORebperc, 100*winperc , xlab = 'Offensive Rebounding Percentage', ylab = 'Regular Season Win %', main = 'No Rebounds No Rings?', col=ifelse(winbig == 1, "red", "blue"),cex=ifelse(winbig == 1, 3, 1),pch=ifelse(winbig == 1, 16,1))
text(20,20, "Teams who 'won big' are in huge red circles", cex =.8, col = 'darkgreen')
abline(v = mean(ORebperc),lwd=3, lty=2, col="darkgreen")

# As expected, teams who 'won big' also won a lot of games in the regular season, and a few of those teams were elite offensive rebounding teams - but many were not.  

mean(ORebperc)

# Avg off reb % of ALL teams in these four seasons was 22.7; from a quick glance at this scatterplot, it looks like roughly half of the big win teams were BELOW AVERAGE in this stat

plot(DRebperc, 100*winperc , xlab = 'Defensive Rebounding Percentage', ylab = 'Regular Season Win %', main = 'Did the Win Big Teams Defensive Rebound That Well? (2017-2020)', col=ifelse(winbig == 1, "red", "orange"),cex=ifelse(winbig == 1, 3, 1),pch=ifelse(winbig == 1, 16,1))
text(74,22, "Teams who 'won big' are in huge red circles", cex =.8, col = 'darkblue')

# This plot, for defensive rebounding, seems to tell a different story.

abline(v = mean(DRebperc),lwd=3, lty=2, col="darkgreen")

# With that said, rebounding big is clearly not necessary to win big - look at the big red dot on the far left of the page.

head(nba5s[with(nba5s, order(-winbig,-DRebperc)),],20) # Turns out that the very poor defensive rebounding team that won big was none other than Google's hometown team, the Warriors!  That Warriors team also won the championship that year!!

########################################

# Ok, now let's quickly check the owner's other hypothesis - is it true that 'defense wins championships'??

head(nba5s,2)

# There are various ways to measure defense, but the most popular one in the basketball analytics community nowadays is Defensive Rating, a measure of how many points you allow per 100 possessions.  This is better than the traditional points allowed per game metric, since Defensive Rating accounts for pace, ie, how fast you and your opponent play.  Fortunately, we also have this metric, 'DRtg', in our dataset.

cor.test(DRtg,winperc) # Nothing unexpected here - the fewer points you allow (ie, the better you play defense), the more games you'll win.

# So let's cut right to the chase - how good of a defense do you need to win big?

tapply(DRtg, winbig, summary) # Remember, the lower the number, the better, since the fewer points you allow, the better

# Unsurprisingly, the big win teams have a lower DRtg average than the non big win teams.  But there are clearly some big win teams with bad defensive ratings (ie, high) - a max 111.9 means that a big win team had a significantly worse defense than an average team

plot(DRtg, winbig, main = 'Four "Big Win" Teams Had Def Ratings WORSE than the League Average!') # Not pretty but instructive. 
abline(v=mean(DRtg),lwd=2,lty=1,col="darkorange")

# Dive deeper

with ( nba5s [ winbig == 1 ,  ], plot(DRtg, xlab = 'All 16 teams who have won big', ylab = 'DRtg, the higher the number, the worse', main ='Does Defense Win Championships?'))
abline(h = mean(DRtg),lwd=3, lty=2, col="darkgreen")
text(8.5,110, "this green line is the average DRtg of ALL teams, not just the 16 win big teams", col='red',cex=.8)

# While the majority of teams who have won big had a defensive rating better than league average, 4 of the 16 teams were below avg, and 2 were just about avg.

# While it makes sense intuitively and is backed by data that playing good defense helps you win games, it is not true that you MUST be a great defensive team to win big - there are several examples clearly disproving the owner's hypothesis

       
##############################################################

# Running out of time, but let's try to get to the heart of the owner's questions before we work on the presentation.

head(nba5s,2)

# A popular way of analyzing the game of basketball is breaking the game down into "four factors" - how well a team shoots, controls the ball, rebounds, and gets to the free throw line.  It's really eight factors, since there are four on offense and four on defense.  Fortunately for us, all eight are in our dataset.

# If we want to get a quick sense of how much each of these factors predicts winning regular season games, we could run a linear regression.  NOTE!!!!!  With more time, we'd want to check if all the OLS assumptions are met.  

summary(lm(W ~ Offense.Four.FactorseFG. + Offense.Four.FactorsTOV. + Offense.Four.FactorsORB. + Offense.Four.FactorsFT.FGA + Defense.Four.FactorseFG. + Defense.Four.FactorsTOV.+ Defense.Four.FactorsDRB. + Defense.Four.FactorsFT.FGA))

# As expected, all eight factors are significant, with the strongest significance found in the two shooting variables - how well you make shots, and how well you prevent your opponent from making shots.  And that makes sense - to score points in basketball, after all, you need to get the ball through the hoop.

# In fact, the regression output implies than each additional 1% in your team's effective shooting percentage will lead to an increase of 3.6 wins on the season!

# How about using these factors in predicting your chance of 'winning big'?  Since the response variable is binary (0 or 1, not a big win team or a big win team), logistic regression makes sense to use.   NOTE!!!!!  With more time, we'd want to check if all the logistic regressions assumptions are met.  

summary(glm(winbig ~ Offense.Four.FactorseFG. + Offense.Four.FactorsTOV. + Offense.Four.FactorsORB. + Offense.Four.FactorsFT.FGA + Defense.Four.FactorseFG. + Defense.Four.FactorsTOV.+ Defense.Four.FactorsDRB. + Defense.Four.FactorsFT.FGA), family = binomial)

# Now ONE single variable dominates - how well the team shoots.  Even how well a team defends shooting, while it may be important, has too high of a standard error to know.  This implies that if you want to win big, it's, by far, most important for you to shoot really well.

# Because it's logistic regression, we can convert to odds ratio or probabilities to see the effect of changes in the predictor variables

exp(-2.1+8.63*.55-3.8*.55-1.162*.20)/(1+exp(-2.1+8.63*.55-3.8*.55-1.162*.20))

exp(-2.1+8.63*.56-3.8*.55-1.162*.20)/(1+exp(-2.1+8.63*.56-3.8*.55-1.162*.20))

# An increase of 1% eFG% means (holding all else constant) a 2% increase in chance of being a big win team!

##########################################################

# Time to deliver the results to the owner!

# He does not like ppts or long emails; he prefers to discuss research in person, and is only OK with me bringing along a one-pager to reference.  So, here it is.


https://docs.google.com/document/d/1BOtUqQ5CfIVzOgOzKTrg3jawW2XU45e6rQwWG3LMapA/edit








