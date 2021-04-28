#Read NBA Seasonal data
library(readr)
NBA_seasonal <- read_csv("~/NYU Basketball Analytics/Class Sessions/Session 4/NBA_seasonal", 
        skip = 1)

#Create scatter plot of league average scoring by season
#(same as homework except showing all years in the dataset)
plot(-NBA_seasonal$Rk, NBA_seasonal$PTS,   
     main="NBA League Average Points Per Game", 
     xlab = "Number of Years before 2021", 
     ylab = "NBA Average PPG" )

#How can we make Season into a numerical variable?
View(NBA_seasonal)
#This is a job for dplyr! 
#Uncomment the next line the first time you run this
#install.packages("dplyr") #say yes if R asks questions
library(dplyr)

#Calculate a numerical field for Season
NBA_seasonal1 = NBA_seasonal %>% #This is a "pipe" 
        mutate(num_season = as.numeric(substr(NBA_seasonal$Season,1,4))+1)
#The "pipe" (%>%) tells dplyr to keep reading.
#"mutate" creates a new column in the data.
#In this case, we are creating a new column called "num_season".
#"substr" allows us to cut the "Season" field into 4-digit years.
#"as.numeric" converts the resulting 4 digits to a numerical variable.
#"+1" at the end makes sure the years reflect the end of the season.

#Now plot the season using our new variable
plot(NBA_seasonal1$num_season, NBA_seasonal1$PTS,   
     main="NBA League Average Points Per Game", 
     xlab = "Season", 
     ylab = "NBA Average PPG" )


#Convert to line graph
plot(NBA_seasonal1$num_season, NBA_seasonal1$PTS, 
     type = "l",
     main="NBA League Average Points Per Game", 
     xlab = "Season", 
     ylab = "NBA Average PPG" )


#Test correlation two variables
#The syntax of the function is cor(x,y)
cor(NBA_seasonal1$`ORB%`, NBA_seasonal1$PTS)
#Error! It's because there are NAs!
cor(NBA_seasonal1$`ORB%`, NBA_seasonal1$PTS, use = "complete.obs")
#.21 (weak positive correlation)


###Create Correlation Matrix

#First, remove extranious and/or non-numeric fields
NBA_seasonal2 = NBA_seasonal1 %>%
        select(-c(1:3,5)) #"select" chooses the columns to show/remove
#"-c" means remove whichever columns are specified in the ()
#"c" means show whichever columns are specified in the ()
JustForFun <- NBA_seasonal1 %>%
        select (c(1:2))

#Correlation Matrix
CM = cor(NBA_seasonal2, use = "complete.obs")
View(CM)
#Visualize Correlation Matrix with CORRPLOT
#Uncomment the next line the first time you run this
#install.packages("corrplot")
library(corrplot)
corrplot(CM, type = "upper")


#Linear Regression 
#The syntax of the function is lm(y ~ x1 + x2 + x3...)
model <- lm(NBA_seasonal2$PTS ~ 
                    NBA_seasonal2$FG + 
                    NBA_seasonal2$'3P' + 
                    NBA_seasonal2$FT)
summary(model)

