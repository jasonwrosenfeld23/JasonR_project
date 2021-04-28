covid <- read.csv("owid-covid-data.csv", header = TRUE)

str(covid) 

Date1 <- as.Date(covid$date, format="%m/%d/%Y") # date stored as characters, must convert to date 

summary(Date1) # looks good

covid <- data.frame(covid,Date1) # add the Date column we want

covid <- covid[,-4]  # remove the one we don't

attach(covid)

summary(covid) # Do diagnostics to the rest of the data - anything wrong?

# Something seems wrong in new_cases - how could there be negative new_cases?  # Also, new_deaths has negative numbers, as does new_tests. 

summary(new_cases)

# Supposedly there were -74347 new cases in a day.  Where and when?

covid[which(new_cases == -74347),]

# Looks like Spain on March 2, 2021 supposedly had -74347 new cases.  Let's check our source to be sure the issue didn't arise here in R

# If you select Confirmed cases New per day and reverse sort On March 2, 2021, there it is - Spain with -74,347!
# https://ourworldindata.org/explorers/coronavirus-data-explorer?tab=table&zoomToSelection=true&time=2021-03-02..2021-04-26&pickerSort=asc&pickerMetric=location&Metric=Confirmed+cases&Interval=New+per+day&Relative+to+Population=false&Align+outbreaks=false&country=~OWID_WRL

# Is this problem widespread?  

summary(new_deaths)

covid[which(new_deaths == -1918),]

# Another worrisome result coming out of Spain.  Supposedly they had negative 1918 new deaths on May 25, 2020.  Also matches our source.

# How worried should we be?  How much data do we know is troublesome?

sum(new_cases < 0, na.rm = TRUE) # 61 negative new cases
sum(new_deaths < 0, na.rm = TRUE) # 83 negative new deaths
sum(new_tests < 0, na.rm = TRUE) # 12 negative new tests

# This is worrisome.  We could now either throw out this data, try to correct it by using previous or more recent numbers, use country or groups of country averages; or write this dataset off completely if we think the quality of the data is that bad.  For the purposes of this exercise, let's do a quick fix to what appears to be a definitely wrong number and proceed with the analysis.

# Look at Spain data surrounding worrisome -74347 deaths.  

covid[71708:71728,1:6]

# Interesting, there are zeros 2 of every 7 days, which turn out to be weekend days, and naturally a big Monday spike.  The -74347 remains hard to explain. Since it's a Tuesday, let's replace it with the following day's number, 6137.

covid[71718,6] = 6137

####  *** Note - handling possibly bad and missing data is important!!!!!  With more time, I'd delve way more deeply into what might be wrong and implement comprehensive solutions, BEFORE I proceeded with additional analyses.  I'm only not now given the time sensitivity around this work. 

# Now let's try getting some insights from the data.

# First, what's happening with the daily number of confirmed cases worldwide? 

plot(Date1, covid$new_cases) # not pretty, and not all that informative

# Start March 15, 2020 when companies started shutting down in-person work and there was an increase in testing; also do per million and add labels

plot(Date1[Date1>'2020-03-14'],new_cases_per_million[Date1>'2020-03-14'], xlab="Date", ylab="New Cases Each Day (in millions)", ylim=c(0,5000))

# Ok, but given the bumpy nature of the data, let's see smoothed data

plot(Date1[Date1>'2020-03-14'],new_cases_smoothed_per_million[Date1>'2020-03-14'], xlab="Date", ylab="New Cases Each Day (in millions)", ylim=c(0,3000))

# Note: The number of confirmed cases is lower than the number of actual cases; the main reason for that is limited testing.

# Looks like peak new cases was reported in Q4 - but when exactly?  Andlet's improve the visual

worldwide_new_cases <- covid[which(location == "World"),]

library(scales)
library(ggplot2)
ggplot(worldwide_new_cases, aes(Date1,new_cases_smoothed_per_million)) +  xlab("Start of Covid Testing Through Today") + ylab("New Cases Each Day (in mm)") + ggtitle("New Covid Cases (excl. China), in millions")  +  geom_bar(fill="red",size=.3,  stat="identity", position="identity") +  scale_x_date(date_breaks = "2 months", labels = date_format("%b-%Y"), limits = as.Date(c('2020-03-10','2021-04-26')))

# New covid cases were at a local peak beginning in the beginning of the year, and are again peaking now.

# Now investigate one country in Europe
  
with ( covid [ location =="Italy",  ], plot(Date1 , new_cases_smoothed_per_million,main = "Italy New Covid Cases - 14 Months Trend", xlab = "Covid Start to Today, April 26 2021", ylab = "New Covid Cases in Italy (in mm)", col="darkgreen"))

# And a country in South America

with ( covid [ location =="Argentina",  ], plot(Date1 , new_cases_smoothed_per_million, main = "Argentina New Covid Cases - 14 Months Trend", xlab = "Covid Start to Today, April 26 2021",  ylab = "New Covid Cases in Argentina (in mm)", col = "blue"))

# Do on one plot to make comparison easier 

with ( covid [ location =="Italy",  ], plot(Date1 , new_cases_smoothed_per_million, main = "Italy and Argentina New Covid Cases - 14 Months Trend", xlab = "Covid Start in March 2020, to Today, April 26 2021",  ylab = "New Covid Cases in Italy and Argentina (in mm)", col="darkgreen", pch=18, lwd="2"))

with ( covid [ location =="Argentina",  ], points(Date1 , new_cases_smoothed_per_million, col ="blue", pch="5", lwd="2"))

legend("topleft", legend = c("Italy","Argentina"), col=c("darkgreen", "blue"),lty=1.2, cex=.8, box.lty=2, box.lwd=2, box.col="red")

# At the start of covid, Argentina was not even tracking its cases while Italy was, hence the gap early on.  But once both were tracking, Argentina's cases quickly surpassed Italy's, and such a trend stayed for nearly 6 months.  Then in Q4, covid got really bad in Italy while things improved in Argentina; now, it's the reverse: there recently have been improvements in Italy while Argentina is getting ravaged.  

# In fact, Argentina has one of the highest rates of new covid cases each day now.

a <- covid[order(Date1, new_cases_per_million, na.last = TRUE,decreasing = TRUE),c(3,11,59)] 

rank <- 1:nrow(a)

head(cbind.data.frame(a,rank),20) # Argentina is 13th in new_cases_per_million per day as of April 26, 2021.

# Now let's see how the wealth of a nation affects vaccines

# Specifically, the relationship between "gdp_per_capita" and "people_vaccinated_per_hundred"

with ( covid [ Date1 =="2021-04-26",  ], plot(gdp_per_capita, people_vaccinated_per_hundred, ylab = "Vaccinated People", col = "blue", xlab = "GDP Per Capita", main = "GDP per Cap vs Vaccinated Ppl per hundred as of April 26 2021"))

# Appears to be a trend - the richer countries are vaccinating more of their people. And the very poor countries are struggling to get their people vaccinated.

# What's the correlation as of April 26, 2021??

with ( covid [ Date1 =="2021-04-26",  ], cor(gdp_per_capita,people_vaccinated_per_hundred, use = "complete.obs")) # .47

# Another look at the plot seems to show a few outliers.  Specifically, a very rich country.

max(gdp_per_capita,na.rm=TRUE)

head((covid[which(gdp_per_capita == 116935.6),]),1)  # 

# Turns out that country is Qatar, with a $116,935.6 gdp per capita.  They stand out not because their vaccine rate is low, but because their gdp per capita is so high.

with ( covid [ Date1 =="2021-04-26",  ], max(people_vaccinated_per_hundred, na.rm = TRUE))

head((covid[which(people_vaccinated_per_hundred == 64.16),]),1)  

# Isle of Man is another visual outlier, an extremely high rate of vaccines distributed given their GDP per capita.

# Let's end on another positive note, by analyzing the huge increase in vaccinations.

library(dplyr)

tot_fully_vaccinated <- covid %>% group_by(Date1) %>% summarise(x = sum(people_fully_vaccinated, na.rm = TRUE))

tail(tot_fully_vaccinated, 3)  # More people fully vaccinated by April 25 then by April 26 2021?  Something must be off...  Ah!  It's because there are already sums within the dataset across continents.  We don't need to sum over countries/continents - we can use the 'World' location directly!

detach("package:dplyr", unload = TRUE)

world_fully_vaccinated <- covid[which(location == "World"),]

tail(world_fully_vaccinated[,c(36,59)],5)  # Great, this makes sense.  Now how positive is the trend of people around the world getting vaccinated?

library(scales)
library(ggplot2)
ggplot(world_fully_vaccinated, aes(Date1,people_fully_vaccinated /1000000)) +  xlab("Start of Vaccine Rollout Through Today") + ylab("Fully vaccinated people worldwide (in mm)") + ggtitle("Fully Vaccinated People Globally (excl. China), in millions")  +  geom_bar(fill="orange",size=.3,  stat="identity", position="identity") +  scale_x_date(date_breaks = "4 weeks", labels = date_format("%b-%Y"), limits = as.Date(c('2021-01-01','2021-04-26')))

# We've made great progress with vaccinations.  While there was a major shortage of vaccines to start the year and it took from the new year to middle of February to get 50 mm people globally fully vaccinated, there is now an additional 50 mm people vaccinated nearly every two weeks (not including those on mainland China)!!


