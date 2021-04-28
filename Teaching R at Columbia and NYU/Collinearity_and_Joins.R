# Let's pick up where we left off in nbastatR.R to 
# look at an example of collinearity

model1 = lm(Adv2019$MEAN_minutes ~ Adv2019$MEAN_PER + Adv2019$ws + Adv2019$BPM)
summary(model1)

model2 = lm(Adv2019$MEAN_minutes ~ Adv2019$MEAN_PER + Adv2019$ws)
summary(model2)

model3 = lm(Adv2019$MEAN_minutes ~ Adv2019$MEAN_PER)
summary(model3)


# If we had looked at the correlation matrix from the 
# beginning, we could've seen this coming

library(dplyr)
library(corrplot)

clean = Adv2019 %>%
  select(-c(1))

result = cor(clean)
corrplot(result)


#And now let's learn about joins!
joined <- 
  inner_join(salary, dataBREFPlayerTotals, 
            by = c("namePlayer" = "namePlayer", "slugSeason" = "slugSeason"), 
            copy = TRUE)
#Be careful when joining by names! Use IDs whenever possible!
#We can do inner joins, left joins, and more!
