# Intro to ggplot ----

library(tidyverse)

james <- read.csv('lebronjames_career.csv') 

line_graph <- james %>% 
  mutate(Season = as.numeric(substr(Season, 1, 4))) %>% # Converting season from factor to numeric
  select(Season, PTS) %>% 
  drop_na() # Drop career row

# Line chart ----

ggplot(data = line_graph, aes(x = Season, y = PTS)) +
  geom_line()
  # geom_line(color = 'red', size = 2) +
  # labs(title = 'Lebron James Scoring by Season',
  #      subtitle = 'Looking at Lebrons Points per 100 possessions over the course of his career',
  #      x = '',
  #      y = 'Points per 100 Possessions',
  #      caption = 'Source: Basketball Reference') +
  # scale_x_continuous(breaks = seq(2003,2019,1)) +
  # theme_minimal() +
  # theme(plot.title = element_text(face = 'bold'),
  #       panel.grid.minor.x = element_blank(),
  #       axis.text.x = element_text(angle = 45))


# Bar Chart ----

off_def <- read.csv('offense_defense.csv') %>% 
  filter(MP >= 1000)

head(off_def, 10)

defense_by_position <- off_def %>% 
  group_by(Pos) %>% 
  summarise(DBPM = mean(DBPM))

head(defense_by_position, 5)

ggplot(data = defense_by_position, aes(x = Pos, DBPM)) +
  geom_bar(stat = 'Identity')

# ggplot(data = defense_by_position, aes(x = reorder(Pos, desc(DBPM)), y = DBPM)) +
#   geom_bar(stat = 'Identity', fill = 'royalblue', color = 'navy') +
#   labs(title = 'Defensive Performance by Position',
#        x = 'Position',
#        y = 'Defensive Box Plus/Minus',
#        caption = 'Source: Basketball Reference') +
#   theme_minimal() +
#   theme(plot.title = element_text(size = 16, face = 'bold', hjust = .5),
#         axis.title = element_text(face = 'bold'))




# Scatter plot ----

ggplot(data = off_def, aes(x = OWS, y = OBPM)) +
  geom_point()
  # geom_point(aes(color = Pos, size = MP)) +
  # geom_point(color = 'royalblue', size = 3, alpha = .60) +
  # geom_smooth(method = 'lm', color = 'red') +
  # labs(title = 'Comparing Offensive Value Metrics',
  #      subtitle = 'Looking at Offensive Win Shares against Offensive Box Plus/Minus',
  #      x = 'Offensive Win Shares',
  #      y = 'Offensive Box Plus/Minus',
  #      caption = 'Source: Basketball Reference') +
  # theme_classic() +
  # theme(plot.title = element_text(face = 'bold', hjust = .5),
  #       plot.subtitle = element_text(face = 'italic', hjust = .5),
  #       axis.title = element_text(face = 'bold'))
  # 
  # 
