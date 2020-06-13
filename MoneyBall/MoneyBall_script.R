# MONEYBALL  PROJECT

# Background explanation of the project: https://www.youtube.com/watch?v=yGf6LNWY9AI

# The main purpose of this script is to recruit under-valued baseball players using data exploration techniques.

# RULES
# The total combined salary of the three players can not exceed 15 million dollars.
# Their combined number of At Bats (AB) needs to be equal to or greater than the lost players.
# Their mean OBP had to equal to or greater than the mean OBP of the lost players

library(ggplot2)
library(dplyr)

batting <- read.csv('Batting.csv')
print(head(batting))
str(batting)
print(head(batting$AB))
print(head(batting$X2B))

# Batting Average (BA)
batting$BA <- batting$H / batting$AB
print(tail(batting$BA, 5))

# On Base Percentage (OBP)
batting$OBP <- (batting$H + batting$BB + batting$HBP) / (batting$AB + batting$BB + batting$HBP + batting$SF) 

# 1B (Singles)
batting$X1B <- batting$H - batting$X2B - batting$X3B - batting$HR

# Slugging Percentage (SLG)
batting$SLG <- (batting$X1B + 2*batting$X2B + 3*batting$X3B + 4*batting$HR) / (batting$AB)

str(batting)

sal <- read.csv('Salaries.csv')
summary(batting)
summary(sal)

# Subset retired players
batting <- subset(batting, yearID >= 1985)

combo <- merge(batting,sal,by=c('playerID','yearID'))

# Players that the 2002 Oakland A's lost and need to be replaced
lost_players <- subset(combo,playerID %in% c('giambja01','damonjo01','saenzol01'))
lost_players <- subset(lost_players,yearID==2001)
lost_players <- lost_players[,c('playerID','H','X2B','X3B','HR','OBP','SLG','BA','AB')]
head(lost_players)
# The combined AB of the lost players is 1469 and their average OBP is 0.364

combo <- subset(combo, yearID == 2001)

ggplot(combo, aes(x=OBP,y=salary)) + geom_point(size=2)

# Salary should be relatively low and OBP should be above 0 to discard players who barely played
combo <- subset(combo, salary < 8000000 & OBP > 0)

# Filter players with an AB above 450
combo <- subset(combo, AB >= 450)

options <- head(arrange(combo, desc(OBP)),10)
options[,c('playerID','AB','salary','OBP')]

print('heltoto01, berkmla01, gonzalu01')
# The players printed meet the conditions
# The combined salary of these players is barely $10088333
# berkmla01 seems to be highly undervalued with an AB of 577, OBP of 0.43 and a salary of only $305000!

# END