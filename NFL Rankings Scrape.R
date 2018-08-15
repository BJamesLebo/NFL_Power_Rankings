library(XML)
library(RCurl)

u <- "https://www.pro-football-reference.com/years/2017/games.htm"
newu <- getURL(u)
data <- readHTMLTable(newu)

#begin new method
#had to convert data to csv file and do manipulations in Excel, due to
#there being no home/away format in the website's data

data <- read.csv("Schedule.csv")

frame <- data
frame$Away.Neutral <- as.character(frame$Away.Neutral)
frame$Home.Neutral <- as.character(frame$Home.Neutral)
frame$PTSA <- as.numeric(as.character(frame[[6]]))
frame$PTSH <- as.numeric(as.character(frame[[10]]))
frame <- frame[!is.na(frame$PTSA),]

#data frame now contains basic game data for whole season including playoffs

games <- data.frame(frame$Away.Neutral,frame$PTSA,frame$Home.Neutral,frame$PTSH)
games$frame.Away.Neutral <- as.character(games$frame.Away.Neutral)
games$frame.Home.Neutral <- as.character(games$frame.Home.Neutral)
games$pt_dif <- games$frame.PTSH - games$frame.PTSA
head(games)

team <- character(length = 2*length(games$pt_dif))
opponent <- character(length = 2*length(games$pt_dif))
location <- character(length = 2*length(games$pt_dif))
ptdif <- vector(mode='numeric',length = 2*length(games$pt_dif))

clean <- data.frame(team,opponent,location,ptdif)
clean$team <- as.character(clean$team)
clean$opponent <- as.character(clean$opponent)
clean$location <- as.character(clean$location)

for(i in 1:length(games$pt_dif)){
  
  clean$team[i] <- games$frame.Home.Neutral[i]
  clean$opponent[i] <- games$frame.Away.Neutral[i]
  clean$location[i] <- "H"
  clean$ptdif[i] <- games$pt_dif[i]
  
  
  clean$team[i + length(games$pt_dif)] <- games$frame.Away.Neutral[i]
  clean$opponent[i + length(games$pt_dif)] <- games$frame.Home.Neutral[i]
  clean$location[i + length(games$pt_dif)] <- "A"
  clean$ptdif[i + length(games$pt_dif)] <- (-1)*games$pt_dif[i]
  
  
}

#Correct for the 3 NFL games this season that were played at neutral locations
clean$location[33] <- "N"
clean$location[49] <- "N"
clean$location[100] <- "N"
clean$location[108] <- "N"
clean$location[158] <- "N"
clean$location[33 + length(games$pt_dif)] <- "N"
clean$location[49 + length(games$pt_dif)] <- "N"
clean$location[100 + length(games$pt_dif)] <- "N"
clean$location[108 + length(games$pt_dif)] <- "N"
clean$location[158 + length(games$pt_dif)] <- "N"
head(clean)

#                   team             opponent location ptdif
# 1 New England Patriots   Kansas City Chiefs        H   -15
# 2     Cleveland Browns  Pittsburgh Steelers        H    -3
# 3   Cincinnati Bengals     Baltimore Ravens        H   -20
# 4  Washington Redskins  Philadelphia Eagles        H   -13
# 5        Detroit Lions    Arizona Cardinals        H    12
# 6       Houston Texans Jacksonville Jaguars        H   -22

boxplot(clean$ptdif[1:(.5*length(clean$ptdif))], col = "blue", horizontal = TRUE, 
        main = "Home Point Differentials for 2017 NFL Season", 
        xlab = "Home Score - Away Score")
abline( v = 0, col = "red")

lm.NFLfootball <- lm(ptdif ~ team + opponent + location, data = clean) 
#Let's take a look at some of the values
lm.NFLfootball$coefficients[1:7]

#           (Intercept)          teamAtlanta Falcons 
#            -2.3759799                    8.0327468 
#  teamBaltimore Ravens            teamBuffalo Bills 
#             7.1068320                   -0.2119995 
# teamCarolina Panthers            teamChicago Bears 
#             8.0924690                    2.4827468 
#teamCincinnati Bengals         teamCleveland Browns 
#            -1.1515829                   -6.9922395

#Now we can create the rankings using these coefficients. We linearly adjust 
#each value so that the mean of all the rankings is 0. This allows us to see 
#teams as better or worse than an average NFL team immediately with their value.

rankings <- data.frame("team" = sort(unique(clean$team)),
                       "bjlsa_coeff" = rep(NA, 32))
scale_factor <- mean(lm.NFLfootball$coefficients[2:32])
rankings$bjlsa_coeff <- c(0, lm.NFLfootball$coefficients[2:32]) - scale_factor
rankings <- rankings[(order(rankings$bjlsa_coeff, decreasing = T)),]
rankings
#                   team bjlsa_coeff
#26  Philadelphia Eagles    9.311059
#18     Los Angeles Rams    9.177991
#22   New Orleans Saints    8.863230
#20    Minnesota Vikings    8.809063
#21 New England Patriots    8.628203
#15 Jacksonville Jaguars    6.587314
#27  Pittsburgh Steelers    4.891796
#5     Carolina Panthers    4.139107
#2       Atlanta Falcons    4.079384
#17 Los Angeles Chargers    3.574765
#16   Kansas City Chiefs    3.337563
#3      Baltimore Ravens    3.153470
#11        Detroit Lions    2.530640
#29     Seattle Seahawks    1.786059
#9        Dallas Cowboys    1.495781
#32  Washington Redskins   -1.410952
#6         Chicago Bears   -1.470616
#30 Tampa Bay Buccaneers   -1.470749
#12    Green Bay Packers   -2.085893
#28  San Francisco 49ers   -2.954219
#31     Tennessee Titans   -3.570504
#1     Arizona Cardinals   -3.953362
#4         Buffalo Bills   -4.165362
#25      Oakland Raiders   -4.685388
#24        New York Jets   -5.058151
#7    Cincinnati Bengals   -5.104945
#19       Miami Dolphins   -6.262749
#13       Houston Texans   -6.519732
#10       Denver Broncos   -6.810223
#23      New York Giants   -7.667197
#14   Indianapolis Colts  -10.183140
#8      Cleveland Browns  -10.945602

#Strip Chart
stripchart(rankings$bjlsa_coeff, pch = 19 , col = "blue", 
xlab = "BJLSA Coefficient", main = "BJLSA Coefficients for 2017 Season")

clean$predscore <- predict(lm.NFLfootball, newdata = clean)
clean$win <- ifelse(clean$ptdif > 0, 1, 0)
glm.pointspread <- glm(win ~ predscore, data = clean, family = "binomial")
clean$winprob <- predict(glm.pointspread, newdata = clean, type = "response")

plot(clean$predscore, clean$winprob, xlab = "Home Predicted Score Differential", 
     ylab = "Home Win Probability", 
     main = "Logistic Function for Predicting Games with the BJLSA NFL Model", 
     pch = 4, col = c("red","green")[(clean$ptdif > 0) + 1])
     legend(5,.5,legend=c("Actual Home Win", "Actual Road Win"), fill=c("green","red"))
  
        