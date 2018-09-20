library(XML)
library(RCurl)
u <- "https://www.pro-football-reference.com/years/2018/games.htm"
newu <- getURL(u)
raw <- readHTMLTable(newu, as.is = T)

#remove unecessary games
today <- raw$games
today <-today[!(today$Pts=='Pts'),]
index <- min(which(today$PtsA==""))
cur_date <- today$Date[index]
cur_date
#changing column names to differentiate btwn home and away
colnames(today)[5] <- "PtsA"
colnames(today)[8] <- "PtsH"


slate <- which(today$Date == cur_date)
predictions <- data.frame(today$`HomeTm`[slate],today$`VisTm`[slate])
colnames(predictions) <- c("Home", "Away")
predictions$Home <- as.character(predictions$Home)
predictions$Away <- as.character(predictions$Away)
locs <- rep("H", length(slate))

#Let's take a look at today's games
predictions

#define a function that takes in a team & opponent & location, then uses the 2 linear
#models to output the predicted home score differential and win probability

ptdif_call <- function(home,away,HN){
  
  arr <- c(0,0)
  
  r1 <- rankings$bjlsa_coeff[which(rankings$team == home)]
  r2 <- rankings$bjlsa_coeff[which(rankings$team == away)]
  
  
  if(HN == "H"){
    pt_dif <- r1 - r2 - coefficients(lm.NFLfootball)[[1]]
  }
  
  if(HN == "N"){
    pt_dif <- r1 - r2
  }
  
  arr[1] <- pt_dif
  prob <- 1 / (1+ exp(- coefficients(glm.pointspread)[[2]] * pt_dif))
  arr[2] <- prob
  
  return(arr)
}

#now call the function for each game that is happening today

predictions$pt_dif <- rep(0,length(slate))
predictions$home_prob <- rep(0,length(slate))

for(i in 1:length(slate)){
  predictions$pt_dif[i] <- ptdif_call(predictions$Home[i], predictions$Away[i], locs[i])[1]
  
  predictions$home_prob[i] <- ptdif_call(predictions$Home[i], predictions$Away[i], locs[i])[2]
}

#clean up the final data by rounding and sorting

predictions$pt_dif <- round(predictions$pt_dif, digits = 2)
predictions$home_prob <- round(predictions$home_prob, digits = 2)
predictions <- predictions[order(predictions$home_prob,decreasing = T),]

names(predictions) <- c("Home", "Away", "home_pt_dif", "home_win_prob")
write.csv(predictions, "NBA_TODAY_PREDICTIONS.csv", row.names = FALSE)

#Let's take a look at our predictions for today  
predictions
