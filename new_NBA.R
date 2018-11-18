library(RJSONIO)
library(httr)
library(BBmisc)
library(stringr)
library(stringi)

# season <- '2014-15'
# season.type <- 'Regular+Season'

getPLAYERdata <- function(season,season.type){

json.url <- paste("http://stats.nba.com/stats/leaguegamelog?Counter=1000&DateFrom=&DateTo=&Direction=DESC&LeagueID=00&PlayerOrTeam=P&Season=",season,"&SeasonType=",season.type,"&Sorter=DATE",sep="")

idk <- fromJSON(json.url, nullValue = NA)

test <- matrix(unlist(idk$resultSets[[1]]$rowSet), ncol=length(idk$resultSets[[1]]$headers), byrow = TRUE) %>% data.frame

colnames(test) <- paste(idk$resultSets[[1]]$headers)

test$GAME_DATE <- paste(
  substr(test$GAME_DATE,6,7),
  "%2F",
  substr(test$GAME_DATE,9,10),
  "%2F",
  substr(test$GAME_DATE,1,4),
  sep = ""
)

#Converting variables to appropriate types
test$PLAYER_ID <- as.character(test$PLAYER_ID)
test$GAME_DATE <- as.character(test$GAME_DATE)
test$MIN <- as.numeric(as.character(test$MIN))
test$FGM <- as.numeric(as.character(test$FGM))
test$FGA <- as.numeric(as.character(test$FGA))
test$FG_PCT <- as.numeric(as.character(test$FG_PCT)) 
test$FG3M <- as.numeric(as.character(test$FG3M)) 
test$FG3A <- as.numeric(as.character(test$FG3A)) 
test$FG3_PCT <- as.numeric(as.character(test$FG3_PCT))
test$FTM<- as.numeric(as.character(test$FTM))
test$FTA <- as.numeric(as.character(test$FTA))
test$FT_PCT <- as.numeric(as.character(test$FT_PCT))
test$OREB <- as.numeric(as.character(test$OREB))
test$DREB <- as.numeric(as.character(test$DREB))
test$REB <- as.numeric(as.character(test$REB))
test$AST <- as.numeric(as.character(test$AST))
test$STL <- as.numeric(as.character(test$STL))
test$BLK <- as.numeric(as.character(test$BLK))
test$TOV <- as.numeric(as.character(test$TOV))
test$PF <- as.numeric(as.character(test$PF))
test$PTS <- as.numeric(as.character(test$PTS))
test$PLUS_MINUS <- as.numeric(as.character(test$PLUS_MINUS))

TEAM_ID_GAME_DATE <- matrix(unique(paste(test$TEAM_ID, test$GAME_DATE, sep="")))

test$PLAYER_ID_GAME_DATE <- paste(test$PLAYER_ID, test$GAME_DATE, sep="")

return(test)

}

reg.data.P <- getPLAYERdata(season = '2014-15',season.type = 'Regular+Season')

playoff.data.P <- getPLAYERdata(season = '2014-15',season.type = 'Playoffs')

#####

getTEAMdata <- function(season, season.type){
  url <- paste("http://stats.nba.com/stats/leaguegamelog?Counter=1000&DateFrom=&DateTo=&Direction=DESC&LeagueID=00&PlayerOrTeam=T&Season=",season,"&SeasonType=",season.type,"&Sorter=PTS",sep="")
  data <- RJSONIO::fromJSON(url, nullValue = NA)
  teamperf <- matrix(unlist(data$resultSets[[1]]$rowSet),ncol=29,byrow = TRUE)
  teamperf <- data.frame(teamperf)
  colnames(teamperf) <- paste(data$resultSets[[1]]$headers)
  
  #Formatting date text to how the NBA JSON links prefer it
  teamperf$GAME_DATE <- paste(
    substr(teamperf$GAME_DATE,6,7),
    "%2F",
    substr(teamperf$GAME_DATE,9,10),
    "%2F",
    substr(teamperf$GAME_DATE,1,4),
    sep = ""
  )
  return(teamperf)
}

reg.data.T <- getTEAMdata(season = '2014-15',season.type = 'Regular+Season')
playoff.data.T <- getTEAMdata(season = '2014-15',season.type = 'Playoffs')

#####

# reg.dates <- apply(X = matrix(paste("http://stats.nba.com/stats/videoStatus?LeagueID=00&gameDate=", matrix(unique(reg.data.T$GAME_DATE)), sep="")),MARGIN = 1, FUN = fromJSON, nullValue = NA)

reg.dates <- list()

for(i in 1:length(unique(reg.data.P$GAME_DATE))){
Sys.sleep(time = 5)
print(i)
reg.dates[[i]] <- fromJSON(matrix(paste("http://stats.nba.com/stats/videoStatus?LeagueID=00&gameDate=", matrix(unique(reg.data.T$GAME_DATE)), sep=""))[i],nullValue = NA)
}

for(i in 1:length(reg.dates))
{
  reg.dates[[i]] <- t(matrix(unlist(reg.dates[[i]]$resultSets[[1]]$rowSet), nrow=14))
}

reg.dates <- do.call("rbind", reg.dates)

reg.dates <- data.frame(reg.dates)

reg.dates$X2 <- gsub("/", "%2F", reg.dates$X2)

#####

playoff.dates <- list()

for(i in 1:length(unique(playoff.data.P$GAME_DATE))){
  Sys.sleep(time = 5)
  print(i)
  playoff.dates[[i]] <- fromJSON(matrix(paste("http://stats.nba.com/stats/videoStatus?LeagueID=00&gameDate=", matrix(unique(playoff.data.T$GAME_DATE)), sep=""))[i],nullValue = NA)
}

for(i in 1:length(playoff.dates))
{
  playoff.dates[[i]] <- t(matrix(unlist(playoff.dates[[i]]$resultSets[[1]]$rowSet), nrow=14))
}

playoff.dates <- do.call("rbind", playoff.dates)

playoff.dates <- data.frame(playoff.dates)

playoff.dates$X2 <- gsub("/", "%2F", playoff.dates$X2)

#####

dates <- rbind(reg.dates, playoff.dates)

#####

query <- function(x){
  gamedate <- str_sub(x,-14)
  player.id <- str_sub(x,1,(length(x)-16))
  url = paste("http://stats.nba.com/stats/playerdashptpass?DateFrom=",gamedate,"&DateTo=",gamedate,"&GameSegment=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PerMode=PerGame&Period=0&PlayerID=",player.id,"&Season=",season,"&SeasonSegment=&SeasonType=",season.type,"&TeamID=0&VsConference=&VsDivision=",sep="")
  input <- RJSONIO::fromJSON(url, nullValue = NA)
  made <- input$resultSets[[1]]$rowSet
  rec <- input$resultSets[[2]]$rowSet
  if(length(made) > 0 & length(rec) > 0){
    c <- do.call("rbind",list(data.frame(cbind(t(matrix(unlist(made),nrow=21)), gamedate, season.type, season)),data.frame(cbind(t(matrix(unlist(rec),nrow=21)), gamedate, season.type, season))))
    colnames(c) <- c(input$resultSets[[1]]$headers,'gamedate', 'season.type', 'season')
    return(c)
  } 
  else if(length(made) > 0 & length(rec) == 0){
    a <- data.frame(cbind(t(matrix(unlist(made),nrow=21)), gamedate, season.type, season))
    colnames(a) <- c(input$resultSets[[1]]$headers,'gamedate', 'season.type', 'season')
    return(a)
  }
  else if(length(rec) > 0 & length(made) == 0){
    b <- data.frame(cbind(t(matrix(unlist(rec),nrow=21)), gamedate, season.type, season))
    colnames(b) <- c(input$resultSets[[2]]$headers,'gamedate', 'season.type', 'season')
    return(b)}
  else if(length(made) == 0 & length(rec) == 0){
   return(NULL)
  }
}

passing_data <- list()

x <- reg.data.P$PLAYER_ID_GAME_DATE

for(i in 1:length(x)){
  if(is.error(query(x[i]))==T){
    repeat{
      passing_data[[i]] <- query(x[[i]])
      if(nrow(passing_data[[i]])>1)
    break }
  } else
  passing_data[[i]] <- query(x[i])
  print(i)
  print(head(passing_data[[i]]))
}

