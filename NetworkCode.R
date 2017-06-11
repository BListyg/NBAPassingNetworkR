#install.packages(c("RJSONIO", "stringr", "reshape2", "igraph", "stringi", "plyr", "psych"))

#load in libraries
library(RJSONIO)
library(stringr)
library(reshape2)
library(igraph)
library(stringi)
library(plyr)
library(psych)

#Function to get individual player performance data 
#Takes input season ("2013-14", "2014-15", "2015-16", "2016-17")
#Takes input season.type ("Playoffs", "Regular+Season")
getPLAYERdata = function(season,season.type){
  #This link goes to a table of all individual player statistics by for every game during the playoffs
  json.url = paste("http://stats.nba.com/stats/leaguegamelog?Counter=1000&DateFrom=&DateTo=&Direction=DESC&LeagueID=00&PlayerOrTeam=P&Season=",season,"&SeasonType=",season.type,"&Sorter=DATE",sep="")
  #Retrieving data, returns a list
  dat = RJSONIO::fromJSON(json.url, nullValue = NA)
  #Turning list into workable data frame
  nbadata = matrix(unlist(dat$resultSets[[1]]$rowSet), ncol=length(dat$resultSets[[1]]$headers), byrow = TRUE)
  #Converting matrix to dataframe
  nbadata = data.frame(nbadata)
  #Setting column names
  colnames(nbadata) = paste(dat$resultSets[[1]]$headers)

  nbadata$GAME_DATE = paste(
    substr(nbadata$GAME_DATE,6,7),
    "%2F",
    substr(nbadata$GAME_DATE,9,10),
    "%2F",
    substr(nbadata$GAME_DATE,1,4),
    sep = ""
  )

  
  #Converting variables to appropriate types
  nbadata$PLAYER_ID = as.character(nbadata$PLAYER_ID)
  nbadata$GAME_DATE = as.character(nbadata$GAME_DATE)
  nbadata$MIN = as.numeric(as.character(nbadata$MIN))
  nbadata$FGM = as.numeric(as.character(nbadata$FGM))
  nbadata$FGA = as.numeric(as.character(nbadata$FGA))
  nbadata$FG_PCT = as.numeric(as.character(nbadata$FG_PCT)) 
  nbadata$FG3M = as.numeric(as.character(nbadata$FG3M)) 
  nbadata$FG3A = as.numeric(as.character(nbadata$FG3A)) 
  nbadata$FG3_PCT = as.numeric(as.character(nbadata$FG3_PCT))
  nbadata$FTM= as.numeric(as.character(nbadata$FTM))
  nbadata$FTA = as.numeric(as.character(nbadata$FTA))
  nbadata$FT_PCT = as.numeric(as.character(nbadata$FT_PCT))
  nbadata$OREB = as.numeric(as.character(nbadata$OREB))
  nbadata$DREB = as.numeric(as.character(nbadata$DREB))
  nbadata$REB = as.numeric(as.character(nbadata$REB))
  nbadata$AST = as.numeric(as.character(nbadata$AST))
  nbadata$STL = as.numeric(as.character(nbadata$STL))
  nbadata$BLK = as.numeric(as.character(nbadata$BLK))
  nbadata$TOV = as.numeric(as.character(nbadata$TOV))
  nbadata$PF = as.numeric(as.character(nbadata$PF))
  nbadata$PTS = as.numeric(as.character(nbadata$PTS))
  nbadata$PLUS_MINUS = as.numeric(as.character(nbadata$PLUS_MINUS))
  nbadata$NBAeff = (nbadata$PTS + nbadata$REB + nbadata$AST + nbadata$STL + nbadata$BLK) - ((nbadata$FGA - nbadata$FGM) + (nbadata$FTA - nbadata$FTM) + nbadata$TOV)
  
  
  #
  
  x = matrix(unique(paste(nbadata$TEAM_ID, nbadata$GAME_DATE, sep="")))
  
  #
  

  
  #
  nbaeffrank = function(x){
    gamedate = str_sub(x,-14)
    teamid = str_sub(x,1,(length(x)-16))
    teamsubset = nbadata[which(nbadata$TEAM_ID ==  paste(teamid) & nbadata$GAME_DATE == paste(gamedate)),]
    teamsubset$NBAeff_RANK <- NA
    order.scores<-order(-teamsubset$NBAeff,teamsubset$PLAYER_NAME)
    teamsubset$NBAeff_RANK[order.scores] <- 1:nrow(teamsubset)
    return(teamsubset)
  }
  #
  
  nbadata = do.call("rbind", apply(X = matrix(x),MARGIN = 1,FUN = nbaeffrank))
  
  #
  
  nbadata$PLAYER_ID_GAME_DATE = paste(nbadata$PLAYER_ID, nbadata$GAME_DATE, sep="")
  
  #

  
  xtra = function(x){
    gamedate = str_sub(x,-14)
    teamid = str_sub(x,1,(length(x)-16))
    url = paste("http://stats.nba.com/stats/leaguedashplayerstats?College=&Conference=&Country=&DateFrom=",
                gamedate,
                "&DateTo=",
                gamedate,
                "&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&LastNGames=0&LeagueID=00&Location=&MeasureType=Advanced&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=",season,"&SeasonSegment=&SeasonType=",season.type,"&ShotClockRange=&StarterBench=&TeamID=",
                teamid,
                "&VsConference=&VsDivision=&Weight=",sep="")
    dat = RJSONIO::fromJSON(url,nullValue = NA)
    colnames = paste(dat$resultSets[[1]]$headers)
    dat = matrix(unlist(dat$resultSets[[1]]$rowSet), ncol=length(dat$resultSets[[1]]$headers), byrow = TRUE)
    dat = data.frame(dat)
    colnames(dat) = colnames
    dat$GAME_DATE = gamedate
    return(dat)
  }
  
  #
  
  nba.xtra = do.call("rbind", apply(X = matrix(x), MARGIN = 1, FUN = xtra))
  nba.xtra$PLAYER_ID_GAME_DATE = paste(nba.xtra$PLAYER_ID, nba.xtra$GAME_DATE, sep="")
  
  #  

  nbadata = merge(x = nbadata, y = nba.xtra, by = "PLAYER_ID_GAME_DATE")
  
  #
  
  nbadata = nbadata[,-c(35,36,37,38,44,60,61,64,92)]
  
  #
  
  colnames(nbadata) = gsub(".x","",colnames(nbadata))
  
  #
  
  nbadata$PIE = as.numeric(as.character(nbadata$PIE))
  
  #
  
  nbadata$SEASON_TYPE = season.type
  
  return(nbadata)
}

playoff.data.P = rbind(getPLAYERdata(season = "2013-14",season.type = "Playoffs"),
getPLAYERdata(season = "2014-15",season.type = "Playoffs"),
getPLAYERdata(season = "2015-16",season.type = "Playoffs"))

reg.data.P = rbind(getPLAYERdata(season = "2013-14",season.type = "Regular+Season"),
                 getPLAYERdata(season = "2014-15",season.type = "Regular+Season"),
                 getPLAYERdata(season = "2015-16",season.type = "Regular+Season"),
                 getPLAYERdata(season = "2016-17",season.type = "Regular+Season"))

reg.data.P$TEAM_ID_GAME_DATE = paste(reg.data.P$TEAM_ID, reg.data.P$GAME_DATE, sep="")

playoff.data.P$TEAM_ID_GAME_DATE = paste(playoff.data.P$TEAM_ID, playoff.data.P$GAME_DATE, sep="")

#Obtaining data from URL
#Team level performance data
getTEAMdata = function(season, season.type){
url = paste("http://stats.nba.com/stats/leaguegamelog?Counter=1000&DateFrom=&DateTo=&Direction=DESC&LeagueID=00&PlayerOrTeam=T&Season=",season,"&SeasonType=",season.type,"&Sorter=PTS",sep="")
data = RJSONIO::fromJSON(url, nullValue = NA)
teamperf = matrix(unlist(data$resultSets[[1]]$rowSet),ncol=29,byrow = TRUE)
teamperf = data.frame(teamperf)
colnames(teamperf) = paste(data$resultSets[[1]]$headers)

#Formatting date text to how the NBA JSON links prefer it
teamperf$GAME_DATE = paste(
  substr(teamperf$GAME_DATE,6,7),
  "%2F",
  substr(teamperf$GAME_DATE,9,10),
  "%2F",
  substr(teamperf$GAME_DATE,1,4),
  sep = ""
)
return(teamperf)
}

playoff.data.T = rbind(getTEAMdata(season = "2013-14",season.type = "Playoffs"),
                     getTEAMdata(season = "2014-15",season.type = "Playoffs"),
                     getTEAMdata(season = "2015-16",season.type = "Playoffs"))

reg.data.T = rbind(getTEAMdata(season = "2013-14",season.type = "Regular+Season"),
                 getTEAMdata(season = "2014-15",season.type = "Regular+Season"),
                 getTEAMdata(season = "2015-16",season.type = "Regular+Season"),
                 getTEAMdata(season = "2016-17",season.type = "Regular+Season"))

###

reg.dates = apply(X = matrix(paste("http://stats.nba.com/stats/videoStatus?LeagueID=00&gameDate=", matrix(unique(reg.data.T$GAME_DATE)), sep="")),MARGIN = 1, FUN = RJSONIO::fromJSON)

for(i in 1:length(reg.dates))
{
  reg.dates[[i]] = t(matrix(unlist(reg.dates[[i]]$resultSets[[1]]$rowSet), nrow=14))
}

reg.dates = do.call("rbind", reg.dates)

reg.dates = data.frame(reg.dates)

reg.dates$X2 = gsub("/", "%2F", reg.dates$X2)

###

playoff.dates = apply(X = matrix(paste("http://stats.nba.com/stats/videoStatus?LeagueID=00&gameDate=", matrix(unique(playoff.data.T$GAME_DATE)), sep="")),MARGIN = 1, FUN = RJSONIO::fromJSON)

for(i in 1:length(playoff.dates))
{
  playoff.dates[[i]] = t(matrix(unlist(playoff.dates[[i]]$resultSets[[1]]$rowSet), nrow=14))
}

playoff.dates = do.call("rbind", playoff.dates)

playoff.dates = data.frame(playoff.dates)

playoff.dates$X2 = gsub("/", "%2F", playoff.dates$X2)

###

dates = rbind(reg.dates, playoff.dates)

###
#Obtains passing data
getPASSdata = function(season, season.type, season.id){
  query = function(x){
    gamedate = str_sub(x,-14)
    player.id = str_sub(x,1,(length(x)-16))
    url = paste("http://stats.nba.com/stats/playerdashptpass?DateFrom=",gamedate,"&DateTo=",gamedate,"&GameSegment=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PerMode=PerGame&Period=0&PlayerID=",player.id,"&Season=",season,"&SeasonSegment=&SeasonType=",season.type,"&TeamID=0&VsConference=&VsDivision=",sep="")
    input = RJSONIO::fromJSON(url, nullValue = NA)
    made = input$resultSets[[1]]$rowSet
    rec = input$resultSets[[2]]$rowSet
    if(length(made) == 0){
      return(NULL)
    }
    else if(length(rec) == 0){
      return(NULL)
    }
    else if(length(made) > 0 | length(rec) > 0){
      return(do.call("rbind",list(data.frame(cbind(t(matrix(unlist(made),nrow=21)), gamedate, season.type, season)),data.frame(cbind(t(matrix(unlist(rec),nrow=21)), gamedate, season.type, season)))))
    }}
  
  subset = rbind(reg.data.P, playoff.data.P)
  subset = subset[which(subset$SEASON_ID == paste(season.id) & subset$SEASON_TYPE == paste(season.type) & subset$GAME_ID %in% dates$X1[which(dates$X14 == 1)]),]
  subset = matrix(unique(subset$PLAYER_ID_GAME_DATE))
  return(do.call("rbind", apply(X = subset, MARGIN = 1, FUN = query)))
}

###

playoff.data.PASS = rbind(getPASSdata(season.id = "42013", season = "2013-14", season.type = "Playoffs"),
getPASSdata(season.id = "42014", season = "2014-15", season.type = "Playoffs"),
getPASSdata(season.id = "42015", season = "2015-16", season.type = "Playoffs"))

###

reg.data.PASS = rbind(getPASSdata(season.id = "22013", season = "2013-14", season.type = "Regular+Season"),
                          getPASSdata(season.id = "22014", season = "2014-15", season.type = "Regular+Season"),
                          getPASSdata(season.id = "22015", season = "2015-16", season.type = "Regular+Season"))

###

colnames(playoff.data.PASS) = c("PLAYER_ID","PLAYER_NAME_LAST_FIRST","TEAM_NAME","TEAM_ID","TEAM_ABBREVIATION","MADE_REC","G","PASS_PLAYER","PASS_TEAMMATE_PLAYER_ID","FREQUENCY","PASS","AST","FGM","FGA","FG_PCT","FG2M","FG2A","FG2_PCT","FG3M","FG3A","FG3_PCT","gamedate")

###

###Calculate network stuff here

###
