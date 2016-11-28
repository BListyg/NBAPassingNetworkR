#load in libraries
library(RJSONIO)
library(stringr)
library(reshape2)
library(igraph)
library(stringi)
library(plyr)
library(psych)

########## NBA *PLAYOFF* PASSING & PERFORMANCE DATA ##########

##SCRAPING THE DATA FROM THE WEB##

#This link goes to a table of all individual player statistics by for every game during the playoffs
json.url = "http://stats.nba.com/stats/leaguegamelog?Counter=1000&Direction=DESC&LeagueID=00&PlayerOrTeam=P&Season=2015-16&SeasonType=Playoffs&Sorter=PTS"
#Retrieving data, returns a list
dat = RJSONIO::fromJSON(json.url, nullValue = NA)
#Turning list into workable data frame
nbadata = matrix(unlist(dat$resultSets[[1]]$rowSet), ncol=length(dat$resultSets[[1]]$headers), byrow = TRUE)
#Converting matrix to dataframe
nbadata = data.frame(nbadata)
#Setting column names
colnames(nbadata) = paste(dat$resultSets[[1]]$headers)

#Retrieving NBA Team ID #s, used later for data retrieval
teamid.url = "https://raw.githubusercontent.com/nickb1080/nba/master/data/teams.json"
teamid = fromJSON(teamid.url,nullValue = NA)
teamid = t(matrix(unlist(teamid),ncol=30,nrow=5))[,1:2]
colnames(teamid)[2] = paste("TEAM_ABBREVIATION")
colnames(teamid)[1] = paste("teamidnumber")

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

#NBA JSON links have '%2F' as spaces in their dates
#Adding '%2F' in the spaces for data retrieval later
nbadata$GAME_DATE = paste(
  substr(nbadata$GAME_DATE,6,7),
  "%2F",
  substr(nbadata$GAME_DATE,9,10),
  "%2F",
  substr(nbadata$GAME_DATE,1,4),
  sep = ""
)

#Merging team id # with first data frame
nbadata = merge(nbadata,teamid,by = "TEAM_ABBREVIATION")

#Function to retrieve player numeric ids for players who 'MADE' passes during individual games
#First portion gets player numeric ids for players who made passes
#Nested function 'finally' gets actual data
madeplayers = function(x)
{
  gamedate = str_sub(x,-14)
  teamid = str_sub(x,1,(length(x)-16))
  url = paste("http://stats.nba.com/stats/teamdashptpass?DateFrom=",
              gamedate,
              "&DateTo=",
              gamedate,
              "&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PaceAdjust=N&PerMode=PerGame&Period=0&PlusMinus=N&Rank=N&Season=2015-16&SeasonSegment=&SeasonType=Playoffs&TeamID=",
              teamid,
              "&VsConference=&VsDivision=",sep="")
  dat = fromJSON(url,nullValue = NA)
  dat = t(matrix(unlist(dat$resultSets[[1]]$rowSet),ncol = length(dat$resultSets[[1]]$rowSet)))[,6]
  dat = as.matrix(paste("http://stats.nba.com/stats/playerdashptpass?DateFrom=",gamedate,"&DateTo=",gamedate,"&GameSegment=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PerMode=PerGame&Period=0&PlayerID=",dat,"&Season=2015-16&SeasonSegment=&SeasonType=Playoffs&TeamID=0&VsConference=&VsDivision=",sep=""))
  dat = apply(dat,1,fromJSON,nullValue = NA)
  player.num = matrix(c(1:length(dat)))
  finally = function(x)
  {
    dat = dat[[x]]$resultSets[[1]]$rowSet
    return(data.frame(t(matrix(unlist(dat),nrow=21)),stringsAsFactors = FALSE))
  }
  made = do.call("rbind",apply(X = player.num, FUN = finally, MARGIN = 1))
  made = cbind(made,gamedate)
  return(made)
}

#Function to retrieve passing data for players who 'RECIEVED' passes during individual games
#First portion gets player numeric ids for players who recieved passes
#Nested function 'finally' gets actual data
recplayers = function(x)
{
  gamedate = str_sub(x,-14)
  teamid = str_sub(x,1,(length(x)-16))
  url = paste("http://stats.nba.com/stats/teamdashptpass?DateFrom=",
              gamedate,
              "&DateTo=",
              gamedate,
              "&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PaceAdjust=N&PerMode=PerGame&Period=0&PlusMinus=N&Rank=N&Season=2015-16&SeasonSegment=&SeasonType=Playoffs&TeamID=",
              teamid,
              "&VsConference=&VsDivision=",sep="")
  dat = fromJSON(url,nullValue = NA)
  dat = t(matrix(unlist(dat$resultSets[[2]]$rowSet),ncol = length(dat$resultSets[[2]]$rowSet)))[,6]
  dat = as.matrix(paste("http://stats.nba.com/stats/playerdashptpass?DateFrom=",gamedate,"&DateTo=",gamedate,"&GameSegment=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PerMode=PerGame&Period=0&PlayerID=",dat,"&Season=2015-16&SeasonSegment=&SeasonType=Playoffs&TeamID=0&VsConference=&VsDivision=",sep=""))
  dat = apply(dat,1,fromJSON,nullValue = NA)
  player.num = matrix(c(1:length(dat)))
  #Function in a function, this actually 
  finally = function(x)
  {
    dat = dat[[x]]$resultSets[[2]]$rowSet
    return(data.frame(t(matrix(unlist(dat),nrow=21)),stringsAsFactors = FALSE))
  }
  rec = do.call("rbind",apply(X = player.num, FUN = finally, MARGIN = 1))
  rec = cbind(rec,gamedate)
  return(rec)
}

#Creating index of team ids and dates that recplayers and madeplayers will be applied to
x = unique(as.matrix(paste(nbadata$teamidnumber,nbadata$GAME_DATE,sep="")))

#Applying madeplayers to index and getting made playoff data
madePO = do.call("rbind",apply(x,1,madeplayers))

#Applying recplayers to index and getting playoff data
recPO = do.call("rbind",apply(x,1,recplayers))

########## NBA *REGULAR SEASON* PASSING & PERFORMANCE DATA ##########
#TAKES A LONG TIME
#Essentially a JSON link web crawler/scrapper on the NBA website

#Regular season performance data for each player for each game
json.url = "http://stats.nba.com/stats/leaguegamelog?Counter=1000&Direction=DESC&LeagueID=00&PlayerOrTeam=P&Season=2015-16&SeasonType=Regular+Season&Sorter=PTS"
dat = RJSONIO::fromJSON(json.url, nullValue = NA)
nbadataRS = matrix(unlist(dat$resultSets[[1]]$rowSet), ncol=length(dat$resultSets[[1]]$headers), byrow = TRUE)
nbadataRS = data.frame(nbadataRS)
colnames(nbadataRS) = paste(dat$resultSets[[1]]$headers)
rm(dat)

#Team numeric IDs 
teamid.url = "https://raw.githubusercontent.com/nickb1080/nba/master/data/teams.json"
teamid = fromJSON(teamid.url,nullValue = NA)
teamid = t(matrix(unlist(teamid),ncol=30,nrow=5))[,1:2]
colnames(teamid)[2] = paste("TEAM_ABBREVIATION")
colnames(teamid)[1] = paste("teamidnumber")

#Converting variable types
nbadataRS$PLAYER_ID = as.character(nbadataRS$PLAYER_ID)
nbadataRS$GAME_DATE = as.character(nbadataRS$GAME_DATE)
nbadataRS$MIN = as.numeric(as.character(nbadataRS$MIN))
nbadataRS$FGM = as.numeric(as.character(nbadataRS$FGM))
nbadataRS$FGA = as.numeric(as.character(nbadataRS$FGA))
nbadataRS$FG_PCT = as.numeric(as.character(nbadataRS$FG_PCT)) 
nbadataRS$FG3M = as.numeric(as.character(nbadataRS$FG3M)) 
nbadataRS$FG3A = as.numeric(as.character(nbadataRS$FG3A)) 
nbadataRS$FG3_PCT = as.numeric(as.character(nbadataRS$FG3_PCT))
nbadataRS$FTM= as.numeric(as.character(nbadataRS$FTM))
nbadataRS$FTA = as.numeric(as.character(nbadataRS$FTA))
nbadataRS$FT_PCT = as.numeric(as.character(nbadataRS$FT_PCT))
nbadataRS$OREB = as.numeric(as.character(nbadataRS$OREB))
nbadataRS$DREB = as.numeric(as.character(nbadataRS$DREB))
nbadataRS$REB = as.numeric(as.character(nbadataRS$REB))
nbadataRS$AST = as.numeric(as.character(nbadataRS$AST))
nbadataRS$STL = as.numeric(as.character(nbadataRS$STL))
nbadataRS$BLK = as.numeric(as.character(nbadataRS$BLK))
nbadataRS$TOV = as.numeric(as.character(nbadataRS$TOV))
nbadataRS$PF = as.numeric(as.character(nbadataRS$PF))
nbadataRS$PTS = as.numeric(as.character(nbadataRS$PTS))
nbadataRS$PLUS_MINUS = as.numeric(as.character(nbadataRS$PLUS_MINUS))

#Matching NBAs format for spaces
nbadataRS$GAME_DATE = paste(
  substr(nbadataRS$GAME_DATE,6,7),
  "%2F",
  substr(nbadataRS$GAME_DATE,9,10),
  "%2F",
  substr(nbadataRS$GAME_DATE,1,4),
  sep = ""
)

#Merging team ids with regular season data frame
nbadataRS = merge(nbadataRS,teamid,by = "TEAM_ABBREVIATION")

#Same function as above, data for players that made passes
madeplayers = function(x)
{
  gamedate = str_sub(x,-14)
  teamid = str_sub(x,1,(length(x)-16))
  url = paste("http://stats.nba.com/stats/teamdashptpass?DateFrom=",
              gamedate,
              "&DateTo=",
              gamedate,
              "&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PaceAdjust=N&PerMode=PerGame&Period=0&PlusMinus=N&Rank=N&Season=2015-16&SeasonSegment=&SeasonType=Regular+Season&TeamID=",
              teamid,
              "&VsConference=&VsDivision=",sep="")
  dat = fromJSON(url,nullValue = NA)
  dat = t(matrix(unlist(dat$resultSets[[1]]$rowSet),ncol = length(dat$resultSets[[1]]$rowSet)))[,6]
  dat = as.matrix(paste("http://stats.nba.com/stats/playerdashptpass?DateFrom=",gamedate,"&DateTo=",gamedate,"&GameSegment=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PerMode=PerGame&Period=0&PlayerID=",dat,"&Season=2015-16&SeasonSegment=&SeasonType=Regular+Season&TeamID=0&VsConference=&VsDivision=",sep=""))
  dat = apply(dat,1,fromJSON,nullValue = NA)
  player.num = matrix(c(1:length(dat)))
  finally = function(x)
  {
    dat = dat[which(!is.null(dat[[x]]$resultSets[[1]]$rowSet)),]
    return(data.frame(t(matrix(unlist(dat),nrow=21)),stringsAsFactors = FALSE))
  }
  made = do.call("rbind",apply(X = player.num, FUN = finally, MARGIN = 1))
  made = cbind(made,gamedate)
  return(made)
}

#Same function as above, data for players that recieved passes
recplayers = function(x)
{
  gamedate = str_sub(x,-14)
  teamid = str_sub(x,1,(length(x)-16))
  url = paste("http://stats.nba.com/stats/teamdashptpass?DateFrom=",
              gamedate,
              "&DateTo=",
              gamedate,
              "&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PaceAdjust=N&PerMode=PerGame&Period=0&PlusMinus=N&Rank=N&Season=2015-16&SeasonSegment=&SeasonType=Regular+Season&TeamID=",
              teamid,
              "&VsConference=&VsDivision=",sep="")
  dat = fromJSON(url,nullValue = NA)
  dat = t(matrix(unlist(dat$resultSets[[2]]$rowSet),ncol = length(dat$resultSets[[2]]$rowSet)))[,6]
  dat = as.matrix(paste("http://stats.nba.com/stats/playerdashptpass?DateFrom=",gamedate,"&DateTo=",gamedate,"&GameSegment=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PerMode=PerGame&Period=0&PlayerID=",dat,"&Season=2015-16&SeasonSegment=&SeasonType=Regular+Season&TeamID=0&VsConference=&VsDivision=",sep=""))
  dat = apply(dat,1,fromJSON,nullValue = NA)
  player.num = matrix(c(1:length(dat)))
  finally = function(x)
  {
    dat = dat[[x]]$resultSets[[2]]$rowSet
    return(data.frame(t(matrix(unlist(dat),nrow=21)),stringsAsFactors = FALSE))
  }
  rec = do.call("rbind",apply(X = player.num, FUN = finally, MARGIN = 1))
  rec = cbind(rec,gamedate)
  return(rec)
}

#Filtering out players with ZERO minutes played
REALNBADATA = nbadataRS[which(nbadataRS$MIN > 0),]

#Creating index to apply made/rec functions 
x2 = as.matrix(paste(REALNBADATA$PLAYER_ID,REALNBADATA$GAME_DATE,sep=""))

#Functions to get passing data
playerdataMADE = function(x)
{
  gamedate = str_sub(x,-14)
  playerid = str_sub(x,1,(length(x)-16))
  url = paste("http://stats.nba.com/stats/playerdashptpass?DateFrom=",
              gamedate,
              "&DateTo=",
              gamedate,
              "&GameSegment=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PerMode=PerGame&Period=0&PlayerID=",
              playerid,
              "&Season=2015-16&SeasonSegment=&SeasonType=Regular+Season&TeamID=0&VsConference=&VsDivision=",sep = "")
  data = fromJSON(url, nullValue = NA)
  return(data.frame(t(matrix(unlist(data$resultSets[[1]]$rowSet),nrow = 21))))
}

playerdataREC = function(x)
{
  gamedate = str_sub(x,-14)
  playerid = str_sub(x,1,(length(x)-16))
  url = paste("http://stats.nba.com/stats/playerdashptpass?DateFrom=",
              gamedate,
              "&DateTo=",
              gamedate,
              "&GameSegment=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PerMode=PerGame&Period=0&PlayerID=",
              playerid,
              "&Season=2015-16&SeasonSegment=&SeasonType=Regular+Season&TeamID=0&VsConference=&VsDivision=",sep = "")
  data = fromJSON(url, nullValue = NA)
  return(data.frame(t(matrix(unlist(data$resultSets[[2]]$rowSet),nrow = 21))))
}

#This next part is tricky

#Creating empty vector to 'put' our results in
#Setting length to x2 index
input.list <- vector("list",length(x2))

#Some NBA players have no made or recieved pass data, which returns errors with an apply function
#Try allows you to skip errors and continuing getting the data
#to reiterate, takes a long time
for(i in 1:length(x2))
{
    input.list[[i]] = try(playerdataMADE(x2[i]))
}
list.condition <- sapply(input.list, function(x) class(x)=="data.frame")
output.list  <- input.list[list.condition]
outputMADE = do.call("rbind",output.list)

#Some NBA players have no made or recieved pass data, which returns errors with an apply function
#Try allows you to skip errors and continuing getting the data
#to reiterate, takes a long time
input.list <- vector("list",length(x2))
for(i in 1:length(x2))
{
    input.list[[i]] = try(playerdataREC(x2[i]))
}
list.condition <- sapply(input.list, function(x) class(x)=="data.frame")
output.list  <- input.list[list.condition]
outputREC = do.call("rbind",output.list)

#rbind these two, should be ~300000 lines of passing data, now you have all passing interactions between players for each game for the whole 2015-2016 regular season
passperf = rbind(outputREC, outputMADE)
gameperf = nbadataRS

####################################
# .csvs for all of these are on my drive #
#########################################

#Renamed regular season performance to 'gameperf' when .csv was read in

#Using TENDEX, NBA's Efficiency, ESPNs rating, PIE, and linear weights PER to quantify the 'best' player


## CALCULATING PLAYER PERFORMANCE METRICS ##

gameperf$tendex = ((gameperf$PTS + gameperf$REB + gameperf$AST + gameperf$STL + gameperf$BLK) - ((gameperf$FGA - gameperf$FGM) - (0.5*(gameperf$FTA - gameperf$FTM)) - gameperf$TOV - gameperf$PF)) / gameperf$MIN

gameperf$NBAeff = (gameperf$PTS + gameperf$REB + gameperf$AST + gameperf$STL + gameperf$BLK) - ((gameperf$FGA - gameperf$FGM) + (gameperf$FTA - gameperf$FTM) + gameperf$TOV)

gameperf$ESPN = gameperf$PTS + gameperf$REB + (1.4*gameperf$AST) + gameperf$STL + (1.4*gameperf$BLK) - (0.7*gameperf$TOV) + gameperf$FGM + (.5*gameperf$FGM) - (0.8*(gameperf$FGA - gameperf$FGM)) + (0.25*gameperf$FTM) - (0.8*(gameperf$FTA - gameperf$FTM))

#PIE is a percentage of a players contributions relative to the team's performance. Thus, we must sum the team's performance for the game, apply the weights, and divide it by the player's performance.
pie = function(x)
{
  gamedate = str_sub(x,-14)
  teamid = str_sub(x,1,(length(x)-16))
  teamtotal = gameperf[which(gameperf$teamidnumber == paste(teamid) & gameperf$GAME_DATE == paste(gamedate)),]
  teamtotal = data.frame(teamtotal[,c(11:29)])
  teamtotal$numerator = teamtotal$PTS + teamtotal$FGM + teamtotal$FTM - teamtotal$FGA - teamtotal$FTA  + teamtotal$DREB + (0.5*teamtotal$OREB) + teamtotal$AST + teamtotal$STL + (0.5*teamtotal$BLK) - teamtotal$PF - teamtotal$TOV
  totalMIN = sum(teamtotal$MIN)
  totalFGM = sum(teamtotal$FGM)
  totalFGA = sum(teamtotal$FGA)
  totalFG3M = sum(teamtotal$FG3M)
  totalFG3A = sum(teamtotal$FG3A)
  totalFTM = sum(teamtotal$FTM)
  totalFTA = sum(teamtotal$FTA)
  totalOREB = sum(teamtotal$OREB)
  totalDREB = sum(teamtotal$OREB)
  totalAST = sum(teamtotal$AST)
  totalSTL = sum(teamtotal$STL)
  totalBLK = sum(teamtotal$BLK)
  totalTOV = sum(teamtotal$TOV)
  totalPF = sum(teamtotal$PF)
  totalPTS = sum(teamtotal$PTS)
  return(teamtotal$numerator / (totalPTS + totalFGM + totalFTM - totalFGA - totalFTA + totalDREB + (0.5*totalOREB) + totalAST + totalSTL + (0.5*totalBLK) - totalPF - totalTOV))
}

gameperf$PIE = unlist(apply(X = x1,MARGIN = 1,FUN = pie))

gameperf$lwPER = (((gameperf$FGM*(85.910)) + (gameperf$STL*(53.897)) + (gameperf$FG3M*(51.757)) + (gameperf$FTM*(46.845)) + (gameperf$BLK*(39.190)) + (gameperf$OREB*(39.190)) + (gameperf$AST*(34.677)) + (gameperf$DREB*(14.707)) + (gameperf$PF*(17.714)) + ((gameperf$FTA - gameperf$FTM)*(20.091)) + ((gameperf$FGA - gameperf$FGM)*(39.190)) - (gameperf$TOV*(53.897))) * (1/gameperf$MIN))

is.na(gameperf$tendex) <- sapply(gameperf$tendex, is.infinite)
is.na(gameperf$lwPER) <- sapply(gameperf$lwPER, is.infinite)

##############################################################################

#Function for getting nets, used to calculate network statistics

nbanet = function(x)
{
  gamedate = str_sub(x,-14)
  teamid = str_sub(x,1,(length(x)-16))
  teamsubset = passperf[which(passperf$X4 ==  paste(teamid) & passperf$gamedate == paste(gamedate)),]
  teamsubset$X2 = as.character(teamsubset$X2)
  teamsubset$X2 = gsub(teamsubset$X2,pattern = "-",replacement = "")
  teamsubset$X8 = gsub(teamsubset$X8,pattern = "-",replacement = "")
  points = gameperf[which(gameperf$teamidnumber == paste(teamid) & gameperf$GAME_DATE == paste(gamedate)),]
  points = cbind(as.character(points$PLAYER_NAME), points$PTS)
  points[,1] = gsub(points[,1],pattern = "-",replacement = "")
  networksubset = cbind(teamsubset$X2, as.character(teamsubset$X8), as.character(teamsubset$X6), teamsubset$X11)
  splits1 <- str_split_fixed(networksubset[,1], ", ", 2)
  splits2 <- str_split_fixed(networksubset[,2], ", ", 2)
  networksubset[,1]=paste(splits1[,2],splits1[,1],sep = " ")
  networksubset[,2]=paste(splits2[,2],splits2[,1],sep = " ")
  networksubset = networksubset[!duplicated(networksubset[,c(1,2,4)]),]
  
  networksubset = gsub(" Nene" ,"Nene", networksubset)
  
  net <- graph.data.frame(d=networksubset, vertices=points[,1], directed=T) 

  E(net)$width = (as.numeric(networksubset[,4])/1.5)
  
  #this scalar constant (40) makes it look more visually appealling 
  V(net)$size = as.numeric(points[,2])*1.25
  
  l = layout_in_circle(net)
  return(net)
  return(plot.igraph(net, layout = l, edge.arrow.size = 0.20, vertex.label.cex =1.2))
}

###############################################################################

#Function for best player based on different metrics

bestplayer = function(x)
{
gamedate = str_sub(x,-14)
teamid = str_sub(x,1,(length(x)-16))
teamtotal = gameperf[which(gameperf$teamidnumber == paste(teamid) & gameperf$GAME_DATE == paste(gamedate)),]
mat = matrix(cbind(x,max(teamtotal$tendex), as.character(teamtotal$PLAYER_NAME[which.max(teamtotal$tendex)]),  
                      max(teamtotal$NBAeff), as.character(teamtotal$PLAYER_NAME[which.max(teamtotal$NBAeff)]), 
                      max(teamtotal$PIE), as.character(teamtotal$PLAYER_NAME[which.max(teamtotal$PIE)]), 
                      max(teamtotal$ESPN), as.character(teamtotal$PLAYER_NAME[which.max(teamtotal$ESPN)]),
                      max(teamtotal$lwPER), as.character(teamtotal$PLAYER_NAME[which.max(teamtotal$lwPER)])))

return(t(mat))
}

##########################TEAM PEFORMANCE DATA FOR ALL THEIR GAMES################################

#Obtaining data from URL
url = "http://stats.nba.com/stats/leaguegamelog?Counter=1000&DateFrom=&DateTo=&Direction=DESC&LeagueID=00&PlayerOrTeam=T&Season=2015-16&SeasonType=Regular+Season&Sorter=PTS"
data = fromJSON(url, nullValue = NA)
teamperf = matrix(unlist(data$resultSets[[1]]$rowSet),ncol=29,byrow = TRUE)
teamperf = data.frame(teamperf)
colnames(teamperf) = paste(data$resultSets[[1]]$headers)

#Retrieving NBA Team ID #s, used later for data retrieval
teamid.url = "https://raw.githubusercontent.com/nickb1080/nba/master/data/teams.json"
teamid = fromJSON(teamid.url,nullValue = NA)
teamid = t(matrix(unlist(teamid),ncol=30,nrow=5))[,1:2]
colnames(teamid)[2] = paste("TEAM_ABBREVIATION")
colnames(teamid)[1] = paste("teamidnumber")

#Formatting date text to how the NBA JSON links prefer it
teamperf$GAME_DATE = paste(
  substr(teamperf$GAME_DATE,6,7),
  "%2F",
  substr(teamperf$GAME_DATE,9,10),
  "%2F",
  substr(teamperf$GAME_DATE,1,4),
  sep = ""
)

x1 = unique(paste(teamperf$TEAM_ID,teamperf$GAME_DATE,sep=""))

#Merging TEAM PEFORMANCE data with TEAM NUMERIC IDs
teamperf = merge(teamperf,teamid,by = "TEAM_ABBREVIATION")

#Dataframe of best players
bp = data.frame(t(apply(matrix(unique(x1)),1,bestplayer)))

teamperf$X1 = paste(teamperf$teamidnumber, teamperf$GAME_DATE, sep="")

#Merging best players with their teams
teamperf = merge(teamperf, bp, by = "X1")

#Naming columns
teamperf$X1 = NULL
colnames(teamperf)[31] = paste("tendex")
colnames(teamperf)[32] = paste("tendex_player")
colnames(teamperf)[33] = paste("NBAEFF")
colnames(teamperf)[34] = paste("NBAEFF_player")
colnames(teamperf)[35] = paste("PIE")
colnames(teamperf)[36] = paste("PIE_player")
colnames(teamperf)[37] = paste("ESPN")
colnames(teamperf)[38] = paste("ESPN_player")
colnames(teamperf)[39] = paste("lwPER")
colnames(teamperf)[40] = paste("lwPER_player")

#Setting variable types
teamperf$tendex_player = as.character(teamperf$tendex_player)
teamperf$NBAEFF_player = as.character(teamperf$NBAEFF_player)
teamperf$PIE_player = as.character(teamperf$PIE_player)
teamperf$ESPN_player = as.character(teamperf$ESPN_player)
teamperf$lwPER_player = as.character(teamperf$lwPER_player)

#Function for calculating how often a given player is called "the best" by these 5 metrics
metricfreq = function(teamname)
{
c= rbind(data.frame(sort(table(teamperf[which(teamperf$TEAM_NAME == paste(teamname)),32]),decreasing = TRUE),metric=paste("tendex")),
          data.frame(sort(table(teamperf[which(teamperf$TEAM_NAME == paste(teamname)),34]),decreasing = TRUE),metric=paste("NBAEFF")),
          data.frame(sort(table(teamperf[which(teamperf$TEAM_NAME == paste(teamname)),36]),decreasing = TRUE),metric=paste("PIE")),
          data.frame(sort(table(teamperf[which(teamperf$TEAM_NAME == paste(teamname)),38]),decreasing = TRUE),metric=paste("ESPN")),
          data.frame(sort(table(teamperf[which(teamperf$TEAM_NAME == paste(teamname)),40]),decreasing = TRUE),metric=paste("lwPER")))

#return(ggplot(c, aes(c$Var1,c$Freq,colour = c$metric)) + geom_point(size = 5) + xlab("Player") + ylab("Metric Frequency") + labs(colour = "Metric", title = paste(teamname)))
c = data.frame(rbind(c[which.max(c$metric == 'ESPN'),],c[which.max(c$metric == 'tendex'),],c[which.max(c$metric == 'lwPER'),],c[which.max(c$metric == 'PIE'),],c[which.max(c$metric == 'NBAEFF'),]))
c$team = paste(teamname)
return(c)
}

teamnames = melt(unique(teamperf$TEAM_NAME))

mostbest = do.call("rbind", apply(X = teamnames, MARGIN = 1, FUN = metricfreq))
mb = do.call("rbind",strsplit(paste(mostbest$Var1, mostbest$metric, sep = " "),split = " "))

#Z-scores for each metric by player
gameperf$z_lwPER <- ave(gameperf$lwPER, gameperf$PLAYER_NAME, FUN=scale)
gameperf$z_tendex <- ave(gameperf$tendex, gameperf$PLAYER_NAME, FUN=scale)
gameperf$z_NBAeff <- ave(gameperf$NBAeff, gameperf$PLAYER_NAME, FUN=scale)
gameperf$z_ESPN <- ave(gameperf$ESPN, gameperf$PLAYER_NAME, FUN=scale)
gameperf$z_PIE <- ave(gameperf$PIE, gameperf$PLAYER_NAME, FUN=scale)

#Getting dates that these best players did > 1 SD in metric performance
bestplayerdata = function(playername, metric, number)
{
  c = data.frame(as.character(gameperf$GAME_DATE[which(gameperf$PLAYER_NAME == paste(playername) & metric >= number)]),playername)
  colnames(c)[1] = "GAME_DATE"
  colnames(c)[2] = "PLAYER_NAME"
  return(c)
}

#Getting dates where these best players < -1 SD in metric performance
worstplayerdata = function(playername, metric, number)
{
  c = data.frame(as.character(gameperf$GAME_DATE[which(gameperf$PLAYER_NAME == paste(playername) & metric <= number)]),playername)
  colnames(c)[1] = "GAME_DATE"
  colnames(c)[2] = "PLAYER_NAME"
  return(c)
}

mostbest$Var1 = as.character(mostbest$Var1)
mostbest$metric = as.character(mostbest$metric)
mostbest = mostbest[order(mostbest$metric),]

#Adding team abbreivations to be able to merge best and worst player dates 
teamabb = data.frame(gameperf$PLAYER_NAME, gameperf$TEAM_ABBREVIATION, paste(gameperf$teamidnumber,gameperf$GAME_DATE,sep=""),gameperf$GAME_DATE)
colnames(teamabb)[1] = paste("PLAYER_NAME")
colnames(teamabb)[2] = paste("TEAM_ABBREVIATION")
colnames(teamabb)[3] = paste("teamiddate")
colnames(teamabb)[4] = paste("GAME_DATE")

#Eliminating what we added earlier
teamperf[,c(31:40)] = NULL

teamperf$teamiddate = paste(teamperf$TEAM_ID,teamperf$GAME_DATE,sep="")

#Adding player names to team performance dates when the best did good and the best did bad
lwper = data.frame(do.call("rbind",apply(as.matrix(mostbest$Var1[which(mostbest$metric=='lwPER')]),MARGIN = 1,FUN = bestplayerdata,metric=gameperf$z_lwPER,number=1)))
lwper = data.frame(unique(join(lwper, teamabb, type = "inner")))
lwper$GAME_DATE = lwper$teamiddate
lwper$teamiddate = NULL
colnames(lwper)[1] = paste("teamiddate")
teamperf = join(teamperf,lwper,type="left")
colnames(teamperf)[32] = paste("lwper_playerGOOD")

##

lwperBAD = data.frame(do.call("rbind",apply(as.matrix(mostbest$Var1[which(mostbest$metric=='lwPER')]),MARGIN = 1,FUN = worstplayerdata,metric=gameperf$z_lwPER,number=-1)))
lwperBAD = data.frame(unique(join(lwperBAD, teamabb, type = "inner")))
lwperBAD$GAME_DATE = lwperBAD$teamiddate
lwperBAD$teamiddate = NULL
colnames(lwperBAD)[1] = paste("teamiddate")
teamperf = join(teamperf,lwperBAD,type="left")
colnames(teamperf)[33] = paste("lwper_playerBAD")

##

tendex = data.frame(do.call("rbind",apply(as.matrix(mostbest$Var1[which(mostbest$metric=='tendex')]),MARGIN = 1,FUN = bestplayerdata,metric=gameperf$z_tendex,number=1)))
tendex = data.frame(unique(join(tendex, teamabb, type = "inner")))
tendex$GAME_DATE = tendex$teamiddate
#Removing Mario Chalmers from Miami
tendex = tendex[-293,]
tendex$teamiddate = NULL
colnames(tendex)[1] = paste("teamiddate")
teamperf = join(teamperf,tendex,type="left")
colnames(teamperf)[34] = paste("tendex_playerGOOD")

##

tendexBAD = data.frame(do.call("rbind",apply(as.matrix(mostbest$Var1[which(mostbest$metric=='tendex')]),MARGIN = 1,FUN = worstplayerdata,metric=gameperf$z_tendex,number=-1)))
tendexBAD = data.frame(unique(join(tendexBAD, teamabb, type = "inner")))
tendexBAD$GAME_DATE = tendexBAD$teamiddate
#Removing Mario Chalmers from Miami
tendexBAD = tendexBAD[-c(293,294),]
tendexBAD$teamiddate = NULL
colnames(tendexBAD)[1] = paste("teamiddate")
teamperf = join(teamperf,tendexBAD,type="left")
colnames(teamperf)[35] = paste("tendex_playerBAD")

##

nbaeff = data.frame(do.call("rbind",apply(as.matrix(mostbest$Var1[which(mostbest$metric=='NBAEFF')]),MARGIN = 1,FUN = bestplayerdata,metric=gameperf$z_NBAeff,number=1)))
nbaeff = data.frame(unique(join(nbaeff, teamabb, type = "inner")))
nbaeff$GAME_DATE = nbaeff$teamiddate
nbaeff$teamiddate = NULL
colnames(nbaeff)[1] = paste("teamiddate")
teamperf = join(teamperf,nbaeff,type="left")
colnames(teamperf)[36] = paste("nbaeff_playerGOOD")

##

nbaeffBAD = data.frame(do.call("rbind",apply(as.matrix(mostbest$Var1[which(mostbest$metric=='NBAEFF')]),MARGIN = 1,FUN = worstplayerdata,metric=gameperf$z_NBAeff,number=-1)))
nbaeffBAD = data.frame(unique(join(nbaeffBAD, teamabb, type = "inner")))
nbaeffBAD$GAME_DATE = nbaeffBAD$teamiddate
nbaeffBAD$teamiddate = NULL
colnames(nbaeffBAD)[1] = paste("teamiddate")
teamperf = join(teamperf,nbaeffBAD,type="left")
colnames(teamperf)[37] = paste("nbaeff_playerBAD")

##

espn = data.frame(do.call("rbind",apply(as.matrix(mostbest$Var1[which(mostbest$metric=='ESPN')]),MARGIN = 1,FUN = bestplayerdata,metric=gameperf$z_ESPN,number=1)))
espn = data.frame(unique(join(espn, teamabb, type = "inner")))
espn$GAME_DATE = espn$teamiddate
espn$teamiddate = NULL
colnames(espn)[1] = paste("teamiddate")
teamperf = join(teamperf,espn,type="left")
colnames(teamperf)[38] = paste("espn_playerGOOD")

##

espnBAD = data.frame(do.call("rbind",apply(as.matrix(mostbest$Var1[which(mostbest$metric=='ESPN')]),MARGIN = 1,FUN = worstplayerdata,metric=gameperf$z_ESPN,number=-1)))
espnBAD = data.frame(unique(join(espnBAD, teamabb, type = "inner")))
espnBAD$GAME_DATE = espnBAD$teamiddate
espnBAD$teamiddate = NULL
colnames(espnBAD)[1] = paste("teamiddate")
teamperf = join(teamperf,espnBAD,type="left")
colnames(teamperf)[39] = paste("espn_playerBAD")

##

pie = data.frame(do.call("rbind",apply(as.matrix(mostbest$Var1[which(mostbest$metric=='PIE')]),MARGIN = 1,FUN = bestplayerdata,metric=gameperf$z_PIE,number=1)))
pie = data.frame(unique(join(pie, teamabb, type = "inner")))
pie$GAME_DATE = pie$teamiddate
pie$teamiddate = NULL
colnames(pie)[1] = paste("teamiddate")
teamperf = join(teamperf,pie,type="left")
colnames(teamperf)[40] = paste("pie_playerGOOD")

##

pieBAD = data.frame(do.call("rbind",apply(as.matrix(mostbest$Var1[which(mostbest$metric=='PIE')]),MARGIN = 1,FUN = worstplayerdata,metric=gameperf$z_PIE,number=-1)))
pieBAD = data.frame(unique(join(pieBAD, teamabb, type = "inner")))
pieBAD$GAME_DATE = pieBAD$teamiddate
pieBAD$teamiddate = NULL
colnames(pieBAD)[1] = paste("teamiddate")
teamperf = join(teamperf,pieBAD,type="left")
colnames(teamperf)[41] = paste("pie_playerBAD")

##

######################################################

x1 = matrix(x1)

######################################################

#Function to return degree, between, closeness, and eigenvalue centrality measures for every graph object nbanet makes. Also returns density as well.
netoutput = function(x)
{
y = cbind(centr_degree(nbanet(x))$centralization,
centr_betw(nbanet(x))$centralization,
centr_clo(nbanet(x))$centralization,
centr_eigen(nbanet(x))$centralization,
edge_density(nbanet(x)))
y = data.frame(y)
colnames(y)[1] = paste("DegCentr")
colnames(y)[2] = paste("BetwCentr")
colnames(y)[3] = paste("CloCentr")
colnames(y)[4] = paste("EigCentr")
colnames(y)[5] = paste("Density")
return(y)
}

#Applying this function to every team
#Some players have points (would be a node), but no passing data which disrupts function. 
#Only occurs 6 times across 30 teams and 82 games. 
#Those are marked as NA.

network.data = 

rbind(
    
do.call("rbind",apply(matrix(x1[1:82]),1,netoutput)),

do.call("rbind",apply(matrix(x1[83:164]),1,netoutput)),

do.call("rbind",apply(matrix(x1[165:246]),1,netoutput)),

do.call("rbind",apply(matrix(x1[247:328]),1,netoutput)),

do.call("rbind",apply(matrix(x1[329:410]),1,netoutput)),

do.call("rbind",apply(matrix(x1[411:492]),1,netoutput)),

do.call("rbind",apply(matrix(x1[493:574]),1,netoutput)),

do.call("rbind",apply(matrix(x1[575:656]),1,netoutput)),

do.call("rbind",apply(matrix(x1[657:738]),1,netoutput)),

rbind(do.call("rbind",apply(matrix(x1[739:759]),1,netoutput)),NA,
do.call("rbind",apply(matrix(x1[761:765]),1,netoutput)),NA,
do.call("rbind",apply(matrix(x1[767:820]),1,netoutput))),

do.call("rbind",apply(matrix(x1[821:902]),1,netoutput)),

do.call("rbind",apply(matrix(x1[903:984]),1,netoutput)),

do.call("rbind",apply(matrix(x1[985:1066]),1,netoutput)),

do.call("rbind",apply(matrix(x1[1067:1148]),1,netoutput)),

do.call("rbind",apply(matrix(x1[1149:1230]),1,netoutput)),

do.call("rbind",apply(matrix(x1[1231:1312]),1,netoutput)),

do.call("rbind",apply(matrix(x1[1313:1394]),1,netoutput)),

do.call("rbind",apply(matrix(x1[1395:1476]),1,netoutput)),

do.call("rbind",apply(matrix(x1[1477:1558]),1,netoutput)),

do.call("rbind",apply(matrix(x1[1559:1640]),1,netoutput)),

do.call("rbind",apply(matrix(x1[1641:1722]),1,netoutput)),

do.call("rbind",apply(matrix(x1[1723:1804]),1,netoutput)),

do.call("rbind",apply(matrix(x1[1805:1886]),1,netoutput)),

do.call("rbind",apply(matrix(x1[1887:1968]),1,netoutput)),

do.call("rbind",apply(matrix(x1[1969:2050]),1,netoutput)),

do.call("rbind",apply(matrix(x1[2051:2132]),1,netoutput)),

rbind(do.call("rbind",apply(matrix(x1[2133:2163]),1,netoutput)),NA,NA,NA,NA,
do.call("rbind",apply(matrix(x1[2168:2214]),1,netoutput))),

do.call("rbind",apply(matrix(x1[2215:2296]),1,netoutput)),

do.call("rbind",apply(matrix(x1[2297:2378]),1,netoutput)),

do.call("rbind",apply(matrix(x1[2379:2460]),1,netoutput))
)

#Binding the columns with teamperf
teamperf = cbind(teamperf, network.data)

########################################
#Fixing 10 day contract and Orlando Johnson issues
#With missing data in teamperf dataframe
########################################

x = x1[760]
gamedate = str_sub(x,-14)
teamid = str_sub(x,1,(length(x)-16))
teamsubset = passperf[which(passperf$X4 ==  paste(teamid) & passperf$gamedate == paste(gamedate)),]
teamsubset$X2 = as.character(teamsubset$X2)
teamsubset$X2 = gsub(teamsubset$X2,pattern = "-",replacement = "")
teamsubset$X8 = gsub(teamsubset$X8,pattern = "-",replacement = "")
points = gameperf[which(gameperf$teamidnumber == paste(teamid) & gameperf$GAME_DATE == paste(gamedate)),]
points = cbind(as.character(points$PLAYER_NAME), points$PTS)
points[,1] = gsub(points[,1],pattern = "-",replacement = "")
networksubset = cbind(teamsubset$X2, as.character(teamsubset$X8), as.character(teamsubset$X6), teamsubset$X11)
splits1 <- str_split_fixed(networksubset[,1], ", ", 2)
splits2 <- str_split_fixed(networksubset[,2], ", ", 2)
networksubset[,1]=paste(splits1[,2],splits1[,1],sep = " ")
networksubset[,2]=paste(splits2[,2],splits2[,1],sep = " ")
networksubset = networksubset[!duplicated(networksubset[,c(1,2,4)]),]
networksubset = networksubset[!networksubset[,2] == "Orlando Johnson", ]

net <- graph.data.frame(d=networksubset, vertices=points[,1], directed=T)

teamperf[760,c(42:46)] =t(rbind(centr_degree(net)$centralization,
centr_betw(net)$centralization,
centr_clo(net)$centralization,
centr_eigen(net)$centralization,
edge_density(net)))

#

x = x1[766]
gamedate = str_sub(x,-14)
teamid = str_sub(x,1,(length(x)-16))
teamsubset = passperf[which(passperf$X4 ==  paste(teamid) & passperf$gamedate == paste(gamedate)),]
teamsubset$X2 = as.character(teamsubset$X2)
teamsubset$X2 = gsub(teamsubset$X2,pattern = "-",replacement = "")
teamsubset$X8 = gsub(teamsubset$X8,pattern = "-",replacement = "")
points = gameperf[which(gameperf$teamidnumber == paste(teamid) & gameperf$GAME_DATE == paste(gamedate)),]
points = cbind(as.character(points$PLAYER_NAME), points$PTS)
points[,1] = gsub(points[,1],pattern = "-",replacement = "")
networksubset = cbind(teamsubset$X2, as.character(teamsubset$X8), as.character(teamsubset$X6), teamsubset$X11)
splits1 <- str_split_fixed(networksubset[,1], ", ", 2)
splits2 <- str_split_fixed(networksubset[,2], ", ", 2)
networksubset[,1]=paste(splits1[,2],splits1[,1],sep = " ")
networksubset[,2]=paste(splits2[,2],splits2[,1],sep = " ")
networksubset = networksubset[!duplicated(networksubset[,c(1,2,4)]),]
networksubset = networksubset[!networksubset[,2] == "Orlando Johnson", ]

points = rbind(points, addon)

net <- graph.data.frame(d=networksubset, vertices=points[,1], directed=T)

teamperf[766,c(42:46)] = t(rbind(centr_degree(net)$centralization,
        centr_betw(net)$centralization,
        centr_clo(net)$centralization,
        centr_eigen(net)$centralization,
        edge_density(net)))

#

x = x1[2164]
gamedate = str_sub(x,-14)
teamid = str_sub(x,1,(length(x)-16))
teamsubset = passperf[which(passperf$X4 ==  paste(teamid) & passperf$gamedate == paste(gamedate)),]
teamsubset$X2 = as.character(teamsubset$X2)
teamsubset$X2 = gsub(teamsubset$X2,pattern = "-",replacement = "")
teamsubset$X8 = gsub(teamsubset$X8,pattern = "-",replacement = "")
points = gameperf[which(gameperf$teamidnumber == paste(teamid) & gameperf$GAME_DATE == paste(gamedate)),]
points = cbind(as.character(points$PLAYER_NAME), points$PTS)
points[,1] = gsub(points[,1],pattern = "-",replacement = "")
networksubset = cbind(teamsubset$X2, as.character(teamsubset$X8), as.character(teamsubset$X6), teamsubset$X11)
splits1 <- str_split_fixed(networksubset[,1], ", ", 2)
splits2 <- str_split_fixed(networksubset[,2], ", ", 2)
networksubset[,1]=paste(splits1[,2],splits1[,1],sep = " ")
networksubset[,2]=paste(splits2[,2],splits2[,1],sep = " ")
networksubset = networksubset[!duplicated(networksubset[,c(1,2,4)]),]
networksubset = gsub(pattern = "Orlando Johnson", replacement = "Alex Stepheson", networksubset)

net <- graph.data.frame(d=networksubset, vertices=points[,1], directed=T)

teamperf[2164,c(42:46)] = t(rbind(centr_degree(net)$centralization,
        centr_betw(net)$centralization,
        centr_clo(net)$centralization,
        centr_eigen(net)$centralization,
        edge_density(net)))

#

x = x1[2165]
gamedate = str_sub(x,-14)
teamid = str_sub(x,1,(length(x)-16))
teamsubset = passperf[which(passperf$X4 ==  paste(teamid) & passperf$gamedate == paste(gamedate)),]
teamsubset$X2 = as.character(teamsubset$X2)
teamsubset$X2 = gsub(teamsubset$X2,pattern = "-",replacement = "")
teamsubset$X8 = gsub(teamsubset$X8,pattern = "-",replacement = "")
points = gameperf[which(gameperf$teamidnumber == paste(teamid) & gameperf$GAME_DATE == paste(gamedate)),]
points = cbind(as.character(points$PLAYER_NAME), points$PTS)
points[,1] = gsub(points[,1],pattern = "-",replacement = "")
networksubset = cbind(teamsubset$X2, as.character(teamsubset$X8), as.character(teamsubset$X6), teamsubset$X11)
splits1 <- str_split_fixed(networksubset[,1], ", ", 2)
splits2 <- str_split_fixed(networksubset[,2], ", ", 2)
networksubset[,1]=paste(splits1[,2],splits1[,1],sep = " ")
networksubset[,2]=paste(splits2[,2],splits2[,1],sep = " ")
networksubset = networksubset[!duplicated(networksubset[,c(1,2,4)]),]
networksubset = gsub(pattern = "Orlando Johnson", replacement = "Alex Stepheson", networksubset)

net <- graph.data.frame(d=networksubset, vertices=points[,1], directed=T)

teamperf[2165,c(42:46)] = t(rbind(centr_degree(net)$centralization,
        centr_betw(net)$centralization,
        centr_clo(net)$centralization,
        centr_eigen(net)$centralization,
        edge_density(net)))

#

x = x1[2166]
gamedate = str_sub(x,-14)
teamid = str_sub(x,1,(length(x)-16))
teamsubset = passperf[which(passperf$X4 ==  paste(teamid) & passperf$gamedate == paste(gamedate)),]
teamsubset$X2 = as.character(teamsubset$X2)
teamsubset$X2 = gsub(teamsubset$X2,pattern = "-",replacement = "")
teamsubset$X8 = gsub(teamsubset$X8,pattern = "-",replacement = "")
points = gameperf[which(gameperf$teamidnumber == paste(teamid) & gameperf$GAME_DATE == paste(gamedate)),]
points = cbind(as.character(points$PLAYER_NAME), points$PTS)
points[,1] = gsub(points[,1],pattern = "-",replacement = "")
networksubset = cbind(teamsubset$X2, as.character(teamsubset$X8), as.character(teamsubset$X6), teamsubset$X11)
splits1 <- str_split_fixed(networksubset[,1], ", ", 2)
splits2 <- str_split_fixed(networksubset[,2], ", ", 2)
networksubset[,1]=paste(splits1[,2],splits1[,1],sep = " ")
networksubset[,2]=paste(splits2[,2],splits2[,1],sep = " ")
networksubset = networksubset[!duplicated(networksubset[,c(1,2,4)]),]
networksubset = gsub(pattern = "Orlando Johnson", replacement = "Alex Stepheson", networksubset)

net <- graph.data.frame(d=networksubset, vertices=points[,1], directed=T)

teamperf[2166,c(42:46)] = t(rbind(centr_degree(net)$centralization,
        centr_betw(net)$centralization,
        centr_clo(net)$centralization,
        centr_eigen(net)$centralization,
        edge_density(net)))

#

x = x1[2167]
gamedate = str_sub(x,-14)
teamid = str_sub(x,1,(length(x)-16))
teamsubset = passperf[which(passperf$X4 ==  paste(teamid) & passperf$gamedate == paste(gamedate)),]
teamsubset$X2 = as.character(teamsubset$X2)
teamsubset$X2 = gsub(teamsubset$X2,pattern = "-",replacement = "")
teamsubset$X8 = gsub(teamsubset$X8,pattern = "-",replacement = "")
points = gameperf[which(gameperf$teamidnumber == paste(teamid) & gameperf$GAME_DATE == paste(gamedate)),]
points = cbind(as.character(points$PLAYER_NAME), points$PTS)
points[,1] = gsub(points[,1],pattern = "-",replacement = "")
networksubset = cbind(teamsubset$X2, as.character(teamsubset$X8), as.character(teamsubset$X6), teamsubset$X11)
splits1 <- str_split_fixed(networksubset[,1], ", ", 2)
splits2 <- str_split_fixed(networksubset[,2], ", ", 2)
networksubset[,1]=paste(splits1[,2],splits1[,1],sep = " ")
networksubset[,2]=paste(splits2[,2],splits2[,1],sep = " ")
networksubset = networksubset[!duplicated(networksubset[,c(1,2,4)]),]
networksubset = gsub(pattern = "Orlando Johnson", replacement = "Alex Stepheson", networksubset)

net <- graph.data.frame(d=networksubset, vertices=points[,1], directed=T)

teamperf[2167,c(42:46)] = t(rbind(centr_degree(net)$centralization,
        centr_betw(net)$centralization,
        centr_clo(net)$centralization,
        centr_eigen(net)$centralization,
        edge_density(net)))

##################################

ben = aggregate(teamperf[, 42:46], list(teamperf$TEAM_NAME), median)
colnames(ben)[1] = paste("TEAM_NAME")

teamperf = merge(teamperf, ben, by = "TEAM_NAME")

##################################

teamperf$DegCentr.diff = teamperf$DegCentr.x - teamperf$DegCentr.y

teamperf$BetwCentr.diff = teamperf$BetwCentr.x - teamperf$BetwCentr.y

teamperf$CloCentr.diff = teamperf$CloCentr.x - teamperf$CloCentr.y

teamperf$EigCentr.diff = teamperf$EigCentr.x - teamperf$EigCentr.y

teamperf$Density.diff = teamperf$Density.x - teamperf$Density.y

###################################

results = matrix(nrow=5,ncol=2)

results[1,1] = paste("DegCentr")
results[2,1] = paste("BetwCentr")
results[3,1] = paste("CloCentr")
results[4,1] = paste("EigCentr")
results[5,1] = paste("Density")

results[,2] = rbind(
biserial(teamperf$DegCentr.diff[which(!is.na(teamperf$lwper_playerBAD) | !is.na(teamperf$tendex_playerBAD) | !is.na(teamperf$nbaeff_playerBAD) | !is.na(teamperf$espn_playerBAD) | !is.na(teamperf$nbaeff_playerBAD))],
         teamperf$WL[which(!is.na(teamperf$lwper_playerBAD) | !is.na(teamperf$tendex_playerBAD) | !is.na(teamperf$nbaeff_playerBAD) | !is.na(teamperf$espn_playerBAD) | !is.na(teamperf$nbaeff_playerBAD))]),

biserial(teamperf$BetwCentr.diff[which(!is.na(teamperf$lwper_playerBAD) | !is.na(teamperf$tendex_playerBAD) | !is.na(teamperf$nbaeff_playerBAD) | !is.na(teamperf$espn_playerBAD) | !is.na(teamperf$nbaeff_playerBAD))],
         teamperf$WL[which(!is.na(teamperf$lwper_playerBAD) | !is.na(teamperf$tendex_playerBAD) | !is.na(teamperf$nbaeff_playerBAD) | !is.na(teamperf$espn_playerBAD) | !is.na(teamperf$nbaeff_playerBAD))]),

biserial(teamperf$CloCentr.diff[which(!is.na(teamperf$lwper_playerBAD) | !is.na(teamperf$tendex_playerBAD) | !is.na(teamperf$nbaeff_playerBAD) | !is.na(teamperf$espn_playerBAD) | !is.na(teamperf$nbaeff_playerBAD))],
         teamperf$WL[which(!is.na(teamperf$lwper_playerBAD) | !is.na(teamperf$tendex_playerBAD) | !is.na(teamperf$nbaeff_playerBAD) | !is.na(teamperf$espn_playerBAD) | !is.na(teamperf$nbaeff_playerBAD))]),

biserial(teamperf$EigCentr.diff[which(!is.na(teamperf$lwper_playerBAD) | !is.na(teamperf$tendex_playerBAD) | !is.na(teamperf$nbaeff_playerBAD) | !is.na(teamperf$espn_playerBAD) | !is.na(teamperf$nbaeff_playerBAD))],
         teamperf$WL[which(!is.na(teamperf$lwper_playerBAD) | !is.na(teamperf$tendex_playerBAD) | !is.na(teamperf$nbaeff_playerBAD) | !is.na(teamperf$espn_playerBAD) | !is.na(teamperf$nbaeff_playerBAD))]),

biserial(teamperf$Density.diff[which(!is.na(teamperf$lwper_playerBAD) | !is.na(teamperf$tendex_playerBAD) | !is.na(teamperf$nbaeff_playerBAD) | !is.na(teamperf$espn_playerBAD) | !is.na(teamperf$nbaeff_playerBAD))],
         teamperf$WL[which(!is.na(teamperf$lwper_playerBAD) | !is.na(teamperf$tendex_playerBAD) | !is.na(teamperf$nbaeff_playerBAD) | !is.na(teamperf$espn_playerBAD) | !is.na(teamperf$nbaeff_playerBAD))]))

results = data.frame(results)






















