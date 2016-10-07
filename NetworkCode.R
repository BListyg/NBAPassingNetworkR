#load in libraries
library(RJSONIO)
library(stringr)
library(reshape2)
library(igraph)
library(stringi)

########## NBA *PLAYOFF* PASSING & PERFORMANCE DATA ##########

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

#Filtering out players with ZERO minutes player
REALNBADATA = nbadataRS[which(nbadataRS$MIN > 0),]

#Creating index to apply made/rec functions 
x2 = x2 = as.matrix(paste(REALNBADATA$PLAYER_ID,REALNBADATA$GAME_DATE,sep=""))

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
#passperf = rbind(outputREC, outputMADE)
#gameperf = read.csv("nbadataRS.csv",header=T)

####################################
# .csvs for all of these are on my drive #
#########################################

#Renamed regular season performance to 'gameperf' when .csv was read in

#Using TENDEX, NBA's Efficiency, ESPNs rating, PIE, and linear weights PER to quantify the 'best' player

gameperf$tendex = ((gameperf$PTS + gameperf$REB + gameperf$AST + gameperf$STL + gameperf$BLK) - ((gameperf$FGA - gameperf$FGM) - (0.5*(gameperf$FTA - gameperf$FTM)) - gameperf$TOV - gameperf$PF)) / gameperf$MIN

gameperf$NBAeff = (gameperf$PTS + gameperf$REB + gameperf$AST + gameperf$STL + gameperf$BLK) - ((gameperf$FGA - gameperf$FGM) + (gameperf$FTA - gameperf$FTM) + gameperf$TOV)

gameperf$ESPN = gameperf$PTS + gameperf$REB + (1.4*gameperf$AST) + gameperf$STL + (1.4*gameperf$BLK) - (0.7*gameperf$TOV) + gameperf$FGM + (.5*gameperf$FGM) - (0.8*(gameperf$FGA - gameperf$FGM)) + (0.25*gameperf$FTM) - (0.8*(gameperf$FTA - gameperf$FTM))

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
  
  net <- graph.data.frame(d=networksubset, vertices=points[,1], directed=T) 
  E(net)$width = (as.numeric(networksubset[,4])/1.5)
  
  #this scalar constant (40) makes it look more visually appealling 
  V(net)$size = as.numeric(points[,2])*1.25
  
  l = layout_in_circle(net)
  plot.igraph(net, layout = l, edge.arrow.size = 0.20, vertex.label.cex =1.2) #, edge.label=networksubset[,4])
  #Need to return networks statistics here
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

#############################################################################

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

teamperf$GAME_DATE = paste(
  substr(teamperf$GAME_DATE,6,7),
  "%2F",
  substr(teamperf$GAME_DATE,9,10),
  "%2F",
  substr(teamperf$GAME_DATE,1,4),
  sep = ""
)

teamperf = merge(teamperf,teamid,by = "TEAM_ABBREVIATION")

bp = data.frame(t(apply(x1,1,bestplayer)))

teamperf$X1 = paste(teamperf$teamidnumber, teamperf$GAME_DATE, sep="")

teamperf = merge(teamperf, bp, by = "X1")

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


























