#install.packages(c("RJSONIO", "stringr", "reshape2", "igraph", "stringi", "plyr", "psych"))

#load in libraries
library(RJSONIO)
library(stringr)
library(reshape2)
library(igraph)
library(stringi)
library(plyr)
library(psych)

##PLEASE ALLOW FOR FULL DAY TO RUN, TAKES QUITE A BIT OF TIME TO GET EVERYTHING##

#Creates 6 files
#playoff.data.T = Team-level playoff data
#reg.data.T = Team-level regular season data
#playoff.data.P = Player-level playoff data
#reg.data.P = Player-level regular season data
#reg.data.PASS = Process-level (e.g. passing) data for regular season
#playoff.data.PASS = Process-level (e.g. passing) data for playoffs

#Function to get individual player data 
#Takes input season ("2013-14", "2014-15", "2015-16", "2016-17")
#Takes input season.type ("Playoffs", "Regular+Season")
#Approximately 600,000 lines across all 4 season (I believe)
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

  #This date format with "%2F" for the / is how the NBA website writes dates
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
  #Creating NBA Efficiency variable, simple addition and subtraction of other variables see http://www.nba.com/statistics/efficiency.html for more information
  nbadata$NBAeff = (nbadata$PTS + nbadata$REB + nbadata$AST + nbadata$STL + nbadata$BLK) - ((nbadata$FGA - nbadata$FGM) + (nbadata$FTA - nbadata$FTM) + nbadata$TOV)
  
  
  #
  
  #Creating an index of team-ids and dates that that will be used to attain more data
  x = matrix(unique(paste(nbadata$TEAM_ID, nbadata$GAME_DATE, sep="")))
  
  #
  

  
  #
  #Function to rank players by their Efficiency on a game by game basis for each team
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
  
  #Applying efficency ranking function to index previously created
  nbadata = do.call("rbind", apply(X = matrix(x),MARGIN = 1,FUN = nbaeffrank))
  
  #
  
  #Creating another index of player ids and dates to apply future functions to
  nbadata$PLAYER_ID_GAME_DATE = paste(nbadata$PLAYER_ID, nbadata$GAME_DATE, sep="")
  
  #

  #Xtra gets the "advanced" statistics the NBA collects that isn't in the "Traditional" player dashboard
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
  
  #Creating dataframe of "Xtra" statistics and applying it to previous player id and gamedate index
  nba.xtra = do.call("rbind", apply(X = matrix(x), MARGIN = 1, FUN = xtra))
  #Creating player id game date index that we will use to merge these two dataframes together
  nba.xtra$PLAYER_ID_GAME_DATE = paste(nba.xtra$PLAYER_ID, nba.xtra$GAME_DATE, sep="")
  
  #  

  #Merging "Xtra" with "Traditional" statistics
  nbadata = merge(x = nbadata, y = nba.xtra, by = "PLAYER_ID_GAME_DATE")
  
  #
  
  #Removing superfluous columns
  nbadata = nbadata[,-c(35,36,37,38,44,60,61,64,92)]
  
  #
  
  #Cleaning columns names
  colnames(nbadata) = gsub(".x","",colnames(nbadata))
  
  #
  
  #Setting PIE value to numeric
  nbadata$PIE = as.numeric(as.character(nbadata$PIE))
  
  #
  
  #Creating season type variable from season type function input
  nbadata$SEASON_TYPE = season.type
  
  #Returning final data frame
  return(nbadata)
}

#Running this function for each season and creating single dataframe
playoff.data.P = rbind(getPLAYERdata(season = "2013-14",season.type = "Playoffs"),
getPLAYERdata(season = "2014-15",season.type = "Playoffs"),
getPLAYERdata(season = "2015-16",season.type = "Playoffs"))

#Running this function for each season and creating single dataframe
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
                     getTEAMdata(season = "2015-16",season.type = "Playoffs"),
                     getTEAMdata(season = "2016-17", season.type = "Playoffs"))

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
getPASSdata(season.id = "42015", season = "2015-16", season.type = "Playoffs"),
getPASSdata(season.id = "42016", season = "2016-17", season.type = "Playoffs"))

###

reg.data.PASS = rbind(getPASSdata(season.id = "22013", season = "2013-14", season.type = "Regular+Season"),
                          getPASSdata(season.id = "22014", season = "2014-15", season.type = "Regular+Season"),
                          getPASSdata(season.id = "22015", season = "2015-16", season.type = "Regular+Season"),
                          getPASSdata(season.id = "22016", season = "2016-17", season.type = "Regular+Season"))

###

colnames(playoff.data.PASS) = c("PLAYER_ID","PLAYER_NAME_LAST_FIRST","TEAM_NAME","TEAM_ID","TEAM_ABBREVIATION","MADE_REC","G","PASS_PLAYER","PASS_TEAMMATE_PLAYER_ID","FREQUENCY","PASS","AST","FGM","FGA","FG_PCT","FG2M","FG2A","FG2_PCT","FG3M","FG3A","FG3_PCT","gamedate", "season.type", "season", "TEAM_ID_GAME_DATE")
colnames(reg.data.PASS) = c("PLAYER_ID","PLAYER_NAME_LAST_FIRST","TEAM_NAME","TEAM_ID","TEAM_ABBREVIATION","MADE_REC","G","PASS_PLAYER","PASS_TEAMMATE_PLAYER_ID","FREQUENCY","PASS","AST","FGM","FGA","FG_PCT","FG2M","FG2A","FG2_PCT","FG3M","FG3A","FG3_PCT","gamedate", "season.type", "season", "TEAM_ID_GAME_DATE")

###

###Calculate network stuff below
#Code below is not complete, just a copy over from previous version

###

nbanetPO = function(x)
{
  gamedate = str_sub(x,-14)
  teamid = str_sub(x,1,(length(x)-16))
  teamsubset = passperfPO[which(passperfPO$TEAM_ID ==  paste(teamid) & passperfPO$gamedate == paste(gamedate)),]
  teamsubset$PLAYER_NAME_LAST_FIRST = gsub(teamsubset$PLAYER_NAME_LAST_FIRST, pattern = "-",replacement = "")
  teamsubset$PASS_PLAYER = gsub(teamsubset$PASS_PLAYER,pattern = "-",replacement = "")
  points = nbadata[which(nbadata$TEAM_ID == paste(teamid) & nbadata$GAME_DATE == paste(gamedate)),]
  points = cbind(as.character(points$PLAYER_NAME), points$PTS)
  points[,1] = gsub(points[,1],pattern = "-",replacement = "")
  networksubset = cbind(teamsubset$PLAYER_NAME_LAST_FIRST, as.character(teamsubset$PASS_PLAYER), as.character(teamsubset$MADE_REC), teamsubset$PASS)
  splits1 <- str_split_fixed(networksubset[,1], ", ", 2)
  splits2 <- str_split_fixed(networksubset[,2], ", ", 2)
  networksubset[,1]=paste(splits1[,2],splits1[,1],sep = " ")
  networksubset[,2]=paste(splits2[,2],splits2[,1],sep = " ")
  networksubset = networksubset[!duplicated(networksubset[,c(1,2,3)]),]
  
  networksubset = gsub(" Nene" ,"Nene", networksubset)
  
  net <- graph.data.frame(d=networksubset, vertices=points[,1], directed=T) 
  
  E(net)$width = (as.numeric(networksubset[,4])/1.5)
  
  #this scalar constant (40) makes it look more visually appealling 
  V(net)$size = as.numeric(points[,2])*1.25
  
  l = layout_in_circle(net)
  return(net)
  #return(plot.igraph(net, layout = l, edge.arrow.size = 0.20, vertex.label.cex =1.2))
}

#Function to return degree, between, closeness, and eigenvalue centrality measures for every graph object nbanet makes. Also returns density as well.
netoutputPO = function(x)
{
  y = cbind(centr_degree(nbanetPO(x))$centralization,
            centr_betw(nbanetPO(x))$centralization,
            centr_clo(nbanetPO(x))$centralization,
            centr_eigen(nbanetPO(x))$centralization,
            edge_density(nbanetPO(x)))
  y = data.frame(y)
  colnames(y)[1] = paste("DegCentr")
  colnames(y)[2] = paste("BetwCentr")
  colnames(y)[3] = paste("CloCentr")
  colnames(y)[4] = paste("EigCentr")
  colnames(y)[5] = paste("Density")
  return(y)
}

########################

network.dataPO =  do.call("rbind",apply(matrix(x),1,netoutputPO))

########################

#Binding the columns with teamperf
teamperfPO = cbind(teamperfPO, network.dataPO)

########################

benPO = aggregate(teamperfPO[, 30:34], list(teamperfPO$TEAM_NAME), median)
colnames(benPO)[1] = paste("TEAM_NAME")

teamperfPO = merge(teamperfPO, benPO, by = "TEAM_NAME")

########################

########################

teamperfPO$DegCentr.diff = teamperfPO$DegCentr.x - teamperfPO$DegCentr.y

teamperfPO$BetwCentr.diff = teamperfPO$BetwCentr.x - teamperfPO$BetwCentr.y

teamperfPO$CloCentr.diff = teamperfPO$CloCentr.x - teamperfPO$CloCentr.y

teamperfPO$EigCentr.diff = teamperfPO$EigCentr.x - teamperfPO$EigCentr.y

teamperfPO$Density.diff = teamperfPO$Density.x - teamperfPO$Density.y

########################

nbadata$z_NBAeff <- ave(nbadata$NBAeff, nbadata$PLAYER_NAME, FUN=scale)

nbadata$z_PIE <- ave(nbadata$PIE, nbadata$PLAYER_NAME, FUN=scale)

########################










































