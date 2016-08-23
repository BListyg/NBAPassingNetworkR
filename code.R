# NBAPassingNetworkR
Code to visualize made/received passes in R from NBA games

library(RJSONIO)
library(rjson)
library(igraph)
library(reshape2)

##################

#1610612744 warr

#1610612739 cavs

#vertex.color = "indianred3"

#02,05,08,10,13,16,19

#https://github.com/nickb1080/nba/blob/master/data/teams.json

#utah 40-40
#golden state

##################

#the day of the game that you want passing networks for
#if the user-entered date didn't have a game played that day, the code doesn't work
#

#month?
x = "10"
#day?
y = "30"
#year?
z = "2015"
#"Regular+Season" or "Playoffs" (Make sure you get this right!)
season = "Regular+Season"
#TeamID (will make a list of all of them soon, see GitHub link above in the meantime)?
teamid = "1610612737"

##################

#team.url is a function to gather the # of players and player ids 
#for those that participated in the game on the date the user set

teamdata.url = 
  function(x,y,z,teamid,mr,season)
  {
    data = paste(
      "http://stats.nba.com/stats/teamdashptpass?DateFrom=",
      
      x,
      
      "%2F",
      
      y,
      
      "%2F",
      
      z,
      
      "&DateTo=",
      
      x,
      
      "%2F",
      
      y,
      
      "%2F",
      
      z,
      
      "&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PaceAdjust=N&PerMode=PerGame&Period=0&PlusMinus=N&Rank=N&Season=2015-16&SeasonSegment=&SeasonType=",
      season,
      "&TeamID=",
      teamid,
      "&VsConference=&VsDivision=",sep="")
    data = RJSONIO::fromJSON(data,nullValue='NA')
    cn = data[[3]][[mr]]$headers
    data = data[[3]][[mr]]$rowSet
    data = as.matrix(data,ncol=length(data))
    data = t(apply(X = data,MARGIN = 1,FUN = unlist))
    data = as.data.frame(data)
    colnames(data) = cn
    return(data)
  }

#####################

#dataframe for 'made' passes from user-entered date

data.made = teamdata.url(x=x,y=y,z=z,teamid=teamid, 1, season=season)

#dataframe for 'recieved' passes from user-entered date

data.rec = teamdata.url(x=x,y=y,z=z,teamid=teamid, 2, season=season)

#####################

#Player ID numbers for those who made / recieved passes in the game on the user-entered date

playerid.made = as.matrix(as.character((sort(data.made$PASS_TEAMMATE_PLAYER_ID))))
playerid.rec = as.matrix(as.character((sort(data.rec$PASS_TEAMMATE_PLAYER_ID))))

#####################

#getting all pass network data for user-entered date
#this is used for the edges in the network graph

playerdata.url = function(x,y,z,playerid,season)
{
  playermatrix = matrix(nrow = nrow(playerid),ncol = 18) 
  playermatrix[,1] = paste("http://stats.nba.com/stats/playerdashptpass?DateFrom=")
  playermatrix[,2] = x
  playermatrix[,3] = paste("%2F")
  playermatrix[,4] = y
  playermatrix[,5] = paste("%2F")
  playermatrix[,6] = z
  playermatrix[,7] = paste("&DateTo=")
  playermatrix[,8] = x
  playermatrix[,9] = paste("%2F")
  playermatrix[,10] = y
  playermatrix[,11] = paste("%2F")
  playermatrix[,12] = z
  playermatrix[,13] = paste("&DateTo=")
  playermatrix[,14] = paste("&GameSegment=&LastNGames=0&LeagueID=00&Location=&Month=0&OpponentTeamID=0&Outcome=&PerMode=PerGame&Period=0&PlayerID=")
  playermatrix[,15] = playerid
  playermatrix[,16] = paste("&Season=2015-16&SeasonSegment=&SeasonType=")
  playermatrix[,17] = season
  playermatrix[,18] = paste("&TeamID=0&VsConference=&VsDivision=")
  playermatrix = paste(playermatrix[,1],
                       playermatrix[,2],
                       playermatrix[,3],
                       playermatrix[,4],
                       playermatrix[,5],
                       playermatrix[,6],
                       playermatrix[,7],
                       playermatrix[,8],
                       playermatrix[,9],
                       playermatrix[,10],
                       playermatrix[,11],
                       playermatrix[,12],
                       playermatrix[,13],
                       playermatrix[,14],
                       playermatrix[,15],
                       playermatrix[,16],
                       playermatrix[,17],
                       playermatrix[,18],
                       sep = '')
  playermatrix = data.frame(playermatrix,stringsAsFactors = FALSE)
  data = apply(X = playermatrix,MARGIN = 1,FUN = RJSONIO::fromJSON, nullValue = "NA")
  return(data)
}

####################

#player scoring and shooting data from date
#used for nodes later

player.scores = function(x,y,z,season,teamid)
{
  data = paste("http://stats.nba.com/stats/teamplayerdashboard?DateFrom=",
               x,
               "%2F",
               y,
               "%2F",
               z,
               "&DateTo=",
               x,
               "%2F",
               y,
               "%2F",
               z,
               "&GameSegment=&LastNGames=0&LeagueID=00&Location=&MeasureType=Scoring&Month=0&OpponentTeamID=0&Outcome=&PaceAdjust=N&PerMode=PerGame&Period=0&PlusMinus=N&Rank=N&Season=2015-16&SeasonSegment=&SeasonType=",
               season,
               "&TeamID=",
               teamid,
               "&VsConference=&VsDivision=",
               sep = "")
  data = RJSONIO::fromJSON(data,nullValue='NA')
  cn = data[[3]][[2]]$headers
  data = data[[3]][[2]]$rowSet
  data = as.matrix(data,ncol=length(data))
  data = t(apply(X = data,MARGIN = 1,FUN = unlist))
  data = data.frame(data,stringsAsFactors = FALSE)
  colnames(data) = cn
  data$PLAYER_ID = as.character(data$PLAYER_ID)
  data$GROUP_SET = as.character(data$GROUP_SET)
  data$PLAYER_ID = as.character(data$PLAYER_ID)
  data$PLAYER_NAME = as.character(data$PLAYER_NAME)
  data$GP = as.numeric(data$GP)
  data$W = as.numeric(data$W)
  data$L = as.numeric(data$L)
  data$W_PCT = as.numeric(data$W_PCT)
  data$MIN = as.numeric(data$MIN)
  data$PCT_FGA_2PT = as.numeric(data$PCT_FGA_2PT)
  data$PCT_FGA_3PT = as.numeric(data$PCT_FGA_3PT)
  data$PCT_PTS_2PT = as.numeric(data$PCT_PTS_2PT)
  data$PCT_PTS_2PT_MR = as.numeric(data$PCT_PTS_2PT_MR)
  data$PCT_PTS_3PT = as.numeric(data$PCT_PTS_3PT)
  data$PCT_PTS_FB = as.numeric(data$PCT_PTS_FB)
  data$PCT_PTS_FT = as.numeric(data$PCT_PTS_FT)
  data$PCT_PTS_OFF_TOV = as.numeric(data$PCT_PTS_OFF_TOV)
  data$PCT_PTS_PAINT = as.numeric(data$PCT_PTS_PAINT)
  data$PCT_AST_2PM = as.numeric(data$PCT_AST_2PM)
  data$PCT_UAST_2PM = as.numeric(data$PCT_UAST_2PM)
  data$PCT_AST_3PM = as.numeric(data$PCT_AST_3PM)
  data$PCT_AST_3PM = as.numeric(data$PCT_AST_3PM)
  data$PCT_UAST_3PM  = as.numeric(data$PCT_UAST_3PM)
  data$PCT_AST_FGM  = as.numeric(data$PCT_AST_FGM)
  data$PCT_UAST_FGM  = as.numeric(data$PCT_UAST_FGM)
  return(data)
}

####################

#recieved pass dataframe
#yas is a Broad City reference because I was really happy this worked
yas1 = playerdata.url(x = x,y = y,z = z,playerid = playerid.made, season = season)

####################

#made pass dataframe
#yas is a Broad City reference because I was really happy this worked
yas2 = playerdata.url(x = x,y = y,z = z,playerid = playerid.rec, season = season)

####################

#getting the number of players who made passes
player.num1 = matrix(c(1:length(yas1)))

####################

#getting the number of players of recieved passes
player.num2 = matrix(c(1:length(yas2)))

####################

#function to convert the JSON data that I spent all this time 
#collecting and manipulating and into a workable dataframe
#does it with made pass data

fucking.finally1 = function(x)
{
  dat = yas1[[x]]$resultSets[[1]]$rowSet
  return(data.frame(t(matrix(unlist(dat),nrow=21)),stringsAsFactors = FALSE))
}

###################

#function to convert the JSON data that I spent all this time 
#collecting and manipulating and into a workable dataframe
#does it with rec pass data

fucking.finally2 = function(x)
{
  dat = yas2[[x]]$resultSets[[2]]$rowSet
  return(data.frame(t(matrix(unlist(dat),nrow=21)),stringsAsFactors = FALSE))
}

###################

made = apply(X = player.num1, FUN = fucking.finally1, MARGIN = 1)
made = do.call("rbind",made)
colnames(made) = yas1[[1]]$resultSets[[1]]$headers
colnames(made)[8] = paste("OTHER_PLAYER")

##################

recv = apply(X = player.num2, FUN = fucking.finally2, MARGIN = 1)
recv = do.call("rbind",recv)
colnames(recv) = yas2[[1]]$resultSets[[2]]$headers
colnames(recv)[8] = paste("OTHER_PLAYER")

###################

final.data = rbind(made, recv)

###################

final.data$TEAM_NAME = as.factor(final.data$TEAM_NAME)
final.data$FREQUENCY = as.numeric(final.data$FREQUENCY)
final.data$PASS = as.numeric(final.data$PASS)
final.data$AST = as.numeric(final.data$AST)
final.data$FGM = as.numeric(final.data$FGM)
final.data$FGA = as.numeric(final.data$FGA)
final.data$FG_PCT = as.numeric(final.data$FG_PCT)
final.data$FG2M = as.numeric(final.data$FG2M)
final.data$FG2A = as.numeric(final.data$FG2A)
final.data$FG2_PCT = as.numeric(final.data$FG2_PCT)
final.data$FG3M = as.numeric(final.data$FG3M)
final.data$FG3A = as.numeric(final.data$FG3A)
final.data$FG3_PCT = as.numeric(final.data$FG3_PCT)

###################

network.data = data.frame(cbind(final.data$PLAYER_NAME_LAST_FIRST, final.data$OTHER_PLAYER, final.data$PASS_TYPE, final.data$PASS), stringsAsFactors = FALSE)
network.data$X4 = as.numeric(network.data$X4)

dat.sort = t(apply(network.data, 1, sort))
network.data = network.data[!duplicated(dat.sort),]

node.data = player.scores(x = x, y = y, z = z, season = season, teamid = teamid)
node.data = cbind(node.data$PLAYER_NAME, node.data$PCT_FGA_2PT)

network.links = network.data 

network.nodes = unique(rbind(melt(unique(network.links$X1)), melt(unique(network.links$X2)))) 
network.nodes = as.character(network.nodes$value)  
  
net <- graph.data.frame(d=network.links, vertices=network.nodes, directed=T) 
E(net)$width = (network.links$X4/2)
V(net)$size = as.numeric(node.data[,2])*40

l = layout_in_circle(net)
plot.igraph(net, layout = l, edge.arrow.size = 0.10, vertex.label.cex =1.75)

###################

rm(list = ls())
