#########################################################################
library(tibble)
library(nbastatR)
library(curl)
library(dplyr)
library(tidyverse)
library(bettoR)
library(MASS)
Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 2)

## Getting all of the team names 
teams=teams_details(all_teams=TRUE)
team.names=teams[[2]][[1]]

today=Sys.Date()
yesterday=Sys.Date()-1
todays.games = days_scores(game_dates = today, include_standings = F, return_message = T)
this.seasons.games = 22100001:as.numeric(todays.games[[2]][[1]][1,9])
todays.games = todays.games[[2]][[3]]

yesterdays.games = days_scores(game_dates = yesterday, include_standings = F, return_message = T)
yesterdays.games = yesterdays.games[[2]][[3]]

## View(todays.games)

games = dim(todays.games)[1]

home.index = numeric(games)
for (i in 1:games){
  hteam = as.character(todays.games[i,3])
  home.index[i] = which(hteam == team.names[,3])
}

away.index = numeric(games)
for (i in 1:games){
  ateam = as.character(todays.games[i,5])
  away.index[i] = which(ateam == team.names[,3])
}

## This requires already having current season box scores in a file bs.1.csv
## Trying to download more than just yesterday's games dramatically increases
## the model's run time
bs.1 = read.csv('bs.1.csv')
bs.1 = bs.1[-1]

bs.new=box_scores(game_ids = yesterdays.games$idGame, box_score_types = c("Traditional", "Advanced", 
                                                                   "Scoring", "Misc", 
                                                                   "Usage", "Four Factors", "Tracking"), 
                 result_types = c("team"), join_data = TRUE, assign_to_environment = TRUE, 
                 return_message = TRUE)
bs.new = bs.new[[2]][[1]]
bs.1 = rbind(bs.1, bs.new)

write.csv(bs.1, file = 'bs.1.csv')

n=dim(bs.1)[1]/2
bs.1$home = rep(c(1,0), n)
g=dim(todays.games)[1]

sch=current_schedule()
sch=sch[67:(n+g+66),]

sch$yesterday=sch$dateGame-1
sch$hb2b=0
sch$ab2b=0

for(i in 14:(n+g)){
  sch.y=subset(sch, sch$dateGame==sch$yesterday[i])
  sch$hb2b[i]=1*(is.element(sch$slugTeamHome[i], sch.y$slugTeamAway)|
                   is.element(sch$slugTeamHome[i], sch.y$slugTeamHome))
  sch$ab2b[i]=1*(is.element(sch$slugTeamAway[i], sch.y$slugTeamAway)|
                   is.element(sch$slugTeamAway[i], sch.y$slugTeamHome))
}
b2b.names=c(sch$nameTeamHome, sch$nameTeamAway)

b2b=c(sch$hb2b, sch$ab2b)
p=length(b2b)
sort.vec=c(seq(from=1, to=p-1, by=2), seq(from=2, to=p, by=2))
b2b=cbind(b2b.names, b2b, sort.vec)
b2b=b2b[order(sort.vec),]
b=dim(b2b)[1]

bs.1$b2b=b2b[1:(b-2*g),2]

#######################################################################
nba.mat=NULL
this.season=NULL
##2020-21 Season data
for (i in 1:30) {
  full=teams_tables(teams = team.names[i,2], 
                    seasons = 2020,
                    tables = c("year over year"), 
                    measures = c("Base", 'Advanced', 'Opponent'),
                    modes = "PerGame",
                    season_types = 'Regular Season')
  
  combined=cbind(full[[15]][[1]], full[[15]][[2]], full[[15]][[3]])
  team=combined[2:10,]
  
  this.season = rbind(this.season, combined[1,])
  nba.mat=rbind(nba.mat,team)
}

names.vec=team.names[,2]
nba.mat=cbind(names.vec, nba.mat)
this.season=cbind(names.vec, this.season)


this.season$pred.o = NA
this.season$pred.d = NA
this.season$pred.o.prev = NA
this.season$pred.d.prev = NA
this.season$home = 0
this.season$b2b = 0

this.season$home = 1*(is.element(this.season$nameTeam, sch$nameTeamHome[(p/2-g+1):(p/2)]))

b2b.c=as.data.frame(b2b[(1+p-2*g):p,])
b2b.c=b2b.c[b2b.c$b2b==1,]
this.season$b2b=1*(is.element(this.season$nameTeam, b2b.c$b2b.names))

if(mean(this.season$b2b)==0)
  this.season$b2b[-c(home.index,away.index)] = 1

if(mean(this.season$b2b)==1)
  this.season$b2b[-c(home.index,away.index)] = 0

bs.1$home=as.factor(bs.1$home)
bs.1$b2b=as.factor(bs.1$b2b)

this.season$home=as.factor(this.season$home)
this.season$b2b=as.factor(this.season$b2b)

this.season$rateFTAOpponent = this.season$ftaOpponent/this.season$paceE
this.season$pctEFGOpponent = (this.season$fgmOpponent + (.5*this.season$fg3mOpponent))/this.season$fgaOpponent

bs.1$lpctFG = log(as.numeric(bs.1$pctFG))
bs.1$lpctFG3 = log(as.numeric(bs.1$pctFG3))
bs.1$lpctAST = log(as.numeric(bs.1$pctAST))
bs.1$lpctOREB = log(as.numeric(bs.1$pctOREB))
bs.1$lpctEFG = log(as.numeric(bs.1$pctEFG))
bs.1$lpctTS = log(as.numeric(bs.1$pctTS))

this.season$lpctFG = log(this.season$pctFG)
this.season$lpctFG3 = log(this.season$pctFG3)
this.season$lpctAST = log(this.season$pctAST)
this.season$lpctOREB = log(this.season$pctOREB)
this.season$lpctEFG = log(this.season$pctEFG)
this.season$lpctTS = log(this.season$pctTS)

om.game=lm(ortg ~ lpctFG + lpctFG3 + fta + oreb + ast + stl + pfd + lpctAST +  lpctOREB +
            lpctEFG + lpctTS + pace + ratioPIE + home + b2b, data=bs.1)

dm.game=lm(drtg ~ pctFG + dreb + tov + pf + pctDREB + pctTOVTeam + pace + 
             rateFTAOpponent+  pctEFGOpponent + ratioPIE +  home + b2b, data=bs.1)


##########################################################################
##########################################################################
## Simulation based estimates for spreads and winners

omse=sum(om.game$residuals^2)/((n*2)-16)
dmse=sum(om.game$residuals^2)/((n*2)-12)

reps=1e4

## Home.spread is the point spread for the home team in each game in the order
## found in todays.games
## The next 4 are the american odds for each category

home.spread=c()
hats = c()
aats = c()
hml = c()
aml = c()

## To run the model without gambling information
## home.spread = hats = aats = hml = aml = rep(0, games)

this.season$pred.o = predict(om.game, newdata = this.season, type = 'response')

teams.last = seq(from = 1, to = 262, by = 9)
last.season = NULL

team.num.mat = matrix(0, nrow = games, ncol = 2)

#Home teams
team.num.mat[,1]= home.index

#Away Teams
team.num.mat[,2]= away.index
colnames(team.num.mat) = c('Home', 'Away')

bet.mat.sim = matrix(NA, nrow = games, ncol = 9)
for(i in 1:games){
  print(i/games)
  
  h=team.num.mat[i,1]
  a=team.num.mat[i,2]
  
  h.pace = this.season$pace[h]
  a.pace = this.season$pace[a]
  t.pace = (h.pace + a.pace)/2
  
  h.predo = this.season$pred.o[h]
  a.predo = this.season$pred.o[a]

  league.o = mean(this.season$ortg)
  league.d = mean(this.season$drtg)
  
  h.predd = this.season$drtg[h]
  a.predd = this.season$drtg[a]
  
  sim.mat=matrix(NA, nrow = reps, ncol = 5)
  for(j in 1:reps){
    ## Find simulated rtg
    h.ortg=rnorm(1, mean = h.predo, sd = (omse))
    a.ortg=rnorm(1, mean = a.predo, sd = (omse))
    
    h.drtg=rnorm(1, mean = h.predd, sd = (dmse))
    a.drtg=rnorm(1, mean = a.predd, sd = (dmse))
    
    sim.pace=rnorm(1, mean = t.pace, sd = sd(this.season$pace))
    
    ## Fining simulation based results
    sim.mat[j,1] = hscore = (((h.ortg - league.o)/100) + ((a.drtg- league.d)/100)+ league.o/100)*t.pace 
    sim.mat[j,2] = ascore = (((a.ortg - league.o)/100) + ((h.drtg- league.d)/100)+ league.o/100)*t.pace
    
    sim.mat[j,3] = ascore - hscore
    sim.mat[j,4] = ifelse((hscore - ascore) + home.spread[i] > 0, 1, 0)
    sim.mat[j,5] = ifelse(hscore - ascore > 0, 1, 0)
  }
  bet.mat.sim[i,]= c(apply(sim.mat, 2, mean), 0, 0,0,0)
  bet.mat.sim[i,6] = implied_odds(bet.mat.sim[i,4])
  bet.mat.sim[i,7] = implied_odds(1-bet.mat.sim[i,4])
  bet.mat.sim[i,8] = implied_odds(bet.mat.sim[i,5])
  bet.mat.sim[i,9] = implied_odds(1-bet.mat.sim[i,5])
}

colnames(bet.mat.sim)= c('eHomeScore', 'eAwayScore', 'eSpread', '%HomeCover', '%HomeWin', 'HomeSpread',
                         'AwaySpread', 'HomeML', 'AwayML')

home.names = numeric(games)
for(i in 1:games){
  index = team.num.mat[i,1]
  name = team.names[index, 2]
  home.names[i]=name
}

rownames(bet.mat.sim)=home.names

ev.mat=matrix(NA, ncol=4, nrow= games)
a=100
k=1

for (i in 1:games){
  ev.mat[i,1]=((bet_calc(a, hats[i])-a)*bet.mat.sim[i,4])-a*(1-bet.mat.sim[i,4])
  ev.mat[i,2]=((bet_calc(a, hml[i])-a)*bet.mat.sim[i,5])-a*(1-bet.mat.sim[i,4])
  ev.mat[i,3]=((bet_calc(a, aats[i])-a)*(1-bet.mat.sim[i,4]))-a*(bet.mat.sim[i,4])
  ev.mat[i,4]=((bet_calc(a, aml[i])-a)*(1-bet.mat.sim[i,5]))-a*(bet.mat.sim[i,4])
}

colnames(ev.mat)=c('hATS', 'hML', 'aATS', 'aML')
rownames(ev.mat)=home.names

bet.mat.sim=cbind(bet.mat.sim, ev.mat, hats, hml, aats, aml)
write.csv(bet.mat.sim, file='01.05.22Predictions.csv')



