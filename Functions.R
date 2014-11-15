library(XML)
library(stringr)

## Gets slotCategory_id for desired position
## Used in conjuction with PositionWeekData to get desired url for position
PosNum <- function(pos){
  pos.upper <- toupper(pos)
  pos.df <- data.frame(position = c("QB","RB","WR","TE","D/ST","K","FLEX"),
                       position.number = c('0','2','4','6','16','17','23'))
  pos_num <- pos.df$position.number[pos.df$position==pos.upper]
}

## Get ESPN data for specified week and position.  For all players eligible for points and not on a bye week
PositionWeekData <- function(pos=FALSE, week=1, year=2014 ){
  if(pos==F) {stop("input a position")}
  pos_num <- PosNum(pos)
  raw <- list()
  #arbitrary number,  assumption is no pull will be greater than 5000 rows
  start.pos <- seq(0,5000,50)
  n <- length(seq(0,5000,50))
  #exception for D/ST
  if(pos_num =='16') {
    raw[[1]] <- readHTMLTable(paste('http://games.espn.go.com/ffl/leaders?leagueId=0&slotCategoryId=',pos_num, '&scoringPeriodId=',week,'&seasonId=',year,'&startIndex=0',sep=''), stringsAsFactors = FALSE, header=T, skip.rows=1)$playertable_0
  }
  else {
    for(i in 1:n) {
      j <- start.pos[i]
      raw[[i]] <- readHTMLTable(paste('http://games.espn.go.com/ffl/leaders?leagueId=0&slotCategoryId=',pos_num, '&scoringPeriodId=',week,'&seasonId=',year,'&startIndex=',j,sep=''), stringsAsFactors = FALSE, header=T, skip.rows=1)$playertable_0
      if('** BYE **' %in% raw[[c(i,3)]]) break
    }
  }
 data <- do.call("rbind",raw)
 data <- data[complete.cases(data),]
 CleanTable(table=data,position=pos,week=week)
}


##Clean table readHTMLTable produces into readable and desired format
##Since this is ESPN point totals, each position has the same table output
CleanTable <- function(table,position, week) {
##  Cleans HTML Table into readable format
  df <- table[,c(1,3:4,6:9,11:13,15:18,20:22,24)]
  df$PLAYER <- gsub(",.*","",df[,1]); df$PLAYER <- as.factor(gsub("[*]","",df$PLAYER))
  df$POS <- toupper(position)
  NOPLAYER <- gsub(".*, ","",df[,1])
  df$TEAM <- str_trim(substr(NOPLAYER,1,3))
  df$LOC <- ifelse(substr(df$OPP,1,1)=="@", "A","H"); df$LOC <- as.factor(df$LOC)
  df$OPP <- gsub("@","",df$OPP)
  df$COMP <- gsub("/.*","",df[,"C/A"])
  df$ATT <- gsub(".*/","",df[,"C/A"])
  df$WK <- as.factor(week)
  df <- df[,c(19:21,25,22,2:3,23:24,5:18)]
  names(df) <- c("PLAYER","POS","TEAM","WK","LOC","OPP","SCORE","COMP","ATT","PASS.YDS","PASS.TD","INT","RUSH","RUSH.YDS","RUSH.TD","REC","REC.YDS","REC.TD","TAR","2PC","FUML","MISC.TD","PTS")
  for(i in 8:23) df[,i] <- suppressWarnings(as.numeric(as.character(df[,i])))
  df
} 

##Provides Multiple weeks within a single season.
PositionSeason <- function(pos=FALSE, weeks = 1:17, year=2014){
  data <- list()
  for(i in weeks){
    data[[i]]<-PositionWeekData(pos=pos, week=i, year= year)
  }
  df<- do.call("rbind",data)
  df <- df[complete.cases(df),]
}

