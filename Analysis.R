source('Functions.R')
library(dplyr)
library(ggplot2)

##Quarterback Analysis
QB<-PositionSeason("qb")
GOOD<-aggregate(POS~PLAYER, QB[QB$ATT>=5,],length)
QB19<-aggregate(PTS~PLAYER+WK,QB[QB$PLAYER %in% GOOD$PLAYER[GOOD$POS>5],],FUN=sum)
QB19$PLAYER <- as.factor(QB19$PLAYER)
QB19$WK <- as.factor(QB19$WK)

QBSTATS<-group_by(QB19,PLAYER)%>%summarize(MEAN=round(mean(PTS),1) ,SD = round(sd(PTS),1), MEDIAN = median(PTS), MAX = max(PTS), MIN = min(PTS), G = length(PTS), Total = sum(PTS))%>%arrange(desc(MEAN))

ggplot(QB19[QB19$PLAYER %in% QBSTATS$PLAYER[1:10],],aes(x=PLAYER,y=PTS,fill=PLAYER))+geom_boxplot()+scale_y_continuous(limits=c(-10,50))+coord_flip()+labs(title="Top 1-10 QB")
ggplot(QB19[QB19$PLAYER %in% QBSTATS$PLAYER[11:20],],aes(x=PLAYER,y=PTS,fill=PLAYER))+geom_boxplot()+scale_y_continuous(limits=c(-10,50))+coord_flip()+labs(title="Top 11-20 QB")
ggplot(QB19[QB19$PLAYER %in% QBSTATS$PLAYER[21:31],],aes(x=PLAYER,y=PTS,fill=PLAYER))+geom_boxplot()+scale_y_continuous(limits=c(-10,50))+coord_flip()+labs(title="Top 21+ QB")


##Runningback Analysis
RB<-PositionSeason("RB")
GOOD<-aggregate(POS~PLAYER, RB[RB$RUSH>=5,],length)
RB19<-aggregate(PTS~PLAYER+WK,RB[RB$PLAYER %in% GOOD$PLAYER[GOOD$POS>5],],FUN=sum)
RB19$PLAYER <- as.factor(RB19$PLAYER)
RB19$WK <- as.factor(RB19$WK)

RBSTATS<-group_by(RB19,PLAYER)%>%summarize(MEAN=round(mean(PTS),1) ,SD = round(sd(PTS),1), MEDIAN = median(PTS), MAX = max(PTS), MIN = min(PTS), G = length(PTS), Total = sum(PTS))%>%arrange(desc(MEAN))

ggplot(RB19[RB19$PLAYER %in% RBSTATS$PLAYER[1:10],],aes(x=PLAYER,y=PTS,fill=PLAYER))+geom_boxplot()+scale_y_continuous(limits=c(0,50))+coord_flip()+labs(title="Top 1-10 RB")
ggplot(RB19[RB19$PLAYER %in% RBSTATS$PLAYER[11:20],],aes(x=PLAYER,y=PTS,fill=PLAYER))+geom_boxplot()+scale_y_continuous(limits=c(0,50))+coord_flip()+labs(title="Top 11-20 RB")
ggplot(RB19[RB19$PLAYER %in% RBSTATS$PLAYER[21:30],],aes(x=PLAYER,y=PTS,fill=PLAYER))+geom_boxplot()+scale_y_continuous(limits=c(0,50))+coord_flip()+labs(title="Top 21-30 RB")
ggplot(RB19[RB19$PLAYER %in% RBSTATS$PLAYER[31:43],],aes(x=PLAYER,y=PTS,fill=PLAYER))+geom_boxplot()+scale_y_continuous(limits=c(0,50))+coord_flip()+labs(title="Top 31+ RB")

##WR Analysis
WR<-PositionSeason("WR")
GOOD<-aggregate(POS~PLAYER, WR[WR$REC>=1,],length)
WR19<-aggregate(PTS~PLAYER+WK,WR[WR$PLAYER %in% GOOD$PLAYER[GOOD$POS>5],],FUN=sum)
WR19$PLAYER <- as.factor(WR19$PLAYER)
WR19$WK <- as.factor(WR19$WK)

WRSTATS<-group_by(WR19,PLAYER)%>%summarize(MEAN=round(mean(PTS),1) ,SD = round(sd(PTS),1), MEDIAN = median(PTS), MAX = max(PTS), MIN = min(PTS), G = length(PTS), Total = sum(PTS))%>%arrange(desc(MEAN))

ggplot(WR19[WR19$PLAYER %in% WRSTATS$PLAYER[1:10],],aes(x=PLAYER,y=PTS,fill=PLAYER))+geom_boxplot()+scale_y_continuous(limits=c(-2,35))+coord_flip()+labs(title="Top 1-10 WR")
ggplot(WR19[WR19$PLAYER %in% WRSTATS$PLAYER[11:20],],aes(x=PLAYER,y=PTS,fill=PLAYER))+geom_boxplot()+scale_y_continuous(limits=c(-2,35))+coord_flip()+labs(title="Top 11-20 WR")
ggplot(WR19[WR19$PLAYER %in% WRSTATS$PLAYER[21:30],],aes(x=PLAYER,y=PTS,fill=PLAYER))+geom_boxplot()+scale_y_continuous(limits=c(-2,35))+coord_flip()+labs(title="Top 21-30 WR")
ggplot(WR19[WR19$PLAYER %in% WRSTATS$PLAYER[31:40],],aes(x=PLAYER,y=PTS,fill=PLAYER))+geom_boxplot()+scale_y_continuous(limits=c(-2,35))+coord_flip()+labs(title="Top 31-40 WR")
ggplot(WR19[WR19$PLAYER %in% WRSTATS$PLAYER[41:50],],aes(x=PLAYER,y=PTS,fill=PLAYER))+geom_boxplot()+scale_y_continuous(limits=c(-2,35))+coord_flip()+labs(title="Top 41-50 WR")

##TE Analysis
TE<-PositionSeason("TE")
GOOD<-aggregate(POS~PLAYER, TE[TE$REC>=1,],length)
TE19<-aggregate(PTS~PLAYER+WK,TE[TE$PLAYER %in% GOOD$PLAYER[GOOD$POS>5],],FUN=sum)
TE19$PLAYER <- as.factor(TE19$PLAYER)
TE19$WK <- as.factor(TE19$WK)

TESTATS<-group_by(TE19,PLAYER)%>%summarize(MEAN=round(mean(PTS),1) ,SD = round(sd(PTS),1), MEDIAN = median(PTS), MAX = max(PTS), MIN = min(PTS), G = length(PTS), Total = sum(PTS))%>%arrange(desc(MEAN))

ggplot(TE19[TE19$PLAYER %in% TESTATS$PLAYER[1:10],],aes(x=PLAYER,y=PTS,fill=PLAYER))+geom_boxplot()+scale_y_continuous(limits=c(0,35))+coord_flip()+labs(title="Top 1-10 TE")
ggplot(TE19[TE19$PLAYER %in% TESTATS$PLAYER[11:20],],aes(x=PLAYER,y=PTS,fill=PLAYER))+geom_boxplot()+scale_y_continuous(limits=c(0,35))+coord_flip()+labs(title="Top 11-20 TE")
ggplot(TE19[TE19$PLAYER %in% TESTATS$PLAYER[21:30],],aes(x=PLAYER,y=PTS,fill=PLAYER))+geom_boxplot()+scale_y_continuous(limits=c(0,35))+coord_flip()+labs(title="Top 21-30 TE")
ggplot(TE19[TE19$PLAYER %in% TESTATS$PLAYER[31:39],],aes(x=PLAYER,y=PTS,fill=PLAYER))+geom_boxplot()+scale_y_continuous(limits=c(0,35))+coord_flip()+labs(title="Top 31+ TE")



