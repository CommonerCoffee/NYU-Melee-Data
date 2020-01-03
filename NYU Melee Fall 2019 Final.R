library(plyr)
library(dplyr)
library("rjson")
library(sqldf)
library(jsonlite)
library(ggplot2)
library(scales)
library(tidyr)
library(PlayerRatings)
library(bsselectR)

setwd("C:/Users/Raymond Luu/Downloads/Coding/R/JSON")

#Importing Challonge Data
Tournaments=c("NYU78","iwbaat74","9gxudghh","81wgy1jl","sxfpv70o",
              "tgw6vt0q","v8ozqrkg","xknaelz4","xswn7m1a","1j3hl7zc",
              "63kb39k8","pqbi15d5","lj1ov9vg","czg1m431","enl7ni84")

matchlist=list()
participantslist=list()


for (i in 1:length(Tournaments))
{
  matches <- paste("matches", i, sep = "")
  #matches <- i
  participants <- paste("participants", i, sep = "")
  tourney= paste(Tournaments[i],".json",sep='')
  matchlist[[matches]] <- na.omit(fromJSON(txt = tourney,flatten = "T")$tournament$matches$match[c("winner_id","loser_id","scores_csv")])
  participantslist[[participants]] <- cbind(date=as.Date(fromJSON(txt = tourney,flatten = "T")$tournament$started_at),
                                            fromJSON(txt = tourney,flatten = "T")$tournament$participants$participant[c("id","name", 'seed','final_rank')])
}
  
  matchlist_df=as.data.frame(do.call(rbind, matchlist))
  matchlist_df <- tibble::rownames_to_column(matchlist_df, "Match")
  matchlist_df$Match=as.numeric(substring(matchlist_df$Match,8,8)) #Change this later
  
  participants_df=as.data.frame(do.call(rbind, participantslist))
  participants_df <- tibble::rownames_to_column(participants_df, "Event")
  participants_df$Event=as.numeric(substring(participants_df$Event,13,13)) #Change this later
  
  
  #Deleting DQs
  matchlist_df=matchlist_df[nchar(matchlist_df$scores_csv)==3,]
  
  #Fixing Misordering
  misordered=as.numeric(substring(matchlist_df$scores_csv,1,1))<
    as.numeric(substring(matchlist_df$scores_csv,3,3))
  
  matchlist_df$scores_csv[misordered]=
    paste(as.numeric(substring(matchlist_df$scores_csv[misordered],3,3)),
          as.numeric(substring(matchlist_df$scores_csv[misordered],1,1)),sep="-")
  
  #Fixing Names
  participants_df$name=tolower(participants_df$name)
  participants_df$name[participants_df$name==
                                     "nebulous | moburu"] <- "moburu"
  participants_df$name[participants_df$name==
                                     "gombit"] <- "gambit"
  participants_df$name[participants_df$name==
                                     "cum | unga"] <- "unga"
  participants_df$name[participants_df$name==
                                     "nyu|ungay"] <- "unga"
  participants_df$name[participants_df$name==
                                     "gehno"] <- "geno"
  participants_df$name[participants_df$name==
                                     "moburu, aspect of earth"] <- "moburu"
  participants_df$name[participants_df$name==
                                     "seman explosion off the coast of victoria australia"] <- "news headline guy"
  participants_df$name[participants_df$name==
                                     "lebron seeks to trademark taco tuuuuuuuuesdaaaaaaays"] <- "news headline guy"
  participants_df$name[participants_df$name==
                                     "s h e i c c"] <- "sheicc"
  participants_df$name[participants_df$name==
                                     "eli"] <- "ziggy"
  participants_df$name[participants_df$name==
                                     "ziggy :)"] <- "ziggy"
  participants_df$name[participants_df$name==
                                     "tess"] <- "tesselation"
  participants_df$name[participants_df$name==
                                     "54$ and a donut"] <- "eli zhu"
  participants_df$name[participants_df$name==
                                     "k7a"] <- "k8a"
  participants_df$name[participants_df$name==
                                     "sosa's biggest fan"] <- "sosa"
  participants_df$name[participants_df$name==
                                     "shnert"] <- "ziggy"
  participants_df$name[participants_df$name==
                                     "happy 1017man"] <- "sheicc"
  participants_df$name[participants_df$name==
                                     "leafwheaf"] <- "leafweaf"
  participants_df$name[participants_df$name==
                                     "shicc"] <- "sheicc"
  participants_df$name[participants_df$name==
                                     "sheicc"] <- "sunset"
  participants_df$name[participants_df$name==
                         "rivertable"] <- "hohep"
  
  
  #Time/Win
  win=1
  matchlist_df=cbind(matchlist_df,win)
  
  names=participants_df$name
  name_merge=cbind(participants_df,names)[,c(2:7)]
  
  total<-merge(matchlist_df,name_merge,by.x="winner_id", by.y="id")
  total2<-merge(total,name_merge,by.x="loser_id", by.y="id")
  
  
  
  ###Outputs for Analysis
  
  #Participants Information
  Participants_Output=participants_df[c(2,4,5,6)]
  colnames(Participants_Output)=c('date','name','seed','placing')
  Participants_Output$seed_difference=Participants_Output$seed-Participants_Output$placing
  
  x=Participants_Output$placing
  x[x==5]=5
  x[x==7]=6
  x[x==9]=7
  x[x==13]=8
  x[x==17]=9
  x[x==25]=10
  x[x==33]=11
  x[x==49]=12
  x[x==65]=13
  
  y=Participants_Output$seed
  y[y>=5 & y<=6]=5
  y[y>=7 & y<=8]=6
  y[y>=9 & y<=12]=7
  y[y>=13 & y<=16]=8
  y[y>=17 & y<=24]=9
  y[y>=25 & y<=32]=10
  y[y>=33 & y<=48]=11
  y[y>=49 & y<=64]=12
  y[y>=65 & y<=96]=13

  Participants_Output$Round_diff=z
  
  #Match Information
  Match_Output=total2[c(6,7,12,4)]
  colnames(Match_Output)=c('date','winner','loser','score')
  
  ###Functions and Elo
  
  #Ratings
  eloinput<-data.frame(total2$Match,Match_Output[c(2:3)],1)
  eloinput$winner=as.character(eloinput$winner)
  eloinput$loser=as.character(eloinput$loser)
  elo<-elo(eloinput,init=1000,kfac=32)
  elo
  
  #Win Probability Function
  EloWin <- function(x,y){
    x_rating <- elo$rating[ which(elo$rating$Player==x),2]
    y_rating <- elo$rating[ which(elo$rating$Player==y),2]
    transformedx <- 10**(x_rating/400)
    transformedy <- 10**(y_rating/400)
    x_winprob <- transformedx/(transformedx+transformedy)
    y_winprob <- transformedy/(transformedx+transformedy)
    message(paste(x,'probability of beating',y,":",round(100*x_winprob,1),'%'))
    message(paste(y,'probability of beating',x,":",round(100*y_winprob,1),'%'))
  }
  
  
  #Match History Function
  History <- function(player1){
    Match_Output[c(which(Match_Output$winner==player1),which(Match_Output$loser==player1)),]
  }
  
  #Head to Head Function
  H2H <- function(player1,player2){
    p1=Match_Output$winner==player1
    p2=Match_Output$loser==player2
    p3=Match_Output$winner==player2
    p4=Match_Output$loser==player1
    Match_Output[c(which(p1+p2==2),which(p3+p4==2)),]
  }
  
  #testing 
  History("haveacookie")
  
  #Writing Matrices
  PR_Consideration=c('guava',
                     'kong',
                     'tesselation',
                     'unga',
                     'hohep',
                     'eli zhu',
                     'modulo',
                     'holla',
                     'cheeseboats',
                     'curtains',
                     'fishpaste',
                     'semicolon',
                     'bchoi',
                     'sunset',
                     'jwp',
                     'bubba',
                     'ziggy')
    
    Top20=c('guava',
            'spare parts',
            'k8a',
            'kong',
            'dbz',
            'tesselation',
            'gambit',
            'sunn',
            'tazio',
            'luu',
            'nation',
            'lumble',
            'panos',
            'ghost',
            'snow halataion',
            'sosa',
            'hohep',
            'eli zhu',
            'geno',
            'jeremy')
    
    H2H_mat_NYU=matrix(0,nrow=length(PR_Consideration),ncol=length(PR_Consideration))
    H2H_mat_Top20=matrix(0,nrow=length(Top20),ncol=length(Top20))
    H2H_mat_NYU_2=matrix(0,nrow=length(PR_Consideration),ncol=length(elo$ratings$Player))
    
    #Writing Matrices
    for (i in 1:length(PR_Consideration)){
      for (j in 1:length(PR_Consideration)){
        wins=nrow(subset(subset(Match_Output,winner==PR_Consideration[i]),loser==PR_Consideration[j]))
        losses=nrow(subset(subset(Match_Output,winner==PR_Consideration[j]),loser==PR_Consideration[i]))
        if (i==j) {value=''} 
        else{value=paste(wins,"-",losses)}
        H2H_mat_NYU[i,j]=value}
    }
    
    for (i in 1:length(Top20)){
      for (j in 1:length(Top20)){
        wins=nrow(subset(subset(Match_Output,winner==Top20[i]),loser==Top20[j]))
        losses=nrow(subset(subset(Match_Output,winner==Top20[j]),loser==Top20[i]))
        if (i==j) {value=''} 
        else{value=paste(wins,"-",losses)}
        H2H_mat_Top20[i,j]=value}
    }
    
    for (i in 1:length(PR_Consideration)){
      for (j in 1:length(elo$ratings$Player)){
        wins=nrow(subset(subset(Match_Output,winner==PR_Consideration[i]),loser==elo$ratings$Player[j]))
        losses=nrow(subset(subset(Match_Output,winner==elo$ratings$Player[j]),loser==PR_Consideration[i]))
        value=paste(wins,"-",losses)
        H2H_mat_NYU_2[i,j]=value}
    }
    
    H2H_mat_NYU=as.data.frame(H2H_mat_NYU)
    rownames(H2H_mat_NYU)<-PR_Consideration
    colnames(H2H_mat_NYU)<-PR_Consideration
    
    H2H_mat_Top20=as.data.frame(H2H_mat_Top20)
    rownames(H2H_mat_Top20)<-Top20
    colnames(H2H_mat_Top20)<-Top20
    
    H2H_mat_NYU_2=as.data.frame(H2H_mat_NYU_2)
    rownames(H2H_mat_NYU_2)<-PR_Consideration
    colnames(H2H_mat_NYU_2)<-elo$ratings$Player
    
    library(writexl)
    sheets <- list("Elo" = elo$rating, 
                   "PR Consideration" = as.data.frame(H2H_mat_NYU),
                   "Top 20 H2H" =as.data.frame(H2H_mat_Top20),
                   "NYU VS Everyone" =as.data.frame(H2H_mat_NYU_2),
                   "History" = Match_Output,
                   "Participants" = Participants_Output
    ) 
    write_xlsx(sheets, "C:/Users/Raymond Luu/Desktop/NYU PR.xlsx")