############################################
# Script to simply play with plots
#     for exploration sake

player_plus_minus <- function(player,db){
  ####
  # Function for individual player plots
  maize_and_blue <- c("#00274C","#FFCB05")
  palette(maize_and_blue)
  
  tmp_db <- subset(db,name==player)
  if(nrow(tmp_db)==0) stop("Name Not Found")
  
  bg_color <- rgb(0,0,0,0.25)
  
  layout(matrix(c(1,3,2,4),2,2))
  par(mar=c(8,4,2,2))
  par(bg=rgb(0.95,0.95,0.95,1.0))
  plot(0:1,0:1,type="n",axes = F,xlab="",ylab="")
  text(0.5,0.5,player,cex=2)
  
  plot(tmp_db$total, 
       type="n", ylab="Total", xlab="", main = "",xaxt="n",font=2,font.lab=2, ylim=range(c(tmp_db$total,0)),
       cex.lab=1.4,cex.axis=1.4)
  polygon(c(-100,-100,100,100),c(-100,100,100,-100),col=bg_color)
  axis(1, at=1:nrow(tmp_db), labels=paste(tmp_db$year,tmp_db$opponent,sep = "-"),las=2)
  lines(tmp_db$total,
        type="h",lwd=4,
        col=tmp_db$year)
  points(tmp_db$total,col=tmp_db$year,pch=19,cex=1.5)
  
  plot(tmp_db$plus, 
       ylab="Plus", xlab="", main = "",xaxt="n",type="n",font=2,font.lab=2,ylim=c(0,1.05*max(tmp_db$plus)),
       cex.lab=1.4,cex.axis=1.4)
  polygon(c(-100,-100,100,100),c(-100,100,100,-100),col=bg_color)
  axis(1, at=1:nrow(tmp_db), labels=paste(tmp_db$year,tmp_db$opponent,sep = "-"),las=2)
  lines(tmp_db$plus,
        type="h",lwd=4,
        col=tmp_db$year)
  points(tmp_db$plus,col=tmp_db$year,pch=19,cex=1.5)
  
  plot(-1*tmp_db$minus,
       ylab="Minus", xlab="", main = "",xaxt="n",type="n",font=2,font.lab=2,ylim=c(-1.05*max(tmp_db$minus),0),
       cex.lab=1.4,cex.axis=1.4)
  polygon(c(-100,-100,100,100),c(-100,100,100,-100),col=bg_color)
  axis(1, at=1:nrow(tmp_db), labels=paste(tmp_db$year,tmp_db$opponent,sep = "-"),las=2)
  lines(-1*tmp_db$minus,
        type="h",lwd=4,
        col=tmp_db$year)
  points(-1*tmp_db$minus,col=tmp_db$year,pch=19,cex=1.5)
  
  invisible()
}

compare_players <- function(player1,player2,db){
  ####
  # Function for individual player plots
  maize_and_blue <- c("#00274C","#FFCB05")
  palette(maize_and_blue)
  
  tmp_db1 <- subset(db,name==player1)
  tmp_db2 <- subset(db,name==player2)
  if(nrow(tmp_db1)==0) stop("Name 1 Not Found")
  if(nrow(tmp_db2)==0) stop("Name 2 Not Found")
  
  bg_color <- rgb(0,0,0,0.25)
  
  par(mfrow=c(1,2))
  par(mar=c(8,4,4,2))
  par(bg=rgb(0.95,0.95,0.95,1.0))
  
  plot(tmp_db1$total, 
       type="n", ylab="Total", xlab="", main = player1,xaxt="n",
       font=2,font.lab=2, ylim=range(c(tmp_db1$total,tmp_db2$total,0)), cex.lab=1.4, cex.axis=1.4, cex.main=1.5)
  polygon(c(-100,-100,100,100),c(-100,100,100,-100),col=bg_color)
  axis(1, at=1:nrow(tmp_db1), labels=paste(tmp_db1$year,tmp_db1$opponent,sep = "-"),las=2)
  lines(tmp_db1$total,
        type="h",lwd=4,
        col=tmp_db1$year)
  points(tmp_db1$total,col=tmp_db1$year,pch=19,cex=1.5)
  
  plot(tmp_db2$total, 
       type="n", ylab="Total", xlab="", main = player2,xaxt="n",
       font=2,font.lab=2, ylim=range(c(tmp_db1$total,tmp_db2$total,0)), cex.lab=1.4, cex.axis=1.4, cex.main=1.5)
  polygon(c(-100,-100,100,100),c(-100,100,100,-100),col=bg_color)
  axis(1, at=1:nrow(tmp_db2), labels=paste(tmp_db2$year,tmp_db2$opponent,sep = "-"),las=2)
  lines(tmp_db2$total,
        type="h",lwd=4,
        col=tmp_db2$year)
  points(tmp_db2$total,col=tmp_db2$year,pch=19,cex=1.5)
  
  invisible()
}

position_plus_minus <- function(pos,db){
  ####
  # Function for individual player plots
  maize_and_blue <- c("#00274C","#FFCB05")
  palette(maize_and_blue)
  
  tmp_db <- subset(db,name=="TOTAL" & position==pos)
  if(nrow(tmp_db)==0) stop("Position Not Found")
  
  bg_color <- rgb(0,0,0,0.25)
  
  layout(matrix(c(1,3,2,4),2,2))
  par(mar=c(8,4,2,2))
  par(bg=rgb(0.95,0.95,0.95,1.0))
  plot(0:1,0:1,type="n",axes = F,xlab="",ylab="")
  text(0.5,0.5,paste(pos," Totals"),cex=2)
  
  plot(tmp_db$total, 
       type="n", ylab="Total", xlab="", main = "",xaxt="n",font=2,font.lab=2, ylim=range(c(tmp_db$total,0)),
       cex.lab=1.4, cex.axis=1.4)
  polygon(c(-100,-100,100,100),c(-100,100,100,-100),col=bg_color)
  axis(1, at=1:nrow(tmp_db), labels=paste(tmp_db$year,tmp_db$opponent,sep = "-"),las=2)
  lines(tmp_db$total,
        type="h",lwd=4,
        col=tmp_db$year)
  points(tmp_db$total,col=tmp_db$year,pch=19,cex=1.5)
  
  plot(tmp_db$plus, 
       ylab="Plus", xlab="", main = "",xaxt="n",type="n",font=2,font.lab=2,ylim=c(0,1.05*max(tmp_db$plus)),
       cex.lab=1.4, cex.axis=1.4)
  polygon(c(-100,-100,100,100),c(-100,100,100,-100),col=bg_color)
  axis(1, at=1:nrow(tmp_db), labels=paste(tmp_db$year,tmp_db$opponent,sep = "-"),las=2)
  lines(tmp_db$plus,
        type="h",lwd=4,
        col=tmp_db$year)
  points(tmp_db$plus,col=tmp_db$year,pch=19,cex=1.5)
  
  plot(-1*tmp_db$minus,
       ylab="Minus", xlab="", main = "",xaxt="n",type="n",font=2,font.lab=2,ylim=c(-1.05*max(tmp_db$minus),0),
       cex.lab=1.4, cex.axis=1.4)
  polygon(c(-100,-100,100,100),c(-100,100,100,-100),col=bg_color)
  axis(1, at=1:nrow(tmp_db), labels=paste(tmp_db$year,tmp_db$opponent,sep = "-"),las=2)
  lines(-1*tmp_db$minus,
        type="h",lwd=4,
        col=tmp_db$year)
  points(-1*tmp_db$minus,col=tmp_db$year,pch=19,cex=1.5)
  
  invisible()
}

###########################
# Function for finding best and worst at a position

summary_position <- function(pos,db){
  sub_db <- subset(db,position==pos & name != "TOTAL")
  cat("Mean total for ",pos," is: ",mean(sub_db$total),"\n")
  cat("Mean number of + for ",pos," is: ",mean(sub_db$plus),"\n")
  cat("Mean number of - for ",pos," is: ",mean(sub_db$minus),"\n")
  tmp <- sub_db[which.max(sub_db$plus),]
  cat(pos," with the most single game + is: ",tmp$name," with ",tmp$plus,
      " in ",tmp$year," against ",tmp$opponent,"\n")
  tmp <- sub_db[which.max(sub_db$minus),]
  cat(pos," with the most single game - is: ",tmp$name," with ",-1*tmp$minus,
      " in ",tmp$year," against ",tmp$opponent,"\n")
  tmp <- sub_db[which.max(sub_db$total),]
  cat(pos," with the best single game total is: ",tmp$name," with ",tmp$total,
      " in ",tmp$year," against ",tmp$opponent,"\n")
  tmp <- sub_db[which.min(sub_db$total),]
  cat(pos," with the worst single game total is: ",tmp$name," with ",tmp$total,
      " in ",tmp$year," against ",tmp$opponent,"\n")
  
  tmp_season <- aggregate(total~name+year,subset(sub_db,position != "TM"),sum)
  tmp <- tmp_season[which.max(tmp_season$total),]
  cat("Player with the best single season total is: ",tmp$name," with ",tmp$total,
      " in ",tmp$year,"\n")
  tmp <- tmp_season[which.min(tmp_season$total),]
  cat("Player with the worst single season total is: ",tmp$name," with ",tmp$total,
      " in ",tmp$year,"\n")
  
  tmp_career <- aggregate(total~name,subset(sub_db,position != "TM"),sum)
  tmp <- tmp_career[which.max(tmp_career$total),]
  cat("Player with the best career total is: ",tmp$name," with ",tmp$total,"\n")
  tmp <- tmp_career[which.min(tmp_career$total),]
  cat("Player with the worst career total is: ",tmp$name," with ",tmp$total,"\n")
  
  invisible()
}

###########################
# Top N at a position by totals

topN_position <- function(pos,n,db,type="career",bottom=FALSE){
  sub_db <- subset(db,position==pos & name != "TOTAL")
  if (type=="game"){
    
    ordered_inds <- order(sub_db$total,decreasing = !bottom)
    tmp <- sub_db[ordered_inds[1:n],]
    cat("The ", type," top ", n," ", pos," are: \n",sep="")
    for (i in 1:n) cat(i, ") ",tmp$name[i]," with ",tmp$total[i]," in ", tmp$year[i]," against ",tmp$opponent[i],"\n",sep="")
    
  } else if (type=="season"){
    
    tmp_season <- aggregate(total~name+year,subset(sub_db,position != "TM"),sum)
    ordered_inds <- order(tmp_season$total,decreasing = !bottom)
    tmp <- tmp_season[ordered_inds[1:n],]
    cat("The ", type," top ", n," ", pos," are: \n",sep="")
    for (i in 1:n) cat(i, ") ",tmp$name[i]," with ",tmp$total[i]," in ", tmp$year[i],"\n",sep="")
    
  } else if (type=="career"){
    
    tmp_career <- aggregate(total~name,subset(sub_db,position != "TM"),sum)
    ordered_inds <- order(tmp_career$total,decreasing = !bottom)
    tmp <- tmp_career[ordered_inds[1:n],]
    cat("The ", type," top ", n," ", pos," are: \n",sep="")
    for (i in 1:n) cat(i, ") ",tmp$name[i]," with ",tmp$total[i],"\n",sep = "")
    
  }
  invisible()
}

topN_df <- function(pos,n,db,type="career",bottom=FALSE){
  sub_db <- subset(db,position==pos & name != "TOTAL")
  if (type=="game"){
    
    ordered_inds <- order(sub_db$total,decreasing = !bottom)
    tmp <- sub_db[ordered_inds[1:n],c(1,4,6,7)]
    
  } else if (type=="season"){
    
    tmp_season <- aggregate(total~name+year,subset(sub_db,position != "TM"),sum)
    ordered_inds <- order(tmp_season$total,decreasing = !bottom)
    tmp <- tmp_season[ordered_inds[1:n],]
    
  } else if (type=="career"){
    
    tmp_career <- aggregate(total~name,subset(sub_db,position != "TM"),sum)
    ordered_inds <- order(tmp_career$total,decreasing = !bottom)
    tmp <- tmp_career[ordered_inds[1:n],]
    
  }
  tmp
}

aggregate_df <- function(pos,db,type="career"){
  sub_db <- subset(db,position==pos & name != "TOTAL")
  if (type=="game"){
    
    tmp <- sub_db[,c(1,4,6,7)]
    
  } else if (type=="season"){
    
    tmp <- aggregate(total~name+year,subset(sub_db,position != "TM"),sum)
    
  } else if (type=="career"){
    
    tmp <- aggregate(total~name,subset(sub_db,position != "TM"),sum)
    
  }
  tmp
}


#########################################################
# Functions specifically for Shiny app
#
position_plus_minus_single <- function(pos,db,num){
  ####
  # Function for individual player plots
  maize_and_blue <- c("#00274C","#FFCB05")
  palette(maize_and_blue)
  
  tmp_db <- subset(db,name=="TOTAL" & position==pos)
  if(nrow(tmp_db)==0) stop("Position Not Found")
  
  bg_color <- rgb(0,0,0,0.25)
  
  par(bg=rgb(0.95,0.95,0.95,1.0))
  
  if (num == 1){
    
    par(mar=c(8,4,4,2))
    plot(tmp_db$total, 
         type="n", ylab="Total", xlab="", main = pos,xaxt="n",font=2,font.lab=2, ylim=range(c(tmp_db$total,0)),
         cex.lab=1.4, cex.axis=1.4, cex.main=1.4)
    polygon(c(-100,-100,10000,10000),c(-100,10000,10000,-100),col=bg_color)
    axis(1, at=1:nrow(tmp_db), labels=paste(tmp_db$year,tmp_db$opponent,sep = "-"),las=2)
    lines(tmp_db$total,
          type="h",lwd=4,
          col=tmp_db$year)
    points(tmp_db$total,col=tmp_db$year,pch=19,cex=1.5)
  } else if( num == 2){
    
    par(mar=c(8,4,2,2))
    plot(tmp_db$plus, 
         ylab="Plus", xlab="", main = "",xaxt="n",type="n",font=2,font.lab=2,ylim=c(0,1.05*max(tmp_db$plus)),
         cex.lab=1.4, cex.axis=1.4)
    polygon(c(-100,-100,10000,10000),c(-100,10000,10000,-100),col=bg_color)
    axis(1, at=1:nrow(tmp_db), labels=paste(tmp_db$year,tmp_db$opponent,sep = "-"),las=2)
    lines(tmp_db$plus,
          type="h",lwd=4,
          col=tmp_db$year)
    points(tmp_db$plus,col=tmp_db$year,pch=19,cex=1.5)
  } else {
    
    par(mar=c(8,4,2,2))
    plot(-1*tmp_db$minus,
         ylab="Minus", xlab="", main = "",xaxt="n",type="n",font=2,font.lab=2,ylim=c(-1.05*max(tmp_db$minus),0),
         cex.lab=1.4, cex.axis=1.4)
    polygon(c(-100,-10000,10000,100),c(-100,10000,10000,-100),col=bg_color)
    axis(1, at=1:nrow(tmp_db), labels=paste(tmp_db$year,tmp_db$opponent,sep = "-"),las=2)
    lines(-1*tmp_db$minus,
          type="h",lwd=4,
          col=tmp_db$year)
    points(-1*tmp_db$minus,col=tmp_db$year,pch=19,cex=1.5)
    
  }
  
  invisible()
}

player_plus_minus_single <- function(player,db,num=1){
  ####
  # Function for individual player plots
  maize_and_blue <- c("#00274C","#FFCB05")
  palette(maize_and_blue)
  
  tmp_db <- subset(db,name==player)
  if(nrow(tmp_db)==0) stop("Name Not Found")
  
  bg_color <- rgb(0,0,0,0.25)
  
  par(bg=rgb(0.95,0.95,0.95,1.0))
  
  if ( num == 1 ){
    
    par(mar=c(8,4,4,2))
    plot(tmp_db$total, 
         type="n", ylab="Total", xlab="", main = player,xaxt="n",font=2,font.lab=2, ylim=range(c(tmp_db$total,0)),
         cex.lab=1.4,cex.axis=1.4,cex.main=1.4)
    polygon(c(-100,-100,10000,10000),c(-100,10000,10000,-100),col=bg_color)
    axis(1, at=1:nrow(tmp_db), labels=paste(tmp_db$year,tmp_db$opponent,sep = "-"),las=2)
    lines(tmp_db$total,
          type="h",lwd=4,
          col=tmp_db$year)
    points(tmp_db$total,col=tmp_db$year,pch=19,cex=1.5)
    
  } else if ( num == 2 ){
    
    par(mar=c(8,4,2,2))
    plot(tmp_db$plus, 
         ylab="Plus", xlab="", main = "",xaxt="n",type="n",font=2,font.lab=2,ylim=c(0,1.05*max(tmp_db$plus)),
         cex.lab=1.4,cex.axis=1.4)
    polygon(c(-100,-100,10000,10000),c(-100,10000,10000,-100),col=bg_color)
    axis(1, at=1:nrow(tmp_db), labels=paste(tmp_db$year,tmp_db$opponent,sep = "-"),las=2)
    lines(tmp_db$plus,
          type="h",lwd=4,
          col=tmp_db$year)
    points(tmp_db$plus,col=tmp_db$year,pch=19,cex=1.5)
    
  } else {
    
    par(mar=c(8,4,2,2))
    plot(-1*tmp_db$minus,
         ylab="Minus", xlab="", main = "",xaxt="n",type="n",font=2,font.lab=2,ylim=c(-1.05*max(tmp_db$minus),0),
         cex.lab=1.4,cex.axis=1.4)
    polygon(c(-100,-100,10000,10000),c(-100,10000,10000,-100),col=bg_color)
    axis(1, at=1:nrow(tmp_db), labels=paste(tmp_db$year,tmp_db$opponent,sep = "-"),las=2)
    lines(-1*tmp_db$minus,
          type="h",lwd=4,
          col=tmp_db$year)
    points(-1*tmp_db$minus,col=tmp_db$year,pch=19,cex=1.5)
    
  }
  invisible()
}
