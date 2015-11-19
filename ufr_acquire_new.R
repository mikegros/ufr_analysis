# File for building UFR Database

# Load relevant libraries
library(rvest)

#################################
# Get the urls for the existing UFRs

ufr_html  <- read_html("http://mgoblog.com/category/post-type/upon-further-review")
new_links <- ufr_html %>% html_nodes("a") %>% xml_attr("href")
new_links <- new_links[grepl("upon-further-review-20",new_links)]
new_links <- new_links[!grepl("#",new_links)]
links     <- c(links,new_links[1])

# Add new urls to csv file
write.table(new_links[1],file="ufr_urls.csv",append = TRUE,sep=",",col.names = FALSE)

#################################
# Initialize relevant parameters
o_position <- c("OL","RB","WR","TM")
d_position <- c("DL","LB","DB","TM")

for (i in (length(links)):length(links)) {
  opp  <- strsplit(links[i],split = "-vs-")[[1]][2]
  year <- as.numeric(strsplit(links[i],split = "-")[[1]][4])
  
  if(grepl("defense",links[i])){
    
    ###################
    # DEFENSE 
    
    ufr_html <- read_html(paste("http://mgoblog.com",links[i],sep=""))
    
    ii <- 2
    ufr_D <- ufr_html %>% 
      html_nodes("table") %>% 
      .[[ii]] %>% 
      html_table(fill=TRUE,header=FALSE)
    
    while(ufr_D[1,1] != "Defensive Line"){
      ii = ii+1
      ufr_D <- ufr_html %>% 
        html_nodes("table") %>% 
        .[[ii]] %>% 
        html_table(fill=TRUE,header=FALSE)
      if(ii == 100) {
        print('Defensive Table Not Found!')
        next
      }
    }
    
    ref_inds <- c(which(ufr_D[,1]=="Defensive Line"),
                  which(ufr_D[,1]=="Linebacker"),
                  which(ufr_D[,1]=="Secondary"),
                  which(ufr_D[,1]=="Metrics"))
    
    for (ii in 1:3){
      tmp <- ufr_D[(ref_inds[ii]+2):(ref_inds[ii+1]-1),1:4]
      tmp[,2:4] <- sapply(tmp[,2:4],as.numeric)
      tmp[,2:4] <- sapply(tmp[,2:4],function(x){x[is.na(x)] <- 0;return(x)})
      tmp$position <- d_position[ii]
      tmp$opponent <- opp
      tmp$year <- year
      new_inds <- (nrow(ufr_D_db)+1):(nrow(ufr_D_db)+nrow(tmp))
      rownames(tmp) <- new_inds
      ufr_D_db[new_inds,] <- tmp
    }
    
    # Get Team Metrics
    tmp <- ufr_D[(ref_inds[4]+1):nrow(ufr_D),1:4]
    tmp[,2:4] <- sapply(tmp[,2:4],as.numeric)
    tmp[,2:4] <- sapply(tmp[,2:4],function(x){x[is.na(x)] <- 0;return(x)})
    tmp$position <- d_position[4]
    tmp$opponent <- opp
    tmp$year <- year
    new_inds <- (nrow(ufr_D_db)+1):(nrow(ufr_D_db)+nrow(tmp))
    rownames(tmp) <- new_inds
    ufr_D_db[new_inds,] <- tmp
    
  } else {
    
    ####################
    # OFFENSE
    
    ufr_html <- read_html(paste("http://mgoblog.com",links[i],sep=""))
    
    ufr_tables <- ufr_html %>% 
      html_nodes("table")
    
    for (ii in 2:length(ufr_tables)){
      tmp <- ufr_tables[[ii]] %>% 
        html_table(fill=TRUE,header=FALSE)
      var_name <- ifelse(tmp[1,1] == "Offensive Line","ufr_oline",
                         ifelse(tmp[1,1] == "Player","ufr_wr","ufr_qb"))
      assign(var_name,tmp)
    }
    
    rm(ufr_tables)
    
    ref_inds <- c(which(ufr_oline[,1]=="Offensive Line"),
                  which(ufr_oline[,1]=="Backs"),
                  which(ufr_oline[,1]=="Receivers" | ufr_oline[,1]=="Receiver"),
                  which(ufr_oline[,1]=="Metrics"))
    
    for (ii in 1:3){
      tmp <- ufr_oline[(ref_inds[ii]+2):(ref_inds[ii+1]-1),1:4]
      tmp[,2:4] <- sapply(tmp[,2:4],as.numeric)
      tmp[,2:4] <- sapply(tmp[,2:4],function(x){x[is.na(x)] <- 0;return(x)})
      tmp$position <- o_position[ii]
      tmp$opponent <- opp
      tmp$year <- year
      new_inds <- (nrow(ufr_O_db)+1):(nrow(ufr_O_db)+nrow(tmp))
      rownames(tmp) <- new_inds
      ufr_O_db[new_inds,] <- tmp
    }
    
    # Get Team Metrics
    tmp <- ufr_oline[(ref_inds[4]+2):nrow(ufr_oline),1:4]
    tmp[,2:4] <- sapply(tmp[,2:4],as.numeric)
    tmp[,2:4] <- sapply(tmp[,2:4],function(x){x[is.na(x)] <- 0;return(x)})
    tmp$position <- o_position[4]
    tmp$opponent <- opp
    tmp$year <- year
    new_inds <- (nrow(ufr_O_db)+1):(nrow(ufr_O_db)+nrow(tmp))
    rownames(tmp) <- new_inds
    ufr_O_db[new_inds,] <- tmp
    
  }
  
}

#################################
# Some cleaning before saving

# Consistent naming for MSU
ufr_O_db$opponent[ufr_O_db$opponent=="michigan-state"] <- "msu"
ufr_D_db$opponent[ufr_D_db$opponent=="michigan-state"] <- "msu"

# Fix names or inconsistent name conventions
ufr_D_db$name[ufr_D_db$name=="C.Gordon"] <- "C. Gordon"
ufr_D_db$name[ufr_D_db$name=="Holowell"] <- "Hollowell"
ufr_D_db$name[ufr_D_db$name=="VanBergen"] <- "Van Bergen"
ufr_O_db$name[ufr_O_db$name=="Reyonlds"] <- "Reynolds"
ufr_O_db$name[ufr_O_db$name=="Magunson"] <- "Magnuson"
ufr_O_db$name[ufr_O_db$name=="TRobinson"] <- "T. Robinson"
ufr_O_db$name[ufr_O_db$name=="JRobinson"] <- "J. Robinson"
ufr_O_db$name[ufr_O_db$name=="Smith" & ufr_O_db$year < 2013] <- "V. Smith"
ufr_O_db$name[ufr_O_db$name=="Smith" & ufr_O_db$year >= 2013] <- "D. Smith"
ufr_D_db$name[ufr_D_db$name=="Clark" & ufr_D_db$position == "DB"] <- "J. Clark"
ufr_D_db$name[ufr_D_db$name=="RJS"] <- "Jenkins-Stone"
ufr_D_db$name[ufr_D_db$name=="Graham" & ufr_D_db$year <  2007] <- "C. Graham"
ufr_D_db$name[ufr_D_db$name=="Graham" & ufr_D_db$year >= 2007] <- "B. Graham"
ufr_D_db$name[ufr_D_db$name=="Taylor" & ufr_D_db$year <  2010] <- "T. Taylor"
ufr_D_db$name[ufr_D_db$name=="Taylor" & ufr_D_db$year >= 2010] <- "R. Taylor"
ufr_D_db$name[ufr_D_db$name=="Johnson" & ufr_D_db$year <  2010] <- "W. Johnson"
ufr_D_db$name[ufr_D_db$name=="Johnson" & ufr_D_db$year >= 2010] <- "C. Johnson"
ufr_D_db$name[ufr_D_db$name=="Watson" & ufr_D_db$year <  2008] <- "G. Watson"
ufr_D_db$name[ufr_D_db$name=="Watson" & ufr_D_db$year >= 2008] <- "S. Watson"
ufr_O_db$name[ufr_O_db$name=="Cole" & ufr_O_db$position == "OL"] <- "M. Cole"
ufr_O_db$name[ufr_O_db$name=="Cole" & ufr_O_db$position == "WR"] <- "B. Cole"

# Add some player positions"
ufr_D_db$position[ufr_D_db$name=="B. Graham"] <- "DL"
ufr_D_db$position[ufr_D_db$name=="Van Bergen"] <- "DL"
ufr_D_db$position[ufr_D_db$name=="Crable"] <- "LB"
ufr_D_db$position[ufr_D_db$name=="Harris"] <- "LB"
ufr_D_db$position[ufr_D_db$name=="Branch"] <- "DL"
ufr_D_db$position[ufr_D_db$name=="Woodley"] <- "DL"
ufr_D_db$position[ufr_D_db$name=="Biggs"] <- "DL"
ufr_D_db$position[ufr_D_db$name=="T. Taylor"] <- "DL"
ufr_D_db$position[ufr_D_db$name=="W. Johnson"] <- "DL"
ufr_D_db$position[ufr_D_db$name=="Germany"] <- "DL"
ufr_D_db$position[ufr_D_db$name=="Jamison"] <- "DL"
ufr_D_db$position[ufr_D_db$name=="C. Graham"] <- "LB"
ufr_D_db$position[ufr_D_db$name=="Burgess"] <- "LB"
ufr_D_db$position[ufr_D_db$name=="Hall"] <- "DB"
ufr_D_db$position[ufr_D_db$name=="Harrison"] <- "DB"
ufr_D_db$position[ufr_D_db$name=="Stewart"] <- "DB"
ufr_D_db$position[ufr_D_db$name=="Sears"] <- "DB"
ufr_D_db$position[ufr_D_db$name=="Adams"] <- "DB"
ufr_D_db$position[ufr_D_db$name=="Engelmon"] <- "DB"
ufr_D_db$position[ufr_D_db$name=="Barringer"] <- "DB"
ufr_D_db$position[ufr_D_db$name=="Mundy"] <- "DB"
ufr_D_db$position[ufr_D_db$name=="Trent"] <- "DB"
ufr_D_db$position[ufr_D_db$name=="Mouton"] <- "LB"
ufr_D_db$position[ufr_D_db$name=="Martin"] <- "DL"
ufr_D_db$position[ufr_D_db$name=="Warren"] <- "DB"
ufr_D_db$position[ufr_D_db$name=="Brown" & ufr_D_db$year <2009] <- "DB"
ufr_D_db$position[ufr_D_db$name=="Ezeh"] <- "LB"
ufr_D_db$position[ufr_D_db$name=="Thompson"] <- "LB"
ufr_D_db$position[ufr_D_db$name=="Woods"] <- "LB"
ufr_D_db$position[ufr_D_db$name=="Van Alstyne"] <- "DL"
ufr_D_db$position[ufr_D_db$name=="Massey"] <- "DL"
ufr_D_db$position[ufr_D_db$name=="G. Watson"] <- "DL"
ufr_D_db$position[ufr_D_db$name=="Mason"] <- "DB"
ufr_D_db$position[ufr_D_db$name=="Gallimore"] <- "DL"
ufr_D_db$position[ufr_D_db$name=="Ferrara"] <- "DL"
ufr_D_db$position[ufr_D_db$name=="Patterson"] <- "DL"
ufr_D_db$position[ufr_D_db$name=="Slocum"] <- "DL"
ufr_D_db$position[ufr_D_db$name=="Banks"] <- "DL"
ufr_D_db$position[ufr_D_db$name=="Logan"] <- "LB"
ufr_D_db$position[ufr_D_db$name=="Van Bergen"] <- "DL"
ufr_D_db$position[ufr_D_db$name=="Panter"] <- "LB"
ufr_D_db$position[ufr_D_db$name=="Evans"] <- "LB"
ufr_D_db$position[ufr_D_db$name=="Chambers"] <- "DB"
ufr_D_db$position[ufr_D_db$name=="Williams"] <- "DB"

ufr_O_db$position[ufr_O_db$name=="Arrington"] <- "WR"
ufr_O_db$position[ufr_O_db$name=="Hart"] <- "RB"
ufr_O_db$position[ufr_O_db$name=="Grady"] <- "RB"
ufr_O_db$position[ufr_O_db$name=="Minor"] <- "RB"
ufr_O_db$position[ufr_O_db$name=="Long"] <- "OL"
ufr_O_db$position[ufr_O_db$name=="Manningham"] <- "WR"
ufr_O_db$position[ufr_O_db$name=="Breaston"] <- "WR"
ufr_O_db$position[ufr_O_db$name=="Ecker"] <- "OL"
ufr_O_db$position[ufr_O_db$name=="Riley"] <- "OL"
ufr_O_db$position[ufr_O_db$name=="Bihl"] <- "OL"
ufr_O_db$position[ufr_O_db$name=="Kraus"] <- "OL"
ufr_O_db$position[ufr_O_db$name=="Mitchell"] <- "OL"
ufr_O_db$position[ufr_O_db$name=="Butler"] <- "OL"
ufr_O_db$position[ufr_O_db$name=="Massey"] <- "OL"
ufr_O_db$position[ufr_O_db$name=="Thompson"] <- "OL"
ufr_O_db$position[ufr_O_db$name=="Oluigbo"] <- "RB"


# Fix any issues with total not being plus - minus
ufr_O_db$total <- ufr_O_db$plus - ufr_O_db$minus
ufr_D_db$total <- ufr_D_db$plus - ufr_D_db$minus

# save(ufr_D_db,ufr_O_db,links,file="ufr_data.RData")
