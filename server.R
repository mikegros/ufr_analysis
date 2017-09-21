
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)

#
# Check for new UFR data and if so, scrape it and add it to the database
#     would prefer it not HAVE to check every time, but for now this seems
#     like the best option for maintaining up-to-date data without 
#     manual updating. Will be a problem if noone checks for a long stretch
#     of UFR writeups
#
source("check_for_new_ufr.R",local=TRUE)

source("./ufr_plot_functions.R",local=TRUE)

plot_select <- function(input,num){
  
  if(input$o_or_d == "offense"){
    
    db <- ufr_O_db
    if (input$o_type=="position"){
      
      if (input$o_agg_pos) db <- aggregate(cbind(total,plus,minus) ~ year+position+name,db,mean)
      position_plus_minus_single(input$o_pos,db,num)
      
    } else if (input$o_type=="player"){
      
      if (input$o_pos_single == "OL"){
        if (input$o_agg_player_ol) db <- aggregate(cbind(total,plus,minus) ~ year+name,db,mean)
        player_plus_minus_single(input$o_pname_ol,db,num)
      } else if(input$o_pos_single == "RB"){
        if (input$o_agg_player_rb) db <- aggregate(cbind(total,plus,minus) ~ year+name,db,mean)
        player_plus_minus_single(input$o_pname_rb,db,num)
      } else if(input$o_pos_single == "WR"){
        if (input$o_agg_player_wr) db <- aggregate(cbind(total,plus,minus) ~ year+name,db,mean)
        player_plus_minus_single(input$o_pname_wr,db,num)
      } else {
        if (input$o_agg_player_tm) db <- aggregate(cbind(total,plus,minus) ~ year+name,db,mean)
        player_plus_minus_single(input$o_pname_tm,db,num)
      }
      
    } else {
      
      if (input$o_agg_players) db <- aggregate(cbind(total,plus,minus) ~ year+name,db,mean)
      compare_players(input$o_pname1,input$o_pname2,db)
      
    }
    
  } else {
    
    db <- ufr_D_db
    if (input$d_type=="position"){
      
      if (input$d_agg_pos) db <- aggregate(cbind(total,plus,minus) ~ year+position+name,db,mean)
      position_plus_minus_single(input$d_pos,db,num)
      
    } else if (input$d_type=="player"){
      
      if (input$d_pos_single == "DL"){
        if (input$d_agg_player_dl) db <- aggregate(cbind(total,plus,minus) ~ year+name,db,mean)
        player_plus_minus_single(input$d_pname_dl,db,num)
      } else if(input$d_pos_single == "LB"){
        if (input$d_agg_player_lb) db <- aggregate(cbind(total,plus,minus) ~ year+name,db,mean)
        player_plus_minus_single(input$d_pname_lb,db,num)
      } else if(input$d_pos_single == "DB"){
        if (input$d_agg_player_db) db <- aggregate(cbind(total,plus,minus) ~ year+name,db,mean)
        player_plus_minus_single(input$d_pname_db,db,num)
      } else {
        if (input$d_agg_player_tm) db <- aggregate(cbind(total,plus,minus) ~ year+name,db,mean)
        player_plus_minus_single(input$d_pname_tm,db,num)
      }
      
    } else {
      
      if (input$d_agg_players) db <- aggregate(cbind(total,plus,minus) ~ year+name,db,mean)
      compare_players(input$d_pname1,input$d_pname2,db)
      
    }
    
  }
  
}

shinyServer(function(input, output) {
  
  output$player_plot <- renderPlot({
    plot_select(input,1)
  })
  
  output$player_plot_single2 <- renderPlot({
    plot_select(input,2)
  })
  
  output$player_plot_single3 <- renderPlot({
    plot_select(input,3)
  })
  
  output$data_table_O <- renderDataTable({
    aggregate_df(input$top_o_pos,ufr_O_db,type = input$top_o_type)
  }, options = list(lengthMenu = c(5, 10, 25), pageLength = 10)
  )
  
  output$data_table_D <- renderDataTable({
    aggregate_df(input$top_d_pos,ufr_D_db,type = input$top_d_type)
  }, options = list(lengthMenu = c(5, 10, 25), pageLength = 10)
  )
  
  output$top10_O    <- renderPrint({
    topN_position(input$top10_o_pos,10,ufr_O_db,type = input$top10_o_type)
  }
  )
  
  output$top10_D    <- renderPrint({
    topN_position(input$top10_d_pos,10,ufr_D_db,type = input$top10_d_type)
  }
  )
  
  output$bottom10_O <- renderPrint({
    topN_position(input$top10_o_pos,10,ufr_O_db,type = input$top10_o_type, bottom = TRUE)
  }
  )
  
  output$bottom10_D <- renderPrint({
    topN_position(input$top10_d_pos,10,ufr_D_db,type = input$top10_d_type, bottom = TRUE)
  }
  )
})
