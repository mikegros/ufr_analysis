
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)

download.file("http://www-personal.umich.edu/~mikegros/ufr_data.RData",destfile = "ufr_data.RData")
load(file="./ufr_data.RData")

condition1 <- '!(input.o_or_d == "offense" && input.o_type == "table")'
condition2 <- '!(input.o_or_d == "defense" && input.d_type == "table")'
condition3 <- '!(input.o_or_d == "offense" && input.o_type == "two_players")'
condition4 <- '!(input.o_or_d == "defense" && input.d_type == "two_players")'


shinyUI(fluidPage(
  headerPanel("MGoBlog Upon Further Review (UFR) Visualization"),
  br(),
  
  # Application title
  fluidRow(
    navlistPanel(
      tabPanel("Offense", value = "offense",
               navlistPanel(
                 tabPanel("Position", value = "position",
                          column(3,
                                 flowLayout(
                                   selectInput("o_pos","Position",
                                               c("OL","RB","WR")),
                                   checkboxInput("o_agg_pos","Aggregate By Year?")
                                 )
                          )
                 ),
                 
                 tabPanel("Single Player", value = "player",
                          navlistPanel(
                            tabPanel("OL", value = "OL",
                                     column(3,
                                            flowLayout(
                                              selectInput("o_pname_ol","Player",
                                                          sort(unique(subset(ufr_O_db,position == "OL" & name != "TOTAL")$name)),selected="M. Cole"),
                                              br(),
                                              checkboxInput("o_agg_player_ol","Aggregate By Year?")
                                            )
                                     )
                            ),
                            tabPanel("RB", value = "RB",
                                     column(3,
                                            flowLayout(
                                              selectInput("o_pname_rb","Player",
                                                          sort(unique(subset(ufr_O_db,position == "RB" & name != "TOTAL")$name)),selected="D. Smith"),
                                              br(),
                                              checkboxInput("o_agg_player_rb","Aggregate By Year?")
                                            )
                                     )
                            ),
                            tabPanel("WR", value = "WR",
                                     column(3,
                                            flowLayout(
                                              selectInput("o_pname_wr","Player",
                                                          sort(unique(subset(ufr_O_db,position == "WR" & name != "TOTAL")$name)),selected="Chesson"),
                                              br(),
                                              checkboxInput("o_agg_player_wr","Aggregate By Year?")
                                            )
                                     )
                            ),
                            tabPanel("Team", value = "TM",
                                     column(3,
                                            flowLayout(
                                              selectInput("o_pname_tm","Player",
                                                          sort(unique(subset(ufr_O_db,position == "TM")$name)),selected="RPS"),
                                              br(),
                                              checkboxInput("o_agg_player_tm","Aggregate By Year?")
                                            )
                                     )
                            ),id="o_pos_single"
                          )
                          
                 ),
                 
                 tabPanel("Compare Players", value = "two_players",
                          flowLayout(
                            selectInput("o_pname1","Player 1",
                                        sort(unique(ufr_O_db$name)),selected="Lewan"),
                            selectInput("o_pname2","Player 2",
                                        sort(unique(ufr_O_db$name)),selected="M. Cole"),
                            checkboxInput("o_agg_players","Aggregate By Year?")
                          )
                          
                 ),
                 
#                  tabPanel("Top/Bottom 10", value = "top10",
#                           flowLayout(
#                             selectInput("top10_o_pos","Position",
#                                         c("OL","RB","WR")),
#                             selectInput("top10_o_type","Type",
#                                         c("career","season","game"))
#                           )
#                           
#                  ),
                 
                 tabPanel("Sortable Table", value = "table",
                          flowLayout(
                            selectInput("top_o_pos","Position",
                                        c("OL","RB","WR")),
                            selectInput("top_o_type","Type",
                                        c("career","season","game"))
                          )
                          
                 ),id="o_type"
               )
      ),
      tabPanel("Defense", value = "defense",
               navlistPanel(
                 tabPanel("Position", value = "position",
                          column(3,
                                 flowLayout(
                                   selectInput("d_pos","Position",
                                               c("DL","LB","DB")),
                                   checkboxInput("d_agg_pos","Aggregate By Year?")
                                 )
                          )
                 ),
                 
                 tabPanel("Single Player", value = "player",
                          navlistPanel(
                            tabPanel("DL", value = "DL",
                                     column(3,
                                            flowLayout(
                                              selectInput("d_pname_dl","Player",
                                                          sort(unique(subset(ufr_D_db,position == "DL" & name != "TOTAL")$name)),selected="Glasgow"),
                                              br(),
                                              checkboxInput("d_agg_player_dl","Aggregate By Year?")
                                            )
                                     )
                            ),
                            tabPanel("LB", value = "LB",
                                     column(3,
                                            flowLayout(
                                              selectInput("d_pname_lb","Player",
                                                          sort(unique(subset(ufr_D_db,position == "LB" & name != "TOTAL")$name)),selected="Morgan"),
                                              br(),
                                              checkboxInput("d_agg_player_lb","Aggregate By Year?")
                                            )
                                     )
                            ),
                            tabPanel("DB", value = "DB",
                                     column(3,
                                            flowLayout(
                                              selectInput("d_pname_db","Player",
                                                          sort(unique(subset(ufr_D_db,position == "DB" & name != "TOTAL")$name)),selected="Lewis"),
                                              br(),
                                              checkboxInput("d_agg_player_db","Aggregate By Year?")
                                            )
                                     )
                            ),
                            tabPanel("Team", value = "TM",
                                     column(3,
                                            flowLayout(
                                              selectInput("d_pname_tm","Player",
                                                          sort(unique(subset(ufr_D_db,position == "TM" & name != "TOTAL")$name)),selected="RPS"),
                                              br(),
                                              checkboxInput("d_agg_player_tm","Aggregate By Year?")
                                            )
                                     )
                            ),id="d_pos_single"
                          )
                          
                 ),
                 
                 tabPanel("Compare Players", value = "two_players",
                          flowLayout(
                            selectInput("d_pname1","Player 1",
                                        sort(unique(ufr_D_db$name)),selected = "B. Graham"),
                            selectInput("d_pname2","Player 2",
                                        sort(unique(ufr_D_db$name)),selected = "Glasgow"),
                            checkboxInput("d_agg_players","Aggregate By Year?")
                          )
                          
                 ),
                 
#                  tabPanel("Top/Bottom 10", value = "top10",
#                           flowLayout(
#                             selectInput("top10_d_pos","Position",
#                                         c("DL","LB","DB")),
#                             selectInput("top10_d_type","Type",
#                                         c("career","season","game"))
#                           )
#                           
#                  ),
                 
                 tabPanel("Sortable Table", value = "table",
                          flowLayout(
                            selectInput("top_d_pos","Position",
                                        c("DL","LB","DB")),
                            selectInput("top_d_type","Type",
                                        c("career","season","game"))
                          )
                          
                 ),id="d_type"
                 
               )
      ),id="o_or_d"
    )
  ),
  
  conditionalPanel(paste(condition1,condition2,sep=" && "),
                            fluidRow(plotOutput("player_plot"))
  ),
  conditionalPanel(paste(condition1,condition2,condition3,condition4,sep=" && "),
                   fluidRow(plotOutput("player_plot_single2"))
  ),
  conditionalPanel(paste(condition1,condition2,condition3,condition4,sep=" && "),
                   fluidRow(plotOutput("player_plot_single3"))
  ),
  conditionalPanel('input.o_or_d == "offense" && input.o_type == "table"',
                   fluidRow(dataTableOutput("data_table_O"))
  ),
  conditionalPanel('input.o_or_d == "defense" && input.d_type == "table"',
                   fluidRow(dataTableOutput("data_table_D"))
#   ),
#   conditionalPanel('input.o_or_d == "offense" && input.o_type == "top10"',
#                    flowLayout(
#                      textOutput("top10_O"),
#                      textOutput("bottom10_O")
#                      )
#   ),
#   conditionalPanel('input.o_or_d == "defense" && input.d_type == "top10"',
#                    flowLayout(
#                      textOutput("top10_D"),
#                      textOutput("bottom10_D")
#                    )
  )
  
))
