library(shiny)
library(shinydashboard)

shinyUI(
  dashboardPage(
    dashboardHeader(title = "PL Prediction"),
    dashboardSidebar(
      titlePanel("Factors"),
      sidebarMenu(
        menuItem("Home", tabName = "HOME"),
        menuItem("Goals", tabName = "gls"),
        menuItem("Clean Sheets", tabName = "cs"),
        
        #menuSubItem("Season 2016-2017"),
        #menuSubItem("Season 2017-2018"),
        #menuSubItem("Season 2018-2019"),
        menuItem("Shots on target", tabName = "st"),
        menuItem("Shots from inside box", tabName = "ib"),
        menuItem("Goals Conceded", tabName = "gc"),
        menuItem("Saves", tabName = "svs"),
        menuItem("Regression", tabName = "reg"),
        menuItem("Prediction", tabName = "final")
        
      )),
    dashboardBody(
      
      tabItems(
        
        tabItem(tabName = "HOME",
                fileInput("file1", "Choose CSV File",
                          accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
                tags$hr(),
                checkboxInput("header", "Header", TRUE),
                placeholder = "No file selected",
                plotOutput("plot")
                
        ),
        
        tabItem(tabName = "gls",
                h2("Analysis of Goals"),
                tabsetPanel(type = "tabs",
                            tabPanel("SCATTER PLOT", plotlyOutput("p0"),
                                     br(),
                                     fluidRow(
                                       column(
                                         width=12,
                                         box(
                                           width =12,
                                           background='lime',
                                           verbatimTextOutput('sct_goals_text'))
                                       )
                                     )
                                     ),
                            tabPanel(
                              title="BAR CHART",
                              fluidRow(
                                column(
                                  width=12,
                                  box(
                                    width = 6,
                                    background='olive',
                                    selectInput('goalazo', 'Team Names', ""))
                                )
                              ),
                              br(),
                              fluidRow(
                                column(
                                  width=12,
                                  box(
                                    width = 12,
                                    background='black',
                                    plotOutput('bar_goalazo'))
                                )
                              )
                              
                              ),
                            tabPanel("DONUT CHART",plotlyOutput("p02"),
                                     br(),
                                     fluidRow(
                                       column(
                                         width=12,
                                         box(
                                           width =12,
                                           background='lime',
                                           verbatimTextOutput('don_goals_text'))
                                       )
                                     )
                                    ),
                            tabPanel(title="Dot & Dumbell plot",
                                     fluidRow(
                                       column(
                                         width=12,
                                         box(
                                           width = 6,
                                           background='olive',
                                           selectInput('ddgoals', 'Seasons', ""))
                                       )
                                       ),
                                     br(),
                                     fluidRow(
                                       column(
                                         width=12,
                                         plotlyOutput('dd_goalazo')
                                        )
                                     ),
                                     br(),
                                     fluidRow(
                                       column(
                                         width=12,
                                         box(
                                           width =12,
                                            background='lime',
                                           verbatimTextOutput('dd_goals_text'))
                                       )
                                     )
                                     
                )
        )
        ),

        
#########################################CLEAN SHEETS#############################        
                
        tabItem(tabName = "cs",
                h2("Analysis of Clean Sheets"),
                tabsetPanel(type = "tabs",
                            tabPanel("SCATTER PLOT", plotlyOutput("p1"),
                                     br(),
                                     fluidRow(
                                       column(
                                         width=12,
                                         box(
                                           width =12,
                                           background='lime',
                                           verbatimTextOutput('sct_cs_text'))
                                            )
                                        )
                                     ),
                            tabPanel(
                              title="BAR CHART",
                              fluidRow(
                                column(
                                  width=12,
                                  box(
                                    width = 6,
                                    background='olive',
                                    selectInput('barcs', 'Team Names', ""))
                                )
                              ),
                              br(),
                              fluidRow(
                                column(
                                  width=12,
                                  box(
                                    width = 12,
                                    background='black',
                                    plotOutput('bar_cs'))
                                )
                              )
                              
                            ),
                            tabPanel("DONUT CHART",plotlyOutput("p12"),
                                     br(),
                                     fluidRow(
                                       column(
                                         width=12,
                                         box(
                                           width =12,
                                           background='lime',
                                           verbatimTextOutput('don_cs_text'))
                                       )
                                     )
                                     ),
                            tabPanel(title="DOT AND DUMBBELL PLOT",
                                     fluidRow(
                                       column(
                                         width=12,
                                         box(
                                           width = 6,
                                           background='olive',
                                           selectInput('ddcs', 'Seasons', ""))
                                       )
                                     ),
                                     br(),
                                     fluidRow(
                                       column(
                                         width=12,
                                         plotlyOutput('dd_cs')
                                        
                                       )
                                     ),
                                     br(),
                                     fluidRow(
                                       column(
                                         width=12,
                                         box(
                                           width =12,
                                           background='lime',
                                           verbatimTextOutput('dd_cs_text'))
                                       )
                                     )
                                     
                            )
                )
        ),


######################################### SHOTS ON TARGET######################

        
        tabItem(tabName = "st",
                h1("Analysis of Shots on Target"),
                tabsetPanel(type = "tabs",
                            # tabPanel("SCATTER PLOT", plotlyOutput("p2")),
                            # tabPanel("PIE CHART", plotlyOutput("p21")),
                            tabPanel(
                              title="BAR CHART",
                              fluidRow(
                                column(
                                  width=12,
                                  box(
                                    width = 6,
                                    background='olive',
                                    selectInput('barst', 'Team Names', ""))
                                )
                              ),
                              br(),
                              fluidRow(
                                column(
                                  width=12,
                                  box(
                                    width = 12,
                                    background='black',
                                    plotOutput('bar_st'))
                                )
                              )
                              
                            ),
                            tabPanel("DONUT CHART", plotlyOutput("p22"),
                                     br(),
                                     fluidRow(
                                       column(
                                         width=12,
                                         box(
                                           width =12,
                                           background='lime',
                                           verbatimTextOutput('don_st_text'))
                                          )
                                        )
                                     )
                )
        ),

################################################# INSIDE BOX ####################

        tabItem(tabName = "ib",
                h1("Analysis of Shots from Inside box"),
                tabsetPanel(type = "tabs",
                            # tabPanel("SCATTER PLOT", plotlyOutput("p3")),
                            # tabPanel("PIE CHART", plotlyOutput("p31")),
                            tabPanel(
                              title="BAR CHART",
                              fluidRow(
                                column(
                                  width=12,
                                  box(
                                    width = 6,
                                    background='olive',
                                    selectInput('barib', 'Team Names', ""))
                                )
                              ),
                              br(),
                              fluidRow(
                                column(
                                  width=12,
                                  box(
                                    width = 12,
                                    background='black',
                                    plotOutput('bar_ib'))
                                )
                              )
                              
                            ),
                            tabPanel("DONUT CHART", plotlyOutput("p32"),
                                     br(),
                                     fluidRow(
                                       column(
                                         width=12,
                                         box(
                                           width =12,
                                           background='lime',
                                           verbatimTextOutput('don_ib_text'))
                                       )
                                     )
                                     )
                )
        ),
 
###################################### GOALS CONCEDED ##########################

        tabItem(tabName = "gc",
                h1("Analysis of Gols Conceded"),
                tabsetPanel(type = "tabs",
                            tabPanel("SCATTER PLOT", plotlyOutput("p4"),
                                     br(),
                                     fluidRow(
                                       column(
                                         width=12,
                                         box(
                                           width =12,
                                           background='lime',
                                           verbatimTextOutput('sct_gc_text'))
                                       )
                                     )
                                     ),
                            tabPanel(
                              title="BAR CHART",
                              fluidRow(
                                column(
                                  width=12,
                                  box(
                                    width = 6,
                                    background='olive',
                                    selectInput('bargc', 'Team Names', ""))
                                )
                              ),
                              br(),
                              fluidRow(
                                column(
                                  width=12,
                                  box(
                                    width = 12,
                                    background='black',
                                    plotOutput('bar_gc'))
                                )
                              )
                              
                            ),
                            tabPanel("PIE CHART",plotlyOutput("p41"),
                                     br(),
                                     fluidRow(
                                       column(
                                         width=12,
                                         box(
                                           width =12,
                                           background='lime',
                                           verbatimTextOutput('pie_gc_text'))
                                       )
                                     )
                                     )
                )
        ),

############################################# SAVES ############################

        tabItem(tabName = "svs",
                h1("Analysis of Saves"),
                tabsetPanel(type = "tabs",
                            tabPanel("SCATTER PLOT", plotlyOutput("p5"),
                                     br(),
                                     fluidRow(
                                       column(
                                         width=12,
                                         box(
                                           width =12,
                                           background='lime',
                                           verbatimTextOutput('sct_svs_text'))
                                       )
                                     )
                                     ),
                            tabPanel(
                              title="BAR CHART",
                              fluidRow(
                                column(
                                  width=12,
                                  box(
                                    width = 6,
                                    background='olive',
                                    selectInput('barsvs', 'Team Names', ""))
                                )
                              ),
                              br(),
                              fluidRow(
                                column(
                                  width=12,
                                  box(
                                    width = 12,
                                    background='black',
                                    plotOutput('bar_svs'))
                                )
                              )
                              
                            ),
                            tabPanel("PIE CHART", plotlyOutput("p51"),
                                     br(),
                                     fluidRow(
                                       column(
                                         width=12,
                                         box(
                                           width =12,
                                           background='lime',
                                           verbatimTextOutput('pie_svs_text'))
                                       )
                                     )
                                     )
                )
        ),

######################################## REGRESSION ###########################

        tabItem(tabName = "reg",
                h1("Linear Regression"),
                tabsetPanel(type = "tabs",
                            tabPanel("Wins & Goals", 
                                     fluidRow(
                                       column(
                                         width=12,
                                         box(
                                           width = 12,
                                           background='black',
                                           plotOutput('preg1'))
                                         )
                                      ),
                                     br(),
                                     fluidRow(
                                       column(
                                         width=12,
                                         box(
                                           width = 12,
                                           background='black',
                                           verbatimTextOutput('preg1_sum'))
                                        )
                                      )
                             ),#tabPanel end
                            
                             tabPanel("Wins & Clean Sheets", 
                                      fluidRow(
                                        column(
                                          width=12,
                                          box(
                                            width = 12,
                                            background='black',
                                            plotOutput('preg2'))
                                        )
                                      ),
                                      br(),
                                      fluidRow(
                                        column(
                                          width=12,
                                          box(
                                            width = 12,
                                            background='black',
                                            verbatimTextOutput('preg2_sum'))
                                        )
                                      )
                              ),
                           
                              tabPanel("Clean Sheets & Saves", 
                                     fluidRow(
                                       column(
                                         width=12,
                                         box(
                                           width = 12,
                                           background='black',
                                           plotOutput('preg3'))
                                       )
                                     ),
                                     br(),
                                     fluidRow(
                                       column(
                                         width=12,
                                         box(
                                           width = 12,
                                           background='black',
                                           verbatimTextOutput('preg3_sum'))
                                       )
                                     )
                              ),
                           
                            tabPanel("Clean Sheets & Goals Conceded", 
                                    fluidRow(
                                      column(
                                        width=12,
                                        box(
                                          width = 12,
                                          background='black',
                                          plotOutput('preg4'))
                                      )
                                    ),
                                    br(),
                                    fluidRow(
                                      column(
                                        width=12,
                                        box(
                                          width = 12,
                                          background='black',
                                          verbatimTextOutput('preg4_sum'))
                                      )
                                    )
                            ),
                            
                            tabPanel("Goals & Shots on Target", 
                                     fluidRow(
                                       column(
                                         width=12,
                                         box(
                                           width = 12,
                                           background='black',
                                           plotOutput('preg5'))
                                       )
                                     ),
                                     br(),
                                     fluidRow(
                                       column(
                                         width=12,
                                         box(
                                           width = 12,
                                           background='black',
                                           verbatimTextOutput('preg5_sum'))
                                       )
                                     )
                            ),
                          
                           tabPanel("Goals & Shots from Inbox", 
                                    fluidRow(
                                      column(
                                        width=12,
                                        box(
                                          width = 12,
                                          background='black',
                                          plotOutput('preg6'))
                                      )
                                    ),
                                    br(),
                                    fluidRow(
                                      column(
                                        width=12,
                                        box(
                                          width = 12,
                                          background='black',
                                          verbatimTextOutput('preg6_sum'))
                                      )
                                    )
                           )
                      ) #tabsetpanel reg
                 ), #tabItemReg
        
        tabItem(tabName = "final",
                h1("Prediction"),
                            tabBox(
                              width = 12,
                              title = "Final Ranking Prediction",
                              id = "abc",height = "600px",
                            
                            tabPanel("Final Ranking Prediction", 
                                     fluidRow(
                                       column(
                                         width=12,
                                         box(
                                           dataTableOutput('table')
                                           )
                                       )
                                     )
                            
                            )
                        )#tabbox final
                 
        ) #tabItem final
        
      ) #tabItems
      
    ) #dashboardbody
  ) #dashboardPage
) #shinyUI
