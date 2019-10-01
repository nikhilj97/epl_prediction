library(shiny)
library(shinydashboard)
library(SnowballC)
library(tm)
library(ggplot2)
library(plotly)
library(plyr)
library(session)
library(dplyr)
library(wordcloud)

shinyServer(
  function(input,output, session)
  {
    # output$plot <- renderPlot(
    #   {
    #     inFile <- input$file1
    #     if (is.null(inFile))
    #     {
    #       return(NULL)
    #     }
    #     else(inFile =="stats_3.csv")
    #     {
    #       booksDS <- read.csv(inFile$datapath, header = input$header)
    #       booksDS <-data.frame(booksDS)
    #     }
    #   }
    # )
  mydata <- reactive({
    req(input$file1)
    inFile <- input$file1
    df <- read.csv(inFile$datapath, header = T, sep = ',')
    
####################################### WORD CLOUD ##########################################################
        
    output$plot <- renderPlot({
      text <- readLines(inFile$datapath)
      docs<- Corpus(VectorSource(text))
      inspect(docs)
      
      toSpace<- content_transformer(function(x,pattern)gsub(pattern,"",x))
      docs<-tm_map(docs,toSpace,"/")
      docs<-tm_map(docs,toSpace,"@")
      docs<-tm_map(docs,content_transformer(tolower))
      docs<-tm_map(docs,removeNumbers)
      docs<-tm_map(docs,removeWords,stopwords("english"))
      docs<-tm_map(docs,removeWords,c("blabla1","blabla2"))
      docs<-tm_map(docs,removePunctuation)
      docs<-tm_map(docs,stripWhitespace)
      docs<-tm_map(docs,stemDocument)
      
      dtm<-TermDocumentMatrix(docs)
      m<-as.matrix(dtm)
      
      v<-sort(rowSums(m),decreasing = TRUE)
      d<-data.frame(word=names(v),freq=v)
      head(d,2)
      set.seed(1234)
      
      wordcloud(words=d$word,freq=d$freq,min.freq=1,max.words=1000,
                random.order=FALSE,rot.per=0.30,
                colors=brewer.pal(8,"Dark2"))
      
    })

#############################################################################################################
            
    updateSelectInput(session,inputId = 'goalazo', label='Team Names', choices = df$team, selected = df$team)
    updateSelectInput(session,inputId = 'barcs', label='Team Names', choices = df$team, selected = df$team)
    updateSelectInput(session,inputId = 'barst', label='Team Names', choices = df$team, selected = df$team)
    updateSelectInput(session,inputId = 'bargc', label='Team Names', choices = df$team, selected = df$team)
    updateSelectInput(session,inputId = 'barsvs', label='Team Names', choices = df$team, selected = df$team)
    updateSelectInput(session,inputId = 'barib', label='Team Names', choices = df$team, selected = df$team)
    updateSelectInput(session,inputId = 'ddgoals', label='Seasons', choices = df$season, selected = df$season)
    updateSelectInput(session,inputId = 'ddcs', label='Seasons', choices = df$season, selected = df$season)
    
    return(df)
  })
  observe({
    
    results17_18 <- lm(wins~goals+clean_sheet,mydata())
    results217_18 <- lm(goals~ontarget_scoring_att+att_ibox_goal,mydata())
    results317_18 <- lm(clean_sheet~goals_conceded+saves,mydata())
    
    ind <- sample(2, nrow(stats_3), replace = TRUE, prob = c(0.8,0.2))
    traindata <- stats_3[ind==1,]
    testdata <- stats_3[ind==2,]
    
    
    output$sct_goals_text <- renderPrint({"The x-axis of scatter plot have wins and y-axis have goals. From the scatter plot we can conclude the relation between wins & goals. Wins is directly proportional to Goals i.e higher the number of goals scored, higher are the chances of winning."})
    output$don_goals_text <- renderPrint({"The Donut chart shows the total number of goals scored by each team across all the 3 seasons. It also shows that, out of the total number of goals scored in 3 seasons how many percent of goals are scored by each team."})
    output$dd_goals_text <- renderPrint({"The Dot Plot shows the number of 'shots on target' and 'shots from inside the box' for a team in a particular season. As 'shots on target' and 'shot from inside box' are the two main factor to determine the goals; this visualization helps to understand the pattern for goals."})
    
    output$sct_cs_text <- renderPrint({"The x-axis of scatter plot have wins and y-axis have clean sheets. From the scatter plot we can conclude the relation between wins & clean sheets. Wins is directly proportional to clean sheets i.e higher the number of clean sheets, higher are the chances of winning."})
    output$don_cs_text <- renderPrint({"The Donut chart shows the total number of clean sheets kept by each team across all the 3 seasons. It also shows that, out of the total number of clean sheets kept in 3 seasons how many percent of clean sheets are kept by each team."})
    output$dd_cs_text <- renderPrint({"The Dot Plot shows the number of 'saves' and 'goals conceded' for a team in a particular season. As 'saves' and 'goals conceded' are the two main factor to determine the clean sheets; this visualization helps to understand the pattern for clean sheets. Goals conceded and clean sheets are inversely proportional to each other i.e the more number of goals conceded the less clean sheets are kept."})
    
    output$don_st_text <- renderPrint({"The Donut chart shows the total number of shots on target made by each team across all the 3 seasons. It also shows that, out of the total number of shots on target made in 3 seasons how many percent of shots on target are made by each team."})
    
    output$don_ib_text <- renderPrint({"The Donut chart shows the total number of shots from inside the box made by each team across all the 3 seasons. It also shows that, out of the total number of shots from inside the box made in 3 seasons how many percent of shots from inside the box are made by each team."})
    
    output$sct_gc_text <- renderPrint({"The x-axis of scatter plot have wins and y-axis have goals conceded. From the scatter plot we can conclude the relation between wins & goals conceded. Wins is inversely proportional to goals conceded i.e higher the number of goals conceded, less are the chances of winning."})
    output$pie_gc_text <- renderPrint({"The Pie chart shows the total number of goals conceded by each team across all the 3 seasons. It also shows that, out of the total number of goals conceded in 3 seasons how many percent of goals are conceded by each team. "})
    
    output$sct_svs_text <- renderPrint({"The x-axis of scatter plot have wins and y-axis have saves. From the scatter plot we can conclude the relation between wins & saves. If the teams saves are higher in number it indicates the team's defence is relatively weak, which further says that the chance of winning is less. Hence, it's safe to say that higher the number of saves, less are the chances of winning."})
    output$pie_svs_text <- renderPrint({"The Pie chart shows the total number of saves made by each team across all the 3 seasons. It also shows that, out of the total number of saves made in 3 seasons how many percent of saves are made by each team."})
      
    
######################################### GOALS #######################################################

#Scatter Plot    
    output$p0 <- renderPlotly({
      # inFile <- input$file1
      # a <- read.csv(inFile$datapath, header = input$header)
      #data(), x = ~wins
      p <- plot_ly(mydata(), x = ~wins, y = ~goals,
                   marker = list(size = 10,
                                 color = 'rgba(255, 182, 193, .9)',
                                 line = list(color = 'rgba(152, 0, 0, .8)',
                                             width = 2))) %>%
        layout(title = 'Goals vs Wins',
               yaxis = list(zeroline = FALSE),
               xaxis = list(zeroline = FALSE))
    })
    
    
     output$bar_goalazo <- renderPlot({
     
      
      a<-mydata()%>%filter(mydata()$team==input$goalazo) %>%select(season,goals,team)
      b<-data.frame(list(c(a)))
      print(b)
      ggplot(b,aes(x=season,y=goals)) + geom_bar(stat = 'identity',fill="#FF9999", colour="black")
      
    })

    
    
#Donut Chart    
    output$p02 <- renderPlotly({
      # inFile <- input$file1
      # a <- read.csv(inFile$datapath, header = input$header)
      goals_data17_18 <- ddply(mydata(), .(team,season), summarize,  goals_mean=mean(goals))
      p <- goals_data17_18 %>%
        group_by(team) %>%
        plot_ly(labels = ~team, values = ~goals_mean) %>%
        add_pie(hole = 0.6) %>%
        layout(title = "Goals data         ",  showlegend = T,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
    })
    
##DDPlot    
    output$dd_goalazo <- renderPlotly({
       a<-mydata()%>%filter(mydata()$season==input$ddgoals) %>%select(ontarget_scoring_att,att_ibox_goal,season,team,goals)
       b<-data.frame(list(c(a)))
       print(b)
      p <- plot_ly(b, x = ~team, y = ~ontarget_scoring_att, name = "on target", type = 'scatter',
                   mode = "markers", marker = list(color = "red")) %>%
        add_trace(x = ~team, y = ~att_ibox_goal, name = "inbox shots",type = 'scatter',
                  mode = "markers", marker = list(color = "blue")) %>%
        layout(
          title = "Goals",
          xaxis = list(title = "teams"),
          yaxis = list(title = "count"),
          margin = list(l = 100)
        )
      
    })
    
      
###################################### CLEAN SHEETS ##########################################################
    
#Scatter Plot
    output$p1 <- renderPlotly({
      # inFile <- input$file1
      # a <- read.csv(inFile$datapath, header = input$header)
      p <- plot_ly(mydata(), x = ~wins, y = ~clean_sheet,
                   marker = list(size = 10,
                                 color = 'rgba(255, 182, 193, .9)',
                                 line = list(color = 'rgba(152, 0, 0, .8)',
                                             width = 2))) %>%
        layout(title = 'Clean Sheets vs Wins',
               yaxis = list(zeroline = FALSE),
               xaxis = list(zeroline = FALSE))
      
    })
    
#Pie Chart
    # output$p11 <- renderPlotly({
    #   # inFile <- input$file1
    #   # a <- read.csv(inFile$datapath, header = input$header)
    #   clean_sheet_data17_18 <- ddply(mydata(), .(team,season), summarize,  clean_sheet_mean=mean(clean_sheet))
    #   pie_cs <- plot_ly(clean_sheet_data17_18, labels = ~team, values = ~clean_sheet_mean, type = 'pie') %>%
    #     layout(title = 'Clean Sheets data         ',
    #            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
    #            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    #   
    #   pie_cs
    #   
    # })
    
#BAR CHART
    
    output$bar_cs <- renderPlot({
      
      a<-mydata()%>%filter(mydata()$team==input$barcs) %>%select(season,clean_sheet,team)
      b<-data.frame(list(c(a)))
      print(b)
      ggplot(b,aes(x=season,y=clean_sheet)) + geom_bar(stat = 'identity',fill="steelblue")
      
    })
    
#Donut Chart
    output$p12 <- renderPlotly({
      # inFile <- input$file1
      # a <- read.csv(inFile$datapath, header = input$header)
      clean_sheet_data17_18 <- ddply(mydata(), .(team,season), summarize,  clean_sheet_mean=mean(clean_sheet))
      p <- clean_sheet_data17_18 %>%
        group_by(team) %>%
        plot_ly(labels = ~team, values = ~clean_sheet_mean) %>%
        add_pie(hole = 0.6) %>%
        layout(title = "Clean Sheets data         ",  showlegend = T,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
    })
    
#DDPlot
    
    output$dd_cs <- renderPlotly({
      a<-mydata()%>%filter(mydata()$season==input$ddcs) %>%select(saves,goals_conceded,season,team,clean_sheet)
      b<-data.frame(list(c(a)))
      print(b)
      p <- plot_ly(b, x = ~team, y = ~saves, name = "saves", type = 'scatter',
                   mode = "markers", marker = list(color = "red")) %>%
        add_trace(x = ~team, y = ~goals_conceded, name = "goals conceded",type = 'scatter',
                  mode = "markers", marker = list(color = "blue")) %>%
        layout(
          title = "Clean Sheets",
          xaxis = list(title = "teams"),
          yaxis = list(title = "count"),
          margin = list(l = 100)
        )
      
    })
    
################################## SHOTS ON TARGET #############################################################    
      
#Scatter Plot
    # output$p2 <- renderPlotly({
    #   # inFile <- input$file1
    #   # a <- read.csv(inFile$datapath, header = input$header)
    #   p <- plot_ly(mydata(), x = ~wins, y = ~ontarget_scoring_att,
    #                marker = list(size = 10,
    #                              color = 'rgba(255, 182, 193, .9)',
    #                              line = list(color = 'rgba(152, 0, 0, .8)',
    #                                          width = 2))) %>%
    #     layout(title = 'Shots on target vs Wins',
    #            yaxis = list(zeroline = FALSE),
    #            xaxis = list(zeroline = FALSE))
    #   
    # })
    
#Pie Chart
    # output$p21 <- renderPlotly({
    #   # inFile <- input$file1
    #   # a <- read.csv(inFile$datapath, header = input$header)
    #   shots_on_target_data17_18 <- ddply(mydata(), .(team,season), summarize,  shots_on_target_mean=mean(ontarget_scoring_att))
    #   pie_st <- plot_ly(shots_on_target_data17_18, labels = ~team, values = ~shots_on_target_mean, type = 'pie') %>%
    #     layout(title = 'Shots on target data         ',
    #            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
    #            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    #   
    #   pie_st
    #   
    # })
    
    
#Bar Chart
    output$bar_st <- renderPlot({
      
      a<-mydata()%>%filter(mydata()$team==input$barst) %>%select(season,ontarget_scoring_att,team)
      b<-data.frame(list(c(a)))
      print(b)
      ggplot(b,aes(x=season,y=ontarget_scoring_att)) + geom_bar(stat = 'identity',fill="steelblue")
      
    })
    
  
#Donut Chart
    output$p22 <- renderPlotly({
      # inFile <- input$file1
      # a <- read.csv(inFile$datapath, header = input$header)
      shots_on_target_data17_18 <- ddply(mydata(), .(team,season), summarize,  shots_on_target_mean=mean(ontarget_scoring_att))
      p <- shots_on_target_data17_18 %>%
        group_by(team) %>%
        plot_ly(labels = ~team, values = ~shots_on_target_mean) %>%
        add_pie(hole = 0.6) %>%
        layout(title = "Shots on target data         ",  showlegend = T,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
    })
    
##################################### SHOTS FROM INSIDE BOX ##########################################################    
    
    
#Scatter Plot
    # output$p3 <- renderPlotly({
    #   # inFile <- input$file1
    #   # a <- read.csv(inFile$datapath, header = input$header)
    #   p <- plot_ly(mydata(), x = ~wins, y = ~att_ibox_goal,
    #                marker = list(size = 10,
    #                              color = 'rgba(255, 182, 193, .9)',
    #                              line = list(color = 'rgba(152, 0, 0, .8)',
    #                                          width = 2))) %>%
    #     layout(title = 'Inbox shots vs Wins',
    #            yaxis = list(zeroline = FALSE),
    #            xaxis = list(zeroline = FALSE))
    #   
    # })
    
#Pie Chart
    # output$p31 <- renderPlotly({
    #   # inFile <- input$file1
    #   # a <- read.csv(inFile$datapath, header = input$header)
    #   inside_box_data17_18 <- ddply(mydata(), .(team,season), summarize,  shots_inside_box_mean=mean(att_ibox_goal))
    #   pie_ib <- plot_ly(inside_box_data17_18, labels = ~team, values = ~shots_inside_box_mean, type = 'pie') %>%
    #     layout(title = 'Inbox shots data         ',
    #            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
    #            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    #   
    #   pie_ib
    #   
    # })

    
#Bar Chart
    output$bar_ib <- renderPlot({
      
      a<-mydata()%>%filter(mydata()$team==input$barib) %>%select(season,att_ibox_goal,team)
      b<-data.frame(list(c(a)))
      print(b)
      ggplot(b,aes(x=season,y=att_ibox_goal)) + geom_bar(stat = 'identity',fill="#FF9999", colour="black")
      
    })
    
    
#Donut Chart
    output$p32 <- renderPlotly({
      # inFile <- input$file1
      # a <- read.csv(inFile$datapath, header = input$header)
      inside_box_data17_18 <- ddply(mydata(), .(team,season), summarize,  shots_inside_box_mean=mean(att_ibox_goal))
      p <- inside_box_data17_18 %>%
        group_by(team) %>%
        plot_ly(labels = ~team, values = ~shots_inside_box_mean) %>%
        add_pie(hole = 0.6) %>%
        layout(title = "Inbox shots data         ",  showlegend = T,
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
    })
    
###################################### GOALS CONCEDED #########################################################    
    
#Scatter Plot
    output$p4 <- renderPlotly({
      # inFile <- input$file1
      # a <- read.csv(inFile$datapath, header = input$header)
      p <- plot_ly(mydata(), x = ~wins, y = ~goals_conceded,
                   marker = list(size = 10,
                                 color = 'rgba(255, 182, 193, .9)',
                                 line = list(color = 'rgba(152, 0, 0, .8)',
                                             width = 2))) %>%
        layout(title = 'Goals conceded vs Wins',
               yaxis = list(zeroline = FALSE),
               xaxis = list(zeroline = FALSE))
      
    })


#Bar Chart
    output$bar_gc <- renderPlot({
      
      a<-mydata()%>%filter(mydata()$team==input$bargc) %>%select(season,goals_conceded,team)
      b<-data.frame(list(c(a)))
      print(b)
      ggplot(b,aes(x=season,y=goals_conceded)) + geom_bar(stat = 'identity',fill="steelblue")
      
    })    
   
     
        
#Pie Chart
    output$p41 <- renderPlotly({
      # inFile <- input$file1
      # a <- read.csv(inFile$datapath, header = input$header)
      goals_conceded_data17_18 <- ddply(mydata(), .(team,season), summarize,  goals_conceded_mean=mean(goals_conceded))
      pie_gc <- plot_ly(goals_conceded_data17_18, labels = ~team, values = ~goals_conceded_mean, type = 'pie') %>%
        layout(title = 'Data of goals conceded         ',
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
      pie_gc
      
    })
    
#Donut Chart
    # output$p42 <- renderPlotly({
    #   # inFile <- input$file1
    #   # a <- read.csv(inFile$datapath, header = input$header)
    #   goals_conceded_data17_18 <- ddply(mydata(), .(team,season), summarize,  goals_conceded_mean=mean(goals_conceded))
    #   p <- goals_conceded_data17_18 %>%
    #     group_by(team) %>%
    #     plot_ly(labels = ~team, values = ~goals_conceded_mean) %>%
    #     add_pie(hole = 0.6) %>%
    #     layout(title = "Data of goals conceded         ",  showlegend = T,
    #            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
    #            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    #   
    # })
    
################################# SAVES ##############################################################    
    
#Scatter Plot
    output$p5 <- renderPlotly({
      # inFile <- input$file1
      # a <- read.csv(inFile$datapath, header = input$header)
      p <- plot_ly(mydata(), x = ~wins, y = ~saves,
                   marker = list(size = 10,
                                 color = 'rgba(255, 182, 193, .9)',
                                 line = list(color = 'rgba(152, 0, 0, .8)',
                                             width = 2))) %>%
        layout(title = 'Saves vs Wins',
               yaxis = list(zeroline = FALSE),
               xaxis = list(zeroline = FALSE))
      
    })
    
    
    
#Bar Chart
    output$bar_svs <- renderPlot({
      
      a<-mydata()%>%filter(mydata()$team==input$barsvs) %>%select(season,saves,team)
      b<-data.frame(list(c(a)))
      print(b)
      ggplot(b,aes(x=season,y=saves)) + geom_bar(stat = 'identity',fill="#FF9999", colour="black")
      
    })    
    
    
    
#Pie Chart
    output$p51 <- renderPlotly({
      # inFile <- input$file1
      # a <- read.csv(inFile$datapath, header = input$header)
      saves_data17_18 <- ddply(mydata(), .(team,season), summarize,  saves_mean=mean(saves))
      pie_saves <- plot_ly(saves_data17_18, labels = ~team, values = ~saves_mean, type = 'pie') %>%
        layout(title = 'Saves data         ',
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
      
      pie_saves
      
    })
    
#Donut Chart
    # output$p52 <- renderPlotly({
    #   # inFile <- input$file1
    #   # a <- read.csv(inFile$datapath, header = input$header)
    #   saves_data17_18 <- ddply(mydata(), .(team,season), summarize,  saves_mean=mean(saves))
    #   p <- saves_data17_18 %>%
    #     group_by(team) %>%
    #     plot_ly(labels = ~team, values = ~saves_mean) %>%
    #     add_pie(hole = 0.6) %>%
    #     layout(title = "Saves data         ",  showlegend = T,
    #            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
    #            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    #   
    # })
    
###############################################################################################    
    
     output$preg1 <-renderPlot({
       ggplot(mydata(),aes(wins,goals)) + geom_point() + geom_smooth(method = lm)
       
     })
    
    output$preg1_sum <- renderPrint({
      print(summary(results17_18))
    })

###############################################################################################    
    
    output$preg2 <-renderPlot({
      ggplot(stats_3,aes(wins,clean_sheet)) + geom_point() + geom_smooth(method = lm)
      
    })
    
    
    output$preg2_sum <- renderPrint({
      print(summary(results17_18))
    })
    
###############################################################################################    
    
    output$preg3 <-renderPlot({
      ggplot(stats_3,aes(clean_sheet,saves)) + geom_point() + geom_smooth(method = lm)
      
    })
    output$preg3_sum <- renderPrint({
      print(summary(results317_18))
    })
  
###############################################################################################    
    
    
    output$preg4 <-renderPlot({
      ggplot(stats_3,aes(clean_sheet,goals_conceded)) + geom_point() + geom_smooth(method = lm)
      
    })
    output$preg4_sum <- renderPrint({
      print(summary(results317_18))
    })
    
###############################################################################################    
    
    output$preg5 <-renderPlot({
      ggplot(stats_3,aes(goals,ontarget_scoring_att)) + geom_point() + geom_smooth(method = lm)
      
    })
    output$preg5_sum <- renderPrint({
      print(summary(results217_18))
    })
    
###############################################################################################    
    
    
    output$preg6 <-renderPlot({
      ggplot(stats_3,aes(goals,att_ibox_goal)) + geom_point() + geom_smooth(method = lm)
      
    })
    output$preg6_sum <- renderPrint({
      print(summary(results217_18))
    })
    
###############################################################################################    
    
    output$table <- renderDataTable(Final_Ranking)
    
    
  })  
  }

)