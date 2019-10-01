library(sqldf)
library(plyr)

head(stats_3)

View(stats_3)

pairs(stats_3[2:15])


saves_data17_18 <- ddply(stats_3, .(team,season), summarize,  saves_mean=mean(saves))
goals_conceded_data17_18 <- ddply(stats_3, .(team,season), summarize,  goals_conceded_mean=mean(goals_conceded))
shots_on_target_data17_18 <- ddply(stats_3, .(team,season), summarize,  shots_on_target_mean=mean(ontarget_scoring_att))
inside_box_data17_18 <- ddply(stats_3, .(team,season), summarize,  shots_inside_box_mean=mean(att_ibox_goal))
clean_sheet_data17_18 <- ddply(stats_3, .(team,season), summarize,  clean_sheet_mean=mean(clean_sheet))
goals_data17_18 <- ddply(stats_3, .(team,season), summarize,  goals_mean=mean(goals))


results17_18 <- lm(wins~goals+clean_sheet,stats_3)
results217_18 <- lm(goals~ontarget_scoring_att+att_ibox_goal,stats_3)
results317_18 <- lm(clean_sheet~goals_conceded+saves,stats_3)

ind <- sample(2, nrow(stats_3), replace = TRUE, prob = c(0.8,0.2))
traindata <- stats_3[ind==1,]
testdata <- stats_3[ind==2,]


summary(results17_18)
summary(results217_18)
summary(results317_18)

saves_data17_18
goals_conceded_data17_18
shots_on_target_data17_18
inside_box_data17_18
clean_sheet_data17_18
goals_data17_18
results17_18
results217_18
results317_18


chelsea17_18 <- stats[stats_3$team=="Chelsea",]$saves
View(chelsea17_18)
mean(chelsea17_18)

saves17_18 <- stats[stats_3$season=="201718",]$saves
View(saves17_18)

a <- unique(stats_3$team)

for (i in a) {
 u <- dataframe[stats_3$team==i,]$goals
 v <- dataframe[stats_3$team==i,]$clean_sheet
 w <- dataframe[stats_3$team==i,]$ontarget_scoring_att
 x <- dataframe[stats_3$team==i,]$att_ibox_goal
 y <- dataframe[stats_3$team==i,]$saves
 z <- dataframe[stats_3$team==i,]$goals_conceded

  print(paste(i,' ',floor(mean(u)),' ',floor(mean(v)),' ',floor(mean(w)),' ',floor(mean(x)),' ',floor(mean(y)),' ',floor(mean(z))))
  
}




#for loop for saves

for (i in a) {
  x <- stats[stats_3$team==i,]$saves
  print(paste(i,' ',floor(mean(x))))
}

#for loop for goals conceded

for (i in a) {
  y <- stats[stats_3$team==i,]$goals_conceded
  print(paste(i,' ',floor(mean(y))))
}

#for loop for shots on target

for (i in a) {
  v <- stats[stats_3$team==i,]$goals
  print(paste(i,' ',floor(mean(v))))
}

#for loop for inbox shots

for (i in a) {
  w <- stats[stats_3$team==i,]$clean_sheet
  print(paste(i,' ',floor(mean(w))))
}

summary(results17_18)
summary(results217_18)
summary(results317_18)


predict(results17_18,data.frame(goals=106,clean_sheet=18),interval='confidence')
predict(results217_18,data.frame(ontarget_scoring_att=209,att_ibox_goal=56),interval='confidence')
predict(results317_18,data.frame(goals_conceded=36,saves=92),interval='confidence')

#REGRESSION PLOTS
ggplot(stats_3,aes(wins,goals)) + geom_point() + geom_smooth(method = lm)
ggplot(stats_3,aes(wins,clean_sheet)) + geom_point() + geom_smooth(method = lm)
ggplot(stats_3,aes(clean_sheet,saves)) + geom_point() + geom_smooth(method = lm)
ggplot(stats_3,aes(clean_sheet,goals_conceded)) + geom_point() + geom_smooth(method = lm)
ggplot(stats_3,aes(goals,ontarget_scoring_att)) + geom_point() + geom_smooth(method = lm)
ggplot(stats_3,aes(goals,att_ibox_goal)) + geom_point() + geom_smooth(method = lm)


#goals
plot(stats_3$wins, stats_3$goals, xlab = "Wins", ylab = "Goals ", main = "Factor 1 - Wins vs Goals")

#clean sheets
plot(stats_3$wins, stats_3$clean_sheet, xlab = "Wins", ylab = "Clean Sheets ", main = "Factor 2 - Wins vs Clean Sheets")

#shots on target
plot(stats_3$goals, stats_3$ontarget_scoring_att, xlab = "Goals", ylab = "Shots on target", main = "Factor 3 - Goals vs Shots on Target")

#shots from inside box
plot(stats_3$goals, stats_3$att_ibox_goal, xlab = "Goals", ylab = "Shots from inside box", main = "Factor 4 - Goals vs Shots from inside box")

#goals conceded
plot(stats_3$clean_sheet, stats_3$goals_conceded, xlab = "Clean Sheets", ylab = "Goals Conceded", main = "Factor 5 - Clean Sheets vs Goals Conceded")

#saves
plot(stats_3$clean_sheet, stats_3$saves, xlab = "Clean Sheets", ylab = "Saves", main = "Factor 5 - Clean Sheets vs Saves")

#pie for goals
pie_goals <- plot_ly(goals_data17_18, labels = ~team, values = ~goals_mean, type = 'pie') %>%
  layout(title = 'Total number & percent of goals made by the Team',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

pie_goals

#pie for clean sheets
pie_cs <- plot_ly(clean_sheet_data17_18, labels = ~team, values = ~clean_sheet_mean, type = 'pie') %>%
  layout(title = 'Total number & percent of clean sheets made by the Team',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

pie_cs

#pie for shots from inside box
pie_ib <- plot_ly(inside_box_data17_18, labels = ~team, values = ~shots_inside_box_mean, type = 'pie') %>%
  layout(title = 'Total number & percent of shots from inside box by the Team',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

pie_ib

#pie for shots on target
pie_st <- plot_ly(shots_on_target_data17_18, labels = ~team, values = ~shots_on_target_mean, type = 'pie') %>%
  layout(title = 'Total number & percent of shots on target by the Team',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

pie_st

#pie for goals conceded
pie_gc <- plot_ly(goals_conceded_data17_18, labels = ~team, values = ~goals_conceded_mean, type = 'pie') %>%
  layout(title = 'Total number & percent of goals conceded by the Team',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

pie_gc

#pie for saves
pie_saves <- plot_ly(saves_data17_18, labels = ~team, values = ~saves_mean, type = 'pie') %>%
  layout(title = 'Total number & percent of saves made by the Team',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

pie_saves

x <- team_ranking[,c(2:7)]
x

new <- cbind(sapply(-x, rank))
new

# demoi <- rank(-x,ties.method = "min")
# demoi
# demo <- rank(x,ties.method = "min")
# demo

y <- team_ranking_final[,c(2:7)]
y

new_y <- cbind(sapply(-y, rank))
new_y

new_y$Mean <- rowMeans(new_y[,1:4])

new_yy <- cbind(sapply(-y, rank))
new_yy$Mean <- floor(rowMeans(new_yy[,1:4]))
desc(new_yy)


########################################################

yy <- team_ranking_final[,c(2:7)]
yy

new_y <- cbind(sapply(-yy, rank))
new_y

new_y$Mean <- rowMeans(new_y[,1:4])

news_yy <- cbind(sapply(-yy, rank))
news_yy
news_yyy <- floor(rowMeans(news_yy[,1:4]))
news_yyy
desc(news_yy)




apply(-team_ranking[,c(2:7)],1, rank, ties.method='min')

team_ranking[order(-rank(team_ranking$goals)),]

str(team_ranking)



p <- plot_ly(stats_3, x = ~ontarget_scoring_att, y = ~team, name = "ontarget_scoring_att", type = 'scatter',
             mode = "markers", marker = list(color = "pink")) %>%
  add_trace(x = ~att_ibox_goal, y = ~team, name = "att_ibox_goal",type = 'scatter',
            mode = "markers", marker = list(color = "blue")) %>%
  layout(
    title = "Gender earnings disparity",
    xaxis = list(title = "Annual Salary (in thousands)"),
    margin = list(l = 100)
  )
p

