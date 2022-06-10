# NBA_Team_Wins_Multivariate_Regression
Files associated with the multivariate regression model to predict the number of wins for each NBA team
```{r}
#Loading data into R
library(readxl)
myData<-read_excel("C:/Users/Michael Osei/Downloads/NBA Team Stats.xlsx")
myData
```

```{r}
#Building original multivariate regression model to predict number of wins
myModel<-lm(myData$W~myData$PTS+myData$FGM+myData$FGA+myData$`FG%`+myData$`3PM`+myData$`3PA`+myData$`3P%`+myData$FTM+myData$FTA+myData$`FT%`+myData$OREB+myData$DREB+myData$AST+myData$TOV+myData$STL+myData$BLK)
summary(myModel)
myModel.res<-residuals(myModel)
myModel.res
```

```{r}
#New model using feedback from previous model
myModel2<-lm(myData$W~myData$PTS+myData$FGM+myData$FGA+myData$`FG%`+myData$`3PM`+myData$`3PA`+myData$`3P%`+myData$`FT%`+myData$OREB+myData$DREB+myData$AST+myData$TOV+myData$STL+myData$BLK)
summary(myModel2)
myModel.res2<-residuals(myModel2)
myModel.res2
```

```{r}
#Third model to try and reduce complexity of previous model
myModel3<-lm(myData$W~myData$PTS+myData$FGM+myData$FGA+myData$`3PM`+myData$`3PA`+myData$`FT%`+myData$OREB+myData$DREB+myData$AST+myData$TOV+myData$STL)
summary(myModel3)
myModel.res3<-residuals(myModel3)
myModel.res3
```

```{r}
#Fourth model to try and reduce complexity
myModel4<-lm(myData$W~+myData$FGM+myData$FGA+myData$OREB+myData$DREB+myData$TOV+myData$STL)
summary(myModel4)
myModel.res4<-residuals(myModel4)
myModel.res4
```
