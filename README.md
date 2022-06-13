# NBA_Team_Wins_Multivariate_Regression
Files associated with the multivariate regression model to predict the number of wins for each NBA team
```{r}
#Loading data into R
library(readxl)
myData<-read_excel("C:/Users/Michael Osei/Downloads/NBA Team Stats.xlsx")
myData
# A tibble: 30 x 27
   Team       GP     W     L `WIN%`   MIN   PTS   FGM   FGA `FG%` `3PM` `3PA` `3P%`   FTM   FTA `FT%`  OREB  DREB   REB
   <chr>   <dbl> <dbl> <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
 1 Phoeni~    82    64    18  0.78   48.1  115.  43.7  90.1  48.5  11.6  31.9  36.4  15.9  19.9  79.7   9.8  35.5  45.3
 2 Memphi~    82    56    26  0.683  48.2  116.  43.5  94.4  46.1  11.5  32.7  35.3  17    23.1  73.4  14.1  35    49.2
 3 Golden~    82    53    29  0.646  48.1  111   40.5  86.4  46.9  14.3  39.4  36.4  15.6  20.3  76.9   9.8  35.7  45.5
 4 Miami ~    82    53    29  0.646  48.4  110   39.6  84.8  46.7  13.6  35.8  37.9  17.3  21.4  80.8   9.8  33.9  43.7
 5 Dallas~    82    52    30  0.634  48.2  108   39.3  85.1  46.1  13.1  37.4  35    16.4  21.2  77.1   9.3  33.8  43  
 6 Boston~    82    51    31  0.622  48.5  112.  40.7  87.4  46.6  13.2  37.1  35.6  17    20.9  81.6  10.5  35.5  46.1
 7 Milwau~    82    51    31  0.622  48.2  116.  41.8  89.4  46.8  14.1  38.4  36.6  17.8  22.9  77.6  10.2  36.5  46.7
 8 Philad~    82    51    31  0.622  48.3  110.  39.4  84.5  46.6  11.6  31.8  36.4  19.6  23.8  82.1   8.5  33.8  42.3
 9 Utah J~    82    49    33  0.598  48.1  114.  40.6  86.2  47.1  14.5  40.3  36    17.9  23.4  76.7  10.8  35.6  46.3
10 Denver~    82    48    34  0.585  48.3  113.  41.7  86.3  48.3  12.7  35.9  35.3  16.7  21    79.5   9.2  34.9  44.1
# ... with 20 more rows, and 8 more variables: AST <dbl>, TOV <dbl>, STL <dbl>, BLK <dbl>, BLKA <dbl>, PF <dbl>,
#   PFD <dbl>, `=+/-` <dbl>
```

```{r}
#Building original multivariate regression model to predict number of wins
myModel<-lm(myData$W~myData$PTS+myData$FGM+myData$FGA+myData$`FG%`+myData$`3PM`+myData$`3PA`+myData$`3P%`+myData$FTM+myData$FTA+myData$`FT%`+myData$OREB+myData$DREB+myData$AST+myData$TOV+myData$STL+myData$BLK)
summary(myModel)
Call:
lm(formula = myData$W ~ myData$PTS + myData$FGM + myData$FGA + 
    myData$`FG%` + myData$`3PM` + myData$`3PA` + myData$`3P%` + 
    myData$FTM + myData$FTA + myData$`FT%` + myData$OREB + myData$DREB + 
    myData$AST + myData$TOV + myData$STL + myData$BLK)

Residuals:
   Min     1Q Median     3Q    Max 
-6.577 -2.221  0.527  2.118  6.082 

Coefficients:
              Estimate Std. Error t value Pr(>|t|)   
(Intercept)   815.8117  1786.7107   0.457  0.65549   
myData$PTS    -14.9125    15.5853  -0.957  0.35611   
myData$FGM     46.3183    29.1899   1.587  0.13657   
myData$FGA     -9.5445    16.9037  -0.565  0.58193   
myData$`FG%`   -9.3511    32.6649  -0.286  0.77918   
myData$`3PM`    6.7062    26.3049   0.255  0.80276   
myData$`3PA`    3.4677     9.4480   0.367  0.71950   
myData$`3P%`    4.0353     9.3251   0.433  0.67230   
myData$FTM     41.5048    28.6800   1.447  0.17153   
myData$FTA    -22.1398    22.4282  -0.987  0.34159   
myData$`FT%`   -5.3418     6.3001  -0.848  0.41184   
myData$OREB     3.6381     1.2425   2.928  0.01176 * 
myData$DREB     2.4804     0.8966   2.767  0.01602 * 
myData$AST     -1.2374     1.0361  -1.194  0.25369   
myData$TOV     -2.5730     1.8987  -1.355  0.19844   
myData$STL      7.0269     2.1044   3.339  0.00533 **
myData$BLK      0.4489     1.6782   0.267  0.79329   
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 4.603 on 13 degrees of freedom
Multiple R-squared:  0.929,	Adjusted R-squared:  0.8416 
F-statistic: 10.63 on 16 and 13 DF,  p-value: 5.368e-05
myModel.res<-residuals(myModel)
myModel.res
 1.6051549  2.5326844 -3.0560312  4.4644936  6.0815315  5.6319075  0.9271285 -1.3103412 -4.1559206  0.4937380 
        11         12         13         14         15         16         17         18         19         20 
 2.1204812  0.8585709  2.4975173 -1.0828781  1.1664615 -6.5767240 -2.0058602 -2.2933071  0.1939221 -4.3174994 
        21         22         23         24         25         26         27         28         29         30 
-0.9564227 -0.2345515  2.1085150  2.1675963 -3.6368162 -4.3746394 -3.3140549  2.9052371  0.9997620  0.5603451 
```

```{r}
#New model using feedback from previous model
myModel2<-lm(myData$W~myData$PTS+myData$FGM+myData$FGA+myData$`FG%`+myData$`3PM`+myData$`3PA`+myData$`3P%`+myData$`FT%`+myData$OREB+myData$DREB+myData$AST+myData$TOV+myData$STL+myData$BLK)
summary(myModel2)
Call:
lm(formula = myData$W ~ myData$PTS + myData$FGM + myData$FGA + 
    myData$`FG%` + myData$`3PM` + myData$`3PA` + myData$`3P%` + 
    myData$`FT%` + myData$OREB + myData$DREB + myData$AST + myData$TOV + 
    myData$STL + myData$BLK)

Residuals:
    Min      1Q  Median      3Q     Max 
-7.5792 -2.8854  0.6011  1.9204  6.9834 

Coefficients:
              Estimate Std. Error t value Pr(>|t|)   
(Intercept)  1050.8920  1155.3916   0.910  0.37744   
myData$PTS     -1.2901     1.2568  -1.027  0.32090   
myData$FGM     31.8235    27.8827   1.141  0.27162   
myData$FGA    -15.5180    12.5548  -1.236  0.23546   
myData$`FG%`  -21.2234    23.9887  -0.885  0.39027   
myData$`3PM`    5.7105    25.3130   0.226  0.82456   
myData$`3PA`   -1.0714     8.9116  -0.120  0.90590   
myData$`3P%`   -0.6283     8.7610  -0.072  0.94378   
myData$`FT%`    0.9721     0.4552   2.135  0.04963 * 
myData$OREB     3.0472     1.1923   2.556  0.02195 * 
myData$DREB     2.3593     0.9000   2.622  0.01925 * 
myData$AST     -0.8549     0.9786  -0.874  0.39611   
myData$TOV     -3.6207     1.3473  -2.687  0.01688 * 
myData$STL      7.8370     1.9597   3.999  0.00116 **
myData$BLK     -0.3131     1.5716  -0.199  0.84476   
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 4.655 on 15 degrees of freedom
Multiple R-squared:  0.9162,	Adjusted R-squared:  0.838 
F-statistic: 11.72 on 14 and 15 DF,  p-value: 1.209e-05
myModel.res2<-residuals(myModel2)
myModel.res2
1.85247127  3.12064616 -3.27308383  4.65387274  6.98336140  1.90322575  1.08436712  0.40275008 -3.61807930 
         10          11          12          13          14          15          16          17          18 
 1.21613332  3.00541695  0.15626619  4.20092996  1.71695047  2.36823946 -7.57915278 -3.14580493 -3.28180995 
         19          20          21          22          23          24          25          26          27 
 0.01069999 -5.94068425 -0.99738501 -0.03762739  0.06720416  1.63412994 -5.21159736 -4.20335691 -2.10429834 
         28          29          30 
 1.92611372  2.29055373  0.79954763 
```

```{r}
#Third model to try and reduce complexity of previous model
myModel3<-lm(myData$W~myData$PTS+myData$FGM+myData$FGA+myData$`3PM`+myData$`3PA`+myData$`FT%`+myData$OREB+myData$DREB+myData$AST+myData$TOV+myData$STL)
summary(myModel3)
Call:
lm(formula = myData$W ~ myData$PTS + myData$FGM + myData$FGA + 
    myData$`3PM` + myData$`3PA` + myData$`FT%` + myData$OREB + 
    myData$DREB + myData$AST + myData$TOV + myData$STL)

Residuals:
    Min      1Q  Median      3Q     Max 
-7.7938 -2.4084  0.9812  2.0217  8.1010 

Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept)   76.9282    75.4515   1.020 0.321441    
myData$PTS    -1.2786     1.0497  -1.218 0.238933    
myData$FGM     7.3387     2.8708   2.556 0.019834 *  
myData$FGA    -4.4396     0.8137  -5.456  3.5e-05 ***
myData$`3PM`   4.7463     2.7891   1.702 0.106019    
myData$`3PA`  -0.7108     1.0855  -0.655 0.520899    
myData$`FT%`   0.8670     0.4094   2.118 0.048373 *  
myData$OREB    2.8967     1.0829   2.675 0.015453 *  
myData$DREB    2.2924     0.8292   2.765 0.012766 *  
myData$AST    -0.6512     0.8217  -0.793 0.438387    
myData$TOV    -3.9635     1.1979  -3.309 0.003906 ** 
myData$STL     7.8172     1.6101   4.855 0.000127 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 4.359 on 18 degrees of freedom
Multiple R-squared:  0.9119,	Adjusted R-squared:  0.858 
F-statistic: 16.93 on 11 and 18 DF,  p-value: 2.693e-07
myModel.res3<-residuals(myModel3)
myModel.res3
3.6510183  3.3244976 -4.4971582  3.9476025  8.1009939  1.2969191  1.0255936 -0.3676377 -3.7183869  1.2280916 
        11         12         13         14         15         16         17         18         19         20 
 1.7615267 -0.4509353  4.3880548  2.6646904  2.0040580 -7.7937829 -2.5592991 -2.0728147  0.9618427 -4.9983839 
        21         22         23         24         25         26         27         28         29         30 
-1.2632966 -1.3343465 -0.4700747  1.5740334 -4.7916642 -4.5271535 -2.5202749  2.4080922  2.0275969  1.0005976 
```

```{r}
#Fourth model to try and reduce complexity
myModel4<-lm(myData$W~+myData$FGM+myData$FGA+myData$OREB+myData$DREB+myData$TOV+myData$STL)
summary(myModel4)

Call:
lm(formula = myData$W ~ +myData$FGM + myData$FGA + myData$OREB + 
    myData$DREB + myData$TOV + myData$STL)

Residuals:
    Min      1Q  Median      3Q     Max 
-8.5704 -3.3322 -0.6302  2.5991  8.8443 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) 149.9390    52.9532   2.832  0.00946 ** 
myData$FGM    4.5188     0.7998   5.650 9.44e-06 ***
myData$FGA   -4.8004     0.6617  -7.255 2.20e-07 ***
myData$OREB   2.2430     1.1150   2.012  0.05611 .  
myData$DREB   3.1256     0.8311   3.761  0.00102 ** 
myData$TOV   -4.7852     1.0200  -4.691  0.00010 ***
myData$STL    8.6987     1.6811   5.174 3.03e-05 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 4.856 on 23 degrees of freedom
Multiple R-squared:  0.8602,	Adjusted R-squared:  0.8238 
F-statistic: 23.59 on 6 and 23 DF,  p-value: 9.536e-09
myModel.res4<-residuals(myModel4)
myModel.res4
 3.0887502 -0.4508865 -4.0070842  8.7478082  8.0177572  4.6417251  2.3827029 -3.2186683 -1.7382535  0.9404806 
        11         12         13         14         15         16         17         18         19         20 
 4.3283940 -0.5721138  8.8443103  4.6545970 -1.2213420 -4.6654137 -2.3067674  2.1073293 -0.6883003 -8.5703915 
        21         22         23         24         25         26         27         28         29         30 
-5.3202844 -3.3700992 -4.5088094 -1.1192392 -3.9084139 -2.4728211 -3.7559753  2.0388344  2.6712261 -0.5690516
```
