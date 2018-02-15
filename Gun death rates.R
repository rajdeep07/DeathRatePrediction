
guns <- read.csv("C:/Users/Arora's Den/Documents/MS - Business Analytics/Competitions/Tableau/DataSet/GunDeaths.csv")

guns_2015<-guns[which(guns$FirearmRelatedDeathsRate!="#VALUE!"),]
guns_2015$FirearmRelatedDeathsRate<-as.character(guns_2015$FirearmRelatedDeathsRate)
guns_2015$FirearmRelatedDeathsRate<-as.numeric(guns_2015$FirearmRelatedDeathsRate)
guns_2017_16<-guns[which(guns$FirearmRelatedDeathsRate=="#VALUE!"),]
guns_2017_16$FirearmRelatedDeathsRate<-as.character(guns_2017_16$FirearmRelatedDeathsRate)
guns_2017_16$FirearmRelatedDeathsRate<-as.numeric(guns_2017_16$FirearmRelatedDeathsRate)

pguns <- plm.data(guns_2015,index=c("State","Year"))

attach(pguns)
model3<-plm(FirearmRelatedDeathsRate~alcoholism+crime+drinking+drugs+I(drugs*drugs)+gun+poverty, model="pooling", data=pguns)
summary(model3)
# 
# Pooling Model
# 
# Call:
#   plm(formula = FirearmRelatedDeathsRate ~ alcoholism + crime + 
#         drinking + drugs + I(drugs * drugs) + gun + poverty, data = pguns, 
#       model = "pooling")
# 
# Balanced Panel: n=51, T=12, N=612
# 
# Residuals :
#   Min.    1st Qu.     Median    3rd Qu.       Max. 
# -0.9100464 -0.1884881  0.0084904  0.1827298  1.6556053 
# 
# Coefficients :
#   Estimate  Std. Error  t-value  Pr(>|t|)    
# (Intercept)       0.60351279  0.15280064   3.9497 8.749e-05 ***
#   alcoholism       -0.05477599  0.01018810  -5.3765 1.087e-07 ***
#   crime             0.01976481  0.00384385   5.1419 3.679e-07 ***
#   drinking         -0.05187601  0.00298916 -17.3547 < 2.2e-16 ***
#   drugs             0.04600438  0.01320266   3.4845 0.0005288 ***
#   I(drugs * drugs) -0.00092188  0.00020749  -4.4430 1.056e-05 ***
#   gun               0.00953015  0.00086502  11.0172 < 2.2e-16 ***
#   poverty          -0.06079056  0.01466563  -4.1451 3.882e-05 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Total Sum of Squares:    111.7
# Residual Sum of Squares: 58.505
# R-Squared:      0.47623
# Adj. R-Squared: 0.47016
# F-statistic: 78.4536 on 7 and 604 DF, p-value: < 2.22e-16


model4<-plm(FirearmRelatedDeathsRate~alcoholism+I(alcoholism*alcoholism)+crime+gun+marijuana+recession, model="within", data=pguns)
summary(model4)
# 
# Oneway (individual) effect Within Model
# 
# Call:
#   plm(formula = FirearmRelatedDeathsRate ~ alcoholism + I(alcoholism * 
#                                                             alcoholism) + crime + gun + marijuana + recession, data = pguns, 
#       model = "within")
# 
# Balanced Panel: n=51, T=12, N=612
# 
# Residuals :
#   Min.    1st Qu.     Median    3rd Qu.       Max. 
# -0.9402281 -0.0520992 -0.0022707  0.0441439  1.0204039 
# 
# Coefficients :
#   Estimate  Std. Error t-value Pr(>|t|)   
# alcoholism                  0.01388048  0.01272718  1.0906 0.275915   
# I(alcoholism * alcoholism) -0.00439877  0.00151827 -2.8972 0.003913 **
#   crime                      -0.00880015  0.00337106 -2.6105 0.009285 **
#   gun                         0.00218477  0.00069609  3.1386 0.001787 **
#   marijuana                   0.00518229  0.00170289  3.0432 0.002451 **
#   recession                  -0.00964002  0.00359804 -2.6792 0.007598 **
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Total Sum of Squares:    14.032
# Residual Sum of Squares: 12.342
# R-Squared:      0.12045
# Adj. R-Squared: 0.031701
# F-statistic: 12.6672 on 6 and 555 DF, p-value: 2.0279e-13
saveRDS(model3,"")


