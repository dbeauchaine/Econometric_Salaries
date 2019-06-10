setwd("L:/Dan/Google Drive/School/EC476/Project")
#load libraries
library(Lahman)
library(dplyr)


##########################
#### 1. Data Prep ########
##########################

#create dataset
data("Batting")


#load salaries
salaries <- Salaries %>%
  select(playerID, yearID, teamID, salary)

#load people information
peopleInfo <- People %>%
  select(playerID, birthYear, birthMonth, nameLast,
         nameFirst, bats)


#create batting stats and join salaries and peopleinfo into batting
batting <- Batting %>%
  left_join(salaries,
            by =c("playerID", "yearID", "teamID")) %>%
  left_join(peopleInfo, by = "playerID") %>%
  #calculate age by year and birthyear
  mutate(age = yearID - birthYear -
           1L *(birthMonth >= 10)) %>%
  #rearrange data
  arrange(playerID, yearID, stint)


rm("Batting")
rm("peopleInfo")
rm("salaries")

attach(batting)

#isolate 1999-present
batting <- batting[which(batting$yearID>1998),]
detach(batting)

#make salaries in terms of 1000s of dollars
batting$salary <- batting$salary/1000


#function to find all players with more than one stint in a season
#combine stints and catch the position of each stint>1 in return integer matrix
attach(batting)
return = as.integer(NULL)

#loop all rows
for(row in 1:nrow(batting)){
  if(stint[row]>1) #check for stint>1
  {
    temp<- (row-stint[row]+1) #calculate position of stint 1
  
    #add the values of all stints to stint 1
    batting[temp,6:22] <- (batting[temp,6:22] + batting[row,6:22])
    #catch all stints > 2 for deletion
    return <- c(return, row) 
  }
}
#use return from function to delete all stints > 1
batting <- batting[-return,]
rm("return")
rm("row")
rm("temp")
detach(batting)

#remove stint, teamID, birth year, and birth month
batting <- batting[-c(3,4,24,25)]
attach(batting)
#remove observations without salary information
batting <- batting[which(!is.na(batting$salary)),]
detach(batting)


#label data
names(batting) <- c("ID", "year","league",
                    "G","AB","R","H","X2B","X3B","HR",
                    "RBI","SB","CS","BB","K","IBB","HBP",
                    "SH","SF","GIDP","salary", "last_name"
                    ,"first_name", "bats","age")

##########################
#### 2. Generate Data ####
##########################

#Generate Singles
batting$H1B <- as.integer(batting$H - batting$X2B - batting$X3B - batting$HR)

#Generate Batting Average
batting$AVG <-as.double(batting$H/batting$AB)

#Generate On Base Percentage
batting$OBP <- as.double((batting$H+batting$BB+batting$HBP)/
                                (batting$AB+batting$BB+batting$HBP+batting$SF))
#Genereate Slugging Percentage
batting$SLG <- as.double((batting$H1B + 2*batting$X2B + 3*batting$X3B 
                               +4*batting$HR)/batting$AB)
#Generate On Base + Slugging
batting$OPS <- as.double(batting$OBP+batting$SLG)

#Generate Batting Average on Balls in Play
batting$BABIP <- (batting$H-batting$HR)/(batting$AB-batting$K-batting$HR+batting$SF)

#load WOBA data per year
woba <- read.csv("wOBA_weights.csv")
names(woba) <- c("season","wOBA","wOBAScale","wBB", "wHBP","w1B","w2B","w3B"
                 ,"wHR","runSB","runCS","R.PA","R.W","cFIP")

#create variable for wOBA and wRAA
batting$wOBA <- NA
batting$wRAA <- NA
batting$PA <- batting$AB + batting$SF + batting$SH + batting$HBP + batting$IBB +batting$BB
batting$uBB <- batting$BB - batting$IBB
#Calculate Weighted On Base Average based on yearly weights
for(row in 1:nrow(batting)){
  year <- batting$year[row] #use the observation year
  wobavals <-woba[which(woba$season==year),] #load woba values for that year
  #use woba weights to calculate woba for the observation
  batting[row,]$wOBA <- ((wobavals$wBB*batting[row,]$uBB + wobavals$wHBP*batting[row,]$HBP
                   +wobavals$w1B*batting[row,]$H1B + wobavals$w2B*batting[row,]$X2B
                   +wobavals$w3B*batting[row,]$X3B + wobavals$wHR*batting[row,]$HR)/
                     (batting[row,]$AB + batting[row,]$BB - batting[row,]$IBB + 
                        batting[row,]$SF+batting[row,]$HBP))
  batting[row,]$wRAA <- as.double(((batting[row,]$wOBA-wobavals$wOBA)/
                                    wobavals$wOBAScale) * batting[row,]$PA)
}
rm("row")
rm("year")
rm("woba")
rm("wobavals")


#remove entries with less than 100 AB
batting <-batting[-which(batting$AB < 100),]



#create a variable for last year's salary
batting$last_salary <- NA
for(row in 2:nrow(batting)){
  year <-batting$year[row] #use the observation year
  if(batting$year[(row-1)] == year-1 & batting$ID[row] == batting$ID[(row-1)]){
    batting$last_salary[row] = batting$salary[(row-1)]
  }
  
}

#use the CPI to create real salaries
library(WDI)
#download CPI
CPI <- WDI(indicator = "CPTOTSAXN", start = 1999,
           end = 2016)
names(CPI)[3] <- "price"

#calculate real salary
batting$real_salary <- NA
for(row in 1:nrow(batting)){
  year <- batting$year[row]
  batting$real_salary[row] <- (batting$salary[row]/CPI[which(CPI$country == "United States"
                                                      & CPI$year == year),]$price)*100
}

#create a variable for last year's real salary
batting$real_last_salary <- NA
for(row in 2:nrow(batting)){
  year <-batting$year[row] #use the observation year
  if(batting$year[(row-1)] == year-1 & batting$ID[row] == batting$ID[(row-1)]){
    batting$real_last_salary[row] = batting$real_salary[(row-1)]
  }
  
}

#generate squared terms to check for functional form
batting$age2 <- batting$age^2

#generate different datsets for different years and 
batting2000 <- batting[which(batting$year == 2000),]

batting2002 <- batting[which(batting$year == 2002),]                                          

batting2004 <- batting[which(batting$year == 2004),]

batting2008 <- batting[which(batting$year == 2008),]

batting2016 <- batting[which(batting$year == 2016),]

#generate different datasets for different ages

batting2016_old <- batting2016[which(batting2016$age>29),]

batting2016_young <-batting2016[which(batting2016$age<30),]



##########################
#### 3. Regressions #####
##########################

library(lmtest)
library(sandwich)



#create subsets based on age
attach(batting)
batting_old <- batting[which(age>29),]
detach(batting)

attach(batting)
batting_young <- batting[which(age<30),]
detach(batting)

#linear regressions on salary


lm_2000 <- lm(salary ~ wRAA + age + age2 + last_salary, data = batting2000)
summary(lm_2000)
lm_2000_rob <- coeftest(lm_2000, vcov = vcovHC(lm_2000, type= "HC4"))

lm_2002 <- lm(salary ~ wRAA + age + age2 + last_salary, data = batting2002)
summary(lm_2002)
lm_2002_rob <- coeftest(lm_2002, vcov = vcovHC(lm_2002, type= "HC4"))

lm_2004 <- lm(salary ~ wRAA + age + age2 + last_salary, data = batting2004)
summary(lm_2004)
lm_2004_rob <- coeftest(lm_2004, vcov = vcovHC(lm_2000, type= "HC4"))

lm_2008 <- lm(salary ~ wRAA + age + age2 + last_salary, data = batting2008)
summary(lm_2008)
lm_2008_rob <- coeftest(lm_2008, vcov = vcovHC(lm_2008, type= "HC4"))

lm_2016 <- lm(salary ~ wRAA + age + age2 + last_salary, data = batting2016)
summary(lm_2016)

#for use in crPlots
lm_2016_age <- lm(salary ~ wRAA + age + last_salary, data = batting2016)


lm_2016_rob <- coeftest(lm_2016, vcov = vcovHC(lm_2016, type= "HC4"))

lm_2016_seperated <- lm(salary ~ AB + H1B + X2B + X3B + HR + BB + K + R + RBI + SB
                        + age + age2 + last_salary, data = batting2016)
summary(lm_2016_seperated)
lm_2016_seperated_rob <- coeftest(lm_2016_seperated, 
                                  vcov = vcovHC(lm_2016_seperated, type= "HC4"))

#linear regressions based on real salary
#test individual variables
lm_wRAA <- lm(real_salary ~ wRAA, data = batting2016)
summary(lm_wRAA)
lm_wRAA_rob <- coeftest(lm_wRAA, vcov = vcovHC(lm_wRAA, type="HC4"))

lm_Age <- lm(real_salary ~ age + age2, data = batting2016)
summary(lm_Age)
lm_Age_rob <- coeftest(lm_Age, vcov = vcovHC(lm_Age, type="HC4"))

lm_last_salary <- lm(real_salary ~ real_last_salary, data = batting2016)
summary(lm_last_salary)
lm_last_rob <- coeftest(lm_last_salary, vcov = vcovHC(lm_last_salary, type="HC4"))

lm_real2000 <- lm(real_salary ~ wRAA + age + age2 + real_last_salary, data = batting2000)
summary(lm_real2000)
lm_real2000_rob <- coeftest(lm_real2000, vcov = vcovHC(lm_real2000, type= "HC4"))

lm_real2002 <- lm(real_salary ~ wRAA + age + age2 + real_last_salary, data = batting2002)
summary(lm_real2002)
lm_real2002_rob <- coeftest(lm_real2002, vcov = vcovHC(lm_real2002, type= "HC4"))

lm_real2004 <- lm(real_salary ~ wRAA + age + age2 + real_last_salary, data = batting2004)
summary(lm_real2004)
lm_real2004_rob <- coeftest(lm_real2004, vcov = vcovHC(lm_real2000, type= "HC4"))

lm_real2008 <- lm(real_salary ~ wRAA + age + age2 + real_last_salary, data = batting2008)
summary(lm_2008)
lm_real2008_rob <- coeftest(lm_real2008, vcov = vcovHC(lm_real2008, type= "HC4"))

lm_real2016 <- lm(real_salary ~ wRAA + age + age2 + real_last_salary, data = batting2016)
summary(lm_real2016)
lm_real2016_rob <- coeftest(lm_real2016, vcov = vcovHC(lm_real2016, type= "HC4"))

lm_real2016_seperated <- lm(real_salary ~ AB + H1B + X2B + X3B + HR + BB + K + R + RBI + SB
                        + age + age2 + real_last_salary, data = batting2016)
summary(lm_real2016_seperated)
lm_real2016_seperated_rob <- coeftest(lm_real2016_seperated, 
                                  vcov = vcovHC(lm_real2016_seperated, type= "HC4"))
#create regression based on age
lm_real2016_old <- lm(real_salary ~ wRAA + age + age2 + real_last_salary, data = batting2016_old)
summary(lm_real2016_old)
lm_real2016_old_rob <- coeftest(lm_real2016_old, vcov = vcovHC(lm_real2016_old, type= "HC4"))

lm_real2016_young <- lm(real_salary ~ wRAA + age + age2 + real_last_salary, data = batting2016_young)
summary(lm_real2016_young)
lm_real2016_young_rob <- coeftest(lm_real2016_young, vcov = vcovHC(lm_real2016_young, type= "HC4"))


#write to formatted HTML Files
library(stargazer)

#on salary
stargazer(lm_2000, lm_2000_rob, lm_2008, lm_2008_rob, lm_2016, lm_2016_rob, type="html",
          title = "Comparison Between Years",
          dep.var.labels = c("2000", "Robust SE", "2008", "Robust SE",
                             "2016", "Robust SE"),
          covariate.labels = c("Weighted Runs Above Average",
                               "Age",
                               "Age<sup>2</sup>",
                               "Last Year's Salary:"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          no.space= TRUE,
          out="nominal_Year_ols.htm")

stargazer(lm_2016_old, lm_2016_old_rob,lm_2016_young, lm_2016_young_rob, type="html",
          title = "Comparison Between Player Age",
          dep.var.labels = c("30 Years and Older", "Robust SE", "29 Years and Younger", "Robust SE"),
          covariate.labels = c("Weighted Runs Above Average",
                               "Age",
                               "Age<sup>2</sup>",
                               "Last Year's Salary:"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          no.space= TRUE,
          out="nominal_Age_ols.htm")

stargazer(lm_2002, lm_2002_rob,lm_2004, lm_2004_rob, type="html",
          title = "Before and After MoneyBall",
          dep.var.labels = c("2002 (Before MoneyBall released)", "Robust SE",
                             "2004 (After MoneyBall Released)", "Robust SE"),
          covariate.labels = c("Weighted Runs Above Average",
                               "Age",
                               "Age<sup>2</sup>",
                               "Last Year's Salary:"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          no.space= TRUE,
          out="nominal_moneyball_ols.htm")

#on real salary
stargazer(lm_real2000, lm_real2000_rob, lm_real2008, lm_real2008_rob, lm_real2016, lm_real2016_rob, type="html",
          title = "Comparison Between Years in real $",
          dep.var.labels = c("2000", "Robust SE", "2008", "Robust SE",
                             "2016", "Robust SE"),
          covariate.labels = c("Weighted Runs Above Average",
                               "Age",
                               "Age<sup>2</sup>",
                               "Last Year's Salary:"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          no.space= TRUE,
          out="real_Year_ols.htm")

stargazer(lm_real2016_old, lm_real2016_old_rob,lm_real2016_young, lm_real2016_young_rob, type="html",
          title = "Comparison Between Player Age in Real $",
          dep.var.labels = c("30 Years and Older", "Robust SE", "29 Years and Younger", "Robust SE"),
          covariate.labels = c("Weighted Runs Above Average",
                               "Age",
                               "Age<sup>2</sup>",
                               "Last Year's Salary:"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          no.space= TRUE,
          out="real_Age_ols.htm")

stargazer(lm_real2002, lm_real2002_rob,lm_real2004, lm_real2004_rob, type="html",
          title = "Before and After MoneyBall in Real $",
          dep.var.labels = c("2002 (Before MoneyBall released)", "Robust SE",
                             "2004 (After MoneyBall Released)", "Robust SE"),
          covariate.labels = c("Weighted Runs Above Average",
                               "Age",
                               "Age<sup>2</sup>",
                               "Last Year's Salary:"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          no.space= TRUE,
          out="real_moneyball_ols.htm")

stargazer(lm_wRAA, lm_wRAA_rob, lm_last_salary, lm_last_rob, lm_Age,lm_Age_rob, type="html",
          title = "Variables Individually Compared",
          dep.var.labels = c("Weighted Runs Above", "Robust SE",
                             "Last Year's Salary", "Robust SE",
                             "Age", "Robust SE"),
          
          covariate.labels = c("Weighted Runs Above Average",
                               "Last Year's Salary",
                               "Age",
                               "Age<sup>2</sup>"),
          
          star.cutoffs = c(0.05, 0.01, 0.001),
          no.space= TRUE,
          out="var_compare_ols.htm")

##########################
######## 4. Plots ########
##########################

#create plots to investigate functional form and check for heteroskedasticity
pdf("Project_graphs.pdf")

#histograms
hist(batting2016$salary, breaks=50, xlab = "$1000 Dollars", main = "Salary Frequency (2016)", col = "blue")
hist(batting2016$age, breaks=30, xlab = "Years", main = "Player Age Frequency (2016)")

#scatterplot the relationship between salary and performance
with(batting2016,{
  plot(wRAA, salary, xlab = "wRAA", ylab = "salary ($1000s)", main = "wRAA by Salary")
  lines(loess.smooth(wRAA, salary), col="blue", lty=2, lwd=2)  
})

with(batting2016,{
  plot(age, salary, xlab = "age", ylab = "salary ($1000s)", main = "Age by Salary")
  lines(loess.smooth(age, salary), col = "blue", lty=2, lwd=2)
})

with(batting2016,{
  plot(last_salary, salary, xlab = "Last Year's Salary ($1000s)"
       , ylab = "salary ($1000s)", main = "Last Year's Salary by Current Salary")
  lines(loess.smooth(last_salary, salary), col = "blue", lty=2, lwd=2)
})

library(hexbin)
with(batting2016,{
bin<-hexbin(wRAA, salary, xbins=50)
plot(bin, main="wRAA by Salary", ylab = NULL)
})

#nominal relationship
library(gplots)
plotmeans(salary ~ year, main = "Player Salaries (Nominal $)", n.label = FALSE,
          data = batting, xlab = "Year", ylab ="Salary ($1000s)")

#real relationship

plotmeans(real_salary ~ year, main = "Player Salaries (Real $)", n.label = FALSE,
          data = batting, xlab = "Year", ylab ="Salary ($1000s)")

#check functional form of independent variables
library(car)
crPlots(lm_real2016)

crPlots(lm_real2016_age)

#check functional form of dependent variable
residualPlot(lm_real2016)

dev.off()

#check for hederoskedasticity
ncvTest(lm_real2016)








