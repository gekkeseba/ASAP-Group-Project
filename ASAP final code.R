#Group Assignment ASAP
setwd("/Users/seba/rcode")
filepathpc <-"/Users/seba/rcode"

codedir <- paste0(filepathpc, "/code/")
datadir <- paste0(filepathpc, "/data/")
outputdir <- paste0(filepathpc, "/output/")
install.packages("regclass")
install.packages("tidyr")
install.packages("tidyquant")
install.packages("Hmisc")
install.packages("car")
install.packages("SciViews")
install.packages("DescTools")
install.packages("usdm")
install.packages("lmtest")
install.packages("plyr")
install.packages("fastDummies")
install.packages("lmtest")
install.packages("sandwich")
install.packages("pacman")
install.packages("dplyr")
install.packages("Rcpp")
install.packages("tsDyn")
install.packages("MASS")
install.packages("AER")
install.packages("pscl")
install.packages("extraDistr")
install.packages("plm")
install.packages("DescTools")
install.packages("xtable")
library(AER)
library(MASS) 
library(lmtest) 
library(sandwich) 
library(fastDummies) 
library(plyr) 
library(dplyr) 
library(plm) 
library(stargazer) 
library(SciViews) 
library(DescTools) 
library(lmtest) 
library(plm) 
library(pscl)
library(ggplot2)
library(xtable)
library(extraDistr)

#Importing the dataset
df_vt <-  read.csv(paste0(datadir,"coronavirus-data-explorer.csv"), 
                                  header = TRUE, sep=",")
df_vt_europe <- 
  as.data.frame(filter(df_vt, df_vt$location == 'Austria' | 
                         df_vt$location == 'Bulgaria'| df_vt$location == 'Belgium' |
                         df_vt$location == 'Croatia'| df_vt$location == 'Cyprus'|
                         df_vt$location == 'Czechia'|df_vt$location == 'Denmark'|
                         df_vt$location == 'Estonia'|df_vt$location == 'Finland'|
                         df_vt$location == 'France'|df_vt$location == 'Germany'|
                         df_vt$location == 'Greece'|df_vt$location == 'Sweden'|
                         df_vt$location == 'Hungary'|df_vt$location == 'Ireland'|
                         df_vt$location == 'Italy'|df_vt$location == 'Latvia'|
                         df_vt$location == 'Liechtenstein'|df_vt$location == 'Lithuania'|
                         df_vt$location == 'Luxembourg'|df_vt$location == 'Malta'|
                         df_vt$location == 'Netherlands'|df_vt$location == 'Norway'|
                         df_vt$location == 'Poland'|df_vt$location == 'Portugal'|
                         df_vt$location == 'Romania'|df_vt$location == 'Slovakia'|
                         df_vt$location == 'Slovenia'|df_vt$location == 'Spain'))
#Making the year variable
df_vt_europe$year <- substr(df_vt_europe$date, 1, 4 )

#Filter the data in order to only keep the year 2021 
df_vt_europe_2021 <- filter(df_vt_europe, df_vt_europe$year == 2021)

#Format the time variable correctly and only keep full weeks containing 7 days
df_vt_europe_2021$weeknumber <- strftime(df_vt_europe_2021$date, format = "%V")
df_vt_europe_2021 <- df_vt_europe_2021 %>% filter(df_vt_europe_2021$date >= "2021-01-04")
df_vt_europe_2021 <- df_vt_europe_2021 %>% filter(df_vt_europe_2021$date <= "2021-09-26")

#Aggregating on week and selecting the variables of interest
df_vt_agg <- 
  aggregate(list(df_vt_europe_2021$people_fully_vaccinated, df_vt_europe_2021$total_cases, 
                 df_vt_europe_2021$new_cases, df_vt_europe_2021$new_deaths, 
                 df_vt_europe_2021$reproduction_rate, df_vt_europe_2021$positive_rate,
                 df_vt_europe_2021$people_fully_vaccinated_per_hundred, df_vt_europe_2021$stringency_index, 
                 df_vt_europe_2021$population_density, df_vt_europe_2021$aged_65_older, 
                 df_vt_europe_2021$median_age,
                 df_vt_europe_2021$gdp_per_capita, df_vt_europe_2021$cardiovasc_death_rate,
                 df_vt_europe_2021$diabetes_prevalence,
                 df_vt_europe_2021$female_smokers, df_vt_europe_2021$male_smokers,
                 df_vt_europe_2021$hospital_beds_per_thousand, df_vt_europe_2021$life_expectancy,
                 df_vt_europe_2021$human_development_index),by=list(df_vt_europe_2021$weeknumber, df_vt_europe_2021$location), mean)

#Renaming all the collumns to meaningful names
colnames(df_vt_agg)[colnames(df_vt_agg) == "Group.1"]       <- "time" 
colnames(df_vt_agg)[colnames(df_vt_agg) == "Group.2"]       <- "group" 
colnames(df_vt_agg)[colnames(df_vt_agg) == 
                      "c.NA..NA..NA..NA..NA..NA..NA..NA..NA..NA..NA..NA..12..388..1281.."]<-
  "People_Fully_Vaccinated" 
colnames(df_vt_agg)[colnames(df_vt_agg) == 
                      "c.1642..2311..2469..2540..2063..2278..1651..1536..1575..1917.."]       <- 
  "New_Cases" 
colnames(df_vt_agg)[colnames(df_vt_agg) ==
                      "c.0.99..1.02..1.02..1..1..0.99..0.98..0.96..0.95..0.96..0.97.."]       <- 
  "Reproduction_Rate" 
colnames(df_vt_agg)[colnames(df_vt_agg) == 
                      "c.NA..NA..NA..NA..NA..NA..NA..NA..NA..NA..NA..NA..NA..NA..NA.."]       <- 
  "Positive_Rate" 
colnames(df_vt_agg)[colnames(df_vt_agg) ==
                      "c.NA..NA..NA..NA..NA..NA..NA..NA..NA..NA..NA..NA..0..0..0.01.."]       <- 
  "People_Fully_Vaccinated_Per_Hundred" 
colnames(df_vt_agg)[colnames(df_vt_agg) == 
                      "c.82.41..82.41..82.41..82.41..82.41..82.41..82.41..82.41..82.41.."]       <- 
  "Stringency_Index" 
colnames(df_vt_agg)[colnames(df_vt_agg) == 
                      "c.106.749..106.749..106.749..106.749..106.749..106.749..106.749.."]       <- 
  "Population_Density" 
colnames(df_vt_agg)[colnames(df_vt_agg) == 
                      "c.19.202..19.202..19.202..19.202..19.202..19.202..19.202..19.202.."]       <- 
  "Aged_65_Older" 
colnames(df_vt_agg)[colnames(df_vt_agg) == 
                      "c.45436.686..45436.686..45436.686..45436.686..45436.686..45436.686.."]       <- 
  "gdp_per_capita" 
colnames(df_vt_agg)[colnames(df_vt_agg) == 
                      "c.6.35..6.35..6.35..6.35..6.35..6.35..6.35..6.35..6.35..6.35.."  ]    <- 
  "diabetes_prevalence" 
colnames(df_vt_agg)[colnames(df_vt_agg) == 
                      "c.28.4..28.4..28.4..28.4..28.4..28.4..28.4..28.4..28.4..28.4.."  ]    <- 
  "female_smoker" 
colnames(df_vt_agg)[colnames(df_vt_agg) == 
                      "c.30.9..30.9..30.9..30.9..30.9..30.9..30.9..30.9..30.9..30.9.." ]    <- 
  "male_smoker" 
colnames(df_vt_agg)[colnames(df_vt_agg) == 
                      "c.7.37..7.37..7.37..7.37..7.37..7.37..7.37..7.37..7.37..7.37.."  ]    <- 
  "hospital_beds_per_thousand" 
colnames(df_vt_agg)[colnames(df_vt_agg) == 
                      "c.81.54..81.54..81.54..81.54..81.54..81.54..81.54..81.54..81.54.."  ]    <- 
  "life_expectancy" 


#Creating a balanced panel data set with the least amount of na's by selecting on weeks and countries
  #Renaming the weeks to numeric characters so the 0 in front is not present anymore for the weeks < 10
df_vt_agg$time <- as.numeric(df_vt_agg$time)

df_vt_agg$time <- ifelse(df_vt_agg$time == "01", 1, df_vt_agg$time)
df_vt_agg$time <- ifelse(df_vt_agg$time == "02", 2, df_vt_agg$time)
df_vt_agg$time <- ifelse(df_vt_agg$time == "03", 3, df_vt_agg$time)
df_vt_agg$time <- ifelse(df_vt_agg$time == "04", 4, df_vt_agg$time)
df_vt_agg$time <- ifelse(df_vt_agg$time == "05", 5, df_vt_agg$time)
df_vt_agg$time <- ifelse(df_vt_agg$time == "06", 6, df_vt_agg$time)
df_vt_agg$time <- ifelse(df_vt_agg$time == "07", 7, df_vt_agg$time)
df_vt_agg$time <- ifelse(df_vt_agg$time == "08", 8, df_vt_agg$time)
df_vt_agg$time <- ifelse(df_vt_agg$time == "09", 9, df_vt_agg$time)

#Checking the time variable (week) it's frequency
table(df_vt_agg$time)

#Removing the rows with NA's for the dependent variable
df_vt_agg_2 <- df_vt_agg[!is.na(df_vt_agg["People_Fully_Vaccinated"]),]

#Checking the frequencies in the number of weeks for each country
df_vt_agg_subset <- as.data.frame(table(df_vt_agg_2$group))
##df_vt_agg_frequency_subset <- subset(df_vt_agg_subset, df_vt_agg_subset$Freq >= 30)

#Checking frequencies in the number of countries
df_vt_agg_subset <- as.data.frame(table(df_vt_agg_2$time))

  #Conclusion: select the weeks ranging for 5 to 24
df_vt_final <- filter(df_vt_agg_2, df_vt_agg_2$time >= 5 & df_vt_agg_2$time <= 24)

#Checking the data
stargazer(df_vt_final, type = "text")

#We want to include the following countries based on the NA's
SubsetCountries <- c('Austria', 'Bulgaria', 'Belgium', 'Czechia', 'Denmark', 'Estonia',  #without france, germany and liechtenstein country subset
                     'Italy','Latvia','Lithuania','Malta','Portugal','Romania','Slovakia',
                     'Slovenia', 'Norway')
  #Keep only the countries defined in the subset above
df_vt_final <- df_vt_final[df_vt_final$group %in% SubsetCountries,]

#-------------------------------------------------------------------------------
#Data Check (assumptions)

#Check if it a balanced panel 
is.pbalanced(df_vt_final)

  #Conclusion: Final data set is balanced without NA's

#check the histograms of all the varaibles in order to see whether they have a normal distribution
hist(df_vt_final$People_Fully_Vaccinated_Per_Hundred)
hist(df_vt_final$People_Fully_Vaccinated) #positive skewed
hist(df_vt_final$New_Cases) #positive skewed
hist(df_vt_final$Reproduction_Rate) #normal distributed
hist(df_vt_final$Positive_Rate) #positive skewed
hist(df_vt_final$Stringency_Index) # relatively normal
hist(df_vt_final$Population_Density) #positive skewed
hist(df_vt_final$Aged_65_Older) # sort of normal distributed
hist(df_vt_final$gdp_per_capita) #positive / normal skewed
hist(df_vt_final$diabetes_prevalence) #positive skewed 
hist(df_vt_final$female_smoker) #normal / positive skewed
hist(df_vt_final$male_smoker) #normal distributed
hist(df_vt_final$hospital_beds_per_thousand) #positive skewed
hist(df_vt_final$life_expectancy) #positive skewed

#transforming the data into normal distributions
df_vt_final$People_Fully_Vaccinated_Per_Hundred_ln <- ln(df_vt_final$People_Fully_Vaccinated_Per_Hundred)
df_vt_final$People_Fully_Vaccinated_ln <- ln(df_vt_final$People_Fully_Vaccinated)
df_vt_final$New_Cases_ln <- ln(df_vt_final$New_Cases)
df_vt_final$Population_Density_ln <- ln(df_vt_final$Population_Density)
df_vt_final$diabetes_prevalence_ln <- ln(df_vt_final$diabetes_prevalence)
df_vt_final$hospital_beds_per_thousand_ln <- ln(df_vt_final$hospital_beds_per_thousand)
df_vt_final$life_expectancy_ln <- ln(df_vt_final$life_expectancy)
df_vt_final$Positive_Rate_ln_percpoint <- ln(100*df_vt_final$Positive_Rate)

#Checking whether the newly made variables are indeed more normal distributed
hist(df_vt_final$People_Fully_Vaccinated_Per_Hundred_ln)
hist(df_vt_final$People_Fully_Vaccinated_ln)
hist(df_vt_final$New_Cases_ln) #winsozir
hist(df_vt_final$Reproduction_Rate) #winsorize
hist(df_vt_final$Positive_Rate_ln_percpoint) #winsorize
hist(df_vt_final$Stringency_Index) 
hist(df_vt_final$Population_Density_ln) #winsorize
hist(df_vt_final$Aged_65_Older) 
hist(df_vt_final$gdp_per_capita) #winsorize
hist(df_vt_final$diabetes_prevalence_ln)  
hist(df_vt_final$female_smoker)
hist(df_vt_final$male_smoker) #winsorize
hist(df_vt_final$hospital_beds_per_thousand_ln) #winsorize
hist(df_vt_final$life_expectancy_ln) #winsorize
stargazer(df_vt_final, type = 'text')

#Winsorize the variables which showed a non-normal distributed histogram
df_vt_final$People_Fully_Vaccinated_Per_Hundred_ln_wins <- Winsorize(df_vt_final$People_Fully_Vaccinated_Per_Hundred_ln, minval = NULL, maxval = NULL, probs = c(0.01, 0.99)) 
df_vt_final$New_Cases_ln_wins <- Winsorize(df_vt_final$New_Cases_ln, minval = NULL, maxval = NULL, probs = c(0.01, 0.99))
df_vt_final$Reproduction_Rate_wins <- Winsorize(df_vt_final$Reproduction_Rate, minval = NULL, maxval = NULL, probs = c(0.01, 0.99))
df_vt_final$Positive_Rate_ln_percpoint_wins <- Winsorize(df_vt_final$Positive_Rate_ln_percpoint, minval = NULL, maxval = NULL, probs = c(0.01, 0.99))
df_vt_final$Population_Density_ln_wins <- Winsorize(df_vt_final$Population_Density_ln, minval = NULL, maxval = NULL, probs = c(0.01, 0.99))
df_vt_final$gdp_per_capita_wins <- Winsorize(df_vt_final$gdp_per_capita, minval = NULL, maxval = NULL, probs = c(0.01, 0.99))
df_vt_final$male_smoker_wins <- Winsorize(df_vt_final$male_smoker, minval = NULL, maxval = NULL, probs = c(0.01, 0.99))
df_vt_final$hospital_beds_per_thousand_ln_wins <- Winsorize(df_vt_final$hospital_beds_per_thousand_ln, minval = NULL, maxval = NULL, probs = c(0.01, 0.99))
df_vt_final$life_expectancy_ln_wins <- Winsorize(df_vt_final$life_expectancy_ln, minval = NULL, maxval = NULL, probs = c(0.01, 0.99))

#Check if the winsorized histograms are actually more normal distributed
hist(df_vt_final$People_Fully_Vaccinated_Per_Hundred_ln_wins)
hist(df_vt_final$People_Fully_Vaccinated_ln)
hist(df_vt_final$New_Cases_ln_wins) #winsozir
hist(df_vt_final$Reproduction_Rate_wins) #winsorize
hist(df_vt_final$Positive_Rate_ln_percpoint_wins) #winsorize
hist(df_vt_final$Stringency_Index) 
hist(df_vt_final$Population_Density_ln_wins) #winsorize
hist(df_vt_final$Aged_65_Older) 
hist(df_vt_final$gdp_per_capita_wins) #winsorize
hist(df_vt_final$diabetes_prevalence_ln)  
hist(df_vt_final$female_smoker)
hist(df_vt_final$male_smoker_wins) #winsorize
hist(df_vt_final$hospital_beds_per_thousand_ln_wins) #winsorize
hist(df_vt_final$life_expectancy_ln_wins) #winsorize

#Create the final subset of data, ready for the panel regression estimation
df_vt_final_panreg <- df_vt_final[c("time", "group", "People_Fully_Vaccinated_Per_Hundred_ln",
                                    "New_Cases_ln_wins","Reproduction_Rate_wins", "Positive_Rate_ln_percpoint_wins",
                                    "Stringency_Index", "Population_Density_ln_wins", "Aged_65_Older" ,
                                    "gdp_per_capita_wins", "diabetes_prevalence_ln", "female_smoker", 
                                    "male_smoker_wins", "hospital_beds_per_thousand_ln_wins", "life_expectancy_ln_wins")]

#Descriptive statistics
stargazer(df_vt_final_panreg, type = 'text')

#Panel model specification and estimation in order to be able to check the VIF
mdlA <- People_Fully_Vaccinated_Per_Hundred_ln ~ New_Cases_ln_wins + 
  Reproduction_Rate_wins + Positive_Rate_ln_percpoint_wins + Stringency_Index + 
  Population_Density_ln_wins + Aged_65_Older + gdp_per_capita_wins + 
  diabetes_prevalence_ln + female_smoker + male_smoker_wins +
  hospital_beds_per_thousand_ln_wins + life_expectancy_ln_wins

rsltpanelA <- plm(mdlA, data = df_vt_final_panreg, index = c("group", "time"), model = "pooling")
stargazer(rsltpanelA, type = "text")
#Correlation matrix is constructed in order to check for multicollinearity
str(df_vt_final_panreg)
correlation_matrix_df <- as.data.frame(cor(df_vt_final_panreg[3:15], use = "complete.obs"))

#conclusion, male and female smokers are highly correlatd and hece we have to aggregate these two variables
df_vt_final_panreg$percentage_smokers <- (df_vt_final_panreg$female_smoker + df_vt_final_panreg$male_smoker_wins)/2

#Create a new model with the smokers combined for the descriptive statistics
df_vt_final_panreg_smokers_ds <- df_vt_final_panreg[c("time", "group", "People_Fully_Vaccinated_Per_Hundred_ln",
                                    "New_Cases_ln_wins","Reproduction_Rate_wins", "Positive_Rate_ln_percpoint_wins",
                                    "Stringency_Index", "Population_Density_ln_wins", "Aged_65_Older" ,
                                    "gdp_per_capita_wins", "diabetes_prevalence_ln", "percentage_smokers",
                                    "hospital_beds_per_thousand_ln_wins", "life_expectancy_ln_wins")]

stargazer(df_vt_final_panreg_smokers_ds, font.size = "tiny")

mdlB <- People_Fully_Vaccinated_Per_Hundred_ln ~ New_Cases_ln_wins + 
  Reproduction_Rate_wins + Positive_Rate_ln_percpoint_wins + Stringency_Index + 
  Population_Density_ln_wins + Aged_65_Older + gdp_per_capita_wins + 
  diabetes_prevalence_ln + percentage_smokers +
  hospital_beds_per_thousand_ln_wins + life_expectancy_ln_wins
stargazer(mdlB)

rsltpanelB <- plm(mdlB, data = df_vt_final_panreg, index = c("group", "time"), model = "pooling")
#vif and tolerance
VIF()
VIF(rsltpanelB)
1/VIF(rsltpanelB)

#scatterplot matrix


#correlation matrix


#Testing for heteroskedasticity
bptest(mdlB, data = df_vt_final_panreg, studentize = F) 
  #conclusion: Heteroskedasticity is present in our model 
    #In order to account for the heteroskedasticity which is present, white standard errors are used
attach(df_vt_final_panreg)
#Testing for autocorrelation
dwtest(mdlB)
  #conclusion: There is autocorrelation present in our model 
    #To account for both, heteroskedasticity and autocorrelation we will include newey west standard errors as well

#-------------------------------------------------------------------------------
#Analysis of the data through various models
#Pooled, Between and Within model preparation

#Model specification
mdlB <- People_Fully_Vaccinated_Per_Hundred_ln ~ New_Cases_ln_wins + 
  Reproduction_Rate_wins + Positive_Rate_ln_percpoint_wins + Stringency_Index + 
  Population_Density_ln_wins + Aged_65_Older + gdp_per_capita_wins + 
  diabetes_prevalence_ln + percentage_smokers +
  hospital_beds_per_thousand_ln_wins + life_expectancy_ln_wins

#Constructing a new dataset which contains the averages of the values in our original data set.
df_vt_final.avg <- 
  ddply(df_vt_final_panreg, .(group), summarise,
        avg.People_Fully_Vaccinated_Per_Hundred_ln = mean(People_Fully_Vaccinated_Per_Hundred_ln, na.rm=TRUE),
        avg.New_Cases_ln_wins       = mean(New_Cases_ln_wins, na.rm=TRUE),
        avg.Reproduction_Rate_wins      = mean(Reproduction_Rate_wins, na.rm=TRUE),
        avg.Positive_Rate_ln_percpoint_wins     = mean(Positive_Rate_ln_percpoint_wins, na.rm=TRUE),
        avg.Stringency_Index   = mean(Stringency_Index, na.rm=TRUE),
        avg.Population_Density_ln_wins = mean(Population_Density_ln_wins, na.rm=TRUE),
        avg.Aged_65_Older   = mean(Aged_65_Older, na.rm=TRUE),
        avg.gdp_per_capita_wins     = mean(gdp_per_capita_wins, na.rm=TRUE),
        avg.diabetes_prevalence_ln      = mean(diabetes_prevalence_ln, na.rm=TRUE),
        avg.percentage_smokers     = mean(percentage_smokers, na.rm=TRUE),
        avg.hospital_beds_per_thousand_ln_wins   = mean(hospital_beds_per_thousand_ln_wins, na.rm=TRUE),
        avg.life_expectancy_ln_wins  = mean(life_expectancy_ln_wins, na.rm=TRUE),
        numValid         = length(group))


df_vt_final_merged_panreg <- merge(df_vt_final_panreg, df_vt_final.avg, by="group")

#Constructing the differences
attach(df_vt_final_merged_panreg)
df_vt_final_merged_panreg$diff.People_Fully_Vaccinated_Per_Hundred_ln   <- People_Fully_Vaccinated_Per_Hundred_ln   - avg.People_Fully_Vaccinated_Per_Hundred_ln
df_vt_final_merged_panreg$diff.New_Cases_ln_wins       <- New_Cases_ln_wins        - avg.New_Cases_ln_wins 
df_vt_final_merged_panreg$diff.Reproduction_Rate_wins       <- Reproduction_Rate_wins      - avg.Reproduction_Rate_wins
df_vt_final_merged_panreg$diff.Positive_Rate_ln_percpoint_wins     <- Positive_Rate_ln_percpoint_wins     - avg.Positive_Rate_ln_percpoint_wins
df_vt_final_merged_panreg$diff.Stringency_Index   <- Stringency_Index   - avg.Stringency_Index
df_vt_final_merged_panreg$diff.Population_Density_ln_wins <- Population_Density_ln_wins - avg.Population_Density_ln_wins
df_vt_final_merged_panreg$diff.Aged_65_Older <- Aged_65_Older - avg.Aged_65_Older
df_vt_final_merged_panreg$diff.gdp_per_capita_wins   <- gdp_per_capita_wins   - avg.gdp_per_capita_wins
df_vt_final_merged_panreg$diff.diabetes_prevalence_ln      <- diabetes_prevalence_ln      - avg.diabetes_prevalence_ln
df_vt_final_merged_panreg$diff.percentage_smokers       <- percentage_smokers       - avg.percentage_smokers
df_vt_final_merged_panreg$diff.hospital_beds_per_thousand_ln_wins    <- hospital_beds_per_thousand_ln_wins    - avg.hospital_beds_per_thousand_ln_wins
df_vt_final_merged_panreg$diff.life_expectancy_ln_wins   <- life_expectancy_ln_wins   - avg.life_expectancy_ln_wins
detach(df_vt_final_merged_panreg)

#test if there are enough amount of observations for every country
df_vt_final_merged_panreg <- df_vt_final_merged_panreg[df_vt_final_merged_panreg$numValid == 20,]
is.pbalanced(df_vt_final_merged_panreg)
  #Conclusion, there still remains a balanced panel data set

# Constructing a dataset containing either the differencers or the averages
mdlVars      <- all.vars(mdlB)
mdlVars.avg  <- paste0("avg.", mdlVars)
mdlVars.diff <- paste0("diff.", mdlVars)

df_vt_final_merged_panreg.between <- df_vt_final_merged_panreg[mdlVars.avg]
df_vt_final_merged_panreg.within  <- df_vt_final_merged_panreg[c(mdlVars.diff)]
str(df_vt_final_merged_panreg)

#Remove the diff. and avg. name from each variable in order to be able to use the same model specification
colnames(df_vt_final_merged_panreg.within) <- 
  gsub("diff\\.", "", colnames(df_vt_final_merged_panreg.within))   
colnames(df_vt_final_merged_panreg.between) <- 
  gsub("avg\\.", "", colnames(df_vt_final_merged_panreg.between))

#Pooled panel model
rslt.Pooled <- plm(mdlB, data = df_vt_final_panreg, index = c("group", "time"), model = "pooling")

stargazer(rslt.Pooled, type = "text")

#Between panel model
rslt.Between <- lm (mdlB, data=df_vt_final_merged_panreg.between)
stargazer(rslt.Between, type = "text")
#Within panel model
rslt.Within <- lm (mdlB, data=df_vt_final_merged_panreg.within)
stargazer(rslt.Within, type = "text")

#Fixed effects panel model
rsltFE.Country <- plm(mdlB, data = df_vt_final_panreg, index=c("group", "time"), model = "within") 
stargazer(rsltFE.Country, type = "text")

#dummies for time fixed effects
df_vt_final_panreg <- dummy_cols(df_vt_final_panreg, select_columns = "time")

mdlB_dummies <- People_Fully_Vaccinated_Per_Hundred_ln ~ New_Cases_ln_wins + 
  Reproduction_Rate_wins + Positive_Rate_ln_percpoint_wins + Stringency_Index + 
  Population_Density_ln_wins + Aged_65_Older + gdp_per_capita_wins + 
  diabetes_prevalence_ln + percentage_smokers +
  hospital_beds_per_thousand_ln_wins + life_expectancy_ln_wins + time_6 +
  time_7 + time_8 + time_9 + time_10 + time_11 + time_12 + time_13 + time_14 +
  time_15 + time_16 + time_17 + time_18 + time_19 + time_20 + time_21 + time_22 + time_23 + time_24

rslt.Pooled_TFE <- plm(mdlB_dummies, data = df_vt_final_panreg, index = c("group", "time"), model = "pooling")
stargazer(rslt.Pooled_TFE, rsltFE.Country, type = "text")

  #Conclusion FE is preferred model
pFtest(rsltFE.Country, rslt.Pooled) 

#Random effects panel model
rsltRE.Country <- plm(mdlB, data = df_vt_final_panreg, index = c("group", "time"), model = "random")
stargazer(rsltRE.Country, type = "text")

#Pooled panel model (white standard errors)
rslt.Pooled_whiteSE <- plm(mdlB, data = df_vt_final_panreg, index = c("group", "time"), model = "pooling")

cov1 <- vcovHC(rslt.Pooled_whiteSE, type = "HC1")
robust_se <- sqrt(diag(cov1))

stargazer(rslt.Pooled_whiteSE, rslt.Pooled, type = "text", se = list(robust_se, NULL))

#Pooled panel model (Newey West SE)  Try out Jasper
rslt.Pooled_NW_SE <- plm(mdlB, data = df_vt_final_panreg, index = c("group", "time"), model = "pooling")

NW_SE_5 <- vcovNW(rslt.Pooled_NW_SE, maxlag = 5) 
NW_SE_19 <- vcovNW(rslt.Pooled_NW_SE, maxlag = 19)
#NW covariance matrix -> accounts for heteroscedasticity and autocorrelation
#BUT we should also determine the lags for the autocorrelation -> the number of weeks that is autocorrelated


stargazer(rslt.Pooled, rslt.Pooled_NW_SE,  rslt.Pooled_NW_SE, se = list(NULL, NW_SE_5, NW_SE_19), type="text")

#Pooled panel model (clustered standard errors)
rslt.Pooled_Clustered = lm(mdlB,data = df_vt_final_panreg) 
a <- coeftest(rslt.Pooled_Clustered, vcov = vcovCL, cluster = ~ time + group)
stargazer(a, rslt.Pooled_Clustered , type = "text")


#setting up the stargazer for the pooled models
stargazer(rslt.Pooled, rslt.Pooled_whiteSE, rslt.Pooled_NW_SE, a,
          se = list(NULL, robust_se, NW_SE_19, NULL), 
          font.size = "tiny", float.env = "sidewaystable",
          column.labels = c("Pooled model", "Pooled model (White SE)", "Pooled model (Newey West SE)", "Pooled model (Clustered SE)"))

#setting up the stargazer for the between, within, (time) fixed and random effects model.
stargazer(rslt.Between, rslt.Within, rsltFE.Country, rslt.Pooled_TFE, rsltRE.Country, 
          font.size = "tiny", float.env = "sidewaystable",
          column.labels = c("Between model", "Within model", "Fixed effects model", "Time fixed effects model", "Random effects model"),
          omit = c("time_6", "time_7","time_8","time_9","time_10","time_11","time_12",
                   "time_13","time_14","time_15","time_16","time_17","time_18","time_19",
                   "time_20","time_21","time_22","time_23","time24" ))

#Count panel models 


df_count <- df_vt_final[c("time", "group", "People_Fully_Vaccinated", 
                          "New_Cases_ln_wins","Reproduction_Rate_wins",
                          "Positive_Rate_ln_percpoint_wins", "Stringency_Index",
                          "Population_Density_ln_wins", "Aged_65_Older" , "gdp_per_capita_wins",
                          "diabetes_prevalence_ln", "female_smoker", "male_smoker_wins", 
                          "hospital_beds_per_thousand_ln_wins", "life_expectancy_ln_wins")]


df_count$percentage_smokers <- (df_count$female_smoker + df_count$male_smoker_wins)/2

#save the data frame
write.csv(df_count, paste0(outputdir, "df_vt_count.csv"), row.names = TRUE)

 #---------------------------------------------------------

mdlCount <- People_Fully_Vaccinated ~ New_Cases_ln_wins + 
  Reproduction_Rate_wins + Positive_Rate_ln_percpoint_wins + Stringency_Index + 
  Population_Density_ln_wins + Aged_65_Older + gdp_per_capita_wins + 
  diabetes_prevalence_ln + percentage_smokers +
  hospital_beds_per_thousand_ln_wins + life_expectancy_ln_wins

#----------------------------- POISSON ----------------------------------
rsltPoisson <- glm(mdlCount, data=df_count, family=c("poisson"))

#-------------------- POISSON robust standard errors --------------------
RobustStErrors <- sqrt(diag(vcovHC(rsltPoisson, type = "HC0")))

#-------------------------- Quasi POISSON -------------------------------
rsltQuasi <- glm(mdlCount, data = df_count,
                 family = c('quasipoisson'))

#-------------------- Negative binomial regression ----------------------
rsltNegBin <- glm.nb(mdlCount, data = df_count)

stargazer(rsltPoisson, rsltPoisson, rsltQuasi, rsltNegBin,
          font.size = "tiny", float.env = "sidewaystable",
          se = list(NULL, RobustStErrors, NULL, NULL), 
          column.labels = c("Poisson Model", "Poisson Model (Robust SE)", "Quasi Poisson Model", "Negative Binomial Model"))


#############################################################################
############# Goodness of fit measures - Count models #######################
#############################################################################


lrtest(rsltPoisson, rsltNegBin)
  #conclusion, negative binomial model is preferred
#---------------------------- Poisson -------------------------------------

#deviance-based pseudo R2 (R2.d)
rsltD1 <- rsltPoisson$deviance
rsltD0 <- rsltPoisson$null.deviance
R2.d <- 1 - rsltD1/rsltD0

round ( cbind ( rsltD1 , rsltD0 , R2.d), 8)


# likelihood ratio index(R2.LRI)

rsltPoisson0 <- glm( People_Fully_Vaccinated ~ 1, data = df_count ,
                     family =c("poisson"))
lnL1 <- logLik(rsltPoisson)
lnL0 <- logLik(rsltPoisson0)
R2.LRI <- 1 - lnL1/lnL0

round(cbind(lnL1 ,lnL0 , R2.LRI), 8)

# Make a data frame
dfSub <-
  data.frame(y = df_count$People_Fully_Vaccinated, 
             mu.hat = predict(rsltPoisson,  
                              type="response"))
dfSub$lnL.ML   <- log(dpois(dfSub$y, dfSub$mu.hat)) 
dfSub$lnL.null <- log(dpois(dfSub$y, mean(dfSub$y))) 
dfSub$lnL.sat  <- log(dpois(dfSub$y, dfSub$y)) 
dfSub$R2Num    <- ((dfSub$y - dfSub$mu.hat)/sqrt(dfSub$mu.hat))^2
dfSub$R2Den    <- ((dfSub$y - mean(dfSub$y))/sqrt(mean(dfSub$y)))^2

# Calculate the column sums
tmp <- colSums(dfSub)
round(tmp, 8)


# Determine the goodness of fit measures
R2.p  <- 1 - tmp["R2Num"]/tmp["R2Den"]

df_r2_poisson <- as.data.frame(round(cbind(R2.p, R2.d, R2.LRI), 3))
#-------------------------- QUASI ----------------------------------

#deviance-based pseudo R2 (R2.d)
rsltD1 <- rsltQuasi$deviance
rsltD0 <- rsltQuasi$null.deviance
R2.d <- 1 - rsltD1/rsltD0

round ( cbind ( rsltD1 , rsltD0 , R2.d), 8)


# likelihood ratio index(R2.LRI)
rsltQuasi0 <- glm( People_Fully_Vaccinated ~ 1, data = df_count ,
                     family =c("quasipoisson"))

lnL1 <- logLik( rsltQuasi )
lnL0 <- logLik( rsltQuasi0 )
R2.LRI <- 1 - lnL1/lnL0

round(cbind(lnL1 ,lnL0 , R2.LRI), 8)

# Make a data frame
dfSub <-
  data.frame(y = df_count$People_Fully_Vaccinated, 
             mu.hat = predict(rsltQuasi,  
                              type="response"))
dfSub$lnL.ML   <- log(dpois(dfSub$y, dfSub$mu.hat)) 
dfSub$lnL.null <- log(dpois(dfSub$y, mean(dfSub$y))) 
dfSub$lnL.sat  <- log(dpois(dfSub$y, dfSub$y)) 
dfSub$R2Num    <- ((dfSub$y - dfSub$mu.hat)/sqrt(dfSub$mu.hat))^2
dfSub$R2Den    <- ((dfSub$y - mean(dfSub$y))/sqrt(mean(dfSub$y)))^2

# Calculate the column sums
tmp <- colSums(dfSub)
round(tmp, 8)


# Determine the goodness of fit measures
R2.p  <- 1 - tmp["R2Num"]/tmp["R2Den"]

df_r2_quasi <- as.data.frame(round(cbind(R2.p, R2.d, R2.LRI), 8))

#------------------ Negative Binomials ------------------------------


#deviance-based pseudo R2 (R2.d)
rsltD1 <- rsltNegBin$deviance
rsltD0 <- rsltNegBin$null.deviance
R2.d <- 1 - rsltD1/rsltD0

round ( cbind ( rsltD1 , rsltD0 , R2.d), 8)


# likelihood ratio index(R2.LRI)
rsltNegBin0 <- glm.nb(People_Fully_Vaccinated ~ 1 , data = df_count )

lnL1 <- logLik( rsltNegBin )
lnL0 <- logLik( rsltNegBin0 )
R2.LRI <- 1 - lnL1/lnL0

round(cbind(lnL1 ,lnL0 , R2.LRI), 8)

# Make a data frame
dfSub <-
  data.frame(y = df_count$People_Fully_Vaccinated, 
             mu.hat = predict(rsltNegBin,  
                              type="response"))
dfSub$lnL.ML   <- log(dpois(dfSub$y, dfSub$mu.hat)) 
dfSub$lnL.null <- log(dpois(dfSub$y, mean(dfSub$y))) 
dfSub$lnL.sat  <- log(dpois(dfSub$y, dfSub$y)) 
dfSub$R2Num    <- ((dfSub$y - dfSub$mu.hat)/sqrt(dfSub$mu.hat))^2
dfSub$R2Den    <- ((dfSub$y - mean(dfSub$y))/sqrt(mean(dfSub$y)))^2

# Calculate the column sums
tmp <- colSums(dfSub)
round(tmp, 8)


# Determine the goodness of fit measures
R2.p.NegBin  <- 1 - tmp["R2Num"]/tmp["R2Den"]

df_r2_negbin <- as.data.frame(round(cbind(R2.p, R2.d, R2.LRI), 8))

df_r2s <- rbind(df_r2_poisson, df_r2_quasi, df_r2_negbin)
df_r2s$Count_model <- c("Poisson Model" , "Quasi Poisson Model", "Negative Binomial Model")
df_r2s <- df_r2s[,c(4, 1, 2,3)]
row.names(df_r2s) <- 1:nrow(df_r2s)
xtable(df_r2s)


###############################################################################
################### United States included ####################################
###############################################################################

#UNITED STATES
df_vt_europe_usa <- 
  as.data.frame(filter(df_vt, df_vt$location == 'Austria' | df_vt$location == 'United States'|
                         df_vt$location == 'Bulgaria'| df_vt$location == 'Belgium' |
                         df_vt$location == 'Croatia'| df_vt$location == 'Cyprus'|
                         df_vt$location == 'Czechia'|df_vt$location == 'Denmark'|
                         df_vt$location == 'Estonia'|df_vt$location == 'Finland'|
                         df_vt$location == 'France'|df_vt$location == 'Germany'|
                         df_vt$location == 'Greece'|df_vt$location == 'Sweden'|
                         df_vt$location == 'Hungary'|df_vt$location == 'Ireland'|
                         df_vt$location == 'Italy'|df_vt$location == 'Latvia'|
                         df_vt$location == 'Liechtenstein'|df_vt$location == 'Lithuania'|
                         df_vt$location == 'Luxembourg'|df_vt$location == 'Malta'|
                         df_vt$location == 'Netherlands'|df_vt$location == 'Norway'|
                         df_vt$location == 'Poland'|df_vt$location == 'Portugal'|
                         df_vt$location == 'Romania'|df_vt$location == 'Slovakia'|
                         df_vt$location == 'Slovenia'|df_vt$location == 'Spain'))
#Making the year variable
df_vt_europe_usa$year <- substr(df_vt_europe_usa$date, 1, 4 )

#Filter the data in order to only keep the year 2021 
df_vt_europe_usa_2021 <- filter(df_vt_europe_usa, df_vt_europe_usa$year == 2021)

#Format the time variable correctly and only keep full weeks containing 7 days
df_vt_europe_usa_2021$weeknumber <- strftime(df_vt_europe_usa_2021$date, format = "%V")
df_vt_europe_usa_2021 <- df_vt_europe_usa_2021 %>% filter(df_vt_europe_usa_2021$date >= "2021-01-04")
df_vt_europe_usa_2021 <- df_vt_europe_usa_2021 %>% filter(df_vt_europe_usa_2021$date <= "2021-09-26")

#Aggregating on week and selecting the variables of interest
df_vt_europe_usa_2021_agg <- 
  aggregate(list(df_vt_europe_usa_2021$people_fully_vaccinated, df_vt_europe_usa_2021$total_cases, 
                 df_vt_europe_usa_2021$new_cases, df_vt_europe_usa_2021$new_deaths, 
                 df_vt_europe_usa_2021$reproduction_rate, df_vt_europe_usa_2021$positive_rate,
                 df_vt_europe_usa_2021$people_fully_vaccinated_per_hundred, df_vt_europe_usa_2021$stringency_index, 
                 df_vt_europe_usa_2021$population_density, df_vt_europe_usa_2021$aged_65_older, 
                 df_vt_europe_usa_2021$median_age,
                 df_vt_europe_usa_2021$gdp_per_capita, df_vt_europe_usa_2021$cardiovasc_death_rate,
                 df_vt_europe_usa_2021$diabetes_prevalence,
                 df_vt_europe_usa_2021$female_smokers, df_vt_europe_usa_2021$male_smokers,
                 df_vt_europe_usa_2021$hospital_beds_per_thousand, df_vt_europe_usa_2021$life_expectancy,
                 df_vt_europe_usa_2021$human_development_index),by=list(df_vt_europe_usa_2021$weeknumber, df_vt_europe_usa_2021$location), mean)

#Renaming all the collumns to meaningful names
colnames(df_vt_europe_usa_2021_agg)[colnames(df_vt_europe_usa_2021_agg) == "Group.1"]       <- "time" 
colnames(df_vt_europe_usa_2021_agg)[colnames(df_vt_europe_usa_2021_agg) == "Group.2"]       <- "group" 
colnames(df_vt_europe_usa_2021_agg)[colnames(df_vt_europe_usa_2021_agg) == 
                      "c.NA..NA..NA..NA..NA..NA..NA..NA..NA..NA..NA..NA..12..388..1281.."]<-
  "People_Fully_Vaccinated" 
colnames(df_vt_europe_usa_2021_agg)[colnames(df_vt_europe_usa_2021_agg) == 
                      "c.1642..2311..2469..2540..2063..2278..1651..1536..1575..1917.."]       <- 
  "New_Cases" 
colnames(df_vt_europe_usa_2021_agg)[colnames(df_vt_europe_usa_2021_agg) ==
                      "c.0.99..1.02..1.02..1..1..0.99..0.98..0.96..0.95..0.96..0.97.."]       <- 
  "Reproduction_Rate" 
colnames(df_vt_europe_usa_2021_agg)[colnames(df_vt_europe_usa_2021_agg) == 
                      "c.NA..NA..NA..NA..NA..NA..NA..NA..NA..NA..NA..NA..NA..NA..NA.."]       <- 
  "Positive_Rate" 
colnames(df_vt_europe_usa_2021_agg)[colnames(df_vt_europe_usa_2021_agg) ==
                      "c.NA..NA..NA..NA..NA..NA..NA..NA..NA..NA..NA..NA..0..0..0.01.."]       <- 
  "People_Fully_Vaccinated_Per_Hundred" 
colnames(df_vt_europe_usa_2021_agg)[colnames(df_vt_europe_usa_2021_agg) == 
                      "c.82.41..82.41..82.41..82.41..82.41..82.41..82.41..82.41..82.41.."]       <- 
  "Stringency_Index" 
colnames(df_vt_europe_usa_2021_agg)[colnames(df_vt_europe_usa_2021_agg) == 
                      "c.106.749..106.749..106.749..106.749..106.749..106.749..106.749.."]       <- 
  "Population_Density" 
colnames(df_vt_europe_usa_2021_agg)[colnames(df_vt_europe_usa_2021_agg) == 
                      "c.19.202..19.202..19.202..19.202..19.202..19.202..19.202..19.202.."]       <- 
  "Aged_65_Older" 
colnames(df_vt_europe_usa_2021_agg)[colnames(df_vt_europe_usa_2021_agg) == 
                      "c.45436.686..45436.686..45436.686..45436.686..45436.686..45436.686.."]       <- 
  "gdp_per_capita" 
colnames(df_vt_europe_usa_2021_agg)[colnames(df_vt_europe_usa_2021_agg) == 
                      "c.6.35..6.35..6.35..6.35..6.35..6.35..6.35..6.35..6.35..6.35.."  ]    <- 
  "diabetes_prevalence" 
colnames(df_vt_europe_usa_2021_agg)[colnames(df_vt_europe_usa_2021_agg) == 
                      "c.28.4..28.4..28.4..28.4..28.4..28.4..28.4..28.4..28.4..28.4.."  ]    <- 
  "female_smoker" 
colnames(df_vt_europe_usa_2021_agg)[colnames(df_vt_europe_usa_2021_agg) == 
                      "c.30.9..30.9..30.9..30.9..30.9..30.9..30.9..30.9..30.9..30.9.." ]    <- 
  "male_smoker" 
colnames(df_vt_europe_usa_2021_agg)[colnames(df_vt_europe_usa_2021_agg) == 
                      "c.7.37..7.37..7.37..7.37..7.37..7.37..7.37..7.37..7.37..7.37.."  ]    <- 
  "hospital_beds_per_thousand" 
colnames(df_vt_europe_usa_2021_agg)[colnames(df_vt_europe_usa_2021_agg) == 
                      "c.81.54..81.54..81.54..81.54..81.54..81.54..81.54..81.54..81.54.."  ]    <- 
  "life_expectancy" 


#Creating a balanced panel data set with the least amount of na's by selecting on weeks and countries
#Renaming the weeks to numeric characters so the 0 in front is not present anymore for the weeks < 10
df_vt_europe_usa_2021_agg$time <- as.numeric(df_vt_europe_usa_2021_agg$time)

df_vt_europe_usa_2021_agg$time <- ifelse(df_vt_europe_usa_2021_agg$time == "01", 1, df_vt_europe_usa_2021_agg$time)
df_vt_europe_usa_2021_agg$time <- ifelse(df_vt_europe_usa_2021_agg$time == "02", 2, df_vt_europe_usa_2021_agg$time)
df_vt_europe_usa_2021_agg$time <- ifelse(df_vt_europe_usa_2021_agg$time == "03", 3, df_vt_europe_usa_2021_agg$time)
df_vt_europe_usa_2021_agg$time <- ifelse(df_vt_europe_usa_2021_agg$time == "04", 4, df_vt_europe_usa_2021_agg$time)
df_vt_europe_usa_2021_agg$time <- ifelse(df_vt_europe_usa_2021_agg$time == "05", 5, df_vt_europe_usa_2021_agg$time)
df_vt_europe_usa_2021_agg$time <- ifelse(df_vt_europe_usa_2021_agg$time == "06", 6, df_vt_europe_usa_2021_agg$time)
df_vt_europe_usa_2021_agg$time <- ifelse(df_vt_europe_usa_2021_agg$time == "07", 7, df_vt_europe_usa_2021_agg$time)
df_vt_europe_usa_2021_agg$time <- ifelse(df_vt_europe_usa_2021_agg$time == "08", 8, df_vt_europe_usa_2021_agg$time)
df_vt_europe_usa_2021_agg$time <- ifelse(df_vt_europe_usa_2021_agg$time == "09", 9, df_vt_europe_usa_2021_agg$time)

#Checking the time variable (week) it's frequency
table(df_vt_europe_usa_2021_agg$time)

#Removing the rows with NA's for the dependent variable

df_vt_europe_usa_2021_agg_2 <- df_vt_europe_usa_2021_agg[!is.na(df_vt_europe_usa_2021_agg["People_Fully_Vaccinated"]),]

#Checking the frequencies in the number of weeks for each country
df_vt_europe_usa_2021_agg_subset <- as.data.frame(table(df_vt_europe_usa_2021_agg_2$group))
##df_vt_agg_frequency_subset <- subset(df_vt_agg_subset, df_vt_agg_subset$Freq >= 30)

#Checking frequencies in the number of countries
df_vt_europe_usa_2021_agg_subset <- as.data.frame(table(df_vt_europe_usa_2021_agg_2$time))

#Conclusion: select the weeks ranging for 5 to 24
df_vt_europe_usa_final <- filter(df_vt_europe_usa_2021_agg_2, df_vt_europe_usa_2021_agg_2$time >= 8 & df_vt_europe_usa_2021_agg_2$time <= 21)

#Checking the data
stargazer(df_vt_final, type = "text")

#We want to include the following countries based on the NA's
SubsetCountries <- c('Austria', 'Bulgaria', 'Belgium', 'Czechia', 'Denmark', 'Estonia',  #without france, germany and liechtenstein country subset
                     'Italy','Latvia','Lithuania','Malta','Portugal','Romania','Slovakia',
                     'Slovenia', 'Norway', 'United States')
#Keep only the countries defined in the subset above
df_vt_europe_usa_final <- df_vt_europe_usa_final[df_vt_europe_usa_final$group %in% SubsetCountries,]

is.pbalanced(df_vt_europe_usa_final)
#Conclusion: Final data set is balanced without NA's

#check the histograms of all the varaibles in order to see whether they have a normal distribution
hist(df_vt_europe_usa_final$People_Fully_Vaccinated_Per_Hundred) # pos
hist(df_vt_europe_usa_final$People_Fully_Vaccinated) #positive skewed
hist(df_vt_europe_usa_final$New_Cases) #positive skewed
hist(df_vt_europe_usa_final$Reproduction_Rate) #normal distributed
hist(df_vt_europe_usa_final$Positive_Rate) #positive skewed
hist(df_vt_europe_usa_final$Stringency_Index) # relatively normal
hist(df_vt_europe_usa_final$Population_Density) #positive skewed
hist(df_vt_europe_usa_final$Aged_65_Older) # sort of normal distributed
hist(df_vt_europe_usa_final$gdp_per_capita) #positive / normal skewed
hist(df_vt_europe_usa_final$diabetes_prevalence) #positive skewed 
hist(df_vt_europe_usa_final$female_smoker) #normal / positive skewed
hist(df_vt_europe_usa_final$male_smoker) #normal distributed
hist(df_vt_europe_usa_final$hospital_beds_per_thousand) #positive skewed
hist(df_vt_europe_usa_final$life_expectancy) #positive skewed

#transforming the data into normal distributions
df_vt_europe_usa_final$People_Fully_Vaccinated_Per_Hundred_ln <- ln(df_vt_europe_usa_final$People_Fully_Vaccinated_Per_Hundred)
df_vt_europe_usa_final$People_Fully_Vaccinated_ln <- ln(df_vt_europe_usa_final$People_Fully_Vaccinated)
df_vt_europe_usa_final$New_Cases_ln <- ln(df_vt_europe_usa_final$New_Cases)
df_vt_europe_usa_final$Population_Density_ln <- ln(df_vt_europe_usa_final$Population_Density)
df_vt_europe_usa_final$diabetes_prevalence_ln <- ln(df_vt_europe_usa_final$diabetes_prevalence)
df_vt_europe_usa_final$hospital_beds_per_thousand_ln <- ln(df_vt_europe_usa_final$hospital_beds_per_thousand)
df_vt_europe_usa_final$life_expectancy_ln <- ln(df_vt_europe_usa_final$life_expectancy)
df_vt_europe_usa_final$Positive_Rate_ln_percpoint <- ln(100*df_vt_europe_usa_final$Positive_Rate)

#Checking whether the newly made variables are indeed more normal distributed
hist(df_vt_final$People_Fully_Vaccinated_Per_Hundred_ln)
hist(df_vt_final$People_Fully_Vaccinated_ln)
hist(df_vt_final$New_Cases_ln) #winsozir
hist(df_vt_final$Reproduction_Rate) #winsorize
hist(df_vt_final$Positive_Rate_ln_percpoint) #winsorize
hist(df_vt_final$Stringency_Index) 
hist(df_vt_final$Population_Density_ln) #winsorize
hist(df_vt_final$Aged_65_Older) 
hist(df_vt_final$gdp_per_capita) #winsorize
hist(df_vt_final$diabetes_prevalence_ln)  
hist(df_vt_final$female_smoker)
hist(df_vt_final$male_smoker) #winsorize
hist(df_vt_final$hospital_beds_per_thousand_ln) #winsorize
hist(df_vt_final$life_expectancy_ln) #winsorize
stargazer(df_vt_final, type = 'text')

#Winsorize the variables which showed a non-normal distributed histogram
df_vt_europe_usa_final$People_Fully_Vaccinated_Per_Hundred_ln_wins <- Winsorize(df_vt_europe_usa_final$People_Fully_Vaccinated_Per_Hundred_ln, minval = NULL, maxval = NULL, probs = c(0.01, 0.99)) 
df_vt_europe_usa_final$New_Cases_ln_wins <- Winsorize(df_vt_europe_usa_final$New_Cases_ln, minval = NULL, maxval = NULL, probs = c(0.01, 0.99))
df_vt_europe_usa_final$Reproduction_Rate_wins <- Winsorize(df_vt_europe_usa_final$Reproduction_Rate, minval = NULL, maxval = NULL, probs = c(0.01, 0.99))
df_vt_europe_usa_final$Positive_Rate_ln_percpoint_wins <- Winsorize(df_vt_europe_usa_final$Positive_Rate_ln_percpoint, minval = NULL, maxval = NULL, probs = c(0.01, 0.99))
df_vt_europe_usa_final$Population_Density_ln_wins <- Winsorize(df_vt_europe_usa_final$Population_Density_ln, minval = NULL, maxval = NULL, probs = c(0.01, 0.99))
df_vt_europe_usa_final$gdp_per_capita_wins <- Winsorize(df_vt_europe_usa_final$gdp_per_capita, minval = NULL, maxval = NULL, probs = c(0.01, 0.99))
df_vt_europe_usa_final$male_smoker_wins <- Winsorize(df_vt_europe_usa_final$male_smoker, minval = NULL, maxval = NULL, probs = c(0.01, 0.99))
df_vt_europe_usa_final$hospital_beds_per_thousand_ln_wins <- Winsorize(df_vt_europe_usa_final$hospital_beds_per_thousand_ln, minval = NULL, maxval = NULL, probs = c(0.01, 0.99))
df_vt_europe_usa_final$life_expectancy_ln_wins <- Winsorize(df_vt_europe_usa_final$life_expectancy_ln, minval = NULL, maxval = NULL, probs = c(0.01, 0.99))

#Check if the winsorized histograms are actually more normal distributed
hist(df_vt_europe_usa_final$People_Fully_Vaccinated_Per_Hundred_ln_wins)
hist(df_vt_europe_usa_final$People_Fully_Vaccinated_ln)
hist(df_vt_europe_usa_final$New_Cases_ln_wins) #winsozir
hist(df_vt_europe_usa_final$Reproduction_Rate_wins) #winsorize
hist(df_vt_europe_usa_final$Positive_Rate_ln_percpoint_wins) #winsorize
hist(df_vt_europe_usa_final$Stringency_Index) 
hist(df_vt_europe_usa_final$Population_Density_ln_wins) #winsorize
hist(df_vt_europe_usa_final$Aged_65_Older) 
hist(df_vt_europe_usa_final$gdp_per_capita_wins) #winsorize
hist(df_vt_europe_usa_final$diabetes_prevalence_ln)  
hist(df_vt_europe_usa_final$female_smoker)
hist(df_vt_europe_usa_final$male_smoker_wins) #winsorize
hist(df_vt_europe_usa_final$hospital_beds_per_thousand_ln_wins) #winsorize
hist(df_vt_europe_usa_final$life_expectancy_ln_wins) #winsorize

#Create the final subset of data, ready for the panel regression estimation
df_vt_europe_usa_final_panreg <- df_vt_europe_usa_final[c("time", "group", "People_Fully_Vaccinated_Per_Hundred_ln", "People_Fully_Vaccinated",
                                    "New_Cases_ln_wins","Reproduction_Rate_wins", "Positive_Rate_ln_percpoint_wins",
                                    "Stringency_Index", "Population_Density_ln_wins", "Aged_65_Older" ,
                                    "gdp_per_capita_wins", "diabetes_prevalence_ln", "female_smoker", 
                                    "male_smoker_wins", "hospital_beds_per_thousand_ln_wins", "life_expectancy_ln_wins")]

#Descriptive statistics
stargazer(df_vt_final_panreg, type = 'text')

#Panel model specification and estimation in order to be able to check the VIF
mdlA <- People_Fully_Vaccinated_Per_Hundred_ln ~ New_Cases_ln_wins + 
  Reproduction_Rate_wins + Positive_Rate_ln_percpoint_wins + Stringency_Index + 
  Population_Density_ln_wins + Aged_65_Older + gdp_per_capita_wins + 
  diabetes_prevalence_ln + female_smoker + male_smoker_wins +
  hospital_beds_per_thousand_ln_wins + life_expectancy_ln_wins

rsltpanelA <- plm(mdlA, data = df_vt_europe_usa_final_panreg, index = c("group", "time"), model = "pooling")
stargazer(rsltpanelA, type = "text")
#Correlation matrix is constructed in order to check for multicollinearity
str(df_vt_europe_usa_final_panreg)
correlation_matrix_df_europe_usa <- as.data.frame(cor(df_vt_europe_usa_final_panreg[3:15], use = "complete.obs"))

#conclusion, male and female smokers are highly correlatd and hece we have to aggregate these two variables
df_vt_europe_usa_final_panreg$percentage_smokers <- (df_vt_europe_usa_final_panreg$female_smoker + 
                                                       df_vt_europe_usa_final_panreg$male_smoker_wins)/2



rsltpanelB <- plm(mdlB, data = df_vt_europe_usa_final_panreg, index = c("group", "time"), model = "pooling")
#vif and tolerance
VIF()
VIF(rsltpanelB)
1/VIF(rsltpanelB)

#scatterplot matrix


#correlation matrix


#Testing for heteroskedasticity
bptest(mdlB, data = df_vt_europe_usa_final_panreg, studentize = F) 
#conclusion: Heteroskedasticity is present in our model 
#In order to account for the heteroskedasticity which is present, white standard errors are used
attach(df_vt_europe_usa_final_panreg)
#Testing for autocorrelation
dwtest(mdlB)
#conclusion: There is autocorrelation present in our model 
#To account for both, heteroskedasticity and autocorrelation we will include newey west standard errors as well

#-------------------------------------------------------------------------------
#Analysis of the data through various models between the United States and Europe
#Pooled, Between and Within model preparation

#Define a dummy for the US
df_vt_europe_usa_final_panreg$dUS <- ifelse(df_vt_europe_usa_final_panreg$group == "United States", 1 , 0)
#dummies for time fixed effects
df_vt_europe_usa_final_panreg_merged <- dummy_cols(df_vt_europe_usa_final_panreg_merged, select_columns = "time")

mdlB_dummies_usa <- People_Fully_Vaccinated_Per_Hundred_ln ~ New_Cases_ln_wins + 
  Reproduction_Rate_wins + Positive_Rate_ln_percpoint_wins + Stringency_Index + 
  Population_Density_ln_wins + Aged_65_Older + gdp_per_capita_wins + 
  diabetes_prevalence_ln + percentage_smokers +
  hospital_beds_per_thousand_ln_wins + life_expectancy_ln_wins + dUS + time_9 + 
  time_10 + time_11 + time_12 + time_13 + time_14 + time_15 + time_16 + time_17 + 
  time_18 + time_19 + time_20 + time_21

#Model specification
mdlB_us <- People_Fully_Vaccinated_Per_Hundred_ln ~ New_Cases_ln_wins + 
  Reproduction_Rate_wins + Positive_Rate_ln_percpoint_wins + Stringency_Index + 
  Population_Density_ln_wins + Aged_65_Older + gdp_per_capita_wins + 
  diabetes_prevalence_ln + percentage_smokers +
  hospital_beds_per_thousand_ln_wins + life_expectancy_ln_wins +dUS

#Constructing a new dataset which contains the averages of the values in our original data set.
df_vt_europe_usa_final_avg <- 
  ddply(df_vt_europe_usa_final_panreg, .(group), summarise,
        avg.People_Fully_Vaccinated_Per_Hundred_ln = mean(People_Fully_Vaccinated_Per_Hundred_ln, na.rm=TRUE),
        avg.New_Cases_ln_wins       = mean(New_Cases_ln_wins, na.rm=TRUE),
        avg.Reproduction_Rate_wins      = mean(Reproduction_Rate_wins, na.rm=TRUE),
        avg.Positive_Rate_ln_percpoint_wins     = mean(Positive_Rate_ln_percpoint_wins, na.rm=TRUE),
        avg.Stringency_Index   = mean(Stringency_Index, na.rm=TRUE),
        avg.Population_Density_ln_wins = mean(Population_Density_ln_wins, na.rm=TRUE),
        avg.Aged_65_Older   = mean(Aged_65_Older, na.rm=TRUE),
        avg.gdp_per_capita_wins     = mean(gdp_per_capita_wins, na.rm=TRUE),
        avg.diabetes_prevalence_ln      = mean(diabetes_prevalence_ln, na.rm=TRUE),
        avg.percentage_smokers     = mean(percentage_smokers, na.rm=TRUE),
        avg.hospital_beds_per_thousand_ln_wins   = mean(hospital_beds_per_thousand_ln_wins, na.rm=TRUE),
        avg.life_expectancy_ln_wins  = mean(life_expectancy_ln_wins, na.rm=TRUE),
        avg.dUS = mean(dUS, na.rm = TRUE),
        numValid         = length(group))


df_vt_europe_usa_final_panreg_merged <- merge(df_vt_europe_usa_final_panreg, df_vt_europe_usa_final_avg, by="group")

#Constructing the differences
attach(df_vt_europe_usa_final_panreg_merged)
df_vt_europe_usa_final_panreg_merged$diff.People_Fully_Vaccinated_Per_Hundred_ln   <- People_Fully_Vaccinated_Per_Hundred_ln   - avg.People_Fully_Vaccinated_Per_Hundred_ln
df_vt_europe_usa_final_panreg_merged$diff.New_Cases_ln_wins       <- New_Cases_ln_wins        - avg.New_Cases_ln_wins 
df_vt_europe_usa_final_panreg_merged$diff.Reproduction_Rate_wins       <- Reproduction_Rate_wins      - avg.Reproduction_Rate_wins
df_vt_europe_usa_final_panreg_merged$diff.Positive_Rate_ln_percpoint_wins     <- Positive_Rate_ln_percpoint_wins     - avg.Positive_Rate_ln_percpoint_wins
df_vt_europe_usa_final_panreg_merged$diff.Stringency_Index   <- Stringency_Index   - avg.Stringency_Index
df_vt_europe_usa_final_panreg_merged$diff.Population_Density_ln_wins <- Population_Density_ln_wins - avg.Population_Density_ln_wins
df_vt_europe_usa_final_panreg_merged$diff.Aged_65_Older <- Aged_65_Older - avg.Aged_65_Older
df_vt_europe_usa_final_panreg_merged$diff.gdp_per_capita_wins   <- gdp_per_capita_wins   - avg.gdp_per_capita_wins
df_vt_europe_usa_final_panreg_merged$diff.diabetes_prevalence_ln      <- diabetes_prevalence_ln      - avg.diabetes_prevalence_ln
df_vt_europe_usa_final_panreg_merged$diff.percentage_smokers       <- percentage_smokers       - avg.percentage_smokers
df_vt_europe_usa_final_panreg_merged$diff.hospital_beds_per_thousand_ln_wins    <- hospital_beds_per_thousand_ln_wins    - avg.hospital_beds_per_thousand_ln_wins
df_vt_europe_usa_final_panreg_merged$diff.life_expectancy_ln_wins   <- life_expectancy_ln_wins   - avg.life_expectancy_ln_wins
df_vt_europe_usa_final_panreg_merged$diff.dUS   <- dUS   - avg.dUS
detach(df_vt_europe_usa_final_panreg_merged)
stargazer(df_vt_europe_usa_final_panreg_merged, type = "text")

#test if there are enough amount of observations for every country
df_vt_europe_usa_final_panreg_merged <- df_vt_europe_usa_final_panreg_merged[df_vt_europe_usa_final_panreg_merged$numValid == 14,]
is.pbalanced(df_vt_final_merged_panreg)
#Conclusion, there still remains a balanced panel data set

# Constructing a dataset containing either the differencers or the averages
mdlVars      <- all.vars(mdlB)
mdlVars.avg  <- paste0("avg.", mdlVars)
mdlVars.diff <- paste0("diff.", mdlVars)

df_vt_europe_usa_final_panreg_merged.between <- df_vt_europe_usa_final_panreg_merged[mdlVars.avg]
df_vt_europe_usa_final_panreg_merged.within  <- df_vt_europe_usa_final_panreg_merged[c(mdlVars.diff)]
str(df_vt_europe_usa_final_panreg_merged)

#Remove the diff. and avg. name from each variable in order to be able to use the same model specification
colnames(df_vt_europe_usa_final_panreg_merged.within) <- 
  gsub("diff\\.", "", colnames(df_vt_europe_usa_final_panreg_merged.within))   
colnames(df_vt_europe_usa_final_panreg_merged.between) <- 
  gsub("avg\\.", "", colnames(df_vt_europe_usa_final_panreg_merged.between))

#Pooled panel model
rslt.Pooled_usa <- plm(mdlB_us, data = df_vt_europe_usa_final_panreg_merged, index = c("group", "time"), model = "pooling")

stargazer(rslt.Pooled, type = "text")

#Between panel model
rslt.Between_usa <- lm (mdlB_us, data=df_vt_europe_usa_final_panreg_merged.between)
stargazer(rslt.Between, type = "text")
#Within panel model
rslt.Within_usa <- lm (mdlB_us, data=df_vt_europe_usa_final_panreg_merged.within)
stargazer(rslt.Within, type = "text")

#Fixed effects panel model
rsltFE.Country_usa <- plm(mdlB_us, data = df_vt_europe_usa_final_panreg_merged, index=c("group", "time"), model = "within") 
stargazer(rsltFE.Country, type = "text")


####
rslt.Pooled_TFE_country_usa <- plm(mdlB_dummies_usa, data = df_vt_europe_usa_final_panreg_merged, index = c("group", "time"), model = "pooling")
stargazer(rslt.Pooled_TFE, rsltFE.Country, type = "text")

#Conclusion FE is preferred model
pFtest(rsltFE.Country_usa, rslt.Pooled_usa) 

#Random effects panel model
rsltRE.Country_usa <- plm(mdlB_us, data = df_vt_europe_usa_final_panreg_merged, index = c("group", "time"), model = "random")
stargazer(rsltRE.Country, type = "text")

#Pooled panel model (white standard errors)
rslt.Pooled_whiteSE_usa <- plm(mdlB_us, data = df_vt_europe_usa_final_panreg_merged, index = c("group", "time"), model = "pooling")

cov1 <- vcovHC(rslt.Pooled_whiteSE_usa, type = "HC1")
robust_se_usa <- sqrt(diag(cov1))

stargazer(rslt.Pooled_whiteSE, rslt.Pooled, type = "text", se = list(robust_se, NULL))

#Pooled panel model (Newey West SE)  Try out Jasper
rslt.Pooled_NW_SE_usa <- plm(mdlB_us, data = df_vt_europe_usa_final_panreg_merged, index = c("group", "time"), model = "pooling")

NW_SE_5_usa <- vcovNW(rslt.Pooled_NW_SE_usa, maxlag = 5) 
NW_SE_13_usa <- vcovNW(rslt.Pooled_NW_SE_usa, maxlag = 13)
#NW covariance matrix -> accounts for heteroscedasticity and autocorrelation
#BUT we should also determine the lags for the autocorrelation -> the number of weeks that is autocorrelated

#Pooled panel model (clustered standard errors)
rslt.Pooled_Clustered_usa = lm(mdlB_us,data = df_vt_europe_usa_final_panreg_merged) 
a_usa <- coeftest(rslt.Pooled_Clustered_usa, vcov = vcovCL, cluster = ~ time + group)
stargazer(a, rslt.Pooled_Clustered_usa , type = "text")


#setting up the stargazer for the pooled models
stargazer(rslt.Pooled_usa, rslt.Pooled_whiteSE_usa, rslt.Pooled_NW_SE_usa, a_usa,
          se = list(NULL, robust_se_usa, NW_SE_13_usa, NULL), 
          font.size = "tiny", float.env = "sidewaystable",
          column.labels = c("Pooled model", "Pooled model (White SE)", "Pooled model (Newey West SE)", "Pooled model (Clustered SE)"))

#setting up the stargazer for the between, within, (time) fixed and random effects model.
stargazer(rslt.Between_usa, rslt.Within_usa, rsltFE.Country_usa, rslt.Pooled_TFE_country_usa, rsltRE.Country_usa, 
          font.size = "tiny", float.env = "sidewaystable",
          column.labels = c("Between model", "Within model", "Fixed effects model", "Time fixed effects model", "Random effects model"),
          omit = c("time_8","time_9","time_10","time_11","time_12",
                   "time_13","time_14","time_15","time_16","time_17","time_18","time_19",
                   "time_20","time_21" ))

#Count panel models 


df_count_usa <- df_vt_europe_usa_final_panreg[c("time", "group", "People_Fully_Vaccinated", 
                          "New_Cases_ln_wins","Reproduction_Rate_wins",
                          "Positive_Rate_ln_percpoint_wins", "Stringency_Index",
                          "Population_Density_ln_wins", "Aged_65_Older" , "gdp_per_capita_wins",
                          "diabetes_prevalence_ln", "percentage_smokers", 
                          "hospital_beds_per_thousand_ln_wins", "life_expectancy_ln_wins", 'dUS')]
ls(df_vt_europe_usa_final_panreg_merged)

df_count$percentage_smokers <- (df_count$female_smoker + df_count$male_smoker_wins)/2

#save the data frame
write.csv(df_count_usa, paste0(outputdir, "df_vt_count_usa.csv"), row.names = TRUE)

#---------------------------------------------------------

mdlCount_usa <- People_Fully_Vaccinated ~ New_Cases_ln_wins + 
  Reproduction_Rate_wins + Positive_Rate_ln_percpoint_wins + Stringency_Index + 
  Population_Density_ln_wins + Aged_65_Older + gdp_per_capita_wins + 
  diabetes_prevalence_ln + percentage_smokers +
  hospital_beds_per_thousand_ln_wins + life_expectancy_ln_wins +dUS

#----------------------------- POISSON ----------------------------------
rsltPoisson_usa <- glm(mdlCount_usa, data=df_count_usa, family=c("poisson"))

#-------------------- POISSON robust standard errors --------------------
RobustStErrors_usa <- sqrt(diag(vcovHC(rsltPoisson_usa, type = "HC0")))

#-------------------------- Quasi POISSON -------------------------------
rsltQuasi_usa <- glm(mdlCount_usa, data = df_count_usa,
                 family = c('quasipoisson'))

#-------------------- Negative binomial regression ----------------------
rsltNegBin_usa <- glm.nb(mdlCount_usa, data = df_count_usa)

stargazer(rsltPoisson_usa, rsltPoisson_usa, rsltQuasi_usa, rsltNegBin_usa,
          font.size = "tiny", float.env = "sidewaystable",
          se = list(NULL, RobustStErrors_usa, NULL, NULL), 
          column.labels = c("Poisson Model", "Poisson Model (Robust SE)", "Quasi Poisson Model", "Negative Binomial Model"))


#############################################################################
############# Goodness of fit measures - Count models #######################
#############################################################################


lrtest(rsltPoisson_usa, rsltNegBin_usa)
  #Conclusion: p-value below 0.05 so we prefer the negative binomial model
#---------------------------- Poisson -------------------------------------

#deviance-based pseudo R2 (R2.d)
rsltD1 <- rsltPoisson_usa$deviance
rsltD0 <- rsltPoisson_usa$null.deviance
R2.d <- 1 - rsltD1/rsltD0

round ( cbind ( rsltD1 , rsltD0 , R2.d), 8)


# likelihood ratio index(R2.LRI)

rsltPoisson0_usa <- glm( People_Fully_Vaccinated ~ 1, data = df_count_usa ,
                     family =c("poisson"))
lnL1 <- logLik(rsltPoisson_usa)
lnL0 <- logLik(rsltPoisson0_usa)
R2.LRI <- 1 - lnL1/lnL0

round(cbind(lnL1 ,lnL0 , R2.LRI), 8)

# Make a data frame
dfSub_usa <-
  data.frame(y = df_count_usa$People_Fully_Vaccinated, 
             mu.hat = predict(rsltPoisson_usa,  
                              type="response"))
dfSub_usa$lnL.ML   <- log(dpois(dfSub_usa$y, dfSub_usa$mu.hat)) 
dfSub_usa$lnL.null <- log(dpois(dfSub_usa$y, mean(dfSub_usa$y))) 
dfSub_usa$lnL.sat  <- log(dpois(dfSub_usa$y, dfSub_usa$y)) 
dfSub_usa$R2Num    <- ((dfSub_usa$y - dfSub_usa$mu.hat)/sqrt(dfSub_usa$mu.hat))^2
dfSub_usa$R2Den    <- ((dfSub_usa$y - mean(dfSub_usa$y))/sqrt(mean(dfSub_usa$y)))^2

# Calculate the column sums
tmp <- colSums(dfSub_usa)
round(tmp, 8)


# Determine the goodness of fit measures
R2.p  <- 1 - tmp["R2Num"]/tmp["R2Den"]

df_r2_poisson <- as.data.frame(round(cbind(R2.p, R2.d, R2.LRI), 3))
#-------------------------- QUASI ----------------------------------

#deviance-based pseudo R2 (R2.d)
rsltD1 <- rsltQuasi$deviance
rsltD0 <- rsltQuasi$null.deviance
R2.d <- 1 - rsltD1/rsltD0

round ( cbind ( rsltD1 , rsltD0 , R2.d), 8)


# likelihood ratio index(R2.LRI)
rsltQuasi0 <- glm( People_Fully_Vaccinated ~ 1, data = df_count ,
                   family =c("quasipoisson"))

lnL1 <- logLik( rsltQuasi )
lnL0 <- logLik( rsltQuasi0 )
R2.LRI <- 1 - lnL1/lnL0

round(cbind(lnL1 ,lnL0 , R2.LRI), 8)

dfSub_usa <-
  data.frame(y = df_count_usa$People_Fully_Vaccinated, 
             mu.hat = predict(rsltQuasi_usa,  
                              type="response"))
dfSub_usa$lnL.ML   <- log(dpois(dfSub_usa$y, dfSub_usa$mu.hat)) 
dfSub_usa$lnL.null <- log(dpois(dfSub_usa$y, mean(dfSub_usa$y))) 
dfSub_usa$lnL.sat  <- log(dpois(dfSub_usa$y, dfSub_usa$y)) 
dfSub_usa$R2Num    <- ((dfSub_usa$y - dfSub_usa$mu.hat)/sqrt(dfSub_usa$mu.hat))^2
dfSub_usa$R2Den    <- ((dfSub_usa$y - mean(dfSub_usa$y))/sqrt(mean(dfSub_usa$y)))^2

# Calculate the column sums
tmp <- colSums(dfSub_usa)
round(tmp, 8)


# Determine the goodness of fit measures
R2.p  <- 1 - tmp["R2Num"]/tmp["R2Den"]

df_r2_quasi_usa <- as.data.frame(round(cbind(R2.p, R2.d, R2.LRI), 3))

#------------------ Negative Binomials ------------------------------


#deviance-based pseudo R2 (R2.d)
rsltD1 <- rsltNegBin$deviance
rsltD0 <- rsltNegBin$null.deviance
R2.d <- 1 - rsltD1/rsltD0

round ( cbind ( rsltD1 , rsltD0 , R2.d), 8)


# likelihood ratio index(R2.LRI)
rsltNegBin0 <- glm.nb(People_Fully_Vaccinated ~ 1 , data = df_count )

lnL1 <- logLik( rsltNegBin )
lnL0 <- logLik( rsltNegBin0 )
R2.LRI <- 1 - lnL1/lnL0

round(cbind(lnL1 ,lnL0 , R2.LRI), 8)

dfSub_usa <-
  data.frame(y = df_count_usa$People_Fully_Vaccinated, 
             mu.hat = predict(rsltNegBin_usa,  
                              type="response"))
dfSub_usa$lnL.ML   <- log(dpois(dfSub_usa$y, dfSub_usa$mu.hat)) 
dfSub_usa$lnL.null <- log(dpois(dfSub_usa$y, mean(dfSub_usa$y))) 
dfSub_usa$lnL.sat  <- log(dpois(dfSub_usa$y, dfSub_usa$y)) 
dfSub_usa$R2Num    <- ((dfSub_usa$y - dfSub_usa$mu.hat)/sqrt(dfSub_usa$mu.hat))^2
dfSub_usa$R2Den    <- ((dfSub_usa$y - mean(dfSub_usa$y))/sqrt(mean(dfSub_usa$y)))^2

# Calculate the column sums
tmp <- colSums(dfSub_usa)
round(tmp, 8)


# Determine the goodness of fit measures
R2.p  <- 1 - tmp["R2Num"]/tmp["R2Den"]

df_r2_negbin_usa <- as.data.frame(round(cbind(R2.p, R2.d, R2.LRI), 3))

df_r2s_usa <- rbind(df_r2_poisson, df_r2_quasi, df_r2_negbin)
df_r2s_usa$Count_model <- c("Poisson Model" , "Quasi Poisson Model", "Negative Binomial Model")
df_r2s_usa <- df_r2s_usa[,c(4, 1, 2,3)]
row.names(df_r2s_usa) <- 1:nrow(df_r2s_usa)
xtable(df_r2s)




