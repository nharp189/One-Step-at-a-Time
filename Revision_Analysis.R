setwd("/Users/nicholasharp/Documents/Nick-Grad/Neta_Lab/exercise_study/Bayes-R/Cross-Sectional-Exercise/")

### install packages if needed ###
#install.packages("Rfast")
#install.packages("BayesMed")
#install.packages("emmeans")
#install.packages("ppcor")

### load packages ###
library(readxl)
library(psych)
library(dplyr)
library(tidyr)
library(ppcor)

R.version
### import exercise data ###
new.data <- read_excel("data/Pilot Data - Health&Pol FINAL.xlsx", sheet = 2)
old.data <- read_excel("data/Maital PA and EX data_KK.xlsx", sheet = 2)
merged.data <- merge.data.frame(new.data, old.data, by = "Participant ID")
#write.csv(merged.data, "merged_data.csv")

### create dataframe of only useful variables ###
cleaned.merged.data <- merged.data[,c(1, 21, 35:37, 43, 44, 47, 48, 50, 51, 
                                      61, 71:76, 93, 95, 96, 99:101, 132, 173:177)]

### re-name columns ###
names(cleaned.merged.data) <- c("sub", "race", "Negative", "Positive", "Ambiguous", 
                                "vigPA_day.week", "vigPA_min.day", "modPA_day.week", 
                                "modPA_min.day","walk_day.week", "walk_min.day", 
                                "exercise_hours.week", "sport_MET", "vig_MET", "mod_MET", "walk_MET", 
                                "total_MET", "overall_MET", "gender", "education", "age", "income",
                                "nationality","state", "dep", "team", "racquet","aerobic", "martial", 
                                "resistance")

### calculate descriptives ###
{
mean(cleaned.merged.data$age, na.rm = TRUE)
sd(cleaned.merged.data$age, na.rm = TRUE)

mean(cleaned.merged.data$education, na.rm = TRUE)
sd(cleaned.merged.data$education, na.rm = TRUE)

mean(cleaned.merged.data$walk_day.week, na.rm = TRUE)
sd(cleaned.merged.data$walk_day.week, na.rm = TRUE)

mean(cleaned.merged.data$walk_min.day, na.rm = TRUE)
sd(cleaned.merged.data$walk_min.day, na.rm = TRUE)

mean(cleaned.merged.data$modPA_day.week, na.rm = TRUE)
sd(cleaned.merged.data$modPA_day.week, na.rm = TRUE)

mean(cleaned.merged.data$modPA_min.day, na.rm = TRUE)
sd(cleaned.merged.data$modPA_min.day, na.rm = TRUE)

mean(cleaned.merged.data$vigPA_day.week, na.rm = TRUE)
sd(cleaned.merged.data$vigPA_day.week, na.rm = TRUE)

mean(cleaned.merged.data$vigPA_min.day, na.rm = TRUE)
sd(cleaned.merged.data$vigPA_min.day, na.rm = TRUE)

mean(merged.data$`STAI-S`, na.rm = TRUE)
sd(merged.data$`STAI-S`, na.rm = TRUE)

mean(merged.data$`STAI-T`, na.rm = TRUE)
sd(merged.data$`STAI-T`, na.rm = TRUE)

mean(cleaned.merged.data$income, na.rm = TRUE)
sd(cleaned.merged.data$income, na.rm = TRUE)
}

### binary gender variable ###
cleaned.merged.data <- subset(cleaned.merged.data, gender <= 2)

### Total MET x Valence Bias ###
### ambiguous images and total MET ###
###                                ###
###                                ###
cor.test(cleaned.merged.data$Ambiguous, cleaned.merged.data$total_MET, 
         method = "spearman", exact = FALSE)

data1 <- cleaned.merged.data[,c(1,5,17,19,21)]
data1 <- data1[complete.cases(data1),]
pcor.test(data1$Ambiguous, data1$total_MET, 
          list(data1$age, data1$gender), method = "spearman")

jzs_partcor(data1$Ambiguous, data1$total_MET, )

install.packages("BayesMed")
library(BayesMed)
install.packages("gsl")
library(rjags)
library(gsl)




### positive images and total MET ###
###                                ###
###                                ###
cor.test(cleaned.merged.data$Positive, cleaned.merged.data$total_MET, 
         method = "spearman", exact = FALSE)

data1 <- cleaned.merged.data[,c(1,4,17,19,21)]
data1 <- data1[complete.cases(data1),]
pcor.test(data1$Positive, data1$total_MET, 
          list(data1$age, data1$gender), method = "spearman")

### negative images and total MET ###
###                                ###
###                                ###
cor.test(cleaned.merged.data$Negative, cleaned.merged.data$total_MET, 
         method = "spearman", exact = FALSE)

data1 <- cleaned.merged.data[,c(1,3,17,19,21)]
data1 <- data1[complete.cases(data1),]
pcor.test(data1$Negative, data1$total_MET, 
          list(data1$age, data1$gender), method = "spearman")

### METminutes x Valence Bias ###
{
### ambiguous images and vigorous MET ###
###                                ###
###                                ###
cor.test(cleaned.merged.data$Ambiguous, cleaned.merged.data$vig_MET, 
         method = "spearman", exact = FALSE)

data1 <- cleaned.merged.data[,c(1,5,14:16,19,21)]
data1 <- data1[complete.cases(data1),]
pcor.test(data1$Ambiguous, data1$vig_MET, 
          list(data1$age, data1$gender,
               data1$mod_MET, data1$walk_MET), method = "spearman")

### positive images and vigorous MET ###
###                                ###
###                                ###
cor.test(cleaned.merged.data$Positive, cleaned.merged.data$vig_MET, 
         method = "spearman", exact = FALSE)

data1 <- cleaned.merged.data[,c(1,4,14:16,19,21)]
data1 <- data1[complete.cases(data1),]
pcor.test(data1$Positive, data1$vig_MET, 
          list(data1$age, data1$gender,
               data1$mod_MET, data1$walk_MET), method = "spearman")

### negative images and vigorous MET ###
###                                ###
###                                ###
cor.test(cleaned.merged.data$Negative, cleaned.merged.data$vig_MET, 
         method = "spearman", exact = FALSE)

data1 <- cleaned.merged.data[,c(1,3,14:16,19,21)]
data1 <- data1[complete.cases(data1),]
pcor.test(data1$Negative, data1$vig_MET, 
          list(data1$age, data1$gender,
               data1$mod_MET, data1$walk_MET), method = "spearman")


### ambiguous images and moderate MET ###
###                                ###
###                                ###
cor.test(cleaned.merged.data$Ambiguous, cleaned.merged.data$mod_MET, 
         method = "spearman", exact = FALSE)

data1 <- cleaned.merged.data[,c(1,5,14:16,19,21)]
data1 <- data1[complete.cases(data1),]
pcor.test(data1$Ambiguous, data1$mod_MET, 
          list(data1$age, data1$gender,
               data1$vig_MET, data1$walk_MET), method = "spearman")

### positive images and moderate MET ###
###                                ###
###                                ###
cor.test(cleaned.merged.data$Positive, cleaned.merged.data$mod_MET, 
         method = "spearman", exact = FALSE)

data1 <- cleaned.merged.data[,c(1,4,14:16,19,21)]
data1 <- data1[complete.cases(data1),]
pcor.test(data1$Positive, data1$mod_MET, 
          list(data1$age, data1$gender,
               data1$vig_MET, data1$walk_MET), method = "spearman")

### negative images and moderate MET ###
###                                ###
###                                ###
cor.test(cleaned.merged.data$Negative, cleaned.merged.data$mod_MET, 
         method = "spearman", exact = FALSE)

data1 <- cleaned.merged.data[,c(1,3,14:16,19,21)]
data1 <- data1[complete.cases(data1),]
pcor.test(data1$Negative, data1$mod_MET, 
          list(data1$age, data1$gender,
               data1$vig_MET, data1$walk_MET), method = "spearman")

### ambiguous images and walk MET ###
###                                ###
###                                ###
cor.test(cleaned.merged.data$Ambiguous, cleaned.merged.data$walk_MET, 
         method = "spearman", exact = FALSE)

data1 <- cleaned.merged.data[,c(1,5,14:16,19,21)]
data1 <- data1[complete.cases(data1),]
pcor.test(data1$Ambiguous, data1$walk_MET, 
          list(data1$age, data1$gender,
               data1$vig_MET, data1$mod_MET), method = "spearman")

### positive images and walk MET ###
###                                ###
###                                ###
cor.test(cleaned.merged.data$Positive, cleaned.merged.data$walk_MET, 
         method = "spearman", exact = FALSE)

data1 <- cleaned.merged.data[,c(1,4,14:16,19,21)]
data1 <- data1[complete.cases(data1),]
pcor.test(data1$Positive, data1$walk_MET, 
          list(data1$age, data1$gender,
               data1$vig_MET, data1$mod_MET), method = "spearman")

### negative images and walk MET ###
###                                ###
###                                ###
cor.test(cleaned.merged.data$Negative, cleaned.merged.data$walk_MET, 
         method = "spearman", exact = FALSE)

data1 <- cleaned.merged.data[,c(1,3,14:16,19,21)]
data1 <- data1[complete.cases(data1),]
pcor.test(data1$Negative, data1$walk_MET, 
          list(data1$age, data1$gender,
               data1$vig_MET, data1$mod_MET), method = "spearman")
}

### Minutes per week analyses ###
cleaned.merged.data$vig_min.week <- (cleaned.merged.data$vigPA_min.day * cleaned.merged.data$vigPA_day.week)
cleaned.merged.data$mod_min.week <- (cleaned.merged.data$modPA_min.day * cleaned.merged.data$modPA_day.week)
cleaned.merged.data$walk_min.week <- (cleaned.merged.data$walk_min.day * cleaned.merged.data$walk_day.week)
{
  ### ambiguous images and vigorous  ###
  ###                                ###
  ###                                ###
  cor.test(cleaned.merged.data$Ambiguous, cleaned.merged.data$vig_min.week, 
           method = "spearman", exact = FALSE)
  
  data1 <- cleaned.merged.data[,c(1,5,19,21,31:33)]
  data1 <- data1[complete.cases(data1),]
  pcor.test(data1$Ambiguous, data1$vig_min.week, 
            list(data1$age, data1$gender,
                 data1$mod_min.week, data1$walk_min.week), method = "spearman")
  
  ### positive images and vigorous  ###
  ###                                ###
  ###                                ###
  cor.test(cleaned.merged.data$Positive, cleaned.merged.data$vig_min.week, 
           method = "spearman", exact = FALSE)
  
  data1 <- cleaned.merged.data[,c(1,4,19,21,31:33)]
  data1 <- data1[complete.cases(data1),]
  pcor.test(data1$Positive, data1$vig_min.week, 
            list(data1$age, data1$gender,
                 data1$mod_min.week, data1$walk_min.week), method = "spearman")
  
  ### negative images and vigorous  ###
  ###                                ###
  ###                                ###
  cor.test(cleaned.merged.data$Negative, cleaned.merged.data$vig_min.week, 
           method = "spearman", exact = FALSE)
  
  data1 <- cleaned.merged.data[,c(1,3,19,21,31:33)]
  data1 <- data1[complete.cases(data1),]
  pcor.test(data1$Negative, data1$vig_min.week, 
            list(data1$age, data1$gender,
                 data1$mod_min.week, data1$walk_min.week), method = "spearman")
  
  ### ambiguous images and moderate  ###
  ###                                ###
  ###                                ###
  cor.test(cleaned.merged.data$Ambiguous, cleaned.merged.data$mod_min.week, 
           method = "spearman", exact = FALSE)
  
  data1 <- cleaned.merged.data[,c(1,5,19,21,31:33)]
  data1 <- data1[complete.cases(data1),]
  pcor.test(data1$Ambiguous, data1$mod_min.week, 
            list(data1$age, data1$gender,
                 data1$vig_min.week, data1$walk_min.week), method = "spearman")
  
  ### positive images and moderate  ###
  ###                                ###
  ###                                ###
  cor.test(cleaned.merged.data$Positive, cleaned.merged.data$mod_min.week, 
           method = "spearman", exact = FALSE)
  
  data1 <- cleaned.merged.data[,c(1,4,19,21,31:33)]
  data1 <- data1[complete.cases(data1),]
  pcor.test(data1$Positive, data1$mod_min.week, 
            list(data1$age, data1$gender,
                 data1$vig_min.week, data1$walk_min.week), method = "spearman")
  
  ### negative images and moderate  ###
  ###                                ###
  ###                                ###
  cor.test(cleaned.merged.data$Negative, cleaned.merged.data$mod_min.week, 
           method = "spearman", exact = FALSE)
  
  data1 <- cleaned.merged.data[,c(1,3,19,21,31:33)]
  data1 <- data1[complete.cases(data1),]
  pcor.test(data1$Negative, data1$mod_min.week, 
            list(data1$age, data1$gender,
                 data1$vig_min.week, data1$walk_min.week), method = "spearman")
  
  ### ambiguous images and walk  ###
  ###                                ###
  ###                                ###
  cor.test(cleaned.merged.data$Ambiguous, cleaned.merged.data$walk_min.week, 
           method = "spearman", exact = FALSE)
  
  data1 <- cleaned.merged.data[,c(1,5,19,21,31:33)]
  data1 <- data1[complete.cases(data1),]
  pcor.test(data1$Ambiguous, data1$walk_min.week, 
            list(data1$age, data1$gender,
                 data1$vig_min.week, data1$mod_min.week), method = "spearman")
  
  ### positive images and walk  ###
  ###                                ###
  ###                                ###
  cor.test(cleaned.merged.data$Positive, cleaned.merged.data$walk_min.week, 
           method = "spearman", exact = FALSE)
  
  data1 <- cleaned.merged.data[,c(1,4,19,21,31:33)]
  data1 <- data1[complete.cases(data1),]
  pcor.test(data1$Positive, data1$walk_min.week, 
            list(data1$age, data1$gender,
                 data1$vig_min.week, data1$mod_min.week), method = "spearman")
  
  ### negative images and walk  ###
  ###                                ###
  ###                                ###
  cor.test(cleaned.merged.data$Negative, cleaned.merged.data$walk_min.week, 
           method = "spearman", exact = FALSE)
  
  data1 <- cleaned.merged.data[,c(1,3,19,21,31:33)]
  data1 <- data1[complete.cases(data1),]
  pcor.test(data1$Negative, data1$walk_min.week, 
            list(data1$age, data1$gender,
                 data1$vig_min.week, data1$mod_min.week), method = "spearman")
}

### Days/week and min/day ###
### Vigorous ###
{
### ambiguous images and vigorous days per week ###
###                                ###
###                                ###
cor.test(cleaned.merged.data$Ambiguous, cleaned.merged.data$vigPA_day.week, 
         method = "spearman", exact = FALSE)

data1 <- cleaned.merged.data[,c(1,5,6,8,10,19,21)]
data1 <- data1[complete.cases(data1),]
pcor.test(data1$Ambiguous, data1$vigPA_day.week, 
          list(data1$age, data1$gender,
               data1$modPA_day.week, data1$walk_day.week), method = "spearman")

### positive images and vigorous days per week ###
###                                ###
###                                ###
cor.test(cleaned.merged.data$Positive, cleaned.merged.data$vigPA_day.week, 
         method = "spearman", exact = FALSE)

data1 <- cleaned.merged.data[,c(1,4,6,8,10,19,21)]
data1 <- data1[complete.cases(data1),]
pcor.test(data1$Positive, data1$vigPA_day.week, 
          list(data1$age, data1$gender,
               data1$modPA_day.week, data1$walk_day.week), method = "spearman")

### negative images and vigorous days per week ###
###                                ###
###                                ###
cor.test(cleaned.merged.data$Negative, cleaned.merged.data$vigPA_day.week, 
         method = "spearman", exact = FALSE)

data1 <- cleaned.merged.data[,c(1,3,6,8,10,19,21)]
data1 <- data1[complete.cases(data1),]
pcor.test(data1$Negative, data1$vigPA_day.week, 
          list(data1$age, data1$gender,
               data1$modPA_day.week, data1$walk_day.week), method = "spearman")

### ambiguous images and vigorous minutes per day ###
###                                ###
###                                ###
cor.test(cleaned.merged.data$Ambiguous, cleaned.merged.data$vigPA_min.day, 
         method = "spearman", exact = FALSE)

data1 <- cleaned.merged.data[,c(1,5,7,9,11,19,21)]
data1 <- data1[complete.cases(data1),]
pcor.test(data1$Ambiguous, data1$vigPA_min.day, 
          list(data1$age, data1$gender,
               data1$modPA_min.day, data1$walk_min.day), method = "spearman")

### positive images and vigorous minutes per day ###
###                                ###
###                                ###
cor.test(cleaned.merged.data$Positive, cleaned.merged.data$vigPA_min.day, 
         method = "spearman", exact = FALSE)

data1 <- cleaned.merged.data[,c(1,4,7,9,11,19,21)]
data1 <- data1[complete.cases(data1),]
pcor.test(data1$Positive, data1$vigPA_min.day, 
          list(data1$age, data1$gender,
               data1$modPA_min.day, data1$walk_min.day), method = "spearman")

### negative images and vigorous minutes per day ###
###                                ###
###                                ###
cor.test(cleaned.merged.data$Negative, cleaned.merged.data$vigPA_min.day, 
         method = "spearman", exact = FALSE)

data1 <- cleaned.merged.data[,c(1,3,7,9,11,19,21)]
data1 <- data1[complete.cases(data1),]
pcor.test(data1$Negative, data1$vigPA_min.day, 
          list(data1$age, data1$gender,
               data1$modPA_min.day, data1$walk_min.day), method = "spearman")

}

### Moderate ###
{
  ### ambiguous images and moderate days per week ###
  ###                                ###
  ###                                ###
  cor.test(cleaned.merged.data$Ambiguous, cleaned.merged.data$modPA_day.week, 
           method = "spearman", exact = FALSE)
  
  data1 <- cleaned.merged.data[,c(1,5,6,8,10,19,21)]
  data1 <- data1[complete.cases(data1),]
  pcor.test(data1$Ambiguous, data1$modPA_day.week, 
            list(data1$age, data1$gender,
                 data1$vigPA_day.week, data1$walk_day.week), method = "spearman")
  
  ### positive images and moderate days per week ###
  ###                                ###
  ###                                ###
  cor.test(cleaned.merged.data$Positive, cleaned.merged.data$modPA_day.week, 
           method = "spearman", exact = FALSE)
  
  data1 <- cleaned.merged.data[,c(1,4,6,8,10,19,21)]
  data1 <- data1[complete.cases(data1),]
  pcor.test(data1$Positive, data1$modPA_day.week, 
            list(data1$age, data1$gender,
                 data1$vigPA_day.week, data1$walk_day.week), method = "spearman")
  
  ### negative images and moderate days per week ###
  ###                                ###
  ###                                ###
  cor.test(cleaned.merged.data$Negative, cleaned.merged.data$modPA_day.week, 
           method = "spearman", exact = FALSE)
  
  data1 <- cleaned.merged.data[,c(1,3,6,8,10,19,21)]
  data1 <- data1[complete.cases(data1),]
  pcor.test(data1$Negative, data1$modPA_day.week, 
            list(data1$age, data1$gender,
                 data1$vigPA_day.week, data1$walk_day.week), method = "spearman")
  
  ### ambiguous images and moderate minutes per day ###
  ###                                ###
  ###                                ###
  cor.test(cleaned.merged.data$Ambiguous, cleaned.merged.data$modPA_min.day, 
           method = "spearman", exact = FALSE)
  
  data1 <- cleaned.merged.data[,c(1,5,7,9,11,19,21)]
  data1 <- data1[complete.cases(data1),]
  pcor.test(data1$Ambiguous, data1$modPA_min.day, 
            list(data1$age, data1$gender,
                 data1$vigPA_min.day, data1$walk_min.day), method = "spearman")
  
  ### positive images and moderate minutes per day ###
  ###                                ###
  ###                                ###
  cor.test(cleaned.merged.data$Positive, cleaned.merged.data$modPA_min.day, 
           method = "spearman", exact = FALSE)
  
  data1 <- cleaned.merged.data[,c(1,4,7,9,11,19,21)]
  data1 <- data1[complete.cases(data1),]
  pcor.test(data1$Positive, data1$modPA_min.day, 
            list(data1$age, data1$gender,
                 data1$vigPA_min.day, data1$walk_min.day), method = "spearman")
  
  ### negative images and moderate minutes per day ###
  ###                                ###
  ###                                ###
  cor.test(cleaned.merged.data$Negative, cleaned.merged.data$modPA_min.day, 
           method = "spearman", exact = FALSE)
  
  data1 <- cleaned.merged.data[,c(1,3,7,9,11,19,21)]
  data1 <- data1[complete.cases(data1),]
  pcor.test(data1$Negative, data1$modPA_min.day, 
            list(data1$age, data1$gender,
                 data1$vigPA_min.day, data1$walk_min.day), method = "spearman")
}

### Walking ###
{
  ### ambiguous images and walking days per week ###
  ###                                ###
  ###                                ###
  cor.test(cleaned.merged.data$Ambiguous, cleaned.merged.data$walk_day.week, 
           method = "spearman", exact = FALSE)
  
  data1 <- cleaned.merged.data[,c(1,5,6,8,10,19,21)]
  data1 <- data1[complete.cases(data1),]
  pcor.test(data1$Ambiguous, data1$walk_day.week, 
            list(data1$age, data1$gender,
                 data1$vigPA_day.week, data1$modPA_day.week), method = "spearman")
  
  ### positive images and walking days per week ###
  ###                                ###
  ###                                ###
  cor.test(cleaned.merged.data$Positive, cleaned.merged.data$walk_day.week, 
           method = "spearman", exact = FALSE)
  
  data1 <- cleaned.merged.data[,c(1,4,6,8,10,19,21)]
  data1 <- data1[complete.cases(data1),]
  pcor.test(data1$Positive, data1$walk_day.week, 
            list(data1$age, data1$gender,
                 data1$vigPA_day.week, data1$modPA_day.week), method = "spearman")
  
  ### negative images and walking days per week ###
  ###                                ###
  ###                                ###
  cor.test(cleaned.merged.data$Negative, cleaned.merged.data$walk_day.week, 
           method = "spearman", exact = FALSE)
  
  data1 <- cleaned.merged.data[,c(1,3,6,8,10,19,21)]
  data1 <- data1[complete.cases(data1),]
  pcor.test(data1$Negative, data1$walk_day.week, 
            list(data1$age, data1$gender,
                 data1$vigPA_day.week, data1$modPA_day.week), method = "spearman")
  
  ### ambiguous images and walking minutes per day ###
  ###                                ###
  ###                                ###
  cor.test(cleaned.merged.data$Ambiguous, cleaned.merged.data$walk_min.day, 
           method = "spearman", exact = FALSE)
  
  data1 <- cleaned.merged.data[,c(1,5,7,9,11,19,21)]
  data1 <- data1[complete.cases(data1),]
  pcor.test(data1$Ambiguous, data1$walk_min.day, 
            list(data1$age, data1$gender,
                 data1$vigPA_min.day, data1$modPA_min.day), method = "spearman")
  
  ### positive images and walking minutes per day ###
  ###                                ###
  ###                                ###
  cor.test(cleaned.merged.data$Positive, cleaned.merged.data$walk_min.day, 
           method = "spearman", exact = FALSE)
  
  data1 <- cleaned.merged.data[,c(1,4,7,9,11,19,21)]
  data1 <- data1[complete.cases(data1),]
  pcor.test(data1$Positive, data1$walk_min.day, 
            list(data1$age, data1$gender,
                 data1$vigPA_min.day, data1$modPA_min.day), method = "spearman")
  
  ### negative images and walking minutes per day ###
  ###                                ###
  ###                                ###
  cor.test(cleaned.merged.data$Negative, cleaned.merged.data$walk_min.day, 
           method = "spearman", exact = FALSE)
  
  data1 <- cleaned.merged.data[,c(1,3,7,9,11,19,21)]
  data1 <- data1[complete.cases(data1),]
  pcor.test(data1$Negative, data1$walk_min.day, 
            list(data1$age, data1$gender,
                 data1$vigPA_min.day, data1$modPA_min.day), method = "spearman")  
}

### Dose-dependent relationship - Vigorous days per week - Ambiguous ###
{
### dummy code ###
cleaned.merged.data <- subset(cleaned.merged.data, gender <= 2)

cleaned.merged.data$vigPA_day.week.f <- factor(cleaned.merged.data$vigPA_day.week, ordered = FALSE)

### reference 0 ###
cleaned.merged.data <- cleaned.merged.data %>%
  mutate(vigPA_day.week.f = relevel(vigPA_day.week.f, ref = "0"))

cleaned.merged.data <- within(cleaned.merged.data, {
  vigPA_day.week.ct0 <- C(vigPA_day.week.f, treatment)
  print(attributes(vigPA_day.week.ct0))
})

summary(lm(Ambiguous ~ vigPA_day.week.ct0 + age + gender, data = cleaned.merged.data))


### reference 1 ###
cleaned.merged.data <- cleaned.merged.data %>%
  mutate(vigPA_day.week.f = relevel(vigPA_day.week.f, ref = "1"))

cleaned.merged.data <- within(cleaned.merged.data, {
  vigPA_day.week.ct1 <- C(vigPA_day.week.f, treatment)
  print(attributes(vigPA_day.week.ct1))
})

summary(lm(Ambiguous ~ vigPA_day.week.ct1 + age + gender, data = cleaned.merged.data))


### reference 2 ###
cleaned.merged.data <- cleaned.merged.data %>%
  mutate(vigPA_day.week.f = relevel(vigPA_day.week.f, ref = "2"))

cleaned.merged.data <- within(cleaned.merged.data, {
  vigPA_day.week.ct2 <- C(vigPA_day.week.f, treatment)
  print(attributes(vigPA_day.week.ct2))
})

summary(lm(Ambiguous ~ vigPA_day.week.ct2 + age + gender, data = cleaned.merged.data))

### reference 3 ###
cleaned.merged.data <- cleaned.merged.data %>%
  mutate(vigPA_day.week.f = relevel(vigPA_day.week.f, ref = "3"))

cleaned.merged.data <- within(cleaned.merged.data, {
  vigPA_day.week.ct3 <- C(vigPA_day.week.f, treatment)
  print(attributes(vigPA_day.week.ct3))
})

summary(lm(Ambiguous ~ vigPA_day.week.ct3 + age + gender, data = cleaned.merged.data))

### reference 4 ###
cleaned.merged.data <- cleaned.merged.data %>%
  mutate(vigPA_day.week.f = relevel(vigPA_day.week.f, ref = "4"))

cleaned.merged.data <- within(cleaned.merged.data, {
  vigPA_day.week.ct4 <- C(vigPA_day.week.f, treatment)
  print(attributes(vigPA_day.week.ct4))
})

summary(lm(Ambiguous ~ vigPA_day.week.ct4 + age + gender, data = cleaned.merged.data))

### reference 5 ###
cleaned.merged.data <- cleaned.merged.data %>%
  mutate(vigPA_day.week.f = relevel(vigPA_day.week.f, ref = "5"))

cleaned.merged.data <- within(cleaned.merged.data, {
  vigPA_day.week.ct5 <- C(vigPA_day.week.f, treatment)
  print(attributes(vigPA_day.week.ct5))
})

summary(lm(Ambiguous ~ vigPA_day.week.ct5 + age + gender, data = cleaned.merged.data))

### reference 6 ###
cleaned.merged.data <- cleaned.merged.data %>%
  mutate(vigPA_day.week.f = relevel(vigPA_day.week.f, ref = "6"))

cleaned.merged.data <- within(cleaned.merged.data, {
  vigPA_day.week.ct6 <- C(vigPA_day.week.f, treatment)
  print(attributes(vigPA_day.week.ct6))
})

summary(lm(Ambiguous ~ vigPA_day.week.ct6 + age + gender, data = cleaned.merged.data))
}

### Dose-dependent relationship - Vigorous days per week - Negative ###
{
  ### dummy code ###
  cleaned.merged.data <- subset(cleaned.merged.data, gender <= 2)
  
  cleaned.merged.data$vigPA_day.week.f <- factor(cleaned.merged.data$vigPA_day.week, ordered = FALSE)
  
  ### reference 0 ###
  cleaned.merged.data <- cleaned.merged.data %>%
    mutate(vigPA_day.week.f = relevel(vigPA_day.week.f, ref = "0"))
  
  cleaned.merged.data <- within(cleaned.merged.data, {
    vigPA_day.week.ct0 <- C(vigPA_day.week.f, treatment)
    print(attributes(vigPA_day.week.ct0))
  })
  
  summary(lm(Negative ~ vigPA_day.week.ct0 + age + gender, data = cleaned.merged.data))
  
  
  ### reference 1 ###
  cleaned.merged.data <- cleaned.merged.data %>%
    mutate(vigPA_day.week.f = relevel(vigPA_day.week.f, ref = "1"))
  
  cleaned.merged.data <- within(cleaned.merged.data, {
    vigPA_day.week.ct1 <- C(vigPA_day.week.f, treatment)
    print(attributes(vigPA_day.week.ct1))
  })
  
  summary(lm(Negative ~ vigPA_day.week.ct1 + age + gender, data = cleaned.merged.data))
  
  
  ### reference 2 ###
  cleaned.merged.data <- cleaned.merged.data %>%
    mutate(vigPA_day.week.f = relevel(vigPA_day.week.f, ref = "2"))
  
  cleaned.merged.data <- within(cleaned.merged.data, {
    vigPA_day.week.ct2 <- C(vigPA_day.week.f, treatment)
    print(attributes(vigPA_day.week.ct2))
  })
  
  summary(lm(Negative ~ vigPA_day.week.ct2 + age + gender, data = cleaned.merged.data))
  
  ### reference 3 ###
  cleaned.merged.data <- cleaned.merged.data %>%
    mutate(vigPA_day.week.f = relevel(vigPA_day.week.f, ref = "3"))
  
  cleaned.merged.data <- within(cleaned.merged.data, {
    vigPA_day.week.ct3 <- C(vigPA_day.week.f, treatment)
    print(attributes(vigPA_day.week.ct3))
  })
  
  summary(lm(Negative ~ vigPA_day.week.ct3 + age + gender, data = cleaned.merged.data))
  
  ### reference 4 ###
  cleaned.merged.data <- cleaned.merged.data %>%
    mutate(vigPA_day.week.f = relevel(vigPA_day.week.f, ref = "4"))
  
  cleaned.merged.data <- within(cleaned.merged.data, {
    vigPA_day.week.ct4 <- C(vigPA_day.week.f, treatment)
    print(attributes(vigPA_day.week.ct4))
  })
  
  summary(lm(Negative ~ vigPA_day.week.ct4 + age + gender, data = cleaned.merged.data))
  
  ### reference 5 ###
  cleaned.merged.data <- cleaned.merged.data %>%
    mutate(vigPA_day.week.f = relevel(vigPA_day.week.f, ref = "5"))
  
  cleaned.merged.data <- within(cleaned.merged.data, {
    vigPA_day.week.ct5 <- C(vigPA_day.week.f, treatment)
    print(attributes(vigPA_day.week.ct5))
  })
  
  summary(lm(Negative ~ vigPA_day.week.ct5 + age + gender, data = cleaned.merged.data))
  
  ### reference 6 ###
  cleaned.merged.data <- cleaned.merged.data %>%
    mutate(vigPA_day.week.f = relevel(vigPA_day.week.f, ref = "6"))
  
  cleaned.merged.data <- within(cleaned.merged.data, {
    vigPA_day.week.ct6 <- C(vigPA_day.week.f, treatment)
    print(attributes(vigPA_day.week.ct6))
  })
  
  summary(lm(Negative ~ vigPA_day.week.ct6 + age + gender, data = cleaned.merged.data))
}

### Dose-dependent relationship - Vigorous minute bins ###
{
  ### dummy code ###
  cleaned.merged.data <- subset(cleaned.merged.data, gender <= 2)
  cleaned.merged.data$vigPA_min.day.f <- cleaned.merged.data$vigPA_min.day
  
  
  cleaned.merged.data$vigPA_min.day.f <- ifelse(cleaned.merged.data$vigPA_min.day.f == 0, 0,
                                                ifelse(((cleaned.merged.data$vigPA_min.day.f >= 5) & (cleaned.merged.data$vigPA_min.day.f <= 25)), 1,
                                                       ifelse(((cleaned.merged.data$vigPA_min.day.f >= 30) & (cleaned.merged.data$vigPA_min.day.f <= 35)), 2,
                                                              ifelse(((cleaned.merged.data$vigPA_min.day.f >= 40) & (cleaned.merged.data$vigPA_min.day.f <= 56)), 3,
                                                                     ifelse(cleaned.merged.data$vigPA_min.day.f >= 60, 4, NA)))))
  
  cleaned.merged.data$vigPA_min.day.f <- factor(cleaned.merged.data$vigPA_min.day.f, ordered = FALSE)
  
  ### reference 0 ###
  cleaned.merged.data <- cleaned.merged.data %>%
    mutate(vigPA_min.day.f = relevel(vigPA_min.day.f, ref = "0"))
  
  cleaned.merged.data <- within(cleaned.merged.data, {
    vigPA_min.day.ct0 <- C(vigPA_min.day.f, treatment)
    print(attributes(vigPA_min.day.ct0))
  })
  
  summary(lm(Ambiguous ~ vigPA_min.day.ct0 + age + gender, data = cleaned.merged.data))
  
  
  ### reference 1 ###
  cleaned.merged.data <- cleaned.merged.data %>%
    mutate(vigPA_min.day.f = relevel(vigPA_min.day.f, ref = "1"))
  
  cleaned.merged.data <- within(cleaned.merged.data, {
    vigPA_min.day.ct1 <- C(vigPA_min.day.f, treatment)
    print(attributes(vigPA_min.day.ct1))
  })
  
  summary(lm(Ambiguous ~ vigPA_min.day.ct1 + age + gender, data = cleaned.merged.data))
  
  
  ### reference 2 ###
  cleaned.merged.data <- cleaned.merged.data %>%
    mutate(vigPA_min.day.f = relevel(vigPA_min.day.f, ref = "2"))
  
  cleaned.merged.data <- within(cleaned.merged.data, {
    vigPA_min.day.ct2 <- C(vigPA_min.day.f, treatment)
    print(attributes(vigPA_min.day.ct2))
  })
  
  summary(lm(Ambiguous ~ vigPA_min.day.ct2 + age + gender, data = cleaned.merged.data))
  
  ### reference 3 ###
  cleaned.merged.data <- cleaned.merged.data %>%
    mutate(vigPA_min.day.f = relevel(vigPA_min.day.f, ref = "3"))
  
  cleaned.merged.data <- within(cleaned.merged.data, {
    vigPA_min.day.ct3 <- C(vigPA_min.day.f, treatment)
    print(attributes(vigPA_min.day.ct3))
  })
  
  summary(lm(Ambiguous ~ vigPA_min.day.ct3 + age + gender, data = cleaned.merged.data))
  
}

### Exercise and images correlation ##
{
data1 <- cleaned.merged.data[,c(1,5,12,19,21)]
data1 <- data1[complete.cases(data1),]
pcor.test(data1$Ambiguous, data1$exercise_hours.week,
          list(data1$age, data1$gender), method = "spearman")

data1 <- cleaned.merged.data[,c(1,4,12,19,21)]
data1 <- data1[complete.cases(data1),]
pcor.test(data1$Positive, data1$exercise_hours.week,
          list(data1$age, data1$gender), method = "spearman")

data1 <- cleaned.merged.data[,c(1,3,12,19,21)]
data1 <- data1[complete.cases(data1),]
pcor.test(data1$Negative, data1$exercise_hours.week,
          list(data1$age, data1$gender), method = "spearman")
}



cleaned.merged.data %>% count(vigPA_min.day.f)


sd(cleaned.merged.data$walk_min.week, na.rm = TRUE)
sd(merged.data$`Total Sports MET Minutes (without assumed 0's).x`, na.rm = TRUE)
