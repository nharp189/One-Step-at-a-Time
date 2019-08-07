setwd("/Users/nicholasharp/Documents/Nick-Grad/Neta_Lab/exercise_study/Bayes-R/Cross-Sectional-Exercise/")

### install packages if needed ###
#install.packages("Rfast")
#install.packages("BayesMed")
#install.packages("emmeans")
#install.packages("ppcor")

### load packages quietly ###
suppressPackageStartupMessages(library(readxl))
suppressPackageStartupMessages(library(psych))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(ppcor))

### import data ###
new.data <- read_excel("data/Pilot Data - Health&Pol FINAL.xlsx", sheet = 2)
old.data <- read_excel("data/Maital PA and EX data_KK.xlsx", sheet = 2)
merged.data <- merge.data.frame(new.data, old.data, by = "Participant ID")
#write.csv(merged.data, "merged_data.csv")

### select variables for analysis ###
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

### Calculate minutes per week ###
cleaned.merged.data$vig_min.week <- (cleaned.merged.data$vigPA_min.day * cleaned.merged.data$vigPA_day.week)
cleaned.merged.data$mod_min.week <- (cleaned.merged.data$modPA_min.day * cleaned.merged.data$modPA_day.week)
cleaned.merged.data$walk_min.week <- (cleaned.merged.data$walk_min.day * cleaned.merged.data$walk_day.week)

### calculate descriptives (Table 1) ###
{
### Percent Negative Ratings - Negative Images ###
mean(cleaned.merged.data$Negative, na.rm = TRUE)
sd(cleaned.merged.data$Negative, na.rm = TRUE)
### Percent  Negative Ratings - Positive Images ###
mean(cleaned.merged.data$Positive, na.rm = TRUE)
sd(cleaned.merged.data$Positive, na.rm = TRUE)
### Percent Negative Ratings - Ambiguous Images ###
mean(cleaned.merged.data$Ambiguous, na.rm = TRUE)
sd(cleaned.merged.data$Ambiguous, na.rm = TRUE)
### Mean & SD Age ###
mean(cleaned.merged.data$age, na.rm = TRUE)
sd(cleaned.merged.data$age, na.rm = TRUE)
### Mean & SD Education ###
cleaned.merged.data$education <- recode(cleaned.merged.data$education, "1" = "Some high school",
                                        "2" = "High school diploma or GED",
                                        "3" = "Some college",
                                        "4" = "Associates",
                                        "5" = "Bachelors",
                                        "6" = "Masters",
                                        "7" = "Professional degree",
                                        "8" = "Trade, technical, or vocational training",
                                        "9" = "PhD, medical, or law degree")

count(cleaned.merged.data, education)
### Count Race ###
count(cleaned.merged.data, race)
### Mean & SD Income Levels ###
mean(cleaned.merged.data$income, na.rm = TRUE)
sd(cleaned.merged.data$income, na.rm = TRUE)
### Mean & SD - Walking (minutes per week) ###
mean(cleaned.merged.data$walk_min.week, na.rm = TRUE)
sd(cleaned.merged.data$walk_min.week, na.rm = TRUE)
### Mean & SD - Moderate PA (minutes per week) ###
mean(cleaned.merged.data$mod_min.week, na.rm = TRUE)
sd(cleaned.merged.data$mod_min.week, na.rm = TRUE)
### Mean & SD - Vigorous (minutes per week) ###
mean(cleaned.merged.data$vig_min.week, na.rm = TRUE)
sd(cleaned.merged.data$vig_min.week, na.rm = TRUE)
### Mean & SD - Exercise (METmins per week) ###
mean(merged.data$`Total Sports MET Minutes (without assumed 0's).x`, na.rm = TRUE)
sd(merged.data$`Total Sports MET Minutes (without assumed 0's).y`, na.rm = TRUE)
### Mean & SD - State-Trait Anxiety Inventory - State ###
mean(merged.data$`STAI-S`, na.rm = TRUE)
sd(merged.data$`STAI-S`, na.rm = TRUE)
### Mean & SD - State-Trait Anxiety Inventory - Trait ###
mean(merged.data$`STAI-T`, na.rm = TRUE)
sd(merged.data$`STAI-T`, na.rm = TRUE)
}

### reduce data to male/female binary sex  ###
cleaned.merged.data <- subset(cleaned.merged.data, gender <= 2)

### Total MET x Valence Bias ###
### ambiguous images and total MET ###
data1 <- cleaned.merged.data[,c(1,5,17,19,21)]
data1 <- data1[complete.cases(data1),]
pcor.test(data1$Ambiguous, data1$total_MET, 
          list(data1$age, data1$gender), method = "spearman")

### positive images and total MET ###
data1 <- cleaned.merged.data[,c(1,4,17,19,21)]
data1 <- data1[complete.cases(data1),]
pcor.test(data1$Positive, data1$total_MET, 
          list(data1$age, data1$gender), method = "spearman")

### negative images and total MET ###
data1 <- cleaned.merged.data[,c(1,3,17,19,21)]
data1 <- data1[complete.cases(data1),]
pcor.test(data1$Negative, data1$total_MET, 
          list(data1$age, data1$gender), method = "spearman")

### Percent Negative Ratings - Minutes per week analyses ###
{
### ambiguous images and vigorous  ###
data1 <- cleaned.merged.data[,c(1,5,19,21,31:33)]
data1 <- data1[complete.cases(data1),]
pcor.test(data1$Ambiguous, data1$vig_min.week, 
            list(data1$age, data1$gender,
                 data1$mod_min.week, data1$walk_min.week), method = "spearman")
  
### positive images and vigorous  ###
data1 <- cleaned.merged.data[,c(1,4,19,21,31:33)]
data1 <- data1[complete.cases(data1),]
pcor.test(data1$Positive, data1$vig_min.week, 
            list(data1$age, data1$gender,
                 data1$mod_min.week, data1$walk_min.week), method = "spearman")
  
### negative images and vigorous  ###
data1 <- cleaned.merged.data[,c(1,3,19,21,31:33)]
data1 <- data1[complete.cases(data1),]
pcor.test(data1$Negative, data1$vig_min.week, 
            list(data1$age, data1$gender,
                 data1$mod_min.week, data1$walk_min.week), method = "spearman")
  
### ambiguous images and moderate  ###
data1 <- cleaned.merged.data[,c(1,5,19,21,31:33)]
data1 <- data1[complete.cases(data1),]
pcor.test(data1$Ambiguous, data1$mod_min.week, 
            list(data1$age, data1$gender,
                 data1$vig_min.week, data1$walk_min.week), method = "spearman")
  
### positive images and moderate  ###
data1 <- cleaned.merged.data[,c(1,4,19,21,31:33)]
data1 <- data1[complete.cases(data1),]
pcor.test(data1$Positive, data1$mod_min.week, 
            list(data1$age, data1$gender,
                 data1$vig_min.week, data1$walk_min.week), method = "spearman")
  
### negative images and moderate  ###
data1 <- cleaned.merged.data[,c(1,3,19,21,31:33)]
data1 <- data1[complete.cases(data1),]
pcor.test(data1$Negative, data1$mod_min.week, 
            list(data1$age, data1$gender,
                 data1$vig_min.week, data1$walk_min.week), method = "spearman")
  
### ambiguous images and walk  ###
data1 <- cleaned.merged.data[,c(1,5,19,21,31:33)]
data1 <- data1[complete.cases(data1),]
pcor.test(data1$Ambiguous, data1$walk_min.week, 
            list(data1$age, data1$gender,
                 data1$vig_min.week, data1$mod_min.week), method = "spearman")
  
### positive images and walk  ###
data1 <- cleaned.merged.data[,c(1,4,19,21,31:33)]
data1 <- data1[complete.cases(data1),]
pcor.test(data1$Positive, data1$walk_min.week, 
            list(data1$age, data1$gender,
                 data1$vig_min.week, data1$mod_min.week), method = "spearman")
  
### negative images and walk  ###
data1 <- cleaned.merged.data[,c(1,3,19,21,31:33)]
data1 <- data1[complete.cases(data1),]
pcor.test(data1$Negative, data1$walk_min.week, 
            list(data1$age, data1$gender,
                 data1$vig_min.week, data1$mod_min.week), method = "spearman")
}

### Percent Negative Ratings - Ambiguous Images - Days per week and mins per day ###
### Vigorous ###
{
### ambiguous images and vigorous days per week ###
data1 <- cleaned.merged.data[,c(1,5,6,8,10,19,21)]
data1 <- data1[complete.cases(data1),]
pcor.test(data1$Ambiguous, data1$vigPA_day.week, 
          list(data1$age, data1$gender,
               data1$modPA_day.week, data1$walk_day.week), method = "spearman")

### ambiguous images and vigorous minutes per day ###
data1 <- cleaned.merged.data[,c(1,5,7,9,11,19,21)]
data1 <- data1[complete.cases(data1),]
pcor.test(data1$Ambiguous, data1$vigPA_min.day, 
          list(data1$age, data1$gender,
               data1$modPA_min.day, data1$walk_min.day), method = "spearman")
}

### Dose-dependent relationship - Vigorous days per week - Ambiguous ###
{
### make day/week factor ###
cleaned.merged.data$vigPA_day.week.f <- factor(cleaned.merged.data$vigPA_day.week, ordered = FALSE)

### Dummy code - reference group 0 days ###
cleaned.merged.data <- cleaned.merged.data %>%
  mutate(vigPA_day.week.f = relevel(vigPA_day.week.f, ref = "0"))

cleaned.merged.data <- within(cleaned.merged.data, {
  vigPA_day.week.ct0 <- C(vigPA_day.week.f, treatment)
  print(attributes(vigPA_day.week.ct0))
})

summary(lm(Ambiguous ~ vigPA_day.week.ct0 + age + gender, data = cleaned.merged.data))


### Dummy code - reference group 1 day ###
cleaned.merged.data <- cleaned.merged.data %>%
  mutate(vigPA_day.week.f = relevel(vigPA_day.week.f, ref = "1"))

cleaned.merged.data <- within(cleaned.merged.data, {
  vigPA_day.week.ct1 <- C(vigPA_day.week.f, treatment)
  print(attributes(vigPA_day.week.ct1))
})

summary(lm(Ambiguous ~ vigPA_day.week.ct1 + age + gender, data = cleaned.merged.data))


### Dummy code - reference group 2 days ###
cleaned.merged.data <- cleaned.merged.data %>%
  mutate(vigPA_day.week.f = relevel(vigPA_day.week.f, ref = "2"))

cleaned.merged.data <- within(cleaned.merged.data, {
  vigPA_day.week.ct2 <- C(vigPA_day.week.f, treatment)
  print(attributes(vigPA_day.week.ct2))
})

summary(lm(Ambiguous ~ vigPA_day.week.ct2 + age + gender, data = cleaned.merged.data))

### Dummy code - reference group 3 ###
cleaned.merged.data <- cleaned.merged.data %>%
  mutate(vigPA_day.week.f = relevel(vigPA_day.week.f, ref = "3"))

cleaned.merged.data <- within(cleaned.merged.data, {
  vigPA_day.week.ct3 <- C(vigPA_day.week.f, treatment)
  print(attributes(vigPA_day.week.ct3))
})

summary(lm(Ambiguous ~ vigPA_day.week.ct3 + age + gender, data = cleaned.merged.data))

### Dummy code - reference group 4 days ###
cleaned.merged.data <- cleaned.merged.data %>%
  mutate(vigPA_day.week.f = relevel(vigPA_day.week.f, ref = "4"))

cleaned.merged.data <- within(cleaned.merged.data, {
  vigPA_day.week.ct4 <- C(vigPA_day.week.f, treatment)
  print(attributes(vigPA_day.week.ct4))
})

summary(lm(Ambiguous ~ vigPA_day.week.ct4 + age + gender, data = cleaned.merged.data))

### Dummy code - reference group 5 days ###
cleaned.merged.data <- cleaned.merged.data %>%
  mutate(vigPA_day.week.f = relevel(vigPA_day.week.f, ref = "5"))

cleaned.merged.data <- within(cleaned.merged.data, {
  vigPA_day.week.ct5 <- C(vigPA_day.week.f, treatment)
  print(attributes(vigPA_day.week.ct5))
})

summary(lm(Ambiguous ~ vigPA_day.week.ct5 + age + gender, data = cleaned.merged.data))

### Dummy code - reference group 6 days ###
cleaned.merged.data <- cleaned.merged.data %>%
  mutate(vigPA_day.week.f = relevel(vigPA_day.week.f, ref = "6"))

cleaned.merged.data <- within(cleaned.merged.data, {
  vigPA_day.week.ct6 <- C(vigPA_day.week.f, treatment)
  print(attributes(vigPA_day.week.ct6))
})

summary(lm(Ambiguous ~ vigPA_day.week.ct6 + age + gender, data = cleaned.merged.data))
}

### Dose-dependent relationship - Vigorous minute bins ###
{
### make min per day factor ###
cleaned.merged.data$vigPA_min.day.f <- cleaned.merged.data$vigPA_min.day
  
  
cleaned.merged.data$vigPA_min.day.f <- ifelse(cleaned.merged.data$vigPA_min.day.f == 0, 0,
                                                ifelse(((cleaned.merged.data$vigPA_min.day.f >= 5) & (cleaned.merged.data$vigPA_min.day.f <= 25)), 1,
                                                       ifelse(((cleaned.merged.data$vigPA_min.day.f >= 30) & (cleaned.merged.data$vigPA_min.day.f <= 35)), 2,
                                                              ifelse(((cleaned.merged.data$vigPA_min.day.f >= 40) & (cleaned.merged.data$vigPA_min.day.f <= 56)), 3,
                                                                     ifelse(cleaned.merged.data$vigPA_min.day.f >= 60, 4, NA)))))
  
cleaned.merged.data$vigPA_min.day.f <- factor(cleaned.merged.data$vigPA_min.day.f, ordered = FALSE)
  
### Dummy code - reference group 0 minutes ###
cleaned.merged.data <- cleaned.merged.data %>%
    mutate(vigPA_min.day.f = relevel(vigPA_min.day.f, ref = "0"))
  
cleaned.merged.data <- within(cleaned.merged.data, {
    vigPA_min.day.ct0 <- C(vigPA_min.day.f, treatment)
    print(attributes(vigPA_min.day.ct0))
  })
  
summary(lm(Ambiguous ~ vigPA_min.day.ct0 + age + gender, data = cleaned.merged.data))
  
  
### Dummy code - reference group less than 30 mins ###
cleaned.merged.data <- cleaned.merged.data %>%
    mutate(vigPA_min.day.f = relevel(vigPA_min.day.f, ref = "1"))
  
cleaned.merged.data <- within(cleaned.merged.data, {
    vigPA_min.day.ct1 <- C(vigPA_min.day.f, treatment)
    print(attributes(vigPA_min.day.ct1))
  })
  
summary(lm(Ambiguous ~ vigPA_min.day.ct1 + age + gender, data = cleaned.merged.data))
  
  
### Dummy code - reference group 30-35 minutes ###
cleaned.merged.data <- cleaned.merged.data %>%
    mutate(vigPA_min.day.f = relevel(vigPA_min.day.f, ref = "2"))
  
cleaned.merged.data <- within(cleaned.merged.data, {
    vigPA_min.day.ct2 <- C(vigPA_min.day.f, treatment)
    print(attributes(vigPA_min.day.ct2))
  })
  
summary(lm(Ambiguous ~ vigPA_min.day.ct2 + age + gender, data = cleaned.merged.data))
  
### Dummy code - reference group 36-59 minutes ###
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

