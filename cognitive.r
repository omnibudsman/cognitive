library(tidyverse)
library(scales)

setwd(Sys.getenv(YOUTH_DIR))

############################ Part 1: ECLSK data ############################ 
# Source: https://nces.ed.gov/ecls/dataproducts.asp#K-5
# Codebook: https://nces.ed.gov/OnlineCodebook/Session/Codebook/6f277751-95d0-4299-a1e8-e3a9f366fbb9

# Variables
# WKBLACK - race = black
# WKWHITE - race = white
# C1R4RTSC  994-999       C1 RC4 READING T-SCORE
# C1R4MTSC  1086-1091     C1 RC4 MATH T-SCORE
# WKPOV_R   3385-3385     WK POVERTY LEVEL (REVISED)
# WKINCOME  3386-3395     WK INCOME (IMPUTED)
# WKSESL    3379-3383     WK CONTINUOUS SES MEASURE
# WKSESQ5   3384-3384     WK CATEGORICAL SES MEASURE
# C7R4RTSC  2310-2315     C7 RC4 READING T-SCORE
# C7R4MTSC  2400-2405     C7 RC4 MATH T-SCORE
# P1PREMAT  5930-5931     P1 CHQ025 MORE THAN 2 WEEKS EARLY # child was premature, 1 = yes
# W1PARED   3503-3503     W1 PARENT HIGHEST EDUCATION LEVEL

########## Prep to read file

# big raw file with 18k variables
header <- as.vector(read.csv("eclsk/ECLSK_1998-99_v1_0_CSV_Datasets/ECLSK_98_99_K8_CHILD_v1_0.csv", nrows=1, header=F)[1,])
cols.to.read <- c("WKBLACK","WKWHITE","C1R4RTSC","C1R4MTSC","WKPOV_R","WKINCOME","WKSESL","WKSESQ5","C7R4RTSC","C7R4MTSC", "P1PREMAT", "W1PARED")

filter.func <- function(x) {
  if(x %in% cols.to.read) {
    return(NA)
  } else {
    return("NULL")
  }
}

header.filter <- as.vector(apply(header, 2, filter.func))

# Read file with only variables we care about
eclsk.raw <- read.csv("eclsk/ECLSK_1998-99_v1_0_CSV_Datasets/ECLSK_98_99_K8_CHILD_v1_0.csv", colClasses = header.filter)

########## Clean data by creating new variables

# clean
eclsk <- eclsk.raw %>%
  mutate(race = "") %>%
  subset(!is.na(WKBLACK) & !is.na(WKWHITE))

eclsk[eclsk$WKBLACK == 1,]$race <- "Black"
eclsk[eclsk$WKWHITE == 1,]$race <- "White"

eclsk.clean <- eclsk %>% subset(race != "")

eclsk.clean$premature <- as.numeric(eclsk.clean$P1PREMAT == 1)


#### TODO HERE: cleanup variables by removing negatives
#### TODO HERE: remove outliers?

# colors
omni.colors <- c("#676a70","#6a7067","#70676a")

########## Create new variables about scores etc.

eclsk.clean$overall.score <- eclsk.clean$C1R4MTSC + eclsk.clean$C1R4RTSC
eclsk.clean$overall.score.grade8 <- eclsk.clean$C7R4RTSC + eclsk.clean$C7R4MTSC

# size of achievement gap?
(mean(eclsk.clean[eclsk.clean$race == "Black",]$overall.score, na.rm=T) - mean(eclsk.clean[eclsk.clean$race == "White",]$overall.score, na.rm=T))/sd(eclsk.clean$overall.score, na.rm=T)

########## Gap between white and black kindergartners on cognitive test scores

# histograms overlaid / densities
ggplot(eclsk.clean) +
  geom_density(aes(x = overall.score, fill = race), alpha=0.5) +
  theme_minimal() +
  theme(text=element_text(size=12, family="Raleway"), plot.margin = margin(20,20,20,20, unit="pt")) +
    ggtitle("Cognitive score distributions at kindergarten") +
    xlab("Aggregate score") +
    ylab("Density") + 
  scale_fill_discrete(name = "")
ggsave("kindergarten_gap_density.png", width=10, height=6)

eclsk.byrace <- eclsk.clean %>% group_by(race) %>% summarise(average.score = mean(overall.score, na.rm=T))

# difference in means
ggplot(eclsk.byrace) +
  geom_line(aes(x = race, y = average.score, group=1), alpha=0.5) +
  geom_point(aes(x = race, y = average.score, color=race), size=5) +
  ylim(80,120) +
  theme_minimal() +
  theme(text=element_text(size=12, family="Raleway"), plot.margin = margin(20,20,20,20, unit="pt")) +
  xlab("") +
  ylab("Average cognitive score") +
  scale_colour_discrete(name = "") +
  ggtitle("Differences in mean cognitive score at kindergarten")
ggsave("kindergarten_gap_means.png", width=10, height=6)


########## Gap by SES

### income (SES gradations) - density
ggplot(eclsk.clean) +
  geom_density(aes(x = overall.score, fill = as.factor(WKSESQ5)), alpha=0.5) +
  theme_minimal() +
  theme(text=element_text(size=12, family="Raleway"), plot.margin = margin(20,20,20,20, unit="pt")) +
  ggtitle("Cognitive score distributions at kindergarten") +
  xlab("Aggregate score") +
  ylab("Density") + 
  scale_fill_discrete(name = "SES Quintile (1st = lowest)")
ggsave("kindergarten_ses_density.png", width=10, height=6)

eclsk.income <- eclsk.clean %>% group_by(WKSESQ5) %>% summarise(average.score = mean(overall.score, na.rm=T))

# difference in means
ggplot(eclsk.income) +
  geom_line(aes(x = WKSESQ5, y = average.score, group=1), alpha=0.5) +
  geom_point(aes(x = WKSESQ5, y = average.score), size=5) +
  ylim(80,120) +
  theme_minimal() +
  theme(text=element_text(size=12, family="Raleway"), plot.margin = margin(20,20,20,20, unit="pt")) +
  xlab("SES Quintile (1st = lowest)") +
  ylab("Average cognitive score") +
  scale_colour_discrete(name = "") +
  ggtitle("Differences in mean cognitive score at kindergarten")
ggsave("kindergarten_ses_means.png", width=10, height=6)


########## Gap by SES for educated parents only

educated.parents <- eclsk.clean %>% subset(W1PARED >= 6)
educated.parents.means <- educated.parents %>% group_by(WKSESQ5) %>% summarise(mean.score = mean(overall.score, na.rm=T))

ggplot(educated.parents.means) +
  geom_line(aes(x = WKSESQ5, y = mean.score, group=1), alpha=0.5) +
  geom_point(aes(x = WKSESQ5, y = mean.score), size=5) +
  ylim(80,120) +
  theme_minimal() +
  theme(text=element_text(size=12, family="Raleway"), plot.margin = margin(20,20,20,20, unit="pt"))+
  xlab("SES Quintile (1st = lowest)") +
  ylab("Average cognitive score") +
  scale_colour_discrete(name = "") +
  ggtitle("Differences in mean cognitive score at kindergarten\n(College-educated parents only)")
ggsave("educated_parents_means.png", width=10, height=6)

########## Kindergarten vs eighth-grade scores

# eliminating negative scores for nonresponse
eclsk.cleanscores <- eclsk.clean %>% subset(C1R4MTSC >= 0 & C1R4RTSC >= 0 & C7R4RTSC >= 0 & C7R4MTSC)

ggplot(eclsk.cleanscores) +
  geom_point(aes(x = overall.score, y = overall.score.grade8), color="#676a70") +
  geom_smooth(aes(x = overall.score, y = overall.score.grade8), method="lm") +
  theme_minimal() +
  theme(text=element_text(size=12, family="Raleway"), plot.margin = margin(20,20,20,20, unit="pt")) +
  xlab("Kindergarten cognitive score") +
  ylab("8th grade cognitive score") +
  scale_colour_discrete(name = "") +
  ggtitle("Kindergarten test scores are highly predictive\nof eighth-grade test scores")
ggsave("k_vs_8.png", width=10, height=6)


############################ Part 2: NLSY data############################ 

# Source: https://nlsinfo.org/investigator/pages/search?s=NLSY97

# Variables
# R0536402 year of birth
# Z9033700 highest SAT math score, 2007
# Z9033900 highest SAT verbal score, 2007
# U4282300 2019 income
# S1552700 2002 math piat standard score
# S1552600 2002 math piat percentile
# R5473700 2000 math piat standard score
# R5473600 2000 math piat percentile
# R1204500  gross household income, 1997
# R9871900 overall gpa
# E8033100 number of arrests
# TODO HERE - likelihood of employment

nlsy.raw <- read.csv("default/default.csv")


########## Create new variables

# just create new variables
nlsy.raw$birth.year <- nlsy.raw$R0536402
nlsy.raw$income.2019 <- nlsy.raw$U4282300
nlsy.raw$highest.sat.math.2007 <- nlsy.raw$Z9033700
nlsy.raw$highest.sat.verbal.2007 <- nlsy.raw$Z9033900
nlsy.raw$highest.sat <- nlsy.raw$highest.sat.math.2007 + nlsy.raw$highest.sat.verbal.2007
nlsy.raw$piat.standard <- nlsy.raw$R5473700
nlsy.raw$piat.percentile <- nlsy.raw$R5473600
nlsy.raw$piat.percentile.bucket <- cut(nlsy.raw$piat.percentile, 5, labels=1:5)
nlsy.raw$gpa <- nlsy.raw$R9871900/100
nlsy.raw$income.ptile <- percent_rank(nlsy.raw$income.2019)
nlsy.raw$parent.income.ptile <- cut(nlsy.raw$R9871900, quantile(nlsy.raw$R9871900, probs=c(0:5/5)), labels=1:5)
nlsy.raw$race <- plyr::mapvalues(nlsy.raw$R1482600, c(1,2,3,4), c("Black or Hispanic","Black or Hispanic","Mixed Race","Other")) # 1 hispanic, 2 black, 3 non-black, non hispanic
nlsy.raw$highest.grade <- nlsy.raw$Z9083800
nlsy.raw$arrested <- as.numeric(nlsy.raw$E8033100 > 0)

nlsy.withpiat.grades <- nlsy.raw %>%
  subset(piat.standard > 0 & highest.grade > 0) %>%
  subset(race != "Mixed Race")

########## Junior high PIAT vs 4-year colleges

nlsy.college <- nlsy.withpiat.grades %>% group_by(piat.percentile.bucket) %>% summarise(pct.college = round(100*mean(highest.grade >= 16)))
ggplot(nlsy.college, aes(x = piat.percentile.bucket, y = pct.college, group=1)) +
  geom_point() +
  geom_line() +
  theme_minimal() +
  theme(text=element_text(size=12, family="Raleway"), plot.margin = margin(20,20,20,20, unit="pt")) +
  xlab("Quintile on cognitive test") +
  ylab("Percent who attended\nfour years of college\n") +
  scale_colour_discrete(name = "") +
  ggtitle("Higher-scoring junior high students\nare more likely to attend college")
ggsave("piat_vs_college.png", width=10, height=6)


########## PIAT vs mean income in 2019

nlsy.withpiat.income <- nlsy.raw %>% 
  subset(piat.standard > 0 & income.2019 >= 0)

nlsy.income <- nlsy.withpiat.income %>% group_by(piat.percentile.bucket) %>% summarise(mean.income = mean(income.2019, na.rm=T))

ggplot(nlsy.income, aes(x = piat.percentile.bucket, y = mean.income, group = 1)) +
  geom_point() +
  geom_line() +
  theme_minimal() +
  theme(text=element_text(size=12, family="Raleway"), plot.margin = margin(20,20,20,20, unit="pt")) +
  scale_y_continuous(labels=comma, limits=c(30000,80000)) +
  xlab("Quintile on cognitive test") +
  ylab("Average income in 2019") +
  scale_colour_discrete(name = "") +
  ggtitle("Higher-scoring junior high students\nhave higher incomes as adults")
ggsave("piat_vs_income.png", width=10, height=6)

########## PIAT vs probability of arrest

nlsy.withpiat.arrests <- nlsy.raw %>%
  subset(piat.standard > 0 & E8033100 >= 0) %>%
  subset(race != "Mixed Race")

nlsy.arrests <- nlsy.withpiat.arrests %>% group_by(piat.percentile.bucket) %>% summarise(percent.arrested = round(100*mean(arrested)))

ggplot(nlsy.arrests, aes(x = piat.percentile.bucket, y = percent.arrested, group = 1)) +
  geom_point() +
  geom_line() +
  theme_minimal() +
  theme(text=element_text(size=12, family="Raleway"), plot.margin = margin(20,20,20,20, unit="pt")) +
  xlab("Quintile on cognitive test") +
  ylab("Likelihood of arrest as adult") +
  scale_colour_discrete(name = "") +
  ggtitle("Higher-scoring junior high students\nare less likely to be arrested")
ggsave("piat_vs_arrests.png", width=10, height=6)






