setwd("D:/Documents/RStudio/MyProject")

library(dplyr)
library(tidyr)
library(ggplot2)
library(ecodist)
library(vegan)
library(factoextra)
library(robustbase)

require(foreign)
require(MASS)

citation("tidyr")

checklist.two <- read.csv("D:/Documents/RStudio/MyProject/Project/2002/checklists.csv", na.strings = c("?", "X", "x"))
checklist.two <- checklist.two %>% 
  filter(STATE_PROVINCE %in% c("Oklahoma", "Kansas", "New Mexico", "New_Mexico") & Anas_platyrhynchos > 0) %>%
  select(SAMPLING_EVENT_ID, LOC_ID, LATITUDE, LONGITUDE, YEAR, MONTH, DAY, TIME, COUNTRY, STATE_PROVINCE,
         COUNTY, COUNT_TYPE, EFFORT_HRS, EFFORT_DISTANCE_KM, EFFORT_AREA_HA, OBSERVER_ID, NUMBER_OBSERVERS,
         GROUP_ID, Anas_platyrhynchos) %>%
  transform(Anas_platyrhynchos = as.numeric(Anas_platyrhynchos)) 

checklist.three <- read.csv("D:/Documents/RStudio/MyProject/Project/2003/checklists.csv", na.strings = c("?", "X", "x"))
checklist.three <- checklist.three %>% 
  filter(STATE_PROVINCE %in% c("Oklahoma", "Kansas", "New Mexico", "New_Mexico") & Anas_platyrhynchos > 0) %>%
  select(SAMPLING_EVENT_ID, LOC_ID, LATITUDE, LONGITUDE, YEAR, MONTH, DAY, TIME, COUNTRY, STATE_PROVINCE,
         COUNTY, COUNT_TYPE, EFFORT_HRS, EFFORT_DISTANCE_KM, EFFORT_AREA_HA, OBSERVER_ID, NUMBER_OBSERVERS,
         GROUP_ID, Anas_platyrhynchos) %>%
  transform(Anas_platyrhynchos = as.numeric(Anas_platyrhynchos)) 

checklist.four <- read.csv("D:/Documents/RStudio/MyProject/Project/2004/checklists.csv", na.strings = c("?", "X", "x"))
checklist.four <- checklist.four %>% 
  filter(STATE_PROVINCE %in% c("Oklahoma", "Kansas", "New Mexico", "New_Mexico") & Anas_platyrhynchos > 0) %>%
  select(SAMPLING_EVENT_ID, LOC_ID, LATITUDE, LONGITUDE, YEAR, MONTH, DAY, TIME, COUNTRY, STATE_PROVINCE,
         COUNTY, COUNT_TYPE, EFFORT_HRS, EFFORT_DISTANCE_KM, EFFORT_AREA_HA, OBSERVER_ID, NUMBER_OBSERVERS,
         GROUP_ID, Anas_platyrhynchos) %>%
  transform(Anas_platyrhynchos = as.numeric(Anas_platyrhynchos)) 

checklist.five<- read.csv("D:/Documents/RStudio/MyProject/Project/2005/checklists.csv", na.strings = c("?", "X", "x"))
checklist.five <- checklist.five %>% 
  filter(STATE_PROVINCE %in% c("Oklahoma", "Kansas", "New Mexico", "New_Mexico") & Anas_platyrhynchos > 0) %>%
  select(SAMPLING_EVENT_ID, LOC_ID, LATITUDE, LONGITUDE, YEAR, MONTH, DAY, TIME, COUNTRY, STATE_PROVINCE,
         COUNTY, COUNT_TYPE, EFFORT_HRS, EFFORT_DISTANCE_KM, EFFORT_AREA_HA, OBSERVER_ID, NUMBER_OBSERVERS,
         GROUP_ID, Anas_platyrhynchos) %>%
  transform(Anas_platyrhynchos = as.numeric(Anas_platyrhynchos))

checklist.six <- read.csv("D:/Documents/RStudio/MyProject/Project/2006/checklists.csv", na.strings = c("?", "X", "x"))
checklist.six <- checklist.six %>% 
  filter(STATE_PROVINCE %in% c("Oklahoma", "Kansas", "New Mexico", "New_Mexico") & Anas_platyrhynchos > 0) %>%
  select(SAMPLING_EVENT_ID, LOC_ID, LATITUDE, LONGITUDE, YEAR, MONTH, DAY, TIME, COUNTRY, STATE_PROVINCE,
         COUNTY, COUNT_TYPE, EFFORT_HRS, EFFORT_DISTANCE_KM, EFFORT_AREA_HA, OBSERVER_ID, NUMBER_OBSERVERS,
         GROUP_ID, Anas_platyrhynchos) %>%
  transform(Anas_platyrhynchos = as.numeric(Anas_platyrhynchos))

checklist.seven <- read.csv("D:/Documents/RStudio/MyProject/Project/2007/checklists.csv", na.strings = c("?", "X", "x"))
checklist.seven <- checklist.seven %>% 
  filter(STATE_PROVINCE %in% c("Oklahoma", "Kansas", "New Mexico", "New_Mexico") & Anas_platyrhynchos > 0) %>%
  select(SAMPLING_EVENT_ID, LOC_ID, LATITUDE, LONGITUDE, YEAR, MONTH, DAY, TIME, COUNTRY, STATE_PROVINCE,
         COUNTY, COUNT_TYPE, EFFORT_HRS, EFFORT_DISTANCE_KM, EFFORT_AREA_HA, OBSERVER_ID, NUMBER_OBSERVERS,
         GROUP_ID, Anas_platyrhynchos) %>%
  transform(Anas_platyrhynchos = as.numeric(Anas_platyrhynchos))

checklist.eight <- read.csv("D:/Documents/RStudio/MyProject/Project/2008/checklists.csv", na.strings = c("?", "X", "x"))
checklist.eight <- checklist.eight %>% 
  filter(STATE_PROVINCE %in% c("Oklahoma", "Kansas", "New Mexico", "New_Mexico") & Anas_platyrhynchos > 0) %>%
  select(SAMPLING_EVENT_ID, LOC_ID, LATITUDE, LONGITUDE, YEAR, MONTH, DAY, TIME, COUNTRY, STATE_PROVINCE,
         COUNTY, COUNT_TYPE, EFFORT_HRS, EFFORT_DISTANCE_KM, EFFORT_AREA_HA, OBSERVER_ID, NUMBER_OBSERVERS,
         GROUP_ID, Anas_platyrhynchos) %>%
  transform(Anas_platyrhynchos = as.numeric(Anas_platyrhynchos))

checklist.nine <- read.csv("D:/Documents/RStudio/MyProject/Project/2009/checklists.csv", na.strings = c("?", "X", "x"))
checklist.nine <- checklist.nine %>% 
  filter(STATE_PROVINCE %in% c("Oklahoma", "Kansas", "New Mexico", "New_Mexico") & Anas_platyrhynchos > 0) %>%
  select(SAMPLING_EVENT_ID, LOC_ID, LATITUDE, LONGITUDE, YEAR, MONTH, DAY, TIME, COUNTRY, STATE_PROVINCE,
         COUNTY, COUNT_TYPE, EFFORT_HRS, EFFORT_DISTANCE_KM, EFFORT_AREA_HA, OBSERVER_ID, NUMBER_OBSERVERS,
         GROUP_ID, Anas_platyrhynchos) %>%
  transform(Anas_platyrhynchos = as.numeric(Anas_platyrhynchos)) 

checklist.ten <- read.csv("D:/Documents/RStudio/MyProject/Project/2010/checklists.csv", na.strings = c("?", "X", "x"))
checklist.ten <- checklist.ten %>% 
  filter(STATE_PROVINCE %in% c("Oklahoma", "Kansas", "New Mexico", "New_Mexico") & Anas_platyrhynchos > 0) %>%
  select(SAMPLING_EVENT_ID, LOC_ID, LATITUDE, LONGITUDE, YEAR, MONTH, DAY, TIME, COUNTRY, STATE_PROVINCE,
         COUNTY, COUNT_TYPE, EFFORT_HRS, EFFORT_DISTANCE_KM, EFFORT_AREA_HA, OBSERVER_ID, NUMBER_OBSERVERS,
         GROUP_ID, Anas_platyrhynchos) %>%
  transform(Anas_platyrhynchos = as.numeric(Anas_platyrhynchos))

checklist.eleven <- read.csv("D:/Documents/RStudio/MyProject/Project/2011/checklists.csv", na.strings = c("?", "X", "x"))
checklist.eleven <- checklist.eleven %>% 
  filter(STATE_PROVINCE %in% c("Oklahoma", "Kansas", "New Mexico", "New_Mexico") & Anas_platyrhynchos > 0) %>%
  select(SAMPLING_EVENT_ID, LOC_ID, LATITUDE, LONGITUDE, YEAR, MONTH, DAY, TIME, COUNTRY, STATE_PROVINCE,
         COUNTY, COUNT_TYPE, EFFORT_HRS, EFFORT_DISTANCE_KM, EFFORT_AREA_HA, OBSERVER_ID, NUMBER_OBSERVERS,
         GROUP_ID, Anas_platyrhynchos) %>%
  transform(Anas_platyrhynchos = as.numeric(Anas_platyrhynchos))

checklist.prehistoric <- read.csv("D:/Documents/RStudio/MyProject/Project/pre2002/checklists.csv", na.strings = c("?", "X", "x"))
checklist.prehistoric <- checklist.prehistoric %>% 
  filter(STATE_PROVINCE %in% c("Oklahoma", "Kansas", "New Mexico", "New_Mexico") & Anas_platyrhynchos > 0) %>%
  select(SAMPLING_EVENT_ID, LOC_ID, LATITUDE, LONGITUDE, YEAR, MONTH, DAY, TIME, COUNTRY, STATE_PROVINCE,
         COUNTY, COUNT_TYPE, EFFORT_HRS, EFFORT_DISTANCE_KM, EFFORT_AREA_HA, OBSERVER_ID, NUMBER_OBSERVERS,
         GROUP_ID, Anas_platyrhynchos) %>%
  transform(Anas_platyrhynchos = as.numeric(Anas_platyrhynchos))

duck.frame <- rbind.data.frame(checklist.prehistoric, checklist.two, checklist.three, checklist.four, checklist.five, checklist.six, checklist.seven,
                               checklist.eight, checklist.nine, checklist.ten, checklist.eleven)

write.csv(duck.frame, file = "duck.frame.csv")
help("memory.size")

memory.size(max = F)
memory.limit(size = 25000)
.ls.objects()

duck.frame <- read.csv("duck.frame.csv", stringsAsFactors = F, as.is = T)


climate.frame <- duck.frame %>%
  distinct(LOC_ID, .keep_all = T) %>%
  select(LOC_ID, COUNTY, LATITUDE, LONGITUDE)

write.csv(climate.frame, file = "climateframe.csv")
climate.frame <- read.csv("climateframe.csv")
cframe <- read.csv("climateframe_1967-2011YT.csv", stringsAsFactors = F, as.is = T)

duck.final <- left_join(duck.frame, cframe, by = c("YEAR" = "Year", "LOC_ID" = "ID1"))
write.csv(duck.final, file = "duck.final.csv")
duck.final <- read.csv("duck.final.csv")
duck.final1 <- duck.final %>%
  select(STATE_PROVINCE, COUNTY, YEAR, LATITUDE, LONGITUDE, EFFORT_HRS, Anas_platyrhynchos, MAT:CMD, RH) %>%
  filter(EFFORT_HRS > 0) %>%
  mutate(count = ceiling(Anas_platyrhynchos/EFFORT_HRS))

plot(duck.final1$count)

write.csv(duck.final1, file = "duck.final1.csv")
duck.final1 <- read.csv("duck.final1.csv")
duck.final1[duck.final1==-9999.0] <- NA

duck.2011 <- duck.final1 %>%
  group_by(STATE_PROVINCE , COUNTY) %>%
  filter(YEAR == 2011) %>%
  summarise_all(mean)

duck.2011$STATE_PROVINCE <- as.character(duck.2011$STATE_PROVINCE)
duck.2011[duck.2011 == "New_Mexico"] <- "New Mexico"
duck.2011$COUNTY <- as.character(duck.2011$COUNTY)
duck.2011$COUNTY <-  str_replace_all(duck.2011$COUNTY, "_", " ")

duck.omit <- na.omit(duck.final1)
duck.count <- data.frame(duck.omit[,31])
duck.env <- duck.omit[,9:30]
Before1  <- data.frame(cor2m(duck.count, duck.env))

duck.res <- stdres(duck.model)
duck.cook <- cooks.distance(duck.model)
duck.cook1 <- cbind(duck.omit, duck.cook)
duck.outlier <- duck.cook1[duck.cook > 4/12060,]

duck.nooutlier <- anti_join(duck.omit, duck.outlier)
  
big.duck <- formula(glm(count ~ MAT + MWMT + MCMT + TD + MAP + MSP + AHM + SHM + DD_0 + DD5 + DD_18 + 
                        DD18 + NFFD + bFFP + eFFP + FFP + PAS + EMT + EXT + Eref + CMD + MAR + RH,
                        data = duck.omit, family = "poisson"))

duck.start <- glm(count ~ 1, data = duck.omit, family = "poisson")
duck.scope <- list(lower = formula(duck.start), upper = big.duck)
small.duck <- glm(count ~ TD, data = duck.omit, family = "poisson")
duck.step <-step(small.duck, scope = duck.scope, direction = "both")

warnings()

summary(small.duck)

duck.model <- glm(count ~ TD + EXT + RH + MCMT + CMD + DD_18 + eFFP + EMT + PAS + 
                    NFFD + MAT + DD18 + SHM + MAP + MSP + DD_0 + MWMT + 
                    Eref + FFP + bFFP + DD5 + AHM, data = duck.omit, family = "poisson")

duck.bad <- glm(Anas_platyrhynchos ~ TD + EXT + RH + MCMT + CMD + DD_18 + eFFP + EMT + PAS + 
                    NFFD + MAT + DD18 + SHM + MAR + MAP + MSP + DD_0 + MWMT + 
                    Eref + FFP + bFFP + DD5 + AHM, data = duck.final1, family = "poisson")

plot(duck.model)

##robust glm
rob.big.duck <- formula(glmrob(count ~ MAT + MWMT + MCMT + TD + MAP + MSP + AHM + SHM + DD_0 + DD5 + DD_18 + 
                          DD18 + NFFD + bFFP + eFFP + FFP + PAS + EMT + EXT + Eref + CMD + MAR + RH,
                        data = duck.omit, family = "poisson"))
logLik(rob.duck.model)
rob.duck.start <- glmrob(count ~ 1, data = duck.final1, family = "poisson")
rob.duck.scope <- list(lower = formula(rob.duck.start), upper = rob.big.duck)
rob.small.duck <- glmrob(count ~ TD, data = duck.final1, family = "poisson")
rob.duck.step <-step(rob.small.duck, scope = rob.duck.scope, direction = "both")

rob.duck.model <- glmrob(count ~ TD + SHM + RH + EXT + DD18 + CMD + Eref + EMT + MWMT + 
                    MCMT + AHM + bFFP + NFFD + DD5 + MAT + DD_18 + MAR + MAP + 
                    FFP + DD_0 + eFFP + PAS + MSP, data = duck.nooutlier, family = "poisson")

summary(rob.duck.model)
##outliers excluded

no.big.duck <- formula(glm(count ~ MAT + MWMT + MCMT + TD + MAP + MSP + AHM + SHM + DD_0 + DD5 + DD_18 + 
                          DD18 + NFFD + bFFP + eFFP + FFP + PAS + EMT + EXT + Eref + CMD +  RH,
                        data = duck.nooutlier, family = "poisson"))

no.duck.start <- glm(count ~ 1, data = duck.nooutlier, family = "poisson")
no.duck.scope <- list(lower = formula(no.duck.start), upper = no.big.duck)
no.small.duck <- glm(count ~ TD, data = duck.nooutlier, family = "poisson")
no.duck.step <-step(no.small.duck, scope = no.duck.scope, direction = "both")

no.duck.model <- glm(count ~ TD + EXT + RH + MCMT + eFFP + MAP + EMT + DD_18 + 
                       PAS + SHM + CMD + NFFD + MAT + DD18 + MWMT + MSP + FFP + 
                       bFFP + AHM + Eref + DD_0, data = duck.nooutlier, family = "poisson")


cor.big.duck <- formula(glm(count ~ MCMT + TD + MAP + MSP + AHM + SHM + DD_0 + DD5 + 
                           NFFD + EXT + Eref + CMD + MAR + RH,
                        data = duck.nooutlier, family = "poisson"))

cor.duck.start <- glm(count ~ 1, data = duck.nooutlier, family = "poisson")
cor.duck.scope <- list(lower = formula(cor.duck.start), upper = cor.big.duck)
cor.small.duck <- glm(count ~ TD, data = duck.nooutlier, family = "poisson")
duck.step <-step(cor.small.duck, scope = cor.duck.scope, direction = "both")

summary(no.duck.model)


summary(rob.duck.model)
duck.counta <- data.frame(duck.nooutlier[,31])
duck.enva <- duck.nooutlier[,9:30]
After <- data.frame(cor2m(duck.counta, duck.enva))

Correlation <- data.frame(Before, After)
colnames(Correlation) <- c("Before", "After")

duck.res <- stdres(duck.model)
duck.cook1 <- cbind(duck.omit, duck.cook, duck.res)
duck.outlier <- duck.cook1[duck.cook > 4/12060,]

duck.resid <- resid(duck.model)
duck.res <- resid(duck.model)
duck.lev <- hatvalues(duck.model)
sigma.hat <- summary(duck.model)$sigma
duck.stu.res <- queen.res/(sigma.hat*sqrt(1-duck.lev))
duck.cook <- queen.stu.res^2*(duck.lev/(1-duck.lev))

duck.cook <- cooks.distance(duck.model)

ggplot(duck.model)
colnames(duck.final)
intervals <- exp(confint(duck.model))
intervals


##standardize latlong to counties

counties <- duck.final1 %>%
  group_by(STATE_PROVINCE, COUNTY) %>%
  summarise(Latitude = mean(LATITUDE), Longitude = mean(LONGITUDE)) 

write.csv(counties, "counties.csv")

#read in county projections

county2025 <- read.csv("counties_GFDL-CM3_rcp45_2025Y.csv")
county2055 <- read.csv("counties_GFDL-CM3_rcp45_2055Y.csv")
county2085 <- read.csv("counties_GFDL-CM3_rcp45_2085Y.csv")

county2025 <- county2025 %>%
  select(ID2, TD, DD5, DD_0, EMT, MSP)

county2055 <- county2025 %>%
  select(ID2, TD, DD5, DD_0, EMT, MSP)

county2085 <- county2025 %>%
  select(ID2, TD, DD5, DD_0, EMT, MSP)


pre2011 <- predict.glm(no.duck.model, newdata = duck.2011)

RMD

county2025$ID1 <- as.character(county2025$ID1)
intervals2 <- exp(confint(no.duck.model))
pretest <- data.frame(predict.glm(no.duck.model, duck.counties1[1,]))
pre2011 <- data.frame(predict.glm(no.duck.model, newdata = duck.2011))
pre2011$pre2011 <- log(pre2011$pre2011)
prediction2025 <- data.frame(predict.glm(no.duck.model, newdata = county2025))
prediction2055 <- data.frame(predict.glm(no.duck.model, newdata = county2055))
prediction2085 <- data.frame(predict.glm(no.duck.model, newdata = county2085))
allpredictions <- cbind(prediction2025, prediction2055, prediction2085)
eallpredictions <- exp(allpredictions)
colnames(eallpredictions) <- c("p2025", "p2055", "p2085")
county.predictions <- cbind(county2025, eallpredictions)
county.predictions$ID1 <- as.character(county.predictions$ID1)
county.predictions[county.predictions == "New_Mexico"] <- "New Mexico"


county.predictions <- county.predictions %>%
  select(ID1, ID2, p2025, p2055, p2085)
county.predictions$ID2 <- as.character(county.predictions$ID2)
county.predictions$ID2 <- str_replace_all(county.predictions$ID2, "_", " ")
pre2011 <- predict.glm(no.duck.model, newdata = duck.2011)
pre2011 <- exp(pre2011)
pre2011 <- cbind(duck.2011, pre2011)
duck.2011$pre <- pre2011
duck.2011 <- duck.2011 %>%
  select(STATE_PROVINCE, COUNTY, pre)

final.predictions <- inner_join(county.predictions, duck.2011, by = c("ID1" = "STATE_PROVINCE", "ID2" = "COUNTY"))
final.predictions[,3:5] <- final.predictions[, 3:5]/final.predictions[,6]
final.predictions$ID2 <- paste(final.predictions$ID2, " County", sep = "")
final.predictions <- na.omit(final.predictions)
write.csv(final.predictions, file = "finalpredictions.csv")
