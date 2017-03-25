species.code <- 3490
sdfsfs
setwd("~/GitHub/Misc")
library(dplyr)
##Sam's paths
NM <- read.csv("D:/Programs/GitHub/Misc/NMexico/NMexico.csv")
UT <- read.csv("D:/Programs/GitHub/Misc/Utah/Utah.csv")
CO <- read.csv("D:/Programs/GitHub/Misc/Colorad/Colorad.csv")
AZ <- read.csv("D:/Programs/GitHub/Misc/Arizona/Arizona.csv")

##Gillian's paths
NM <- read.csv("C:/Users/Gillian/Documents/GitHub/Misc/NMexico/NMexico.csv")
UT <- read.csv("C:/Users/Gillian/Documents/GitHub/Misc/Utah/Utah.csv")
CO <- read.csv("C:/Users/Gillian/Documents/GitHub/Misc/Colorad/Colorad.csv")
AZ <- read.csv("C:/Users/Gillian/Documents/GitHub/Misc/Arizona/Arizona.csv")

NM2 <- NM %>%
  group_by(statenum, Year) %>%
  filter(Aou %in% species.code) %>%
  summarise(counts = sum(SpeciesTotal), year = mean(Year)) %>%
  mutate(state = "NM")
  
UT2 <- UT %>%
  group_by(statenum, Year) %>%
  filter(Aou %in% species.code) %>%
  summarise(counts = sum(SpeciesTotal), year = mean(Year)) %>%
  mutate(state = "UT")

CO2 <- CO %>%
  group_by(statenum, Year) %>%
  filter(Aou %in% species.code) %>%
  summarise(counts = sum(SpeciesTotal), year = mean(Year)) %>%
  mutate(state = "CO")

AZ2 <- AZ %>%
  group_by(statenum, Year) %>%
  filter(Aou %in% species.code) %>%
  summarise(counts = sum(SpeciesTotal), year = mean(Year)) %>%
  mutate(state = "AZ")

owls <- rbind.data.frame(AZ2, CO2, NM2, UT2)
owls2 <- owls %>%
  group_by(year) %>%
  summarise(counts = sum(counts), Y1 = (mean(year) - 1967))

write.csv(owls, file = "owls2.csv")
owls2 <- read.csv("owls2.csv")

owl.model1 <- lm(counts ~ Y1, data = owls2)
summary(owl.model1)
