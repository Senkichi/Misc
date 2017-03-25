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
  owls2 <- owls2 %>%
    group_by(year) %>%
    summarise(counts = sum(counts), Y1 = (mean(year) - 1967))

owl.model1 <- lm(counts ~ Y1, data = owls2)
summary(owl.model1)


lambda_owl <- numeric()
lambda_owl <- owls2$counts[-1]/owls2$counts[-169]

mu.owl <- sum(log(lambda_owl))/168

lamb.g <-exp(mu.owl)

xstar <- sqrt(Bears$Year[-1]-Bears$Year[-45])
xstar
ystar <- (log(lambda_bear)/xstar)
ystar  

model <- lm(ystar~xstar +0, data=Bears)

plot(model)
summary(model)

qt(0.975, 43)

0.0224+2.02*0.0169
0.0224-2.02*0.0169

0.112^2#sigma squared
qchisq(0.975, 43)
qchisq(0.025, 43)
43*0.0125
0.5375/62.99
0.5375/26.78

