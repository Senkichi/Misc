
library(dplyr)
library(tidyr)

classes <- c("character", "numeric", "numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric")
invert <- read.csv("Invertebrate ID.csv", header = T,  colClasses = classes)
  

invert <- invert %>%
    mutate(Vegetation = rowSums(.[14:16], na.rm = T), Pool = rowSums(.[17:19], na.rm = T), Riffle = rowSums(.[20:22], na.rm = T))

invert <- t(invert)
  invert <- data.frame(invert)

type.vector <- c("Vegetation", "Vegetation", "Vegetation", "Pool","Pool","Pool", "Riffle", "Riffle", "Riffle")
  
invert2 <- invert[c(1, 14:22),]
  colnames(invert2) <- as.character(unlist(invert2[1,]))
    invert2 <- invert2[-1,]
      invert2$Type <- type.vector

invert2 <- invert2 %>%
  add_rownames(var = "Site") %>%
  gather(Family, Count, Aeshnidae:Corydalidae) %>%
  arrange(Site)

invert2 <- transform(invert2, Count = as.numeric(Count))

ept.list <- c("Ephemeridae","Polymitarcyidae", "Isonychiidae", "Oligoneuriidae", "Leptohyphidae", "Caenidae", 
              "Ephemerellidae", "Heptageniidae", "Leptophlebiidae", "Ametropodidae", "Baetidae", "Ameletidae", 
              "Siphlonuridae", "Pteronarcyidae", "Taeniopterygidae", "Nemouridae", "Capniidae", "Leuctridae", 
              "Perlidae", "Chloroperlidae", "Perlodidae", "Helicopsychidae", "Hydroptilidae", "Hydropsychidae", 
              "Leptoceridae", "Glossosomatidae", "Rhyacophlidae", "Philopotamidae", "Polycentropodidae", 
              "Psychomyiidae", "Brachycentridae", "Uenoidae", "Limnephilidae", "Apataniidae")

ept.counts <- invert2 %>%
  filter(Family %in% ept.list) %>%
  group_by(Site) %>%
  summarise(EPT = sum(Count, na.rm = T))
  
ept.counts$Type <- type.vector  

ept.model <- lm(EPT ~ Type, data = ept.counts)
summary(ept.model)

site.richness <- invert2 %>%
  filter(Count > 0) %>%
  group_by(Site) %>%
  summarise(Richness = length(Family))

site.richness$Type <- type.vector

richness.model <- lm(Richness ~ Type, data = site.richness)
summary(richness.model)
