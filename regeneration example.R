
# Forest regeneration at forest edges

# uses data from FERN network
# can be downloaded here: https://borealisdata.ca/dataset.xhtml?persistentId=doi:10.5683/SP3/YO7LE9



# package -----------------------------------------------------------------

library(readr)
library(tidyr)
library(dplyr)
library(sjPlot)
library(ggplot2)
library(ggeffects)
library(mgcv)
library(DHARMa)


# Load and prepare data ---------------------------------------------------

Density <- read_csv("Density.csv")
Transects <- read_csv("Transects.csv")
Sites = read_csv("Sites.csv")

# limit to forest interior
Density = Density[Density$DISTANCE_M >= 0, ]

# clean columns
Density %>% 
  select(-8, -9) -> Density


# zero counts are missing in the database
# must be expanded
table(Density$TRANSECT_ID, Density$DISTANCE_M)
temp = Density[Density$TRANSECT_ID == "ABCut16A", ]
table(temp$DISTANCE_M, temp$SPECIES)
rm(temp)

Density %>% 
  group_by(TRANSECT_ID) %>% 
  tidyr::expand(DISTANCE_M, SUBPLOT, SPECIES, SIZE_CLASS) %>% 
  left_join(y = Density) %>% 
  mutate(COUNT = ifelse(is.na(COUNT), 0, COUNT),
         UID = TRANSECT_ID) %>% 
  separate(TRANSECT_ID, 
           into = c("STUDY", "TRANSECT"), 
           sep = "(?<=[A-Za-z])(?=[0-9])") -> Density


# Total density -----------------------------------------------------------

# calculate total density
Density %>% 
  group_by(STUDY, TRANSECT, DISTANCE_M, SUBPLOT, SIZE_CLASS, UID) %>% 
  summarise(regeneration = sum(COUNT)) %>% 
  ungroup() -> Density



# Select data -------------------------------------------------------------


# only studies with plots of 2x2m
sel = c("ABCut", "ABFire", "ABLake", "BERip", "BRAtlCut", "ONFire", "QUCut", "QUCutSpruce", "QUFire")

# only studies with plots of 2x2m next to cuts
sel = c("ABCut", "BRAtlCut", "QUCut", "QUCutSpruce")

# only four size classes
sel_size = c("A", "B", "C", "D")

# reduced dataset
dat = Density[Density$STUDY %in% sel & 
               Density$SIZE_CLASS %in% sel_size, ]

dat$SIZE_CLASS = plyr::revalue(dat$SIZE_CLASS, c("A" = "< 1 yr old", 
                                                 "B" = "≥ 1 yr old and < 1 m height",
                                                 "C" = "> 1 m height and < 1 cm dbh",
                                                 "D" = "between 1 and 5 cm dbh"))

# convert to factor
dat %>% mutate_if(is.character, as.factor) -> dat



# Model regeneration along transects --------------------------------------

# flexible model with splines for illustration
m_gam = gam(regeneration ~ s(DISTANCE_M, by = SIZE_CLASS, k = 5) 
            # + s(TRANSECT, bs = "re")
            + s(UID, SUBPLOT, bs = "re")
            , data = dat, family = "nb")

# check assumptions
res = simulateResiduals(m_gam)
plot(res)
testZeroInflation(res)
# okayish

# analyze model
summary(m_gam)

# plot model
pred = predict_response(m_gam, c("DISTANCE_M [n=100]", "SIZE_CLASS"))
plot(pred, grid = T, use_theme = T, show_title = F)





