

library(dplyr)
library(tidyr)


sk_data <- read.csv("C:/Users/sharo/OneDrive/Sharon Kay/Masters/Analysis/R scripts/Manuscript 2/For submission to journal/data_maternalcosts_reproduction.csv")


females <- sk_data

females$pregnancy <- factor(females$pregnancy, levels = c("Not pregnant",
                                                            "Pregnant - early stages",
                                                            "Pregnant",
                                                            "Nursing 1",
                                                            "Nursing 2",
                                                            "Nursing 3"))


pre_meno <- females %>% 
  filter(menopause == "pre")

pre_meno <- pre_meno %>% 
  filter(remove.visual.pregnancy != "y")

pre_meno$year <- as.factor(pre_meno$year)


##### glmm

library(glmmTMB)
library(splines)

m.1 <- glmmTMB(mean.ep ~ ns(age, df = 2) + pregnancy  
                + (1 | whale.id) + (1| matriline) + ar1(year + 0 | whale.id), data = pre_meno)

m.1
summary(m.1)

