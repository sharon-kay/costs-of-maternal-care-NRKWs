
#### Load libraries 

# use ?library_name to see library usage

?install.packages("optimx")


# data organization and manipulation
library(plyr)
library(dplyr)
library(tidyr)

# modelling, diagnostics, and interpretation

library(AICcmodavg)
library(lmerTest) 
library(fitdistrplus)
library(nlme) # for correlation ar1 function in model
library(glmmTMB) # for making GLMMs - can use guassian distribution
library(lme4) # for making LMMs
library(multcomp) # for Bonferoni comparison
library(emmeans) # for interpreting GLMMS - psot hoc testing
library(car) # for VIF
library(moonBook)
library(merDeriv)
library(MuMIn) # for making marginal R squared value based on fixed effects in model
library(performance) # for making conditional R squared value based on fixed effects and random effects in model
library(sjstats) # for making ICC to explain the proportion of variance explained by random effects in model
library(DHARMa) # model diagnostics
library(tmbstan) # for splines interaction terms with glmmTMB
library(splines) # for splines non linear relationships in glmmTMB
library(optimx) # for optimizers in glmmTMB


# visualization

library(ggplot2)
library(ggpubr)
library(patchwork) # for stitching plots together
library(ggiraph)
library(ggiraphExtra)
library(dotwhisker) # for coefficient plot
library(sjPlot) # for coefficient plot - need to install xfun then sjPlot will work
library(ggeffects) # plotting gg predict
library(Hmisc) # for correlation plot
library(corrplot) # for correlation plot
library(classInt) # for using Jenks to find natural breaks in variables
library(viridis) # palette for ggplot
library(RColorBrewer) # pallette for ggplot
library(stringr) # to format axis labels on ggplot





############# LOAD DATA ###################################################################################################

# data file comes from first 
# 1) running annual matriline characteristics script
# 2) then running combining all data sets script
# 3) running Making csv for modelling R script - this is the script that gives me the below csv.



# load csv file made in EP data 2014 to 2023 analysis R script - with correct matriline belonging and all social variables as proportions etc


data2014_2023 <- read.csv("C:/Users/sharo/OneDrive/Sharon Kay/Masters/Data/Manuscript 2/data2014_2023_maternalcosts.csv")




###### Data Exploration ##########################################################################


### make variables their correct class! 

# Numeric (whole and decimal #s), Integer (whole #s), Character (words), Logical (yes/no), Factor (categorical with levels)

str(data2014_2023)

# change character variables to factors (if have levels)


data2014_2023$has.son <- as.factor(data2014_2023$has.son)
data2014_2023$has.adultson <- as.factor(data2014_2023$has.adultson)
data2014_2023$mother.status <- as.factor(data2014_2023$mother.status)
data2014_2023$pregnancy.simple <- as.factor(data2014_2023$pregnancy.simple)
data2014_2023$year <- as.factor(data2014_2023$year)


# check variable structure

print(table(data2014_2023$pregnancy.simple))
str(data2014_2023$pregnancy.simple)

print(table(data2014_2023$age))
str(data2014_2023$age)
hist(data2014_2023$age)

print(table(data2014_2023$sex))
str(data2014_2023$sex)

print(table(data2014_2023$age.sex.class))
str(data2014_2023$age.sex.class)

print(table(data2014_2023$salmon))
str(data2014_2023$salmon)

print(table(data2014_2023$num.offspring)) 
str(data2014_2023$num.offspring)
hist(data2014_2023$num.offspring)

print(table(data2014_2023$num.sons)) 
str(data2014_2023$num.sons)
hist(data2014_2023$num.sons)

print(table(data2014_2023$num.daughters)) 
str(data2014_2023$num.daughters)
hist(data2014_2023$num.daughters)


print(table(data2014_2023$year)) 
str(data2014_2023$year)



# number of observations

nrow(data2014_2023)

# number of whales

n_distinct(data2014_2023$whale.id)

# number of matrilines

n_distinct(data2014_2023$matriline)

# Number of unique whales sampled by year

# Step 1: Group by year and count the number of unique whale.id's for each year
unique_whales_per_year <- data2014_2023 %>%
  group_by(year) %>%
  summarise(unique_whales = n_distinct(whale.id))

# Step 2: Calculate the average number of unique whale.id's per year
average_unique_whales_per_year <- mean(unique_whales_per_year$unique_whales)

# Print the results
print(unique_whales_per_year)
print(average_unique_whales_per_year)


# how many whales have multi year sampling? 

# Step 1: Group by whale.id and count the number of unique years for each whale.id
whale_year_counts <- data2014_2023 %>%
  group_by(whale.id) %>%
  summarise(unique_years = n_distinct(year))

# Step 2: Filter to retain only whale.id's that appear in more than one unique year
multi_year_whales <- whale_year_counts %>%
  filter(unique_years > 1)

# Step 3: Count the number of such whale.id's
number_of_multi_year_whales <- n_distinct(multi_year_whales$whale.id)





# Mean EP ratio

summary_stats <- data2014_2023 %>%
  summarise(
    mean_value = mean(mean.ep),
    sem_value = sd(mean.ep) / sqrt(length(mean.ep)),
    min_value = min(mean.ep),
    max_value = max(mean.ep)
  )

# count number of observations for each age sex class

table(data2014_2023$age.sex.class)

# get the mean EP ratio value for all age sex classes


summary_stats_agesexclass <- data2014_2023 %>%
  group_by(age.sex.class) %>%
  summarise(
    mean_meaep = mean(mean.ep),
    sem_mean.ep = sd(mean.ep) / sqrt(n())
  )



## how many photos do I have per indiviudal on average?

n.ep_stats <- data2014_2023 %>%
  summarise(
    mean_value = mean(n.ep),
    sem_value = sd(n.ep) / sqrt(length(n.ep)),
    min_value = min(n.ep),
    max_value = max(n.ep))


### how many individuals are pregnant or nursing in the all whales data set?




print(table(data2014_2023$pregnancy))

print(table(data2014_2023$pregnancy.simple))


pregnancy_counts <- table(data2014_2023$pregnancy)


barplot(pregnancy_counts, 
        xlab = "Pregnancy Status",
        ylab = "Count",
        col = "grey", # Set the color of the bars
        main = "",
        ylim = c(0, 400)) # Set Y-axis limits

# Add counts on top of the bars
text(x = barplot(pregnancy_counts, col = "grey", add = TRUE) - 0.0, 
     y = pregnancy_counts + 0.1,
     labels = pregnancy_counts,
     pos = 3,
     cex = 1.0, # Adjust the size of the text
     col = "black") # Set the color of the text

notpregnant <- data2014_2023 %>% filter(pregnancy == "Not pregnant") # find the number whales
n_distinct(notpregnant$whale.id)

nursing1 <- data2014_2023 %>% filter(pregnancy == "Nursing 1") # find the number whales
n_distinct(nursing1$whale.id)

nursing2 <- data2014_2023 %>% filter(pregnancy == "Nursing 2") # find the number whales
n_distinct(nursing2$whale.id)

nursing3 <- data2014_2023 %>% filter(pregnancy == "Nursing 3") # find the number whales
n_distinct(nursing3$whale.id)

pregnantcalf <- data2014_2023 %>% filter(pregnancy == "Pregnant - calf") # find the number whales
n_distinct(pregnantcalf$whale.id)

pregnantearly <- data2014_2023 %>% filter(pregnancy == "Pregnant - early stages") # find the number whales
n_distinct(pregnantearly$whale.id)

pregnantvisual <- data2014_2023 %>% filter(pregnancy == "Pregnant - visual") # find the number whales
n_distinct(pregnantvisual$whale.id)



# look at variation in salmon
data2014_2023 %>%
  group_by(year) %>% 
  summarise(salmon = unique(salmon)) %>% 
  ungroup()


ggplot(data = data2014_2023, aes(x = year, y = salmon)) +
  geom_point()  + geom_line() +
  labs(title = "",
       x = "Year",
       y = "Chinook Salmon Abundance Index") +
  theme_classic() 
# wont get trend line if use year as factor. 

# make it numeric for just this graph: 
ggplot(data = data2014_2023, aes(x = as.numeric(levels(year)[year]), y = salmon)) +
  geom_point(size = 3, colour = "navy")  + geom_line(linewidth = 1.25, colour = "navy") +
  labs(title = "",
       x = "Year",
       y = "Chinook Salmon Abundance Index") +
  scale_x_continuous(breaks = seq(min(as.numeric(levels(data2014_2023$year))), 
                                  max(as.numeric(levels(data2014_2023$year))), 
                                  by = 1)) +
  scale_y_continuous(limits = c(0.5, 1.75)) +
  theme_classic(base_size = 18)







##### MODEL FIT AND TRYING TRANSFORMATIONS #############################################################################################


## model fit 

descdist(data2014_2023$mean.ep, discrete=FALSE)#check if normal distribution, on continuous eye patch data

# This data is VERY NORMAL

descdist(data2014_2023$mean.ep, discrete=FALSE, boot=1000) # check bootstrap spread



fit.normal <- fitdist(data2014_2023$mean.ep, distr = "norm", method = "mle")
fit.normal
plot(fit.normal)
summary(data2014_2023$mean.ep)


# maybe try a gamma distribution since my distribution is so close to 0 values


fit.gamma <- fitdist(data2014_2023$mean.ep, distr = "gamma", method = "mle") #maximum likelihood estimate
fit.gamma
plot(fit.gamma)
summary(data2014_2023$mean.ep)

#looks like they visual are almost identical.
# go with normal




########################## STANDARDIZE and SCALE VARIABLES ##################################################

# we may want to standarize the response variable and not the predictors
# In our predictors many of them are catgorical and so not all of them are standardized anyways
# IF we scale our response variable, we will get results that may be more interpretable
# we can get an average EP value of 0 and then all values below average EP are negative and all values above are positive

data2014_2023_ST <- data2014_2023 %>% 
  mutate_at(c("mean.ep"), ~(scale(.) %>% as.vector))

mean(data2014_2023_ST$mean.ep)
sd(data2014_2023_ST$mean.ep)
min(data2014_2023_ST$mean.ep)
max(data2014_2023_ST$mean.ep)

# Standardize predictor variables as option as well


data2014_2023_STpred <- data2014_2023 %>% mutate_at(c('num.births',
                                                      'salmon',
                                                      'num.offspring',
                                                      'num.sons',
                                                      'num.daughters',
                                                      'age'),
                                                    ~(scale(.) %>% as.vector))

mean(data2014_2023_STpred$num.births)
sd(data2014_2023_STpred$num.births)
mean(data2014_2023_STpred$age)
sd(data2014_2023_STpred$age)


######################################################################################################

############ BUILDING THE MODEL SET Linear Mixed Effects Models ###############################################################################

#######################################################################################################

# added in RMEL = FALSE to avoid errors with candidate model select aictab function for fixed effects models
# add in RMEL = TRUE for finding which random effects are in top model

  
### Subset only adult females  


# first look at sample size with all sexually mature females (10 years old and up)
data2014_2023_maturefemales <- data2014_2023 %>% filter(age > 9)
data2014_2023_maturefemales <- data2014_2023_maturefemales %>% filter(sex == "f")
n_distinct(data2014_2023_maturefemales$whale.id)

# and at sample size with all reproductive aged females (10 years old to 43 years old)
data2014_2023_reproductivefemales <- data2014_2023 %>% filter(age > 9 & age < 43)
data2014_2023_reproductivefemales <- data2014_2023_reproductivefemales %>% filter(sex == "f")
n_distinct(data2014_2023_reproductivefemales$whale.id)


# summary stats:

# how many photos do I have per indiviudal on average?

n.ep_stats <- data2014_2023_maturefemales %>%
  summarise(
    mean_value = mean(n.ep),
    sem_value = sd(n.ep) / sqrt(length(n.ep)),
    min_value = min(n.ep),
    max_value = max(n.ep),
    median_value = median(n.ep))

# Now subset only reproductive aged females (12 years old and up)
  data2014_2023_ST_adultfemales <- data2014_2023_ST %>% filter(age.sex.class %in% c("reproductive female", "post reproductive female"))
  data2014_2023_adultfemales <- data2014_2023 %>% filter(age.sex.class %in% c("reproductive female", "post reproductive female"))
  data2014_2023_STpred_adultfemales <- data2014_2023_STpred %>% filter(age.sex.class %in% c("reproductive female", "post reproductive female"))
  
  # check how many observations I have
  nrow(data2014_2023_adultfemales)
  n_distinct(data2014_2023_adultfemales$whale.id)
  
  min(data2014_2023_adultfemales$mean.ep)
  max(data2014_2023_adultfemales$mean.ep)
  
  
  # check if I only have female with known sex?
  
  (data2014_2023_ST_adultfemales$sex)
  
  
  
  ### Check distibution with adult female only data
  ## model fit 
  
  descdist(data2014_2023_adultfemales$mean.ep, discrete=FALSE)#check if normal distribution, on continuous eye patch data
  
  descdist(data2014_2023_adultfemales$mean.ep, discrete=FALSE, boot=1000) # check bootstrap spread
  
  
  
  fit.normal <- fitdist(data2014_2023_adultfemales$mean.ep, distr = "norm", method = "mle")
  fit.normal
  plot(fit.normal)
  summary(data2014_2023$mean.ep)
  
  
  
  
  # how many whales sampled multiple years?
  
  #make sure year is numeric
  data2014_2023_adultfemales$year <- as.numeric(as.character(data2014_2023_adultfemales$year))
  
  id_year_counts <- aggregate(year ~ whale.id, data = data2014_2023_adultfemales, FUN = function(x) length(unique(x)))
  
  sum(id_year_counts$year > 1)
  
  # how many whales sampled year year on average?
  
  unique_ID_year <- aggregate(whale.id ~ year, data = data2014_2023_adultfemales, FUN = function(x) length(unique(x)))
 
  mean(unique_ID_year$whale.id)
  
  #Now do the same but for all mature whales (10 years old and up)
  
  # how many whales sampled multiple years?
  
  #make sure year is numeric
  data2014_2023_maturefemales$year <- as.numeric(as.character(data2014_2023_maturefemales$year))
  
  id_year_counts <- aggregate(year ~ whale.id, data = data2014_2023_maturefemales, FUN = function(x) length(unique(x)))
  
  sum(id_year_counts$year > 1)
  
  # how many whales sampled year year on average?
  
  unique_ID_year <- aggregate(whale.id ~ year, data = data2014_2023_maturefemales, FUN = function(x) length(unique(x)))
  
  mean(unique_ID_year$whale.id)
  
  

  nrow(data2014_2023_adultfemales)
  
  # look at what pregnancy.simple is for adult female data set
  print(table(data2014_2023_adultfemales$pregnancy.simple))
  pregnancy_counts <- table(data2014_2023_adultfemales$pregnancy.simple)
  barplot(pregnancy_counts, 
          xlab = "Pregnancy Status (simple)",
          ylab = "Count",
          col = "grey", # Set the color of the bars
          main = "",
          ylim = c(0, 200)) # Set Y-axis limits
  
  # look at pregnacy - full categories
  
  print(table(data2014_2023_adultfemales$pregnancy))
  pregnancy_counts <- table(data2014_2023_adultfemales$pregnancy)
  barplot(pregnancy_counts, 
          xlab = "Pregnancy Status (full)",
          ylab = "Count",
          col = "grey", # Set the color of the bars
          main = "",
          ylim = c(0, 200)) # Set Y-axis limits
  
  # Need to turn Not applicable whales (post reproductive) to not pregnant.
  
  data2014_2023_ST_adultfemales$pregnancy[data2014_2023_ST_adultfemales$pregnancy == "Not Applicable"] <- "Not pregnant"
  data2014_2023_adultfemales$pregnancy[data2014_2023_adultfemales$pregnancy == "Not Applicable"] <- "Not pregnant"
  
  # look at distribution of number of offspring, number of births, number of sons, number of daughters,
  
  hist(data2014_2023_adultfemales$num.offspring)
  hist(data2014_2023_adultfemales$num.births)
  hist(data2014_2023_adultfemales$num.sons)
  hist(data2014_2023_adultfemales$num.daughters)
  
  # categorical has son / adutl son distributions
  
  has.son_counts <- table(data2014_2023_adultfemales$has.son)
  barplot(has.son_counts, 
          xlab = "Has son",
          ylab = "Count",
          col = "grey", # Set the color of the bars
          main = "",
          ylim = c(0, 200)) # Set Y-axis limits
  
  has.adultson_counts <- table(data2014_2023_adultfemales$has.adultson)
  barplot(has.adultson_counts, 
          xlab = "Has adult son",
          ylab = "Count",
          col = "grey", # Set the color of the bars
          main = "",
          ylim = c(0, 200)) # Set Y-axis limits
  
  
  
  ### look at age term to see what degree of natural spline to use
  
  ggplot(data = data2014_2023_adultfemales, aes(x = age, y = mean.ep)) +
    geom_point() +
    geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = TRUE) + 
    labs(title = "", x = "Age", y = "Mean Eye Patch Ratio") +
    theme_classic(base_size = 18) 
  
  # try natural splines
  
  age.model <- glmmTMB(mean.ep ~ ns(age, df = 2)
                   + (1| whale.id) + (1| matriline) ,
                   data = data2014_2023_adultfemales, REML = FALSE)
  
  ggpred <- ggeffects::ggpredict(age.model, term = c("age [all]"))
  
  plot(ggpred, facet = FALSE) +
    labs(title = "",
         x = "Age",
         y = "Eye Patch Ratio") +
    theme_classic(base_size = 16 )
  
  # try age as polynomial
  
  age.model <- glmmTMB(mean.ep ~ poly(age, 2)
                       + (1| whale.id) + (1| matriline) ,
                       data = data2014_2023_ST_adultfemales, REML = FALSE)
  
  ggpred <- ggeffects::ggpredict(age.model, term = c("age [all]"))
  
  plot(ggpred, facet = FALSE) +
    labs(title = "",
         x = "Age",
         y = "Eye Patch Ratio") +
    theme_classic(base_size = 16 )
  
  # plot raw values with age spline
  
  age.model <- glmmTMB(mean.ep ~ ns(age, df = 2)
                       + (1 | whale.id) + (1 | matriline),
                       data = data2014_2023_adultfemales, REML = FALSE)
  
  # Get predicted values
  ggpred <- ggpredict(age.model, term = c("age [all]"))
  
  # Convert predictions to a data frame
  ggpred_df <- as.data.frame(ggpred)
  
  # Plot predictions and raw data
  ggplot() +
    # Raw data points
    geom_point(data = data2014_2023_adultfemales,
               aes(x = age, y = mean.ep),
               alpha = 0.3, color = "black") +
    # Model predictions (line and confidence ribbon)
    geom_ribbon(data = ggpred_df,
                aes(x = x, ymin = conf.low, ymax = conf.high),
                fill = "grey2", alpha = 0.2) +
    geom_line(data = ggpred_df,
              aes(x = x, y = predicted),
              color = "blue", linewidth = 0.5) +
    scale_y_continuous(limits = c(0.99,1.3)) + 
    labs(title = "",
         x = "Age",
         y = "Eye patch ratio") +
    theme_classic(base_size = 12)
  
  
  
  
  
  #### ######## TEST FOR CORRELATION BETWEEN VARIABLES - correlation matrix ############################################
  
  
  
  # need to remove categorical variables from data set to work with only numeric data
  
  data2014_2023_adultfemales_num.variables.corr <- subset(data2014_2023_adultfemales, select = c(num.births, num.offspring, num.sons, num.daughters,
                                                                       salmon, age))
  
  
 
  
  #Pearson correlation assumes normality of variables, a continuous scale, and a linear correlation
  #Spearman correlation does not require normality, continuous scale, or linearity.
  # it does assume relationship is monotonic (values either go up or down with other variable throughout whole range. )
  
  
  data2014_2023_adultfemales_correlation <- cor(data2014_2023_adultfemales_num.variables.corr, method = "spearman")
  
  #make correlation plot
  

  
  corrplot(data2014_2023_adultfemales_correlation, method = "number")
  
  
  # Rename the variables
  colnames(data2014_2023_adultfemales_correlation) <- c("# of births", "# of living offspring", "# of living sons", "# of living daughters", "Salmon", "Age")
  rownames(data2014_2023_adultfemales_correlation) <- c("# of births", "# of living offspring", "# of living sons", "# of living daughters", "Salmon", "Age")
  
  
  # make correlation plot with more visible colours
 
  
  
  color_palette <- colorRampPalette(c("red", "orange", "lightyellow", "lightblue", "royalblue"))
  
  
  corrplot(
    data2014_2023_adultfemales_correlation,
    method = "number",
    col = color_palette(200), # Adjust colors
    bg = "black",
    tl.col = "black",        # Set text label color to black
    tl.cex = 1.0 )            # Optionally adjust text size # set background colour
  
  # make correlation plot with white background
  
  
  color_palette <- colorRampPalette(c("firebrick", "darkgray", "steelblue"))

  
  
  corrplot(
    data2014_2023_adultfemales_correlation,
    method = "number",
    col = color_palette(200), # Adjust colors
    bg = "white",
    tl.col = "black",        # Set text label color to black
    tl.cex = 1.0 )            # Optionally adjust text size # set background colour
  
  
  
#### Candidate models #####
 
  
  null.model <- glmmTMB(mean.ep ~ 1 + 
                          (1| matriline) + (1| whale.id) ,
                        data = data2014_2023_ST_adultfemales, REML = FALSE)
  
  m.1 <- glmmTMB(mean.ep ~ pregnancy.simple + num.offspring*ns(age, df = 2) + num.offspring*salmon +
                   (1| matriline) + (1| whale.id) ,
                 data = data2014_2023_ST_adultfemales, REML = FALSE)
  
  m.2 <- glmmTMB(mean.ep ~ pregnancy.simple + num.births*ns(age, df = 2) + num.births*salmon +
                   (1| matriline) + (1| whale.id) ,
                 data = data2014_2023_ST_adultfemales, REML = FALSE)
  
  m.3 <- glmmTMB(mean.ep ~ pregnancy.simple + num.sons*ns(age, df = 2) + num.sons*salmon +
                   (1| matriline) + (1| whale.id) ,
                 data = data2014_2023_ST_adultfemales, REML = FALSE)
  
  m.4 <- glmmTMB(mean.ep ~ pregnancy.simple + num.daughters*ns(age, df = 2) + num.daughters*salmon +
                   (1| matriline) + (1| whale.id) ,
                 data = data2014_2023_ST_adultfemales, REML = FALSE)
  
  
  
  
  #### model diagnostics with DHARMa
  
  res.top.model <- simulateResiduals(fittedModel = m.4) # change out for the model I am interested in
  plot(res.top.model)
  
  #testUniformity() - tests if the overall distribution conforms to expectations
  testUniformity(res.top.model)
  #testOutliers() - tests if there are more simulation outliers than expected
  testOutliers(res.top.model)
  #testDispersion() - tests if the simulated dispersion is equal to the observed dispersion
  testDispersion(res.top.model)
  #testQuantiles() - fits a quantile regression or residuals against a predictor (default predicted value), and tests of this conforms to the expected quantile
  testQuantiles(res.top.model)
  #testZeroinflation() - tests if there are more zeros in the data than expected from the simulations
  testZeroInflation(res.top.model)
  
  
  # check heteroskedacity of residuals
  
  residuals_data <- res.top.model$scaledResiduals
  
  
  
  # and plot residuals for num.sons variable
  
  plot(data2014_2023_ST_adultfemales$num.sons, residuals_data,
       main = "Residuals vs. Num sons",
       xlab = "Num sons",
       ylab = "Residuals",
       pch = 20)
  abline(h = 0, col = "red") # have problem with heteroskadasity in Num sons. 
  
  # and plot residuals for num.daughters variable
  
  plot(data2014_2023_ST_adultfemales$num.daughters, residuals_data,
       main = "Residuals vs. Num daughters",
       xlab = "Num daughters",
       ylab = "Residuals",
       pch = 20)
  abline(h = 0, col = "red") # have problem with heteroskadasity in Num sons.
  
  
  
  # check distribution of random effects: 
  random_effects <- ranef(m.4)
  # For each random effect grouping factor (matriline, whale.id):
  # Extract random intercepts for 'matriline'
  matriline_effects <- random_effects$cond$matriline[[1]]
  # Extract random intercepts for 'whale.id'
  whale_effects <- random_effects$cond$whale.id[[1]]
  # Plot histogram of random effects for 'matriline'
  ggplot(data.frame(effect = matriline_effects), aes(x = effect)) +
    geom_histogram(binwidth = 0.1, color = "black") +
    ggtitle("Random Effects for Matriline") +
    theme_minimal()
  # Plot histogram of random effects for 'whale.id'
  ggplot(data.frame(effect = whale_effects), aes(x = effect)) +
    geom_histogram(binwidth = 0.1, color = "black") +
    ggtitle("Random Effects for Whale ID") +
    theme_minimal()
  # Q-Q plot for 'matriline' random effects
  qqnorm(matriline_effects)
  qqline(matriline_effects)
  # Q-Q plot for 'whale.id' random effects
  qqnorm(whale_effects)
  qqline(whale_effects)
  # Boxplot for 'matriline' random effects
  boxplot(matriline_effects, main = "Random Effects for Matriline")
  # Boxplot for 'whale.id' random effects
  boxplot(whale_effects, main = "Random Effects for Whale ID")
  
  
  # looks like I have outliers in the random effect of whale.id and matriline
  # identify which whales and matriline are outliers for the random effects distribution
  
  ranef(m.3)
  
  # looks like I27s are the matriline outside of the normal distribution
  # and I63 is the other outlier for whale.id
  # I63 could be a real outlier. Whale with no offspring that died in 2014 the year she was measured. She was severly emaciated
  # try removing her from data set
  
  data2014_2023_ST_adultfemales_noI63 <- subset(data2014_2023_ST_adultfemales, whale.id != "W45")
  data2014_2023_adultfemales_noI63 <- subset(data2014_2023_adultfemales, whale.id != "W45")
  data2014_2023_STpred_adultfemales_noI63 <- subset(data2014_2023_STpred_adultfemales, whale.id != "W45")
  
  
  
  
  
  ### rerun models #####
  
 
   null.model <- glmmTMB(mean.ep ~ 1 + 
                          + (1| matriline) + (1| whale.id),
                        data = data2014_2023_ST_adultfemales_noI63, REML = FALSE)
  
  m.1 <- glmmTMB(mean.ep ~ pregnancy.simple + num.offspring*ns(age, df =2) + num.offspring*salmon +
                   + (1| matriline) + (1| whale.id) ,
                 data = data2014_2023_ST_adultfemales_noI63, REML = FALSE)
  
  m.2 <- glmmTMB(mean.ep ~ pregnancy.simple + num.births*ns(age, df = 2) + num.births*salmon +
                   + (1| matriline) + (1| whale.id) ,
                 data = data2014_2023_ST_adultfemales_noI63, REML = FALSE)
  
  
  m.3 <- glmmTMB(mean.ep ~ pregnancy.simple + num.sons*ns(age, df = 2) + num.sons*salmon 
                 + (1| matriline) + (1| whale.id) ,
                 data = data2014_2023_ST_adultfemales_noI63, REML = FALSE)
  
  m.4 <- glmmTMB(mean.ep ~ pregnancy.simple + num.daughters*ns(age, df = 2) + num.daughters*salmon
                  + (1| matriline) + (1| whale.id) ,
                 data = data2014_2023_ST_adultfemales_noI63, REML = FALSE)
  

  

  # re run diagnotsics 
  
  res.top.model <- simulateResiduals(fittedModel = m.4) # change out for the model I am interested in
  plot(res.top.model)
  
  
  ### MOdel seletion
  ## make list of models
  
  Cand.models.adultfemales <- list(m.1,
                                   m.2,
                                   m.3,
                                   m.4,
                                   null.model)
  
  ## build a model selection table
  
  names <- c("m.1", "m.2", "m.3", "m.4", "null") # List your model names accordingly
  
  selectionTable.adultfemales <- aictab(cand.set = Cand.models.adultfemales, modnames = names)
  selectionTable.adultfemales
  
  summary(m.1)
  summary(m.2)
  summary(m.3)
  summary(m.4)
  
  # R squared 
  
  performance::r2(m.1)
  performance::r2(m.2)
  performance::r2(m.3)
  performance::r2(m.4)
  
 
  

  ########### Investigating number of births model ##############
  
  m.2 <- glmmTMB(mean.ep ~ pregnancy.simple + num.births*ns(age, df = 2) + num.births*salmon +
                   (1| matriline) + (1| whale.id) ,
                 data = data2014_2023_ST_adultfemales_noI63, REML = FALSE)
  
  #VIF - ned to use lmer
  
  # GVIF^(1/(2*Df)) accounts for the degrees of freedom in the model
  # This adjusted version is particularly useful when dealing with models with different numbers of predictors, as it takes into consideration the impact of the degrees of freedom on the collinearity assessment.
  
  
  vif(lmer(mean.ep ~ pregnancy.simple + num.births*ns(age, df = 2) + num.births*salmon +
             (1| matriline) + (1| whale.id) ,
           data = data2014_2023_ST_adultfemales_noI63, REML = FALSE))
  
  # R squared 
  
  performance::r2(m.2)
  
  ### TOP MODEL ###
  
  summary(m.2)
  
  # COeficient plot
  
  
  
  sjPlot::plot_model(m.2, axis.labels=c(""),
                     show.values=TRUE, show.p=TRUE, vline.color = "grey", type = "est",
                     value.offset = 0.3) +
    ggtitle("") +
    theme_classic(base_size = 16)  
  
  
  
  
  #### USE EMTRENDS to see differences between interactions
  
  
  # use real EP values
  
  
  
  m.2_noST <- glmmTMB(mean.ep ~ pregnancy.simple + num.births*ns(age, df = 2) + num.births*salmon +
                        (1| matriline) + (1| whale.id) ,
                      data = data2014_2023_adultfemales_noI63, REML = FALSE)
  
  emtrends(m.2_noST, pairwise ~ age, var="num.births", at=list(age=c(16.9,28.8,40.7))) # using values from mean age + - 1 SD
  
  
  summary(m.2_noST)
  
  ## Use GG predict to plot the model
  
  
  
  ggpred <- ggeffects::ggpredict(m.2_noST, term = c("num.births[all]", "age"))
  
  
  
  plot(ggpred, facet = FALSE) +
    labs(title = "",
         x = "Number of births",
         y = "Eye Patch Ratio") +
    theme_classic(base_size = 16 ) +
    scale_fill_brewer(palette = "Set1")  # Using a color palette that supports more levels
  
  
  
  
  # make a raw data plot and predicted plot:
  
  # with continous viridis colour plalette for continuous age
  
  
  
  raw_plot <- ggplot(data = data2014_2023_adultfemales_noI63, aes(x = num.births, y = mean.ep, color = age)) +
    geom_point(size = 2, alpha = 0.8) +
    xlab("Number of Births") +
    ylab("Eye Patch Ratio") +
    theme_classic(base_size = 20) +
    scale_y_continuous(limits = c(1.05, 1.27)) +
    scale_x_continuous(breaks = 0:8) + 
    scale_color_viridis(option = "D") + 
    labs(color = "Age")
  
  # need ggpred plot to be match viridis palette
  
  
  custom_palette <- c("#1F9A8AFF", "#365D8DFF", "#471164FF")
  
  
  # Reorder levels of the 'group' variable in descending order
  ggpred$group <- factor(ggpred$group, levels = rev(levels(ggpred$group)))
  
  ggpred_plot <- ggplot(data = ggpred, aes(x = x, y = predicted, colour = group, fill = group)) +
    geom_smooth(method = lm, se = FALSE) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high, colour = group), alpha = 0.5, colour = NA) + 
    xlab("Number of Births") +
    ylab("Eye Patch Ratio") +
    theme_classic(base_size = 20) + 
    guides(color = guide_legend(title = "Age"), fill = guide_legend(title = "Age")) +
    scale_y_continuous(limits = c(1.05, 1.27)) +
    scale_x_continuous(breaks = 0:8) +
    scale_color_manual(values = custom_palette) +  # Set custom colors
    scale_fill_manual(values = custom_palette)    # Set custom fill colors
  
 
  
  
  #Convert to a dataframe to restrict age 16.9 to biologically plausible numbers!
  ggpred_df <- as.data.frame(ggpred)
  
  # Filter: keep full x-range for other ages; keep only x <= 3 for age = 16.9
  ggpred_df <- ggpred_df |>
    dplyr::filter(!(group == "16.9" & x > 4))
  
  
  ggpred_plot <- ggplot(data = ggpred_df,
                        aes(x = x, y = predicted, colour = group, fill = group)) +
    geom_smooth(method = lm, se = FALSE) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.5, colour = NA) +
    xlab("Number of Births") +
    ylab("Eye Patch Ratio") +
    theme_classic(base_size = 20) +
    guides(color = guide_legend(title = "Age"),
           fill = guide_legend(title = "Age")) +
    scale_y_continuous(limits = c(1.05, 1.27)) +
    scale_x_continuous(breaks = 0:8) +
    scale_color_manual(values = custom_palette) +
    scale_fill_manual(values = custom_palette)
  
  
  
  ggpred_plot + raw_plot
  
  
  
  
  ### Investigate salmon relationships
  
  # Just plot effect of salmon from top model
  
  ggpred_salmon <- ggeffects::ggpredict(m.2_noST, term = c("salmon[all]"))
  
  ggplot(ggpred_salmon, aes(x, predicted)) +
    geom_point(aes(salmon, mean.ep), data = data2014_2023_adultfemales, size = 2, colour = "navy", alpha = 0.4) +
    geom_smooth(method = lm, linewidth = 1, colour = "navy") +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "navy") +
    xlab("Salmon Abundance Index") +
    ylab("Eye Patch Ratio") +
    theme_classic(base_size = 16) +
    theme(plot.margin = unit(c(1, 1, 1, 1), "cm"))



  
  
  ggpred <- ggeffects::ggpredict(m.2_noST, term = c("num.births[all]", "age", "salmon[meansd]"))
  
  
  
   ggplot(data = ggpred, aes(x = x, y = predicted, colour = group, fill = group)) +
    geom_smooth(method = lm, se = FALSE) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high, colour = group), alpha = 0.2, colour = NA) + 
    xlab("Number of birhts") +
    ylab("EP Ratio") +
    theme_classic(base_size = 16) + 
    guides(color = guide_legend(title = "Age"), fill = guide_legend(title = "Age")) +
    facet_wrap(~ facet, scales = "free", labeller = labeller(sex = c("0.84" = "Salmon = 0.84", "1.1" = "Salmon = 1.09", "1.35" = "Salmon = 1.35"))) +
    scale_y_continuous(limits = c(1.05, 1.27))
   
   
   # plot raw and ggpred plots with custom colours
   
   
   
   # Reorder levels of the 'group' variable in descending order
   ggpred$group <- factor(ggpred$group, levels = rev(levels(ggpred$group)))
   
   ggplot(data = ggpred, aes(x = x, y = predicted, colour = group, fill = group)) +
     geom_smooth(method = lm, se = FALSE) +
     geom_ribbon(aes(ymin = conf.low, ymax = conf.high, colour = group), alpha = 0.4, colour = NA) + 
     xlab("Number of Births") +
     ylab("Eye Patch Ratio") +
     theme_classic(base_size = 16) + 
     guides(color = guide_legend(title = "Age"), fill = guide_legend(title = "Age")) +
     facet_wrap(~ facet, scales = "free", ) +
     scale_y_continuous(limits = c(1.05, 1.27))  +
     scale_color_manual(values = custom_palette) +  # Set custom colors
     scale_fill_manual(values = custom_palette)    # Set custom fill colors
   
   #Convert to a dataframe to restrict age 16.9 to biologically plausible numbers!
   ggpred_df <- as.data.frame(ggpred)
   
   # Filter: keep full x-range for other ages; keep only x <= 3 for age = 16.9
   ggpred_df <- ggpred_df |>
     dplyr::filter(!(group == "16.9" & x > 4))
   
   
   ggplot(data = ggpred_df, aes(x = x, y = predicted, colour = group, fill = group)) +
     geom_smooth(method = lm, se = FALSE) +
     geom_ribbon(aes(ymin = conf.low, ymax = conf.high, colour = group), alpha = 0.4, colour = NA) + 
     xlab("Number of Births") +
     ylab("Eye Patch Ratio") +
     theme_classic(base_size = 16) + 
     guides(color = guide_legend(title = "Age"), fill = guide_legend(title = "Age")) +
     facet_wrap(~ facet, scales = "free", ) +
     scale_y_continuous(limits = c(1.05, 1.27))  +
     scale_color_manual(values = custom_palette) +  # Set custom colors
     scale_fill_manual(values = custom_palette)    # Set custom fill colors
   
    
    # Now jsut plot with out age term for simplicity
    
    # make new custom colours since not using age - all one nice colour
    
    custom_colours <- c("#365D8DFF","#365D8DFF", "#365D8DFF")
    
    ggpred <- ggeffects::ggpredict(m.2_noST, term = c("num.births[all]", "salmon[meansd]"))
    
    
    
    ggplot(data = ggpred, aes(x = x, y = predicted, colour = group, fill = group)) +
      geom_smooth(method = lm, se = FALSE) +
      geom_ribbon(aes(ymin = conf.low, ymax = conf.high, colour = group), alpha = 0.4, colour = NA) + 
      xlab("Number of Births") +
      ylab("Eye Patch Ratio") +
      theme_classic(base_size = 16) +
      facet_wrap(~ group, scales = "free", ) +
      scale_y_continuous(limits = c(1.05, 1.27))  +
      scale_color_manual(values = custom_colours) +  # Set custom colors
      scale_fill_manual(values = custom_colours)    # Set custom fill colors
    
    
   
    
################ Investigate number of offspring model ###################################
    
    # COeficient plot
    sjPlot::plot_model(m.1, axis.labels=c(""),
                       show.values=TRUE, show.p=TRUE, vline.color = "grey", type = "est",
                       value.offset = 0.3) +
      ggtitle("") +
      theme_classic(base_size = 16)  
    
    # R squared 
    
    performance::r2(m.1)
    
    
    # use real EP values
    
    m.1_noST <- glmmTMB(mean.ep ~ pregnancy.simple + num.offspring*ns(age, df = 2) + num.offspring*salmon +
                          (1| matriline) + (1| whale.id) ,
                        data = data2014_2023_adultfemales_noI63, REML = FALSE)
    
    summary(m.1_noST)
    
    # look at slopes
    
    emtrends(m.1_noST, pairwise ~ age, var="num.offspring", at=list(age=c(16.9,28.8,40.7))) # using values from mean age + - 1 SD
    
    ## Use GG predict to plot the model
    
    ggpred <- ggeffects::ggpredict(m.1_noST, term = c("num.offspring[all]", "age"))
    
    plot(ggpred, facet = FALSE) +
      labs(title = "",
           x = "Number of alive offspring",
           y = "Eye Patch Ratio") +
      theme_classic(base_size = 16 ) +
      scale_fill_brewer(palette = "Set1")  # Using a color palette that supports more levels
    
    
    
    # make a raw data plot and predicted plot:
    
    # with continous viridis colour plalette for continuous age
    
    
    
    raw_plot <- ggplot(data = data2014_2023_adultfemales_noI63, aes(x = num.offspring, y = mean.ep, color = age)) +
      geom_point(size = 2, alpha = 0.8) +
      xlab("Number of Living Offspring") +
      ylab("Eye Patch Ratio") +
      theme_classic(base_size = 20) +
      scale_y_continuous(limits = c(1.05, 1.27)) +
      scale_x_continuous(breaks = 0:5) +
      scale_color_viridis(option = "D") + 
      labs(color = "Age")
    
    
    
    
    # remake gg pred plot with colour palette
    
    # Reorder levels of the 'group' variable in descending order
    ggpred$group <- factor(ggpred$group, levels = rev(levels(ggpred$group)))
    
    ggpred_plot <- ggplot(data = ggpred, aes(x = x, y = predicted, colour = group, fill = group)) +
      geom_smooth(method = lm, se = FALSE) +
      geom_ribbon(aes(ymin = conf.low, ymax = conf.high, colour = group), alpha = 0.4, colour = NA) + 
      xlab("Number of Alive Offspring") +
      ylab("Eye Patch Ratio") +
      theme_classic(base_size = 20) + 
      guides(color = guide_legend(title = "Age"), fill = guide_legend(title = "Age")) +
      scale_y_continuous(limits = c(1.05, 1.27)) +
      scale_x_continuous(breaks = 0:5) +
      scale_color_manual(values = custom_palette) +  # Set custom colors
      scale_fill_manual(values = custom_palette)    # Set custom fill colors
    
    #Convert to a dataframe to restrict age 16.9 to biologically plausible numbers!
    ggpred_df <- as.data.frame(ggpred)
    
    # Filter: keep full x-range for other ages; keep only x <= 2 for age = 16.9
    ggpred_df <- ggpred_df |>
      dplyr::filter(!(group == "16.9" & x > 2))
    
    ggpred_plot <- ggplot(data = ggpred_df, aes(x = x, y = predicted, colour = group, fill = group)) +
      geom_smooth(method = lm, se = FALSE) +
      geom_ribbon(aes(ymin = conf.low, ymax = conf.high, colour = group), alpha = 0.4, colour = NA) + 
      xlab("Number of Alive Offspring") +
      ylab("Eye Patch Ratio") +
      theme_classic(base_size = 20) + 
      guides(color = guide_legend(title = "Age"), fill = guide_legend(title = "Age")) +
      scale_y_continuous(limits = c(1.05, 1.27)) +
      scale_x_continuous(breaks = 0:5) +
      scale_color_manual(values = custom_palette) +  # Set custom colors
      scale_fill_manual(values = custom_palette)    # Set custom fill colors
    
    
    ggpred_plot + raw_plot 
    
    # Investigate salmon relationships
    
    # Make salmon categorical column for raw plots
    
    ggpred <- ggeffects::ggpredict(m.1_noST, term = c("num.offspring[all]", "age", "salmon[meansd]"))
    
    
    
    # Just plot effect of salmon from top model
    
    ggpred_salmon <- ggeffects::ggpredict(m.1_noST, term = c("salmon[all]"))
    
    ggplot(ggpred_salmon, aes(x, predicted)) +
      geom_point(aes(salmon, mean.ep), data = data2014_2023_adultfemales, size = 2, colour = "navy", alpha = 0.4) +
      geom_smooth(method = lm, linewidth = 1, colour = "navy") +
      geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, fill = "navy") +
      xlab("Chinook salmon abundance index") +
      ylab("Eye patch ratio") +
      theme_classic(base_size = 16) +
      theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
      scale_y_continuous(limits = c(1.05, 1.27))

    # plot raw and ggpred plots with custom colours
    
    # Reorder levels of the 'group' variable in descending order
    ggpred$group <- factor(ggpred$group, levels = rev(levels(ggpred$group)))
    
    ggplot(data = ggpred, aes(x = x, y = predicted, colour = group, fill = group)) +
      geom_smooth(method = lm, se = FALSE) +
      geom_ribbon(aes(ymin = conf.low, ymax = conf.high, colour = group), alpha = 0.4, colour = NA) + 
      xlab("Number of living offspring") +
      ylab("Eye patch ratio") +
      theme_classic(base_size = 16) + 
      guides(color = guide_legend(title = "Age"), fill = guide_legend(title = "Age")) +
      facet_wrap(~ facet, scales = "free", ) +
      scale_y_continuous(limits = c(1.05, 1.27))  +
      scale_color_manual(values = custom_palette) +  # Set custom colors
      scale_fill_manual(values = custom_palette)    # Set custom fill colors
    
    #Convert to a dataframe to restrict age 16.9 to biologically plausible numbers!
    ggpred_df <- as.data.frame(ggpred)
    
    # Filter: keep full x-range for other ages; keep only x <= 2 for age = 16.9
    ggpred_df <- ggpred_df |>
      dplyr::filter(!(group == "16.9" & x > 2))
    
    #remake plot
    
    ggplot(data = ggpred_df, aes(x = x, y = predicted, colour = group, fill = group)) +
      geom_smooth(method = lm, se = FALSE) +
      geom_ribbon(aes(ymin = conf.low, ymax = conf.high, colour = group), alpha = 0.4, colour = NA) + 
      xlab("Number of living offspring") +
      ylab("Eye patch ratio") +
      theme_classic(base_size = 16) + 
      guides(color = guide_legend(title = "Age"), fill = guide_legend(title = "Age")) +
      facet_wrap(~ facet, scales = "free", ) +
      scale_y_continuous(limits = c(1.05, 1.27))  +
      scale_color_manual(values = custom_palette) +  # Set custom colors
      scale_fill_manual(values = custom_palette)    # Set custom fill colors
  
    
    
    # Now jsut plot with out age term for simplicity
    
    # make new custom colours since not using age - all one nice colour
    
    custom_colours <- c("#365D8DFF","#365D8DFF", "#365D8DFF")
    
    ggpred <- ggeffects::ggpredict(m.1_noST, term = c("num.offspring[all]", "salmon[meansd]"))
    
    
    
    ggplot(data = ggpred, aes(x = x, y = predicted, colour = group, fill = group)) +
      geom_smooth(method = lm, se = FALSE) +
      geom_ribbon(aes(ymin = conf.low, ymax = conf.high, colour = group), alpha = 0.4, colour = NA) + 
      xlab("Number of Alive Offspring") +
      ylab("Eye Patch Ratio") +
      theme_classic(base_size = 16) +
      facet_wrap(~ group, scales = "free", ) +
      scale_y_continuous(limits = c(1.05, 1.27))  +
      scale_color_manual(values = custom_colours) +  # Set custom colors
      scale_fill_manual(values = custom_colours)    # Set custom fill colors
    
    
    
    
  ### make plot of first 2 ranking models compared
    
    ggpred_m.2 <- ggeffects::ggpredict(m.2_noST, term = c("num.births[all]", "age"))
  
    raw_plot_m.2 <- ggplot(data = data2014_2023_adultfemales_noI63, aes(x = num.births, y = mean.ep, color = age)) +
      geom_point(size = 2, alpha = 0.8) +
      xlab("Number of births") +
      ylab("Eye patch ratio") +
      theme_classic(base_size = 12) +
      scale_y_continuous(limits = c(1.05, 1.27)) +
      scale_x_continuous(breaks = 0:8) + 
      scale_color_viridis(option = "D") + 
      labs(color = "Age")
    
    
    #Convert to a dataframe and restrict age 16.9 to biologically plausible numbers!
    ggpred_m.2 <- as.data.frame(ggpred_m.2)
    ggpred_m.2 <- ggpred_m.2 |>
      dplyr::filter(!(group == "16.9" & x > 4))
    
    # Reorder levels of the 'group' variable in descending order
    ggpred_m.2$group <- factor(ggpred_m.2$group, levels = rev(levels(ggpred_m.2$group)))
    
    ggpred_m.2_plot <- ggplot(data = ggpred_m.2, aes(x = x, y = predicted, colour = group, fill = group)) +
      geom_smooth(method = lm, se = FALSE) +
      geom_ribbon(aes(ymin = conf.low, ymax = conf.high, colour = group), alpha = 0.5, colour = NA) + 
      xlab("Number of births") +
      ylab("Eye patch ratio") +
      theme_classic(base_size = 12) + 
      guides(color = guide_legend(title = "Age"), fill = guide_legend(title = "Age")) +
      scale_y_continuous(limits = c(1.05, 1.27)) +
      scale_x_continuous(breaks = 0:8) +
      scale_color_manual(values = custom_palette) +  # Set custom colors
      scale_fill_manual(values = custom_palette)    # Set custom fill colors
    
    
    
    # Now same for first ranking model
    
    ggpred_m.1 <- ggeffects::ggpredict(m.1_noST, term = c("num.offspring[all]", "age"))
    
    raw_plot_m.1 <- ggplot(data = data2014_2023_adultfemales_noI63, aes(x = num.offspring, y = mean.ep, color = age)) +
      geom_point(size = 2, alpha = 0.8) +
      xlab("Number of living offspring") +
      ylab("Eye patch ratio") +
      theme_classic(base_size = 12) +
      scale_y_continuous(limits = c(1.05, 1.27)) +
      scale_x_continuous(breaks = 0:6) + 
      scale_color_viridis(option = "D") + 
      labs(color = "Age")
  
    
    #Convert to a dataframe and restrict age 16.9 to biologically plausible numbers!
    ggpred_m.1 <- as.data.frame(ggpred_m.1)
    ggpred_m.1 <- ggpred_m.1 |>
      dplyr::filter(!(group == "16.9" & x > 2))
    
    
    ggpred_m.1$group <- factor(ggpred_m.1$group, levels = rev(levels(ggpred_m.1$group)))
    
    ggpred_m.1_plot <- ggplot(data = ggpred_m.1, aes(x = x, y = predicted, colour = group, fill = group)) +
      geom_smooth(method = lm, se = FALSE) +
      geom_ribbon(aes(ymin = conf.low, ymax = conf.high, colour = group), alpha = 0.5, colour = NA) + 
      xlab("Number of living offspring") +
      ylab("Eye patch ratio") +
      theme_classic(base_size = 12) + 
      guides(color = guide_legend(title = "Age"), fill = guide_legend(title = "Age")) +
      scale_y_continuous(limits = c(1.05, 1.27)) +
      scale_x_continuous(breaks = 0:6) +
      scale_color_manual(values = custom_palette) +  # Set custom colors
      scale_fill_manual(values = custom_palette)    # Set custom fill colors
    
    ggpred_m.1_plot + ggpred_m.2_plot + raw_plot_m.1 + raw_plot_m.2
    
  
   ############# Investigate number of sons variable #################################################
   
   m.3 <- glmmTMB(mean.ep ~ pregnancy.simple + num.sons*ns(age, df = 2) + num.sons*salmon +
                    (1| matriline) + (1| whale.id) ,
                  data = data2014_2023_ST_adultfemales_noI63, REML = FALSE)
   
   
   # R squared 
   
   performance::r2(m.3)
   
   
   
   m.3_noST <- glmmTMB(mean.ep ~ pregnancy.simple + num.sons*ns(age, df = 2) + num.sons*salmon +
                    (1| matriline) + (1| whale.id) ,
                  data = data2014_2023_adultfemales_noI63, REML = FALSE)
   
   summary(m.3_noST)
   
   emtrends(m.3_noST, pairwise ~ age, var="num.sons", at=list(age=c(16.9,28.8,40.7))) # using values from mean age + - 1 SD
   
   # Coefficient plot 
   
   sjPlot::plot_model(m.3, axis.labels=c(""),
                      show.values=TRUE, show.p=TRUE, vline.color = "red", type = "est") +
     theme_classic(base_size = 16) 
   
   # plot using ggpredict
   
   ggpred <- ggeffects::ggpredict(m.3_noST, term = c("num.sons[all]", "age", "salmon[meansd]"))
   
   ggplot(data = ggpred, aes(x = x, y = predicted, colour = group, fill = group)) +
     geom_smooth(method = lm, se = FALSE) +
     geom_ribbon(aes(ymin = conf.low, ymax = conf.high, colour = group), alpha = 0.2, colour = NA) + 
     xlab("Number of Sons") +
     ylab("EP Ratio") +
     theme_classic(base_size = 16) + 
     guides(color = guide_legend(title = "Age"), fill = guide_legend(title = "Age")) +
     facet_wrap(~ facet, scales = "free")
   
   # Plot without age interaction or salmon
   
   ggpred <- ggeffects::ggpredict(m.3_noST, term = c("num.sons[all]", "salmon[meansd]"))
   
   ggplot(data = ggpred, aes(x = x, y = predicted, colour = group, fill = group)) +
     geom_smooth(method = lm, se = FALSE) +
     geom_ribbon(aes(ymin = conf.low, ymax = conf.high, colour = group), alpha = 0.2, colour = NA) + 
     xlab("Number of Sons") +
     ylab("EP Ratio") +
     theme_classic(base_size = 16) + 
     guides(color = guide_legend(title = "Salmon"), fill = guide_legend(title = "Salmon"))
   
   ggpred <- ggeffects::ggpredict(m.3_noST, term = c("num.sons[all]", "age"))
   
   
   ggplot(data = ggpred, aes(x = x, y = predicted, colour = group, fill = group)) +
     geom_smooth(method = lm, se = FALSE) +
     geom_ribbon(aes(ymin = conf.low, ymax = conf.high, colour = group), alpha = 0.2, colour = NA) + 
     xlab("Number of Sons") +
     ylab("EP Ratio") +
     theme_classic(base_size = 16) + 
     guides(color = guide_legend(title = "Age"), fill = guide_legend(title = "Age"))
   
   
   # make plot with custom colours
   
   # Reorder levels of the 'group' variable in descending order
   ggpred$group <- factor(ggpred$group, levels = rev(levels(ggpred$group)))
   
   m.3plot <- ggplot(data = ggpred, aes(x = x, y = predicted, colour = group, fill = group)) +
     geom_smooth(method = lm, se = FALSE) +
     geom_ribbon(aes(ymin = conf.low, ymax = conf.high, colour = group), alpha = 0.5, colour = NA) + 
     xlab("Number of Alive Sons") +
     ylab("Eye Patch Ratio") +
     theme_classic(base_size = 16) + 
     guides(color = guide_legend(title = "Age"), fill = guide_legend(title = "Age")) +
     scale_y_continuous(limits = c(1.05, 1.27)) +
     scale_color_manual(values = custom_palette) +  # Set custom colors
     scale_fill_manual(values = custom_palette)    # Set custom fill colors
   
   # make raw plot
   
   m.3plotraw <- ggplot(data = data2014_2023_adultfemales_noI63, aes(x = num.sons, y = mean.ep, color = age)) +
     geom_point(size = 2, alpha = 0.8) +
     xlab("Number of Alive Sons") +
     ylab("Eye Patch Ratio") +
     theme_classic(base_size = 16) +
     scale_y_continuous(limits = c(1.05, 1.27)) + 
     scale_color_viridis(option = "D") +    
     labs(colour = "Age")
   
   m.3plot + m.3plotraw
   
   #### Investigate number of daughters variable ###########################################
   
   
   m.4 <- glmmTMB(mean.ep ~ pregnancy.simple + num.daughters*ns(age, df = 2) + num.daughters*salmon +
                  (1| matriline) + (1| whale.id) ,
                  data = data2014_2023_ST_adultfemales_noI63, REML = FALSE)
   
   m.4_noST <- glmmTMB(mean.ep ~ pregnancy.simple + num.daughters*ns(age, df = 2) + num.daughters*salmon +
                         (1| matriline) + (1| whale.id) ,
                       data = data2014_2023_adultfemales_noI63, REML = FALSE)
   
   summary(m.4_noST)
   
   # R squared 
   
   performance::r2(m.4)
   
   
   emtrends(m.4_noST, pairwise ~ age, var="num.daughters", at=list(age=c(16.9,28.8,40.7))) # using values from mean age + - 1 SD
   
   # Coefficient plot 
   
   sjPlot::plot_model(m.4, axis.labels=c(""),
                      show.values=TRUE, show.p=TRUE, vline.color = "red", type = "est") +
     theme_classic(base_size = 16) 
   
   # plot using ggpredict
   
   ggpred <- ggeffects::ggpredict(m.4_noST, term = c("num.daughters[all]", "age", "salmon[meansd]"))
   
   ggplot(data = ggpred, aes(x = x, y = predicted, colour = group, fill = group)) +
     geom_smooth(method = lm, se = FALSE) +
     geom_ribbon(aes(ymin = conf.low, ymax = conf.high, colour = group), alpha = 0.2, colour = NA) + 
     xlab("Number of daughters") +
     ylab("EP Ratio") +
     theme_classic(base_size = 16) + 
     guides(color = guide_legend(title = "Age"), fill = guide_legend(title = "Age")) +
     facet_wrap(~ facet, scales = "free")
   
   # Plot without age interaction or salmon
   
   ggpred <- ggeffects::ggpredict(m.4_noST, term = c("num.daughters[all]", "salmon[meansd]"))
   
   ggplot(data = ggpred, aes(x = x, y = predicted, colour = group, fill = group)) +
     geom_smooth(method = lm, se = FALSE) +
     geom_ribbon(aes(ymin = conf.low, ymax = conf.high, colour = group), alpha = 0.2, colour = NA) + 
     xlab("Number of daughters") +
     ylab("EP Ratio") +
     theme_classic(base_size = 16) + 
     guides(color = guide_legend(title = "Salmon"), fill = guide_legend(title = "Salmon"))
   

   # Plot with custom colours
   ggpred <- ggeffects::ggpredict(m.4_noST, term = c("num.daughters[all]", "age"))
   
   # Reorder levels of the 'group' variable in descending order
   ggpred$group <- factor(ggpred$group, levels = rev(levels(ggpred$group)))
   
   m.4plot <- ggplot(data = ggpred, aes(x = x, y = predicted, colour = group, fill = group)) +
     geom_smooth(method = lm, se = FALSE) +
     geom_ribbon(aes(ymin = conf.low, ymax = conf.high, colour = group), alpha = 0.5, colour = NA) + 
     xlab("Number of Alive Daughters") +
     ylab("Eye Patch Ratio") +
     theme_classic(base_size = 16) + 
     guides(color = guide_legend(title = "Age"), fill = guide_legend(title = "Age")) +
     scale_y_continuous(limits = c(1.05, 1.27)) +
     scale_color_manual(values = custom_palette) +  # Set custom colors
     scale_fill_manual(values = custom_palette)    # Set custom fill colors
   
   # make raw plot
   
  m.4plotraw <- ggplot(data = data2014_2023_adultfemales_noI63, aes(x = num.daughters, y = mean.ep, color = age)) +
     geom_point(size = 2, alpha = 0.8) +
     xlab("Number of Alive Daughters") +
     ylab("Eye Patch Ratio") +
     theme_classic(base_size = 16) +
     scale_y_continuous(limits = c(1.05, 1.27)) + 
     scale_color_viridis(option = "D") +    
     labs(colour = "Age")
   
   
   m.4plot + m.4plotraw
   
   
   
   ### make plot of of sons and daughters models compared
   
   ggpred_m.3 <- ggeffects::ggpredict(m.3_noST, term = c("num.sons[all]", "age"))
   
   raw_plot_m.3 <- ggplot(data = data2014_2023_adultfemales_noI63, aes(x = num.sons, y = mean.ep, color = age)) +
     geom_point(size = 2, alpha = 0.8) +
     xlab("Number of living sons") +
     ylab("Eye patch ratio") +
     theme_classic(base_size = 12) +
     scale_y_continuous(limits = c(1.05, 1.27)) +
     scale_x_continuous(breaks = 0:4) + 
     scale_color_viridis(option = "D") + 
     labs(color = "Age")
   
   #Convert to a dataframe and restrict age 16.9 to biologically plausible numbers!
   ggpred_m.3 <- as.data.frame(ggpred_m.3)
   ggpred_m.3 <- ggpred_m.3 |>
     dplyr::filter(!(group == "16.9" & x > 2))
   
   
   # Reorder levels of the 'group' variable in descending order
   ggpred_m.3$group <- factor(ggpred_m.3$group, levels = rev(levels(ggpred_m.3$group)))
   
   ggpred_m.3_plot <- ggplot(data = ggpred_m.3, aes(x = x, y = predicted, colour = group, fill = group)) +
     geom_smooth(method = lm, se = FALSE) +
     geom_ribbon(aes(ymin = conf.low, ymax = conf.high, colour = group), alpha = 0.5, colour = NA) + 
     xlab("Number of living sons") +
     ylab("Eye patch ratio") +
     theme_classic(base_size = 12) + 
     guides(color = guide_legend(title = "Age"), fill = guide_legend(title = "Age")) +
     scale_y_continuous(limits = c(1.05, 1.27)) +
     scale_x_continuous(breaks = 0:4) +
     scale_color_manual(values = custom_palette) +  # Set custom colors
     scale_fill_manual(values = custom_palette)    # Set custom fill colors
   
   
   # Now same for daughters model
   
   ggpred_m.4 <- ggeffects::ggpredict(m.4_noST, term = c("num.daughters[all]", "age"))
   
   raw_plot_m.4 <- ggplot(data = data2014_2023_adultfemales_noI63, aes(x = num.daughters, y = mean.ep, color = age)) +
     geom_point(size = 2, alpha = 0.8) +
     xlab("Number of living daughters") +
     ylab("Eye patch ratio") +
     theme_classic(base_size = 12) +
     scale_y_continuous(limits = c(1.05, 1.27)) +
     scale_x_continuous(breaks = 0:4) + 
     scale_color_viridis(option = "D") + 
     labs(color = "Age")
   
   
   #Convert to a dataframe and restrict age 16.9 to biologically plausible numbers!
   ggpred_m.4 <- as.data.frame(ggpred_m.4)
   ggpred_m.4 <- ggpred_m.4 |>
     dplyr::filter(!(group == "16.9" & x > 2))
   
   ggpred_m.4$group <- factor(ggpred_m.4$group, levels = rev(levels(ggpred_m.4$group)))
   
   ggpred_m.4_plot <- ggplot(data = ggpred_m.4, aes(x = x, y = predicted, colour = group, fill = group)) +
     geom_smooth(method = lm, se = FALSE) +
     geom_ribbon(aes(ymin = conf.low, ymax = conf.high, colour = group), alpha = 0.5, colour = NA) + 
     xlab("Number of living daughters") +
     ylab("Eye patch ratio") +
     theme_classic(base_size = 12) + 
     guides(color = guide_legend(title = "Age"), fill = guide_legend(title = "Age")) +
     scale_y_continuous(limits = c(1.05, 1.27)) +
     scale_x_continuous(breaks = 0:4) +
     scale_color_manual(values = custom_palette) +  # Set custom colors
     scale_fill_manual(values = custom_palette)    # Set custom fill colors
   
   ggpred_m.3_plot + ggpred_m.4_plot + raw_plot_m.3 + raw_plot_m.4
   
   

### Evaluate the costs of sons and daughters relative the number of total living offspring ###
   
   # This is a supplementary analysis  to validate our modelling approach for assessing the costs of sons and daughters
   #by adding in the number of living offspring as a covariate in models 3 and 4. 
   
   
   ### supplementary model set #####
   
   
   null.model <- glmmTMB(mean.ep ~ 1 + 
                           + (1| matriline) + (1| whale.id),
                         data = data2014_2023_ST_adultfemales_noI63, REML = FALSE)
   
   m.1 <- glmmTMB(mean.ep ~ pregnancy.simple + num.offspring*ns(age, df =2) + num.offspring*salmon +
                    + (1| matriline) + (1| whale.id) ,
                  data = data2014_2023_ST_adultfemales_noI63, REML = FALSE)
   
   m.2 <- glmmTMB(mean.ep ~ pregnancy.simple + num.births*ns(age, df = 2) + num.births*salmon +
                    + (1| matriline) + (1| whale.id) ,
                  data = data2014_2023_ST_adultfemales_noI63, REML = FALSE)
   
   
   m.3 <- glmmTMB(mean.ep ~ pregnancy.simple + num.sons*ns(age, df = 2) + num.sons*salmon 
                  + num.offspring
                  + (1| matriline) + (1| whale.id) ,
                  data = data2014_2023_ST_adultfemales_noI63, REML = FALSE)
   
   m.4 <- glmmTMB(mean.ep ~ pregnancy.simple + num.daughters*ns(age, df = 2) + num.daughters*salmon
                  + num.offspring
                  + (1| matriline) + (1| whale.id) ,
                  data = data2014_2023_ST_adultfemales_noI63, REML = FALSE)
   
   
   
   
   # re run diagnotsics 
   
   res.top.model <- simulateResiduals(fittedModel = m.3) # change out for the model I am interested in
   plot(res.top.model)
   
   
   ### MOdel seletion
   ## make list of models
   
   Cand.models.adultfemales <- list(m.1,
                                    m.2,
                                    m.3,
                                    m.4,
                                    null.model)
   
   ## build a model selection table
   
   names <- c("m.1", "m.2", "m.3", "m.4", "null") # List your model names accordingly
   
   selectionTable.adultfemales <- aictab(cand.set = Cand.models.adultfemales, modnames = names)
   selectionTable.adultfemales
   
   summary(m.1)
   summary(m.2)
   summary(m.3)
   summary(m.4)
   
   # R squared 
   
   performance::r2(m.1)
   performance::r2(m.2)
   performance::r2(m.3)
   performance::r2(m.4)
   
   
   
   
   

##### MAKE A SUMMARY TABLE FOR ADULT FEMALES AND THEIR REPRO HISTORY #########
   
   # Need female ID, age, number of births, number of offspring that died, number of offspring that survived
   # number of daughters, number of sons. 
   
   summary_table <- data2014_2023_adultfemales %>%
     group_by(whale.id) %>%
     summarise(
       total_births = max(num.births, na.rm = TRUE), # finds highest number of births for any year for a certain whale
       total_alive_offspring = num.offspring[which.max(year)],# finds the index of the latest year for that whale, and then you extract the num.offspring from that row
       total_dead_offspring = total_births - total_alive_offspring,
       total_alive_sons = num.sons[which.max(year)],
       total_alive_daughters = num.daughters[which.max(year)],
       total_alive_unknown = total_alive_offspring - (total_alive_sons + total_alive_daughters)
     )
   
  
   

   
  
  