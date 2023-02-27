# Beliefs-and-Attitudes-towards-Environmental-Conservation-in-German-and-Iranian-students-A-cross-cul
# bring rtools
##############################
# install packages
install.packages("lme4")
install.packages("car")
install.packages("sjPlot")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("dplyr")

# loading the packages
library(lme4)
library(car)
library(sjPlot)
library(tidyverse)
library(ggplot2)
library(dplyr)
##############################
# set working directory
# read csv file
read.csv("data.csv")
original_data <- read.csv("data.csv")
names(original_data)
head(original_data)
##############################
library(tidyverse)
glimpse(original_data)
# converting ID to factor, factors have predefined levels.
original_data$ID <- as.factor(original_data$ID)
glimpse(original_data)
##############################
# data Wrangling
library(tidyr)
original_data
data_wide <- data.frame(original_data$ID, original_data$country, original_data$participantNR, original_data$gender, original_data$age, original_data$animals.moral, original_data$animals.emotion, original_data$animals.environment, original_data$animals.convenience, original_data$plants.moral, original_data$plants.emotion, original_data$plants.environment, original_data$plants.convenience, original_data$air.moral, original_data$air.emotion, original_data$air.environment, original_data$air.convenience, original_data$land.moral, original_data$land.emotion, original_data$land.environment, original_data$land.convenience)
data_wide
glimpse(data_wide)
head(data_wide)

# convert the data to long format using the 'gather' function
data_long <- gather(data_wide, condition, measurement, original_data.animals.moral:original_data.land.convenience, factor_key=TRUE)
data_long
print(data_long$condition)
##############################
# import extra columns for domains and justifications
additional_columns <- read.csv("additional columns - domains_and_justifications.csv")
head(additional_columns)

# Summarize data frames, data sets, regression models.
summary(additional_columns)

# add columns to dataframe
data_long$domains <- additional_columns$domain
data_long$justifications <- additional_columns$justification
view(data_long)
#############################
# creating the null model
library(lme4)
library(tidyverse)
library(tidyr)
library(lattice)
library(dplyr)

glmer(measurement ~ (1|original_data.ID), data=data_long, family="binomial")
nullmodel <- glmer(measurement ~ (1|original_data.ID), data=data_long, family="binomial")
summary(nullmodel)
#############################
# selecting all the Iranian participants from the country column
subset(data_long, original_data.country == "IR")
IR_participants <- subset(data_long, original_data.country == "IR")
view(IR_participants)

# justifications in Iranians
IR_participants_justification <- glmer(measurement ~ (1|original_data.ID) + justifications, data=IR_participants, family="binomial")
summary(IR_participants_justification)

# domains in Iranians
IR_participants_domains <- glmer(measurement ~ (1|original_data.ID) + domains, data=IR_participants, family="binomial")
summary(IR_participants_domains)

# all the German participants
subset(data_long, original_data.country == "DE")
de_participants <- subset(data_long, original_data.country == "DE")
view(de_participants)

#justifications in Germans
de_participants_justification <- glmer(measurement ~ (1|original_data.ID) + justifications, data=de_participants, family="binomial")
summary(de_participants_justification)

# domains in Germans
de_participants_domains <- glmer(measurement ~ (1|original_data.ID) + domains, data=de_participants, family="binomial")
summary(de_participants_domains)
################################################################
# gender
onlygender <- glmer(measurement ~ (1|original_data.ID) + original_data.gender, data=data_long, family="binomial")
summary(onlygender)

# age
onlyage <- glmer(measurement ~ (1|original_data.ID) + original_data.age, data=data_long, family="binomial")
summary(onlyage)

# domain
domain_model1 <- glmer(measurement ~ domains + (1|original_data.ID), data=data_long, family="binomial")
summary(domain_model1)

#Anova
anova(nullmodel, domain_model1)
# justification
justification_model1 <- glmer(measurement ~ justifications + (1|original_data.ID), data=data_long, family="binomial")
summary(justification_model1)
#Anova
anova(nullmodel, justification_model1)

# country
country_model <- glmer(measurement ~ original_data.country + (1|original_data.ID), data=data_long, family="binomial")
summary(country_model)
#Anova
anova(nullmodel, country_model)

# vif
library(car)
vif(justification_model1)
vif(country_model)
vif(condition_model1)

# interactions

# domains and justifications
interaction_model_1 <- glmer (measurement ~ domains*justifications + (1|original_data.ID), data=data_long, family="binomial")
summary(interaction_model_1)
# country interactions
interaction_model_2 <- glmer (measurement ~ domains*original_data.country + (1|original_data.ID), data=data_long, family="binomial")
summary(interaction_model_2)

interaction_model_3 <- glmer (measurement ~ justifications*original_data.country + (1|original_data.ID), data=data_long, family="binomial")
summary(interaction_model_3)

#Anova between Justification and country*justification
anova(justification_model1, interaction_model_3)
#Anova between country vs country*justification
anova(country_model, interaction_model_3)

anova(nullmodel,interaction_model_1)
#Anova null model with domains*country
anova(nullmodel,interaction_model_2)
#Anova null model with justifications*country
anova(nullmodel,interaction_model_3)
 
# domain*country and justification*country
anova(interaction_model_2, interaction_model_3)
# justification*country with domain*country
anova(interaction_model_3, interaction_model_2)

vif(interaction_model_4)

# odds ratio
exp(coef(summary(domain_model1))[,1])
exp(coef(summary(justification_model1))[,1])

exp(coef(summary(interaction_model_2))[,1])
exp(coef(summary(interaction_model_3))[,1])

# Crosstabulation and Barplot to give the frequency

# domains
table(data_long$measurement, data_long$domains)

# justifications
table(data_long$measurement, data_long$justifications)

# country
table(data_long$measurement, data_long$justifications, data_long$original_data.country)
table(data_long$measurement, data_long$domains, data_long$original_data.country)

barplot(table(data_long$measurement, data_long$domains), beside=T)
barplot(table(data_long$measurement, data_long$justifications), beside=T)

square_domains <- 2.836400
square_domains^2
# we got 8.045165


