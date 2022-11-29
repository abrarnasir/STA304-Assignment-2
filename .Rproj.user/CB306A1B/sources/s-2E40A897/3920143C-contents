library(tidyverse)
library(car)
library(flexmix)
library(pscl)
library(patchwork)

#census_data <- read_csv("gss_clean.csv")
survey_data <- read_csv("ces2019-phone_clean.csv")

#Response variable: Q11
#Predictor variable: p3, p4, age, q3: gender, age_range, q10, q31, q77, q54, q68, q33, q61, q69, q63, q27_a, q27_b, q27_c, q27_d, q27_e, p35_a, p35_b, p35_c, p26

#survey_data <- survey_data %>% mutate(age = 2019-q2, vote_liberal = ifelse(q11==1, 1, 0)) %>% select(age, vote_liberal)
#census_data <- census_data %>% mutate(age=round(age)) %>% select(age)

vars <- c("q11", "age_range", "q3", "q10", "q27_a", "q27_b", "q27_c", "q27_d", "q27_e", "q31", "q33", "q54", "q61", "q63", "q68",  "q69", "q71", "p3", "p4", "p26")
survey_data <- survey_data[vars]
survey_data <- drop_na(survey_data)

survey_data$q11 <- ifelse(survey_data$q11 == 2, 1, 0)

survey_data <- survey_data %>% 
  rename(
    party_to_vote_for = q11,
    gender = q3,
    voting_probability = q10,
    education_spending = q27_a,
    environment_spending = q27_b,
    crime_spending = q27_c,
    defence_spending = q27_d,
    immigrants_spending = q27_e,
    relative_economic_performance = q31,
    best_party_economic_performance = q33,
    provincial_party_performance = q54,
    education_level = q61,
    religion_importance = q63,
    employment_status = q68,
    household_income = q69,
    members_household = q71,
    previous_voted_candidate = p3,
    democracy_satisfaction = p4,
    corruption = p26
  )

hist1 <- ggplot(survey_data, aes(x = party_to_vote_for)) + geom_histogram()
hist2 <- ggplot(survey_data, aes(x = age_range)) + geom_histogram()
hist3 <- ggplot(survey_data, aes(x = gender)) + geom_histogram()
hist4 <- ggplot(survey_data, aes(x = voting_probability)) + geom_histogram()
hist5 <- ggplot(survey_data, aes(x = education_spending)) + geom_histogram()
hist6 <- ggplot(survey_data, aes(x = environment_spending)) + geom_histogram()
hist7 <- ggplot(survey_data, aes(x = crime_spending)) + geom_histogram()
hist8 <- ggplot(survey_data, aes(x = defence_spending)) + geom_histogram()
hist9 <- ggplot(survey_data, aes(x = immigrants_spending)) + geom_histogram()
hist10 <- ggplot(survey_data, aes(x = relative_economic_performance)) + geom_histogram()
hist11 <- ggplot(survey_data, aes(x = best_party_economic_performance)) + geom_histogram()
hist12 <- ggplot(survey_data, aes(x = provincial_party_performance)) + geom_histogram()
hist13 <- ggplot(survey_data, aes(x = education_level)) + geom_histogram()
hist14 <- ggplot(survey_data, aes(x = religion_importance)) + geom_histogram()
hist15 <- ggplot(survey_data, aes(x = employment_status)) + geom_histogram()
hist16 <- ggplot(survey_data, aes(x = household_income)) + geom_histogram()
hist17 <- ggplot(survey_data, aes(x = members_household)) + geom_histogram()
hist18 <- ggplot(survey_data, aes(x = previous_voted_candidate)) + geom_histogram()
hist19 <- ggplot(survey_data, aes(x = democracy_satisfaction)) + geom_histogram()
hist20 <- ggplot(survey_data, aes(x = corruption)) + geom_histogram()

hist1 + hist2 + hist3 + hist4 + hist5 + hist6 + hist7 + hist8 + hist9 + hist10 + hist11 + hist12 + hist13 + hist14 + hist15 + hist16 + hist17 + hist18 + hist19 + hist20 + plot_layout(nrow = 5, byrow = FALSE) + plot_annotation(title = 'Histograms of Variables')

#dev.off() 
#pairs(survey_data[, -c(1)])

model0 <- glm(party_to_vote_for ~ gender +
                age_range + 
                voting_probability +
                education_spending +
                environment_spending +
                crime_spending +
                defence_spending +
                immigrants_spending +
                relative_economic_performance +
                best_party_economic_performance +
                provincial_party_performance +
                education_level +
                religion_importance +
                employment_status +
                household_income +
                members_household +
                previous_voted_candidate +
                democracy_satisfaction +
                corruption,
              data = survey_data, family = 'binomial')
summary(model0)

model1 <- glm(party_to_vote_for ~ environment_spending +
                best_party_economic_performance +
                education_level +
                religion_importance +
                employment_status +
                household_income +
                members_household +
                previous_voted_candidate,
              data = survey_data, family = 'binomial')
summary(model1)

BIC(model0)
BIC(model1)

pR2(model0)['McFadden']
pR2(model1)['McFadden']
