library(tidyverse)
library(car)
library(flexmix)
library(pscl)
library(patchwork)

survey_data <- read_csv("ces2019-phone_clean.csv")
survey_data$q11 <- ifelse(survey_data$q11 == 1, 1, 0)
survey_data <- survey_data[, -c(1:21)]
survey_data <- survey_data[,colSums(is.na(survey_data)) < nrow(survey_data)]
survey_data <- survey_data %>% dplyr::select(where(is.numeric))
survey_data <- drop_na(survey_data)

model0_lib <- glm(q11 ~., data = survey_data, family = 'binomial')
summary(model0_lib)