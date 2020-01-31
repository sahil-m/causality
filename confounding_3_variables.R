# population generation ---------------------------------------------------
population_size = 10000

confounder_classes = c('Male', 'Female')
prob_confounder_classes = c(0.5, 0.5)
set.seed(123)
confounder = sample(confounder_classes, population_size, replace = TRUE, prob = prob_confounder_classes)
round(prop.table(table(confounder))*100)

cause_1_purely_from_treatment = recode(confounder, Male = 'Treatment', Female = 'No_Treatment')
round(prop.table(table(cause_1_purely_from_treatment))*100)

cause_1_classes = c('Treatment', 'No_Treatment')
cause_1 = as.character()
for (i in 1:population_size) {
  if (confounder[i] == 'Male')
    cause_1 = c(cause_1, sample(cause_1_classes, 1, prob=c(.34, .66))) else
      cause_1 = c(cause_1, sample(cause_1_classes, 1, prob=c(.66, .34)))
}
round(prop.table(table(confounder, cause_1), 1)*100)


effect_classes = c('Heart_Attack', 'No_Heart_Attack')
effect = as.character()
for (i in 1:population_size) {
  if (confounder[i] == 'Male') {
    if (cause_1[i] == 'Treatment')
      effect = c(effect, sample(effect_classes, 1, prob=c(.4, .6))) else
        effect = c(effect, sample(effect_classes, 1, prob=c(.3, .7)))
  } else {
    if (cause_1[i] == 'Treatment')
      effect = c(effect, sample(effect_classes, 1, prob=c(.075, .925))) else
        effect = c(effect, sample(effect_classes, 1, prob=c(.05, .95)))
  }
}

round(prop.table(table(cause_1, effect, confounder), c(1,3))*100)

data_pop <- data.frame(confounder = confounder, cause_1 = cause_1, effect = recode(effect, Heart_Attack = 1, No_Heart_Attack = 0)) 

data_pop %>% 
  group_by(confounder, cause_1) %>% 
  summarise(total_patients = n(),
            heart_attack = sum(effect),
            no_heart_attack = total_patients - heart_attack) %>% 
  mutate(heart_attack_percent = round(heart_attack/total_patients*100, 2)) %>% 
  dplyr::select(gender = confounder, treatment = cause_1, heart_attack, no_heart_attack, total_patients, heart_attack_percent) %>% 
  pivot_wider(names_from = treatment, values_from = c(heart_attack, no_heart_attack, total_patients, heart_attack_percent))
  

## actual impact of cause_1 on effect
# Male -> Treatment -> 38%
# Female -> Treatment -> 7%
# Proportion of male and female -> (49, 51)
# So, Treatment -> (.49 * 38) + (.51 * 7) -> 22.19
# 
# Male -> No_Treatment -> 29%
# Female -> No_Treatment -> 5%
# Proportion of male and female -> (49, 51)
# So, No_Treatment -> (.49 * 29) + (.51 * 5) -> 16.76
# 
# i.e. treatment should be directly propotional to heart attack


# regression on population ------------------------------------------------
contrasts(data_pop$cause_1)
contrasts(data_pop$confounder)

reg_model_noConfounder <- lm(effect ~ cause_1, data = data_pop)
summary(reg_model_noConfounder)
# inversely proportional

reg_model_withConfounder <- lm(effect ~ cause_1 + confounder, data = data_pop)
summary(reg_model_withConfounder)
# directly proportional -> 0.06

## Which model to believe?

data_pop_male = dplyr::filter(data_pop, confounder == 'Male')
reg_model_noConfounder_male <- lm(effect ~ cause_1, data = data_pop_male)
summary(reg_model_noConfounder_male)
# coeff -> 0.0945

data_pop_female = dplyr::filter(data_pop, confounder == 'Female')
reg_model_noConfounder_female <- lm(effect ~ cause_1, data = data_pop_female)
summary(reg_model_noConfounder_female)
# coeff -> 0.02639

# avg -> (.49 * 0.0945) + (.51 * 0.02639) -> 0.05976 -> almost same as when done with the confounder



# tree on population ------------------------------------------------------
data_pop <- data.frame(confounder = confounder, cause_1 = cause_1, effect = effect, stringsAsFactors = TRUE) 

cart_model_noConfounder <- rpart(effect ~ cause_1, data = data_pop)
print(cart_model_noConfounder, digits = 2)
prp(cart_model_noConfounder)

cart_model_withConfounder <- rpart(effect ~ cause_1 + confounder, data = data_pop, method = 'class', control = rpart.control(cp = .000001))
prp(cart_model_withConfounder)



# sample generation -------------------------------------------------------
sample_prop = 0.3
sample_size = round(sample_prop * population_size)



