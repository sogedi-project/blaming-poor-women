# 0. Identification ---------------------------------------------------

# R code to reproduce analysis presented in the paper:
# Sexualized Stereotypes of Low-SES Women and Their Role in Supporting Paternalistic and Hostile Classism Policies - An Experimental Study
# Written by: Mario Sainz 
# e-mail: msainz@psi.uned.es
# Please email us if you see any errors or have any questions
# Last update: 22/09/2025

# 1. Packages  -----------------------------------------------------

if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse,
               sjmisc, 
               sjPlot,
               here,
               lavaan,
               effectsize,
               sjlabelled,
               data.table,
               psych,
               gtools,
               jmv,
               semTools, 
               haven,
               rstatix)

options(scipen=999)
rm(list = ls())

# 2. Data -----------------------------------------------------------------

db <- read_sav(here("input/data/original/Study_2_low-ses women.sav")) # Reading data from the SPSS file

glimpse(db)

# 3. Preparation ----------------------------------------------------------

# Factors
db$sex <- factor(db$sex1)       # 1 = women, 2 = Men, 3 = Other.
db$promis <- factor(db$promis)  # Study 2a: high vs. low perceived promiscuity. Entonces 0 = high, 1 = low
db$risk <- factor(db$risk)      # Study 2b: high vs. low risky sexual behavior.Entonces 0 = high, 1 = low
db$pregna <- factor(db$pregna)  # Study 2c: high vs. low perceived risk of teenage motherhood.Entonces 0 = high, 1 = low
#db&quintiles                    # CREAR ESTO

#ANDREAS AÑADE LO QUE NECESITEMOS PARA PROCESAR LA BASE DE DATOS, GRACIAS.

db <- db %>%
  filter(filter == 0) # Excluding participants that fail the MC or does not meet the inclusion criteria.

db <- db %>% 
  select(
    Response_ID,
    Country_Code,
    Region,
    Language,
    Country,
    coun,
    natio,
    langu,
    promis,
    pregna,
    risk,
    mc_promi,
    mc_risk,
    mc_emba,
    vd1abuse,
    vd2abuse,
    vd3abuse,
    vd4abuse,
    vd5pate,
    vd6pate,
    vd7pate,
    vd8pate,
    vd9hosti,
    vd10hosti,
    vd11hosti,
    vd12hosti,
    classism1pate,
    classism2pate,
    classism3pate,
    classism4pate,
    classism5ccd,
    classism6ccd,
    classism7ccd,
    classism8ccd,
    classism9hosti,
    classism10hosti,
    classism11hosti,
    classism12hosti,
    age,
    sex,
    edu,
    ses,
    curren,
    income,
    person) 

# income
frq(db$curren)
frq(db$income)
frq(db$person)

db$n_person <- if_else(db$person > 11, NA, db$person)

db <- db %>% 
  mutate(exchange_rates = case_when(Country == "Chile" ~ 944.457))

db$equiv_income_local <- db$income / (1 + 0.5 * (db$n_person - 1))

db$equiv_income_usd <- (db$equiv_income_local / db$exchange_rates)

db <- db %>% 
  mutate(income_quintile = ntile(-desc(equiv_income_usd), n = 5))

frq(db$income_quintile)

db$income_quintile <- if_else(is.na(db$income_quintile), 6, db$income_quintile)

db$income_quintile <- factor(db$income_quintile,
                             levels = c(1, 2, 3, 4, 5, 6),
                             labels = c('Q1', 'Q2', 'Q3', 'Q4', 'Q5', "NA"))

frq(db$income_quintile)

# Se elimina caso sex == 3 por ser solo 1 en toda la bbdd y puede distorsionar analisis

db <- db %>% filter(sex != 3)

db$sex <- factor(db$sex)

# Crear dummies excluyendo la primera categoría de cada uno
income_dummies <- model.matrix(~ income_quintile, data = db)[, -1]

# Añadir al dataframe
db <- cbind(db, income_dummies)

frq(db$income_quintile)

db$income_quintile <- if_else(db$income_quintile == "NA", NA, db$income_quintile)

db$income_quintile <- factor(db$income_quintile,
                             levels = c('Q1', 'Q2', 'Q3', 'Q4', 'Q5'))

db <- db %>% select(-income_quintileNA)

income_dummies <- income_dummies[,-5]

income_vars <- colnames(income_dummies)

# education
frq(db$edu)

db$edu_dic <- if_else(db$edu > 4, "Universitary education or more", "Less than Universitary")
db$edu_dic <- factor(db$edu_dic, levels = c("Less than Universitary", "Universitary education or more"))

frq(db$edu_dic)


# 4. Analysis --------------------------------------------------------------

db %>% # Descriptives age
  summarise(
    media_edad = mean(age, na.rm = TRUE),
    sd_edad = sd(age, na.rm = TRUE),
    n = n()
  )

db %>% # Frequencies language (All spanish speakers)
  count(langu) %>%
  mutate(porcentaje = n / sum(n) * 100)

db %>% # Frequencies country (All from Chile)
  count(Country) %>%
  mutate(porcentaje = n / sum(n) * 100)

db %>% # Frequencies participants sex
  count(sex) %>%
  mutate(porcentaje = n / sum(n) * 100)

#Perceived abuse of social support for low-SES women:
db <- cbind(db, "abuse_low_SES" = rowMeans(db %>% select(
                                                    vd1abuse,
                                                    vd2abuse,
                                                    vd3abuse,
                                                    vd4abuse), na.rm=TRUE))
abuse_low_SES_alpha <- alpha(na.omit(db %>% select(vd1abuse, vd2abuse, vd3abuse, vd4abuse)))
abuse_low_SES_alpha$total[2]

#Support for paternalistic measures:
db <- cbind(db, "paternalistic_low_SES" = rowMeans(db %>% select(
                                                            vd5pate,
                                                            vd6pate,
                                                            vd7pate,
                                                            vd8pate), na.rm=TRUE))
pate_low_SES_alpha <- alpha(na.omit(db %>% select(vd5pate, vd6pate, vd7pate, vd8pate)))
pate_low_SES_alpha$total[2]

#Support for hostility measures:
db <- cbind(db, "hostile_low_SES" = rowMeans(db %>% select(
                                                      vd9hosti,
                                                      vd10hosti,
                                                      vd11hosti,
                                                      vd12hosti), na.rm=TRUE))
hosti_low_SES_alpha <- alpha(na.omit(db %>% select(vd9hosti, vd10hosti, vd11hosti, vd12hosti)))
hosti_low_SES_alpha$total[2]

#Participants paternalistic classism:
db <- cbind(db, "paterna_classism" = rowMeans(db %>% select(
                                                    classism1pate,
                                                    classism2pate,
                                                    classism3pate,
                                                    classism4pate), na.rm=TRUE))
pater_classism_alpha <- alpha(na.omit(db %>% select(classism1pate, classism2pate, classism3pate, classism4pate)))
pater_classism_alpha$total[2]

#Participants hostile classism:
db <- cbind(db, "hostile_classism" = rowMeans(db %>% select(
                                                    classism9hosti,
                                                    classism10hosti,
                                                    classism11hosti,
                                                    classism12hosti), na.rm=TRUE))
hosti_classism_alpha <- alpha(na.omit(db %>% select(classism9hosti, classism10hosti, classism11hosti, classism12hosti)))
hosti_classism_alpha$total[2]

# Manipulation checks:

db %>% # MC for Study 2a
  group_by(promis) %>%
  get_summary_stats(mc_promi, type = "mean_sd")
t.test(mc_promi ~ promis, data = db, var.equal = TRUE) 
db %>% cohens_d(mc_promi ~ promis, var.equal = TRUE)

db %>% # MC for Study 2b
  group_by(risk) %>%
  get_summary_stats(mc_risk, type = "mean_sd")
t.test(mc_risk ~ risk, data = db, var.equal = TRUE) 
db %>% cohens_d(mc_risk ~ risk, var.equal = TRUE)

db %>% # MC for Study 2c
  group_by(pregna) %>%
  get_summary_stats(mc_emba, type = "mean_sd")
t.test(mc_emba ~ pregna, data = db, var.equal = TRUE) 
db %>% cohens_d(mc_emba ~ pregna, var.equal = TRUE)

# Selecting the final variables:

db <- db %>% 
  select(
    vd1abuse,
    vd2abuse,
    vd3abuse,
    vd4abuse,
    vd5pate,
    vd6pate,
    vd7pate,
    vd8pate,
    vd9hosti,
    vd10hosti,
    vd11hosti,
    vd12hosti,
    classism1pate,
    classism2pate,
    classism3pate,
    classism4pate,
    classism9hosti,
    classism10hosti,
    classism11hosti,
    classism12hosti,
    promis,
    pregna,
    risk,
    mc_promi,
    mc_emba,
    mc_risk,
    abuse_low_SES,
    paternalistic_low_SES,
    hostile_low_SES,
    paterna_classism,
    hostile_classism,
    age,
    sex,
    edu_dic,
    ses,
    income_quintile,
    income_quintileQ2,
    income_quintileQ3,
    income_quintileQ4,
    income_quintileQ5
    ) %>% 
  remove_all_labels() %>% 
  mutate_all(~as.numeric(.)) %>% 
  as_tibble()


# Means and SDs per condition:

table1 <- db %>% select(
  promis,
  pregna,
  risk,
  abuse_low_SES,
  paternalistic_low_SES,
  hostile_low_SES) %>%
  mutate(
    promis = as.factor(promis),
    pregna = as.factor(pregna),
    risk = as.factor(risk))

tabla_resumen <- table1 %>% 
  pivot_longer(cols = c(promis, pregna, risk),
               names_to = "dimension", 
               values_to = "grupo") %>% 
  filter(!is.na(grupo)) %>% 
  group_by(dimension, grupo) %>% 
  summarise(
    abuse_mean = mean(abuse_low_SES, na.rm = TRUE),
    abuse_sd   = sd(abuse_low_SES, na.rm = TRUE),
    paternalistic_mean = mean(paternalistic_low_SES, na.rm = TRUE),
    paternalistic_sd   = sd(paternalistic_low_SES, na.rm = TRUE),
    hostile_mean = mean(hostile_low_SES, na.rm = TRUE),
    hostile_sd   = sd(hostile_low_SES, na.rm = TRUE),
    .groups = "drop"
  )

tabla_resumen

# ANCOVA analysis  ---- AQUÍ DEBERIAMOS HACER LO MISMO QUE EN EL ESTUDIO RPEVIO PERO PARA ESTOS TRES ESTUDIOS.

glimpse(db)

df_anova_pro <- db %>% 
  as_tibble() %>% 
  filter(!is.na(promis)) %>% 
  select(promis, mc_promi, age, edu_dic, ses, sex, income_quintile) %>% 
  mutate(promis = if_else(promis == 1, "High", "Low"))

df_anova_pro$promis <- factor(df_anova_pro$promis, 
                          levels = c("High", "Low"))

# Modelo ANCOVA para 'pro'
ancova_pro <- lm(mc_promi ~ promis +
                 age + edu_dic + ses + sex + income_quintile +
                 promis:age + promis:edu_dic + promis:ses + promis:sex + 
                 promis:income_quintile,
                 data = df_anova_pro)
summary(ancova_pro)
anova(ancova_pro)

df_anova_pre <- db %>% 
  as_tibble() %>% 
  filter(!is.na(pregna)) %>% 
  select(pregna, mc_emba, age, edu_dic, ses, sex, income_quintile) %>% 
  mutate(pregna = if_else(pregna == 1, "High", "Low"))

df_anova_pre$pregna <- factor(df_anova_pre$pregna, 
                              levels = c("High", "Low"))

# Modelo ANCOVA para 'pre'
ancova_pre <- lm(mc_emba ~ pregna +
                   age + edu_dic + ses + sex + income_quintile +
                   pregna:age + pregna:edu_dic + pregna:ses + pregna:sex + 
                   pregna:income_quintile,
                 data = df_anova_pre)
summary(ancova_pre)
anova(ancova_pre)

df_anova_ris <- db %>% 
  as_tibble() %>% 
  filter(!is.na(risk)) %>% 
  select(risk, mc_risk, age, edu_dic, ses, sex, income_quintile) %>% 
  mutate(risk = if_else(risk == 1, "High", "Low"))

df_anova_ris$risk <- factor(df_anova_ris$risk, 
                            levels = c("High", "Low"))

# Modelo ANCOVA para 'risk'
ancova_ris <- lm(mc_risk ~ risk +
                   age + edu_dic + ses + sex + income_quintile +
                   risk:age + risk:edu_dic + risk:ses + risk:sex + 
                   risk:income_quintile,
                 data = df_anova_ris)
summary(ancova_ris)
anova(ancova_ris)

effectsize::eta_squared(ancova_pro, partial = TRUE)
effectsize::eta_squared(ancova_ris, partial = TRUE)
effectsize::eta_squared(ancova_pre, partial = TRUE)


# CFA models ----

model_cfa <- ('
  # Factores latentes
  abuse =~ vd1abuse + vd2abuse + vd3abuse + vd4abuse
  paternalistic =~ vd5pate + vd6pate + vd7pate + vd8pate
  hostile =~  vd9hosti + vd10hosti + vd11hosti + vd12hosti
')

# Estimación 
subset(db, select = c(vd1abuse,
                      vd2abuse,
                      vd3abuse,
                      vd4abuse,
                      vd5pate,
                      vd6pate,
                      vd7pate,
                      vd8pate,
                      vd9hosti,
                      vd10hosti,
                      vd11hosti,
                      vd12hosti
                      )) %>% 
  as_numeric() %>% 
  mardia(na.rm = TRUE, plot=TRUE)

fit_cfa <- lavaan::cfa(model_cfa, 
               data = db, 
               estimator = "MLR")

summary(fit_cfa, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

fitmeasures(fit_cfa, c("chisq", "pvalue", "df", "cfi", "tli", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr"))

#### 3.3 SEM model for the different studies ####

db <- db %>% 
  mutate(
    across(
      .cols = c(promis, pregna, risk),
      .fns = ~ factor(., levels = c(1,2), labels = c("High", "Low"))
    )
  )

# model structure
model_base <- ('
  # Latent factors
  abuse =~ vd1abuse + vd2abuse + vd3abuse + vd4abuse
  paternalistic =~ vd5pate + vd6pate + vd7pate + vd8pate
  hostile =~ vd9hosti + vd10hosti + vd11hosti + vd12hosti

  # Main effects de la condición
  abuse ~ a1*COND
  paternalistic ~ b1*COND
  hostile ~ c1*COND

  # Mediación
  paternalistic ~ m1*abuse
  hostile ~ m2*abuse

  # Correlación entre factores dependientes
  paternalistic ~~ hostile

  # Indirectos (nombrados con la condición)
  ind_paternalistic_COND := a1*m1
  ind_hostile_COND := a1*m2
')

controls <- c("age", "edu_dic", "ses", "sex", income_vars)

control_formula <- paste(controls, collapse = " + ")

control_block <- paste0(
  "abuse ~ ", control_formula, "\n",
  "paternalistic ~ ", control_formula, "\n",
  "hostile ~ ", control_formula, "\n"
)

# estimation (loop) 

conds <- c("promis","pregna","risk")
res <- setNames(vector("list", length(conds)), conds)

for (cv in conds) {
  message("\n### SEM condición: ", cv, " ###")
  
  dsub <- db %>% filter(!is.na(.data[[cv]]))
  
  model_cv <- gsub("COND", cv, model_base)
  model_cv <- paste(model_cv, control_block)
  
  fit <- lavaan::sem(model_cv, data = dsub, estimator = "MLR")
  
  fits <- fitmeasures(fit, c("chisq","df","pvalue","cfi","tli","rmsea",
                             "rmsea.ci.lower","rmsea.ci.upper","srmr"))
  
  est <- parameterEstimates(fit, standardized = TRUE)
  
  direct <- subset(est, op == "~" & lhs %in% c("paternalistic","hostile") & rhs == cv,
                   select = c("lhs","rhs","std.all","pvalue"))
  names(direct)[3:4] <- c("est.direct","p.direct")
  
  ind_pat <- paste0("ind_paternalistic_", cv)
  ind_hos <- paste0("ind_hostile_", cv)
  indirect <- subset(est, op == ":=" & lhs %in% c(ind_pat, ind_hos),
                     select = c("lhs","std.all","pvalue","se","ci.lower","ci.upper"))
  indirect$outcome <- ifelse(grepl("paternalistic", indirect$lhs), "paternalistic","hostile")
  indirect$predictor <- cv
  names(indirect)[2:3] <- c("est.indirect","p.indirect")
  
  effects <- merge(
    direct[, c("lhs","rhs","est.direct","p.direct")],
    indirect[, c("outcome","predictor","est.indirect","p.indirect","se","ci.lower","ci.upper")],
    by.x = c("lhs","rhs"), by.y = c("outcome","predictor"), all.x = TRUE
  )
  effects$total <- effects$est.direct + effects$est.indirect
  names(effects)[1:2] <- c("outcome","predictor")
  
  res[[cv]] <- list(fit = fit, fit_meas = fits, effects = effects)
}

summary(res$promis$fit, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
summary(res$pregna$fit, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
summary(res$risk$fit, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

res$promis$fit_meas
res$pregna$fit_meas
res$risk$fit_meas

# 4. Save and export  ----------------------------------------------------------------

save(db,
     tabla_resumen,
     ancova_pre,
     ancova_pro, 
     ancova_ris,
     fit_cfa,
     res,
     file = here("input/data/proc/models_study2.RData"))
