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
    sex1,
    edu,
    ses,
    curren,
    income,
    person) 

# sex

db <- db %>% 
  mutate(sex = if_else(sex1 == 3, NA, sex1),
         sex = factor(sex, 
                      levels = (1:2), 
                      labels = c("Mujer" ,"Hombre")))

frq(db$sex)

# Tratamientos

db %>% 
  select(promis, risk, pregna) %>% 
  frq()

db <- db %>% 
  mutate(
    across(
      .cols = c(promis, risk, pregna),
      .fns = ~ factor(.,
                      levels = c(0:1),
                      labels = c("Low", "High")) 
    )
  )

frq(db$promis)

#db$promis <- factor(db$promis)  # Study 2a: high vs. low perceived promiscuity. Entonces 0 = high, 1 = low
#db$risk <- factor(db$risk)      # Study 2b: high vs. low risky sexual behavior.Entonces 0 = high, 1 = low
#db$pregna <- factor(db$pregna)  # Study 2c: high vs. low perceived risk of teenage motherhood.Entonces 0 = high, 1 = low
#db&quintiles                    # CREAR ESTO

#ANDREAS AÑADE LO QUE NECESITEMOS PARA PROCESAR LA BASE DE DATOS, GRACIAS.

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
  filter(!is.na(promis)) %>% 
  group_by(promis) %>%
  get_summary_stats(mc_promi, type = "mean_sd")
t.test(mc_promi ~ promis, data = db, var.equal = TRUE) 
db %>% cohens_d(mc_promi ~ promis, var.equal = TRUE)

db %>% # MC for Study 2b
  filter(!is.na(risk)) %>% 
  group_by(risk) %>%
  get_summary_stats(mc_risk, type = "mean_sd")
t.test(mc_risk ~ risk, data = db, var.equal = TRUE) 
db %>% cohens_d(mc_risk ~ risk, var.equal = TRUE)

db %>% # MC for Study 2c
  filter(!is.na(pregna)) %>% 
  group_by(pregna) %>%
  get_summary_stats(mc_emba, type = "mean_sd")
t.test(mc_emba ~ pregna, data = db, var.equal = TRUE) 
db %>% cohens_d(mc_emba ~ pregna, var.equal = TRUE)

# Selecting the final variables:

db <- db %>% 
  select(
    Response_ID,
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
  as_tibble()


# Means and SDs per condition:

table1 <- db %>% select(
  promis,
  pregna,
  risk,
  abuse_low_SES,
  paternalistic_low_SES,
  hostile_low_SES)

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
  select(promis, mc_promi, age, edu_dic, ses, sex, income_quintile) 

levels(df_anova_pro$promis)

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
  select(pregna, mc_emba, age, edu_dic, ses, sex, income_quintile) 

levels(df_anova_pre$pregna)

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
  select(risk, mc_risk, age, edu_dic, ses, sex, income_quintile) 

levels(df_anova_ris$risk)

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

df_pre <- db %>% filter(!is.na(pregna))
df_pro <- db %>% filter(!is.na(promis))
df_ris <- db %>% filter(!is.na(risk))

model_cfa <- ('
  # Factores latentes
  abuse =~ vd1abuse + vd2abuse + vd3abuse + vd4abuse
  paternalistic =~ vd5pate + vd6pate + vd7pate + vd8pate
  hostile =~  vd9hosti + vd10hosti + vd11hosti + vd12hosti
')



datas <- list(df_pro = df_pro, df_pre = df_pre, df_ris = df_ris)

cfa_per_condi <- function(d, etiqueta) {
  message("\n##### CFA ", etiqueta, " #####")
  
  fit_cfa <- lavaan::cfa(model_cfa, 
                         data = d, 
                         estimator = "MLR")
  
  print(summary(fit_cfa, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE))
  
  fit_meas <- fitmeasures(fit_cfa, c("chisq", "pvalue", "df", "cfi", "tli", 
                                     "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr"))
  
  return(list(
    condicion = etiqueta,
    fit = fit_cfa,
    fit_meas = fit_meas
  ))
}

resultados_cfa <- imap(datas, cfa_per_condi)

resultados_cfa$df_pro$fit_meas
resultados_cfa$df_pre$fit_meas
resultados_cfa$df_ris$fit_meas

#### 3.3 SEM model for the different studies ####

# model structure
model_base <- ('
  # Latent factors
  abuse =~ vd1abuse + vd2abuse + vd3abuse + vd4abuse
  paternalistic =~ vd5pate + vd6pate + vd7pate + vd8pate
  hostile =~ vd9hosti + vd10hosti + vd11hosti + vd12hosti
  
  # Modification indices
  vd5pate ~~   vd6pate
  vd3abuse ~~  vd4abuse
  vd11hosti ~~ vd12hosti
  vd9hosti ~~ vd10hosti
  vd10hosti ~~ vd12hosti
  vd1abuse ~~  vd2abuse
  vd9hosti ~~ vd12hosti
  vd10hosti ~~ vd11hosti
  vd9hosti ~~ vd11hosti

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
  ind_hostile_COND       := a1*m2
  
  # Totales (directo + indirecto)
  tot_paternalistic_COND := b1 + a1*m1
  tot_hostile_COND       := c1 + a1*m2
')

controls <- c("age", "edu_dic", "ses", "sex", income_vars)
control_formula <- paste(controls, collapse = " + ")
control_block <- paste0(
  "abuse ~ ", control_formula, "\n",
  "paternalistic ~ ", control_formula, "\n",
  "hostile ~ ", control_formula, "\n"
)

# estimation (loop) 
conds <- c("promis","pregna","risk")  # <- corregido (antes estaba 'onds')
res <- setNames(vector("list", length(conds)), conds)

for (cv in conds) {
  cat("\n### SEM condición:", cv, "###\n")
  
  dsub <- db[!is.na(db[[cv]]), , drop = FALSE]
  
  model_cv <- gsub("COND", cv, model_base)
  model_cv <- paste(model_cv, control_block)
  
  fit <- lavaan::sem(model_cv, data = dsub, estimator = "MLR")
  
  fits <- fitmeasures(fit, c("chisq","df","pvalue","cfi","tli","rmsea",
                             "rmsea.ci.lower","rmsea.ci.upper","srmr"))
  
  est <- parameterEstimates(fit, standardized = TRUE, ci = TRUE)
  
  ## --- DIRECTOS: (paternalistic|hostile) ~ cv ---
  direct <- est[est$op=="~" & est$lhs %in% c("paternalistic","hostile") & est$rhs==cv,
                c("lhs","rhs","est","std.all","pvalue","se","ci.lower","ci.upper"),
                drop = FALSE]
  names(direct) <- c("outcome","predictor","est.direct","std.direct","p.direct",
                     "se.direct","ci.lower.direct","ci.upper.direct")
  
  ## --- INDIRECTOS: labels tipo ind_paternalistic_cv e ind_hostile_cv ---
  ind_labels <- c(paste0("ind_paternalistic_", cv),
                  paste0("ind_hostile_", cv))
  indirect <- est[est$op==":=" & est$lhs %in% ind_labels,
                  c("lhs","std.all","est","pvalue","se","ci.lower","ci.upper"),
                  drop = FALSE]
  if (nrow(indirect) == 0L) {
    indirect <- data.frame(lhs=character(), std.all=numeric(), est=numeric(),
                           pvalue=numeric(), se=numeric(),
                           ci.lower=numeric(), ci.upper=numeric())
  }
  indirect$outcome   <- ifelse(grepl("paternalistic", indirect$lhs),
                               "paternalistic","hostile")
  indirect$predictor <- cv
  names(indirect)[names(indirect)=="std.all"]   <- "std.indirect"
  names(indirect)[names(indirect)=="est"]       <- "est.indirect"
  names(indirect)[names(indirect)=="pvalue"]    <- "p.indirect"
  names(indirect)[names(indirect)=="se"]        <- "se.indirect"
  names(indirect)[names(indirect)=="ci.lower"]  <- "ci.lower.indirect"
  names(indirect)[names(indirect)=="ci.upper"]  <- "ci.upper.indirect"
  indirect <- indirect[, c("outcome","predictor","est.indirect","std.indirect",
                           "p.indirect","se.indirect","ci.lower.indirect","ci.upper.indirect"),
                       drop = FALSE]
  
  ## --- TOTALES: labels tipo tot_paternalistic_cv y tot_hostile_cv (desde :=) ---
  tot_labels <- c(paste0("tot_paternalistic_", cv),
                  paste0("tot_hostile_", cv))
  total <- est[est$op==":=" & est$lhs %in% tot_labels,
               c("lhs","std.all","est","pvalue","se","ci.lower","ci.upper"),
               drop = FALSE]
  if (nrow(total) == 0L) {
    total <- data.frame(lhs=character(), std.all=numeric(), est=numeric(),
                        pvalue=numeric(), se=numeric(),
                        ci.lower=numeric(), ci.upper=numeric())
  }
  total$outcome   <- ifelse(grepl("paternalistic", total$lhs),
                            "paternalistic","hostile")
  total$predictor <- cv
  names(total)[names(total)=="std.all"]   <- "std.total"
  names(total)[names(total)=="est"]       <- "est.total"
  names(total)[names(total)=="pvalue"]    <- "p.total"
  names(total)[names(total)=="se"]        <- "se.total"
  names(total)[names(total)=="ci.lower"]  <- "ci.lower.total"
  names(total)[names(total)=="ci.upper"]  <- "ci.upper.total"
  total <- total[, c("outcome","predictor","est.total","std.total",
                     "p.total","se.total","ci.lower.total","ci.upper.total"),
                 drop = FALSE]
  
  ## --- Unir todo ---
  effects <- Reduce(function(x, y) merge(x, y, by = c("outcome","predictor"), all = TRUE),
                    list(direct, indirect, total))
  
  res[[cv]] <- list(fit = fit, fit_meas = fits, effects = effects)
}


summary(res$promis$fit, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
summary(res$risk$fit, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
summary(res$pregna$fit, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

res$promis$fit_meas
res$risk$fit_meas
res$pregna$fit_meas

res$promis$effects %>% mutate_at(.vars = 3:15, ~round(.,3))
res$risk$effects %>% mutate_at(.vars = 3:10, ~round(.,3))
res$pregna$effects %>% mutate_at(.vars = 3:10, ~round(.,3))

# 4. Save and export  ----------------------------------------------------------------

save(db,
     tabla_resumen,
     ancova_pre,
     ancova_pro, 
     ancova_ris,
     resultados_cfa,
     res,
     file = here("input/data/proc/models_study2.RData"))

library(writexl)



writexl::write_xlsx(
  bind_rows(
    res$promis$effects %>% 
      mutate(across(3:20, ~ round(., 3))) %>% 
      filter(outcome == "paternalistic") %>% 
      select(ends_with("total")) %>% 
      select(1, 4, 3, 5, 6, 2) %>% 
      mutate(
        p.total = case_when(
          p.total < 0.001 ~ "< .001",
          p.total < 0.05  ~ "< .05",
          TRUE               ~ sprintf("%.3f", p.total)
        )
      )
    ,
    res$risk$effects %>% 
      mutate(across(3:20, ~ round(., 3))) %>% 
      filter(outcome == "paternalistic") %>% 
      select(ends_with("total")) %>% 
      select(1, 4, 3, 5, 6, 2) %>% 
      mutate(
        p.total = case_when(
          p.total < 0.001 ~ "< .001",
          p.total < 0.05  ~ "< .05",
          TRUE               ~ sprintf("%.3f", p.total)
        )
      )
    ,
    res$pregna$effects %>% 
      mutate(across(3:20, ~ round(., 3))) %>% 
      filter(outcome == "paternalistic") %>% 
      select(ends_with("total")) %>% 
      select(1, 4, 3, 5, 6, 2) %>% 
      mutate(
        p.total = case_when(
          p.total < 0.001 ~ "< .001",
          p.total < 0.05  ~ "< .05",
          TRUE               ~ sprintf("%.3f", p.total)
        )
      )), path = "output/total_patern.xlsx")
