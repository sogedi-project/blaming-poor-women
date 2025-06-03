# 0. Identification ---------------------------------------------------

# Title: Data analysis for research paper on Blaming Poor Women
# Institution: Centro de Estudios de Conflicto y Cohesión Social (COES)
# Responsible: Researcher

# Executive Summary: This script contains the code to data preparation for Contact and Classism
# Date: February 6, 2025

# 1. Packages  -----------------------------------------------------

if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse,
               sjmisc, 
               sjPlot,
               here,
               lavaan,
               semTools,
               sjlabelled,
               data.table,
               psych,
               rstatix,
               easystats,
               summarytools)

options(scipen=999)
rm(list = ls())

# 2. Data -----------------------------------------------------------------

load(here("input/data/proc/df_study2.RData"))

glimpse(df_study2)


# Asegurar factores
df_study2$sex <- factor(df_study2$sex)
df_study2$income_quintile <- factor(df_study2$income_quintile)
df_study2$country <- factor(df_study2$country)

# Crear dummies excluyendo la primera categoría de cada uno
sex_dummies <- model.matrix(~ sex, data = df_study2)[, -1]
income_dummies <- model.matrix(~ income_quintile, data = df_study2)[, -1]
country_dummies <- model.matrix(~ country, data = df_study2)[, -1]

# Añadir al dataframe
df_study2 <- cbind(df_study2, sex_dummies, income_dummies, country_dummies)

sex_vars <- colnames(sex_dummies)
income_vars <- colnames(income_dummies)
country_vars <- colnames(country_dummies)

# 3. Analysis --------------------------------------------------------------

#### 3.1 Descriptive ####

#### 3.1.1 All countries ####

df_study2 %>% 
  select(starts_with(c("pro", "ris", "pre", "wel"))) %>% 
  summarytools::dfSummary()

M <- df_study2 %>% 
  select(starts_with(c("pro", "ris", "pre", "wel"))) %>% 
  remove_all_labels()

descrip <-psych::describe(M) %>% 
  as.data.frame() %>% 
  select(mean, sd) %>% 
  mutate_all(.funs = ~ round(.,2))

sjPlot::tab_corr(M, 
                 na.deletion = "pairwise", 
                 corr.method = "pearson", 
                 triangle = "lower")

df <- rstatix::cor_test(M, method = "pearson", use = "pairwise.complete.obs") %>% 
  mutate(cor = round(cor, 2),
         p = gtools::stars.pval(p),
         cor = paste0(cor,p)) %>% 
  select(var1, var2, cor) %>% 
  pivot_wider(id_cols = var1, 
              names_from = var2,
              values_from = cor)

df <- as.data.frame(df)
rownames(df) <- df$var1  
df <- df[, -1]
mat_cor <- as.matrix(df)
colnames(mat_cor) <- rownames(mat_cor)
mat_cor[upper.tri(mat_cor, diag = TRUE)] <- NA

t1 <- bind_cols(mat_cor, descrip)

t1 <- t1 %>% 
  rename(`1`= pro_pw,
         `2`=pro_rw,
         `3`=ris_pw,
         `4`=ris_rw,
         `5`=pre_pw,
         `6`=pre_rw,
         `7`=wel_abu_1,
         `8`=wel_abu_2,
         `9`=wel_pa_1,
         `10`=wel_pa_2,
         `11`=wel_ho_1,
         `12`=wel_ho_2)

rownames(t1) <- c(
  "1. Promiscuity (low-SES)",
  "2. Promiscuity (high-SES)",
  "3. Risky sex (low-SES)",
  "4. Risky sex (high-SES)",
  "5. Unplanned pregnancy (low-SES)",
  "6. Unplanned pregnancy (high-SES)",
  "7. Poor women: children for benefits",
  "8. Poor women: pregnancy for welfare",
  "9. Poor women: counsel to avoid pregnancy",
  "10. Poor women: counsel on risky sex",
  "11. Poor women: fertility procedures",
  "12. Poor women: prevent pregnancy"
)

t1 %>% 
  kableExtra::kable(., format = "markdown")


#### 3.1.2 Argentina ####

M <- df_study2 %>% 
  group_by(country) %>% 
  select(starts_with(c("pro", "ris", "pre", "wel"))) %>% 
  remove_all_labels() %>% 
  ungroup()

vars <- c("pro_pw", "pro_rw", "ris_pw", "ris_rw", "pre_pw", "pre_rw",
          "wel_abu_1", "wel_abu_2", "wel_pa_1", "wel_pa_2", "wel_ho_1", "wel_ho_2")

descriptivos_por_pais <- M %>%
  group_by(country) %>%
  summarise(across(
    all_of(vars),
    list(mean = ~round(mean(., na.rm = TRUE), 2),
         sd = ~round(sd(., na.rm = TRUE), 2)),
    .names = "{.col}_{.fn}"
  )) %>% 
  ungroup()


df_arg <- M %>% 
  filter(country == "Argentina") %>% 
  select(-country) %>% 
  rstatix::cor_test(., 
                    method = "pearson", 
                    use = "pairwise.complete.obs") %>% 
  mutate(cor = round(cor, 2),
         p = gtools::stars.pval(p),
         cor = paste0(cor,p)) %>% 
  select(var1, var2, cor) %>% 
  pivot_wider(id_cols = var1, 
              names_from = var2,
              values_from = cor)

df_arg <- df_arg[, -1]
mat_cor <- as.matrix(df_arg)
rownames(mat_cor) <- colnames(mat_cor)
mat_cor[upper.tri(mat_cor, diag = TRUE)] <- NA

t1_arg <- bind_cols(mat_cor, 
                    subset(descriptivos_por_pais, country == "Argentina") %>% 
                      pivot_longer(., cols = !country,
                                   names_to = c("variable", ".value"),
                                   names_pattern = "^(.*)_(mean|sd)$") %>% 
                      select(-country))




t1_arg <- t1_arg %>% 
  rename(`1`= pro_pw,
         `2`=pro_rw,
         `3`=ris_pw,
         `4`=ris_rw,
         `5`=pre_pw,
         `6`=pre_rw,
         `7`=wel_abu_1,
         `8`=wel_abu_2,
         `9`=wel_pa_1,
         `10`=wel_pa_2,
         `11`=wel_ho_1,
         `12`=wel_ho_2)

rownames(t1_arg) <- c(
  "1. Promiscuity (low-SES)",
  "2. Promiscuity (high-SES)",
  "3. Risky sex (low-SES)",
  "4. Risky sex (high-SES)",
  "5. Unplanned pregnancy (low-SES)",
  "6. Unplanned pregnancy (high-SES)",
  "7. Poor women: children for benefits",
  "8. Poor women: pregnancy for welfare",
  "9. Poor women: counsel to avoid pregnancy",
  "10. Poor women: counsel on risky sex",
  "11. Poor women: fertility procedures",
  "12. Poor women: prevent pregnancy"
)

t1_arg %>% 
  select(-variable) %>% 
  kableExtra::kable(., format = "markdown")


#### 3.1.2 Chile ####

df_cl <- M %>% 
  filter(country == "Chile") %>% 
  select(-country) %>% 
  rstatix::cor_test(., 
                    method = "pearson", 
                    use = "pairwise.complete.obs") %>% 
  mutate(cor = round(cor, 2),
         p = gtools::stars.pval(p),
         cor = paste0(cor,p)) %>% 
  select(var1, var2, cor) %>% 
  pivot_wider(id_cols = var1, 
              names_from = var2,
              values_from = cor)

df_cl <- df_cl[, -1]
mat_cor <- as.matrix(df_cl)
rownames(mat_cor) <- colnames(mat_cor)
mat_cor[upper.tri(mat_cor, diag = TRUE)] <- NA

t1_cl<- bind_cols(mat_cor, 
                  subset(descriptivos_por_pais, country == "Chile") %>% 
                    pivot_longer(., cols = !country,
                                 names_to = c("variable", ".value"),
                                 names_pattern = "^(.*)_(mean|sd)$") %>% 
                    select(-country))




t1_cl <- t1_cl %>% 
  rename(`1`= pro_pw,
         `2`=pro_rw,
         `3`=ris_pw,
         `4`=ris_rw,
         `5`=pre_pw,
         `6`=pre_rw,
         `7`=wel_abu_1,
         `8`=wel_abu_2,
         `9`=wel_pa_1,
         `10`=wel_pa_2,
         `11`=wel_ho_1,
         `12`=wel_ho_2)

rownames(t1_cl) <- c(
  "1. Promiscuity (low-SES)",
  "2. Promiscuity (high-SES)",
  "3. Risky sex (low-SES)",
  "4. Risky sex (high-SES)",
  "5. Unplanned pregnancy (low-SES)",
  "6. Unplanned pregnancy (high-SES)",
  "7. Poor women: children for benefits",
  "8. Poor women: pregnancy for welfare",
  "9. Poor women: counsel to avoid pregnancy",
  "10. Poor women: counsel on risky sex",
  "11. Poor women: fertility procedures",
  "12. Poor women: prevent pregnancy"
)

t1_cl %>% 
  select(-variable) %>% 
  kableExtra::kable(., format = "markdown")


#### 3.1.3 Colombia ####

df_col <- M %>% 
  filter(country == "Colombia") %>% 
  select(-country) %>% 
  rstatix::cor_test(., 
                    method = "pearson", 
                    use = "pairwise.complete.obs") %>% 
  mutate(cor = round(cor, 2),
         p = gtools::stars.pval(p),
         cor = paste0(cor,p)) %>% 
  select(var1, var2, cor) %>% 
  pivot_wider(id_cols = var1, 
              names_from = var2,
              values_from = cor)

df_col <- df_col[, -1]
mat_cor <- as.matrix(df_col)
rownames(mat_cor) <- colnames(mat_cor)
mat_cor[upper.tri(mat_cor, diag = TRUE)] <- NA

t1_col<- bind_cols(mat_cor, 
                   subset(descriptivos_por_pais, country == "Colombia") %>% 
                     pivot_longer(., cols = !country,
                                  names_to = c("variable", ".value"),
                                  names_pattern = "^(.*)_(mean|sd)$") %>% 
                     select(-country))




t1_col <- t1_col %>% 
  rename(`1`= pro_pw,
         `2`=pro_rw,
         `3`=ris_pw,
         `4`=ris_rw,
         `5`=pre_pw,
         `6`=pre_rw,
         `7`=wel_abu_1,
         `8`=wel_abu_2,
         `9`=wel_pa_1,
         `10`=wel_pa_2,
         `11`=wel_ho_1,
         `12`=wel_ho_2)

rownames(t1_col) <- c(
  "1. Promiscuity (low-SES)",
  "2. Promiscuity (high-SES)",
  "3. Risky sex (low-SES)",
  "4. Risky sex (high-SES)",
  "5. Unplanned pregnancy (low-SES)",
  "6. Unplanned pregnancy (high-SES)",
  "7. Poor women: children for benefits",
  "8. Poor women: pregnancy for welfare",
  "9. Poor women: counsel to avoid pregnancy",
  "10. Poor women: counsel on risky sex",
  "11. Poor women: fertility procedures",
  "12. Poor women: prevent pregnancy"
)

t1_col %>% 
  select(-variable) %>% 
  kableExtra::kable(., format = "markdown")


#### 3.1.3 Mexico ####

df_mex <- M %>% 
  filter(country == "Mexico") %>% 
  select(-country) %>% 
  rstatix::cor_test(., 
                    method = "pearson", 
                    use = "pairwise.complete.obs") %>% 
  mutate(cor = round(cor, 2),
         p = gtools::stars.pval(p),
         cor = paste0(cor,p)) %>% 
  select(var1, var2, cor) %>% 
  pivot_wider(id_cols = var1, 
              names_from = var2,
              values_from = cor)

df_mex <- df_mex[, -1]
mat_cor <- as.matrix(df_mex)
rownames(mat_cor) <- colnames(mat_cor)
mat_cor[upper.tri(mat_cor, diag = TRUE)] <- NA

t1_mex <- bind_cols(mat_cor, 
                    subset(descriptivos_por_pais, country == "Mexico") %>% 
                      pivot_longer(., cols = !country,
                                   names_to = c("variable", ".value"),
                                   names_pattern = "^(.*)_(mean|sd)$") %>% 
                      select(-country))




t1_mex <- t1_mex %>% 
  rename(`1`= pro_pw,
         `2`=pro_rw,
         `3`=ris_pw,
         `4`=ris_rw,
         `5`=pre_pw,
         `6`=pre_rw,
         `7`=wel_abu_1,
         `8`=wel_abu_2,
         `9`=wel_pa_1,
         `10`=wel_pa_2,
         `11`=wel_ho_1,
         `12`=wel_ho_2)

rownames(t1_mex) <- c(
  "1. Promiscuity (low-SES)",
  "2. Promiscuity (high-SES)",
  "3. Risky sex (low-SES)",
  "4. Risky sex (high-SES)",
  "5. Unplanned pregnancy (low-SES)",
  "6. Unplanned pregnancy (high-SES)",
  "7. Poor women: children for benefits",
  "8. Poor women: pregnancy for welfare",
  "9. Poor women: counsel to avoid pregnancy",
  "10. Poor women: counsel on risky sex",
  "11. Poor women: fertility procedures",
  "12. Poor women: prevent pregnancy"
)

t1_mex %>% 
  select(-variable) %>% 
  kableExtra::kable(., format = "markdown")


#### 3.1.3 Spain ####

df_spa <- M %>% 
  filter(country == "Spain") %>% 
  select(-country) %>% 
  rstatix::cor_test(., 
                    method = "pearson", 
                    use = "pairwise.complete.obs") %>% 
  mutate(cor = round(cor, 2),
         p = gtools::stars.pval(p),
         cor = paste0(cor,p)) %>% 
  select(var1, var2, cor) %>% 
  pivot_wider(id_cols = var1, 
              names_from = var2,
              values_from = cor)

df_spa <- df_spa[, -1]
mat_cor <- as.matrix(df_spa)
rownames(mat_cor) <- colnames(mat_cor)
mat_cor[upper.tri(mat_cor, diag = TRUE)] <- NA

t1_spa <- bind_cols(mat_cor, 
                    subset(descriptivos_por_pais, country == "Spain") %>% 
                      pivot_longer(., cols = !country,
                                   names_to = c("variable", ".value"),
                                   names_pattern = "^(.*)_(mean|sd)$") %>% 
                      select(-country))




t1_spa <- t1_spa %>% 
  rename(`1`= pro_pw,
         `2`=pro_rw,
         `3`=ris_pw,
         `4`=ris_rw,
         `5`=pre_pw,
         `6`=pre_rw,
         `7`=wel_abu_1,
         `8`=wel_abu_2,
         `9`=wel_pa_1,
         `10`=wel_pa_2,
         `11`=wel_ho_1,
         `12`=wel_ho_2)

rownames(t1_spa) <- c(
  "1. Promiscuity (low-SES)",
  "2. Promiscuity (high-SES)",
  "3. Risky sex (low-SES)",
  "4. Risky sex (high-SES)",
  "5. Unplanned pregnancy (low-SES)",
  "6. Unplanned pregnancy (high-SES)",
  "7. Poor women: children for benefits",
  "8. Poor women: pregnancy for welfare",
  "9. Poor women: counsel to avoid pregnancy",
  "10. Poor women: counsel on risky sex",
  "11. Poor women: fertility procedures",
  "12. Poor women: prevent pregnancy"
)

t1_spa %>% 
  select(-variable) %>% 
  kableExtra::kable(., format = "markdown")


#### 3.2 Reliability ####

##### 3.2.1. Misconduct poor women ####

pp_rel_all <- psych::alpha(subset(df_study2, select = c("pro_pw", "ris_pw", "pre_pw")))
pp_rel_all$total[2]

pp_rel_arg <- psych::alpha(subset(df_study2, country == "Argentina", select = c("pro_pw", "ris_pw", "pre_pw")))
pp_rel_arg$total[2]

pp_rel_cl <- psych::alpha(subset(df_study2, country == "Chile", select = c("pro_pw", "ris_pw", "pre_pw")))
pp_rel_cl$total[2]

pp_rel_co <- psych::alpha(subset(df_study2, country == "Colombia", select = c("pro_pw", "ris_pw", "pre_pw")))
pp_rel_co$total[2]

pp_rel_mex <- psych::alpha(subset(df_study2, country == "Mexico", select = c("pro_pw", "ris_pw", "pre_pw")))
pp_rel_mex$total[2]

pp_rel_spa <- psych::alpha(subset(df_study2, country == "Spain", select = c("pro_pw", "ris_pw", "pre_pw")))
pp_rel_spa$total[2]

##### 3.2.2. Misconduct rich women ####

pr_rel_all <- psych::alpha(subset(df_study2, select = c("pro_rw", "ris_rw", "pre_rw")))
pr_rel_all$total[2]

pr_rel_arg <- psych::alpha(subset(df_study2, country == "Argentina", select = c("pro_rw", "ris_rw", "pre_rw")))
pr_rel_arg$total[2]

pr_rel_cl <- psych::alpha(subset(df_study2, country == "Chile", select = c("pro_rw", "ris_rw", "pre_rw")))
pr_rel_cl$total[2]

pr_rel_co <- psych::alpha(subset(df_study2, country == "Colombia", select = c("pro_rw", "ris_rw", "pre_rw")))
pr_rel_co$total[2]

pr_rel_mex <- psych::alpha(subset(df_study2, country == "Mexico", select = c("pro_rw", "ris_rw", "pre_rw")))
pr_rel_mex$total[2]

pr_rel_spa <- psych::alpha(subset(df_study2, country == "Spain", select = c("pro_rw", "ris_rw", "pre_rw")))
pr_rel_spa$total[2]

##### 3.2.3. Abuse ####

wel_abu_rel_all <- psych::alpha(subset(df_study2, select = c("wel_abu_1", "wel_abu_2")))
wel_abu_rel_all$total[2]

wel_abu_rel_arg <- psych::alpha(subset(df_study2, country == "Argentina", select = c("wel_abu_1", "wel_abu_2")))
wel_abu_rel_arg$total[2]

wel_abu_rel_cl <- psych::alpha(subset(df_study2, country == "Chile", select = c("wel_abu_1", "wel_abu_2")))
wel_abu_rel_cl$total[2]

wel_abu_rel_co <- psych::alpha(subset(df_study2, country == "Colombia", select = c("wel_abu_1", "wel_abu_2")))
wel_abu_rel_co$total[2]

wel_abu_rel_mex <- psych::alpha(subset(df_study2, country == "Mexico", select = c("wel_abu_1", "wel_abu_2")))
wel_abu_rel_mex$total[2]

wel_abu_rel_spa <- psych::alpha(subset(df_study2, country == "Spain", select = c("wel_abu_1", "wel_abu_2")))
wel_abu_rel_spa$total[2]

##### 3.2.3. Hostile ####

wel_ho_rel_all <- psych::alpha(subset(df_study2, select = c("wel_ho_1", "wel_ho_2")))
wel_ho_rel_all$total[2]

wel_ho_rel_arg <- psych::alpha(subset(df_study2, country == "Argentina", select = c("wel_ho_1", "wel_ho_2")))
wel_ho_rel_arg$total[2]

wel_ho_rel_cl <- psych::alpha(subset(df_study2, country == "Chile", select = c("wel_ho_1", "wel_ho_2")))
wel_ho_rel_cl$total[2]

wel_ho_rel_co <- psych::alpha(subset(df_study2, country == "Colombia", select = c("wel_ho_1", "wel_ho_2")))
wel_ho_rel_co$total[2]

wel_ho_rel_mex <- psych::alpha(subset(df_study2, country == "Mexico", select = c("wel_ho_1", "wel_ho_2")))
wel_ho_rel_mex$total[2]

wel_ho_rel_spa <- psych::alpha(subset(df_study2, country == "Spain", select = c("wel_ho_1", "wel_ho_2")))
wel_ho_rel_spa$total[2]

##### 3.2.4. Paternalism ####

wel_pa_rel_all <- psych::alpha(subset(df_study2, select = c("wel_pa_1", "wel_pa_2")))
wel_pa_rel_all$total[2]

wel_pa_rel_arg <- psych::alpha(subset(df_study2, country == "Argentina", select = c("wel_pa_1", "wel_pa_2")))
wel_pa_rel_arg$total[2]

wel_pa_rel_cl <- psych::alpha(subset(df_study2, country == "Chile", select = c("wel_pa_1", "wel_pa_2")))
wel_pa_rel_cl$total[2]

wel_pa_rel_co <- psych::alpha(subset(df_study2, country == "Colombia", select = c("wel_pa_1", "wel_pa_2")))
wel_pa_rel_co$total[2]

wel_pa_rel_mex <- psych::alpha(subset(df_study2, country == "Mexico", select = c("wel_pa_1", "wel_pa_2")))
wel_pa_rel_mex$total[2]

wel_pa_rel_spa <- psych::alpha(subset(df_study2, country == "Spain", select = c("wel_pa_1", "wel_pa_2")))
wel_pa_rel_spa$total[2]

#### 3.2.5 Compute variables ####

df_study2 <- cbind(df_study2, 
                   "abuse_index" = rowMeans(df_study2 %>% select(wel_abu_1,
                                                                 wel_abu_2), na.rm=TRUE))

df_study2 <- cbind(df_study2, 
                   "hostile_index" = rowMeans(df_study2 %>% select(wel_ho_1,
                                                                 wel_ho_2), na.rm=TRUE))

df_study2 <- cbind(df_study2, 
                   "paternalistic_index" = rowMeans(df_study2 %>% select(wel_pa_1,
                                                                   wel_pa_2), na.rm=TRUE))


# SEM models ----

#### SEM All countries #### 

# Definir modelo 

model_base <- '
  # Factores latentes
  abuse =~ wel_abu_1 + wel_abu_2
  paternalistic =~ wel_pa_1 + wel_pa_2
  hostile =~ wel_ho_1 + wel_ho_2

  # Efectos principales
  abuse ~ a1*pro_pw + a2*ris_pw + a3*pre_pw
  paternalistic ~ b1*pro_pw + b2*ris_pw + b3*pre_pw
  hostile       ~ c1*pro_pw + c2*ris_pw + c3*pre_pw

  paternalistic ~ m1*abuse
  hostile       ~ m2*abuse

  # Correlación entre dependientes
  paternalistic ~~ hostile

  # Efectos indirectos
  ind_paternalistic_promiscuity := a1 * m1
  ind_hostile_promiscuity       := a1 * m2
  ind_paternalistic_risk := a2 * m1
  ind_hostile_risk       := a2 * m2
  ind_paternalistic_pregnancy := a3 * m1
  ind_hostile_pregnancy       := a3 * m2
'

controls <- c("age", "edu_dic", "ses", "po", sex_vars, income_vars, country_vars)

control_formula <- paste(controls, collapse = " + ")

control_block <- paste0(
  "abuse ~ ", control_formula, "\n",
  "paternalistic ~ ", control_formula, "\n",
  "hostile ~ ", control_formula, "\n"
)

model <- paste(model_base, control_block)

# Estimación 

subset(df_study2, select = c(pro_pw, ris_pw, pre_pw, wel_abu_1, 
                             wel_abu_2, wel_pa_1, wel_pa_2, 
                             wel_ho_1, wel_ho_2, age, sex,
                             income_quintile, edu_dic, ses,
                             po)) %>% 
  as_numeric() %>% 
  mardia(na.rm = TRUE, plot=TRUE)


fit <- sem(model, data = df_study2, estimator = "MLR")

summary(fit, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

fitmeasures(fit, c("chisq", "pvalue", "df", "cfi", "tli", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr"))

# Monte Carlo confidence intervals 
monteCarloCI(fit, nRep = 10000, fast = TRUE, level = .95, plot = TRUE)

# modelo con los predictores como un factor latente presenta ajuste deficiente, es mejor el modelo con predictores cada uno por separado

# Efectos directos, indirectos y totales
parameterEstimates(fit, standardized = TRUE) %>%
  dplyr::filter(op %in% c("~", ":="))


# Todos los efectos
est <- parameterEstimates(fit, standardized = TRUE)

# Efectos directos (regresiones)
direct <- est %>%
  filter(op == "~", lhs %in% c("paternalistic", "hostile"), 
         rhs %in% c("pro_pw", "ris_pw", "pre_pw")) %>%
  select(lhs, rhs, est.std = std.all, p.direct = pvalue)

# Efectos indirectos (los que tú definiste con :=)
indirect <- est %>%
  filter(op == ":=", grepl("ind_", lhs)) %>%
  mutate(
    outcome = case_when(
      grepl("paternalistic", lhs) ~ "paternalistic",
      grepl("hostile", lhs) ~ "hostile"
    ),
    predictor = case_when(
      grepl("promiscuity", lhs) ~ "pro_pw",
      grepl("risk", lhs) ~ "ris_pw",
      grepl("pregnancy", lhs) ~ "pre_pw"
    )
  ) %>%
  select(lhs, outcome, predictor, est.indirect = est, p.indirect = pvalue, std.indirect = std.all)

# Unir por outcome y predictor
effects_total <- left_join(direct,
                           indirect,
                           by = c("lhs" = "outcome", "rhs" = "predictor")) %>%
  mutate(total = est.std + std.indirect)


#### SEM for each country ####

controls1 <- c("age", "edu_dic", "ses", "po", sex_vars, income_vars)

control_formula1 <- paste(controls1, collapse = " + ")

control_block1 <- paste0(
  "abuse ~ ", control_formula1, "\n",
  "paternalistic ~ ", control_formula1, "\n",
  "hostile ~ ", control_formula1, "\n"
)

model1 <- paste(model_base, control_block1)

vars_sem <- c("pro_pw", "ris_pw", "pre_pw", 
              "wel_abu_1", "wel_abu_2", 
              "wel_pa_1", "wel_pa_2", 
              "wel_ho_1", "wel_ho_2", 
              "age", "sex", "income_quintile", 
              "edu_dic", "ses", "po")

paises <- c("Argentina", "Chile", "Colombia", "Mexico", "Spain")

model_colombia <- gsub("sexOther \\+ ", "", model1)
model_colombia <- gsub(" \\+ sexOther", "", model_colombia)

run_sem_country <- function(pais) {
  message("\n##### SEM ", pais, " #####")
  
  modelo_pais <- if (pais == "Colombia") model_colombia else model1
  
  df <- df_study2 %>%
    filter(country == pais) %>%
    select(all_of(vars_sem)) %>%
    as_numeric()
  
  mardia(df, na.rm = TRUE, plot = TRUE)
  
  fit <- sem(modelo_pais, data = subset(df_study2, country == pais), estimator = "MLR")
  
  print(summary(fit, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE))
  
  print(fitmeasures(fit, c("chisq", "pvalue", "df", "cfi", "tli", 
                           "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr")))
  
  monteCarloCI(fit, nRep = 10000, fast = TRUE, level = .95, plot = TRUE)
  
  params <- parameterEstimates(fit, standardized = TRUE) %>%
    filter(op %in% c("~", ":="))
  
  return(list(
    country = pais,
    fit = fit,
    parameters = params
  ))
}

resultados_sem <- map(paises, run_sem_country)

names(resultados_sem) <- paises



