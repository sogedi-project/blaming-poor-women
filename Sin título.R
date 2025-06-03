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

#### SEM for each country ####

controls1 <- c("age", "edu_dic", "ses", "po", sex_vars, income_vars)

control_formula1 <- paste(controls1, collapse = " + ")

control_block1 <- paste0(
  "abuse ~ ", control_formula1, "\n",
  "paternalistic ~ ", control_formula1, "\n",
  "hostile ~ ", control_formula1, "\n"
)

model1 <- paste(model_base, control_block1)

#### SEM Argentina ####

subset(df_study2, select = c(pro_pw, ris_pw, pre_pw, wel_abu_1, 
                             wel_abu_2, wel_pa_1, wel_pa_2, 
                             wel_ho_1, wel_ho_2, age, sex,
                             income_quintile, edu_dic, ses,
                             po), country == "Argentina") %>% 
  as_numeric() %>% 
  mardia(na.rm = TRUE, plot=TRUE)


fit_arg <- sem(model1, data = subset(df_study2, country == "Argentina"), estimator = "MLR")

summary(fit_arg, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

fitmeasures(fit_arg, c("chisq", "pvalue", "df", "cfi", "tli", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr"))

# Monte Carlo confidence intervals 
monteCarloCI(fit_arg, nRep = 10000, fast = TRUE, level = .95, plot = TRUE)

# modelo con los predictores como un factor latente presenta ajuste deficiente, es mejor el modelo con predictores cada uno por separado

# Efectos directos, indirectos y totales
parameterEstimates(fit_arg, standardized = TRUE) %>%
  dplyr::filter(op %in% c("~", ":="))

#### SEM Chile ####

subset(df_study2, select = c(pro_pw, ris_pw, pre_pw, wel_abu_1, 
                             wel_abu_2, wel_pa_1, wel_pa_2, 
                             wel_ho_1, wel_ho_2, age, sex,
                             income_quintile, edu_dic, ses,
                             po), country == "Chile") %>% 
  as_numeric() %>% 
  mardia(na.rm = TRUE, plot=TRUE)


fit_cl <- sem(model1, data = subset(df_study2, country == "Chile"), estimator = "MLR")

summary(fit_cl, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

fitmeasures(fit_cl, c("chisq", "pvalue", "df", "cfi", "tli", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr"))

# Monte Carlo confidence intervals 
monteCarloCI(fit_cl, nRep = 10000, fast = TRUE, level = .95, plot = TRUE)

# modelo con los predictores como un factor latente presenta ajuste deficiente, es mejor el modelo con predictores cada uno por separado

# Efectos directos, indirectos y totales
parameterEstimates(fit_cl, standardized = TRUE) %>%
  dplyr::filter(op %in% c("~", ":="))

#### SEM Colombia ####

subset(df_study2, select = c(pro_pw, ris_pw, pre_pw, wel_abu_1, 
                             wel_abu_2, wel_pa_1, wel_pa_2, 
                             wel_ho_1, wel_ho_2, age, sex,
                             income_quintile, edu_dic, ses,
                             po), country == "Colombia") %>% 
  as_numeric() %>% 
  mardia(na.rm = TRUE, plot=TRUE)


model_colombia <- gsub("sexOther \\+ ", "", model1)  # si está al inicio
model_colombia <- gsub(" \\+ sexOther", "", model_colombia)  # si está al final o al medio

fit_col <- sem(model_colombia, data = subset(df_study2, country == "Colombia"), estimator = "MLR")

summary(fit_col, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

fitmeasures(fit_col, c("chisq", "pvalue", "df", "cfi", "tli", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr"))

# Monte Carlo confidence intervals 
monteCarloCI(fit_col, nRep = 10000, fast = TRUE, level = .95, plot = TRUE)

# modelo con los predictores como un factor latente presenta ajuste deficiente, es mejor el modelo con predictores cada uno por separado

# Efectos directos, indirectos y totales
parameterEstimates(fit_col, standardized = TRUE) %>%
  dplyr::filter(op %in% c("~", ":="))


#### SEM Mexico ####

subset(df_study2, select = c(pro_pw, ris_pw, pre_pw, wel_abu_1, 
                             wel_abu_2, wel_pa_1, wel_pa_2, 
                             wel_ho_1, wel_ho_2, age, sex,
                             income_quintile, edu_dic, ses,
                             po), country == "Mexico") %>% 
  as_numeric() %>% 
  mardia(na.rm = TRUE, plot=TRUE)


fit_mex <- sem(model1, data = subset(df_study2, country == "Mexico"), estimator = "MLR")

summary(fit_mex, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

fitmeasures(fit_mex, c("chisq", "pvalue", "df", "cfi", "tli", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr"))

# Monte Carlo confidence intervals 
monteCarloCI(fit_mex, nRep = 10000, fast = TRUE, level = .95, plot = TRUE)

# modelo con los predictores como un factor latente presenta ajuste deficiente, es mejor el modelo con predictores cada uno por separado

# Efectos directos, indirectos y totales
parameterEstimates(fit_mex, standardized = TRUE) %>%
  dplyr::filter(op %in% c("~", ":="))


#### SEM Spain ####

subset(df_study2, select = c(pro_pw, ris_pw, pre_pw, wel_abu_1, 
                             wel_abu_2, wel_pa_1, wel_pa_2, 
                             wel_ho_1, wel_ho_2, age, sex,
                             income_quintile, edu_dic, ses,
                             po), country == "Spain") %>% 
  as_numeric() %>% 
  mardia(na.rm = TRUE, plot=TRUE)


fit_spa <- sem(model1, data = subset(df_study2, country == "Spain"), estimator = "MLR")

summary(fit_spa, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)

fitmeasures(fit_spa, c("chisq", "pvalue", "df", "cfi", "tli", "rmsea", "rmsea.ci.lower", "rmsea.ci.upper", "srmr"))

# Monte Carlo confidence intervals 
monteCarloCI(fit_spa, nRep = 10000, fast = TRUE, level = .95, plot = TRUE)

# modelo con los predictores como un factor latente presenta ajuste deficiente, es mejor el modelo con predictores cada uno por separado

# Efectos directos, indirectos y totales
parameterEstimates(fit_spa, standardized = TRUE) %>%
  dplyr::filter(op %in% c("~", ":="))
