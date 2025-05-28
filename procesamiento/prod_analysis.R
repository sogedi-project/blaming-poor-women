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

# 3. Analysis --------------------------------------------------------------

# descriptive

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

# SEM models ----

# Montecarlo simulations ----
