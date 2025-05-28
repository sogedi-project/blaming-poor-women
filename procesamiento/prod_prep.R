# 0. Identification ---------------------------------------------------

# Title: Data preparation for research paper on Blaming Poor Women
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
               sjlabelled,
               naniar,
               data.table,
               psych,
               rstatix)

options(scipen=999)
rm(list = ls())

# 2. Data -----------------------------------------------------------------

load(url("https://github.com/sogedi-project/sogedi-data/raw/refs/heads/main/output/data/db_proc.RData"))

glimpse(db_proc)

# 3. Processing --------------------------------------------------------------

db_proc_lab <- db_proc

db_proc <- sjlabelled::remove_all_labels(db_proc)

db_proc <- sjlabelled::copy_labels(df_new = db_proc, df_origin = db_proc_lab)

# 3.1 Select ----

db_proc <- db_proc %>% 
  dplyr::select(ID, natio_recoded, age, sex, edu, ses, po, inc, n_perso, currency, 
                pro_pw, pro_rw, ris_pw, ris_rw, pre_pw, pre_rw,
                wel_abu_1, wel_abu_2, wel_pa_1, wel_pa_2, 
                wel_ho_1, wel_ho_2) %>% 
  as_tibble()

# 3.2 Filter ----

db_proc <- db_proc %>% 
  filter(natio_recoded %in% c(1, 3, 4, 9, 13) & age >= 18)


# 3.3 Recode and transform ----

# country
frq(db_proc$natio_recoded)

db_proc$natio_recoded <- car::recode(db_proc$natio_recoded, 
                                     recodes = c("1 = 'Argentina';
                                                 3 = 'Chile';
                                                 4 = 'Colombia';
                                                 9 = 'Spain';
                                                 13 = 'Mexico'"), 
                                     as.factor = T)


# sex
frq(db_proc$sex)

db_proc$sex <- car::recode(db_proc$sex, recodes = c("2 = 'Hombre'; 1 = 'Mujer'; 0 = 'Otro'"),
                                     as.factor = T)


# age
frq(db_proc$age)

db_proc$age <- sjlabelled::set_na(db_proc$age, na = c(999))

# ses
frq(db_proc$ses) #ok

# po
frq(db_proc$po) # ok

# education
frq(db_proc$edu)

db_proc$edu_dic <- if_else(db_proc$edu > 4, "Universitary education or more", "Less than Universitary")
db_proc$edu_dic <- factor(db_proc$edu_dic, levels = c("Less than Universitary", "Universitary education or more"))

frq(db_proc$edu_dic)

# income
frq(db_proc$currency)
frq(db_proc$inc)
frq(db_proc$n_perso)

db_proc$n_perso <- if_else(db_proc$n_perso > 60, NA, db_proc$n_perso)

db_proc <- db_proc %>% 
  mutate(exchange_rates = case_when(natio_recoded == "Argentina" ~ 915.161,
                                    natio_recoded == "Chile" ~ 944.457,
                                    natio_recoded == "Colombia" ~ 4307.27,
                                    natio_recoded == "Mexico" ~ 20.06,
                                    natio_recoded == "Spain" ~ 0.924))

db_proc$equiv_income_local <- db_proc$inc / (1 + 0.5 * (db_proc$n_perso - 1))

db_proc$equiv_income_usd <- (db_proc$equiv_income_local / db_proc$exchange_rates)

db_proc <- db_proc %>% 
  group_by(natio_recoded) %>% 
  mutate(income_quintile = ntile(-desc(equiv_income_usd), n = 5)) %>% 
  ungroup()

db_proc$income_quintile <- factor(db_proc$income_quintile,
         levels = c(1, 2, 3, 4, 5),
         labels = c('Q1', 'Q2', 'Q3', 'Q4', 'Q5'))

frq(db_proc$income_quintile)

# perceived sexual misconduct

## promiscuity
db_proc %>% 
  select(pro_pw, pro_rw, natio_recoded) %>% 
  group_by(natio_recoded) %>% 
  frq()

## risk sexual behavior
db_proc %>% 
  select(ris_pw, ris_rw, natio_recoded) %>% 
  group_by(natio_recoded) %>% 
  frq()

## pregnancy
db_proc %>% 
  select(pre_pw, pre_rw, natio_recoded) %>% 
  group_by(natio_recoded) %>% 
  frq()

# abuse of social assitance

db_proc %>% 
  select(wel_abu_1, wel_abu_2, natio_recoded) %>% 
  group_by(natio_recoded) %>% 
  frq()

# paternalistic support

db_proc %>% 
  select(wel_pa_1, wel_pa_2, natio_recoded) %>% 
  group_by(natio_recoded) %>% 
  frq()

# hostile support

db_proc %>% 
  select(wel_ho_1, wel_ho_2, natio_recoded) %>% 
  group_by(natio_recoded) %>% 
  frq()

# 3.4 Missings values ----

colSums(is.na(db_proc))

n_miss(db_proc)

prop_miss(db_proc)*100

miss_var_summary(db_proc)

miss_var_table(db_proc)

vis_miss(db_proc) + theme(axis.text.x = element_text(angle=80))

db_proc <- na.omit(db_proc)

# 3.5 Labels ----

db_proc$natio_recoded <- sjlabelled::set_label(db_proc$natio_recoded, "Country")
db_proc$age <- sjlabelled::set_label(db_proc$age, "Age")
db_proc$sex <- sjlabelled::set_label(db_proc$sex, "Gender")
db_proc$edu <- sjlabelled::set_label(db_proc$edu, "Educational level")
db_proc$edu_dic <- sjlabelled::set_label(db_proc$edu_dic, "Universitary education")
db_proc$ses <- sjlabelled::set_label(db_proc$ses, "Subjective social status")
db_proc$po <- sjlabelled::set_label(db_proc$po, "Political identification")
db_proc$income_quintile <- sjlabelled::set_label(db_proc$income_quintile, "Household equivalent income quintile per capita")

db_proc$pro_pw <- sjlabelled::set_label(db_proc$pro_pw, "Perceived promiscuity for low-SES women")
db_proc$pro_rw <- sjlabelled::set_label(db_proc$pro_rw, "Perceived promiscuity for high-SES women")

db_proc$ris_pw <- sjlabelled::set_label(db_proc$ris_pw, "Risky sexual behaviour for low-SES women")
db_proc$ris_rw <- sjlabelled::set_label(db_proc$ris_rw, "Risky sexual behaviour for high-SES women")

db_proc$pre_pw <- sjlabelled::set_label(db_proc$pre_pw, "Unplanned pregnancy for low-SES women")
db_proc$pre_rw <- sjlabelled::set_label(db_proc$pre_rw, "Unplanned pregnancy for high-SES women")

db_proc$wel_abu_1 <- sjlabelled::set_label(db_proc$wel_abu_1, "Poor women have children in order to receive more social benefits")
db_proc$wel_abu_2 <- sjlabelled::set_label(db_proc$wel_abu_2, "Poor women use their pregnancy to live on welfare benefits")

db_proc$wel_pa_1 <- sjlabelled::set_label(db_proc$wel_pa_1, "Poor women should receive counseling to avoid unwanted pregnancies")
db_proc$wel_pa_2 <- sjlabelled::set_label(db_proc$wel_pa_2, "Poor women should be counselled to avoid risky sexual behaviours")

db_proc$wel_ho_1 <- sjlabelled::set_label(db_proc$wel_ho_1, "Poor women should undergo procedures to reduce their fertility")
db_proc$wel_ho_2 <- sjlabelled::set_label(db_proc$wel_ho_2, "Poor women should receive treatment to prevent them from becoming pregnant")

# 4. Save and export  ----------------------------------------------------------------

df_study2 <- db_proc %>% 
  select(ID, country = natio_recoded, age, sex, edu, edu_dic, income_quintile, ses, po,
         equiv_income_usd, starts_with(c("pro", "ris", "pre", "wel"))) 

save(df_study2, file = here("input/data/proc/df_study2.RData"))
