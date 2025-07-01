df_anova <- df_study2 %>% 
  select(ID, pro_pw, pro_rw, ris_pw, ris_rw, pre_pw, pre_rw,
         pp = pp_pw, cc = cc_pw, hc = hc_pw) %>%
  tidyr::pivot_longer(cols = -c(ID, pp, cc, hc),    
                      names_to = "variables",
                      values_to = "values")  %>% 
  tidyr::separate(variables, into = c("variables", "target"), sep = 3) %>% 
  pivot_wider(names_from = "variables",
              values_from = "values") %>% 
  mutate(target = if_else(target == "_pw", "PW", "RW"))

head(df_anova)

#calculas las diferencias entre los targets en esos tres ítems metiendo 
#como covariales las dimensiones del clasismo.

library(rstatix)

library("haven")
library("dplyr")
library("tidyr")
library("plyr")
library("psych")

library("lavaan")
library("semTools")
library("lsr")
library("rstatix")
library("ggpubr")
library("ggplot2")
library("jmv")

mancova(data = DB, deps = vars(hc, pp, cc, aut, depe), factors = vars(valence_mani, sex_mani), covs = vars(quantity_pre, ses, sex))

require(devtools)
install_version("jmv", version = "2.5.6", repos = "http://cran.us.r-project.org")
