# Analysis.R

# This code:
# > Replicates all figures and tables presented in the manuscript:
# > "Technical Change in Agriculture and Homicides: The Case of Genetically-Modified Soy Seeds in Brazil"

###########################################################################

# Step 0: Cleaning workplace and loading libraries

rm(list = ls())

setwd("P:/Luca Louzada/Soy/")

library(dplyr)
library(data.table)
library(tidyr)
library(stargazer)
library(ggplot2)
library(cowplot)
library(broom)
library(fixest)
library(haven)
library(tidylog)
library(readxl)
library(sandwich)
library(lfe)
library(stargazer)

options(scipen = 999) # disable scientific notation

###########################################################################

# Step 1: Preparing data

# This is the main dataset. Soy variables were obtained directly from
# the Bustos et al (2016) replication package. Mortality rates were
# calculated using data from DATASUS and the rural indicator is from IBGE
data = fread("dataset.csv")

# This is the first differenced dataset using data from the 2000 and 2010 
# Census (IBGE). Age-adjsted mortality rates were calculated using data from DATASUS
data_00_10 = fread("dataset_00_10.csv")

# This dataset comes directly from PAM-IBGE
soyproduction = fread("dataset_soyproduction.csv")

never_soy =  soyproduction %>%
  group_by(amc) %>% 
  summarise(total_soy = sum(soy_prod)) %>% 
  filter(total_soy == 0) %>% 
  pull(amc)

mun_abovemedian_soy = data %>% 
                      select(dA_soy, amc, rural) %>% 
                      unique() %>% 
                      ungroup() %>% 
                      filter(rural == TRUE) %>% 
                      filter(dA_soy >= median(dA_soy, na.rm = T)) %>% 
                      pull(amc)

# Regions
centro_oeste = c(52, 51, 50, 53)
sudeste = c(35, 33, 31, 32)
sul = c(41, 43, 42)

###########################################################################

# Step 2: Tables

# Main table
m1 = felm(mort_total_violence ~ post03:dA_soy + i(year, rural_91, 2003)  | amc + year| 0 | amc,
          data = data) 

m2 = felm(mort_total_violence ~ post03:dA_soy +  i(year, rural_91, 2003) + i(year,log_y_pc_r_91, 2003) + 
            i(year,log_pop_area_91, 2003) + i(year,alpha_adult_91, 2003)  | amc + year | 0 | amc,
          data = data) 

m3 = felm(mort_total_violence ~ post03:dA_soy +  i(year, rural_91, 2003) + i(year,log_y_pc_r_91, 2003) + 
            i(year,log_pop_area_91, 2003) + i(year,alpha_adult_91, 2003)  + i(year, factor(cod_uf), 2003)| amc | 0 | amc,
          data = data) 

stargazer(m1, m2, m3, keep = "soy", type = "latex")


# Robustness:
m1 = felm(mort_total_violence_ageadj ~ dA_soy | cod_uf| 0 | microregion,
          data = data_00_10) 

m2 = felm(mort_total_violence ~ post03:dA_soy +  i(year, rural_91, 2003) + i(year,log_y_pc_r_91, 2003) + 
            i(year,log_pop_area_91, 2003) + i(year,alpha_adult_91, 2003)  | amc + year | 0 | amc,
          data = filter(data, cod_uf %in% c(sul, sudeste, centro_oeste))) 

m3 = felm(mort_total_violence ~ post03:dA_soy +  i(year, rural_91, 2003) + i(year,log_y_pc_r_91, 2003) + 
            i(year,log_pop_area_91, 2003) + i(year,alpha_adult_91, 2003) | amc +year | 0 | amc,
          data = filter(data, rural == T)) 

m4 =  felm(mort_total_violence ~ post03:dA_soy +  i(year, rural_91, 2003) + i(year,log_y_pc_r_91, 2003) + 
             i(year,log_pop_area_91, 2003) + i(year,alpha_adult_91, 2003)  | amc + year | 0 | mesoregion,
           data = data) 

m5 =  felm(mort_total_violence ~ post03:dA_soy +  post03:dA_mze+ i(year, rural_91, 2003) + i(year,log_y_pc_r_91, 2003) + 
             i(year,log_pop_area_91, 2003) + i(year,alpha_adult_91, 2003)  |  amc + year | 0 | amc,
           data = data) 

stargazer(m1, m2, m3, m4, m5, keep = "soy", type = "latex")

# Mechanisms: employment
m1 = felm(lincome ~ dA_soy + alpha_adult_91 + rural_91 + log_y_pc_r_91 +
            log_pop_area_91 + dA_mze  | cod_uf| 0 | microregion,
          data = data_00_10) 

m2 = felm(unemployment ~ dA_soy + alpha_adult_91 + rural_91 + log_y_pc_r_91 +
            log_pop_area_91 + dA_mze  | cod_uf| 0 | microregion,
          data = data_00_10) 
m3 = felm(employment ~ dA_soy + alpha_adult_91 + rural_91 + log_y_pc_r_91 +
            log_pop_area_91 + dA_mze  | cod_uf| 0 | microregion,
          data = data_00_10) 
m4 = felm(share_manuf ~ dA_soy + alpha_adult_91 + rural_91 + log_y_pc_r_91 +
            log_pop_area_91 + dA_mze  | cod_uf| 0 | microregion,
          data = data_00_10) 
m5 = felm(share_prim ~ dA_soy + alpha_adult_91 + rural_91 + log_y_pc_r_91 +
            log_pop_area_91 + dA_mze  | cod_uf| 0 | microregion,
          data = data_00_10) 
m6 = felm(share_nontrad ~ dA_soy + alpha_adult_91 + rural_91 + log_y_pc_r_91 +
            log_pop_area_91 + dA_mze  | cod_uf| 0 | microregion,
          data = data_00_10) 

stargazer(m2, m3, m4, m5, m6, type = "latex", keep = "soy")

###########################################################################

# Step 3: Plots

# Descriptive - deaths
df = data %>% mutate(group = case_when(amc %in% mun_abovemedian_soy ~ "abovemedian",
                                             TRUE ~ "belowmedian")) %>% 
  filter(rural == T) %>% 
  group_by(year, group) %>% 
  summarise(mortality = weighted.mean(mort_total_violence, population)) %>% 
  group_by(year) %>% 
  summarise(ratio = mortality[group == "abovemedian"] / mortality[group == "belowmedian"])

plot1 = ggplot(data = df, aes(x = as.numeric(as.character(year)), y = ratio, group = 1)) +
  geom_line(color = "#3B4992", size = 1.5) +
  geom_vline(xintercept = 2003, color = "black", linetype = "dashed", size = 1.5) +
  geom_hline(yintercept = 1, color = "lightgrey", linetype = "dashed", size = 1.5) +
  ylab("Violent mortality ratio (rural - high soy / rural - low soy)") + xlab("Year") +
  theme_classic() +
  theme(text = element_text(size = 20))

ggsave("figure1.jpeg", plot1, height = 8, width = 12)

# Descriptive - soy area
areas =  soyproduction %>% 
  group_by(year) %>% 
  summarise(other_agro = sum(other_agro),
            soy = sum(soy_prod)) %>% 
  mutate(other_agro = other_agro / other_agro[year == 1991] * 100,
         soy = soy / soy[year == 1991] * 100) %>% 
  filter(year >= 1991 & year < 2020) %>% 
  pivot_longer(cols = -year)

plot2 = ggplot(data = areas, aes(x = as.numeric(year),  y = value, group = name)) + 
  geom_line(aes(color = name), size = 1.5) +
  geom_vline(xintercept = 2003, color = "black", linetype = "dashed", size = 1.5) +
  ylab("Planted area (1991 = 100)") + xlab("Year") +
  scale_colour_manual("", 
                      values = c("other_agro" = "#BB0021FF", 
                                 "soy"="#3B4992"),
                      labels = c("other_agro" = "Other products", 
                                 "soy"="Soy")) +
  theme_classic() +
  theme(legend.position = "bottom") +
  theme(text = element_text(size = 20))

ggsave("figure2.jpeg", plot2, height = 8, width = 12)

# Pre-trends figure
model = felm(mort_total_violence ~ i(year,dA_soy, 2003) +  i(year, rural_91, 2003) + i(year,log_y_pc_r_91, 2003) + 
      i(year,log_pop_area_91, 2003) + i(year,alpha_adult_91, 2003)  | amc + year| 0 | amc,
      data = data) 

tbl = model %>%
       tidy(conf.int = T) %>% filter(grepl("soy", term)) 

tbl$term = gsub("i\\(year, dA_soy, 2003\\)", "", tbl$term)
tbl = tbl %>% add_row(term = "2003",  std.error = 0, statistic  = 0 ,
                 p.value = 0, estimate = 0, conf.low = 0, conf.high = 0)

plot3 = ggplot(data = tbl, aes(x = as.numeric(term), y = estimate)) + 
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1) +
  geom_point(color = "#3B4992") + 
  geom_line(color = "#3B4992") +
  geom_hline(yintercept = 0, linetype='dotted', col = '#BB0021FF', size = 0.8) +
  ylab("Coefficient") + xlab("Year") + 
  geom_vline(xintercept = 2003, color = "black", linetype = "dashed") +
  theme_classic() +
  theme(text = element_text(size = 20))

ggsave("figure3.jpeg", plot3, height = 8, width = 12)

# Placebo: 
df = data %>% filter((amc %in% never_soy) & (amc %in% mun_abovemedian_soy))

model = felm(mort_total_violence ~ i(year,dA_soy, 2003) +  i(year, rural_91, 2003) + i(year,log_y_pc_r_91, 2003) + 
               i(year,log_pop_area_91, 2003) + i(year,alpha_adult_91, 2003) | amc + year| 0 | amc,
             data = df) 

tbl = model %>%
  tidy(conf.int = T) %>% filter(grepl("soy", term)) 

tbl$term = gsub("i\\(year, dA_soy, 2003\\)", "", tbl$term)
tbl = tbl %>% add_row(term = "2003",  std.error = 0, statistic  = 0 ,
                      p.value = 0, estimate = 0, conf.low = 0, conf.high = 0)

plot4 =  ggplot(data = tbl, aes(x = as.numeric(term), y = estimate)) + 
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.1) +
  geom_point(color = "#3B4992") + 
  geom_line(color = "#3B4992") +
  geom_hline(yintercept = 0, linetype='dotted', col = '#BB0021FF', size = 0.8) +
  ylab("Coefficient") + xlab("Year") + 
  geom_vline(xintercept = 2003, color = "black", linetype = "dashed") +
  theme_classic() +
  theme(text = element_text(size = 20))

ggsave("figure4.jpeg", plot4, height = 8, width = 12)

###########################################################################
