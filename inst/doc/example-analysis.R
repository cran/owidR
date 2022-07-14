## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7.2,
  fig.height = 4,
  error = TRUE
)

## ----setup, message=FALSE-----------------------------------------------------
library(owidR)
library(dplyr)
library(ggplot2)
library(plm)
library(texreg)

## ----internet_search, results='hide'------------------------------------------
owid_search("internet")

## ----import_internet----------------------------------------------------------
internet <- owid("share-of-individuals-using-the-internet", rename = "internet_use")
internet

## ----source_internet----------------------------------------------------------
owid_source(internet)

## ----plot_internet------------------------------------------------------------
owid_plot(internet, filter = "World") +
  labs(title = "Share of the World Population using the Internet") +
  scale_y_continuous(limits = c(0, 100))

## ----map_internet-------------------------------------------------------------
owid_map(internet, year = 2017) +
  labs(title = "Share of Population Using the Internet, 2017")

## ----internet_plot2-----------------------------------------------------------
owid_plot(internet, summarise = FALSE, filter = c("United Kingdom", "Spain", "Russia", "Egypt", "Nigeria")) +
  labs(title = "Share of Population with Using the Internet") +
  scale_y_continuous(limits = c(0, 100), labels = scales::label_number(suffix = "%")) # The labels argument allows you to make it clear that the value is a percentage

## ----democ--------------------------------------------------------------------
democracy <- owid("electoral-democracy", rename = c("electoral_democracy", "vdem_high", "vdem_low"))
democracy

owid_source(democracy)

owid_map(democracy, palette = "YlGn") +
  labs(title = "Electoral Democracy")


## ----import_conf--------------------------------------------------------------
gdp <- owid("gdp-per-capita-worldbank", rename = "gdp")

gov_exp <- owid("total-gov-expenditure-gdp-wdi", rename = "gov_exp")

age_dep <- owid("age-dependency-ratio-of-working-age-population", rename = "age_dep")

unemployment <- owid("unemployment-rate", rename = "unemp")


## ----combine_data-------------------------------------------------------------
data <- internet %>% 
  left_join(democracy) %>% 
  left_join(gdp) %>% 
  left_join(gov_exp) %>% 
  left_join(age_dep) %>% 
  left_join(unemployment)
  

## ----lm_graph-----------------------------------------------------------------
data %>% 
  filter(year == 2015) %>% 
  ggplot(aes(internet_use, electoral_democracy)) +
  geom_point(colour = "#57677D") +
  geom_smooth(method = "lm", colour = "#DC5E78") +
  labs(title = "Relationship Between Internet Use and Polity IV Score", x = "Internet Use", y = "Polity IV") +
  theme_owid()

## ----analysis, results='asis'-------------------------------------------------
fe_model <- plm(electoral_democracy ~ internet_use, data, 
                effect = c("individual"), index = "entity")

fe_model_2 <- plm(electoral_democracy ~ internet_use + gdp + gov_exp + age_dep + unemp, data, 
                  effect = c("individual"), index = "entity")

htmlreg(list(fe_model, fe_model_2))



