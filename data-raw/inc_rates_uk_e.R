library(tbinenglanddataclean)
library(tidyverse)
library(epiR)

## Calculate incidence rates using the tbinenglandataclean - see ?tbinenglanddataclean for details
# See: https://www.samabbott.co.uk/tbinenglanddataclean/
inc_rates_list <- calculate_incidence_ets_lfs_ons(data_path = "~/data-raw/tbinenglanddataclean",
                                                ets_name = "clean_ets_2016.rds",
                                                demo_name = "E_ons_lfs_2000_2016.rds",
                                                return = TRUE,
                                                save = FALSE,
                                                incidence_name = "incidence" ,
                                                grouped_incidence_name = "age_grouped_incidence",
                                                condensed_grouped_incidence_name = "condensed_age_group_incidence",
                                                cases_demo_incidence_name = "cases_demo_incidence")


## Extract just grouped incidence rates for target age groups, UK born
inc_rates_uk_e <- inc_rates_list[[2]] %>%
  filter(`Age group` %in% c("15-19", "20-24", "25-29")) %>%
  filter(CoB %in% "UK born") %>%
  select(-CoB) %>%
  rename(age_group = `Age group`,
         incidence = Incidence,
         lci = Inc_LCI,
         uci = Inc_UCI,
         year = Year) %>%
  mutate(year = as.numeric(year)) %>%
  mutate(age_group = as.character(age_group) %>%
           paste0(" years") %>%
           factor) %>%
  filter(year != max(year))


## Add standard deviation assuming that it is normal and that CI's are 0.95
inc_rates_uk_e <- inc_rates_uk_e %>%
  ungroup %>%
  mutate(sd =  (uci-lci)/(2*qnorm(0.975)))


#' Add to package and keep in data-raw
devtools::use_data(inc_rates_uk_e, overwrite = TRUE)
write_csv(inc_rates_uk_e, "inc_rates_uk_e.csv")


