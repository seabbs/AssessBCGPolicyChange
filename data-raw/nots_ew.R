
library(tidyverse)
## Load in the data from tbinenglanddataclean
## see www.samabbott.co.uk/tbinenglanddataclean for details

nots_ew <- tbinenglanddataclean::tb_not_ew


## Total nots repiratory/non-respiratory and pulmonary/non-pulmonary
nots_ew <- nots_ew %>%
  gather(key = "tb_type", value = "notifications", -year) %>%
  group_by(year) %>%
  summarise(notifications = sum(notifications, na.rm = T))


#' Add to package and keep in data-raw
devtools::use_data(nots_ew, overwrite = TRUE)
write_csv(nots_ew, "nots_ew.csv")
