## Sutherland assumed annual TB decrease in each age group
sutherland_TB_decrease <- c(9,9,9)

devtools::use_data(sutherland_TB_decrease, overwrite = TRUE)
readr::write_csv(as.data.frame(sutherland_TB_decrease), "sutherland_TB_decrease.csv")



