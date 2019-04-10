## Assumption from sutherland et al.
sutherland_gen_time <- 2

#' Add to package and keep in data-raw
devtools::use_data(sutherland_gen_time, overwrite = TRUE)
readr::write_csv(as.data.frame(sutherland_gen_time), "sutherland_gen_time.csv")
