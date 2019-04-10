## Sutherland results
## Total notifications from ending the scheme
sutherland_results <- matrix(c(288, 288, 288, 226, 165, 165, 208,
                                           130,  90, 181, 128,  77, 128, 115,
                                           80,  80,  80,  72), 3, 6)

colnames(sutherland_results) <- c('1988', '1993', '1998', '2003', '2008', '2013')
rownames(sutherland_results) <- c('1986','1991','1996')


#' Add to package and keep in data-raw
devtools::use_data(sutherland_results, overwrite = TRUE)
readr::write_csv(as.data.frame(sutherland_results), "sutherland_results.csv")
