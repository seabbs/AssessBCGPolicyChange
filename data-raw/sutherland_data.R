
##################### Build 1983 data
Cohort <- c('1967-71','1972-1976','1977-1981')
Cohort.Ranges <- c('15-19 years', '20-24 years', '25-29 years')

##################### Population of Cohorts
Cohort.Pop <- t(c(2970000,3448000,3666000))
colnames(Cohort.Pop) <- Cohort

################### Estimated Efficacy of vaccine per 100,000
BCG.efficacy <- c(80,73,67)
names(BCG.efficacy) <- Cohort.Ranges

################## No. of BCG vaccinations given:

BCG.vaccinations <- c(2.210,2.548,2.892,2.55,2.00,2.10)  #Whilst Sutherland states these are 75% of populations, for years surveyed they are actual data.
BCG.vaccinations <- BCG.vaccinations*10^6
names(BCG.vaccinations) <- c('1967-71','1972-1976','1977-1981','1982-86','1987-91','1992-1996')

################# No. of Negative unvaccinated people: set to be the average of collected data at 11%
BCG.Unvaccinated <- c(0.465*0.95, 0.664 * 0.943, 0.585 * 0.936)
BCG.Unvaccinated <- BCG.Unvaccinated*10^6
BCG.Unvaccinated <- c(BCG.Unvaccinated, BCG.vaccinations[4:6]* 4/3*0.11)
names(BCG.Unvaccinated) <- c('1967-71','1972-1976','1977-1981','1982-86','1987-91','1992-1996')

################# No. of Tuberculin postive people: set to be average of collected data at 8%
BCG.Tuberculin <- c(0.336*0.95, 0.291 * 0.943, 0.241 * 0.936)
BCG.Tuberculin <- BCG.Tuberculin*10^6
BCG.Tuberculin <- c(BCG.Tuberculin, BCG.vaccinations[4:6]* 4/3*0.08)
names(BCG.Tuberculin) <- c('1967-71','1972-1976','1977-1981','1982-86','1987-91','1992-1996')

################# No. of Tuberculin postive people: set to be average of collected data at 6 %
BCG.Inelgible <- c(0.248*0.95, 0.201 * 0.943, 0.155 * 0.936)
BCG.Inelgible <- BCG.Inelgible*10^6
BCG.Inelgible <- c(BCG.Inelgible, BCG.vaccinations[4:6]*4/3*0.06)
names(BCG.Inelgible) <- c('1967-71','1972-1976','1977-1981','1982-86','1987-91','1992-1996')

################### Summation of all data
sutherland_data <- list(Cohort, Cohort.Ranges, Cohort.Pop, BCG.efficacy, BCG.vaccinations, BCG.Unvaccinated, BCG.Tuberculin, BCG.Inelgible)
names(sutherland_data) <- c('Cohort', 'Cohort.Ranges', 'Cohort.Pop', 'BCG.efficacy','BCG.vaccinations', 'BCG.Unvaccinated', 'BCG.Tuberculin', 'BCG.Inelgible')


#' Add to package and keep in data-raw
devtools::use_data(sutherland_data, overwrite = TRUE)
save(sutherland_data, file = "sutherland_data.rda")
