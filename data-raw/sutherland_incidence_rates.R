Cohort <- c('1967-71','1972-1976','1977-1981')
Cohort.Ranges <- c('15-19 years', '20-24 years', '25-29 years')

### Assumed Incidence rates from sutherland et al.
#################### TB rates in unvaccinated people per 100,000
Unvaccinated <- t(matrix(c(22.88, 20.06, 11.58, 10.81, 11.42, 0, 7.59, 0, 0),3,3))
rownames(Unvaccinated) <- Cohort
colnames(Unvaccinated) <- Cohort.Ranges

#################### TB rates in vaccinated people per 100,000
Vaccinated <- t(matrix(c(2.57,4.446,3.83,2.70,3.12,0,1.53,0,0),3,3))
rownames(Vaccinated) <-  Cohort
colnames(Vaccinated) <-  Cohort.Ranges

#################### Notifcations prevented per 100,000
Notifications.Prevented <- Unvaccinated-Vaccinated

################# TB rates in Tuberculin positive people
Tuberculin <- t(matrix(c(19.98, 7.21, 3.83, 14.34, 3.02, 0, 4.5, 0, 0),3,3)) # Real Notifcations rate from previous papers
rownames(Tuberculin) <- Cohort
colnames(Tuberculin) <-Cohort.Ranges

################# TB rates in Inelgible people - assumed to be equal to those in the unvaccinated as not stated in
## published work

Inelgible  <- Unvaccinated

sutherland_incidence_rates <- list(Unvaccinated, Vaccinated, Notifications.Prevented, Tuberculin, Inelgible)
names(sutherland_incidence_rates) <- c("Unvaccinated", "Vaccinated", "Notifications.Prevented", "Tuberculin", "Inelgible")


#' Add to package and keep in data-raw
devtools::use_data(sutherland_incidence_rates, overwrite = TRUE)
save(sutherland_incidence_rates, file = "sutherland_incidence_rates.rda")
