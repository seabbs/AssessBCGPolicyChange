default: all
RDIR = .

all: sutherland_data.rda sutherland_results.csv sutherland_gen_time.csv sutherland_TB_decrease.csv sutherland_incidence_rates.rda nots_ew.csv inc_rates_uk_e.csv

## Update sutherland data
sutherland_data.rda: sutherland_data.R
			Rscript sutherland_data.R


## Update sutherland results
sutherland_results.csv: sutherland_results.R
			Rscript sutherland_results.R

## Update sutherland generation time
sutherland_gen_time.csv: sutherland_gen_time.R
			Rscript sutherland_gen_time.R


## Update sutherland TB decrease
sutherland_TB_decrease.csv: sutherland_TB_decrease.R
			Rscript sutherland_TB_decrease.R


## Update sutherland incindence rates
sutherland_incidence_rates.rda: sutherland_incidence_rates.R
			Rscript sutherland_incidence_rates.R


## Update sutherland incindence rates
nots_ew.csv: nots_ew.R
			Rscript nots_ew.R

## Import, clean and munge incidence rates for white UK born in England and Wales
inc_rates_uk_e.csv: inc_rates_uk_e.R
			Rscript inc_rates_uk_e.R
