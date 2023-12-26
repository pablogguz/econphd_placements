
/*------------------------------------------------------------------------------
Author: Pablo García-Guzmán
Project: PhD placements project
This do-file: 
	Creates figure on placement distrbution over time
------------------------------------------------------------------------------*/

clear
set more off
cap log close
set scheme cleanplots

**# Load data
	use "${all}/scraped_data_proc.dta", clear

**# Collapse 
	keep if year >= 2012
	#d;
	gcollapse (mean) tenure_track central_bank government international_inst 
					 postdoc private thinktank non_tenure_track, by(year);
	
	#d cr

	gen a = tenure_track
	gen b = a + postdoc
	gen c = b + central_bank
	gen d = c + government
	gen e = d + international_inst
	gen f = e + thinktank
	gen g = f + private

	
* Graph			
	#d;
	twoway area g year, color(red*0.6)
		   || area f year, color(purple*0.7)
		   || area e year, color(black*0.8)
		   || area d year, color(gold*0.8)
		   || area c year, color(midblue*0.2)
		   || area b year, color(midblue*0.4)
		   || area a year, color(midblue*0.8)
		   xlabel(2012(1)2023, glpattern(solid) glcolor("245 245 245")) xtitle("") ytitle("Share of placements")
		   ylabel(0(.2)1, glpattern(solid) glcolor("245 245 245"))
		   legend(rows(1) pos(6) size(vsmall) symxsize(vsmall) order(7 "Tenure-track" 6 "Post-doc"
									  5 "Central banks" 4 "Government" 3 "Intl. organizations" 
									  2 "Think-tanks" 1 "Private sector"));
	#d cr
	*graph export "${fig}/evolution_placements_area_all.pdf", replace
	graph export "${fig}/evolution_placements_area_all.png", replace
