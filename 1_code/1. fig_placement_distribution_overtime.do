********************************************************************************
* Author: Pablo García-Guzmán
* Project: PhD placements project
* Objective: creates figure on placement distrbution over time
********************************************************************************

clear
set more off
cap log close
set scheme cleanplots

use "${all}/data_all_unis_proc.dta", clear

******************************* All categories *********************************

* Drop departments for which we only have info since 2016 to avoid sample inconsistencies
	drop if inlist(uni,"ucl","upf","warwick") 

* Graph
	preserve
		#d;
		gcollapse (mean) tenure_track central_bank government international_inst other 
						 postdoc private thinktank non_tenure_track, by(year);
		
		#d cr

		gen a = tenure_track
		gen b = a + postdoc
		gen c = b + other
		gen d = c + central_bank
		gen e = d + government
		gen f = e + international_inst
		gen g = f + thinktank
		gen h = g + private

			
		#d;
		twoway area h year, color(red*0.8)
			   || area g year, color(gs12*0.9)
			   || area f year, color(purple*0.7)
			   || area e year, color(black*0.8)
			   || area d year, color(gold*0.8)
			   || area c year, color(midblue*0.2)
			   || area b year, color(midblue*0.4)
			   || area a year, color(midblue*0.8)
			   xlabel(2012(1)2022, glpattern(solid) glcolor("245 245 245")) xtitle("") ytitle("Share of placements")
			   ylabel(0(.2)1, glpattern(solid) glcolor("245 245 245"))
			   legend(row(2) pos(6) order(8 "Tenure-track" 7 "Post-doc" 6 "Other academic" 
										  5 "Central banks" 4 "Government" 3 "Intl. organizations" 
										  2 "Think-tanks" 1 "Private sector"));
		#d cr
		graph export "${fig}/evolution_placements_area_all.pdf", replace
		graph export "${fig}/evolution_placements_area_all.png", replace
	restore
