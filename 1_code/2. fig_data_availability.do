
/*------------------------------------------------------------------------------
Author: Pablo García-Guzmán
Project: PhD placements project
This do-file: 
	Create Gantt plot of availability
------------------------------------------------------------------------------*/

clear
set more off
cap log close
set scheme cleanplots

**# Load data 
	use "${all}/scraped_data_proc.dta", clear
	
* Cosmetic adjustments
	replace inst = proper(inst)
	replace inst = "LSE" if inst == "Lse"
	replace inst = "UCL" if inst == "Ucl"
	replace inst = "UCLA" if inst == "Ucla"
	replace inst = "UPF" if inst == "Upf"
	replace inst = "NYU" if inst == "Nyu"
	replace inst = "MIT" if inst == "Mit"
	replace inst = "UPenn" if inst == "Upenn"
	replace inst = "EUI" if inst == "Eui"
	replace inst = "BostonU" if inst == "Bostonu"
	replace inst = "UCSD" if inst == "Ucsd"
	
**# Create variables 

	preserve // field
		keep if field != ""
		
		bys inst: gegen y_max_field = max(year)
		
		bys inst: gegen y_min_field = min(year)
		
		bys inst: keep if _n == 1
		keep inst y_min y_max 
		
		tempfile field 
		save `field'
	restore
	
	bys inst: gegen y_max = max(year)
	
	bys inst: gegen y_min = min(year)
	
	bys inst: keep if _n == 1
	keep inst y_min y_max 
	
	merge 1:1 inst using `field'
	drop _m 
	
	g diff = y_max - y_min
	gsort -y_min diff 
	g order = _n 
	labmask order, values(inst)

	replace y_max = . if y_max == y_max_field & y_min == y_min_field
	replace y_min = . if y_max == . & y_min == y_min_field
	replace y_max = y_min_field if y_min_field > y_min & y_min_field !=. & y_min !=.
	
	gegen y_min_overall = rowmin(y_min y_min_field)
	gegen y_max_overall = rowmax(y_max y_max_field)
	
**# Export chart
	#d;
	twoway (rbar y_min y_max order, barw(.5) horizontal color(midblue%0)) // phantom axis
		   (rbar y_min y_max order, barw(.5) horizontal color(midblue%50) lcolor(midblue%0) yaxis(2))
		   (rbar y_min_field y_max_field order, barw(.5) horizontal color(red%50) lcolor(red%0) yaxis(2))
		   (sc order y_max_overall, mlabel(y_max_overall) msymbol(none) mlabcolor(black%50) mlabsize(vsmall))
		   (sc order y_min_overall, mlabel(y_min_overall) msymbol(none) mlabcolor(black%50) mlabposition(9) mlabsize(vsmall)),
		    ylab(none, nogrid nolabels noticks) ylab(1(1)29, val angle(0) nogrid axis(2) labsize(vsmall))
			ytitle("") ytitle("", axis(2))
			xtitle("Data availability (years)", size(small))
		    xlab(1998(5)2023, nogrid labsize(vsmall))
			legend(pos(7) ring(0) size(vsmall) symxsize(small) order(4 "Field info. unavailable" 5 "Field info. available"))
			plotregion(margin(r+5))
			graphregion(margin(l+5))
			title("{bf}The Econ PhD placements dataset", pos(11) size(3.5))
			subtitle("Web-scraped individual-level data on placements of graduates from top PhD programs in economics", pos(11) size(2.5))
			caption("@pablogguz_ | Last updated: December 2023", color(gs11) size(2) ring(1) pos(7))
			/*note("Notes: Insert notes", color(gs11) size(2))*/;
	#d cr
	graph export "${fig}/data_availability.png", replace width(2400) 


	