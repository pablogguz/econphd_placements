
/*------------------------------------------------------------------------------
Author: Pablo García-Guzmán
Project: PhD placements project
This do-file: 
	 Creates figure on placement distrbution by department
------------------------------------------------------------------------------*/

clear
set more off
cap log close
set scheme cleanplots

**# Load data
	use "${all}/scraped_data_proc.dta", clear

* Cosmetic adjustments
	ren inst uni
	replace uni = proper(uni)
	replace uni = "LSE" if uni == "Lse"
	replace uni = "UCL" if uni == "Ucl"
	replace uni = "UCLA" if uni == "Ucla"
	replace uni = "UPF" if uni == "Upf"
	replace uni = "NYU" if uni == "Nyu"
	replace uni = "MIT" if uni == "Mit"
	replace uni = "UPenn" if uni == "Upenn"
	replace uni = "EUI" if uni == "Eui"
	replace uni = "BostonU" if uni == "Bostonu"
	replace uni = "UCSD" if uni == "Ucsd"
	
* Structure (condensed hierarchy)
	drop if thinktank == 1 // drop
	keep if year >= 2012 
	
	replace government = 1 if inlist(1, central_bank, international_inst) // central banks, itl. inst. into gov. (public sector)
	ren government publicsector
	gcollapse (mean) tenure_track private publicsector postdoc, by(uni)
	
	gen a = tenure_track
	gen b = a + postdoc
	gen c = b + private
	gen d = c + publicsector
	
	gsort -tenure_track
	g uni2 = _n
	labmask uni2, values(uni)
	
	#d;
	twoway bar d uni2 , barw(0.8) color(red*0.8)
		   || bar c uni2 , barw(0.8) color(midblue*0.2)
		   || bar b uni2 , barw(0.8) color(midblue*0.4)
		   || bar a uni2 , barw(0.8) color(midblue*0.8)
		   xlabel(1(1)28, valuelabel angle(45) labcolor(black) labsize(vsmall))
		   xtitle("") ytitle("Share of placements", size(small))
		   ylab(0(0.2)1)
		   legend(row(4) pos(4) size(vsmall) symxsize(vsmall) order(4 "Tenure-track" 3 "Post-doc"
									  2 "Private sector" 1 "Public sector"));
	#d cr
	*graph export "${fig}/placement_structure_condensed.pdf", replace	
	graph export "${fig}/placement_structure_condensed.png", replace width(2400) 


		
	