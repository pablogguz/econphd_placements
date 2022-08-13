********************************************************************************
* Author: Pablo García-Guzmán
* Project: PhD placements project
* Objective: creates figure on placement distrbution by department
********************************************************************************

clear
set more off
cap log close
set scheme cleanplots

use "${all}/data_all_unis_proc.dta", clear

* Cosmetic adjustments
	replace uni = proper(uni)
	replace uni = "LSE" if uni == "Lse"
	replace uni = "UCL" if uni == "Ucl"
	replace uni = "UCLA" if uni == "Ucla"
	replace uni = "UPF" if uni == "Upf"
	replace uni = "NYU" if uni == "Nyu"
	replace uni = "MIT" if uni == "Mit"
	replace uni = "UPenn" if uni == "Upenn"
	replace uni = "EUI" if uni == "Eui"
	
* Structure (condensed hierarchy)
	preserve
		replace private = 1 if thinktank == 1 // thinktank into private
		replace government = 1 if inlist(1, central_bank, international_inst) // central banks, itl. inst. into gov. (public sector)
		ren government publicsector
		gcollapse (mean) tenure_track private publicsector other postdoc, by(uni)
		
		gen a = tenure_track
		gen b = a + postdoc
		gen c = b + other
		gen d = c + private
		gen e = d + publicsector
		
		gsort -tenure_track
		g uni2 = _n
		labmask uni2, values(uni)
		
		#d;
		twoway bar e uni2 , barw(0.8) color(black)
			   || bar d uni2 , barw(0.8) color(red*0.8)
			   || bar c uni2 , barw(0.8) color(midblue*0.2)
			   || bar b uni2 , barw(0.8) color(midblue*0.4)
			   || bar a uni2 , barw(0.8) color(midblue*0.8)
			   xlabel(1(1)25, valuelabel angle(45) labcolor(black))
			   xtitle("") ytitle("Share of placements")
			   ylab(0(0.2)1)
			   legend(row(1) pos(6) order(5 "Tenure-track" 4 "Post-doc" 3 "Other academic" 
										  2 "Private sector" 1 "Public sector"));
		#d cr
		
		graph export "${fig}/placement_structure_condensed.pdf", replace	
		graph export "${fig}/placement_structure_condensed.png", replace width(2400) 
	restore
		
	
		
	