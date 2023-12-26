
/*------------------------------------------------------------------------------
Author: Pablo García-Guzmán
Project: PhD placements project
This do-file: 
	 Creates figure on placement distrbution by field
------------------------------------------------------------------------------*/

clear
set more off
cap log close
set scheme cleanplots

**# Load data
	use "${all}/scraped_data_proc.dta", clear

**# Cosmetic adjustments
	bys field_type: keep if _N >= 20  // keep fields w/ at least 20 obs.
	
	replace field_type = proper(field_type)
	tab field_type
	replace field_type = "Applied Micro" if field_type == "Appliedmicro"
	replace field_type = "IO" if field_type == "Io"
	replace field_type = "Food & Agricultural Econ" if field_type == "Food"
	replace field_type = "Political Economy" if field_type == "Political"
	drop if field_type == "Other"
	drop if field_type == ""
	
**# Plot
	keep if year >= 2016
	mylabels 0(20)100, myscale(@) local(pctlabel) suffix("%")
	
	foreach var in tenure_track private {
		replace `var' = `var' * 100
	}
	
	#d
	graph bar (mean) tenure_track private, over(field_type, sort(1) desc label(angle(45) labsize(vsmall))) 
					 stack ylab(`pctlabel') bar(2, color(red%70) lcolor(red%0))
					 bar(1, color(midblue%70) lcolor(midblue%0)) ytitle("Share of placements", size(small))
					 legend(pos(2) ring(0) size(vsmall) symxsize(small) order(1 "Tenure-track" 2 "Private sector"))
					 blabel(bar, pos(center) size(vsmall) format(%9.0fc) color(white));
	#d;
	graph export "${fig}/combined_by_field.png", replace width(2400) 
