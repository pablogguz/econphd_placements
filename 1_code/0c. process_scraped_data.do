
/*------------------------------------------------------------------------------
Author: Pablo García-Guzmán
Project: PhD placements project
This do-file: 
	Process and cleans scraped data
------------------------------------------------------------------------------*/

clear
set more off
cap log close
set scheme cleanplots

**# Load data 
	use "${all}/scraped_data_raw_wfield.dta", clear
	
**# Clean
	format placement %-50s
	lab var placement "Placement"
	
	lab var placement_type "Placement type"
	ren placement_type type
	
	lab var inst "Institutiton"
	
	replace region = upper(region)
	lab var region "Region"
	
	lab var name "Name of candidate"
	lab var field "Field of candidate (categories, based on primary field)"
	
	lab var primary_field "Primary field"
	lab var secondary_field "Secondary field"
	
	lab var year "Year of placement"
	
	tab type, gen(type_)
	foreach var of varlist type_* {
		local lab: variable label `var'
		di "`lab'"
		
		local lab : subinstr local lab "type==" ""
		local lab = proper("`lab'")
		di "`lab'"
		
		lab var `var' "`lab'"
	}
	
	ren type_1 central_bank
	ren type_2 government
	ren type_3 international_inst
	ren type_4 postdoc
	ren type_5 private
	ren type_6 tenure_track
	ren type_7 thinktank
	
	g non_tenure_track = .
	foreach var in central_bank government international_inst postdoc private thinktank {
		replace non_tenure_track = 1 if `var' == 1
	}
	replace non_tenure_track = 0 if tenure_track == 1
	lab var non_tenure_track "Non-tenure-track"

**# Append data for before 2018 for Chicago
	preserve
		import excel "$us/raw/chicago_before2019/chicago_raw_before2019.xlsx", clear first
		drop if year ==. 
		
		g inst = "chicago"
		
		tempfile chicago 
		save `chicago'
	restore
	
	append using `chicago'
		
**# Check duplicates
	bys name inst: g dup = _N 
	replace dup = . if name == ""
	tab dup 
	
	* Keep only first placement
	bys name inst (year): g n = _n
	order name year n
	drop if n > 1 & dup !=. 
	drop dup n 

**# Save
	save "${all}/scraped_data_proc.dta", replace
