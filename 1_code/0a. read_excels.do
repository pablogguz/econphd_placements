***********************************************************
* Author: Pablo García-Guzmán
* Project: PhD placements project
* Objective: builds panel for all departments
***********************************************************

clear
set more off
cap log close
set scheme cleanplots

* Load us data
	foreach uni in columbia harvard princeton stanford ucla chicago northwestern yale mit brown berkeley nyu upenn michigan cornell duke minnesota maryland boston {
		import excel using "${us}/raw/`uni'.xls", firstrow clear
		g uni = "`uni'"
		
		destring year, replace
		tempfile `uni'dta
		save ``uni'dta', replace
		}

* Load eu data
	foreach uni in lse ucl upf bonn warwick eui {
		import excel using "${eu}/raw/`uni'.xlsx", firstrow clear
		g uni = "`uni'"
		
		destring year, replace
		tempfile `uni'dta
		save ``uni'dta', replace
		}
		
* Import data
	clear
	foreach uni in columbia harvard princeton stanford lse ucla chicago northwestern yale mit ucl brown berkeley upf nyu bonn warwick upenn michigan cornell duke minnesota maryland boston eui {
		append using ``uni'dta', force
		}

* Formatting
	foreach var in name field placement {
		format %50s `var'
	}

* Drop field
	drop field

* Clean missings
	drop if placement_type == ""
	
* Anonymize data
	gen id = _n 
	order year id placement placement_type
	
* Encode placement_type
	encode placement_type, gen(type)
	drop placement_type

* Gen variables
	tab type, gen(type_)
	ren type_1 central_bank
	ren type_2 government
	ren type_3 international_inst
	ren type_4 other
	ren type_5 postdoc
	ren type_6 private
	ren type_7 tenure_track
	ren type_8 thinktank
	
	g non_tenure_track = .
	foreach var in central_bank government international_inst other postdoc private thinktank {
		replace non_tenure_track = 1 if `var' == 1
	}
	replace non_tenure_track = 0 if tenure_track == 1
	
* Anonymize
	drop name
	
save "${all}/data_all_unis_proc.dta", replace

tab uni year
order year id placement type
sort type