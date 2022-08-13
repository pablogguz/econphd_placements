********************************************************************************
* Author: Pablo García-Guzmán
* Project: PhD placements project
* Objective: builds panel for departments with information on field
********************************************************************************

clear
set more off
cap log close
set scheme cleanplots

* Load data	
	import excel using "${all}/data_all_unis_fields.xlsx", firstrow clear
	
* Formatting
	foreach var in name placement {
		format %50s `var'
	}

* Anonymize data
	gen id = _n 
	drop name
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

* Final adjustment to fields
	replace primary_field_final = "other" if primary_field_final == "innovation" 

* Save
	save "${all}/data_all_unis_proc_fields.dta", replace

	sort primary_field_final
	tab primary_field_final
	
	