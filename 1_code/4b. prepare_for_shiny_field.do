********************************************************************************
* Author: Pablo García-Guzmán
* Project: PhD placements project
* Objective: creates dataset for ShinyApp
********************************************************************************

clear
set more off
cap log close
set scheme cleanplots

use "${all}/data_all_unis_proc_fields.dta", clear

* 0. Cosmetic adjustments
	bys primary_field_final: keep if _N >= 20 // keep fields w/ at least 20 obs.
	
	replace primary_field_final = proper(primary_field_final)
	tab primary_field_final
	replace primary_field_final = "Applied Micro" if primary_field_final == "Appliedmicro"
	replace primary_field_final = "IO" if primary_field_final == "Io"
	replace primary_field_final = "Food & Agricultural Econ" if primary_field_final == "Food"
	replace primary_field_final = "Political Economy" if primary_field_final == "Political"
			
* Clean
	replace placement = strtrim(placement)
	replace primary_field_final = strtrim(primary_field_final)
	
	keep year id placement primary_field_final type
	
* Save
	save "${all}/shiny_app_field.dta", replace