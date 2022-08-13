********************************************************************************
* Author: Pablo García-Guzmán
* Project: PhD placements project
* Objective: creates dataset for ShinyApp
********************************************************************************

clear
set more off
cap log close
set scheme cleanplots

use "${all}/data_all_unis_proc.dta", clear

* 0. Cosmetic adjustments
	replace uni = proper(uni)
	replace uni = "LSE" if uni == "Lse"
	replace uni = "UCL" if uni == "Ucl"
	replace uni = "UCLA" if uni == "Ucla"
	replace uni = "UPF" if uni == "Upf"
	replace uni = "NYU" if uni == "Nyu"
	replace uni = "MIT" if uni == "Mit"
	replace uni = "UPenn" if uni == "Upenn"
	replace uni = "Boston University" if uni == "Boston"
	replace uni = "EUI" if uni == "Eui"
	
	keep year id placement uni type
	
* Clean
	replace placement = strtrim(placement)
	
* Re-codify type variable
	tab type
	labelbook type

	g typebis = 1 if type == 7
	replace typebis = 2 if type == 5
	replace typebis = 3 if type == 4
	replace typebis = 4 if type == 1
	replace typebis = 5 if type == 2
	replace typebis = 6 if type == 3
	replace typebis = 7 if type == 8
	replace typebis = 8 if type == 6
	
* Save
	save "${all}/data_all_unis_proc_shiny.dta", replace