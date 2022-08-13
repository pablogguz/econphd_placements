********************************************************************************
* Author: Pablo García-Guzmán
* Project: PhD placements project
* Objective: creates figure on placement distrbution by field
********************************************************************************

clear
set more off
cap log close
set scheme cleanplots

use "${all}/data_all_unis_proc_fields.dta", clear

* 0. Cosmetic adjustments
	bys primary_field_final: keep if _N >= 20  // keep fields w/ at least 20 obs.
	
	replace primary_field_final = proper(primary_field_final)
	tab primary_field_final
	replace primary_field_final = "Applied Micro" if primary_field_final == "Appliedmicro"
	replace primary_field_final = "IO" if primary_field_final == "Io"
	replace primary_field_final = "Food & Agricultural Econ" if primary_field_final == "Food"
	replace primary_field_final = "Political Economy" if primary_field_final == "Political"
					
* 1. Tenure-track placement rate by uni (all periods since 2016)

	keep if year >= 2016
	gcollapse (mean) tenure_track private, by(primary_field_final)

	* Tenure-track
	gsort -tenure_track
	g order = _n
	labmask order, values(primary_field_final)
	global max = _N // get N of unis
	
	replace tenure_track = tenure_track * 100
	mylabels 0(20)80, suffix(%) myscale(@) local(foo)
	
	#d;
	twoway bar tenure_track order, barw(0.8) bcolor(red%60)
					  xlabel(1(1)$max, glpattern(solid) glcolor("245 245 245") valuelabel angle(45))
					  ylabel(`foo', glpattern(solid) glcolor("245 245 245"))
					  xtitle("")
					  ytitle("")
					  legend(off)
					  title("Tenure-track placement rate")
					  name(panela, replace);
	#d cr
	drop order
	
	* Private sector
	gsort -private
	g order = _n
	labmask order, values(primary_field_final)
	global max = _N // get N of unis
	
	replace private = private * 100
	mylabels 0(20)80, suffix(%) myscale(@) local(foo)
	
	#d;
	twoway bar private order, barw(0.8) bcolor(red%60)
					  xlabel(1(1)$max, glpattern(solid) glcolor("245 245 245") valuelabel angle(45))
					  ylabel(`foo', glpattern(solid) glcolor("245 245 245"))
					  xtitle("")
					  ytitle("")
					  legend(off)
					  title("Private sector placement rate")
					  name(panelb, replace);
	#d cr
	
    graph combine panela panelb, imargin(b=0 t=0)
	graph export "${fig}/combined_by_field.png", replace width(2400)
	
	