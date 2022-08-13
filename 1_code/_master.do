********************************************************************************
* Author: Pablo García-Guzmán
* Project: PhD placements project
* Objective: master dofile
********************************************************************************

clear
set more off, perm
cap log close
set logtype text, perm
set min_memory 1g, perm

* Sequence
	* First: run _globals.do
	* Then:
	rscript using "${do}/_master_Rscript.R"
	do "${do}/0a. read_excels.do"
	do "${do}/0b. read_excels_fields.do"
	do "${do}/1. fig_placement_distribution_overtime.do"
	do "${do}/2. fig_placement_distribution_bydepartment.do"
	do "${do}/3. fig_placement_distribution_byfield.do"
	do "${do}/4a. prepare_for_shiny.do"
	do "${do}/4b. prepare_for_shiny_field.do"
	rscript using "${do}/wordcloud.R"
	