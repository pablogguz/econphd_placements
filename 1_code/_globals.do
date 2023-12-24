
/*------------------------------------------------------------------------------
Author: Pablo García-Guzmán
Project: PhD placements project
This do-file: 
	Set paths 
------------------------------------------------------------------------------*/

clear
set more off, perm
cap log close
set logtype text, perm
set min_memory 1g, perm

if c(username) == "pablo"{
	global eu "C:/Users/pablo/Documents/GitHub/econphd_placements/0_data/eu/"
	global all "C:/Users/pablo/Documents/GitHub/econphd_placements/0_data/all/"
	global us "C:/Users/pablo/Documents/GitHub/econphd_placements/0_data/us/"
	global do "C:/Users/pablo/Documents/GitHub/econphd_placements/1_code/"
	global fig "C:/Users/pablo/Documents/GitHub/econphd_placements/2_figures/"
}


