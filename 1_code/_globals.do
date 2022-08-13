***********************************************************
* Author: Pablo García-Guzmán
* Project: PhD placements project
* Objective: set paths 
***********************************************************

clear
set more off, perm
cap log close
set logtype text, perm
set min_memory 1g, perm

* Customize your paths accordingly!

if c(username) == "Pablo"{
	global eu "C:/Users/Pablo/Documents/econphd_placements/0_data/eu/"
	global all "C:/Users/Pablo/Documents/econphd_placements/0_data/all/"
	global us "C:/Users/Pablo/Documents/econphd_placements/0_data/us/"
	global do "C:/Users/Pablo/Documents/econphd_placements/1_code/"
	global fig "C:/Users/Pablo/Documents/econphd_placements/2_figures/"
}


