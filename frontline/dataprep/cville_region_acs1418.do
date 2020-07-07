/*
INITIAL
File:	cepr_acs_read_1418.do
Date:	May 30, 2020, CEPR ACS 2014-2018 Beta Version
Desc:	Reads and pre-processes raw ACS 5-year sample (2014-2018) data

MODIFIED
File:	cville_region_acs1418.do
Date:	May 19, 2020
Desc:	Read and process ACS 5-year sample data for Cville region PUMAs
*/


/* set directories */
cd "/Users/mpc8t/Box Sync/mpc/dataForDemocracy/equitymetrics/covid_dash/dataprep/vapums"



/* read raw ACS person and household records */

/* read person records */

cd csv_pva
qui insheet using psam_p51.csv, comma names clear
sort serialno sporder
keep if puma == 51090 | puma == 51089
compress
cd ..
save "psam_p51.dta", replace

/* read household records */

cd csv_hva
qui insheet using psam_h51.csv, comma names clear
sort serialno
keep if puma == 51090 | puma == 51089
compress
cd ..
save "psam_h51.dta", replace

merge serialno using "psam_p51"

drop rt /* drop record type, since the P and H are now combined */
assert _merge==1 /* vacant houses */ | _merge==3
	/* keep vacant housing units for subsequent analysis of vacancies */
drop _merge

sort serialno sporder
compress
save "acs_1418_all", replace
