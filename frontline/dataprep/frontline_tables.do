set more 1

/*
File: frontline_tables.do
Date: May 18, 2020
Desc: industry / occupation of frontline workers
*/


use acs_1418_frontline.dta, clear
	
cap rm `"tables_all.xlsx"'


/* additional recoding */

gen nonw=0 if wbhao~=.
replace nonw=1 if wbhao~=1
lab var nonw "Non-white"


/* tables */

tabout flind1 [fw=perwgt] if lfstat==1 ///
	using table_cvl.xls, c(freq col) f(0c 1) clab(_) font(bold) replace	
tab flind1 [fw=perwgt] if lfstat == 1 

tabout lfstat [fw=perwgt] using table_cvl.xls, c(freq) f(0c) append	
tab lfstat [fw=perwgt]

tabout female ftpt wbhao forborn educ age50 hmown pubtran poor pov200 hins ///
	hhoc_b hhsenior_b flind1 [fw=perwgt] if lfstat==1 ///
	using table_cvl.xls, c(col) f(1) clab(_) font(bold) append
tab female flind1 [fw=perwgt] if lfstat == 1, col	
	
tabout female ftpt wbhao forborn educ age50 hmown pubtran poor pov200 hins ///
	hhoc_b hhsenior_b flind [fw=perwgt] if lfstat==1 ///
	using table_cvl.xls, c(col) f(1) clab(_) font(bold) append
tab female flind [fw=perwgt] if lfstat == 1, col		
	
foreach v in female nonw forborn pov200 {
	tabout flind_dd `v' [fw=perwgt] if lfstat==1 & flind==1 /// 
		using table_cvl.xls, c(row) f(1) clab(_) font(bold) append
}
	* occupation crosstab for each industry
log using indocc.log, replace
forval i = 1/6 {
	tab socp18 [fw=perwgt] if flind1==`i' & lfstat==1, sort
	tab socp18 female [fw=perwgt] if flind1==`i' & lfstat==1, row	
}
log close	
	
log using indocc2.log, replace
foreach v in female nonw forborn pov200 {
	forval i = 1/6 {
		tabsort socp18 `v' [fw=perwgt] if lfstat==1 & flind1==`i', row nofreq
	}
}
log close		



/* 
Copyright 2020 CEPR, Hye Jin Rho Hayley Brown
Center for Economic and Policy Research
1611 Connecticut Avenue, NW Suite 400
Washington, DC 20009
Tel: (202) 293-5380
Fax: (202) 588-1356
http://www.cepr.net
This file and all related programs are free software. You can redistribute the
program or modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2 of the
License, or (at your option) any later version.
This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.
You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307
USA.
*/
