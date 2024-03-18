*--------------------------------------------------------------------------*

*--------------------------------------------------------------------------*

/* defining path */
cd "E:\trade\PS_2023Solutions (1)\MaterialEx2"

cap log close
log using Workingsample1.log, replace


use dots1960_2005, replace
ssc install reghdfe
ssc install ppmlhdfe
ssc instal  xtpqml
ssc install ftools

/* merge with geographical data */
sort  iso_o iso_d 
merge iso_o iso_d using dist_cepii, nokeep
tab  _merge
drop _merge
*
/* merge with RTA data */
sort  iso_o iso_d year
merge iso_o iso_d year using rta, nokeep
tab  _merge
drop _merge
*
/* merge with RTA data */
sort  iso_o iso_d year
merge iso_o iso_d year using cu, nokeep
tab  _merge
drop _merge
*
/* merge with importer and exporter characteristics */
foreach var in o d{
	rename iso_`var' iso3
	sort iso3 year
	merge iso3 year using pwt, nokeep
	tab _merge
	drop _merge
	g gdp  = cgdp*pop
	rename iso3 iso_`var'
	rename pop  pop_`var'
	rename cgdp cgdp_`var'
	rename pc   pc_`var' 
	rename gdp  gdp_`var'
}
/* generate country pair identifier  */
egen dyad = group(iso_o iso_d)
g ltrade  = log(flow)
g ldist   = log(dist)
g lgdp_o  = log(gdp_o)
g lgdp_d  = log(gdp_d)
tsset dyad year
*
/* generate year dummy  */
tab year, gen(yeard)
/* generate variables for importer and exporter FE */ 
egen imp = group(iso_d)
egen exp = group(iso_o)
/* generate variable for importer*year and exporter*year FE */
egen it = group(exp year)
egen jt = group(imp year)

save working_sample1, replace


**************
* Summary*
**************

ssc install distinct 
use working_sample1, clear
distinct year
distinct iso_o iso_d
sum flow
sum flow if comcur == 1
*
tab comcur 
tab rta

**************
* Looking at the share of countries and trade belonging to RTAs and CUs evolved over the period *
**************

* Share of trade, FTAs *
use working_sample1, clear
bys year: egen tot_trade = sum(flow)
keep if rta == 1
collapse (sum) flow (max) tot_trade, by(year)
g share_trade = flow/tot_trade
twoway line share_trade year 
graph export trade_fta.eps, as(eps) replace

* Share of country pairs, FTA *
use working_sample1, clear
collapse (sum) rta (max) dyad,  by(year)
g share_pair = rta/dyad
twoway line share_pair year 
graph export pairs_fta.eps, as(eps) replace

* Share of trade, currency unions *
use working_sample1, clear
bys year: egen tot_trade = sum(flow)
keep if comcur == 1
collapse (sum) flow (max) tot_trade, by(year)
g share_trade = flow/tot_trade
twoway line share_trade year 
graph export trade_comcur.eps, as(eps) replace

* Share of country pairs, currency unions *
use working_sample1, clear
collapse (sum) comcur (max) dyad,  by(year)
g share_pair = comcur/dyad
twoway line share_pair year 
graph export pairs_comcur.eps, as(eps) replace


**************
* Estimate the currency union effect *
**************

use working_sample1, clear

/* only GDP / distance */* have to install eststo*
eststo: reg ltrade lgdp_o lgdp_d ldist yeard*, cluster(dyad)
/* GDP / distance + regional */
eststo: reg ltrade comcur lgdp_o lgdp_d ldist rta yeard*, cluster(dyad)
/* other trade costs proxies */
eststo: reg ltrade comcur lgdp_o lgdp_d ldist rta contig comlang_off colony yeard*, cluster(dyad)
/*country FE */
eststo: reghdfe ltrade comcur lgdp_o lgdp_d ldist rta contig comlang_off colony yeard*, a(exp imp) vce(cluster dyad)

*
set linesize 250
esttab, mtitles drop(yeard* _cons ) b(%5.3f) se(%5.3f) compress r2 starlevels(c 0.1 b 0.05 a 0.01)  se 
esttab, mtitles drop(yeard* _cons ) b(%5.3f) se(%5.3f) r2  starlevels({$^c$} 0.1 {$^b$} 0.05 {$^a$} 0.01) se tex label  title() 
eststo clear

**************
* Estimating the baseline gravity specification of Rose for each year adding importer    and export fixed effects *
**************

forvalues x=1960(5)2005{
	use working_sample1, clear
	keep if year == `x'
	eststo: reghdfe ltrade comcur ldist rta contig comlang_off colony, a(exp imp) vce(cluster dyad)
}
*
set linesize 250
esttab, mtitles drop() b(%5.3f) se(%5.3f) compress r2 starlevels(c 0.1 b 0.05 a 0.01)  se 
esttab, mtitles drop() b(%5.3f) se(%5.3f) r2  starlevels({$^c$} 0.1 {$^b$} 0.05 {$^a$} 0.01) se tex label title()  
eststo clear

**************
* estimate the baseline gravity equation adding country-pair fixed effects *
**************

use working_sample1, clear
tsset dyad year
eststo: xtreg ltrade comcur lgdp_o lgdp_d rta yeard*,  fe cluster(dyad)
*
set linesize 250
esttab, mtitles drop(yeard*) b(%5.3f) se(%5.3f) compress r2 starlevels(c 0.1 b 0.05 a 0.01)  se 
esttab, mtitles drop(yeard*) b(%5.3f) se(%5.3f) r2  starlevels({$^c$} 0.1 {$^b$} 0.05 {$^a$} 0.01) se tex label  title() 
eststo clear

***************
* Using reghdfe to run regressions controlling for importer*year FE and exporter*year FE *
***************

use working_sample1, clear
eststo: reghdfe ltrade comcur ldist rta contig comlang_off colony, a(it jt) vce(cluster dyad)  
***************
* Using reghdfe to run regressions controlling for importer*year FE and exporter*year FE and dyadic FE *
***************
eststo: reghdfe ltrade comcur rta, a(it jt dyad) vce(cluster dyad)
*
set linesize 250
esttab, mtitles drop() b(%5.3f) se(%5.3f) compress r2 starlevels(c 0.1 b 0.05 a 0.01)  se 
esttab, mtitles drop() b(%5.3f) se(%5.3f) r2  starlevels({$^c$} 0.1 {$^b$} 0.05 {$^a$} 0.01) se tex label title() 
eststo clear

***************
* checking if small countries benefit more *
***************

use working_sample1, clear
*
g comcur_gdpo = comcur*lgdp_o
g comcur_gdpd = comcur*lgdp_d
*
eststo: xtreg ltrade comcur lgdp_o lgdp_d comcur_gdpo comcur_gdpd rta yeard*,  fe cluster(dyad)
*
eststo: reghdfe ltrade comcur comcur_gdpo comcur_gdpd  ldist rta contig comlang_off colony,  a(it jt) vce(cluster dyad)  
*
eststo: reghdfe ltrade comcur comcur_gdpo comcur_gdpd  rta, a(it jt dyad) vce(cluster dyad)
*
eststo: xtpqml flow_dots comcur lgdp_o lgdp_d comcur_gdpo comcur_gdpd  rta yeard*, fe cluster(dyad)
*
set linesize 250
esttab, mtitles drop(yeard*) b(%5.3f) se(%5.3f) compress r2 starlevels(c 0.1 b 0.05 a 0.01)  se 
esttab, mtitles drop(yeard*) b(%5.3f) se(%5.3f) r2  starlevels({$^c$} 0.1 {$^b$} 0.05 {$^a$} 0.01) se tex label title() 
eststo clear

***************
* Using a Poisson estimator to estimate the gravity equation *
***************

use working_sample1, clear

eststo: xtpqml flow_dots lgdp_o  lgdp_d comcur  rta yeard*, fe cluster(dyad)
* Or with exporter x year and importer x year FE
eststo: ppmlhdfe flow_dots comcur  rta, a (it jt dyad) cluster(dyad)

set linesize 250
esttab, mtitles drop(yeard*) b(%5.3f) se(%5.3f) compress r2 starlevels(c 0.1 b 0.05 a 0.01)  se 
esttab, mtitles drop(yeard*) b(%5.3f) se(%5.3f) r2  starlevels({$^c$} 0.1 {$^b$} 0.05 {$^a$} 0.01) se tex label title() 
eststo clear
*

* Investigating the effect of the Euro on trade using this dataset *
**************
* build the eurozone dummy   *
**************
use working_sample1, clear
foreach x in o d{
	g currency_`x' = ""
	/* list countries from J. de Sousa */
	** Andorra
	replace currency_`x'= "EUR" if (iso_`x'=="AND") & (year>=1999)			
	** Austria
	replace currency_`x'= "EUR" if (iso_`x'=="AUT") & (year>=1999)			
	** Belgium
	replace currency_`x'= "EUR" if (iso_`x'=="BEL") & (year>=1999)			
	** Finland
	replace currency_`x'= "EUR" if (iso_`x'=="FIN") & (year>=1999)			
	** France
	replace currency_`x'= "EUR" if (iso_`x'=="FRA") & (year>=1999)			
	** French Guiana
	replace currency_`x'= "EUR" if (iso_`x'=="GUF") & (year>=1999)			
	** Guadeloupe
	replace currency_`x'= "EUR" if (iso_`x'=="GLP") & (year>=1999)			
	** Germany
	replace currency_`x'= "EUR" if (iso_`x'=="DEU") & (year>=1999)			
	** Ireland
	replace currency_`x'= "EUR" if (iso_`x'=="IRL") & (year>=1999)			
	** Italy
	replace currency_`x'= "EUR" if (iso_`x'=="ITA") & (year>=1999)			
	** Luxembourg
	replace currency_`x'= "EUR" if (iso_`x'=="LUX") & (year>=1999)			
	** Martinique
	replace currency_`x'= "EUR" if (iso_`x'=="MTQ") & (year>=1999)			
	** Monaco
	replace currency_`x'= "EUR" if (iso_`x'=="MCO") & (year>=1999)			
	** Netherlands
	replace currency_`x'= "EUR" if (iso_`x'=="NLD") & (year>=1999)			
	** Portugal
	replace currency_`x'= "EUR" if (iso_`x'=="PRT") & (year>=1999)			
	** Reunion 
	replace currency_`x'= "EUR" if (iso_`x'=="REU") & (year>=1999)			
	** San Marino
	replace currency_`x'= "EUR" if (iso_`x'=="SMR") & (year>=1999)			
	** Spain
	replace currency_`x'= "EUR" if (iso_`x'=="ESP") & (year>=1999)			
	** St. Pierre and Miquelon
	replace currency_`x'= "EUR" if (iso_`x'=="SPM") & (year>=1999)			
	** Greece
	replace currency_`x'= "EUR" if (iso_`x'=="GRC") & (year>=2001)			
	** Montenegro 
	replace currency_`x'= "EUR" if (iso_`x'=="MNE") & (year>=2002)			
	** Slovenia
	replace currency_`x'= "EUR" if (iso_`x'=="SVN") & (year>=2007)			
	** Cyprus
	replace currency_`x'= "EUR" if (iso_`x'=="CYP") & (year>=2008)			
	** Malta
	replace currency_`x'= "EUR" if (iso_`x'=="MLT") & (year>=2008)
	** Slovakia
	replace currency_`x'= "EUR" if (iso_`x'=="SVK") & (year>=2009)		
	tab currency_`x'
}
g euro = (currency_o == "EUR" & currency_d == "EUR")
*
tsset dyad year
*
replace comcur = 0 if euro == 1
*
eststo: xtreg ltrade euro comcur lgdp_o lgdp_d rta yeard*,  fe cluster(dyad)
*
eststo: reghdfe ltrade euro comcur ldist rta contig comlang_off colony, a(it jt) vce(cluster dyad)  
*
eststo: reghdfe ltrade euro comcur rta,  a(it jt dyad) vce(cluster dyad)  
*
eststo: xtpqml flow_dots lgdp_o lgdp_d euro comcur rta yeard*, fe cluster(dyad)
*
eststo: ppmlhdfe flow_dots  euro comcur rta, a (it jt dyad) cluster(dyad)

set linesize 250
esttab, mtitles drop(yeard*) b(%5.3f) se(%5.3f) compress r2 starlevels(c 0.1 b 0.05 a 0.01)  se 
esttab, mtitles drop(yeard*) b(%5.3f) se(%5.3f) r2  starlevels({$^c$} 0.1 {$^b$} 0.05 {$^a$} 0.01) se tex label title() 
eststo clear

log close

