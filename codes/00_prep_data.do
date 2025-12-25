********************************************
* Do File
* 
* Function: Prep of data
* IPUMS Data extraction
* This extraction includes 1960-2019 data
* Note: 6 files included for 1970
*
* substantive variables: 
* - occupation (OCC, OCC1950, OCC1990, OC2010)
* - industry (IND, IND1950, IND1990)
* - number of children below 5, sex, marital status, race
* - educational attainment, employment status, labor force status, class of work
* - weeks worked, hours worked
* - income
********************************************


*****************************
* Load data & create master *
*****************************

* The original IPUMS data can be downloaded using ipumsr package
* with the file named extract.xml in the data folder
* The codes below were written when Stata was still the main language
* We switched to R for later analyses

* use "your dta file name", clear

* sample selection & corrections

* drop non-usable observations
keep if empstat==1  
	// keep those at work (also "has job, not working") (note: unemployed = 20)
	// Note: this variable has about 25% missings overall, all of which come from respondents below the age of 16
drop if occ2010 >= 9800  // Military, Unemployed Unknown
drop if incwage==0  // zero income (there are other cases with very little income, this will be corrected later)
drop if classwkrd == 26 | classwkrd==29   // unpaid family workers + armed forces
drop if ind1990==0 | ind1990==992 | ind1990==999  // NA, Last worked 1984 or earlier, Did not respond
drop if weight == 0  // a few thousand obs we cannot use because weight = 0

* note: we so far include all employed and self-employed

save "processed/master_1_00.dta", replace

***************************
* Clean & construct var's *
***************************

use "processed/master_1_00.dta", clear

* build aggregate industry variable
	gen ind1990_agg=.
	label var ind1990_agg "industrial codes, 1990 scheme, aggregated"
	label define ind1990_agg 0 "(0) Agriculture, forestry, and fisheries" 1 "(1) Mining" 2 "(2) Construction" 3 "(3) Manufacturing" 4 "(4) Transportation, communications, and other public utilities" 5 "(5) Wholesale trade" 6 "(6) Retail trade" 7 "(7) Finance, insurance, and real estate" 8 "(8) Business and repair services" 9 "(9) Personal services" 10 "(10) Entertainment and recreation services" 11 "(11) Professional and related services" 12 "(12) Public administration" 13 "(13) Active duty military"
	label values ind1990_agg ind1990_agg
	replace ind1990_agg=0 if ind1990>=10 & ind1990<=32
	replace ind1990_agg=1 if ind1990>=40 & ind1990<=50
	replace ind1990_agg=2 if ind1990>=60 & ind1990<=60
	replace ind1990_agg=3 if ind1990>=100 & ind1990<=392
	replace ind1990_agg=4 if ind1990>=400 & ind1990<=472
	replace ind1990_agg=5 if ind1990>=500 & ind1990<=571
	replace ind1990_agg=6 if ind1990>=580 & ind1990<=691
	replace ind1990_agg=7 if ind1990>=700 & ind1990<=712
	replace ind1990_agg=8 if ind1990>=721 & ind1990<=760
	replace ind1990_agg=9 if ind1990>=761 & ind1990<=791
	replace ind1990_agg=10 if ind1990>=800 & ind1990<=810
	replace ind1990_agg=11 if ind1990>=812 & ind1990<=893
	replace ind1990_agg=12 if ind1990>=900 & ind1990<=932
	replace ind1990_agg=13 if ind1990>=940 & ind1990<=960
	
	* generate variables: industry dummies
	ta ind1990_agg, gen(ind1990_agg)


* Clean substantive var's (individual-level)

	* construct consistent hours per week var
	// From 1980 onwards, we have "uhrswork", which are the "usual hours worked per week".
	// Before that, we only have a categorical variable for "hours worked last week" (hrswork2).
	gen hours=.
	replace hours=7.5 if hrswork2==1 & year<1980  // categorical before 1980
	replace hours=22 if hrswork2==2 & year<1980
	replace hours=32 if hrswork2==3 & year<1980
	replace hours=37 if hrswork2==4 & year<1980
	replace hours=40 if hrswork2==5 & year<1980
	replace hours=44.5 if hrswork2==6 & year<1980
	replace hours=54 if hrswork2==7 & year<1980
	replace hours=60 if hrswork2==8 & year<1980
	replace hours=uhrswork if year>=1980  // continuous from 1980 on
	drop if hours == 0 | hours == .  // the missing values are from 1960 and 1970 where we have some missings in hrswork2

	* construct continuous weeks var
	// We have a categorical variable for "weeks worked last year" for all years (wkswork2).
	// There is also a continuous variable (wkswork1), but it is only available for selected years.
	// In order to treat all years identically, we only use wkswork2.
	gen weeks=.
	replace weeks=7 if wkswork2==1 & weeks==.
	replace weeks=20 if wkswork2==2 & weeks==.
	replace weeks=33 if wkswork2==3 & weeks==.
	replace weeks=43.5 if wkswork2==4 & weeks==.
	replace weeks=48.5 if wkswork2==5 & weeks==.
	replace weeks=51 if wkswork2==6 & weeks==.

	* construct hourly labor income var
	// In order to deflate current USD, there are different indices one can use.
	// The IPUMS data contain, by default, a deflator from the Bureau of Labor Statistics, based on the CPI-U.
	// See here: https://usa.ipums.org/usa-action/variables/CPI99#description_section
	// However, there is an argument that this leads to under-estimated wage growth over time.
	// See here: https://www.forbes.com/sites/scottwinship/2015/06/15/debunking-disagreement-over-cost-of-living-adjustment
	// We, hence, deflate wages with an alternative index (based on the PCE, Personal Consumption Expenditures), by the Bureau of Economic Analysis in the Commerce Department
	
		// compute hourly wage in current USD
		gen hours_ann = weeks*hours // annual hours
		gen income_current = incwage / hours_ann
		label var income_current "current hourly labor income"
		drop if income_current == .
	
		// merge PCE and CPI-U-RS Indices
		// Income year is not always identical with survey year (https://usa.ipums.org/usa/cpi99.shtml)
		gen income_year = .
		replace income_year = 1959 if sample == 196002  // 1960 5%
		replace income_year = 1969 if sample >= 197001 & sample <= 197006 // six 1% 1970 samples
		replace income_year = 1979 if sample == 198001  // 1980 5%
		replace income_year = 1989 if sample == 199001  // 1990 5%
		replace income_year = 1999 if sample == 200001  // 2000 5%
		replace income_year = 2010 if sample == 201001  // 2010 ACS
		replace income_year = 2019 if sample == 201901  // 2019 ACS
		merge m:1 income_year using "PCE/PCE_annual_1959_to_2020.dta", keep(match) nogen
		merge m:1 income_year using "CPI-U-RS/CPI-U-RS_1978_to_2020.dta", keep(match master) nogen
	
		* compute real hourly wage (using CPI-U)
		// We change wages to 2019. The factor to convert 1999 to 2019 USD is 1.534 = 1 / 0.652
		// See transformation here: https://usa.ipums.org/usa/cpi99.shtml
		gen income_cpi = income_current * cpi99 * 1.534
		label var income_cpi "real hourly labor income in previous year, 2019, based on CPI-U"
		
		* compute real hourly wage (using PCE)
		// We use 109.851 in the numerator in order to convert to 2019 instead of 2012 USD
		gen income_pce = income_current * (109.851 / pce_index)
		label var income_pce "real hourly labor income in previous year, 2019, based on PCE"	
		
		* compute real hourly wage (using CPI-U-RS)
		// We use 109.851 in the numerator in order to convert to 2019 USD
		gen income_cpi_urs = income_current * (376.5 / cpi_u_rs_index)
		label var income_cpi_urs "real hourly labor income in previous year, 2019, based on CPI-U-RS"
		
		* compute real hourly wage in 1999 USD (using CPI-U)
		// This is for robustness checks only, so we can check differences to our previous methodology using 1999 real wages
		gen income_cpi_99 = income_current * cpi99
		label var income_cpi_99 "real hourly labor income in previous year, 1999, based on CPI-U"		
		
		* compare the wage dynamics with the two indices (running will collapse the data set)
// 		collapse (mean) income_cpi income_cpi_urs income_pce [pw=weight], by(year)
// 		tw (connected income_cpi year, sort)  (connected income_cpi_urs year, sort) (connected income_pce year, sort), ///
// 			ytitle("Mean hourly wage in 2019 USD") xtitle("Year") ///
// 			xlabel(1960 1970 1980 1990 2000 2010 2019) ///
// 			legend(order(1 "Wage (CPI-U adjusted)" 2 "Wage (CPI-U-RS adjusted)" 3 "Wage (PCE adjusted)"))
		
		
		
	* income_cpi: top- and bottom-coding of real income
		/* 
		Applying upper and lower bounds to restrict extreme values (producing unrealistic spikes in time-series)
		We apply these bounds by year: 
		- Upper bound = median * 20
		- Lower bound = median / 20
		Overall, this affects 93,071 of 24,540,943 (0.38%)
		*/
	gen income_cpi_cap = income_cpi
	label var income_cpi_cap "real hourly labor income in previous year, 2019 USD, based on CPI-U, top/bottom-coded"
	local count_bottom=0
	local count_top=0
	foreach year in 1960 1970 1980 1990 2000 2010 2019 {
		quietly sum income_cpi if year == `year', d
		local median = r(p50)
		local min = `median' / 20
		local max = `median' * 20
		quietly sum income_cpi_cap if income_cpi_cap < `min' & year == `year'
		local count_bottom = `count_bottom' + r(N)
		quietly sum income_cpi_cap if income_cpi_cap > `max' & year == `year'
		local count_top = `count_top' + r(N)
		replace income_cpi_cap = `min' if income_cpi_cap < `min' & year == `year'
		replace income_cpi_cap = `max' if income_cpi_cap > `max' & year == `year'
	}
	local count_total = `count_bottom' + `count_top'
	di "Bottom coded total of `count_bottom' observations"
	di "Top coded total of `count_top' observations"
	di "Total: `count_total' observations"				

	
	* income_cpi_urs: top- and bottom-coding of real income
		// Overall, this affects 57,245 of 18,225,361 observations from 1980 to 2019 (0.31%)
	gen income_cpi_urs_cap = income_cpi_urs
	label var income_cpi_urs_cap "real hourly labor income in previous year, 2019 USD, based on CPI-U-RS, top/bottom-coded"
	local count_bottom=0
	local count_top=0
	foreach year in 1980 1990 2000 2010 2019 {
		quietly sum income_cpi_urs if year == `year', d
		local median = r(p50)
		local min = `median' / 20
		local max = `median' * 20
		quietly sum income_cpi_urs_cap if income_cpi_urs_cap < `min' & year == `year'
		local count_bottom = `count_bottom' + r(N)
		quietly sum income_cpi_urs_cap if income_cpi_urs_cap > `max' & year == `year'
		local count_top = `count_top' + r(N)
		replace income_cpi_urs_cap = `min' if income_cpi_urs_cap < `min' & year == `year'
		replace income_cpi_urs_cap = `max' if income_cpi_urs_cap > `max' & year == `year'
	}
	local count_total = `count_bottom' + `count_top'
	di "Bottom coded total of `count_bottom' observations"
	di "Top coded total of `count_top' observations"
	di "Total: `count_total' observations"		
		
		
	* income_pce: top- and bottom-coding of real income
		// Overall, this affects 93,071 of 24,540,943 (0.38%) [identical with CPI-U]
	gen income_pce_cap = income_pce
	label var income_pce_cap "real hourly labor income in previous year, 2019 USD, based on PCE, top/bottom-coded"
	local count_bottom=0
	local count_top=0
	foreach year in 1960 1970 1980 1990 2000 2010 2019 {
		quietly sum income_pce if year == `year', d
		local median = r(p50)
		local min = `median' / 20
		local max = `median' * 20
		quietly sum income_pce_cap if income_pce_cap < `min' & year == `year'
		local count_bottom = `count_bottom' + r(N)
		quietly sum income_pce_cap if income_pce_cap > `max' & year == `year'
		local count_top = `count_top' + r(N)
		replace income_pce_cap = `min' if income_pce_cap < `min' & year == `year'
		replace income_pce_cap = `max' if income_pce_cap > `max' & year == `year'
	}
	local count_total = `count_bottom' + `count_top'
	di "Bottom coded total of `count_bottom' observations"
	di "Top coded total of `count_top' observations"
	di "Total: `count_total' observations"		
		
		
	* income_cpi_99: top- and bottom-coding of real income
	gen income_cpi_99_cap = income_cpi_99
	label var income_cpi_99_cap "real hourly labor income in previous year, 1999 USD, based on CPI-U, top/bottom-coded"
	local count_bottom=0
	local count_top=0
	foreach year in 1960 1970 1980 1990 2000 2010 2019 {
		quietly sum income_cpi_99 if year == `year', d
		local median = r(p50)
		local min = `median' / 20
		local max = `median' * 20
		quietly sum income_cpi_99_cap if income_cpi_99_cap < `min' & year == `year'
		local count_bottom = `count_bottom' + r(N)
		quietly sum income_cpi_99_cap if income_cpi_99_cap > `max' & year == `year'
		local count_top = `count_top' + r(N)
		replace income_cpi_99_cap = `min' if income_cpi_99_cap < `min' & year == `year'
		replace income_cpi_99_cap = `max' if income_cpi_99_cap > `max' & year == `year'
	}
	local count_total = `count_bottom' + `count_top'
	di "Bottom coded total of `count_bottom' observations"
	di "Top coded total of `count_top' observations"
	di "Total: `count_total' observations"
	
	* gen log-transformed wage
	gen income_cpi_cap_ln = ln(income_cpi_cap)
	label var income_cpi_cap_ln "hourly ln wage, 2019 USD, based on CPI-U, top/bottom-coded"
	gen income_cpi_urs_cap_ln = ln(income_cpi_urs_cap)
	label var income_cpi_urs_cap_ln "hourly ln wage, 2019 USD, based on CPI-U-RS, top/bottom-coded"
	gen income_pce_cap_ln = ln(income_pce_cap)
	label var income_pce_cap_ln "hourly ln wage, 2019 USD, based on PCE, top/bottom-coded"
	gen income_cpi_99_cap_ln = ln(income_cpi_99_cap)
	label var income_cpi_99_cap_ln "hourly ln wage, 1999 USD, based on CPI-U, top/bottom-coded"	
	
	* construct variable: educational attainment (from EDUCD)
	// Problem: There is a discontinuity post-1980 (due to different coding); see: https://usa.ipums.org/usa-action/variables/EDUC#comparability_section
	// In a previous version, I coded "5+ years of college" pre-1990 as "College (graduate)"", but this led to a visible discontinuity when plotting average educ. att. over time.
	// Thus, in this version, I code "5+ years of college" as "College (undergrad)".
	
		// version with 4 categories
		gen edu_attain_4cat = .
		label define edu_attain_4cat ///
			0 "[0] HS or below" ///
			1 "[1] some College" ///
			2 "[2] College (undergrad)" ///
			3 "[3] College (graduate)"
		label values edu_attain_4cat edu_attain_4cat
		replace edu_attain_4cat = 0 if educd >= 2 & educd <= 64
		replace edu_attain_4cat = 1 if educd >= 65 & educd <= 90
		replace edu_attain_4cat = 2 if educd >= 100 & educd <= 110
		replace edu_attain_4cat = 3 if educd >= 111 & educd <= 116
		label var edu_attain_4cat "Educational attainment in 4 categories"
		ta edu_attain_4cat, gen(edu_attain_4cat)	
		
		// version with 3 categories (without discontinuity)
		gen edu_attain_3cat = .
		label define edu_attain_3cat ///
			0 "[0] some College/HS/below HS" ///
			1 "[1] College (undergrad)" ///
			2 "[2] College (graduate)"
		label values edu_attain_3cat edu_attain_3cat
		replace edu_attain_3cat = 0 if educd >= 2 & educd <= 90
		replace edu_attain_3cat = 1 if educd >= 100 & educd <= 110
		replace edu_attain_3cat = 2 if educd >= 111 & educd <= 116
		label var edu_attain_3cat "Educational attainment in 3 categories"
		ta edu_attain_3cat, gen(edu_attain_3cat)
		
		// version with 3 categories (with discontinuity)
		gen edu_attain_3cat_v2 = .
		label define edu_attain_3cat_v2 ///
			0 "[0] some College/HS/below HS" ///
			1 "[1] College (undergrad)" ///
			2 "[2] College (graduate)"
		label values edu_attain_3cat_v2 edu_attain_3cat_v2
		replace edu_attain_3cat_v2 = 0 if educd >= 2 & educd <= 90
		replace edu_attain_3cat_v2 = 1 if educd >= 100 & educd <= 101
		replace edu_attain_3cat_v2 = 2 if educd >= 110 & educd <= 116
		label var edu_attain_3cat_v2 "Educational attainment in 3 categories (with discontinuity)"
		ta edu_attain_3cat_v2, gen(edu_attain_3cat_v2)		
	
	* generate variable: years in education var
		* Note: comes from "160619_Female_Occs_Inds_All_groups.do"
	gen edu_years=.
	label var edu_years "Years in education"
	replace edu_years=0 if educd==0
	replace edu_years=0 if educd==1
	replace edu_years=0 if educd==2
	replace edu_years=4 if educd==10
	replace edu_years=0 if educd==11
	replace edu_years=0 if educd==12
	replace edu_years=2.5 if educd==13
	replace edu_years=1 if educd==14
	replace edu_years=2 if educd==15
	replace edu_years=3 if educd==16
	replace edu_years=4 if educd==17
	replace edu_years=6.5 if educd==20
	replace edu_years=5.5 if educd==21
	replace edu_years=5 if educd==22
	replace edu_years=6 if educd==23
	replace edu_years=7.5 if educd==24
	replace edu_years=7 if educd==25
	replace edu_years=8 if educd==26
	replace edu_years=9 if educd==30
	replace edu_years=10 if educd==40
	replace edu_years=11 if educd==50
	replace edu_years=12 if educd==60
	replace edu_years=12 if educd==61
	replace edu_years=12 if educd==62
	replace edu_years=12 if educd==63
	replace edu_years=12 if educd==64
	replace edu_years=12 if educd==65
	replace edu_years=13 if educd==70
	replace edu_years=13 if educd==71
	replace edu_years=14 if educd==80
	replace edu_years=14 if educd==81
	replace edu_years=14 if educd==82
	replace edu_years=14 if educd==83
	replace edu_years=15 if educd==90
	replace edu_years=16 if educd==100
	replace edu_years=16 if educd==101
	replace edu_years=17 if educd==110
	replace edu_years=18 if educd==111
	replace edu_years=19 if educd==112
	replace edu_years=20 if educd==113
	replace edu_years=17 if educd==114
	replace edu_years=19 if educd==115
	replace edu_years=20 if educd==116
	
	* generate variable: LM experience
	gen lmexp = age - edu_years - 6
	replace lmexp = 0 if lmexp < 0  // bottom limit
	label var lmexp "potential labor market experience"
	
		
	* generate variable: LM experience squared
	gen lmexp_sq = lmexp * lmexp
	label var lmexp "potential labor market experience. squared"
	
	
	* generate variable: race (6 categories)
		* Note: non-hispanic white, black, native american, asian, hispanic
	ren race race_2
	gen race=.
	label var race "race, 6 categories"
	label define race /// 
		0 "(0) non-hispanic white" ///
		1 "(1) black" ///
		2 "(2) native american" ///
		3 "(3) asian" ///
		4 "(4) hispanic" ///
		5 "(5) other"
	label values race race
	replace race=0 if race_2==1 & hispan==0
	replace race=1 if race_2==2 & hispan==0
	replace race=2 if race_2==3 & hispan==0
	replace race=3 if (race_2==4 | race_2==5 | race_2==6) & hispan==0
	replace race=4 if hispan==1 | hispan==2 | hispan==3 | hispan==4
	replace race=5 if race==.
	ta race race_2
	
	* generate variable: race dummies
	ta race, gen(race)
	
	* generate variable: marital status
	gen mar=1 if marst==1
	replace mar=0 if mar==.
	label var mar "married"
	label define mar 0 "(0) Other" 1 "(1) Married"
	label values mar mar
	
	* generate variable: private vs. public sector
	gen public=.
	label define public 0 "(0) private" 1 "(1) public", replace
	label var public "public vs. private sector"
	label values public public
	replace public=0 if classwkrd==22 | classwkrd==23
	replace public=1 if classwkrd>=24 & classwkrd<=28
	ta public classwkrd,m
	drop if public==.  // we are dropping self-employed here (could be done earlier)
	
	* generate variables: public dummies
	ta public, gen(public)
	
	* generate variable: EGP class
	// This is the adapted EGP social class classification by Morgan & Lee 2017
	// It does not take into account self-employment, so it does not integrated class IVa and IVb.
	// The coding can be reviewed in: /Users/felixbusch/Documents/04_Library/01_Academic/02_by_Tool/02_Classifications/OCC2010/codes.xlsx
	gen class_egp = .
	label variable class_egp "social class via updated EGP (Morgan & Lee 2017)"
	label define class_egp /// 
		1 "[1] I Higher-grade professionals, administrators, managers, and officials" ///
		2 "[2] II Lower-grade professionals, administrators, managers, and officials" ///
		3 "[3] IIIa Routine non-manual and service employees, higher-grade" ///
		4 "[4] IIIb Routine non-manual and service employees, lower-grade" ///
		5 "[5] IVa Non-professional self-employed workers with employees" ///
		6 "[6] IVb Non-professional self-employed workers without employees" ///
		7 "[7] IVc Owners and managers of agricultural establishments" ///
		8 "[8] V Higher-grade technicians and repairers, public safety workers, performers, and supervisors of manual workers" ///
		9 "[9] VI Skilled manual workers, lower-grade technicians, installers, and repairers" ///
		10 "[10] VIIa Semiskilled and unskilled manual workers, not in agriculture" ///
		11 "[11] VIIb Agricultural workers and their first-line supervisors, and other workers in primary production" ///
		12 "[12] Military All members of the armed forces", replace
	label values class_egp class_egp
	replace class_egp = 1 if occ2010 == 10
	replace class_egp = 2 if occ2010 == 20
	replace class_egp = 1 if occ2010 == 30
	replace class_egp = 3 if occ2010 == 100
	replace class_egp = 1 if occ2010 == 110
	replace class_egp = 2 if occ2010 == 120
	replace class_egp = 2 if occ2010 == 130
	replace class_egp = 2 if occ2010 == 140
	replace class_egp = 2 if occ2010 == 150
	replace class_egp = 8 if occ2010 == 160
	replace class_egp = 7 if occ2010 == 205
	replace class_egp = 8 if occ2010 == 220
	replace class_egp = 1 if occ2010 == 230
	replace class_egp = 1 if occ2010 == 300
	replace class_egp = 4 if occ2010 == 310
	replace class_egp = 4 if occ2010 == 320
	replace class_egp = 8 if occ2010 == 330
	replace class_egp = 1 if occ2010 == 350
	replace class_egp = 1 if occ2010 == 360
	replace class_egp = 8 if occ2010 == 410
	replace class_egp = 2 if occ2010 == 420
	replace class_egp = 2 if occ2010 == 430
	replace class_egp = 2 if occ2010 == 500
	replace class_egp = 8 if occ2010 == 510
	replace class_egp = 4 if occ2010 == 520
	replace class_egp = 2 if occ2010 == 530
	replace class_egp = 3 if occ2010 == 540
	replace class_egp = 2 if occ2010 == 560
	replace class_egp = 8 if occ2010 == 600
	replace class_egp = 3 if occ2010 == 620
	replace class_egp = 2 if occ2010 == 700
	replace class_egp = 1 if occ2010 == 710
	replace class_egp = 3 if occ2010 == 720
	replace class_egp = 2 if occ2010 == 730
	replace class_egp = 1 if occ2010 == 800
	replace class_egp = 3 if occ2010 == 810
	replace class_egp = 1 if occ2010 == 820
	replace class_egp = 3 if occ2010 == 830
	replace class_egp = 1 if occ2010 == 840
	replace class_egp = 2 if occ2010 == 850
	replace class_egp = 2 if occ2010 == 860
	replace class_egp = 2 if occ2010 == 900
	replace class_egp = 3 if occ2010 == 910
	replace class_egp = 3 if occ2010 == 930
	replace class_egp = 3 if occ2010 == 940
	replace class_egp = 2 if occ2010 == 950
	replace class_egp = 2 if occ2010 == 1000
	replace class_egp = 2 if occ2010 == 1010
	replace class_egp = 1 if occ2010 == 1020
	replace class_egp = 3 if occ2010 == 1050
	replace class_egp = 2 if occ2010 == 1060
	replace class_egp = 2 if occ2010 == 1100
	replace class_egp = 1 if occ2010 == 1200
	replace class_egp = 1 if occ2010 == 1220
	replace class_egp = 1 if occ2010 == 1230
	replace class_egp = 1 if occ2010 == 1240
	replace class_egp = 1 if occ2010 == 1300
	replace class_egp = 2 if occ2010 == 1310
	replace class_egp = 1 if occ2010 == 1320
	replace class_egp = 1 if occ2010 == 1350
	replace class_egp = 1 if occ2010 == 1360
	replace class_egp = 1 if occ2010 == 1400
	replace class_egp = 1 if occ2010 == 1410
	replace class_egp = 1 if occ2010 == 1420
	replace class_egp = 1 if occ2010 == 1430
	replace class_egp = 1 if occ2010 == 1440
	replace class_egp = 1 if occ2010 == 1450
	replace class_egp = 1 if occ2010 == 1460
	replace class_egp = 1 if occ2010 == 1520
	replace class_egp = 1 if occ2010 == 1530
	replace class_egp = 8 if occ2010 == 1540
	replace class_egp = 8 if occ2010 == 1550
	replace class_egp = 8 if occ2010 == 1560
	replace class_egp = 1 if occ2010 == 1600
	replace class_egp = 1 if occ2010 == 1610
	replace class_egp = 1 if occ2010 == 1640
	replace class_egp = 1 if occ2010 == 1650
	replace class_egp = 1 if occ2010 == 1700
	replace class_egp = 1 if occ2010 == 1710
	replace class_egp = 1 if occ2010 == 1720
	replace class_egp = 1 if occ2010 == 1740
	replace class_egp = 1 if occ2010 == 1760
	replace class_egp = 1 if occ2010 == 1800
	replace class_egp = 1 if occ2010 == 1820
	replace class_egp = 2 if occ2010 == 1830
	replace class_egp = 1 if occ2010 == 1840
	replace class_egp = 8 if occ2010 == 1900
	replace class_egp = 8 if occ2010 == 1910
	replace class_egp = 8 if occ2010 == 1920
	replace class_egp = 8 if occ2010 == 1930
	replace class_egp = 8 if occ2010 == 1960
	replace class_egp = 8 if occ2010 == 1980
	replace class_egp = 2 if occ2010 == 2000
	replace class_egp = 2 if occ2010 == 2010
	replace class_egp = 2 if occ2010 == 2020
	replace class_egp = 2 if occ2010 == 2040
	replace class_egp = 2 if occ2010 == 2050
	replace class_egp = 4 if occ2010 == 2060
	replace class_egp = 1 if occ2010 == 2100
	replace class_egp = 3 if occ2010 == 2140
	replace class_egp = 3 if occ2010 == 2150
	replace class_egp = 1 if occ2010 == 2200
	replace class_egp = 4 if occ2010 == 2300
	replace class_egp = 2 if occ2010 == 2310
	replace class_egp = 2 if occ2010 == 2320
	replace class_egp = 2 if occ2010 == 2330
	replace class_egp = 3 if occ2010 == 2340
	replace class_egp = 2 if occ2010 == 2400
	replace class_egp = 2 if occ2010 == 2430
	replace class_egp = 3 if occ2010 == 2440
	replace class_egp = 3 if occ2010 == 2540
	replace class_egp = 2 if occ2010 == 2550
	replace class_egp = 8 if occ2010 == 2600
	replace class_egp = 8 if occ2010 == 2630
	replace class_egp = 8 if occ2010 == 2700
	replace class_egp = 8 if occ2010 == 2720
	replace class_egp = 8 if occ2010 == 2740
	replace class_egp = 8 if occ2010 == 2750
	replace class_egp = 8 if occ2010 == 2760
	replace class_egp = 8 if occ2010 == 2800
	replace class_egp = 2 if occ2010 == 2810
	replace class_egp = 2 if occ2010 == 2825
	replace class_egp = 2 if occ2010 == 2840
	replace class_egp = 2 if occ2010 == 2850
	replace class_egp = 8 if occ2010 == 2860
	replace class_egp = 8 if occ2010 == 2900
	replace class_egp = 8 if occ2010 == 2910
	replace class_egp = 8 if occ2010 == 2920
	replace class_egp = 1 if occ2010 == 3000
	replace class_egp = 1 if occ2010 == 3010
	replace class_egp = 2 if occ2010 == 3030
	replace class_egp = 1 if occ2010 == 3040
	replace class_egp = 1 if occ2010 == 3050
	replace class_egp = 1 if occ2010 == 3060
	replace class_egp = 2 if occ2010 == 3110
	replace class_egp = 1 if occ2010 == 3120
	replace class_egp = 2 if occ2010 == 3130
	replace class_egp = 2 if occ2010 == 3140
	replace class_egp = 2 if occ2010 == 3150
	replace class_egp = 2 if occ2010 == 3160
	replace class_egp = 3 if occ2010 == 3200
	replace class_egp = 2 if occ2010 == 3210
	replace class_egp = 3 if occ2010 == 3220
	replace class_egp = 2 if occ2010 == 3230
	replace class_egp = 2 if occ2010 == 3240
	replace class_egp = 1 if occ2010 == 3250
	replace class_egp = 2 if occ2010 == 3260
	replace class_egp = 8 if occ2010 == 3300
	replace class_egp = 8 if occ2010 == 3310
	replace class_egp = 8 if occ2010 == 3320
	replace class_egp = 8 if occ2010 == 3400
	replace class_egp = 8 if occ2010 == 3410
	replace class_egp = 3 if occ2010 == 3500
	replace class_egp = 3 if occ2010 == 3510
	replace class_egp = 8 if occ2010 == 3520
	replace class_egp = 8 if occ2010 == 3530
	replace class_egp = 2 if occ2010 == 3540
	replace class_egp = 4 if occ2010 == 3600
	replace class_egp = 4 if occ2010 == 3610
	replace class_egp = 4 if occ2010 == 3620
	replace class_egp = 4 if occ2010 == 3630
	replace class_egp = 4 if occ2010 == 3640
	replace class_egp = 4 if occ2010 == 3650
	replace class_egp = 8 if occ2010 == 3700
	replace class_egp = 8 if occ2010 == 3710
	replace class_egp = 8 if occ2010 == 3720
	replace class_egp = 8 if occ2010 == 3730
	replace class_egp = 8 if occ2010 == 3740
	replace class_egp = 8 if occ2010 == 3750
	replace class_egp = 8 if occ2010 == 3800
	replace class_egp = 8 if occ2010 == 3820
	replace class_egp = 4 if occ2010 == 3900
	replace class_egp = 8 if occ2010 == 3910
	replace class_egp = 4 if occ2010 == 3930
	replace class_egp = 10 if occ2010 == 3940
	replace class_egp = 8 if occ2010 == 3950
	replace class_egp = 8 if occ2010 == 4000
	replace class_egp = 4 if occ2010 == 4010
	replace class_egp = 10 if occ2010 == 4030
	replace class_egp = 4 if occ2010 == 4040
	replace class_egp = 4 if occ2010 == 4050
	replace class_egp = 4 if occ2010 == 4060
	replace class_egp = 4 if occ2010 == 4110
	replace class_egp = 4 if occ2010 == 4120
	replace class_egp = 4 if occ2010 == 4130
	replace class_egp = 10 if occ2010 == 4140
	replace class_egp = 4 if occ2010 == 4150
	replace class_egp = 4 if occ2010 == 4200
	replace class_egp = 8 if occ2010 == 4210
	replace class_egp = 10 if occ2010 == 4220
	replace class_egp = 10 if occ2010 == 4230
	replace class_egp = 4 if occ2010 == 4240
	replace class_egp = 10 if occ2010 == 4250
	replace class_egp = 4 if occ2010 == 4300
	replace class_egp = 4 if occ2010 == 4320
	replace class_egp = 11 if occ2010 == 4340
	replace class_egp = 4 if occ2010 == 4350
	replace class_egp = 4 if occ2010 == 4400
	replace class_egp = 4 if occ2010 == 4420
	replace class_egp = 4 if occ2010 == 4430
	replace class_egp = 4 if occ2010 == 4460
	replace class_egp = 4 if occ2010 == 4500
	replace class_egp = 4 if occ2010 == 4510
	replace class_egp = 4 if occ2010 == 4520
	replace class_egp = 4 if occ2010 == 4530
	replace class_egp = 3 if occ2010 == 4540
	replace class_egp = 4 if occ2010 == 4600
	replace class_egp = 4 if occ2010 == 4610
	replace class_egp = 4 if occ2010 == 4620
	replace class_egp = 4 if occ2010 == 4640
	replace class_egp = 4 if occ2010 == 4650
	replace class_egp = 4 if occ2010 == 4700
	replace class_egp = 4 if occ2010 == 4720
	replace class_egp = 4 if occ2010 == 4740
	replace class_egp = 4 if occ2010 == 4750
	replace class_egp = 4 if occ2010 == 4760
	replace class_egp = 3 if occ2010 == 4800
	replace class_egp = 3 if occ2010 == 4810
	replace class_egp = 2 if occ2010 == 4820
	replace class_egp = 3 if occ2010 == 4830
	replace class_egp = 3 if occ2010 == 4840
	replace class_egp = 3 if occ2010 == 4850
	replace class_egp = 4 if occ2010 == 4900
	replace class_egp = 3 if occ2010 == 4920
	replace class_egp = 2 if occ2010 == 4930
	replace class_egp = 4 if occ2010 == 4940
	replace class_egp = 4 if occ2010 == 4950
	replace class_egp = 3 if occ2010 == 4965
	replace class_egp = 3 if occ2010 == 5000
	replace class_egp = 3 if occ2010 == 5010
	replace class_egp = 3 if occ2010 == 5020
	replace class_egp = 3 if occ2010 == 5030
	replace class_egp = 3 if occ2010 == 5100
	replace class_egp = 3 if occ2010 == 5110
	replace class_egp = 3 if occ2010 == 5120
	replace class_egp = 4 if occ2010 == 5130
	replace class_egp = 3 if occ2010 == 5140
	replace class_egp = 3 if occ2010 == 5150
	replace class_egp = 3 if occ2010 == 5160
	replace class_egp = 3 if occ2010 == 5165
	replace class_egp = 3 if occ2010 == 5200
	replace class_egp = 3 if occ2010 == 5220
	replace class_egp = 3 if occ2010 == 5230
	replace class_egp = 3 if occ2010 == 5240
	replace class_egp = 3 if occ2010 == 5250
	replace class_egp = 3 if occ2010 == 5260
	replace class_egp = 3 if occ2010 == 5300
	replace class_egp = 3 if occ2010 == 5310
	replace class_egp = 3 if occ2010 == 5320
	replace class_egp = 3 if occ2010 == 5330
	replace class_egp = 3 if occ2010 == 5340
	replace class_egp = 3 if occ2010 == 5350
	replace class_egp = 3 if occ2010 == 5360
	replace class_egp = 4 if occ2010 == 5400
	replace class_egp = 3 if occ2010 == 5410
	replace class_egp = 3 if occ2010 == 5420
	replace class_egp = 3 if occ2010 == 5500
	replace class_egp = 4 if occ2010 == 5510
	replace class_egp = 3 if occ2010 == 5520
	replace class_egp = 3 if occ2010 == 5530
	replace class_egp = 3 if occ2010 == 5540
	replace class_egp = 10 if occ2010 == 5550
	replace class_egp = 10 if occ2010 == 5560
	replace class_egp = 3 if occ2010 == 5600
	replace class_egp = 10 if occ2010 == 5610
	replace class_egp = 10 if occ2010 == 5620
	replace class_egp = 3 if occ2010 == 5630
	replace class_egp = 3 if occ2010 == 5700
	replace class_egp = 3 if occ2010 == 5800
	replace class_egp = 3 if occ2010 == 5810
	replace class_egp = 3 if occ2010 == 5820
	replace class_egp = 3 if occ2010 == 5840
	replace class_egp = 10 if occ2010 == 5850
	replace class_egp = 3 if occ2010 == 5860
	replace class_egp = 3 if occ2010 == 5900
	replace class_egp = 3 if occ2010 == 5910
	replace class_egp = 3 if occ2010 == 5920
	replace class_egp = 3 if occ2010 == 5940
	replace class_egp = 11 if occ2010 == 6005
	replace class_egp = 11 if occ2010 == 6010
	replace class_egp = 11 if occ2010 == 6040
	replace class_egp = 11 if occ2010 == 6050
	replace class_egp = 11 if occ2010 == 6100
	replace class_egp = 9 if occ2010 == 6120
	replace class_egp = 10 if occ2010 == 6130
	replace class_egp = 8 if occ2010 == 6200
	replace class_egp = 9 if occ2010 == 6210
	replace class_egp = 9 if occ2010 == 6220
	replace class_egp = 9 if occ2010 == 6230
	replace class_egp = 9 if occ2010 == 6240
	replace class_egp = 10 if occ2010 == 6250
	replace class_egp = 10 if occ2010 == 6260
	replace class_egp = 10 if occ2010 == 6300
	replace class_egp = 9 if occ2010 == 6320
	replace class_egp = 9 if occ2010 == 6330
	replace class_egp = 9 if occ2010 == 6355
	replace class_egp = 10 if occ2010 == 6360
	replace class_egp = 9 if occ2010 == 6400
	replace class_egp = 9 if occ2010 == 6420
	replace class_egp = 9 if occ2010 == 6430
	replace class_egp = 9 if occ2010 == 6440
	replace class_egp = 10 if occ2010 == 6460
	replace class_egp = 9 if occ2010 == 6500
	replace class_egp = 10 if occ2010 == 6515
	replace class_egp = 9 if occ2010 == 6520
	replace class_egp = 9 if occ2010 == 6530
	replace class_egp = 10 if occ2010 == 6600
	replace class_egp = 8 if occ2010 == 6660
	replace class_egp = 8 if occ2010 == 6700
	replace class_egp = 10 if occ2010 == 6710
	replace class_egp = 9 if occ2010 == 6720
	replace class_egp = 10 if occ2010 == 6730
	replace class_egp = 10 if occ2010 == 6740
	replace class_egp = 10 if occ2010 == 6765
	replace class_egp = 10 if occ2010 == 6800
	replace class_egp = 10 if occ2010 == 6820
	replace class_egp = 9 if occ2010 == 6830
	replace class_egp = 10 if occ2010 == 6840
	replace class_egp = 10 if occ2010 == 6940
	replace class_egp = 8 if occ2010 == 7000
	replace class_egp = 9 if occ2010 == 7010
	replace class_egp = 8 if occ2010 == 7020
	replace class_egp = 8 if occ2010 == 7030
	replace class_egp = 8 if occ2010 == 7040
	replace class_egp = 8 if occ2010 == 7100
	replace class_egp = 8 if occ2010 == 7110
	replace class_egp = 8 if occ2010 == 7120
	replace class_egp = 8 if occ2010 == 7125
	replace class_egp = 9 if occ2010 == 7130
	replace class_egp = 9 if occ2010 == 7140
	replace class_egp = 10 if occ2010 == 7150
	replace class_egp = 10 if occ2010 == 7160
	replace class_egp = 9 if occ2010 == 7200
	replace class_egp = 9 if occ2010 == 7210
	replace class_egp = 9 if occ2010 == 7220
	replace class_egp = 9 if occ2010 == 7240
	replace class_egp = 10 if occ2010 == 7260
	replace class_egp = 9 if occ2010 == 7300
	replace class_egp = 9 if occ2010 == 7315
	replace class_egp = 9 if occ2010 == 7320
	replace class_egp = 9 if occ2010 == 7330
	replace class_egp = 9 if occ2010 == 7340
	replace class_egp = 9 if occ2010 == 7350
	replace class_egp = 9 if occ2010 == 7360
	replace class_egp = 9 if occ2010 == 7410
	replace class_egp = 8 if occ2010 == 7420
	replace class_egp = 9 if occ2010 == 7430
	replace class_egp = 4 if occ2010 == 7510
	replace class_egp = 9 if occ2010 == 7540
	replace class_egp = 9 if occ2010 == 7550
	replace class_egp = 10 if occ2010 == 7560
	replace class_egp = 10 if occ2010 == 7610
	replace class_egp = 9 if occ2010 == 7630
	replace class_egp = 8 if occ2010 == 7700
	replace class_egp = 10 if occ2010 == 7710
	replace class_egp = 10 if occ2010 == 7720
	replace class_egp = 10 if occ2010 == 7730
	replace class_egp = 9 if occ2010 == 7740
	replace class_egp = 10 if occ2010 == 7750
	replace class_egp = 10 if occ2010 == 7800
	replace class_egp = 10 if occ2010 == 7810
	replace class_egp = 10 if occ2010 == 7830
	replace class_egp = 10 if occ2010 == 7840
	replace class_egp = 10 if occ2010 == 7850
	replace class_egp = 10 if occ2010 == 7855
	replace class_egp = 8 if occ2010 == 7900
	replace class_egp = 10 if occ2010 == 7920
	replace class_egp = 10 if occ2010 == 7930
	replace class_egp = 10 if occ2010 == 7940
	replace class_egp = 10 if occ2010 == 7950
	replace class_egp = 10 if occ2010 == 7960
	replace class_egp = 10 if occ2010 == 8000
	replace class_egp = 10 if occ2010 == 8010
	replace class_egp = 9 if occ2010 == 8030
	replace class_egp = 10 if occ2010 == 8040
	replace class_egp = 9 if occ2010 == 8060
	replace class_egp = 9 if occ2010 == 8100
	replace class_egp = 9 if occ2010 == 8130
	replace class_egp = 10 if occ2010 == 8140
	replace class_egp = 10 if occ2010 == 8150
	replace class_egp = 10 if occ2010 == 8200
	replace class_egp = 10 if occ2010 == 8210
	replace class_egp = 10 if occ2010 == 8220
	replace class_egp = 10 if occ2010 == 8230
	replace class_egp = 9 if occ2010 == 8250
	replace class_egp = 10 if occ2010 == 8300
	replace class_egp = 10 if occ2010 == 8310
	replace class_egp = 10 if occ2010 == 8320
	replace class_egp = 10 if occ2010 == 8330
	replace class_egp = 10 if occ2010 == 8340
	replace class_egp = 9 if occ2010 == 8350
	replace class_egp = 10 if occ2010 == 8400
	replace class_egp = 10 if occ2010 == 8410
	replace class_egp = 10 if occ2010 == 8420
	replace class_egp = 10 if occ2010 == 8450
	replace class_egp = 10 if occ2010 == 8460
	replace class_egp = 9 if occ2010 == 8500
	replace class_egp = 10 if occ2010 == 8510
	replace class_egp = 10 if occ2010 == 8530
	replace class_egp = 10 if occ2010 == 8540
	replace class_egp = 9 if occ2010 == 8550
	replace class_egp = 9 if occ2010 == 8600
	replace class_egp = 9 if occ2010 == 8610
	replace class_egp = 9 if occ2010 == 8620
	replace class_egp = 9 if occ2010 == 8630
	replace class_egp = 10 if occ2010 == 8640
	replace class_egp = 10 if occ2010 == 8650
	replace class_egp = 10 if occ2010 == 8710
	replace class_egp = 10 if occ2010 == 8720
	replace class_egp = 10 if occ2010 == 8730
	replace class_egp = 10 if occ2010 == 8740
	replace class_egp = 9 if occ2010 == 8750
	replace class_egp = 9 if occ2010 == 8760
	replace class_egp = 10 if occ2010 == 8800
	replace class_egp = 10 if occ2010 == 8810
	replace class_egp = 10 if occ2010 == 8830
	replace class_egp = 10 if occ2010 == 8850
	replace class_egp = 10 if occ2010 == 8860
	replace class_egp = 9 if occ2010 == 8910
	replace class_egp = 9 if occ2010 == 8920
	replace class_egp = 10 if occ2010 == 8930
	replace class_egp = 10 if occ2010 == 8940
	replace class_egp = 10 if occ2010 == 8950
	replace class_egp = 10 if occ2010 == 8965
	replace class_egp = 8 if occ2010 == 9000
	replace class_egp = 2 if occ2010 == 9030
	replace class_egp = 3 if occ2010 == 9040
	replace class_egp = 3 if occ2010 == 9050
	replace class_egp = 4 if occ2010 == 9100
	replace class_egp = 10 if occ2010 == 9130
	replace class_egp = 10 if occ2010 == 9140
	replace class_egp = 10 if occ2010 == 9150
	replace class_egp = 9 if occ2010 == 9200
	replace class_egp = 9 if occ2010 == 9230
	replace class_egp = 8 if occ2010 == 9240
	replace class_egp = 9 if occ2010 == 9260
	replace class_egp = 9 if occ2010 == 9300
	replace class_egp = 8 if occ2010 == 9310
	replace class_egp = 4 if occ2010 == 9350
	replace class_egp = 4 if occ2010 == 9360
	replace class_egp = 8 if occ2010 == 9410
	replace class_egp = 4 if occ2010 == 9420
	replace class_egp = 9 if occ2010 == 9510
	replace class_egp = 10 if occ2010 == 9520
	replace class_egp = 10 if occ2010 == 9560
	replace class_egp = 10 if occ2010 == 9600
	replace class_egp = 10 if occ2010 == 9610
	replace class_egp = 10 if occ2010 == 9620
	replace class_egp = 10 if occ2010 == 9630
	replace class_egp = 10 if occ2010 == 9640
	replace class_egp = 10 if occ2010 == 9650
	replace class_egp = 10 if occ2010 == 9720
	replace class_egp = 10 if occ2010 == 9750
	replace class_egp = 12 if occ2010 == 9800
	replace class_egp = 12 if occ2010 == 9810
	replace class_egp = 12 if occ2010 == 9820
	replace class_egp = 12 if occ2010 == 9830
	replace class_egp = 99 if occ2010 == 9920
	
	* generate variable: is PM (based on EGP)
	gen is_PM_egp = .
	replace is_PM_egp = 1 if class_egp == 1 | class_egp == 2
	replace is_PM_egp = 0 if is_PM_egp == .
	label variable is_PM_egp "is professional/managerial (based on EGP class)"
	label define is_PM 0 "[0] not PM" 1 "[1] is PM"
	label values is_PM_egp is_PM
	
	* generate variable: OCC2010 group
	// This adds the major group that comes with the OCC2010 classification
	do "codes/00_gen_occ2010_group.do"
	
	* generate variable: is female
	recode sex (2 = 1 "[1] female") (1 = 0 "[0] male"), gen(female)
	label variable female "gender is female"
	ta female, gen(female)
	
	* merge in occupation-level data via O*NET
	// I use data from the inequality reader project
	// Original path: /Users/felixbusch/Documents/06_Studium/07_Papers/2021_ineq_reader/01_Data/01_ONET
	// From the original data, I only keep the 5 skill components
	* ren occ2010 OCC2010
	* cd "/Users/felixbusch/Documents/06_Studium/07_Papers/2019_PM_occupations_Paula/01_Data"
	* merge m:1 OCC2010 using "processed/variables_by_OCC2010.dta", keepusing(analytic_std mechanical_std interpersonal_std management_std engineering_std analytic_std_terc mechanical_std_terc interpersonal_std_terc management_std_terc engineering_std_terc) nogen keep(master match)
	* ren OCC2010 occ2010
	* ta occ2010 if analytic_std == .  
	// => no occupation-level data available for 16 occupations (724,826 observations, 3% of the total sample)
	// We would have to drop these 3% from the sample, when including occupational variables in the analysis.
	// However, 97% coverage with these variables is a very high value, and dropping 3% should not lead to large biases.
	
	* gen variable: percent female (continuous)
	// We create a collapsed dataset for this step and merge it into our current dataset
	
		// store current data
		// cd "/Users/felixbusch/Documents/06_Studium/07_Papers/2019_PM_occupations_Paula/01_Data"
		
		cd "/Users/wenhao/Dropbox/RA Paula/R&R/Codes"
		save "processed/master_1_01.dta", replace
		
		// generate separate data with pcf
		keep year weight occ2010 female
		collapse (mean) female [pweight=weight], by(occ2010 year)
		ren female pcf
		replace pcf = round(pcf*100, 0.01)
		label var pcf "% female in occ2010-year cell, continuous"
		save "processed/pcf_by_occ2010_year.dta", replace
		
		// merge pcf into analytic dataset
		use "processed/master_1_01.dta", clear
		merge m:1 occ2010 year using "processed/pcf_by_occ2010_year.dta", nogen keep(master match)

// keep selected variables
keep year weight sex occ2010 occ1990 ind1990_agg hours income_cpi_cap income_pce_cap income_cpi_urs_cap income_cpi_99_cap income_cpi_urs_cap_ln income_pce_cap_ln income_cpi_cap_ln income_cpi_99_cap_ln  metarea metaread met2013 edu_attain* edu_years lmexp race mar public race1-race6 class_egp is_PM_egp ind1990_agg1 ind1990_agg2 ind1990_agg3 ind1990_agg4 ind1990_agg5 ind1990_agg6 ind1990_agg7 ind1990_agg8 ind1990_agg9 ind1990_agg10 ind1990_agg11 ind1990_agg12 ind1990_agg13 ind1990_agg14 public1 public2 lmexp_sq occ2010_group female*

// cd "/Users/felixbusch/Documents/06_Studium/07_Papers/2019_PM_occupations_Paula/01_Data"
save "processed/master_1_01.dta", replace	

