
*********************************************************************************
* 1/18/2017
* do file to check ICC involvement dataset
* correct Zuhaib mistakes and fill in missing data that Zuhaib messed up
*********************************************************************************

clear
cd "/Users/aprorok/Dropbox/icc involvement paper w ben"
set more off
insheet using countrymonthdata_icczuh.csv




*********************************************************
* 1. make sure the case universe is correct
*********************************************************
sum ccode year month
sum year month if ccode==626 /* south sudan */
sum year month if ccode==347 /* kosovo */

** South Sudan obs begin in 2011, Kosovo begins in 2008
** this looks right, so I'm assuming that he actually managed to get all country years between 2001-2015 in the dataset




*********************************************************
* 2. ICC involvement variables
*********************************************************

** add icclevel variable to my original icc involvement data. 
** will then merge with Zuhaib data to check against his codings

clear
use monthly_iccinvolvement_countrylevel.dta
gen icclevel=.
replace icclevel=0 if iccinvolvement==0
replace icclevel=1 if ccode==700 & year>2006 & icclevel==.
replace icclevel=1 if ccode==2 & year>2006 & icclevel==.
replace icclevel=1 if ccode==100 & icclevel==.
replace icclevel=1 if ccode==372 & icclevel==. & year<2016
replace icclevel=2 if ccode==372 & icclevel==. & year==2016
replace icclevel=1 if ccode==365 & icclevel==.
replace icclevel=1 if ccode==438 & icclevel==.
replace icclevel=1 if ccode==91 & icclevel==.
replace icclevel=1 if ccode==200 & icclevel==.
replace icclevel=1 if ccode==475 & icclevel==.
replace icclevel=1 if ccode==101 & icclevel==.
replace icclevel=1 if ccode==369 & icclevel==.
replace icclevel=1 if ccode==731 & icclevel==.
replace icclevel=1 if ccode==666 & icclevel==.
replace icclevel=1 if ccode==516 & icclevel==.
* Libya
replace icclevel=1 if ccode==620 & icclevel==. & year==2011 & month==2
replace icclevel=2 if ccode==620 & icclevel==. & year==2011 & month>2 & month<6
replace icclevel=3 if ccode==620 & icclevel==. & year==2011 & month>5
replace icclevel=3 if ccode==620 & icclevel==. & year>2011
* Mali
replace icclevel=1 if ccode==432 & icclevel==. & year==2012
replace icclevel=2 if ccode==432 & icclevel==. & year>2012 & year<2015
replace icclevel=2 if ccode==432 & icclevel==. & year==2015 & month<9
replace icclevel=4 if ccode==432 & icclevel==. & year==2015 & month>8
replace icclevel=4 if ccode==432 & icclevel==. & year==2016 & month<3
replace icclevel=5 if ccode==432 & icclevel==. & year==2016 & month>2 & month<8
* Uganda
replace icclevel=1 if ccode==500 & icclevel==. & year==2003
replace icclevel=1 if ccode==500 & icclevel==. & year==2004 & month<7
replace icclevel=2 if ccode==500 & icclevel==. & year==2004 & month>6
replace icclevel=2 if ccode==500 & icclevel==. & year==2005 & month<7
replace icclevel=3 if ccode==500 & icclevel==. & year==2005 & month>6
replace icclevel=3 if ccode==500 & icclevel==. & year>2005 & year<2015
replace icclevel=4 if ccode==500 & icclevel==. & year==2015
replace icclevel=4 if ccode==500 & icclevel==. & year==2016 & month<3
replace icclevel=5 if ccode==500 & icclevel==. & year==2016 & month>2
* Ivory Coast
replace icclevel=1 if ccode==437 & icclevel==. & year==2011 & month>5 & month<10
replace icclevel=2 if ccode==437 & icclevel==. & year==2011 & month==10
replace icclevel=4 if ccode==437 & icclevel==. & year==2011 & month>10
replace icclevel=4 if ccode==437 & icclevel==. & year==2012
replace icclevel=4 if ccode==437 & icclevel==. & year==2013 & month<6
replace icclevel=5 if ccode==437 & icclevel==. & year==2013 & month>5
replace icclevel=5 if ccode==437 & icclevel==. & year>2013 & year<2016
replace icclevel=6 if ccode==437 & icclevel==. & year==2016
* CAR
replace icclevel=1 if ccode==482 & icclevel==. & year>2004 & year<2007
replace icclevel=1 if ccode==482 & icclevel==. & year==2007 & month<5
replace icclevel=2 if ccode==482 & icclevel==. & year==2007 & month>4
replace icclevel=2 if ccode==482 & icclevel==. & year==2008 & month<5
replace icclevel=3 if ccode==482 & icclevel==. & year==2008 & month>4 & month<7
replace icclevel=4 if ccode==482 & icclevel==. & year==2008 & month>6
replace icclevel=4 if ccode==482 & icclevel==. & year==2009 & month<6
replace icclevel=5 if ccode==482 & icclevel==. & year==2009 & month>5
replace icclevel=5 if ccode==482 & icclevel==. & year==2010 & month<11
replace icclevel=6 if ccode==482 & icclevel==. & year==2010 & month>10
replace icclevel=6 if ccode==482 & icclevel==. & year>2010
* DRC
replace icclevel=1 if ccode==490 & icclevel==. & year==2004 & month>3 & month<6
replace icclevel=2 if ccode==490 & icclevel==. & year==2004 & month>5
replace icclevel=2 if ccode==490 & icclevel==. & year==2005
replace icclevel=2 if ccode==490 & icclevel==. & year==2006 & month==1
replace icclevel=3 if ccode==490 & icclevel==. & year==2006 & month==2
replace icclevel=4 if ccode==490 & icclevel==. & year==2006 & month>2
replace icclevel=5 if ccode==490 & icclevel==. & year>2006 & year<2009
replace icclevel=6 if ccode==490 & icclevel==. & year>2008 & year<2014
replace icclevel=6 if ccode==490 & icclevel==. & year==2014 & month<5
replace icclevel=5 if ccode==490 & icclevel==. & year==2014 & month>4
replace icclevel=5 if ccode==490 & icclevel==. & year==2015 & month<9
replace icclevel=6 if ccode==490 & icclevel==. & year==2015 & month>8
replace icclevel=6 if ccode==490 & icclevel==. & year==2016
* Sudan
replace icclevel=1 if ccode==625 & icclevel==. & year==2005 & month>2 & month<6
replace icclevel=2 if ccode==625 & icclevel==. & year==2005 & month>5 
replace icclevel=2 if ccode==625 & icclevel==. & year==2006
replace icclevel=2 if ccode==625 & icclevel==. & year==2007 & month<4 
replace icclevel=3 if ccode==625 & icclevel==. & year==2007 & month>3
replace icclevel=3 if ccode==625 & icclevel==. & year==2008
replace icclevel=3 if ccode==625 & icclevel==. & year==2009 & month<5
replace icclevel=4 if ccode==625 & icclevel==. & year==2009 & month>4
replace icclevel=4 if ccode==625 & icclevel==. & year==2010 & month==1
replace icclevel=3 if ccode==625 & icclevel==. & year==2010 & month>1 & month<6
replace icclevel=4 if ccode==625 & icclevel==. & year==2010 & month>5
replace icclevel=4 if ccode==625 & icclevel==. & year==2011 & month<3
replace icclevel=5 if ccode==625 & icclevel==. & year==2011 & month>2
replace icclevel=5 if ccode==625 & icclevel==. & year>2001 & year<2014
replace icclevel=5 if ccode==625 & icclevel==. & year==2014 & month<9
replace icclevel=3 if ccode==625 & icclevel==. & year==2014 & month>8
replace icclevel=3 if ccode==625 & icclevel==. & year>2014
* Kenya
replace icclevel=1 if ccode==501 & icclevel==. & year==2009 & month>10
replace icclevel=1 if ccode==501 & icclevel==. & year==2010 & month<3
replace icclevel=2 if ccode==501 & icclevel==. & year==2010 & month>2
replace icclevel=2 if ccode==501 & icclevel==. & year==2011 & month<3
replace icclevel=3 if ccode==501 & icclevel==. & year==2011 & month==3
replace icclevel=4 if ccode==501 & icclevel==. & year==2011 & month>3
replace icclevel=5 if ccode==501 & icclevel==. & year==2012
replace icclevel=5 if ccode==501 & icclevel==. & year==2013 & month<9
replace icclevel=6 if ccode==501 & icclevel==. & year==2013 & month>8
replace icclevel=6 if ccode==501 & icclevel==. & year>2013 & year<2016
replace icclevel=6 if ccode==501 & icclevel==. & year==2016 & month<4
replace icclevel=3 if ccode==501 & icclevel==. & year==2016 & month>3

tab icclevel
sum icclevel year ccode


* directed ICC level codings (separating level of action agaisnt state and opposition into separate variables):
gen icclevel_state=.
replace icclevel_state=0 if icclevel==0
gen icclevel_opp=.
replace icclevel_opp=0 if icclevel==0

replace icclevel_state=1 if ccode==700 & icclevel_state==.
replace icclevel_opp=1 if ccode==700 & icclevel_opp==.
replace icclevel_state=1 if ccode==2 & icclevel_state==.
replace icclevel_opp=0 if ccode==2 & icclevel_opp==.
replace icclevel_state=1 if ccode==100 & icclevel_state==.
replace icclevel_opp=1 if ccode==100 & icclevel_opp==.
replace icclevel_state=1 if ccode==372 & icclevel_state==. & year<2016
replace icclevel_state=2 if ccode==372 & icclevel_state==. & year==2016
replace icclevel_opp=1 if ccode==372 & icclevel_opp==. & year<2016
replace icclevel_opp=2 if ccode==372 & icclevel_opp==. & year==2016
replace icclevel_state=1 if ccode==365 & icclevel_state==.
replace icclevel_opp=0 if ccode==365 & icclevel_opp==.
replace icclevel_state=1 if ccode==91 & icclevel_state==.
replace icclevel_opp=1 if ccode==91 & icclevel_opp==.
replace icclevel_state=1 if ccode==200 & icclevel_state==.
replace icclevel_opp=0 if ccode==200 & icclevel_opp==.
replace icclevel_state=icclevel if ccode==475 & icclevel_state==.
replace icclevel_opp=icclevel if ccode==475 & icclevel_opp==.
replace icclevel_state=icclevel if ccode==101 & icclevel_state==.
replace icclevel_opp=icclevel if ccode==101 & icclevel_opp==.
replace icclevel_opp=icclevel if ccode==369 & icclevel_opp==.
replace icclevel_state=0 if ccode==369 & icclevel_state==. & year==2014
replace icclevel_state=0 if ccode==369 & icclevel_state==. & year==2015 & month<9
replace icclevel_state=1 if ccode==369 & icclevel_state==. & year==2015 & month>8
replace icclevel_state=1 if ccode==369 & icclevel_state==. & year==2016
replace icclevel_state=icclevel if ccode==731 & icclevel_state==.
replace icclevel_opp=0 if ccode==731 & icclevel_opp==.
replace icclevel_state=icclevel if ccode==666 & icclevel_state==.
replace icclevel_opp=icclevel if ccode==666 & icclevel_opp==. & year<2013 
replace icclevel_opp=icclevel if ccode==666 & icclevel_opp==. & year>2014
replace icclevel_opp=0 if ccode==666 & icclevel_opp==. & year>2012 & year<2015
* Libya
replace icclevel_state=icclevel if ccode==620 & icclevel_state==. & year==2011 & month<9
replace icclevel_state=0 if ccode==620 & icclevel_state==. 
replace icclevel_opp=0 if ccode==620 & icclevel_opp==. & year==2011 & month<9
replace icclevel_opp=icclevel if ccode==620 & icclevel_opp==. & year==2011 & month>8
replace icclevel_opp=icclevel if ccode==620 & icclevel_opp==. & year>2011
* Mali
replace icclevel_state=0 if ccode==432 & icclevel_state==. 
replace icclevel_opp=icclevel if ccode==432 & icclevel_opp==.
* Uganda
replace icclevel_state=0 if ccode==500 & icclevel_state==. 
replace icclevel_opp=icclevel if ccode==500 & icclevel_opp==.
* Ivory Coast
replace icclevel_state=0 if ccode==437 & icclevel_state==. 
replace icclevel_opp=icclevel if ccode==437 & icclevel_opp==.
* CAR
replace icclevel_state=0 if ccode==482 & icclevel_state==. 
replace icclevel_opp=icclevel if ccode==482 & icclevel_opp==.
* Sudan
replace icclevel_state=1 if ccode==625 & icclevel_state==. & year==2005 & month>2 & month<6
replace icclevel_state=2 if ccode==625 & icclevel_state==. & year==2005 & month>5 
replace icclevel_state=2 if ccode==625 & icclevel_state==. & year==2006
replace icclevel_state=2 if ccode==625 & icclevel_state==. & year==2007 & month<4 
replace icclevel_state=3 if ccode==625 & icclevel_state==. & year==2007 & month>3
replace icclevel_state=3 if ccode==625 & icclevel_state==. & year>2007
replace icclevel_opp=1 if ccode==625 & icclevel_opp==. & year==2005 & month>2 & month<6
replace icclevel_opp=2 if ccode==625 & icclevel_opp==. & year==2005 & month>5 
replace icclevel_opp=2 if ccode==625 & icclevel_opp==. & year>2005 & year<2009
replace icclevel_opp=2 if ccode==625 & icclevel_opp==. & year==2009 & month<5
replace icclevel_opp=4 if ccode==625 & icclevel_opp==. & year==2009 & month>4
replace icclevel_opp=4 if ccode==625 & icclevel_opp==. & year==2010 & month==1
replace icclevel_opp=3 if ccode==625 & icclevel_opp==. & year==2010 & month>1 &  month<6
replace icclevel_opp=4 if ccode==625 & icclevel_opp==. & year==2010 & month>5
replace icclevel_opp=4 if ccode==625 & icclevel_opp==. & year==2011 & month<3
replace icclevel_opp=5 if ccode==625 & icclevel_opp==. & year==2011 & month>2
replace icclevel_opp=5 if ccode==625 & icclevel_opp==. & year>2011 & year<2014
replace icclevel_opp=5 if ccode==625 & icclevel_opp==. & year==2014 & month<9
replace icclevel_opp=3 if ccode==625 & icclevel_opp==. & year==2014 & month>8
replace icclevel_opp=3 if ccode==625 & icclevel_opp==. & year>2014
* Guinea
replace icclevel_state=1 if ccode==438 & icclevel_state==. & year==2009 & month>9
replace icclevel_state=1 if ccode==438 & icclevel_state==. & year==2010 & month<12
replace icclevel_state=0 if ccode==438 & icclevel_state==. & year==2010 & month==12
replace icclevel_state=0 if ccode==438 & icclevel_state==. & year>2010
replace icclevel_opp=0 if ccode==438 & icclevel_opp==. & year==2009 & month>9
replace icclevel_opp=0 if ccode==438 & icclevel_opp==. & year==2010 & month<12
replace icclevel_opp=1 if ccode==438 & icclevel_opp==. & year==2010 & month==12
replace icclevel_opp=1 if ccode==438 & icclevel_opp==. & year>2010
* Kenya
replace icclevel_state=1 if ccode==501 & icclevel_state==. & year==2009 & month>10
replace icclevel_state=1 if ccode==501 & icclevel_state==. & year==2010 & month<3
replace icclevel_state=2 if ccode==501 & icclevel_state==. & year==2010 & month>2
replace icclevel_state=2 if ccode==501 & icclevel_state==. & year==2011 & month<3
replace icclevel_state=3 if ccode==501 & icclevel_state==. & year==2011 & month==3
replace icclevel_state=4 if ccode==501 & icclevel_state==. & year==2011 & month>3
replace icclevel_state=5 if ccode==501 & icclevel_state==. & year==2012
replace icclevel_state=5 if ccode==501 & icclevel_state==. & year==2013 & month<9
replace icclevel_state=6 if ccode==501 & icclevel_state==. & year==2013 & month>8
replace icclevel_state=6 if ccode==501 & icclevel_state==. & year>2013 & year<2016
replace icclevel_state=6 if ccode==501 & icclevel_state==. & year==2016 & month<4
replace icclevel_state=3 if ccode==501 & icclevel_state==. & year==2016 & month>3
replace icclevel_opp=0 if ccode==501 & icclevel_opp==.
* DRC
replace icclevel_state=0 if ccode==490 & icclevel_state==. & year==2004 & month>3 & month<12
replace icclevel_state=2 if ccode==490 & icclevel_state==. & year==2004 & month==12
replace icclevel_state=2 if ccode==490 & icclevel_state==. & year==2005 & month<3
replace icclevel_state=0 if ccode==490 & icclevel_state==. & year==2005 & month>2
replace icclevel_state=0 if ccode==490 & icclevel_state==. & year==2006 & month<10
replace icclevel_state=2 if ccode==490 & icclevel_state==. & year==2006 & month>9
replace icclevel_state=2 if ccode==490 & icclevel_state==. & year==2007 & month<7
replace icclevel_state=3 if ccode==490 & icclevel_state==. & year==2007 & month>6
replace icclevel_state=3 if ccode==490 & icclevel_state==. & year==2008 & month==1
replace icclevel_state=4 if ccode==490 & icclevel_state==. & year==2008 & month>1 & month<9
replace icclevel_state=5 if ccode==490 & icclevel_state==. & year==2008 & month>8
replace icclevel_state=5 if ccode==490 & icclevel_state==. & year==2009 & month<11
replace icclevel_state=6 if ccode==490 & icclevel_state==. & year==2009 & month>10
replace icclevel_state=6 if ccode==490 & icclevel_state==. & year>2009 & year<2013
replace icclevel_state=0 if ccode==490 & icclevel_state==. & year>2012
replace icclevel_opp=1 if ccode==490 & icclevel_opp==. & year==2004 & month>3 & month<6
replace icclevel_opp=2 if ccode==490 & icclevel_opp==. & year==2004 & month>5
replace icclevel_opp=2 if ccode==490 & icclevel_opp==. & year==2005
replace icclevel_opp=2 if ccode==490 & icclevel_opp==. & year==2006 & month<2
replace icclevel_opp=3 if ccode==490 & icclevel_opp==. & year==2006 & month==2
replace icclevel_opp=4 if ccode==490 & icclevel_opp==. & year==2006 & month>2
replace icclevel_opp=5 if ccode==490 & icclevel_opp==. & year>2006 & year<2009
replace icclevel_opp=6 if ccode==490 & icclevel_opp==. & year>2008 & year<2014
replace icclevel_opp=6 if ccode==490 & icclevel_opp==. & year==2014 & month<5
replace icclevel_opp=5 if ccode==490 & icclevel_opp==. & year==2014 & month>4
replace icclevel_opp=5 if ccode==490 & icclevel_opp==. & year==2015 & month<9
replace icclevel_opp=6 if ccode==490 & icclevel_opp==. & year==2015 & month>8
replace icclevel_opp=6 if ccode==490 & icclevel_opp==. & year==2016 

tab icclevel
tab icclevel_state
tab icclevel_opp
sum icclevel_*

tab icclevel icclevel_state
tab icclevel icclevel_opp

sort ccode year month
save monthly_icclevel_directed.dta, replace

* merge into full dataset:
clear
insheet using countrymonthdata_icczuh.csv

browse ccode month year icc_stage1 icc_stage2 icc_onset investigation_onset targetlevel sideimplicated iccinv
rename icc_stage1 icc_stage1_old
rename icc_stage2 icc_stage2_old
rename icc_onset icc_onset_old
rename investigation_onset investigation_onset_old
rename targetlevel targetlevel_old
rename sideimplicated sideimplicated_old
rename iccinv iccinv_old

sort ccode year month
merge 1:1 ccode year month using monthly_icclevel_directed.dta
tab year if _merge==2
drop if _merge==2
drop _merge

corr icc_stage1 icc_stage1_old
corr icc_stage2 icc_stage2_old
corr icc_onset icc_onset_old
corr investigation_onset investigation_onset_old
corr targetlevel targetlevel_old
corr sideimplicated sideimplicated_old
corr iccinv_old icclevel
tab icc_stage1_old if icc_stage1==.
tab icc_stage2_old if icc_stage2==.
tab icc_onset_old if icc_onset==.
sum iccinvolvement
browse if iccinv_old!=icclevel & icclevel!=.

* fix missing data/start all ICC vars in July 2002 (court doesn't exist before then)
replace icc_stage1=0 if icc_stage1==. & year>2002 | icc_stage1==. & year==2002 & month>6
replace icc_stage1=. if icc_stage1!=. & year==2002 & month<7
tab icc_stage1
tab icc_stage1 icc_stage1_old
*drop icc_stage1_old

replace icc_stage2=0 if icc_stage2==. & year>2002 | icc_stage2==. & year==2002 & month>6
replace icc_stage2=. if icc_stage2!=. & year==2002 & month<7
tab icc_stage2
tab icc_stage2 icc_stage2_old
*drop icc_stage2_old

tab icc_onset_old
tab icc_onset
replace icc_onset=0 if icc_onset==. & year>2002 | icc_onset==. & year==2002 & month>6
replace icc_onset=. if icc_onset!=. & year==2002 & month<7
tab icc_onset
tab icc_onset icc_onset_old

tab investigation_onset
tab investigation_onset_old
replace investigation_onset=0 if investigation_onset==. & year>2002 | investigation_onset==. & year==2002 & month>6
replace investigation_onset=. if investigation_onset!=. & year==2002 & month<7
tab investigation_onset

tab targetlevel
tab targetlevel_old

tab sideimplicated 
tab sideimplicated_old

tab iccinvolvement
replace iccinvolvement=0 if iccinvolvement==. & year>2002 | iccinvolvement==. & year==2002 & month>6
replace iccinvolvement=. if iccinvolvement!=. & year==2002 & month<7
tab iccinvolvement

tab icclevel
tab iccinv_old
replace icclevel=0 if icclevel==. & year>2002 | icclevel==. & year==2002 & month>6
replace icclevel=. if icclevel!=. & year==2002 & month<7
tab icclevel

tab icclevel_state
replace icclevel_state=0 if icclevel_state==. & year>2002 | icclevel_state==. & year==2002 & month>6
replace icclevel_state=. if icclevel_state!=. & year==2002 & month<7
tab icclevel_state

tab icclevel_opp
replace icclevel_opp=0 if icclevel_opp==. & year>2002 | icclevel_opp==. & year==2002 & month>6
replace icclevel_opp=. if icclevel_opp!=. & year==2002 & month<7
tab icclevel_opp


save countrymonthdata_corrected.dta, replace
clear






*********************************************************
* 3. PTS data
*********************************************************

clear
insheet using pts_2016.csv
rename cow_code_n ccode
drop if ccode=="NA"
destring ccode, replace
replace pts_a=" " if pts_a=="NA"
replace pts_h=" " if pts_h=="NA"
replace pts_s=" " if pts_s=="NA"
destring pts_a, replace
destring pts_h, replace
destring pts_s, replace
keep ccode year pts_a pts_h pts_s
sort ccode year
by ccode: gen pts_a_lag=pts_a[_n-1]
by ccode: gen pts_h_lag=pts_h[_n-1]
by ccode: gen pts_s_lag=pts_s[_n-1]
save pts_2016.dta, replace

clear 
use countrymonthdata_corrected.dta
sort ccode year
merge m:1 ccode year using pts_2016.dta
drop if _merge==2
drop _merge
sum pts_a pts_h pts_s pts_a_lag pts_h_lag pts_s_lag amnesty hrw statedept pts year if year>2001
rename amnesty amnesty_old
rename hrw hrw_old
rename statedept statedept_old
rename pts pts_old

* create new pts and pts_lag variables 
** PTS = amnesty score, unless amnesty is missing, then State Dept.  if both are missing, HRW score is used
gen pts=pts_a
replace pts=pts_s if pts==.
replace pts=pts_h if pts==.
tab pts
browse ccode year month pts pts_a pts_s pts_h if pts==. & year>2001
tab icclevel if pts==.

gen pts_lag=pts_a_lag
replace pts_lag=pts_s_lag if pts_lag==.
replace pts_lag=pts_h_lag if pts_lag==.
tab pts_lag
browse ccode year month pts_lag pts_a_lag pts_s_lag pts_h_lag if pts_lag==. & year>2001
tab icclevel if pts_lag==.

corr pts_old pts_lag
corr pts_old pts_lag if year>2001
sum pts_old pts_lag
browse if pts_old!=. & pts_lag==.
* fill in South Sudan and Kosovo with current level for first year of independence
replace pts_lag=pts if pts_lag==. & ccode==347 & year==2008 | pts_lag==. & ccode==626 & year==2011

* this was correct, just labeled poorly.  
* new variable names: pts is current year pts score, pts_lag is last year's score








*********************************************************
* 3. POI PTS data
*********************************************************
rename poi_pts poi_pts_old
gen poi_pts=poi_pts_old
browse ccode year month icclevel focus_of_investigation poi1 poi2 poi3 pts poi_pts if iccinvolvement==1 | iccinvolvement==2 | poi1==1 | poi2==1 | poi3==1

* fix missing/incorrect poi_pts variable:

* usa (ccode =2): correct
* ccode 91: correct
* colombia (100): correct
* 101 : correct
replace poi_pts=2 if ccode==200 & iccinvolvement==1 & year>2003 & year<2007 /* this was missing */
replace poi_pts=2 if ccode==200 & iccinvolvement==1 & year>2013  /* this was missing */
replace poi_pts=4 if ccode==365 & poi_pts==. & iccinvolvement==1 & year>2007 & year<2015  /* this was missing */
replace poi_pts=4 if ccode==365 & poi_pts==. & iccinvolvement==1 & year==2015 & month<9  /* this was missing */
replace poi_pts=4 if ccode==369 & poi_pts==. & iccinvolvement==1 & year==2014  /* this was missing */
replace poi_pts=3 if ccode==372 & poi_pts==. & iccinvolvement==1 & year>2007  /* this was missing */
* 432 correct
replace poi1=0 if ccode==437 & poi1==1 & year==2002 & month<9 /* this was wrong - pd of investigation was coded starting in Jan, but didn't start until Sept. */
* 437 correct
replace poi1=0 if ccode==438 & poi1==1 & year==2009 & month<9 /* this was wrong - pd of investigation is only sept-oct 2009 */
replace poi_pts=5 if ccode==438 & poi_pts==. & iccinvolvement==1 & year>2008 /* this was missing */
replace poi_pts=5 if ccode==475 & poi_pts==4 & year==2015 & month==1 /* this was wrong */
replace poi_pts=4 if ccode==482 & poi_pts==. & year>2011 & year<2014 /* missing */
replace poi_pts=4 if ccode==482 & poi_pts==. & year==2014 & month==1 /* missing */
* 490 correct
* 500 correct
replace poi_pts=4 if ccode==501 & poi_pts==. & iccinvolvement==1 & year>2008
* 620 correct
* 625 correct
replace poi_pts=4 if ccode==666 & iccinvolvement==1 & year==2013 & month>4
replace poi_pts=4 if ccode==666 & iccinvolvement==1 & year==2014 & month<12
replace poi_pts=4 if ccode==666 & iccinvolvement==1 & year==2015
* 700 correct
replace poi_pts=5 if ccode==731 & iccinvolvement==1 & year==2010 & month==12
replace poi_pts=5 if ccode==731 & iccinvolvement==1 & year>2010 & year<2015

sum poi_pts icclevel if icclevel>0 & icclevel!=.
tab poi_pts
tab poi_pts icclevel

save countrymonthdata_corrected.dta, replace
clear







*********************************************************
* 3. OSV data
*********************************************************

clear 
insheet using ged50.csv
keep if type_of_violence==3
sum dyad_dset_id
sort dyad_dset_id
keep if dyad_dset_id<1000

split date_start, p(-)
rename date_start1 startyear
rename date_start2 startmonth
destring startyear, replace
destring startmonth, replace
sum startyear startmonth
corr best_est deaths_civilians
replace side_a_dset_id=679 if side_a_dset_id==678 /*change Yemen ccode to match what's in master dataset*/

collapse (sum) deaths_civilians , by(startyear startmonth side_a_dset_id side_a)
sort side_a_dset_id startyear startmonth
rename side_a_dset_id ccode
rename startyear year
rename startmonth month
drop if year<2001
sort ccode year month
sum deaths_civilian
save state_osv.dta, replace
clear

use countrymonthdata_corrected.dta
merge 1:1 ccode year month using state_osv.dta
sum year
drop _merge
rename osv_state osv_state_old
rename deaths_civilians osv_state
replace osv_state=0 if osv_state==. & ccode!=652
corr osv_state_old osv_state
browse if osv_state==. & year>2001

* input Syria data from syria_osv_yearly.xlsx
replace osv_state=0 if ccode==652 & osv_state==. & year<2011
replace osv_state=244 if ccode==652 & osv_state==. & year==2011
replace osv_state=28 if ccode==652 & osv_state==. & year==2012
replace osv_state=51 if ccode==652 & osv_state==. & year==2013
replace osv_state=3 if ccode==652 & osv_state==. & year>2013

* create running sum of osv_state
gen date=ym(year, month)
sort ccode date
by ccode: gen runsum_osvstate=sum(osv_state) if date>509
sum runsum_osvstate year if date>509
corr runsum_osvstate osv_state_old

save countrymonthdata_corrected.dta, replace
clear

* REBEL OSV
clear 
insheet using ged50.csv
keep if type_of_violence==3
sum dyad_dset_id
sort dyad_dset_id
keep if dyad_dset_id>1000

split date_start, p(-)
rename date_start1 startyear
rename date_start2 startmonth
destring startyear, replace
destring startmonth, replace
sum startyear startmonth
corr best_est deaths_civilians /* not exact, but close */

* add ccodes for country location
browse year conflict_name country
gen cowcode = .
replace country = "Congo Democratic Republic of" if country=="Congo, Dem. Rep."
replace country = "Congo" if country=="Congo, Rep."
replace country = "Ivory Coast" if country=="Cote d'Ivoire"
replace country= "Czechoslovakia" if country=="Czech Republic"
replace country= "Egypt" if country=="Agypt, Arab Rep."
replace country = "Gambia" if country=="Gambia, The"
replace country= "Germany Federal Republic of" if country=="Germany"
replace country= "Iran" if country=="Iran, Islamic Rep."
replace cowcode = 2 if country=="United States"
replace cowcode = 2 if country=="USA"
replace cowcode = 20 if country == "Canada"
replace cowcode = 31 if country=="Bahamas, The"
replace cowcode = 31 if country=="Bahamas"
replace cowcode = 40 if country=="Cuba"
replace cowcode = 41 if country == "Haiti"
replace cowcode = 42 if country == "Dominican Republic"
replace cowcode = 42 if country == "Dom. Rep."
replace cowcode = 51 if country == "Jamaica"
replace cowcode = 52 if country == "Trinidad and Tobago"
replace cowcode = 52 if country == "Trinidad &Tobago"
replace cowcode = 52 if country == "Trinidad-Tobago"
replace cowcode = 53 if country == "Barbados"
replace cowcode = 54 if country == "Dominica"
replace cowcode = 55 if country == "Grenada"
replace cowcode = 56 if country == "St. Lucia"|country=="Saint Lucia"
replace cowcode = 57 if country == "St. Vincent and the Grenadines"
replace cowcode = 57 if country == "St.Vincent & Grenadines"
replace cowcode = 58 if country == "Antigua and Barbuda"
replace cowcode = 58 if country == "Antigua"
replace cowcode=60 if country=="St. Kitts and Nevis"
replace cowcode=60 if country=="St. Kitts & Nevis"
replace cowcode = 70 if country == "Mexico"
replace cowcode = 80 if country == "Belize"
replace cowcode = 90 if country == "Guatemala"
replace cowcode = 91 if country == "Honduras"
replace cowcode = 92 if country == "El Salvador"
replace cowcode = 93 if country == "Nicaragua"
replace cowcode = 94 if country == "Costa Rica"
replace cowcode = 95 if country == "Panama"
replace cowcode = 100 if country == "Colombia"
replace cowcode = 100 if country == "Columbia"
replace cowcode = 101 if country == "Venezuela"
replace cowcode = 101 if country == "Venezuela, RB"
replace cowcode = 110 if country == "Guyana"
replace cowcode=115 if country=="Suriname"
replace cowcode = 115 if country == "Surinam"
replace cowcode = 130 if country == "Ecuador"
replace cowcode = 135 if country == "Peru"
replace cowcode = 140 if country == "Brazil"
replace cowcode = 145 if country == "Bolivia"
replace cowcode = 150 if country == "Paraguay"
replace cowcode = 155 if country == "Chile"
replace cowcode = 160 if country == "Argentina"
replace cowcode = 165 if country == "Uruguay"
replace cowcode = 200 if country == "United Kingdom"
replace cowcode = 200 if country == "UK"
replace cowcode = 205 if country == "Ireland"
replace cowcode = 210 if country == "Netherlands"
replace cowcode = 211 if country == "Belgium"
replace cowcode = 212 if country == "Luxembourg"
replace cowcode = 220 if country == "France"
replace cowcode = 221 if country == "Monaco"
replace cowcode = 223 if country == "Liechtenstein"|country=="Lichtenstein"
replace cowcode = 225 if country == "Switzerland"
replace cowcode = 230 if country == "Spain"
replace cowcode = 232 if country == "Andorra"
replace cowcode = 235 if country == "Portugal"
replace cowcode = 260 if country == "Germany"
replace cowcode = 260 if country=="West Germany"
replace cowcode = 260 if country=="Germany Federal Republic of"
replace cowcode = 260 if country=="Germany Federal Republic"
replace cowcode = 260 if country=="FRG/Germany"
replace cowcode=265 if country=="East Germany"
replace cowcode=265 if country=="German Democratic Republic"
replace cowcode=265 if country=="GDR"
replace cowcode = 290 if country == "Poland"
replace cowcode = 305 if country == "Austria"
replace cowcode = 310 if country == "Hungary"
replace cowcode = 315 if country == "Czech Republic"
replace cowcode = 315 if country == "Czechoslovakia"
replace cowcode = 315 if country == "Czech Rep."
replace cowcode = 317 if country == "Slovak Republic"
replace cowcode = 317 if country == "Slovakia"
replace cowcode = 325 if country == "Italy"
replace cowcode=331 if country=="San Marino"
replace cowcode = 338 if country == "Malta"
replace cowcode = 339 if country == "Albania"
replace cowcode = 343 if country=="Macedonia" 
replace cowcode = 343 if country=="Macedonia, FYR" 
replace cowcode = 344 if country == "Croatia"
replace cowcode = 345 if country == "Yugoslavia"
replace cowcode = 345 if country == "Serbia and Montenegro"
replace cowcode = 345 if country == "Yugoslavia, FR (Serbia/Montenegro)" 
replace cowcode = 345 if country == "Serbia & Montenegro"
replace cowcode = 345 if country == "Serbia"
replace cowcode = 346 if country=="Bosnia and Herzegovina"
replace cowcode = 346 if country=="Bosnia-Herz"
replace cowcode = 349 if country == "Slovenia"
replace cowcode = 350 if country == "Greece"
replace cowcode = 352 if country == "Cyprus"
replace cowcode = 355 if country == "Bulgaria"
replace cowcode = 359 if country=="Moldova" 
replace cowcode = 360 if country == "Romania"
replace cowcode = 365 if country == "Russia"
replace cowcode = 365 if country == "Russian Federation"
replace cowcode = 365 if country=="USSR"
replace cowcode = 365 if country=="Soviet Union"
replace cowcode = 366 if country == "Estonia"
replace cowcode = 367 if country == "Latvia"
replace cowcode = 368 if country == "Lithuania"
replace cowcode = 369 if country == "Ukraine"
replace cowcode = 370 if country == "Belarus"
replace cowcode = 371 if country == "Armenia"
replace cowcode = 372 if country == "Georgia"
replace cowcode = 373 if country == "Azerbaijan"
replace cowcode = 375 if country == "Finland"
replace cowcode = 380 if country == "Sweden"
replace cowcode = 385 if country == "Norway"
replace cowcode = 390 if country == "Denmark"
replace cowcode = 395 if country == "Iceland"
replace cowcode = 402 if country == "Cape Verde"
replace cowcode = 402 if country == "C. Verde Is."
replace cowcode = 403 if country == "Sao Tome and Principe"
replace cowcode = 403 if country == "Sao Tome & Principe"
replace cowcode = 404 if country == "Guinea-Bissau"
replace cowcode = 411 if country == "Equatorial Guinea"
replace cowcode = 411 if country == "Eq. Guinea"
replace cowcode = 420 if country == "Gambia"
replace cowcode = 420 if country=="The Gambia" 
replace cowcode = 420 if country=="Gambia, The" 
replace cowcode = 432 if country == "Mali"
replace cowcode = 433 if country == "Senegal"
replace cowcode = 434 if country == "Benin"
replace cowcode = 435 if country == "Mauritania"
replace cowcode = 436 if country == "Niger"
replace cowcode = 437 if country == "Ivory Coast"
replace cowcode = 437 if country == "Cote d'Ivoire"
replace cowcode = 437 if country == "Cote d`Ivoire"
replace cowcode = 438 if country == "Guinea"
replace cowcode = 439 if country == "Burkina Faso"
replace cowcode = 450 if country == "Liberia"
replace cowcode = 451 if country == "Sierra Leone"
replace cowcode = 452 if country == "Ghana"
replace cowcode = 461 if country == "Togo"
replace cowcode = 471 if country == "Cameroon"
replace cowcode = 475 if country == "Nigeria"
replace cowcode = 481 if country == "Gabon"
replace cowcode = 482 if country == "Central African Republic"
replace cowcode = 482 if country == "Cent. Af. Rep."
replace cowcode = 483 if country == "Chad"
replace country = "Congo Republic" if country=="Congo Republic of"
replace cowcode = 484 if country == "Congo Republic"
replace cowcode = 484 if country == "Congo, Rep."
replace cowcode = 484 if country=="Congo"
replace cowcode= 484 if country=="Congo, Republic of"
replace cowcode = 490 if country == "Congo Democratic Republic"
replace cowcode = 490 if country=="Congo Democratic Republic of"
replace cowcode = 490 if country=="Congo, Dem. Rep."|country=="Congo, Democratic Republic Of"
replace cowcode = 490 if country == "Zaire (Democ Republic Congo)"
replace cowcode = 500 if country == "Uganda"
replace cowcode = 501 if country == "Kenya"
replace cowcode = 510 if country == "Tanzania"
replace cowcode = 516 if country == "Burundi"
replace cowcode = 517 if country == "Rwanda"
replace cowcode = 520 if country == "Somalia"
replace cowcode = 522 if country == "Djibouti"
replace cowcode = 530 if country == "Ethiopia"
replace cowcode = 531 if country == "Eritrea"
replace cowcode = 540 if country == "Angola"
replace cowcode = 541 if country == "Mozambique"
replace cowcode = 551 if country == "Zambia"
replace cowcode = 552 if country == "Zimbabwe"
replace cowcode = 553 if country == "Malawi"
replace cowcode = 560 if country == "South Africa"
replace cowcode = 560 if country == "S. Africa"
replace cowcode = 565 if country == "Namibia"
replace cowcode = 570 if country == "Lesotho"
replace cowcode = 571 if country == "Botswana"
replace cowcode = 572 if country == "Swaziland"
replace cowcode = 580 if country == "Madagascar"
replace cowcode = 581 if country == "Comoros"
replace cowcode = 581 if country == "Comoro Is."
replace cowcode = 590 if country == "Mauritius"
replace cowcode = 591 if country == "Seychelles"
replace cowcode = 600 if country == "Morocco"
replace cowcode = 615 if country == "Algeria"
replace cowcode = 616 if country == "Tunisia"
replace cowcode = 620 if country == "Libya"
replace cowcode = 625 if country == "Sudan"
replace cowcode = 630 if country == "Iran"
replace cowcode = 630 if country == "Iran, Islamic Rep."
replace cowcode = 630 if country == "Iran, Islamic Republic"
replace cowcode = 640 if country == "Turkey"
replace cowcode = 645 if country == "Iraq"
replace cowcode = 651 if country == "Egypt"
replace cowcode = 651 if country == "Egypt, Arab Rep."
replace cowcode = 651 if country == "Egypt, Arab Republic"
replace cowcode = 652 if country == "Syria"
replace cowcode = 652 if country == "Syrian Arab Republic"
replace cowcode = 652 if country == "Syrian Arab Rep."
replace cowcode = 660 if country == "Lebanon"
replace cowcode = 663 if country == "Jordan"
replace cowcode = 666 if country == "Israel"
replace cowcode = 670 if country == "Saudi Arabia"
replace cowcode = 690 if country == "Kuwait"
replace cowcode = 692 if country == "Bahrain"
replace cowcode = 694 if country == "Qatar"
replace cowcode = 696 if country == "United Arab Emirates"
replace cowcode = 696 if country == "UAE"
replace cowcode = 698 if country == "Oman"
replace cowcode = 700 if country == "Afghanistan"
replace cowcode = 701 if country == "Turkmenistan"
replace cowcode = 702 if country == "Tajikistan"
replace cowcode = 703 if country == "Kyrgyz Republic"
replace cowcode = 703 if country == "Kyrgyzstan"
replace cowcode = 704 if country == "Uzbekistan"
replace cowcode = 705 if country == "Kazakhstan"
replace cowcode = 710 if country == "China"
replace cowcode = 710 if country == "PRC"
replace cowcode = 712 if country == "Mongolia"
replace cowcode = 713 if country == "Taiwan"
replace cowcode = 731 if country=="Korea, Dem. Rep."
replace cowcode = 731 if country=="Korea, Democratic Republic"
replace cowcode = 731 if country=="Korea Democratic Republic"
replace cowcode = 731 if country=="Korea, North"
replace cowcode = 731 if country=="PRK"
replace cowcode = 732 if country=="Korea Republic of"
replace cowcode = 732 if country=="Korea, Republic of"
replace cowcode = 732 if country=="Korea, Rep."
replace cowcode = 732 if country=="Korea, South"
replace cowcode = 732 if country=="ROK"
replace cowcode = 740 if country == "Japan"
replace cowcode = 750 if country == "India"
replace cowcode = 760 if country == "Bhutan"
replace cowcode = 770 if country=="Pakistan"
replace cowcode = 771 if country == "Bangladesh"
replace cowcode = 775 if country == "Myanmar"
replace cowcode = 775 if country=="Burma" 
replace cowcode = 679 if country=="Yemen People's Republic" 
replace cowcode = 679 if country=="Yemen" 
replace cowcode = 679 if country=="Yemen, Rep." 
replace cowcode = 679 if country=="Yemen Republic" 
replace cowcode=680 if country== "Yemen (PDR)"
replace cowcode = 678 if country=="Yemen (AR)" 
replace cowcode = 780 if country == "Sri Lanka"
replace cowcode=781 if country=="Maldives"
replace cowcode = 790 if country == "Nepal"
replace cowcode = 800 if country == "Thailand"
replace cowcode = 811 if country == "Cambodia"
replace cowcode = 812 if country == "Laos"
replace cowcode = 812 if country == "Lao PDR"
replace cowcode = 818 if country == "Vietnam"
replace cowcode = 820 if country == "Malaysia"
replace cowcode = 830 if country == "Singapore"
replace cowcode = 835 if country == "Brunei"|country=="Brunei Darussalam" 
replace cowcode = 840 if country == "Philippines"
replace cowcode = 850 if country == "Indonesia"
replace cowcode=860 if country=="Timor-Leste"
replace cowcode=860 if country=="East Timor"
replace cowcode = 900 if country == "Australia"
replace cowcode = 910 if country == "Papua New Guinea"
replace cowcode = 910 if country == "P. N. Guinea"
replace cowcode = 920 if country == "New Zealand"
replace cowcode = 935 if country == "Vanuatu"
replace cowcode = 940 if country == "Solomon Islands"
replace cowcode = 940 if country == "Solomon Is."
replace cowcode = 946 if country=="Kiribati"
replace cowcode = 950 if country == "Fiji"
replace cowcode=955 if country=="Tonga"
replace cowcode=983 if country=="Marshall Islands"
replace cowcode=986  if country=="Palau"
replace cowcode = 987 if country=="Micronesia, Fed. Sts."
replace cowcode = 987 if country=="Micronesia"
replace cowcode = 987 if country=="Federated States of Micronesia"
replace cowcode=990 if country=="Samoa"
replace cowcode=990 if country=="W. Samoa"

replace cowcode=626 if country=="South Sudan"
replace cowcode=347 if country=="Kosovo"
replace cowcode=341 if country=="Montenegro"
replace cowcode = 679 if country=="Yemen (North Yemen)"
replace cowcode = 365 if country=="Russia (Soviet Union)"
replace cowcode = 811 if country == "Cambodia (Kampuchea)"
replace cowcode = 346 if country=="Bosnia-Herzegovina"
replace cowcode = 490 if country == "DR Congo (Zaire)"
replace cowcode = 552 if country == "Zimbabwe (Rhodesia)"
replace cowcode = 2 if country=="United States of America"
replace cowcode = 345 if country == "Serbia (Yugoslavia)"
 
sum year cowcode
rename cowcode ccode

collapse (sum) deaths_civilians , by(startyear startmonth ccode)
sort ccode startyear startmonth
rename startyear year
rename startmonth month
drop if year<2001
sort ccode year month
sum deaths_civilian
save rebel_osv.dta, replace
clear

use countrymonthdata_corrected.dta
merge 1:1 ccode year month using rebel_osv.dta
sum year
drop _merge
rename osv_rebel osv_rebel_old
rename deaths_civilians osv_rebel
replace osv_rebel=0 if osv_rebel==. & ccode!=652
corr osv_rebel_old osv_rebel

* input Syria data from syria_osv_yearly.xlsx
replace osv_rebel=0 if ccode==652 & osv_rebel==. & year<2013
replace osv_rebel=73 if ccode==652 & osv_rebel==. & year==2013
replace osv_rebel=47 if ccode==652 & osv_rebel==. & year==2014
replace osv_rebel=213 if ccode==652 & osv_rebel==. & year==2015
browse if osv_rebel==.

* create running sum of osv_rebel
sort ccode year month
by ccode: gen runsum_osvrebel=sum(osv_rebel) if date>509
sum runsum_osvrebel year if date>509
corr runsum_osvrebel osv_rebel_old

* create osv_total variables
rename osv_total osv_total_old
gen osv_total=osv_state+osv_rebel
sum osv_total

gen runsum_osvtotal=runsum_osvstate+runsum_osvrebel
sum runsum_osvtotal
corr osv_total_old runsum_osvtotal 

save countrymonthdata_corrected.dta, replace
clear





*********************************************************
* 4. POI_OSV data
*********************************************************

clear
use countrymonthdata_corrected.dta
sort ccode year month
rename poi_osv_state poi_osv_state_old
rename poi_osv_rebel poi_osv_rebel_old
gen poi_osv_state=poi_osv_rebel_old
gen poi_osv_rebel=poi_osv_state_old
browse ccode year month date icclevel focus_of_investigation poi1 poi2 poi3 osv_state poi_osv_state if iccinvolvement==1 | iccinvolvement==2 | poi1==1 | poi2==1 | poi3==1

* 2 correct
* 91 correct
* 100
by ccode: gen runsums100=sum(osv_state) if date>513
replace poi_osv_state=runsums100 if icclevel>0 & icclevel!=. & ccode==100
drop runsums100
by ccode: gen runsums101=sum(osv_state) if date>509 & ccode==101
replace poi_osv_state=runsums101 if icclevel>0 & icclevel!=. & ccode==101
drop runsums101
* 200 correct
by ccode: gen runsum365=sum(osv_state) if date>648  & ccode==365
replace poi_osv_state=runsum365 if ccode==365 & icclevel==1 & focus_of_investigation=="July-Oct 2008 & Feb 2014-present"
drop runsum365
* 369 correct
* 372 correct
by ccode: gen runsum432=sum(osv_state) if date>623 & ccode==432
replace poi_osv_state=runsum432 if icclevel>0 & icclevel!=. & ccode==432
drop runsum432
by ccode: gen runsum437=sum(osv_state) if date>511 & ccode==437
replace poi_osv_state=runsum437 if icclevel>0 & icclevel!=. & ccode==437
drop runsum437
*438 correct
by ccode: gen runsum475=sum(osv_state) if date>509 & ccode==475
replace poi_osv_state=runsum475 if icclevel>0 & icclevel!=. & ccode==475
drop runsum475
by ccode: gen runsum482=sum(osv_state) if date>509 & ccode==482
replace poi_osv_state=runsum482 if icclevel>0 & icclevel!=. & ccode==482 & date<624
replace poi_osv_state=176 if ccode==482 & date>623 & date<649
replace poi_osv_state=runsum482 if icclevel>0 & icclevel!=. & ccode==482 & date>648
drop runsum482
by ccode: gen runsum490=sum(osv_state) if date>509 & ccode==490
replace poi_osv_state=runsum490 if icclevel>0 & icclevel!=. & ccode==490
drop runsum490
by ccode: gen runsum500=sum(osv_state) if date>509 & ccode==500
replace poi_osv_state=runsum500 if icclevel>0 & icclevel!=. & ccode==500
drop runsum500
* 501 correct
by ccode: gen runsum620=sum(osv_state) if date>612 & ccode==620
replace poi_osv_state=runsum620 if icclevel>0 & icclevel!=. & ccode==620 & date>612
drop runsum620
by ccode: gen runsum625=sum(osv_state) if date>509 & ccode==625
replace poi_osv_state=runsum625 if icclevel>0 & icclevel!=. & ccode==625
drop runsum625
by ccode: gen runsum666=sum(osv_state) if date>509 & ccode==666
replace poi_osv_state=runsum666 if icclevel>0 & icclevel!=. & ccode==666 & focus_of_investigation=="July 2002-present"
drop runsum666
replace poi_osv_state=0 if icclevel>0 & icclevel!=. & ccode==666 & focus_of_investigation=="May-June 2010"
replace poi_osv_state=0 if icclevel>0 & icclevel!=. & ccode==666 & focus_of_investigation=="June 2014-present"
by ccode: gen runsum700=sum(osv_state) if date>519 & ccode==700
replace poi_osv_state=runsum700 if icclevel>0 & icclevel!=. & ccode==700
drop runsum700
* 731 correct


browse ccode year month date icclevel focus_of_investigation poi1 poi2 poi3 osv_rebel poi_osv_rebel if iccinvolvement==1 | iccinvolvement==2 | poi1==1 | poi2==1 | poi3==1
* 2 correct
* 91 correct
by ccode: gen runsum100=sum(osv_rebel) if date>513
replace poi_osv_rebel=runsum100 if icclevel>0 & icclevel!=. & ccode==100
drop runsum100
* 101 correct
by ccode: gen runsum200=sum(osv_rebel) if date>515 & date<588 & ccode==200
replace poi_osv_rebel=55 if icclevel>0 & icclevel!=. & ccode==200 & focus_of_investigation=="2003-2008"
drop runsum200
replace poi_osv_rebel=1 if icclevel>0 & icclevel!=. & ccode==365 & date>583 & focus_of_investigation=="July-Oct 2008"
replace poi_osv_rebel=5 if icclevel>0 & icclevel!=. & ccode==365 & focus_of_investigation=="July-Oct 2008 & Feb 2014-present"
* 369 correct
* 372 correct
by ccode: gen runsum432=sum(osv_rebel) if date>623 & ccode==432
replace poi_osv_rebel=runsum432 if icclevel>0 & icclevel!=. & ccode==432
drop runsum432
by ccode: gen runsum437=sum(osv_rebel) if date>511 & ccode==437
replace poi_osv_rebel=runsum437 if icclevel>0 & icclevel!=. & ccode==437
drop runsum437
*438 correct
by ccode: gen runsum475=sum(osv_rebel) if date>509 & ccode==475
replace poi_osv_rebel=runsum475 if icclevel>0 & icclevel!=. & ccode==475
drop runsum475
by ccode: gen runsum482=sum(osv_rebel) if date>509 & ccode==482
replace poi_osv_rebel=runsum482 if icclevel>0 & icclevel!=. & ccode==482 & date<624
replace poi_osv_rebel=runsum482 if icclevel>0 & icclevel!=. & ccode==482 & date>648
drop runsum482
by ccode: gen runsum490=sum(osv_rebel) if date>509 & ccode==490
replace poi_osv_rebel=runsum490 if icclevel>0 & icclevel!=. & ccode==490
drop runsum490
by ccode: gen runsum500=sum(osv_rebel) if date>509 & ccode==500
replace poi_osv_rebel=runsum500 if icclevel>0 & icclevel!=. & ccode==500
drop runsum500
by ccode: gen runsum501=sum(osv_rebel) if date>563 & date<588 & ccode==501
replace poi_osv_rebel=161 if icclevel>0 & icclevel!=. & ccode==501 
drop runsum501
by ccode: gen runsum620=sum(osv_rebel) if date>612 & ccode==620
replace poi_osv_rebel=runsum620 if icclevel>0 & icclevel!=. & ccode==620 & date>612
drop runsum620
by ccode: gen runsum625=sum(osv_rebel) if date>509 & ccode==625
replace poi_osv_rebel=runsum625 if icclevel>0 & icclevel!=. & ccode==625
drop runsum625
by ccode: gen runsum666=sum(osv_rebel) if date>509 & ccode==666
replace poi_osv_rebel=runsum666 if icclevel>0 & icclevel!=. & ccode==666 & focus_of_investigation=="July 2002-present"
drop runsum666
replace poi_osv_rebel=3 if icclevel>0 & icclevel!=. & ccode==666 & focus_of_investigation=="May-June 2010"
by ccode: gen runsum666=sum(osv_rebel) if date>652 & ccode==666
replace poi_osv_rebel=runsum666 if icclevel>0 & icclevel!=. & ccode==666 & focus_of_investigation=="June 2014-present"
drop runsum666
by ccode: gen runsum700=sum(osv_rebel) if date>519 & ccode==700
replace poi_osv_rebel=runsum700 if icclevel>0 & icclevel!=. & ccode==700
drop runsum700
* 731 correct

rename poi_osv_total poi_osv_total_old
gen poi_osv_total=poi_osv_state+poi_osv_rebel
sum poi_osv_state poi_osv_rebel poi_osv_total icclevel if icclevel>0 & icclevel!=.

save countrymonthdata_corrected.dta, replace
clear






*********************************************************
* 5. Civil War data
*********************************************************
clear
insheet using ucdp-prio-acd-4-2016.csv
drop if typeofconflict<3
drop if year<2001
destring gwnoa, replace
sum gwnoa

keep year gwnoa sidea intensitylevel typeofconflict
collapse (max) intensitylevel, by(gwnoa year sidea)
duplicates list gwnoa year
gen civilwar=1
rename gwnoa ccode
drop sidea
sort ccode year
tsset ccode year
tsfill, full
drop if ccode==626 & year<2011
replace ccode=679 if ccode==678
replace intensitylevel=0 if intensitylevel==.
replace civilwar=0 if civilwar==.
sort ccode year
by ccode: gen civilwar_lag=civilwar[_n-1]
by ccode: gen intensitylevel_lag=intensitylevel[_n-1]
save ucdp_cwar.dta, replace
clear

use countrymonthdata_corrected.dta
rename civilwar civilwar_old
merge m:1 ccode year using ucdp_cwar.dta
drop _merge
tab civilwar
replace civilwar=0 if civilwar==.
replace civilwar_lag=0 if civilwar_lag==. & year>2001
tab civilwar civilwar_old
tab civilwar_old civilwar_lag
browse ccode year month civilwar civilwar_lag civilwar_old if civilwar_lag!=civilwar_old & civilwar_lag!=.
* not sure what the inconsistencies are caused by
tab intensitylevel
replace intensitylevel=0 if intensitylevel==.
replace intensitylevel_lag=0 if intensitylevel_lag==. & year>2001
tab intensitylevel
tab intensitylevel_lag

* create running max civil war variables (has civil war happened since 2002 var)
sort ccode year month
browse ccode year month date civilwar intensitylevel
gen runmax_civilwar=civilwar if date==510
by ccode: replace runmax_civilwar=max(civilwar,runmax_civilwar[_n-1]) if date>510 & runmax_civilwar==.
tab runmax_civilwar
tab civilwar
sum runmax_civilwar date if date>509

browse ccode year month date intensitylevel
gen runmax_intensitylevel=intensitylevel if date==510
by ccode: replace runmax_intensitylevel=max(intensitylevel,runmax_intensitylevel[_n-1]) if date>510 & runmax_intensitylevel==.
tab runmax_intensitylevel
tab intensitylevel
sum runmax_intensitylevel date if date>509

save countrymonthdata_corrected.dta, replace
clear








****************************************************************
* 6. GDP data (world bank gdp per capita in US current dollars)
****************************************************************

clear 
insheet using worldbank_gdp_2016.csv

drop v1
drop v2
rename v3 countryname
rename v4 countrycode
rename v5 yr2000
rename v6 yr2001
rename v7 yr2002
rename v8 yr2003
rename v9 yr2004
rename v10 yr2005
rename v11 yr2006
rename v12 yr2007
rename v13 yr2008
rename v14 yr2009
rename v15 yr2010
rename v16 yr2011
rename v17 yr2012
rename v18 yr2013
rename v19 yr2014
rename v20 yr2015
rename v21 yr2016
drop yr2016
drop if countryname=="Country Name"
save worldbankgdpdata.dta, replace

keep countryname countrycode yr2015
rename yr2015 gdp
gen year=.
order countryname countrycode year gdp
drop if _n>0 /* Create an empty file for appending in newly saved csts files */
save temp_gdp_2000-2015.dta , replace

program csts_gdp
	forvalues i = 2000(1)2015 {
		use worldbankgdpdata.dta, clear
		keep countryname countrycode yr`i'
		rename yr`i' gdp
		gen year=`i'
		order countryname countrycode year gdp
		save temp_gdp_`i'.dta , replace
		use temp_gdp_2000-2015.dta , clear
		append using temp_gdp_`i'.dta
		save temp_gdp_2000-2015.dta , replace
	}
end

csts_gdp
sort countryname year
browse

replace gdp=" " if gdp==".."
destring gdp, replace

* add ccodes for country location
gen cowcode = .
rename countryname country
replace country = "Congo Democratic Republic of" if country=="Congo, Dem. Rep."
replace country = "Congo" if country=="Congo, Rep."
replace country = "Ivory Coast" if country=="Cote d'Ivoire"
replace country= "Czechoslovakia" if country=="Czech Republic"
replace country= "Egypt" if country=="Agypt, Arab Rep."
replace country = "Gambia" if country=="Gambia, The"
replace country= "Germany Federal Republic of" if country=="Germany"
replace country= "Iran" if country=="Iran, Islamic Rep."
replace cowcode = 2 if country=="United States"
replace cowcode = 2 if country=="USA"
replace cowcode = 20 if country == "Canada"
replace cowcode = 31 if country=="Bahamas, The"
replace cowcode = 31 if country=="Bahamas"
replace cowcode = 40 if country=="Cuba"
replace cowcode = 41 if country == "Haiti"
replace cowcode = 42 if country == "Dominican Republic"
replace cowcode = 42 if country == "Dom. Rep."
replace cowcode = 51 if country == "Jamaica"
replace cowcode = 52 if country == "Trinidad and Tobago"
replace cowcode = 52 if country == "Trinidad &Tobago"
replace cowcode = 52 if country == "Trinidad-Tobago"
replace cowcode = 53 if country == "Barbados"
replace cowcode = 54 if country == "Dominica"
replace cowcode = 55 if country == "Grenada"
replace cowcode = 56 if country == "St. Lucia"|country=="Saint Lucia"
replace cowcode = 57 if country == "St. Vincent and the Grenadines"
replace cowcode = 57 if country == "St.Vincent & Grenadines"
replace cowcode = 58 if country == "Antigua and Barbuda"
replace cowcode = 58 if country == "Antigua"
replace cowcode=60 if country=="St. Kitts and Nevis"
replace cowcode=60 if country=="St. Kitts & Nevis"
replace cowcode = 70 if country == "Mexico"
replace cowcode = 80 if country == "Belize"
replace cowcode = 90 if country == "Guatemala"
replace cowcode = 91 if country == "Honduras"
replace cowcode = 92 if country == "El Salvador"
replace cowcode = 93 if country == "Nicaragua"
replace cowcode = 94 if country == "Costa Rica"
replace cowcode = 95 if country == "Panama"
replace cowcode = 100 if country == "Colombia"
replace cowcode = 100 if country == "Columbia"
replace cowcode = 101 if country == "Venezuela"
replace cowcode = 101 if country == "Venezuela, RB"
replace cowcode = 110 if country == "Guyana"
replace cowcode=115 if country=="Suriname"
replace cowcode = 115 if country == "Surinam"
replace cowcode = 130 if country == "Ecuador"
replace cowcode = 135 if country == "Peru"
replace cowcode = 140 if country == "Brazil"
replace cowcode = 145 if country == "Bolivia"
replace cowcode = 150 if country == "Paraguay"
replace cowcode = 155 if country == "Chile"
replace cowcode = 160 if country == "Argentina"
replace cowcode = 165 if country == "Uruguay"
replace cowcode = 200 if country == "United Kingdom"
replace cowcode = 200 if country == "UK"
replace cowcode = 205 if country == "Ireland"
replace cowcode = 210 if country == "Netherlands"
replace cowcode = 211 if country == "Belgium"
replace cowcode = 212 if country == "Luxembourg"
replace cowcode = 220 if country == "France"
replace cowcode = 221 if country == "Monaco"
replace cowcode = 223 if country == "Liechtenstein"|country=="Lichtenstein"
replace cowcode = 225 if country == "Switzerland"
replace cowcode = 230 if country == "Spain"
replace cowcode = 232 if country == "Andorra"
replace cowcode = 235 if country == "Portugal"
*replace cowcode = 260 if country == "Germany"
*replace cowcode = 260 if country=="West Germany"
*replace cowcode = 260 if country=="Germany Federal Republic of"
*replace cowcode = 260 if country=="Germany Federal Republic"
*replace cowcode = 260 if country=="FRG/Germany"
replace cowcode=265 if country=="East Germany"
replace cowcode=265 if country=="German Democratic Republic"
replace cowcode=265 if country=="GDR"
replace cowcode = 290 if country == "Poland"
replace cowcode = 305 if country == "Austria"
replace cowcode = 310 if country == "Hungary"
*replace cowcode = 315 if country == "Czech Republic"
replace cowcode = 315 if country == "Czechoslovakia"
replace cowcode = 315 if country == "Czech Rep."
replace cowcode = 317 if country == "Slovak Republic"
replace cowcode = 317 if country == "Slovakia"
replace cowcode = 325 if country == "Italy"
replace cowcode=331 if country=="San Marino"
replace cowcode = 338 if country == "Malta"
replace cowcode = 339 if country == "Albania"
replace cowcode = 343 if country=="Macedonia" 
replace cowcode = 343 if country=="Macedonia, FYR" 
replace cowcode = 344 if country == "Croatia"
replace cowcode = 345 if country == "Yugoslavia"
replace cowcode = 345 if country == "Serbia and Montenegro"
replace cowcode = 345 if country == "Yugoslavia, FR (Serbia/Montenegro)" 
replace cowcode = 345 if country == "Serbia & Montenegro"
replace cowcode = 345 if country == "Serbia"
replace cowcode = 346 if country=="Bosnia and Herzegovina"
replace cowcode = 346 if country=="Bosnia-Herz"
replace cowcode = 349 if country == "Slovenia"
replace cowcode = 350 if country == "Greece"
replace cowcode = 352 if country == "Cyprus"
replace cowcode = 355 if country == "Bulgaria"
replace cowcode = 359 if country=="Moldova" 
replace cowcode = 360 if country == "Romania"
replace cowcode = 365 if country == "Russia"
replace cowcode = 365 if country == "Russian Federation"
replace cowcode = 365 if country=="USSR"
replace cowcode = 365 if country=="Soviet Union"
replace cowcode = 366 if country == "Estonia"
replace cowcode = 367 if country == "Latvia"
replace cowcode = 368 if country == "Lithuania"
replace cowcode = 369 if country == "Ukraine"
replace cowcode = 370 if country == "Belarus"
replace cowcode = 371 if country == "Armenia"
replace cowcode = 372 if country == "Georgia"
replace cowcode = 373 if country == "Azerbaijan"
replace cowcode = 375 if country == "Finland"
replace cowcode = 380 if country == "Sweden"
replace cowcode = 385 if country == "Norway"
replace cowcode = 390 if country == "Denmark"
replace cowcode = 395 if country == "Iceland"
replace cowcode = 402 if country == "Cape Verde"
replace cowcode = 402 if country == "C. Verde Is."
replace cowcode = 403 if country == "Sao Tome and Principe"
replace cowcode = 403 if country == "Sao Tome & Principe"
replace cowcode = 404 if country == "Guinea-Bissau"
replace cowcode = 411 if country == "Equatorial Guinea"
replace cowcode = 411 if country == "Eq. Guinea"
replace cowcode = 420 if country == "Gambia"
replace cowcode = 420 if country=="The Gambia" 
replace cowcode = 420 if country=="Gambia, The" 
replace cowcode = 432 if country == "Mali"
replace cowcode = 433 if country == "Senegal"
replace cowcode = 434 if country == "Benin"
replace cowcode = 435 if country == "Mauritania"
replace cowcode = 436 if country == "Niger"
replace cowcode = 437 if country == "Ivory Coast"
replace cowcode = 437 if country == "Cote d'Ivoire"
replace cowcode = 437 if country == "Cote d`Ivoire"
replace cowcode = 438 if country == "Guinea"
replace cowcode = 439 if country == "Burkina Faso"
replace cowcode = 450 if country == "Liberia"
replace cowcode = 451 if country == "Sierra Leone"
replace cowcode = 452 if country == "Ghana"
replace cowcode = 461 if country == "Togo"
replace cowcode = 471 if country == "Cameroon"
replace cowcode = 475 if country == "Nigeria"
replace cowcode = 481 if country == "Gabon"
replace cowcode = 482 if country == "Central African Republic"
replace cowcode = 482 if country == "Cent. Af. Rep."
replace cowcode = 483 if country == "Chad"
replace country = "Congo Republic" if country=="Congo Republic of"
replace cowcode = 484 if country == "Congo Republic"
replace cowcode = 484 if country == "Congo, Rep."
replace cowcode = 484 if country=="Congo"
replace cowcode= 484 if country=="Congo, Republic of"
replace cowcode = 490 if country == "Congo Democratic Republic"
replace cowcode = 490 if country=="Congo Democratic Republic of"
replace cowcode = 490 if country=="Congo, Dem. Rep."|country=="Congo, Democratic Republic Of"
replace cowcode = 490 if country == "Zaire (Democ Republic Congo)"
replace cowcode = 500 if country == "Uganda"
replace cowcode = 501 if country == "Kenya"
replace cowcode = 510 if country == "Tanzania"
replace cowcode = 516 if country == "Burundi"
replace cowcode = 517 if country == "Rwanda"
replace cowcode = 520 if country == "Somalia"
replace cowcode = 522 if country == "Djibouti"
replace cowcode = 530 if country == "Ethiopia"
replace cowcode = 531 if country == "Eritrea"
replace cowcode = 540 if country == "Angola"
replace cowcode = 541 if country == "Mozambique"
replace cowcode = 551 if country == "Zambia"
replace cowcode = 552 if country == "Zimbabwe"
replace cowcode = 553 if country == "Malawi"
replace cowcode = 560 if country == "South Africa"
replace cowcode = 560 if country == "S. Africa"
replace cowcode = 565 if country == "Namibia"
replace cowcode = 570 if country == "Lesotho"
replace cowcode = 571 if country == "Botswana"
replace cowcode = 572 if country == "Swaziland"
replace cowcode = 580 if country == "Madagascar"
replace cowcode = 581 if country == "Comoros"
replace cowcode = 581 if country == "Comoro Is."
replace cowcode = 590 if country == "Mauritius"
replace cowcode = 591 if country == "Seychelles"
replace cowcode = 600 if country == "Morocco"
replace cowcode = 615 if country == "Algeria"
replace cowcode = 616 if country == "Tunisia"
replace cowcode = 620 if country == "Libya"
replace cowcode = 625 if country == "Sudan"
replace cowcode = 630 if country == "Iran"
replace cowcode = 630 if country == "Iran, Islamic Rep."
replace cowcode = 630 if country == "Iran, Islamic Republic"
replace cowcode = 640 if country == "Turkey"
replace cowcode = 645 if country == "Iraq"
replace cowcode = 651 if country == "Egypt"
replace cowcode = 651 if country == "Egypt, Arab Rep."
replace cowcode = 651 if country == "Egypt, Arab Republic"
replace cowcode = 652 if country == "Syria"
replace cowcode = 652 if country == "Syrian Arab Republic"
replace cowcode = 652 if country == "Syrian Arab Rep."
replace cowcode = 660 if country == "Lebanon"
replace cowcode = 663 if country == "Jordan"
replace cowcode = 666 if country == "Israel"
replace cowcode = 670 if country == "Saudi Arabia"
replace cowcode = 690 if country == "Kuwait"
replace cowcode = 692 if country == "Bahrain"
replace cowcode = 694 if country == "Qatar"
replace cowcode = 696 if country == "United Arab Emirates"
replace cowcode = 696 if country == "UAE"
replace cowcode = 698 if country == "Oman"
replace cowcode = 700 if country == "Afghanistan"
replace cowcode = 701 if country == "Turkmenistan"
replace cowcode = 702 if country == "Tajikistan"
replace cowcode = 703 if country == "Kyrgyz Republic"
replace cowcode = 703 if country == "Kyrgyzstan"
replace cowcode = 704 if country == "Uzbekistan"
replace cowcode = 705 if country == "Kazakhstan"
replace cowcode = 710 if country == "China"
replace cowcode = 710 if country == "PRC"
replace cowcode = 712 if country == "Mongolia"
replace cowcode = 713 if country == "Taiwan"
replace cowcode = 731 if country=="Korea, Dem. Rep."
replace cowcode = 731 if country=="Korea, Democratic Republic"
replace cowcode = 731 if country=="Korea Democratic Republic"
replace cowcode = 731 if country=="Korea, North"
replace cowcode = 731 if country=="PRK"
replace cowcode = 732 if country=="Korea Republic of"
replace cowcode = 732 if country=="Korea, Republic of"
replace cowcode = 732 if country=="Korea, Rep."
replace cowcode = 732 if country=="Korea, South"
replace cowcode = 732 if country=="ROK"
replace cowcode = 740 if country == "Japan"
replace cowcode = 750 if country == "India"
replace cowcode = 760 if country == "Bhutan"
replace cowcode = 770 if country=="Pakistan"
replace cowcode = 771 if country == "Bangladesh"
replace cowcode = 775 if country == "Myanmar"
replace cowcode = 775 if country=="Burma" 
replace cowcode = 679 if country=="Yemen People's Republic" 
replace cowcode = 679 if country=="Yemen" 
replace cowcode = 679 if country=="Yemen, Rep." 
replace cowcode = 679 if country=="Yemen Republic" 
replace cowcode=680 if country== "Yemen (PDR)"
replace cowcode = 678 if country=="Yemen (AR)" 
replace cowcode = 780 if country == "Sri Lanka"
replace cowcode=781 if country=="Maldives"
replace cowcode = 790 if country == "Nepal"
replace cowcode = 800 if country == "Thailand"
replace cowcode = 811 if country == "Cambodia"
replace cowcode = 812 if country == "Laos"
replace cowcode = 812 if country == "Lao PDR"
replace cowcode = 818 if country == "Vietnam"
replace cowcode = 820 if country == "Malaysia"
replace cowcode = 830 if country == "Singapore"
replace cowcode = 835 if country == "Brunei"|country=="Brunei Darussalam" 
replace cowcode = 840 if country == "Philippines"
replace cowcode = 850 if country == "Indonesia"
replace cowcode=860 if country=="Timor-Leste"
replace cowcode=860 if country=="East Timor"
replace cowcode = 900 if country == "Australia"
replace cowcode = 910 if country == "Papua New Guinea"
replace cowcode = 910 if country == "P. N. Guinea"
replace cowcode = 920 if country == "New Zealand"
replace cowcode = 935 if country == "Vanuatu"
replace cowcode = 940 if country == "Solomon Islands"
replace cowcode = 940 if country == "Solomon Is."
replace cowcode = 946 if country=="Kiribati"
replace cowcode = 950 if country == "Fiji"
replace cowcode=955 if country=="Tonga"
replace cowcode=983 if country=="Marshall Islands"
replace cowcode=986  if country=="Palau"
replace cowcode = 987 if country=="Micronesia, Fed. Sts."
replace cowcode = 987 if country=="Micronesia"
replace cowcode = 987 if country=="Federated States of Micronesia"
replace cowcode=990 if country=="Samoa"
replace cowcode=990 if country=="W. Samoa"

replace cowcode=626 if country=="South Sudan"
replace cowcode=347 if country=="Kosovo"
replace cowcode=341 if country=="Montenegro"
replace cowcode = 679 if country=="Yemen (North Yemen)"
replace cowcode = 365 if country=="Russia (Soviet Union)"
replace cowcode = 811 if country == "Cambodia (Kampuchea)"
replace cowcode = 346 if country=="Bosnia-Herzegovina"
replace cowcode = 490 if country == "DR Congo (Zaire)"
replace cowcode = 552 if country == "Zimbabwe (Rhodesia)"
replace cowcode = 2 if country=="United States of America"
replace cowcode = 345 if country == "Serbia (Yugoslavia)"

replace cowcode = 255 if country=="Germany Federal Republic of"
replace cowcode = 316 if country == "Czechoslovakia"
replace cowcode= 816 if country=="Vietnam"
replace cowcode=947 if country=="Tuvalu"
replace cowcode=970 if country=="Nauru"
replace cowcode=626 if country=="South Sudan"

drop if cowcode==.
rename cowcode ccode
sort ccode year
by ccode: gen gdp_lag=gdp[_n-1]
drop countrycode
sort ccode year
save worldbank_gdp_2000-2015.dta, replace
clear

use countrymonthdata_corrected.dta, replace
merge m:1 ccode year using worldbank_gdp_2000-2015.dta
drop if year==2000
tab _merge
drop if _merge==2
browse if _merge!=3
drop _merge
corr gdp gdp_cap
corr gdp_cap gdp_lag

save countrymonthdata_corrected.dta, replace
clear








