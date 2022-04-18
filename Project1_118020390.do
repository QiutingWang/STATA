
***Calculation of Portfolio return***
*append the two index opening price data into one file
import delimited /Users/macbookpro/Desktop/IDX_Idxtrd1.csv
save "/Users/macbookpro/Desktop/index_price1.dta"
clear
import delimited /Users/macbookpro/Desktop/IDX_Idxtrd2.csv
save "/Users/macbookpro/Desktop/index_price2.dta"
clear
use "/Users/macbookpro/Desktop/index_price1.dta"
append using "/Users/macbookpro/Desktop/index_price2.dta"
rename ïindexcd indexcd
*match the market tpye to corresponding index code
gen marketype=0 
replace markettype=1 if indexcd==2
replace markettype=4 if indexcd==399107
replace markettype=16 if indexcd==399006
*data clear the missing value
drop if idxrd08==.
sort indexcd idxtrd01
*unify the unit
replace idxtrd08= idxtrd08/100
*calculate portfolio return[0,1]
bys indexcd:gen index_ret_0_1=(1+ idxtrd08)*(1+ idxtrd08[_n+`i'])-1
*claculate portfolioreturn[2,61]
forvalue i=1/61 {
bys indexcd: gen log_1_ret`i' = log(1+idxtrd08[_n+`i'])
}
egen sum = rowtotal(log_1_ret2 - log_1_ret61)
gen index_ret_2_61 = exp(sum)-1
drop log_1_ret1 - sum
*destring idxtrd01 for generateing new date variables in order to do the following merge
replace idxtrd01 = subinstr(idxtrd01,"-","",.)
rename idxtrd01 date
rename idxtrd08 index_return
save "/Users/macbookpro/Desktop/portfolio_return.dta"


***calculate firm return***
*import the firm return data
import delimited /Users/macbookpro/Desktop/TRD_Dalyr1.csv
save "/Users/macbookpro/Desktop/firm_return1.dta"
clear
import delimited /Users/macbookpro/Desktop/TRD_Dalyr2.csv
save "/Users/macbookpro/Desktop/firm_return2.dta"
clear
use "/Users/macbookpro/Desktop/firm_return1.dta"
append using "/Users/macbookpro/Desktop/firm_return2.dta"
save "/Users/macbookpro/Desktop/firm_return.dta"
*adjust variables
rename ïstkcd stkcd
rename trddt date
replace date = subinstr(date,"-","",.)
gen year=substr(date,1,4)
gen month=substr(date,5,2)
*change the unit
replace dretwd = dretwd/100
*calculate [0,1] firm retun
bys stkcd: gen firm_ret_0_1 = (1+dretwd)*(1+dretwd[_n+1])-1
*calculate [2,61] firm return
forvalue i=1/61 {
bys stkcd: gen log_1_ret`i' = log(1+dretwd[_n+`i'])
}
egen sum = rowtotal(log_1_ret2 - log_1_ret61)
gen firm_ret_2_61 = exp(sum)-1
drop log_1_ret1 - sum
save "/Users/macbookpro/Desktop/*firm_return.dta"


***Calculate CAR***
*merge the firm return with portfolio return
merge m:1 date markettype using "/Users/macbookpro/Desktop/portfolio_return.dta"
keep if _merge==3
drop _merge
*calculate the CAR[0,1]
gen car_0_1= firm_ret_0_1 - index_ret_0_1
*calculate the CAR[2,61]
gen car_2_61= firm_ret_2_61 - index_ret_2_61
*destring date for the following merge
destring date,replace
save "/Users/macbookpro/Desktop/CAR.dta"
clear


***Analyst forecast data***
*append two forecast files into one whole file
import delimited /Users/macbookpro/Desktop/AF_Forecast1.csv
save "/Users/macbookpro/Desktop/Fiq1.dta"
clear
import delimited /Users/macbookpro/Desktop/AF_Forecast2.csv
save "/Users/macbookpro/Desktop/Fiq2.dta"
use "/Users/macbookpro/Desktop/Fiq1.dta"
append using "/Users/macbookpro/Desktop/Fiq2.dta"
drop reportid
rename ïstkcd stkcd
*create the variables for following merge
gen year=substr(fenddt,1,4)
gen month=substr(fenddt,5,2)
destring year month,replace
*delect the duplicated observations
duplicates drop fenddt stkcd,force
*keep the latest record of each firm in each period
destring rptdt
bys stkcd fenddt:egen last=max(rptdt) 
keep if rptdt==last
drop last
save "/Users/macbookpro/Desktop/Fiq.dta"
clear

***Actual firm EPS data***
*append quarterly report data with annual report data
*clean quarterly report data,only keep report data on December each year
import delimited /Users/macbookpro/Desktop/Quarter_Actual.csv
drop stockname numquitrafinreport
rename ïstockcode stkcd
gen year=substr(accperiod,1,4)
gen month=substr(accperiod,6,2)
destring month,replace
drop if month==3
drop if month==6
drop if month==9
save "/Users/macbookpro/Desktop/Quarterly_Actual.dta'
clear
*clean annually report data,only keep December report data.
import delimited /Users/macbookpro/Desktop/Annual_Actual.csv
rename ïstockcode stkcd
drop stockname
gen year=substr(accperiod,1,4)
gen month=substr(accperiod,6,2)
destring month, replace
drop if month==6
drop if month==9
drop if month==3
save "/Users/macbookpro/Desktop/Annual_Actual.dta"
*append annual data and quarter data into one dta file
append using "/Users/macbookpro/Desktop/Quarterly_Actual.dta"
destring year,replace
dupliactes drop stkcd year,force
save "/Users/macbookpro/Desktop/Actual_eiq.dta"
*merge forecast EPS data and actual data
merge 1:1 year stkcd using "/Users/macbookpro/Desktop/Fiq.dta"
keep if _merge==3
drop _merge
*drop the observations if the forecasting dates are later than actual values have been already released.
replace accperiod =subinstr(accperiod,"
> -","",.)
destring publidate rptdt,replace
drop if rptdt>= publidate
*drop if the observations with negative or too small EPS
drop if eps==.
drop if eps<=0
drop if feps==.
drop if feps<=0
save "/Users/macbookpro/Desktop/Actual_eiq&Fiq.dta"
clear

***clean and merge price data***
import delimited /Users/macbookpro/Desktop/TRD_Year.csv
rename ïstkcd stkcd
rename yclsprc prc
gen month=substr(clsdt,1,2)
destring month,replace
keep if month==12
rename trdynt year
save "/Users/macbookpro/Desktop/Price.dta"
merge 1:1 year month stkcd using "/Users/macbookpro/Desktop/Actual_eiq&Fiq.dta"
keep if _merge==3
drop _merge
*drop the observations with negative or too small price
drop if prc<1
*calculate the forecast error
gen FE=(eps-feps)/prc
*calculate number of earning announcement on a given day
bys rptdt:egen no_ea=count(eps)
sum no_ea
save "/Users/macbookpro/Desktop/FE.dta"
clear

***Prepare the table2***
*merge CAR data with FE data
use "/Users/macbookpro/Desktop/FE.dta"
rename publidate date
save "/Users/macbookpro/Desktop/FE.dta", replace
clear
*data clear for the duplicated data in CAR file
use "/Users/macbookpro/Desktop/CAR.dta"
duplicates drop stkcd date,force
save "/Users/macbookpro/Desktop/CAR.dta", replace
clear
use "/Users/macbookpro/Desktop/FE.dta"
merge 1:1 date stkcd using "/Users/macbookpro/Desktop/CAR.dta"
keep if _merge==3
drop _merge
save "/Users/macbookpro/Desktop/FE_CAR.dta"

***calculate NRANK***
*delet the duplicated data
use "FE_CAR.dta",clear
keep date no_ea
duplicates drop date no_ea,force
*create the 10-quantile of variable no_ea
xtile NRANK = no_ea, nq(10)
drop no_ea
save "/Users/macbookpro/Desktop/FE_NRANK.dta"
*Use FE_CAR.dta merge the FE_NRANK back to FE_CAR
use "/Users/macbookpro/Desktop/FE_CAR.dta"
duplicates drop date,force
merge m:1 date using "/Users/macbookpro/Desktop/FE_NRANK.dta"
drop _merge
*create the 10-quantiles of variable FE
xtile FE10=FE, nq(10)
*get the tables---table1
tab NRANK FE10,sum(car_0_1) mean
tab NRANK FE10,sum(car_2_61) mean
*get the tables---table2
gen test=1 if NRANK==1 & FE10==1
replace test=0 if NRANK==1 & FE10==10
ttest car_0_1, by(test)
*get figure 1 and figure 2
bys FE10: egen car_0_1_mean_1=mean(car_0_1) if NRANK==1
bys FE10: egen car_2_61_mean_1=mean(car_2_61) if NRANK==1
bys FE10: egen car_0_1_mean_10=mean(car_0_1) if NRANK==10
bys FE10: egen car_2_61_mean_10=mean(car_2_61) if NRANK==10
twoway (line car_0_1_mean_1 FE10) (line car_0_1_mean_10 FE10)
twoway (line car_2_61_mean_1 FE10) (line car_2_61_mean_10 FE10)
 




