***Calculate KS and MAD***
*Different industries, calculate their MAD, KS and aggregate FSD Scores indivudially; The code for each industry is the same*
*Data Clean*
*For balance sheet data*
import delimited /Users/macbookpro/Desktop/FS_Combas.csv
gen date=date(accper,"YMD")
gen month=month(date)
keep if month==12
drop if typrep=="B"
save "/Users/macbookpro/Desktop/BalanceSheet.dta"
clear
*For Cash flow statement data*
import delimited /Users/macbookpro/Desktop/FS_Comscfd.csv
gen date=date(accper,"YMD")
gen month=month(date)
keep if month==12
drop if typrep=="B"
save "Cashflowstatement.dta"
clear
*For Income statement data*
import delimited /Users/macbookpro/Desktop/FS_Comins.csv
gen date=date(accper,"YMD")
gen month=month(date)
keep if month==12
drop if typrep=="B"
save "/Users/macbookpro/Desktop/IncomeStatement.dta"
*merge the three datasets*
merge 1:1 ïstkcd accper using "/Users/macbookpro/Desktop/BalanceSheet.dta"
keep if _merge==3
drop _merge
merge 1:1 ïstkcd accper using "/Users/macbookpro/Desktop/CashFlowStatment.dta"
keep if _merge==3
drop _merge
drop date month
drop if a001101000<=0

*Generate the first digit of all the numbers and take the absolute value of negative numbers*
foreach var of varlist a001101000 - c006000000 {
replace `var'=abs(`var')
replace `var'=`var'*100 if `var'<=1 &`var'>0
tostring `var', replace force
gen `var'_1digit=substr(`var',1,1)
}
*Use loop to convert the `var'_1digit to numeric type*
foreach var of varlist a001101000_1digit - c006000000_1digit {
destring `var', replace
}
*Generate a dummy matrix to proxy if a number is 1,2,3,4...*
forvalues i=0/9 {
foreach var of varlist a001101000_1digit - c006000000_1digit {
gen `var'dummy`i'=1 if `var'==`i'
}
}
*Generate a dummy matrix to indicate the value missing*
foreach var of varlist a001101000_1digit - c006000000_1digit {
gen `var'dummy_missing=1 if `var'==.
}
**Calculate the frequency**
*Calculate the frequency of each leading digit*
forvalues i=0/9 {
egen freq_`i'=rowtotal( a001101000_1digitdummy`i'- c006000000_1digitdummy`i')
}
*Calculate the frequency of missing*
egen freq_missing=rowtotal( a001101000_1digitdummy_missing- c006000000_1digitdummy_missing)
**Calculate the empirical distribution**
*Drop the observations which have more than half of variables are missing.*
drop if freq_missing > 118
forvalues i=1/9 {
gen prop_`i'= freq_`i'/(236- freq_0-freq_missing)
}
drop a001101000_1digit-c006000000_1digitdummy_missing
*Generate the Benford's theoretical distribution*
forvalues i=1/9{
gen benford`i'=log10(`i'+1)-log10(`i')
}

**Calculate KS Statistics**
*Calculate the actual cumulative distribution for i:prop_cum_`i'=AD1+AD2+...+ADi*
forvalue i=1/9{
egen prop_cum_`i'=rowtotal(prop_1-prop_`i')
}
*Calculate the expected cumulative distribution for i:benford_cum_`i'=ED1+ED2+...+EDi*
forvalues i=1/9 {
egen benford_cum_`i'=rowtotal( benford1-benford`i')
}
*Calculate the absoluate value of deviation of the actual cumulative distribution from the expected culumlative distribution*
forvalues i=1/9 {
gen ks_deviation`i'=abs( prop_cum_`i'- benford_cum_`i')
}
*Find the maximum of the ks_devation`i', and get the KS sta*
egen KS=rowmax( ks_deviation1- ks_deviation9)
*calculate the test statistics*
gen ks_test=1.36/(sqrt(236-freq_0-freq_missing))
*generate the dummy comparing KS statistic the test value*
gen follow= KS< ks_test

**Calculate the MAD**
forvalues i=1/9 {
gen mad_deviation`i'=abs( prop_`i'- benford`i')
}
egen MAD=rowtotal(mad_deviation1- mad_deviation9)
replace MAD=MAD/9
save "/Users/macbookpro/Desktop/KS&MAD.dta"

***Calculate the FSD Score, Prepare Table2***
*The FSD Score for industryA*
sum( freq_1), detail
gen total1=sum( freq_1)
drop total1
forvalues i=1/9 {
gen total`i'=sum( freq_`i')
}
egen total=rowtotal( total1- total9)
forvalues i=1/9{
gen total_pro`i'= total`i'/ total
}
drop total1- total
forvalues i=1/9 {
gen totalmad_deviation`i'=abs( total_pro`i'- benford`i')
}
egen totalMAD=rowtotal(totalmad_deviation1- totalmad_deviation9)
replace totalMAD =totalMAD/9
save "/Users/macbookpro/Desktop/A.dta"
*Notes:The FSD Score for other industry B-M are calculated similarly, then append the different industries' firm-years*
use A
append using B C D E F G H J K L M
save "/Users/macbookpro/Desktop/Total.dta"
gen year=substr(accper,1,4)
destring year,replace
*Calculate firm-years confirming by years*
egen sum=sum(follow),by (year)
tab follow
*Calculate aggregate FSD_Score by years*
use "/Users/macbookpro/Desktop/Total.dta"
forvalues i=1/9{
egen total`i'=sum( freq_`i'),by (year)
}
egen total=rowtotal( total1-total9)
forvalues i=1/9{
gen total _pro`i'=total`i'/total
}
drop total1-total
forvalues i=1/9{
gen totalmad_deviation`i'=abs( total_pro`i'- benford`i')
}
egen totalMAD=rowtotal(totalmad_deviation1- totalmad_deviation9)
replace totalMAD =totalMAD/9
tabstat totalMAD,by (year) stats(mean)


***Prepare Table3***
**Calculate KS Statistic for Balance Sheet**
use "/Users/macbookpro/Desktop/BalanceSheet.dta"
*generate the first digit of all the numbers*
foreach var of varlist a001101000 - a004000000 {
replace `var'=abs(`var')
replace `var'=`var'*100 if `var'<=1 &`var'>0
tostring `var', replace force
gen `var'_1digit=substr(`var',1,1)
}
*Convert the variables to numercial numbers*
foreach var of varlist a001101000_1digit - a004000000_1digit {
destring `var', replace
}
*Generate a dummy matrix to proxy if a number is 1,2,3,4...*
forvalues i=0/9 {
foreach var of varlist a001101000_1digit - a004000000_1digit {
gen `var'dummy`i'=1 if `var'==`i'
}
}
* count number of missing variables*
foreach var of varlist a001101000_1digit - a004000000_1digit {
gen `var'dummy_missing=1 if `var'==.
}
*generate the frequency of each leading digit & missing*
forvalues i=0/9 {
egen freq_`i'=rowtotal( a001101000_1digitdummy`i'- a004000000_1digitdummy`i')
}
*Calculate the frequency of missing*
egen freq_missing=rowtotal( a001101000_1digitdummy_missing- a004000000_1digitdummy_missing)

**Calculate the empirical distribution**
*Drop the observations which have more than half of variables are missing.*
drop if freq_missing > 63
forvalues i=1/9 {
gen prop_`i'= freq_`i'/(126- freq_0-freq_missing)
}
drop a001101000_1digit-a004000000_1digitdummy_missing
*Use loop to calculate the Benford’s distribution.*
forvalues i=1/9{
gen benford`i'=log10(`i'+1)-log10(`i')
}
**Calculate KS**
*Calculate the actual cumulative distribution for i:prop_cum_`i'=AD1+AD2+...+ADi*
forvalue i=1/9{
egen prop_cum_`i'=rowtotal(prop_1-prop_`i')
}
*Calculate the expected cumulative distribution for i:benford_cum_`i'=ED1+ED2+...+EDi*
forvalues i=1/9 {
egen benford_cum_`i'=rowtotal( benford1-benford`i')
}
*Calculate the absoluate value of deviation of the actual cumulative distribution from the expected culumlative distribution*
forvalues i=1/9 {
gen ks_deviation`i'=abs( prop_cum_`i'- benford_cum_`i')
}
*Find the maximum of the ks_devation`i', and get the KS sta*
egen KS=rowmax( ks_deviation1- ks_deviation9)
gen ks_test=1.36/(sqrt(126-freq_0-freq_missing))
gen follow= KS< ks_test
tab follow
gen year=year(date)

**Calculate KS Statistics for Income Statement; The logic is simliar**
use "/Users/macbookpro/Desktop/IncomeStatement.dta"
foreach var of varlist b001100000 - b006000102 {
replace `var'=abs(`var')
replace `var'=`var'*100 if `var'<=1 &`var'>0
tostring `var', replace force
gen `var'_1digit=substr(`var',1,1)
}
foreach var of varlist b001100000_1digit - b006000102_1digit {
destring `var', replace
}
forvalues i=0/9 {
foreach var of varlist b001100000_1digit - b006000102_1digit {
gen `var'dummy`i'=1 if `var'==`i'
}
}
foreach var of varlist b001100000_1digit - b006000102_1digit {
gen `var'dummy_missing=1 if `var'==.
}
forvalues i=0/9 {
egen freq_`i'=rowtotal( b001100000_1digitdummy`i'- b006000102_1digitdummy`i')
}
egen freq_missing=rowtotal( b001100000_1digitdummy_missing- b006000102_1digitdummy_missing)
drop if freq_missing>30
forvalues i=1/9 {
gen prop_`i'= freq_`i'/(60- freq_0-freq_missing)
}
forvalues i=1/9{
gen benford`i'=log10(`i'+1)-log10(`i')
}
drop b001100000_1digit- b006000102_1digitdummy_missing
forvalue i=1/9{
egen prop_cum_`i'=rowtotal(prop_1-prop_`i')
}
forvalues i=1/9{
egen benford_cum_`i'=rowtotal( benford1-benford`i')
}
forvalues i=1/9{
gen ks_deviation`i'=abs( prop_cum_`i'- benford_cum_`i')
}
egen KS=rowmax( ks_deviation1- ks_deviation9)
gen ks_test=1.36/(sqrt(60-freq_0-freq_missing))
gen follow= KS< ks_test
tab follow
gen year=year(date)

**Calculate KS Statistic for Cash Flow Statement; The logic is similar**
use "/Users/macbookpro/Desktop/CashFlowStatment.dta"
foreach var of varlist c001001000- c006000000 {
replace `var'=abs(`var')
replace `var'=`var'*100 if `var'<=1 &`var'>0
tostring `var', replace force
gen `var'_1digit=substr(`var',1,1)
}
foreach var of varlist c001001000_1digit - c006000000_1digit{
destring `var', replace
}
forvalues i=0/9{
foreach var of varlist c001001000_1digit - c006000000_1digit{
gen `var'dummy`i'=1 if `var'==`i'
}
}
foreach var of varlist c001001000_1digit - c006000000_1digit{
gen `var'dummy_missing=1 if `var'==.
}
forvalues i=0/9{
egen freq_`i'=rowtotal( c001001000_1digitdummy`i'- c006000000_1digitdummy`i')
}
egen freq_missing=rowtotal( c001001000_1digitdummy_missing- c006000000_1digitdummy_missing)
drop if freq_missing>25
forvalues i=1/9{
gen prop_`i'= freq_`i'/(50- freq_0-freq_missing)
}
forvalues i=1/9{
gen benford`i'=log10(`i'+1)-log10(`i')
}
drop c001001000_1digit- c006000000_1digitdummy_missing
forvalue i=1/9 {
egen prop_cum_`i'=rowtotal(prop_1-prop_`i')
}
forvalues i=1/9 {
egen benford_cum_`i'=rowtotal( benford1-benford`i')
}
forvalues i=1/9 {
gen ks_deviation`i'=abs( prop_cum_`i'- benford_cum_`i')
}
egen KS=rowmax( ks_deviation1- ks_deviation9)
gen ks_test=1.36/(sqrt(50-freq_0-freq_missing))
gen follow= KS< ks_test
tab follow
gen year=year(date)
**Get table3 PanelC data; Notes:the code is the same with each industry**
tab follow
**Get table3 PanelD data**
tabstat follow,by(year)

***Prepare Table 4***
**Calculate Aggregate FSD_Score for Balance Sheet**
use "/Users/macbookpro/Desktop/BalanceSheet_KS&MAD.dta"
*calculate the total frequency for the numbers*
sum( freq_1), detail
gen total1=sum( freq_1)
drop total1
forvalues i=1/9 {
gen total`i'=sum( freq_`i')
}
*calculate the pdf for each number*
egen total=rowtotal( total1- total9)
forvalues i=1/9{
gen total_pro`i'= total`i'/ total
}
drop total1- total
*calculate MAD Statistics*
forvalues i=1/9 {
gen totalmad_deviation`i'=abs( total_pro`i'- benford`i')
}
egen totalMAD=rowtotal(totalmad_deviation1- totalmad_deviation9)
replace totalMAD=totalMAD/9
sum totalMAD

**Calculate Aggregate FSD_Score for Income Statement**
use "/Users/macbookpro/Desktop/IncomeStatement_KS&MAD.dta"
*calculate the total frequency for the numbers*
sum(freq_1), detail
gen total1=sum(freq_1)
drop total1
forvalues i=1/9 {
gen total`i'=sum(freq_`i')
}
*calculate the pdf for each number*
egen total=rowtotal( total1- total9)
forvalues i=1/9{
gen total_pro`i'= total`i'/ total
}
drop total1- total
*calculate MAD Statistics*
forvalues i=1/9 {
gen totalmad_deviation`i'=abs( total_pro`i'- benford`i')
}
egen totalMAD=rowtotal(totalmad_deviation1- totalmad_deviation9)
replace totalMAD=totalMAD/9
sum totalMAD

**Calculate Aggregate FSD_Score for Cash Flow Statement**
sum(freq_1), detail
gen total1=sum(freq_1)
drop total1
forvalues i=1/9 {
gen total`i'=sum(freq_`i')
}
*calculate the pdf for each number*
egen total=rowtotal(total1- total9)
forvalues i=1/9{
gen total_pro`i'= total`i'/ total
}
drop total1- total
*calculate MAD Statistics*
forvalues i=1/9 {
gen totalmad_deviation`i'=abs( total_pro`i'- benford`i')
}
egen totalMAD=rowtotal(totalmad_deviation1- totalmad_deviation9)
replace totalMAD=totalMAD/9
sum totalMAD

**calculate aggregate FSD Score by financial statment subcategory**
*Asset*
use "/Users/macbookpro/Desktop/BalanceSheet.dta"
*Date cleaning*
keep ïstkcd accper typrep a001101000-a001000000
*Generate the first digit*
foreach var of varlist a001101000-a001000000{
replace `var'=abs(`var')
replace `var'=`var'*100 if `var'<=1 & `var'>0
tostring `var',replace force
gen `var'_1digit=substr(`var',1,1)
}
*Convert to numeric type*
foreach var of varlist a001101000_1digit - a001000000_1digit {
destring `var', replace
}
*Generate dummy matrix*
forvalues i=0/9 {
foreach var of varlist a001101000_1digit - a001000000_1digit {
gen `var'dummy`i'=1 if `var'==`i'
}
}
*Indicate the missing value*
foreach var of varlist a001101000_1digit - a001000000_1digit {
gen `var'dummy_missing=1 if `var'==.
}
*Calculate the frequency of each leading digit*
forvalues i=0/9 {
egen freq_`i'=rowtotal( a001101000_1digitdummy`i'- a001000000_1digitdummy`i')
}
egen freq_missing=rowtotal( a001101000_1digitdummy_missing- a001000000_1digitdummy_missing)
drop if freq_missing > 29
forvalues i=1/9 {
gen prop_`i'= freq_`i'/(59- freq_0-freq_missing)
}
drop a001101000_1digit-a001000000_1digitdummy_missing
forvalues i=1/9{
gen benford`i'=log10(`i'+1)-log10(`i')
}
forvalues i=1/9 {
gen mad_deviation`i'=abs( prop_`i'- benford`i')
}
egen MAD=rowtotal(mad_deviation1- mad_deviation9)
replace MAD=MAD/9
sum( freq_1), detail
gen total1=sum( freq_1)
drop total1
forvalues i=1/9 {
gen total`i'=sum( freq_`i')
}
egen total=rowtotal( total1- total9)
forvalues i=1/9{
gen total_pro`i'= total`i'/ total
}
drop total1- total
forvalues i=1/9 {
gen totalmad_deviation`i'=abs( total_pro`i'- benford`i')
}
egen totalMAD=rowtotal(totalmad_deviation1- totalmad_deviation9)
replace totalMAD=totalMAD/9
sum totalMAD
save "/Users/macbookpro/Desktop/Asset_MAD.dta"

*Liability*
use "/Users/macbookpro/Desktop/BalanceSheet.dta"
keep ïstkcd accper typrep a002101000-a002000000
foreach var of varlist a002101000-a002000000{
replace `var'=abs(`var')
replace `var'=`var'*100 if `var'<=1 & `var'>0
tostring `var',replace force
gen `var'_1digit=substr(`var',1,1)
}
foreach var of varlist a002101000_1digit - a002000000_1digit {
destring `var', replace
}
forvalues i=0/9{
foreach var of varlist a002101000_1digit - a002000000_1digit {
gen `var'dummy`i'=1 if `var'==`i'
}
}
foreach var of varlist a002101000_1digit - a002000000_1digit {
gen `var'dummy_missing=1 if `var'==.
}
forvalues i=0/9 {
egen freq_`i'=rowtotal( a002101000_1digitdummy`i'- a002000000_1digitdummy`i')
}
egen freq_missing=rowtotal( a002101000_1digitdummy_missing- a002000000_1digitdummy_missing)
drop if freq_missing > 24
forvalues i=1/9 {
gen prop_`i'= freq_`i'/(48- freq_0-freq_missing)
}
drop a002101000_1digit-a002000000_1digitdummy_missing
forvalues i=1/9{
gen benford`i'=log10(`i'+1)-log10(`i')
}
forvalues i=1/9 {
gen mad_deviation`i'=abs( prop_`i'- benford`i')
}
egen MAD=rowtotal(mad_deviation1- mad_deviation9)
replace MAD=MAD/9
sum( freq_1), detail
gen total1=sum( freq_1)
drop total1
forvalues i=1/9 {
gen total`i'=sum( freq_`i')
}
egen total=rowtotal( total1- total9)
forvalues i=1/9{
gen total_pro`i'= total`i'/ total
}
drop total1- total
forvalues i=1/9 {
gen totalmad_deviation`i'=abs( total_pro`i'- benford`i')
}
egen totalMAD=rowtotal(totalmad_deviation1- totalmad_deviation9)
replace totalMAD=totalMAD/9
save "/Users/macbookpro/Desktop/Liability_MAD.dta"

*Equity*
use "/Users/macbookpro/Desktop/BalanceSheet.dta"
keep ïstkcd accper typrep a003101000-a004000000
foreach var of varlist a003101000-a004000000{
replace `var'=abs(`var')
replace `var'=`var'*100 if `var'<=1 & `var'>0
tostring `var',replace force
gen `var'_1digit=substr(`var',1,1)
}
foreach var of varlist a003101000_1digit - a004000000_1digit {
destring `var', replace
}
forvalues i=0/9{
foreach var of varlist a003101000_1digit - a004000000_1digit {
gen `var'dummy`i'=1 if `var'==`i'
}
}
foreach var of varlist a003101000_1digit - a004000000_1digit {
gen `var'dummy_missing=1 if `var'==.
}
forvalues i=0/9 {
egen freq_`i'=rowtotal( a003101000_1digitdummy`i'- a004000000_1digitdummy`i')
}
egen freq_missing=rowtotal( a003101000_1digitdummy_missing- a004000000_1digitdummy_missing)
drop if freq_missing > 9
forvalues i=1/9 {
gen prop_`i'= freq_`i'/(18- freq_0-freq_missing)
}
drop a003101000_1digit-a004000000_1digitdummy_missing
forvalues i=1/9{
gen benford`i'=log10(`i'+1)-log10(`i')
}
forvalues i=1/9 {
gen mad_deviation`i'=abs( prop_`i'- benford`i')
}
egen MAD=rowtotal(mad_deviation1- mad_deviation9)
replace MAD=MAD/9
sum( freq_1), detail
gen total1=sum( freq_1)
drop total1
forvalues i=1/9 {
gen total`i'=sum( freq_`i')
}
egen total=rowtotal( total1- total9)
forvalues i=1/9{
gen total_pro`i'= total`i'/ total
}
drop total1- total
forvalues i=1/9 {
gen totalmad_deviation`i'=abs( total_pro`i'- benford`i')
}
egen totalMAD=rowtotal(totalmad_deviation1- totalmad_deviation9)
replace totalMAD=totalMAD/9
sum totalMAD
save "/Users/macbookpro/Desktop/Equity_MAD.dta"

*Expense*
use "/Users/macbookpro/Desktop/IncomeStatement.dta"
keep ïstkcd accper typrep b0i1103203 b0i1103303 b001200000 b001201000 b0i1202000 b0i1203000 b0i1203101 b0i1203203 b0i1204000 b0i1204101 b0i1204203 b0i1205000 b0i1206000 b001207000 b0f1208000 b0i1208103 b001209000 b001210000 b001211000 b001212000 b0f1213000 b001500000 b001500201 b002100000 b002200000 b002000201
foreach var of varlist  b0i1103203 -  b002000201{
replace `var'=abs(`var')
replace `var'=`var'*100 if `var'<=1 & `var'>0
tostring `var',replace force
gen `var'_1digit=substr(`var',1,1)
}
foreach var of varlist b0i1103203_1digit - b002000201_1digit{
destring `var', replace
}
forvalues i=0/9 {
foreach var of varlist b0i1103203_1digit - b002000201_1digit {
gen `var'dummy`i'=1 if `var'==`i'
}
}
foreach var of varlist b0i1103203_1digit - b002000201_1digit{
gen `var'dummy_missing=1 if `var'==.
}
forvalues i=0/9 {
egen freq_`i'=rowtotal( b0i1103203_1digitdummy`i'- b002000201_1digitdummy`i')
}
egen freq_missing=rowtotal(b0i1103203_1digitdummy_missing- b002000201_1digitdummy_missing)
drop if freq_missing > 13
forvalues i=1/9 {
gen prop_`i'= freq_`i'/(26- freq_0-freq_missing)
}
drop b0i1103203_1digit-b002000201_1digitdummy_missing
forvalues i=1/9{
gen benford`i'=log10(`i'+1)-log10(`i')
}
forvalues i=1/9 {
gen mad_deviation`i'=abs( prop_`i'- benford`i')
}
egen MAD=rowtotal(mad_deviation1- mad_deviation9)
replace MAD=MAD/9
sum( freq_1), detail
gen total1=sum( freq_1)
drop total1
forvalues i=1/9{
gen total`i'=sum( freq_`i')
}
egen total=rowtotal( total1- total9)
forvalues i=1/9{
gen total_pro`i'= total`i'/ total
}
drop total1- total
forvalues i=1/9{
gen totalmad_deviation`i'=abs( total_pro`i'- benford`i')
}
egen totalMAD=rowtotal(totalmad_deviation1- totalmad_deviation9)
replace totalMAD=totalMAD/9
sum totalMAD
save "/Users/macbookpro/Desktop/Expense_MAD.dta"

*Income*
use "/Users/macbookpro/Desktop/IncomeStatement.dta"
keep ïstkcd accper typrep b001100000-bbd1102101 b0i1103000-b0i1103111 b0d1104000-b0d1104401 b001301000-b001400101
foreach var of varlist  b001100000 -  b001400101{
replace `var'=abs(`var')
replace `var'=`var'*100 if `var'<=1 & `var'>0
tostring `var',replace force
gen `var'_1digit=substr(`var',1,1)
}
foreach var of varlist b001100000_1digit - b001400101_1digit {
destring `var', replace
}
forvalues i=0/9 {
foreach var of varlist b001100000_1digit - b001400101_1digit {
gen `var'dummy`i'=1 if `var'==`i'
}
}
foreach var of varlist b001100000_1digit - b001400101_1digit {
gen `var'dummy_missing=1 if `var'==.
}
forvalues i=0/9 {
egen freq_`i'=rowtotal( b001100000_1digitdummy`i'- b001400101_1digitdummy`i')
}
egen freq_missing=rowtotal( b001100000_1digitdummy_missing- b001400101_1digitdummy_missing)
drop if freq_missing > 9
forvalues i=1/9 {
gen prop_`i'= freq_`i'/(19- freq_0-freq_missing)
}
drop b001100000_1digit-b001400101_1digitdummy_missing
forvalues i=1/9{
gen benford`i'=log10(`i'+1)-log10(`i')
}
forvalues i=1/9 {
gen mad_deviation`i'=abs( prop_`i'- benford`i')
}
egen MAD=rowtotal(mad_deviation1- mad_deviation9)
replace MAD=MAD/9
sum( freq_1), detail
gen total1=sum( freq_1)
drop total1
forvalues i=1/9 {
gen total`i'=sum( freq_`i')
}
egen total=rowtotal( total1- total9)
forvalues i=1/9{
gen total_pro`i'= total`i'/ total
}
drop total1- total
forvalues i=1/9 {
gen totalmad_deviation`i'=abs( total_pro`i'- benford`i')
}
egen totalMAD=rowtotal(totalmad_deviation1- totalmad_deviation9)
replace totalMAD=totalMAD/9
sum totalMAD
save "/Users/macbookpro/Desktop/Income_MAD.dta"
clear

***Table5***
*Sales Growth*
use "/Users/macbookpro/Desktop/IncomeStatement.dta"
gen year=year( date)
tsset ïstkcd year
rename b001100000 revenue
gen growth= ( rev-l.rev)/l.rev
drop if growth==.
drop typrep revenue b001000000 b002100000 date year
save "/Users/macbookpro/Desktop/Sales_Growth.dta"
*For table1*
sum growth,detail

*RET_VOL*
*Get data from CSMAR *
import delimited /Users/macbookpro/Desktop/TRD_Mnth.csv
rename trdmnt date
gen year = substr(date,1,4)
gen month=substr(date,6,2)
egen sd=sd(mretwd), by(year stkcd)
rename sd RET_VOL
duplicates drop ïstkcd year, force
drop mretwd date month
save RET_VOL
*For table1*
sum RET_VOL,detail

*AGE*
*gen the year the company is listed*
import delimited /Users/macbookpro/Desktop/QX_IPO.csv
gen date=date(companylisteddate,"YMD")
gen listedyear=year(date)
save "/Users/macbookpro/Desktop/AGE.dta"
use "/Users/macbookpro/Desktop/Total.dta"
merge m:m stkcd using "/Users/macbookpro/Desktop/Total.dta"
keep if _merge==3
drop _merge
gen AGE=year-listedyear
save "/Users/macbookpro/Desktop/AGE'.dta"
*For table1*
sum AGE, detail

**CH_CS**
**Data clean and variables generate**
*the revenue data*
import delimited /Users/macbookpro/Desktop/Revenue.csv
gen date=date(accper,"YMD")
gen month=month(date)
keep if month==12
drop date month
gen date=date(accper,"YMD")
gen year=year(date)
save "/Users/macbookpro/Desktop/Revenue.dta"
*the receivable data*
import delimited /Users/macbookpro/Desktop/Receivable.csv
gen date=date(accper,"YMD")
gen month=month(date)
keep if month==12
drop if typrep=="B"
gen year=year(date)
drop month date year
save "/Users/macbookpro/Desktop/Receivables.dta"
*merge Receviables&Revenue and calculate CH_CS*
use "/Users/macbookpro/Desktop/Revenue.dta"
merge m:m ïstkcd using "/Users/macbookpro/Desktop/Receivables.dta"
keep if _merge==3
drop _merge
gen CS= b001100000- a001111000
gen CH_CS= (CS[_n]- CS[_n-1])/CS[_n-1]
save "/Users/macbookpro/Desktop/CH_CS.dta"
*For table1*
sum CH_CS,detail

***ISSUE***
**Long term debt issuance----DLTIS>0**
*Data clean and generate new variables*
import delimited /Users/macbookpro/Desktop/Longtermdebt.csv
gen date=date(fn04003, "YMD")
gen year=year(date)
duplicates drop year ïstkcd, force
gen DLTIS=1
rename ïstkcd stkcd
save "/Users/macbookpro/Desktop/DLTIS.dta"
*generate new variables for total, to do the following merge*
using "C:\Users\asus\Desktop\total.dta" 
gen date=date(accper, "YMD")
*Merge the datasets*
merge 1:1 year stkcd using "/Users/macbookpro/Desktop/DLTIS.dta"
drop if _merge==2
drop _merge
replace DLTIS=0 if DLTIS==.
save "/Users/macbookpro/Desktop/DLTIS_final.dta"

**Sale of common or preferred stock----SSTK>0**
import delimited /Users/macbookpro/Desktop/RS_Aibasic.csv 
*Data clean and generate new variables*
gen date=date(ailtadt, "YMD")
gen year=year(date)
duplicates drop year ïstkcd, force
gen SSTK=1
*generate new variables for total, to do the following merge*
use "/Users/macbookpro/Desktop/Total.dta"
gen date=date(accper, "YMD")
gen year=year(date)
*Merge the dataset*
merge 1:1 ïstkcd year using "/Users/macbookpro/Desktop/SSTK.dta"
drop if _merge==2
drop _merge
replace SSTK=0 if SSTK==.
save "/Users/macbookpro/Desktop/SSTK'.dta"

**Merge the DLTIS & SSTK data---Get ISSUE data**
use "/Users/macbookpro/Desktop/DLTIS_final.dta"
merge 1:1 ïstkcd year using "/Users/macbookpro/Desktop/SSTK'.dta"
keep if _merge==3
drop _merge
gen ISSUE=1 if DLTIS==1|SSTK==1
replace ISSUE=0 if ISSUE==.
save "/Users/macbookpro/Desktop/ISSUE.dta"
*For table1*
sum ISSUE,detail

***PE Ratio***
*Closing price*
import delimited /Users/macbookpro/Desktop/TRD_Year.csv 
rename yclsprc closingprice
rename trdynt year
save "/Users/macbookpro/Desktop/Closingprice.dta"
*Numbers of shares outstanding*
import delimited /Users/macbookpro/Desktop/HLD_Capstru.csv
gen date=date(reptdt,"YMD")
gen year=year(date)
sort year
tsset ÿþstkcd year
gen last_num=l.nshrn
gen average_num=(nshrn+last_num)/2
save "/Users/macbookpro/Desktop/Number_of_share.dta"
*Earning data*
import delimited /Users/macbookpro/Desktop/FS_Comins.csv 
rename b002000000 net_income
*Data clean and generate new variables*
gen date= date(accper,"YMD")
gen month=month(date)
keep if month==12
gen year=year(date)
drop if typrep=="B"
*Data Merging*
merge 1:1 ïstkcd year using "/Users/macbookpro/Desktop/Number_of_share.dta"
keep if _merge==3
drop _merge
gen EPS= net_income/average_num
merge 1:1 ïstkcd year using "/Users/macbookpro/Desktop/Closingprice.dta"
keep if _merge==3
drop _merge
*Calculate PE ratio*
gen PE= closingprice/EPS
save "/Users/macbookpro/Desktop/PE.dta"
*For table1*
sum PE,detail

**CH_ROA & ROA**
*Last year's Asset*
import delimited /Users/macbookpro/Desktop/FS_Combas.csv
drop if typrep=="B"
gen date= date(accper,"YMD")
gen month=month(date)
keep if month==12
gen year=year(date)
sort year
tsset ïstkcd year
rename a001000000 AT
drop if AT <=0
gen AT_last=l.a001000000
save "/Users/macbookpro/Desktop/Asset_L.dta"
*Income before extraordinary items*
import delimited /Users/macbookpro/Desktop/利润总额.csv
drop if typrep=="B"
gen date=date(accper,"YMD")
gen month=month(date)
keep if month==12
gen year=year(date)
sort year
tsset ïstkcd year
merge 1:1 ïstkcd year using "/Users/macbookpro/Desktop/Asset_L.dta"
gen income= b001000000
**Calculate ROA**
gen ROA= income/AT_last
sort year
tsset ïstkcd year
**Calculate CH_ROA**
gen ROA_last=l.ROA
gen CH_ROA= ROA-ROA_last
save "/Users/macbookpro/Desktop/CH_ROA&ROA.dta"
*For table1*
sum CH_ROA,detail
sum AT,detail

**MKT_VAL & MTB**
*Calculate Book value of total equity*
rename a001000000 AT
rename a002000000 AL
gen BV= AT- AL
duplicates drop ïstkcd accper,force
*Generate new variables for later merge*
gen date=date(accper,"YMD")
gen month = month(date)
keep if month==12
gen year=year(date)
drop month date
save "/Users/macbookpro/Desktop/BV.dta"
*Merge the MV and BV datasets*
import delimited /Users/macbookpro/Desktop/Closingprice.csv
rename trdynt year
merge 1:1 ïstkcd year using "/Users/macbookpro/Desktop/BV.dta"
keep if _merge==3
drop _merge
rename yclsprc MKT_VAL
gen MTB= MKT_VAL/BV
save "/Users/macbookpro/Desktop/MTB.dta"
*For table1*
sum MKT_VAL MTB,detail

**DIV**
*Dividend issurance data*
import delimited /Users/macbookpro/Desktop/CD_Dividend.csv
gen date=date(ppdadt, "YMD")
gen year=year(date)
duplicates drop year ïstkcd, force
gen DIV=1
save "/Users/macbookpro/Desktop/Dividend.dta"
*Data merging*
use "/Users/macbookpro/Desktop/Total.dta" 
merge 1:1 ïstkcd year using "/Users/macbookpro/Desktop/Dividend.dta"
drop if _merge==2
drop _merge
replace DIV=0 if DIV==.
save "/Users/macbookpro/Desktop/DIV.dta"
*For table1*
sum DIV,detail

***Prepare Table5***
**merge all variables**
use "/Users/macbookpro/Desktop/CH_CS.dta"
duplicates drop ïstkcd accper,force
merge 1:1 ïstkcd year using "/Users/macbookpro/Desktop/Sales_Growth.dta"
keep if _merge==3
drop _merge
drop if typrep=="B"
merge 1:1 ïstkcd year using "/Users/macbookpro/Desktop/CH_ROA&ROA.dta"
keep if merge_==3
drop _merge
merge 1:1 ïstkcd year using "/Users/macbookpro/Desktop/DIV.dta",force
keep if merge_==3
drop _merge
merge 1:1 ïstkcd year using "/Users/macbookpro/Desktop/ISSUE.dta"
keep if merge_==3
drop _merge
merge 1:1 ïstkcd year using "/Users/macbookpro/Desktop/MTB.dta"
keep if merge_==3
drop _merge
merge 1:1 ïstkcd year using "/Users/macbookpro/Desktop/PE.dta"
keep if merge_==3
drop _merge
merge 1:1 ïstkcd year using "/Users/macbookpro/Desktop/Sales_Growth.dta",force
keep if merge_==3
drop _merge
merge 1:1 ïstkcd year using "/Users/macbookpro/Desktop/RET_VOL.dta",force
keep if merge_==3
drop _merge
save "/Users/macbookpro/Desktop/Merge.dta"

**Generate Top, middle, and bottom tercile**
xtile NMAD=MAD, nq(3)

**Winsor all variables, move the extreme values**
winsor AGE, gen(WAGE) p(0.01)
winsor CH_CS, gen(WCH_CS) p(0.01)
winsor CH_ROA, gen(WCH_ROA) p(0.01)
winsor MTB, gen(WMTB) p(0.01)
winsor PE, gen(WPE) p(0.01)
winsor RET_VOL, gen(WRET_VOL) p(0.01)
winsor g, gen(Wg) p(0.01)
winsor DIV, gen(WDIV) p(0.01)
winsor ISSUE, gen(WISSUE) p(0.01)
winsor MKT_VAL, gen(WMKT_VAL) p(0.01)
winsor NMAD, gen(WNMAD) p(0.01)

*Calculate the means*
tab NMAD, sum(WAGE) mean
tab NMAD, sum(WCH_CS) mean
tab NMAD, sum(WCH_ROA) mean
tab NMAD, sum(WMTB) mean
tab NMAD, sum(WDIV) mean
tab NMAD, sum(WISSUE) mean
tab NMAD, sum(WPE) mean
tab NMAD, sum(WRET_VOL) mean
tab NMAD, sum(Wg) mean
tab NMAD, sum(WMKT_VAL) mean

***Prepare for Table6***
***Modified Jones Model, because there are several industries, divide the datasets into multiple sub-datasets basing on CSRC Industry code***
*clean data and merge*
*For AT--Asset total*
import delimited /Users/macbookpro/Desktop/AT&RECT&PPE.csv
drop if typrep=="B"
gen date=date(accper,"YMD")
gen year=year(date)
gen month=month(date)
keep if month==12
drop month date
*Check the duplication*
duplicates tag accper ïstkcd,gen(dup)
tab dup
rename a001000000 AT
save "/Users/macbookpro/Desktop/AT.dta"

**For IB**
*extraordinary data*
import delimited /Users/macbookpro/Desktop/非经常性损益.csv
gen date=date(accper,"YMD")
gen year=year(date)
gen month=month(date)
keep if month==12
drop month date
duplicates drop ïstkcd accper,force
save "/Users/macbookpro/Desktop/Extra.dta"
*For income data*
import delimited /Users/macbookpro/Desktop/利润总额.csv
gen date=date(accper,"YMD")
gen year=year(date)
gen month=month(date)
keep if month==12
drop month date
duplicates drop ïstkcd accper,force
*Data merging*
merge 1:1 ïstkcd year using "/Users/macbookpro/Desktop/Extra.dta",force
drop if _merge==2
drop _merge
replace fn_fn00902=0 if fn_fn00902==.
gen IB= b001000000+ fn_fn00902
drop b001000000 fn_fn00902
save "/Users/macbookpro/Desktop/IB.dta"

*For PPEGT*
import delimited /Users/macbookpro/Desktop/PPE.csv
*Calculate the depreciation expense*
keep if fn02001==1 & typrep==1
drop typrep fn02001 fn02002
gen date=date(accper,"YMD")
gen year=year(date)
gen month=month(date)
drop month date
duplicates drop ïstkcd accper,force
save "/Users/macbookpro/Desktop/DepreciationExpense.dta"
clear
import delimited /Users/macbookpro/Desktop/AT&RECT&PPE.csv
duplicates drop ïstkcd accper,force
merge 1:1 ïstkcd year using "/Users/macbookpro/Desktop/DepreciationExpense.dta"
drop if _merge==2
rename fn02013 PPE
drop _merge
replace PPE= a001212000 if PPE==.
save "/Users/macbookpro/Desktop/PPEGT.dta"

**For operating activities net cash flow--OANCE**
import delimited /Users/macbookpro/Desktop/OANCE.csv
rename c001000000 OANCE
gen date=date(accper,"YMD")
gen year=year(date)
gen month=month(date)
keep if month==12
drop month date
save "/Users/macbookpro/Desktop/OANCE.dta"

**For RECT**
import delimited /Users/macbookpro/Desktop/REC.csv
keep if typrep==1 & fn011a01==1
gen date=date(accper,"YMD")
gen year=year(date)
gen month=month(date)
keep if month==12
drop month date
rename fn011a13 RECT
save "/Users/macbookpro/Desktop/REC.dta"
import delimited /Users/macbookpro/Desktop/AT&RECT&PPE.csv
merge 1:1 ïstkcd year using "/Users/macbookpro/Desktop/REC.dta",force
drop if _merge==2
drop _merge
replace RECT= a001111000 if RECT==.
drop a001111000 a001212000
save "/Users/macbookpro/Desktop/RECT.dta"

**For REVT**
import delimited /Users/macbookpro/Desktop/利润总额.csv
gen date=date(accper,"YMD")
gen year=year(date)
gen month=month(date)
keep if month==12
drop month date
duplicates drop ïstkcd accper,force
drop if typrep=="B"
rename b001000000 REVT
save "/Users/macbookpro/Desktop/REVT.dta"

**Merge all the variables**
use "/Users/macbookpro/Desktop/IB.dta"
merge 1:1 ïstkcd year using "/Users/macbookpro/Desktop/OANCE.dta",force
keep if _merge==3
drop _merge
merge 1:1 ïstkcd year using "/Users/macbookpro/Desktop/REVT.dta",force
keep if _merge==3
drop _merge
merge 1:1 ïstkcd year using "/Users/macbookpro/Desktop/RECT.dta",force
keep if _merge==3
drop _merge
merge 1:1 ïstkcd year using "/Users/macbookpro/Desktop/AT.dta",force
keep if _merge==3
drop _merge
merge 1:1 ïstkcd year using "/Users/macbookpro/Desktop/PPEGT.dta",force
keep if _merge==3
drop _merge
save "/Users/macbookpro/Desktop/Merge!.dta"

**Calculate Modified Jones Model**
drop if AT==.
*prepare dataset for regression*
gen uhat =.
xtset ïstkcd year
gen ta = ( IB - OANCE) / L.AT
gen x1 = 1/L.AT
gen x2 = (d.REVT - d.RECT)/L.AT
gen x3 = PPE / L.AT
drop if x1 ==.
drop if x2 ==.
drop if ta ==.
drop if x3 ==.
sum year
scalar c= r(min)
scalar d= r(max)
gen obs= [_n]
sum obs
scalar e= r(min)
scalar f= r(max)
save "/Users/macbookpro/Desktop/BeforeRegression.dta"
*Redgression*
forvalues x= `=scalar(c)'/`=scalar(d)' {
forvalues j= `=scalar(e)'/`=scalar(f)' {
capture noisily reg ta x1 x2 x3 if year==`x' & obs != `j', nocons
capture noisily predict uhat_2, resid
capture noisily replace uhat_2=. if e(N) <10
capture noisily replace uhat= uhat_2 if year==`x' & obs==`j'
capture noisily drop uhat_2
di `x'
di `j'
}
}

**Prepare table6**
xtile NMAD=MAD, nq(3)
winsor uhat, gen(W_uhat) p(0.005)
reg NMAD W_uhat
*LOSS*
destring b002000000, gen(net_income)
gen LOSS=1
replace LOSS=0 if net_income>0
tab NMAD, sum(net_income) mean
reg NMAD net_income

***Table10***
**Soft Asset**
*change the names*
import delimited /Users/macbookpro/Desktop/SA.csv
rename a001101000 cash
rename a001212000 netPPE
rename a001000000 TA
gen date=date(accper, "YMD")
gen year=year(date)
gen month=month(date)
keep if month==12
drop if TA<0
keep if month==12
keep if typrep=="A"
drop typrep month date
*calculate sort assets*
tsset ïstkcd year
gen SA=( TA- netPPE- cash)/l.TA
drop if SA==.
save "/Users/macbookpro/Desktop/SA.dta"
*merge data*
use "/Users/macbookpro/Desktop/CSV/Total.dta"
merge 1:1 ïstkcd year using "/Users/macbookpro/Desktop/SA.dta"
keep if _merge==3
drop _merge
merge 1:1 ïstkcd year using "/Users/macbookpro/Desktop/Table6.dta",force
keep if _merge==3
drop _merge
merge 1:1 ïstkcd year using "/Users/macbookpro/Desktop/CSV/Merge.dta",force
keep if _merge==3
drop _merge
import delimited /Users/macbookpro/Desktop/STK_Violation_Main.csv
rename symbol ïstkcd
gen date=date(declaredate,"YMD")
gen year=year(date)
duplicates drop ïstkcd declaredate,force
gen AAER=1
replace AAER =0 if isviolated=="N"
save "/Users/macbookpro/Desktop/AAER.dta"
use "/Users/macbookpro/Desktop/CSV/table10.dta"
merge 1:m ïstkcd year using "/Users/macbookpro/Desktop/AAER.dta"
drop if _merge==2
drop _merge
*logit regression*
logit AAER totalMAD W_uhat WAGE WCH_CS WCH_ROA SA ISSUE WMTB WMKT_VAL TA 
logit AAER l.totalMAD W_uhat WAGE WCH_CS WCH_ROA SA ISSUE WMTB WMKT_VAL TA 
logit AAER l.l.totalMAD W_uhat WAGE WCH_CS WCH_ROA SA ISSUE WMTB WMKT_VAL TA 

***Table1***
**Panel A has already done in previous procedure**
**Panel B**
reg WNMAD LOSS AAER WPE TA WMKT_VAL WRET_VOL WMTB
reg LOSS WNMAD AAER WPE TA WMKT_VAL WRET_VOL WMTB
reg AAER WNMAD LOSS WPE TA WMKT_VAL WRET_VOL WMTB
reg WPE WNMAD LOSS AAER TA WMKT_VAL WRET_VOL WMTB
reg TA WNMAD LOSS AAER Wpe WMKT_VAL WRET_VOL WMTB
reg WRET_VOL WNMAD LOSS AAER WPE TA WRET_VOL WMTB
reg WPET_VOL WNMAD LOSS AAER WPE TA WMKT_VAL WMTB
reg WMTB WNMAD LOSS AAER WPE TA WMKT_VAL WPET_VOL
**Panel C**
*convert to numerical and prepare count*
foreach var of varlist a001101000-c006000000{
destring `var',replace
replace count=count+1 if `var'!=.
}
*Generate the three tercile*
xtile Ncount=count, nq(3)
quantiles count, gen(Ncount100) nq(100)
sum(count),detail
tab(count)
*Average digit distributions*
tab Ncount, sum(pro_1) mean
tab Ncount, sum(pro_2) mean
tab Ncount, sum(pro_3) mean
tab Ncount, sum(pro_4) mean
tab Ncount, sum(pro_5) mean
tab Ncount, sum(pro_6) mean
tab Ncount, sum(pro_7) mean
tab Ncount, sum(pro_8) mean
tab Ncount, sum(pro_9) mean
tab Ncount100, sum(pro_1) mean
tab Ncount100, sum(pro_2) mean
tab Ncount100, sum(pro_3) mean
tab Ncount100, sum(pro_4) mean
tab Ncount100, sum(pro_5) mean
tab Ncount100, sum(pro_6) mean
tab Ncount100, sum(pro_7) mean
tab Ncount100, sum(pro_8) mean
tab Ncount100, sum(pro_9) mean

**Pancel D**
xtile NTA=WTA, nq(3)
xtile NTA100=WTA, nq(100)
sum(NTA),detail
tab(NTA)
*Average digit distributions*
tab NTA, sum(pro_1) mean
tab NTA, sum(pro_2) mean
tab NTA, sum(pro_3) mean
tab NTA, sum(pro_4) mean
tab NTA, sum(pro_5) mean
tab NTA, sum(pro_6) mean
tab NTA, sum(pro_7) mean
tab NTA, sum(pro_8) mean
tab NTA, sum(pro_9) mean
tab NTA100, sum(pro_1) mean
tab NTA100, sum(pro_2) mean
tab NTA100, sum(pro_3) mean
tab NTA100, sum(pro_4) mean
tab NTA100, sum(pro_5) mean
tab NTA100, sum(pro_6) mean
tab NTA100, sum(pro_7) mean
tab NTA100, sum(pro_8) mean
tab NTA100, sum(pro_9) mean

***Figure3***
use "/Users/macbookpro/Desktop/CSV/Total.dta"
**Select 002069獐子岛（2014），300268万福生科（2012），002200绿大地（2010） as three cases**
keep if ïstkcd==2069
keep if year==2014
list benford1-benford9
list prop_1-prop_9

keep if ïstkcd==300268
keep if year==2012
list prop_1-prop_9

keep if ïstkcd==2200
keep if year==2010
list prop_1-prop_9









