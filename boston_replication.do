********************************************************************************
* PROJECT: Replication of 1996 Boston Fed Racial Disparities Study
* DATA: 2023 HMDA Public Extract for Boston MSA
* AUTHOR: Benjamin D'Alessandro
* SOFTWARE: Stata 19.0
* DATE: March 2026
********************************************************************************

** Environment Setup **
version 19.0
clear all
macro drop _all
set more off
capture log close
cd "/Users/bendalessandro/Desktop/Work/Stata"
foreach pkg in estout {
    capture which `pkg'
    if _rc ssc install `pkg'
}
capture mkdir "Outputs"
log using "Outputs/boston_replication_log.txt", replace
import delimited "Raw_Data/boston_2023_hmda_subset.csv", clear

** Variable Cleaning **

* 1. Initial Filtering *
replace debt_to_income_ratio = "" if inlist(debt_to_income_ratio, "Exempt", "NA")
keep if !missing(debt_to_income_ratio)
keep if loan_purpose == 1
keep if occupancy_type == 1
keep if derived_dwelling_category == "Single Family (1-4 Units):Site-Built"
keep if loan_type == 1
keep if lien_status == 1
keep if openend_line_of_credit == 2
keep if reverse_mortgage == 2
keep if business_or_commercial_purpose == 2
drop if inlist(derived_race, "Race Not Available", "Free Form Text Only", /// 
	"Joint")
drop if inlist(derived_ethnicity, "Ethnicity Not Available", "Free Form Text Only", ///
	"Joint")

* 2. Destring Variables *
foreach var in loan_to_value_ratio property_value income loan_amount {
	destring `var', replace ignore("Exempt" "NA" " ") force
}

encode lei, gen(lei_long)

* 3. Outlier Removal (Non-Positive or Top 1%) *
drop if income <= 0 | income == .
drop if loan_amount == .
drop if property_value == .
drop if loan_to_value_ratio == .

sum income, detail
local p99_income = r(p99)
sum loan_amount, detail
local p99_loan = r(p99)
sum property_value, detail
local p99_property = r(p99)

drop if income > `p99_income'
drop if loan_amount > `p99_loan'
drop if property_value > `p99_property'

* 4. Categorical and Natural Logarithm Variable Generation *
gen dti_cat = .
replace dti_cat = 1 if inlist(debt_to_income_ratio, "<20%", "20%-<30%", ///
	"30%-<36%")
replace dti_cat = 2 if debt_to_income_ratio == "36%-<43%"
replace dti_cat = 3 if debt_to_income_ratio == "43%-<50%"
replace dti_cat = 4 if inlist(debt_to_income_ratio, "50%-60%", ">60%")

destring debt_to_income_ratio, gen(dti_num) force
replace dti_cat = 1 if dti_num < 36 & missing(dti_cat)
replace dti_cat = 2 if dti_num >= 36 & dti_num < 43 & missing(dti_cat)
replace dti_cat = 3 if dti_num >= 43 & dti_num <= 50 & missing(dti_cat)
replace dti_cat = 4 if dti_num > 50 & missing(dti_cat)

label define dti_lbl 1 "<36%" 2 "36-42%" 3 "43-50%" 4 ">50%"
label values dti_cat dti_lbl

gen aus_reported = 0
replace aus_reported = 1 if inlist(aus1, 1, 2, 3, 4, 5, 7)

gen aus_gse = 0
gen aus_internal = 0
gen aus_other = 0

foreach ausvar in aus1 aus2 aus3 aus4 aus5 {
	replace aus_gse = 1 if inlist(`ausvar', 1, 2)
	replace aus_internal = 1 if `ausvar' == 7
	replace aus_other = 1 if inlist(`ausvar', 3, 4, 5)
}

gen ln_income = ln(income)

* 5. Outcome and Race/Ethnicity Definitions *
gen denied = (action_taken == 3) if inlist(action_taken, 1, 3)
label define denied_lbl 0 "Approved" 1 "Denied"
label values denied denied_lbl

gen white_nh = (derived_race == "White" & derived_ethnicity == "Not Hispanic or Latino")

gen minority = (white_nh == 0) if !missing(white_nh)
label var minority "Minority"

gen race_eth = derived_race
replace race_eth = "Black" if derived_race == "Black or African American"
replace race_eth = "Hispanic" if derived_race == "White" & ///
	derived_ethnicity == "Hispanic or Latino"
replace race_eth = "Native Hawaiian or Pacific Islander" if ///
	derived_race == "Native Hawaiian or Other Pacific Islander"
encode race_eth, gen(granular_race)

gen race_collapsed = race_eth
replace race_collapsed = "Non-Primary Minority" if inlist(derived_race, ///
    "American Indian or Alaska Native", ///
    "Native Hawaiian or Other Pacific Islander", ///
    "2 or more minority races")
encode race_collapsed, gen(race)

** Analysis & Visualizations **

* 1. Font Setup for Visualizations *
graph set window fontface "Times New Roman"
graph set print fontface "Times New Roman"

* 2. Descriptive Statistics *
tabulate denied granular_race, row
tabulate granular_race

tabstat income, by(granular_race) statistics(median sd) columns(statistics)
tabstat dti_num loan_to_value_ratio, by(granular_race) statistics(mean sd) ///
	columns(statistics)

* 3. Model 1: Denial Disparities for All Minority *
logit denied i.minority ln_income loan_to_value_ratio i.dti_cat, vce(cluster lei_long)
eststo m1
esttab m1 using "Outputs/reg_m1_minority.tex", ///
    replace b(3) se(3) booktabs nomtitles nonumber label ///
    title("Logit Results for Denial of Minorities") ///
    addnote("Standard errors clustered by lender in parentheses") ///
    substitute("%" "\%" "<" "\textless{}" ">" "\textgreater{}")
	
margins minority
marginsplot, recast(scatter) ///
    plotopts(msymbol(O) msize(vsmall) mcolor(navy) lcolor(navy)) ///
    ciopts(recast(rcap) lcolor(navy)) ///
    title("Figure 1: Predicted White vs. Minority Loan Denial Probability", ///
	size(medium)) ///
    ytitle("Probability of Denial") xtitle("") ///
    xlabel(0 "White" 1 "Minority") ///
	xscale(range(-0.5 1.5)) ///
    ylabel(0(0.02)0.1) ///
	aspectratio(0.8) ///
	graphregion(fcolor(white) lcolor(white)) ///
    plotregion(margin(small)) ///
    note("Error bars represent a 95% confidence interval." ///
	"Controls: Income, LTV, DTI.", size(vsmall)) ///
    name(minority_estimates, replace)
graph export "Outputs/denial_minority.png", replace

margins, dydx(minority) post
eststo margins_m1
esttab margins_m1 using "Outputs/margins_m1_minority.tex", ///
    replace b(3) se(3) booktabs nomtitles nonumber label ///
    title("Average Marginal Effects for All Minority Applicants") ///
    addnote("Standard errors clustered by lender in parentheses") ///
    substitute("%" "\%" "<" "\textless{}" ">" "\textgreater{}")

* 4. Model 2: Denial by Race *
levelsof race if race_eth == "White", local(white_code)
if "`white_code'" == "" {
	di as error "WARNING: 'White' not found in race_eth; base category not set."
}
else {
	fvset base `white_code' race
}
logit denied i.race ln_income loan_to_value_ratio i.dti_cat, vce(cluster lei_long)
eststo m2
esttab m2 using "Outputs/reg_m2_race.tex", ///
    replace b(3) se(3) booktabs nomtitles nonumber label ///
    title("Logit Results for Denial by Race") ///
    addnote("Standard errors clustered by lender in parentheses") ///
    substitute("%" "\%" "<" "\textless{}" ">" "\textgreater{}")

margins race
marginsplot, recast(scatter) ///
    plotopts(msymbol(O) msize(vsmall) mcolor(navy) lcolor(navy)) ///
    ciopts(recast(rcap) lcolor(navy)) ///
    title("Figure 2: Predicted Loan Denial Probability by Racial and Ethnic Group", ///
	size(medium)) ///
    ytitle("Probability of Denial") xtitle("") ///
    xlabel(, angle(45) labsize(small)) ///
    xscale(range(0.7 5.3)) ///
    ylabel(0(0.05)0.2) ///
    note("Error bars represent a 95% confidence interval." ///
         "Controls: Income, LTV, DTI.", size(vsmall)) ///
    name(race_estimates, replace)
graph export "Outputs/denial_by_race.png", replace

margins, dydx(race) post
eststo margins_m2
esttab margins_m2 using "Outputs/margins_m2_race.tex", ///
    replace b(3) se(3) booktabs nomtitles nonumber label ///
    title("Average Marginal Effects of Race on Denial") ///
    addnote("Standard errors clustered by lender in parentheses") ///
    substitute("%" "\%" "<" "\textless{}" ">" "\textgreater{}")
	
* 5. Model 3: Denial Disparities Controlling for Lender Fixed Effects *
logit denied i.minority ln_income loan_to_value_ratio i.dti_cat i.lei_long, ///
	vce(cluster lei_long)
eststo m3
esttab m3 using "Outputs/reg_m3_lender_fe.tex", ///
    replace b(3) se(3) booktabs nomtitles nonumber label ///
    drop(*.lei_long) ///
    title("Logit Results Controlling for Lender Fixed Effects") ///
    addnote("Standard errors clustered by lender in parentheses") ///
    substitute("%" "\%" "<" "\textless{}" ">" "\textgreater{}")

margins minority
margins, dydx(minority) post
eststo margins_m3
esttab margins_m3 using "Outputs/margins_m3_lender_fe.tex", ///
    replace b(3) se(3) booktabs nomtitles nonumber label ///
    title("Average Marginal Effects for Lender Fixed Effects") ///
    addnote("Standard errors clustered by lender in parentheses") ///
    substitute("%" "\%" "<" "\textless{}" ">" "\textgreater{}")

* 6. Model 4: High Income Sample *
logit denied i.minority ln_income loan_to_value_ratio i.dti_cat ///
	if income > 150, vce(cluster lei_long)
eststo m4
esttab m4 using "Outputs/reg_m4_high_income.tex", ///
    replace b(3) se(3) booktabs nomtitles nonumber label ///
    title("Logit Results for High-Income Sample") ///
    addnote("Standard errors clustered by lender in parentheses") ///
    substitute("%" "\%" "<" "\textless{}" ">" "\textgreater{}")

margins, dydx(minority) post
eststo margins_m4
esttab margins_m4 using "Outputs/margins_m4_high_income.tex", ///
    replace b(3) se(3) booktabs nomtitles nonumber label ///
    title("Average Marginal Effects for High-Income Sample") ///
    addnote("Standard errors clustered by lender in parentheses") ///
    substitute("%" "\%" "<" "\textless{}" ">" "\textgreater{}")

* 7. Model 5: Tract Control *
logit denied i.minority ln_income loan_to_value_ratio i.dti_cat ///
	tract_to_msa_income_percentage, vce(cluster lei_long)
eststo m5
esttab m5 using "Outputs/reg_m5_tract.tex", ///
    replace b(3) se(3) booktabs nomtitles nonumber label ///
    title("Logit Results with Tract Control") ///
    addnote("Standard errors clustered by lender in parentheses") ///
    substitute("%" "\%" "<" "\textless{}" ">" "\textgreater{}")

margins, dydx(minority) post
eststo margins_m5
esttab margins_m5 using "Outputs/margins_m5_tract.tex", ///
    replace b(3) se(3) booktabs nomtitles nonumber label ///
    title("Average Marginal Effects with Tract Control") ///
    addnote("Standard errors clustered by lender in parentheses") ///
    substitute("%" "\%" "<" "\textless{}" ">" "\textgreater{}")
	
* 8. Model 6: AUS Usage Reported *
logit denied i.minority ln_income loan_to_value_ratio i.dti_cat ///
	i.aus_reported, vce(cluster lei_long)
eststo m6
esttab m6 using "Outputs/reg_m6_aus_reported.tex", ///
    replace b(3) se(3) booktabs nomtitles nonumber label ///
    title("Logit Results Controlling for Reported AUS Usage") ///
    addnote("Standard errors clustered by lender in parentheses") ///
    substitute("%" "\%" "<" "\textless{}" ">" "\textgreater{}")

margins, dydx(minority) post
eststo margins_m6
esttab margins_m6 using "Outputs/margins_m6_aus_reported.tex", ///
    replace b(3) se(3) booktabs nomtitles nonumber label ///
    title("Average Marginal Effects for Reported AUS Usage") ///
    addnote("Standard errors clustered by lender in parentheses") ///
    substitute("%" "\%" "<" "\textless{}" ">" "\textgreater{}")
	
* 9. Model 7: AUS Type *
logit denied i.minority ln_income loan_to_value_ratio i.dti_cat ///
	aus_gse aus_internal aus_other, vce(cluster lei_long)
eststo m7
esttab m7 using "Outputs/reg_m7_aus_type.tex", ///
    replace b(3) se(3) booktabs nomtitles nonumber label ///
    title("Logit Results Controlling for AUS Model") ///
    addnote("Standard errors clustered by lender in parentheses") ///
    substitute("%" "\%" "<" "\textless{}" ">" "\textgreater{}")

margins, dydx(minority) post
eststo margins_m7
esttab margins_m7 using "Outputs/margins_m7_aus_type.tex", ///
    replace b(3) se(3) booktabs nomtitles nonumber label ///
    title("Average Marginal Effects for AUS Model") ///
    addnote("Standard errors clustered by lender in parentheses") ///
    substitute("%" "\%" "<" "\textless{}" ">" "\textgreater{}")

* 10. Model 8: Link Test *
quietly logit denied i.minority ln_income loan_to_value_ratio i.dti_cat, ///
	vce(cluster lei_long)
linktest

log close
