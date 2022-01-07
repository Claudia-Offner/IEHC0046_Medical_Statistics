// IEHC0046 DATA ANALYSIS EXERCISE 
// SCN: RLVM8
// Set working directory (Note: datasets assessment2021a & assessment2021b must be in this location)
cd "/Users/offne/OneDrive - University College London/IEHC0046 Statistics for Medical Sciences/ASSESSMENT/"

// ----------------------------------- PART 1 -----------------------------------

*** open data ***
use "assessment2021a.dta", clear

*** open log file ***
// log using "RLVM8_results.log"


// 0. DATA CLEANING
// Check min/max & missing values
sum bronch sex motheduc age damp mcig car famhist cooking border heating
misstable summarize
display (23+7+9+3+1+19+1)

// Power Test 1 (Before dropping values)
tab bronch damp, col 
display (280/1589)
display (367/1711)
power twoproportions .17621145 .21449445, n1(1589) n2(1711) // Power of 0.7913
display (1344/2653)
display (367/647)
power twoproportions .50659631 .56723338, n1(2653) n2(647) // Power of 0.7919

// Drop missing values in data for complete case analysis
drop if bronch ==.
drop if motheduc ==.
drop if damp ==.
drop if car ==.
drop if cooking ==.
drop if border ==.
drop if heating ==.

// Power Test 2 (After dropping values)
tab bronch damp, col 
display (278/1572)
display (362/1698)
power twoproportions 0.17684478 0.21319199, n1(1572) n2(1698) // Power of 0.7454
display (1336/2630)
display (362/640)
power twoproportions .50798479 .565625, n1(2630) n2(640) // Power of 0.7459

// Create binary label for mother smoker
generate mcig_bin = 0 
replace mcig_bin = 1 if mcig>0
label var mcig_bin "mother smoker (1=yes)"
tab mcig mcig_bin,m

// Analytical sample - 3 categorical; 8 binary 
sum bronch sex motheduc age damp mcig_bin car famhist cooking border heating

// 1. SUMMARISE THE STUDY SAMPLE
tab damp 
tab bronch 
tab sex 
tab age 
tab border
tab motheduc
tab mcig_bin 
tab famhist 
tab car 
tab cooking 
tab heating 


// NOT IN REPORT: Cross tabulate with dampness 
// tab bronch damp, col 
// tab sex damp, col
// tab motheduc damp, col
// tab age damp, col
// tab mcig_bin damp, col
// tab car damp, col
// tab famhist damp, col
// tab cooking damp, col
// tab border damp, col
// tab heating damp, col
// tab damp 

// 2. ASSOCIATIONS WITH BRONCHITIS AND DAMP EXPOSURE 

// A. Evaluate the association between possible risk factors and study outcome using non-regression analysis
// CHI SQUARE TEST (Significant: damp, motheduc, famhist)
tab damp bronch, col chi
tab sex bronch, col chi
tab age bronch, col chi // Categorical 
tabodds bronch age // + test for trend
tab border bronch, col chi // Categorical 
tabodds bronch border // + test for trend
// mhodds bronch border, c(2,1) 
// mhodds bronch border, c(3,1) 
// mhodds bronch border, c(4,1) 
tab motheduc bronch, col chi // Categorical 
tabodds bronch motheduc // + test for trend
mhodds bronch motheduc, c(2,1) 
mhodds bronch motheduc, c(3,1) 
tab mcig_bin bronch, col chi
tab famhist bronch, col chi
tab car bronch, col chi
tab cooking bronch, col chi
tab heating bronch, col chi

// B. Evaluate the association between possible risk factors and damp exposure as above.
// CHI SQUARE TEST (Significant: sex, age, motheduc, famhist, car, heating)
tab sex damp, col chi
tab age damp, col chi // Categorical
tabodds damp age // + test for trend
mhodds damp age, c(8,7) 
mhodds damp age, c(9,7) 
mhodds damp age, c(10,7) 
tab border damp, col chi // Categorical
tabodds damp border // + test for trend
tab motheduc damp, col chi // Categorical
tabodds damp motheduc // + test for trend
mhodds damp motheduc, c(2,1) 
mhodds damp motheduc, c(3,1) 
mhodds damp motheduc, c(1,3) 
mhodds damp motheduc, c(2,3) 
tab mcig_bin damp, col chi
tab famhist damp, col chi
tab car damp, col chi
tab cooking damp, col chi
tab heating damp, col chi


// 3. ASSESSMENT OF CONFOUNDING AND EFFECT MODIFICATION
// Effect modifier: homogeneity significant & stratum shows meaninful differences
// Confounder: homogeneity not significant & mantel-haenszel output different from crude
// Remove: homogeneity not significant & mantel-haenszel output not very different from crude

// CHECK: sex, age, motheduc, famhist, car, heating
mhodds bronch damp // Crude
mhodds bronch damp, by(sex) // Effect modifier
mhodds bronch damp, by(motheduc) // Confounder
mhodds bronch damp, by(famhist) // Remove
mhodds bronch damp, by(age) // Remove
mhodds bronch damp, by(car) // Remove
mhodds bronch damp, by(heat) // Remove

mhodds bronch damp motheduc, by(sex) // Final model


// ----------------------------------- PART 2 -----------------------------------

use "assessment2021b.dta", clear


// 0. DATA CLEANING
// Check min/max & missing values
sum sex wpactive smoker lungfunc nssec3 partner agegroup children bmigroup coal
codebook smoker bmigroup sex

// Check smoker -8 (remove 3 instances)
drop if smoker == -8

// Merge underweight BMI with normal (not many of them)
generate bmi2 = 0 
replace bmi2 = 1 if bmigroup <= 2
replace bmi2 = 2 if bmigroup == 3
replace bmi2 = 3 if bmigroup == 4
label define bmilabel 1 "Normal/Underweight" 2 "Pre-obesity" 3 "Obesity"
label values bmi2 bmilabel
label var bmi2 "BMI Group (underweight merged)"
tab bmigroup bmi2,m

// Rename lung function for plotting
label var lungfunc "Lung Function (Litres)"

// Recode sex to 0/1
generate sex2 = 0 
replace sex2 = 1 if sex == 2
label define sexlabel 0 "Male" 1 "Female"
label values sex2 sexlabel
label var sex2 "Sex coded 0/1"

// Reverse code occupational class
vreverse nssec3, gen(nssec3_rev)
codebook nssec3_rev

// Analytical sample - 1 continous; 3 categorical; 6 binary 
sum sex2 wpactive smoker lungfunc nssec3_rev partner agegroup children bmi2 coal


// 1. Summarise the study sample 
sum lungfunc, detail // median is 50%
tab nssec3_rev
tab wpactive
tab sex2
tab agegroup
tab bmi2
tab smoker
tab coal
tab partner
tab children

histogram lungfunc, normal color(eltblue) title("Distribution of Lung Function") subtitle("n = 3,896 participants") graphregion(color(white)) bgcolor(white)

// 2. ASSOCIATIONS WITH LUNG FUNCTION AND OCCUPATIONAL CLASS 

// A. Evaluate the association between possible risk factors and lung function using non-regression analysis.
// ANOVA (Significant: All)
oneway lungfunc nssec3_rev, tab // failed equivlance assumption
oneway lungfunc wpactive, tab
oneway lungfunc sex2, tab // failed equivlance assumption
oneway lungfunc agegroup, tab
oneway lungfunc bmi2, tab // failed equivlance assumption
oneway lungfunc smoker, tab
oneway lungfunc coal, tab // failed equivlance assumption
oneway lungfunc partner, tab
oneway lungfunc children, tab

// Kruskal-Wallis test
kwallis lungfunc, by (nssec3_rev)
kwallis lungfunc, by (sex2)
kwallis lungfunc, by (bmi2)
kwallis lungfunc, by (coal)

// Boxplots
graph box lungfunc, over(nssec3_rev) box(1, color(eltblue)) title("Distribution of Occupational Class by Lung Function") subtitle("n = 3,896 participants") graphregion(color(white)) bgcolor(white)

graph box lungfunc, over(sex2) box(1, color(eltblue)) title("Distribution of Sex by Lung Function") subtitle("n = 3,896 participants") graphregion(color(white)) bgcolor(white)

graph box lungfunc, over(bmi2) box(1, color(eltblue)) title("Distribution of BMI by Lung Function") subtitle("n = 3,896 participants") graphregion(color(white)) bgcolor(white)

graph box lungfunc, over(coal) box(1, color(eltblue)) title("Distribution of Coal Heating by Lung Function") subtitle("n = 3,896 participants") graphregion(color(white)) bgcolor(white)

// B.	Evaluate the association between possible risk factors and occupational class as above.

//  CHI SQUARE TEST (Significant: All, except children) 
tab nssec3_rev wpactive,col chi
tabodds wpactive nssec3_rev // + test for trend
tab nssec3_rev sex2, col chi 
tabodds sex2 nssec3_rev // + test for trend
tab nssec3_rev agegroup, col chi // cannot test for trends
tab nssec3_rev bmigroup, col chi // cannot test for trends
tab nssec3_rev smoker, col chi
tabodds smoker nssec3_rev // + test for trend
tab nssec3_rev coal, col chi
tabodds coal nssec3_rev // + test for trend
tab nssec3_rev partner, col chi
tabodds partner nssec3_rev // + test for trend
tab nssec3_rev children, col chi 
tabodds children nssec3_rev // + test for trend

// NOT IN REPORT:
// // ANOVA (same as test for trends)
// oneway nssec3_rev wpactive, tab
// oneway nssec3_rev sex, tab // failed equivlance assumption
// oneway nssec3_rev agegroup, tab
// oneway nssec3_rev bmi2, tab
// oneway nssec3_rev smoker, tab
// oneway nssec3_rev coal, tab
// oneway nssec3_rev partner, tab
// oneway nssec3_rev children, tab
//
// // Kruskal-Wallis test
// kwallis nssec3_rev, by (sex)

// 3. ASSESSMENT OF CONFOUNDING AND EFFECT MODIFICATION
// Occupational Class: suggests that occpational class should be treated as categorical (not ordinal)
regress lungfunc nssec3_rev 
est store m1
regress lungfunc i.nssec3_rev 
est store m2
lrtest m1 m2 //p-value= 0.0141 (model2 is better)

codebook bmi2
// Crude association
regress lungfunc i.nssec3_rev 

// Assess potential MODIFIERS (using testparam interactions)
// Assess potential CONFOUNDERS (using changes in regression coefficients)

regress lungfunc i.nssec3_rev##i.coal
testparm i.nssec3_rev#i.coal // p=0.050 (marginally consider as EM)
regress lungfunc i.nssec3_rev  coal
regress lungfunc i.nssec3_rev  if coal == 0 // no 
regress lungfunc i.nssec3_rev  if coal == 1 // yes

regress lungfunc i.nssec3_rev##i.sex2
testparm i.nssec3_rev#i.sex2 // p=0.006 (EM)
regress lungfunc i.nssec3_rev  sex2
regress lungfunc i.nssec3_rev  if sex2 == 0 // male
regress lungfunc i.nssec3_rev  if sex2 == 1 // female

regress lungfunc i.nssec3_rev##i.partner
testparm i.nssec3_rev#i.partner // p=0.012 (EM)
regress lungfunc i.nssec3_rev  partner
regress lungfunc i.nssec3_rev  if partner == 0 // no
regress lungfunc i.nssec3_rev  if partner == 1 // yes

regress lungfunc i.nssec3_rev##i.agegroup
testparm i.nssec3_rev#i.agegroup // p=0.670 (not EM, maybe Confounder)
regress lungfunc i.nssec3_rev agegroup
regress lungfunc i.nssec3_rev if agegroup == 1 // 50-54 years
regress lungfunc i.nssec3_rev if agegroup == 2 // 55-59 years
regress lungfunc i.nssec3_rev if agegroup == 3 // 60-64 years
regress lungfunc i.nssec3_rev if agegroup == 4 // 65-70 years

regress lungfunc i.nssec3_rev##i.wpactive
testparm i.nssec3_rev#i.wpactive // p=0.163 (not EM, maybe confounder) - REMOVE
regress lungfunc i.nssec3_rev  wpactive
regress lungfunc i.nssec3_rev  if wpactive == 0 // not in work
regress lungfunc i.nssec3_rev  if wpactive == 1 // in work

regress lungfunc i.nssec3_rev##i.smoker
testparm i.nssec3_rev#i.smoker // p=0.842 (not EM, maybe confounder) - REMOVE
regress lungfunc i.nssec3_rev  smoker
regress lungfunc i.nssec3_rev  if smoker == 0 // not a current smoker
regress lungfunc i.nssec3_rev  if smoker == 1 // current smoker

regress lungfunc i.nssec3_rev##i.bmi2
testparm i.nssec3_rev#i.bmi2 // p=0.510 (not EM, not confounder) - REMOVE
regress lungfunc i.nssec3_rev  bmi2
regress lungfunc i.nssec3_rev  if bmi2 == 1 // normal / underweight
regress lungfunc i.nssec3_rev  if bmi2 == 2 // pre-obesity
regress lungfunc i.nssec3_rev  if bmi2 == 3 // obesity

regress lungfunc i.nssec3_rev##i.children
testparm i.nssec3_rev#i.children // p=0.192 (not EM, not confounder) - REMOVE
regress lungfunc i.nssec3_rev  children
// regress lungfunc i.nssec3_rev  if children == 0 // never
// regress lungfunc i.nssec3_rev  if children == 1 // have a child

// Check for multiple EM intereactions (without confounders)
regress lungfunc agegroup i.nssec3_rev##sex2 i.nssec3_rev##partner i.nssec3_rev##coal 
testparm i.nssec3_rev#i.sex2 // EM (0.035 marginally)
testparm i.nssec3_rev#i.partner // EM (0.027 marginally)
testparm i.nssec3_rev#i.coal // not EM (0.203) - REMOVE

// FINAL MODEL
bysort sex2 partner: regress lungfunc i.nssec3_rev agegroup
regress lungfunc agegroup i.nssec3_rev##sex2 i.nssec3_rev##partner 

// ASSUMPTION CHECKS

// 1. Normality of residuals - normally distributed (for the most part)
predict residual, residuals
kdensity residual, normal  title("Kernel Density Estimate of Normality") graphregion(color(white)) bgcolor(white)
pnorm residual, title("Standardized Normal Probability Plot") graphregion(color(white)) bgcolor(white)

// 2. Linearity of residuals - slight linearity
twoway (scatter lungfunc nssec3_rev) (lfit lungfunc nssec3_rev), title("Linearity of Residuals") graphregion(color(white)) bgcolor(white)

// 3. Equal variance of residuals (heteroskedasticity) - assumption met
rvfplot, yline(0) title("Equal Variance of Residuals") graphregion(color(white)) bgcolor(white) // no clear pattern of heteroskedasticity (red line)


// 4. Independence of residuals (multicollinearity)
// estat vif // assumption violated (beyond scope of module)


