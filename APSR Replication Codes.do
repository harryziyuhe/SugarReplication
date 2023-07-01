*Sugar Bill APSR - Replication Code. Results in the paper used STATA version 15.

*these are our main results (Table 1)

* Import Data

use "SugarReplicate.dta", clear


*Generate Dummy Voting Variables and Make Nominal Contributions into Real Ones

gen yesvote=0
replace yesvote=1 if vote=="Aye"

gen repub=0
replace repub=1 if party == "Republican"

gen rsugcont=totalcont
replace rsugcont= totalcont / 1.103 if cong == 115


*install program to create the marginal effects graphs

ssc install marhis

xtset id cong

set matsize 11000

*Create Column 1 of Table 1 (District Fixed Effects) We add the FE manually (i.id) because MARHIS only works with plain Logit command

logit yesvote rsugcont ncu tenure poverty bach medincome over65 agcom i.id, robust

*Create Figure 1

marhis rsugcont, points(50) percent

*Create Figure 2

marhis ncu, points(50) percent

*Create Figure 3

marhis tenure, points(50) percent

*Create Column 2 of Table 1 (Incumbent Fixed Effects) Keep both == 1 keeps only cases where the same incumbent voted on both ammendments

keep if both == 1

logit yesvote rsugcont ncu tenure poverty bach medincome over65 agcom i.id, robust


*Generate Row 1 of Table 3 (District & Incumbent Fixed Effects using a different window as robustness)
* this time, contributions are measured during the calendar year the vote occurred (2013 and 2018)

use "SugarReplicate_iter1.dta", replace

gen yesvote=0

replace yesvote=1 if vote=="Aye"

gen repub=0
replace repub=1 if party == "Republican"

gen rsugcont=totalcont
replace rsugcont= totalcont / 1.103 if cong == 115

logit yesvote rsugcont ncu tenure poverty bach medincome over65 agcom i.id, robust

keep if both == 1

logit yesvote rsugcont ncu tenure poverty bach medincome over65 agcom i.id, robust


*Generate Row 2 of Table 3 (District & Incumbent Fixed Effects using a different window as robustness)
* this time, contributions are measured during the 12 months before each vote occurred

use "SugarReplicate_iter2.dta", replace

gen yesvote=0
replace yesvote=1 if vote=="Aye"
gen repub=0
replace repub=1 if party == "Republican"

gen rsugcont=totalcont
replace rsugcont= totalcont / 1.103 if cong == 115

logit yesvote rsugcont ncu tenure poverty bach medincome over65 agcom i.id, robust

keep if both == 1

logit yesvote rsugcont ncu tenure poverty bach medincome over65 agcom i.id, robust

