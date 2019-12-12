/*
Replication archive for "Where Self-Interest Trumps Ideology: Liberal
Homeowners and Local Opposition to Housing Development" by William Marble
and Clayton Nall. 

This do-file generates survey sampling quotas from the November 2014 CPS. 
*/

clear all


infix year 18-21 state 93-94 county 101-103 faminc 39-40 region 89-90 cbsa 96-100 metstatus 105-105 /*
*/ age 122-123 sex 129-130 race 139-140 hisp 157-158 vote2014 951-952 reg2014 953-954 /*
*/ str8 weight 613-622 citizen 172 - 173 housing 29-30 educ 137-138 /*
*/ using "data/nov14pub.dat", clear


gen len = length(weight)
gen weight2 = ""
replace weight2 = substr(weight, 1, 4) + "." + substr(weight, 5,.) if len >= 5
destring weight2, replace

lab def region 1 "northeast" 2 "midwest" 3 "south" 4 "west"
lab val region region

lab def faminc 1 "less than 5k" 2 "5k to 7500" 3 "7500 to 10k" 4 "10k to 12500" /*
*/ 5 "12500 to 15k" 6 "15k to 20k" 7 "20k to 25k" 8 "25k to 30k" 9 "30k to 35k" /*
*/ 10 "35k to 40k" 11 "40k to 50k" 12 "50k to 60k" 13 "60k to 75k" 14 "75k to 100k" /*
*/ 15 "100k to 150k" 16 "150k or more"
lab val faminc faminc

recode citizen 1/4 = 1 5 = 0

recode educ -3/-1 = . /* missing
*/ 31/38 = 0 /* less than hs
*/ 39 = 1 /* hs diploma
*/ 40/42 = 2 /* some college/assoc. degree
*/ 43 = 3 /* bach. degree
*/ 44/46 = 4 // graduate degree
lab def educ 0 "less than hs" 1 "hs diploma" 2 "some college/assoc. degree" 3 "bachelors degree" 4 "graduate degree"
lab val educ educ



recode race 6/26 = 7
lab def race 1 "white only" 2 "black only" 3 "american indian, alaskan native only" /*
*/ 4 "asian only" 5 "hawaiin/PI only" 6 "white-black" 7 "multiple races"
lab val race race


lab def hisp 1 "hispanic" 2 "non-hispanic"
lab val hisp hisp

lab def sex 1 "male" 2 "female"
lab val sex sex

lab def metstatus 1 "metro" 2 "nonmetro" 3 "not id'd"
lab val metstatus metstatus

lab def housing 1 "own" 2 "rent" 3 "occupy w/o paymnent"
lab val housing housing

gen homeowner = .
replace homeowner = 1 if housing == 1
replace homeowner = 0 if housing > 1


recode cbsa 31100 = 31080
lab def cbsa /*
*/ 16980 "Chicago-Naperville-Elgin, IL-IN-WI" /*
*/ 35620 "New York-Newark-Jersey City, NY-NJ-PA" /*
*/ 31080 "Los Angeles-Long Beach-Anaheim, CA" /*
*/ 19100 "Dallas-Fort Worth-Arlington, TX" /*
*/ 26420 "Houston-The Woodlands-Sugar Land, TX" /*
*/ 47900 "Washington-Arlington-Alexandria, DC-VA-MD-WV" /*
*/ 37980 "Philadelphia-Camden-Wilmington, PA-NJ-DE-MD" /*
*/ 33100 "Miami-Fort Lauderdale-West Palm Beach, FL" /*
*/ 12060 "Atlanta-Sandy Springs-Roswell, GA" /*
*/ 14460 "Boston-Cambridge-Newton, MA-NH" /*
*/ 41860 "San Francisco-Oakland-Hayward, CA" /*
*/ 38060 "Phoenix-Mesa-Scottsdale, AZ" /*
*/ 40140 "Riverside-San Bernardino-Ontario, CA" /*
*/ 19820 "Detroit-Warren-Dearborn, MI" /*
*/ 42660 "Seattle-Tacoma-Bellevue, WA" /*
*/ 33460 "Minneapolis-St. Paul-Bloomington, MN-WI" /*
*/ 41740 "San Diego-Carlsbad, CA" /*
*/ 45300 "Tampa-St. Petersburg-Clearwater, FL" /*
*/ 19740 "Denver-Aurora-Lakewood, CO" /*
*/ 41180 "St. Louis, MO-IL" 
lab val cbsa cbsa

replace reg2014 = 1 if vote2014 == 1
lab def reg2014 1 "yes" 
lab val reg2014 reg2014

keep if ///
cbsa == 16980  | ///
cbsa == 35620  | ///
cbsa == 31080  | ///
cbsa == 19100  | ///
cbsa == 26420  | ///
cbsa == 47900  | ///
cbsa == 37980  | ///
cbsa == 33100  | ///
cbsa == 12060  | ///
cbsa == 14460  | ///
cbsa == 41860  | ///
cbsa == 38060  | ///
cbsa == 40140  | ///
cbsa == 19820  | ///
cbsa == 42660  | ///
cbsa == 33460  | ///
cbsa == 41740  | ///
cbsa == 45300  | ///
cbsa == 19740  | ///
cbsa == 41180  

// registered voter indicator
gen reg = reg2014 == 1

// generate smaller faminc cat
recode faminc 1/5 = 1 5/7 = 2 8/9 = 3 10/11 = 4 12/13 = 5 14/. = 6, gen(faminc2)
lab def faminc2 /*
*/ 1 "Under $15,000" /*
*/ 2 "$15,000 to $24,999" /*
*/ 3 "$25,000 to $34,999" /*
*/ 4 "$35,000 to $49,999" /*
*/ 5 "$50,000 to $74,999" /*
*/ 6 "$75,000 to $99,999+" 
lab val faminc2 faminc2

// collapse race/ethnicity. if hispanic, regardless of other race, count that as
// race
decode race, gen(race2)
replace race2 = "hispanic" if hisp == 1
replace race2 = "other/multiracial" if race == 7 | race == 3 | race == 5
tab race2

// collapse age
recode age 18/24 = 1 25/44 = 2 45/64 = 3 65/. = 4, gen(agecat)
lab def agecat 1 "18 - 24" 2 "25 - 44" 3 "45 - 64" 4 "65+"
lab val agecat agecat
keep if age >= 18


// PRETEND ALL OF PHILADELPHIA IS IN THE NORTHEAST CENSUS REGION
// this is due to the philly MSA comprising parts of delaware, which is in the 
// south census region. 
replace region = 1 if cbsa == 37980

// gen count variable
gen n = 1

save temp.dta, replace


// list MSA's by region
duplicates drop region cbsa, force
sort region cbsa
keep region cbsa
outsheet using "data/msas_by_region.csv", replace comma



// tab sample size in each region
use temp.dta, clear
local tot_n = 4000
collapse (sum) n [aw=weight2], by(region)
egen temp = total(n)
gen proportion = n / temp
gen samp_size = round(proportion * `tot_n')
drop temp
drop n
list





// MARGINALS FOR NORTHEAST CENSUS REGION
use temp.dta, clear
keep if region == 1
di "REGION: NORTHEAST"
tab faminc2 [aw = weight2]
tab sex [aw = weight2]
tab agecat [aw = weight2]
tab race2 [aw = weight2]
tab cbsa [aw = weight2]
tab educ [aw = weight2]
tab homeowner [aw= weight2]



// MARGINALS FOR MIDWEST CENSUS REGION
use temp.dta, clear
keep if region == 2 
di "REGION: MIDWEST"
tab faminc2 [aw = weight2]
tab sex [aw = weight2]
tab agecat [aw = weight2]
tab race2 [aw = weight2]
tab cbsa [aw = weight2]
tab educ [aw = weight2]
tab homeowner [aw= weight2]


// MARGINALS FOR SOUTH CENSUS REGION
use temp.dta, clear
keep if region == 3
di "REGION: SOUTH"
tab faminc2 [aw = weight2]
tab sex [aw = weight2]
tab agecat [aw = weight2]
tab race2 [aw = weight2]
tab cbsa [aw = weight2]
tab educ [aw = weight2]
tab homeowner [aw= weight2]




// MARGINALS FOR WEST CENSUS REGION
use temp.dta, clear
keep if region == 4
di "REGION: WEST"
tab faminc2 [aw = weight2]
tab sex [aw = weight2]
tab agecat [aw = weight2]
tab race2 [aw = weight2]
tab cbsa [aw = weight2]
tab educ [aw = weight2]
tab homeowner [aw= weight2]


rm temp.dta
