** Install ados
* findit polychoric
* findit kr20
* findit fre
* findit labutil

* Syntax associated with Source-manuscript
* Last update: 09-04-2020

* set folders
cd "G:\My Drive\Paper Eprivacy"
global save "C:\Users\u1277277\surfdrive\Shared\PhD Outline - Angelica\Educational divide in e-privacy\For submission to SRO\Revision\Figures"

* open data: EUROBAROMETER 87.1
u ZA6861_v1-2-0.dta, clear
 
***************************
**   Data preparation    **
***************************
* country
fre country
replace country=9 if country==10 // reunite GB
replace country=4 if country==14 // renunite DE
label def COUNTRY 9 "UK - United Kingdom" 4 "DE - Germany", modify
fre country

* e-privacy
global varprivacy qd17_1 qd17_2 qd17_3 qd17_4 qd17_5 qd17_6 qd17_7 qd17_8 qd17_9 qd17_10 qd17_11
sum $varprivacy
tab1 $varprivacy, m nol

egen nmiss=rowmiss($varprivacy)
tab nmiss
keep if nmiss==0 //retain only those who were asked about e-privacy

** rename to make vars more meaningful
rename qd17_1 lesspurchase
rename qd17_2 lessbank
rename qd17_3 lessinfo
rename qd17_4 settings
rename qd17_5 trusted
rename qd17_6 differentpw
rename qd17_7 reject
rename qd17_8 useownpc
rename qd17_9 antivirus
rename qd17_10 cancelpurchase
rename qd17_11 changepw

global varprivacy lesspurchase lessbank lessinfo settings trusted differentpw ///
					reject useownpc antivirus cancelpurchase changepw
					

		
** graph bar % mentions
save temp.dta, replace
use temp.dta, clear

sum reject
global nobsfig1 = r(N)
di "$nobsfig1" 

preserve


local j = 1 
foreach var of varlist $varprivacy {
	di `j'
	clonevar priv`j'=`var'
	local ++j
}

lab var priv1 "Are less likely to buy goods or services online"
lab var priv2 "Are less likely to bank online"
lab var priv3 "Are less likely to give personal information on websites"
lab var priv4 "Have changed the security settings of your browser, online social network accounts, search engine, etc."
lab var priv5 "Decided to only visit websites you know and trust"
lab var priv6 "Started using different passwords for different websites "
lab var priv7 "Started opening only emails from people and addresses that you know "
lab var priv8 "Only use your own computer"
lab var priv9 "Have installed anti-virus software or you changed the one you had "
lab var priv10 "Cancelled an online purchase because of suspicions about the seller or website"
lab var priv11 "Started changing your passwords regularly"


foreach n of numlist 1/11 {
	local lab: variable label priv`n'  
    gen str labpriv`n' =  "`lab'" 
	di "labpriv`n'"
}

keep serialid priv* labpriv*
reshape long priv labpriv, i(serialid) j(privvar)
*browse

collapse priv, by(labpriv)
replace priv = priv*100
format priv %8.2f

set scheme plotplain

browse


 *** FIGURE 1 ****
graph hbar priv, over(labpriv, sort(1) lab(labsize(vsmall)) ///
	relabel(6 `" "Have changed the security settings of your browser, online social network" "accounts, search engine, etc." "')) ///
	ylab(0 (10) 50) ///
	ytitle("% of respondents mentioning action", size(small)) ///
	caption("Source: Eurobarometer 87.1 | N = $nobsfig1", size(vsmall) position(5))	///
	name(fig1, replace)
graph export "$save/figure1.pdf", replace
graph export "$save/figure1.eps", replace
graph export "$save/figure1.emf", replace
graph export "$save/figure1.tif", replace
graph export "$save/figure1.png", replace

restore

				
					

** cross tabulations
foreach x of varlist $varprivacy {
foreach y of varlist $varprivacy {
tab `x' `y', cell nof chi V
}
}

polychoric $varprivacy
display r(sum_w)
global N = r(sum_w)

matrix r = r(R)
factormat r, pcf n($N) factor(1)
rotate, blank(.4)

** factor analysis
/* **Commented because polychoric takes some time **
*** all variables
polychoric $varprivacy
display r(sum_w)
global N = r(sum_w)

matrix r = r(R)
factormat r, pcf n($N) factor(1)
rotate, blank(.4)

*** remove ownpc because of high uniqueness/low factor loading
global varprivacy2 lesspurchase lessbank lessinfo settings trusted differentpw ///
					reject antivirus cancelpurchase changepw
polychoric $varprivacy2
display r(sum_w)
global N = r(sum_w)

matrix r = r(R)
factormat r, pcf n($N) factor(1)
rotate, blank(.4)

*** remove lessbank because of high uniqueness/low factor loading
global varprivacy2 lesspurchase lessinfo settings trusted differentpw ///
					reject antivirus cancelpurchase changepw
polychoric $varprivacy2
display r(sum_w)
global N = r(sum_w)

matrix r = r(R)
factormat r, pcf n($N) factor(1)
rotate, blank(.4)
*/

*** remove lesspurchase because of high uniqueness/low factor loading
global varprivacy2 lessinfo settings trusted differentpw ///
					reject antivirus cancelpurchase changepw
polychoric $varprivacy2
display r(sum_w)
global N = r(sum_w)

matrix r = r(R)
factormat r, pcf n($N) factor(1)
predict factorprivacy
estat kmo
rotate, blank(.4)

*** KR20 **
**Computes the reliability coefficient of a set of dichotomous items, 
** whereas Cronbach's alpha is used for multipoint scales. In addition, 
** KR20 computes the item difficulty (proportion of 'right' answers), 
** the average value of item difficulty, the item variance, the item-test 
** point-biserial correlation coefficients, and the average value of item-test 
** correlation coefficients. 
kr20 $varprivacy2
alpha $varprivacy2, c d i 
// equivalent result: 0.6674

egen sumprivacy = anycount($varprivacy2), v(1)
corr factorprivacy sumprivacy // high correlatin = high validity
sum sumprivacy	
	
* Education
numlabel D8 D8R1 D8R2, add force
tab1 d8 d8r1 d8r2

rename d11 age
tab age

// Add also those who still study basing on age (final variable is " years spent in education");
// I will need to later check if the impact of education varies by age categories 
// as I can expect it matters less for younger people, digital natives, and more for
// elderly people
clonevar eduage=d8

gen stillstud=eduage==98
tab stillstud 

replace eduage=age if eduage==98

list eduage d8 age in 1/100 if d8==98
recode eduage (97=.c)(99=.a)(0=.b)(2 3 4 5 = 5) 
lab def D8 .c "No FT edu" .a "DK" .b "Refusal" 2 "2" 5 "<5", modify
tab eduage

lab var eduage "Age at finishing FT education"

tab d8r2
recode d8r2 (8 = .a) (7 = .b) (5 = 1) (4= .c), gen(educat)

** Years of schooling
fre country
gen agestart=6
replace agestart=5 if country==3 | country==9 | country==25
replace agestart=7 if country==7 | country==16 | country==17 ///
		| country==21 | country==23 | country==26 | country==29 | country ==30
table country, c(mean agestart)

gen yedu=eduage-agestart
recode yedu (-2 -1 0 = 1)
replace yedu=0 if eduage==.c
lab def yedu 0 "No ft education"
lab val yedu yedu
tab yedu 

lab var yedu "Years of schooling"

recode yedu (0=0 "No ft education") (1/7=7 "7 years or less") (8=8 "8 years") ///
	(9=9 "9 years") (10=10 "10 years") ///
	(11=11 "11 years") (12=12 "12 years") (13=13 "13 years") ///
	(14=14 "14 years") (15=15 "15 years") (16=16 "16 years") ///
	(17=17 "17 years") (18=18 "18 years") (19=19 "19 years") ///
	(20/max=20 "20 years or more"), gen(yedu2)
	
* Cohorts
gen yborn=2016-age
sum yborn
gen generations=.
lab var generations "Generations"
lab def generations ///
 1 "Silent generation" ///
 2 "Baby boomers" ///
 3 "Generation X" ///
 4 "Millennials" ///
 5 "iGen", modify
lab val generations generations

replace generations=1 if yborn>=1918 & yborn<=1945
replace generations=2 if yborn>=1946 & yborn<=1964
replace generations=3 if yborn>=1965 & yborn<=1976
replace generations=4 if yborn>=1977 & yborn<=1995
replace generations=5 if yborn>=1996 

tab generations

gen agecat=.
lab def agecat ///
 1 "15-24" ///
 2 "25-34" ///
 3 "35-44" ///
 4 "45-54" ///
 5 "55-64" ///
 6 "65-74" ///
 7 "75+" , modify

sum age 
 
replace agecat=1 if age<=24
replace agecat=2 if age>24 & age<35
replace agecat=3 if age>34 & age<45
replace agecat=4 if age>44 & age<55
replace agecat=5 if age>54 & age<65
replace agecat=6 if age>64 & age<75
replace agecat=7 if age>74

lab val agecat agecat
lab var agecat "Age categories"
tab agecat 
 
* Occupation
fre d15a d15a_r1 d15a_r2 d15b d15b_r

// make dummies for students and unemployed
gen student=0
replace student=1 if d15a_r2==8
lab var student "Student (full time)"
gen unemployed = d15a_r2==7
lab var unemployed "Unemployed"

* Internet use
tab1 d62_1 d62_2 d62_3 d62_4 d62_5
global internetuse d62_1 d62_2 d62_3 d62_4 d62_5
fre $internetuse

fre netuse //turn!
gen intuse=5-netuse
lab def intuse 0"Less often" 1"Two or three times a month" ///
	2 "About once a week" 3 "Two or three times a week" ///
	4 "Everyday/Almost everyday", modify
lab val intuse intuse
fre intuse

* Sex
fre d10
gen female= d10==2
fre female
lab var female "Female"

* Settlement
fre d25
recode d25 (8=.)
rename d25 settlement
lab var settlement "Type of settlement"

* Digital skills
global skills qd4_1 qd4_2 qd4_3 qd4_4 qd4_5
sum $skills
tab1 $skills

foreach var of varlist $skills {
recode `var' (5 = .a)(1=4)(2=3)(3=2)(4=1)
}
*
sum $skills
alpha $skills

factor $skills, pcf
estat kmo
// qd4_2 only asked if currently employed
// qd4_3 only asked if currently not retired
// nap is .i

gen skills=.
egen skills1=rowmean(qd4_1 qd4_4 qd4_5) if qd4_2==.i & qd4_3==.i
sum skills1
egen skills2=rowmean(qd4_1 qd4_3 qd4_4 qd4_5) if qd4_2==.i & qd4_3!=.i
sum skills2
egen skills3=rowmean(qd4_1 qd4_2 qd4_3 qd4_4 qd4_5) if qd4_2!=.i & qd4_3!=.i
sum skills3
sum skills* // total= 21189

replace skills=skills1 if qd4_2==.i & qd4_3==.i
replace skills=skills2 if qd4_2==.i & qd4_3!=.i
replace skills=skills3 if qd4_2!=.i & qd4_3!=.i
sum skills
 
* Reflexivity
numlabel, add force
tab1 qd1_1 qd1_2 qd1_3
foreach i of varlist qd1_1 qd1_2 qd1_3 {
recode `i' (7=.a)(6=.b)(5=2.5)
}
*
tab qc3_5 // climate change: your responsibility to tackle it (1)
rename qc3_5 responsible
lab var responsible "Climate change: own responsibility"

tab qd12_3 //  ROBOTS/AI: REQUIRE CAREFUL   MANAGEMENT
recode qd12_3 (1=4)(2=3)(3=2)(4=1)(5=.a), gen(robots) // high values: wary

tab1 qc2 
recode qc2 (11=.a)
rename qc2 climate // how serious is climate change

sum robots responsible climate
corr robots responsible climate yedu

tab qc5 // take action against cc
recode qc5 (3=0)(2=0)
rename qc5 takeaction
tab takeaction responsible, cell chi V

sum climate
recode climate  ( 7 8 9 10 = 1 "Serious issue") (1 2 3 4 5 6  = 0), gen(climated)
tab climate climated

** factor
polychoric climated responsible takeaction
display r(sum_w)
global N = r(sum_w)
matrix r = r(R)
factormat r, pcf n($N) factor(1)
estat kmo
rotate, blank(.4)

predict reflexive
sum reflexive

alpha climated responsible takeaction , c d i
kr20 climated responsible takeaction
// low reliability >> maybe better using single item... 

egen sumreflexive = anycount(climated responsible takeactio), v(1)
corr reflexive sumreflexive

lab var sumreflexive "Index of reflexivity"
sum sumreflexive
numlabel, remove

***
save data_analysis.dta, replace // save modified dataset
****

*******************************
*** COUNTRY LEVEL VARIABLES ***
*******************************

* DESI 
* Import file
import delimited using desi.csv, rowr(16:161) clear varnames(16)
*browse

****
save desi2017.dta, replace
u desi2017.dta, clear // to be used in R to produce map
****
encode series, gen(indicator)
fre indicator

encode name, gen(country)

fre country

gen cntry=.			
replace cntry=	18	if country==	1
replace cntry=	2	if country==	2
replace cntry=	29	if country==	3
replace cntry=	32	if country==	4
replace cntry=	19	if country==	5
replace cntry=	20	if country==	6
replace cntry=	7	if country==	7
replace cntry=	21	if country==	8
replace cntry=	16	if country==	10
replace cntry=	1	if country==	11
replace cntry=	4	if country==	12
replace cntry=	11	if country==	13
replace cntry=	22	if country==	14
replace cntry=	8	if country==	15
replace cntry=	5	if country==	16
replace cntry=	23	if country==	17
replace cntry=	24	if country==	18
replace cntry=	6	if country==	19
replace cntry=	25	if country==	20
replace cntry=	3	if country==	21
replace cntry=	26	if country==	22
replace cntry=	13	if country==	23
replace cntry=	30	if country==	24
replace cntry=	27	if country==	25
replace cntry=	28	if country==	26
replace cntry=	12	if country==	27
replace cntry=	17	if country==	28
replace cntry=	9	if country==	29
drop if cntry==.

reshape wide series name code y country, i(cntry) j(indicator)

keep cntry name1 code1 y1 y2 y3 y4 y5
egen desi=rowtotal(y*)
sum desi // mean 47.67
table code1, c(mean desi)

****
save desitomerge.dta, replace
****

* GDP 
import excel using TEC001141573650576038.xlsx, sh("Sheet 1") cellrange(A10:B38) clear firstrow
sort geoLabels
*browse

rename B gdp

encode geoLabels, gen(country)

gen cntry=.			
replace cntry=	18	if country==	1
replace cntry=	2	if country==	2
replace cntry=	29	if country==	3
replace cntry=	32	if country==	4
replace cntry=	19	if country==	5
replace cntry=	20	if country==	6
replace cntry=	7	if country==	7
replace cntry=	21	if country==	8
replace cntry=	16	if country==	9
replace cntry=	1	if country==	10
replace cntry=	4	if country==	11
replace cntry=	11	if country==	12
replace cntry=	22	if country==	13
replace cntry=	8	if country==	14
replace cntry=	5	if country==	15
replace cntry=	23	if country==	16
replace cntry=	24	if country==	17
replace cntry=	6	if country==	18
replace cntry=	25	if country==	19
replace cntry=	3	if country==	20
replace cntry=	26	if country==	21
replace cntry=	13	if country==	22
replace cntry=	30	if country==	23
replace cntry=	27	if country==	24
replace cntry=	28	if country==	25
replace cntry=	12	if country==	26
replace cntry=	17	if country==	27
replace cntry=	9	if country==	28
drop if cntry==.

****
save gdp.dta, replace
****
u  gdp.dta, replace
sum gdp


*****************
** Merge files **
*****************

****
u data_analysis.dta, clear
rename country cntry
****
* merge files

* desi
merge m:m cntry using desitomerge.dta

****
save datamerged.dta, replace
****
drop _merge

* gdp
merge m:m cntry using gdp.dta
drop _merge



****
save datamerged.dta, replace

*******************
** Data analysis **
*******************

u datamerged.dta, clear
****

corr desi gdp 

* Truncate outliers
gen noftedu = yedu==0
tab noftedu
tab stillstud

sum yedu, de
gen yedut = yedu 
*replace yedut=r(p1) if yedu<=r(p1)
replace yedut=r(p95) if yedu>=r(p95) 
sum yedut

* Center variables
summarize yedut, meanonly
gen yeduc = yedut - r(mean)
sum yeduc
lab var yeduc "Years in education (cent.)" 

summarize intuse, meanonly
gen intusec = intuse - r(mean)
sum intusec
lab var intusec "Internet use index (cent.)" 

*summarize climate, meanonly
*gen climatec = climate - r(mean)
*sum climatec
*lab var climatec "Climate change is a serious issue (cent.)" 

summarize skills, meanonly
gen skillsc = skills - r(mean)
sum skillsc
lab var skillsc "Digital skills (cent.)" 

summarize sumreflexive, meanonly
gen sumreflexivec = sumreflexive - r(mean)
sum sumreflexivec
lab var sumreflexivec "Reflexivity (cent.)" 

//  mean desi 47.67
gen desic = desi - 47.67 
sum desic
lab var desic "DESI (cent.)"

// mean gdp 99.92
gen gdpc = gdp - 99.92
sum gdpc
lab var gdpc "GDP pro capita (cent.)"


*** SELECTION ***



* Drop missing
global varlist sumprivacy yedut female agecat d15a_r2 intusec ///
	settlement skills responsible student unemployed
egen missno2 = rowmiss($varlist)
tab missno2
keep if missno2==0

fre cntry

*+++ TABLE 2 +++* 
sum $varlist
sum sumprivacy yedut skills responsible intuse student unemployed ///
	i.female i.agecat i.settlement 

** Distribution e-privacy *
set scheme plotplain
sum sumprivacy
global N = r(sum_w)
histogram sumprivacy, percent barw(1) d bcolor(g6*1.25) xlab(0(1)8) ///
	title("") xtitle("E-privacy management") ///
	caption("Source: Eurobarometer 87.1 | N = $N", size(small) position(5))
graph export "$save/distribution_eprivacy.emf", replace

table cntry, c(mean sumprivacy sd sumprivacy)
*+++ FIGURE 4 +++* 
sum sumprivacy
local N = r(sum_w)
local mean = r(mean)
graph bar sumprivacy, over(cntry, sort((mean)sumprivacy) label(angle(vertical)))  ///
	bar(1, color(gs9)) ///
	yline(`mean', lcolor(black)) ///
	title("") ///
	ytitle("Mean") ///
	caption("Source: Eurobarometer 87.1 | N = $N", size(small) position(5))
graph export "$save/figure4.pdf", replace
graph export "$save/figure4.eps", replace
graph export "$save/figure4.emf", replace
graph export "$save/figure4.tif", replace
graph export "$save/figure4.png", replace


save datadd.dta, replace 
 
{ // run in one go
preserve

sum yedut
global N = r(sum_w)

* recode country without gaps and sort
statsby, by(cntry) : ci mean yedut
sort mean
gen ccntry = _n
decode cntry, gen(labcntry)
labmask ccntry,  lblname(cntry) values(labcntry)
browse


** education by country 
set scheme plotplain
twoway bar mean ccntry, xla(1(1)28, angle(90) valuelabel) barw(1) bcolor(gs9)  ///
	|| rspike lb ub ccntry, pstyle(p1) legend(off) yla(10(4)22, ang(h)) ytitle("Years in education") xtitle(" ") ///
	caption("Source: Eurobarometer 87.1 | N = $N", size(small) position(5))
graph export "$save/figure2.pdf", replace
graph export "$save/figure2.eps", replace
graph export "$save/figure2.emf", replace
graph export "$save/figure2.tif", replace
graph export "$save/figure2.png", replace
restore
}

u datadd.dta, clear


* Inspect single item multilevel logistic regression to look at the effect of education
*foreach var of varlist $varprivacy {
*mixed `var' yeduc noftedu female i.generations i.d15a_r2 i.settlement intusec || cntry:,var cov(unstructured)
*}

* Individual level models
qui: mixed sumprivacy  || cntry:,var cov(unstructured)
estat icc
est store m0
di -2*e(ll)

qui: mixed sumprivacy yeduc female ib3.agecat student unemployed i.settlement || cntry:,var cov(unstructured)
est store m1
di -2*e(ll)

qui: mixed sumprivacy yeduc female ib3.agecat student unemployed i.settlement intusec || cntry:,var cov(unstructured)
est store m2
di -2*e(ll)

qui: mixed sumprivacy yeduc female ib3.agecat student unemployed i.settlement skillsc|| cntry:,var cov(unstructured)
est store m3
di -2*e(ll)

qui: mixed sumprivacy yeduc female ib3.agecat student unemployed i.settlement responsible  || cntry:,var cov(unstructured)
est store m4
di -2*e(ll)

qui: mixed sumprivacy yeduc female ib3.agecat student unemployed i.settlement intusec skillsc responsible  || cntry:,var cov(unstructured)
est store m5
di -2*e(ll)
estadd beta
matrix list e(beta)

qui: mixed sumprivacy c.yeduc##ib3.agecat female student unemployed  i.settlement intusec skillsc responsible  || cntry:,var cov(unstructured)
est store m6
di -2*e(ll)

* ++++++ TABLE 3 ++++++*
local date : di %tdDNCY daily("$S_DATE", "DMY")
esttab m0 m1 m2 m3 m4 m5 using `date'_ind_model.csv, replace wide noparentheses  ///
	star(+ 0.10 * 0.05 ** 0.01 *** 0.001) ///
	se ty l b(%4.3f) se(%4.3f) transform(ln*: (exp(@)^2) (exp(@)^2))	///
	eqlabels("" "var(_cons)" "var(Residual)") //stardrop(ln*:)
esttab m0 m1 m2 m3 m4 m5 m6, wide noparentheses  ///
		star(+ 0.10 * 0.05 ** 0.01 *** 0.001) ///
	se ty l b(%4.3f) se(%4.3f) transform(ln*: (exp(@)^2) (exp(@)^2))	///
	eqlabels("" "var(_cons)" "var(Residual)")

	
* +++ FIGURE 5 +++ *
*qui: mixed sumprivacy c.yeduc##ib3.agecat female i.settlement || cntry:,var cov(unstructured)
*margins, dydx(yeduc) at(agecat=(1(1)7)) atmeans vsquish
*marginsplot,yline(0)
/* with transparency
{
mixed sumprivacy c.yeduc##ib3.agecat female student unemployed i.settlement intusec skillsc responsible  || cntry:,var cov(unstructured)
summarize yeduc, d
local p10 `r(p10)'
local p50 `r(p50)'
local p90 `r(p90)'
local nobs `r(N)'
qui: margins, at(agecat=(1 2 3 4 5 6 7) yeduc=(`p10' `p50' `p90')) atmeans post
margins, coeflegend
set scheme s1mono
marginsplot, recastci(rarea) ///
		title ("") ///
			note("Source: Eurobarometer 87.1" ///
				"N{subscript:obs}=`nobs', N{subscript:countries}=28", pos(5) size(small)) ///
			xtitle("Age categories") ///
			name(c6, replace) ///
			ciopt(color(%30)) ///
			plot(, label("10th" "Median" "90th")) ///
			legend(row(1) pos(6) size(small) ///
				title("Years in education: percentile", size(msmall)))
 graph export "$save\figure3.pdf", replace
 graph export "$save\figure3.eps", replace 		
}
*/

*without transparency
{
mixed sumprivacy c.yeduc##ib3.agecat female student unemployed i.settlement intusec skillsc responsible  || cntry:,var cov(unstructured)
summarize yeduc, d
local p10 `r(p10)'
local p50 `r(p50)'
local p90 `r(p90)'
local nobs `r(N)'
qui: margins, at(agecat=(1 2 3 4 5 6 7) yeduc=(`p10' `p50' `p90')) atmeans post
margins, coeflegend
set scheme s1mono
marginsplot,  ///
		title ("") ///
			note("Source: Eurobarometer 87.1" ///
				"N{subscript:obs}=`nobs', N{subscript:countries}=28", pos(5) size(small)) ///
			xtitle("Age categories") ///
			name(c6, replace) ///
			plot1opts(lc(black) mc(black)) ///
			ci1opts(color(black)) /// 
			plot2opts(lc(gs10) mc(gs10)) ///
			ci2opts(color(gs10)) /// 
			plot3opts(lc(gs7) mc(gs7)) ///
			ci3opts(color(gs7)) ///
			plot(, label("10th" "Median" "90th")) ///
			legend(row(1) pos(6) size(small) ///
				title("Years in education: percentile", size(msmall)))
 graph export "$save\figure5.pdf", replace
 graph export "$save\figure5.eps", replace 
 graph export "$save\figure5.emf", replace
 graph export "$save\figure5.tif", replace 
  graph export "$save\figure5.png", replace 
}


/* plot1opts(lp(solid) lc(ply2) mc(ply2)) ///
			ci1opts(color(ply2%20)) /// 
			plot2opts(lp(solid) lc(ebblue) mc(ebblue)) ///
			ci2opts(color(ebblue%20)) /// 
			plot3opts(lp(solid) lc(pll2) mc(pll2)) ///
			ci3opts(color(pll22%20)) /// */ 


* Plot random slopes
mixed sumprivacy yeduc || cntry: yeduc, cov(unstructured)
*mixed sumprivacy yeduc female i.generations i.d15a_r2 i.settlement intusec desic  || cntry: yeduc,var cov(unstructured)
predict scorehat, fitted
sort cntry yeduc 

* +++ FIGURE++++ 
twoway line scorehat yeduc, /// //by(cntry, rows(4)) 
	connect(ascending)
graph export "$save/figurea.pdf", replace
graph export "$save/figurea.png", replace


* Country level models
mixed sumprivacy yeduc female ib3.agecat student unemployed i.settlement desic  || cntry: yeduc,var cov(unstructured)
est store m7	
mixed sumprivacy yeduc female ib3.agecat student unemployed  i.settlement desic gdpc || cntry: yeduc,var cov(unstructured)
est store m8	
mixed sumprivacy c.desic##c.yeduc student unemployed female ib3.agecat i.settlement gdpc || cntry: yeduc,var cov(unstructured)
est store m9

/*
* other two options to obtain same graph:
predict u1 u0, reffects
by cntry, sort: generate tolist = (_n==1)
list cntry u0 u1 if cntry <=10 & tolist
generate intercept = _b[_cons] + u0
generate slope = _b[yeduc] + u1
list cntry intercept slope if cntry<=10 & tolist
predict yhat, fitted
twoway connected yhat yeduc, connect(L)

predict slope1 intercept1, reffects // random effects
describe
predict yhat2, fitted
gen check = (_b[_cons] + intercept1) + (_b[yeduc] + slope1)*yeduc
list yhat2 check in 1/5
line yhat2 yeduc, connect(ascending)
*/

	
* +++ TABLE 4 ++++ 
esttab m7 m8 m9 using macro.csv, replace wide noparentheses ///
	se ty nobase l b(%4.3f) se(%6.5f) transform(ln*: exp(@)^2 exp(@)^2 ) 	///
	star(+ 0.10 * 0.05 ** 0.01 *** 0.001) ///
	eqlabels("" "var(yeduc)" "var(_cons)" "cov(yeduc,_cons)" "var(Residual)" , none) //stardrop(ln*:)
*	## remember: change covariance manually
esttab m7 m8 m9, wide noparentheses ///
	se ty l b(%4.3f) se(%6.5f) transform(ln*: exp(@)^2 exp(@)^2 ) 	///
	star(+ 0.10 * 0.05 ** 0.01 *** 0.001) ///
	eqlabels("" "var(yeduc)" "var(_cons)" "cov(yeduc,_cons)" "var(Residual)" , none) 
	
* ++++ FIGURE 6 ++++ 
{
qui: mixed sumprivacy c.desic##c.yeduc student unemployed female ib3.agecat i.settlement gdpc || cntry: yeduc,var cov(unstructured)
summarize yeduc, d
local p10 `r(p10)'
local p50 `r(p50)'
local p90 `r(p90)'
local nobs `r(N)'
qui: margins, at(desic=(-16(3)18) yeduc=(`p10' `p50' `p90')) atmeans post
margins, coeflegend
set scheme s1mono
marginsplot, recast(line)  ///
			title("") ///
			note("Source: Eurobarometer 87.1 and EC" ///
			"N{subscript:obs}=`nobs', N{subscript:countries}=28" ///
			, pos(5) size(small)) ///
			xtitle("DESI (centered)") ///
			name(c6, replace) ///
			plot(, label("10th" "Median" "90th")) ///
			plot1opts(lc(black) mc(black)) ///
			ci1opts(color(black)) /// 
			plot2opts(lc(gs8) mc(gs8)) ///
			ci2opts(color(gs8)) /// 
			plot3opts(lc(gs5) mc(gs5)) ///
			ci3opts(color(gs5)) ///
			legend(row(1) pos(6) size(small) ///
				title("Years in education: percentile", size(msmall)))
 graph export "$save/figure6.pdf", replace
 graph export "$save/figure6.eps", replace
 graph export "$save/figure6.emf", replace
 graph export "$save/figure6.tif", replace
 graph export "$save/figure6.png", replace
}




/* with transparency
{
qui: mixed sumprivacy c.desic##c.yeduc student unemployed female ib3.agecat i.settlement gdpc || cntry: yeduc,var cov(unstructured)
summarize yeduc, d
local p10 `r(p10)'
local p50 `r(p50)'
local p90 `r(p90)'
local nobs `r(N)'
qui: margins, at(desic=(-16(3)18) yeduc=(`p10' `p50' `p90')) atmeans post
margins, coeflegend
set scheme s1mono
marginsplot, recast(line) recastci(rarea) ///
			title("") ///
			note("Source: Eurobarometer 87.1 and EC" ///
			"N{subscript:obs}=`nobs', N{subscript:countries}=28" ///
			, pos(5) size(small)) ///
			xtitle("DESI (centered)") ///
			name(c6, replace) ///
			ciopt(color(%30)) ///
			plot(, label("10th" "Median" "90th")) ///
			plot1opts(lc(gs5) mc(gs5)) ///
			ci1opts(color(gs5%20)) /// 
			plot2opts(lc(black) mc(black)) ///
			ci2opts(color(black%20)) /// 
			plot3opts(lc(gs9) mc(gs9)) ///
			ci3opts(color(gs9%20)) ///
			legend(row(1) pos(6) size(small) ///
				title("Years in education: percentile", size(msmall)))
 graph export "$save/figure4.pdf", replace
 graph export "$save/figure4.eps", replace
}	
*/

** lr test | - 2LL test
 lrtest m0 m1
 lrtest m1 m2
 lrtest m1 m3
 lrtest m1 m4
 lrtest m1 m5
 lrtest m1 m6
 lrtest m1 m7
 lrtest m7 m8
 lrtest m8 m9
 
 
* pseudo r2 country level  [vc0 - vc(n)]/vc0
{
local vc0 0.371
local vc1 0.367
local vc2 0.323	
local vc3 0.289	
local vc4 0.327	
local vc5 0.243
local vc6 0.242
local vc7 0.104	
local vc8 0.100
local vc9 0.100
foreach n of numlist 1 2 3 4 5 6 7 8 9 {
di ((`vc0' - `vc`n'')/`vc0') *100
}
}
* pseudo r2 individual level
{ 
local vi0 3.232
local vi1 3.121	
local vi2 3.036
local vi3 2.948	 
local vi4 3.102	
local vi5 2.894 
local vi6 2.893	
local vi7 3.110
local vi8 3.110
local vi9 3.110
foreach n of numlist 1 2 3 4 5 6 7 8 9  {
di ((`vi0' - `vi`n'')/`vi0') *100
}
}
*
 
 
*** Supplementary materials
* selection effects
* open data: EUROBAROMETER 87.1
u ZA6861_v1-2-0.dta, clear
fre netuse

gen nointernet=0
replace nointernet=1 if netuse==6
replace nointernet=1 if netuse==7


** Copied from above:

* Education
numlabel D8 D8R1 D8R2, add force
tab1 d8 d8r1 d8r2

rename d11 age
tab age

// Add also those who still study basing on age (final variable is " years spent in education");
// I will need to later check if the impact of education varies by age categories 
// as I can expect it matters less for younger people, digital natives, and more for
// elderly people
clonevar eduage=d8

gen stillstud=eduage==98
tab stillstud 

replace eduage=age if eduage==98

list eduage d8 age in 1/100 if d8==98
recode eduage (97=.c)(99=.a)(0=.b)(2 3 4 5 = 5) 
lab def D8 .c "No FT edu" .a "DK" .b "Refusal" 2 "2" 5 "<5", modify
tab eduage

lab var eduage "Age at finishing FT education"

tab d8r2
recode d8r2 (8 = .a) (7 = .b) (5 = 1) (4= .c), gen(educat)

** Years of schooling
fre country
gen agestart=6
replace agestart=5 if country==3 | country==9 | country==25
replace agestart=7 if country==7 | country==16 | country==17 ///
		| country==21 | country==23 | country==26 | country==29 | country ==30
table country, c(mean agestart)

gen yedu=eduage-agestart
recode yedu (-2 -1 0 = 1)
replace yedu=0 if eduage==.c
lab def yedu 0 "No ft education"
lab val yedu yedu
tab yedu 

lab var yedu "Years of schooling"

recode yedu (0=0 "No ft education") (1/7=7 "7 years or less") (8=8 "8 years") ///
	(9=9 "9 years") (10=10 "10 years") ///
	(11=11 "11 years") (12=12 "12 years") (13=13 "13 years") ///
	(14=14 "14 years") (15=15 "15 years") (16=16 "16 years") ///
	(17=17 "17 years") (18=18 "18 years") (19=19 "19 years") ///
	(20/max=20 "20 years or more"), gen(yedu2)
	
* Cohorts
gen yborn=2016-age
sum yborn

gen agecat=.
lab def agecat ///
 1 "15-24" ///
 2 "25-34" ///
 3 "35-44" ///
 4 "45-54" ///
 5 "55-64" ///
 6 "65-74" ///
 7 "75+" , modify

sum age 
 
replace agecat=1 if age<=24
replace agecat=2 if age>24 & age<35
replace agecat=3 if age>34 & age<45
replace agecat=4 if age>44 & age<55
replace agecat=5 if age>54 & age<65
replace agecat=6 if age>64 & age<75
replace agecat=7 if age>74

lab val agecat agecat
lab var agecat "Age categories"
tab agecat 
 
* Occupation
fre d15a d15a_r1 d15a_r2 d15b d15b_r

// make dummies for students and unemployed
gen student=0
replace student=1 if d15a_r2==8
lab var student "Student (full time)"
gen unemployed = d15a_r2==7
lab var unemployed "Unemployed"

* Sex
fre d10
gen female= d10==2
fre female
lab var female "Female"

* Settlement
fre d25
recode d25 (8=.)
rename d25 settlement
lab var settlement "Type of settlement"

* Truncate outliers
gen noftedu = yedu==0
tab noftedu
tab stillstud

sum yedu, de
gen yedut = yedu 
*replace yedut=r(p1) if yedu<=r(p1)
replace yedut=r(p95) if yedu>=r(p95) 
sum yedut

* Center variables
summarize yedut, meanonly
gen yeduc = yedut - r(mean)
sum yeduc
lab var yeduc "Years in education (cent.)" 

* country
fre country
replace country=9 if country==10 // reunite GB
replace country=4 if country==14 // renunite DE
label def COUNTRY 9 "UK - United Kingdom" 4 "DE - Germany", modify
fre country
rename country cntry

bys cntry: table nointernet
sum nointernet
*return list
local N2 = r(N)
local mean = r(mean)
set scheme plotplain
graph bar nointernet, over(cntry, sort((mean)nointernet) label(angle(vertical)))  ///
	bar(1, color(gs9)) ///
	yline(`mean') ///
	title("") ///
	ytitle("Proportion") ///
	caption("Source: Eurobarometer 87.1 | N = `N2'", size(small) position(5))
graph export "$save/figure_a1.pdf", replace
graph export "$save/figure_a1.eps", replace
graph export "$save/figure_a1.emf", replace
graph export "$save/figure_a1.tif", replace
graph export "$save/figure_a1.png", replace

egen miss = rowmiss(nointernet yeduc female agecat unemployed student settlement)
fre miss

***
drop if miss==1
***

***
save data_appendix.dta, replace
merge m:m cntry using desitomerge.dta
***


//  mean desi 47.67
gen desic = desi - 47.67 
sum desic
lab var desic "DESI (cent.)"

logit nointernet yeduc female ib3.agecat unemployed student i.settlement, or

mixed nointernet ///
		|| cntry: ,var 
estat icc
di -2*e(ll)


mixed nointernet yeduc female ib3.agecat unemployed student i.settlement ///
		|| cntry: yeduc ,var cov(unstructured)
est sto a1
di -2*e(ll)
esttab a1, wide noparentheses ///
	se ty l b(%4.3f) se(%6.5f) transform(ln*: exp(@)^2 exp(@)^2 ) 	///
	star(+ 0.10 * 0.05 ** 0.01 *** 0.001) ///
	eqlabels("" "var(yeduc)" "var(_cons)" "cov(yeduc,_cons)" "var(Residual)" , none) 
	

mixed nointernet c.yeduc##c.desic female ib3.agecat unemployed student i.settlement ///
		|| cntry: yeduc ,var cov(unstructured)
est sto a2
di -2*e(ll)
esttab a2, wide noparentheses ///
	se ty l b(%4.3f) se(%6.5f) transform(ln*: exp(@)^2 exp(@)^2 ) 	///
	star(+ 0.10 * 0.05 ** 0.01 *** 0.001) ///
	eqlabels("" "var(yeduc)" "var(_cons)" "cov(yeduc,_cons)" "var(Residual)" , none) 	
summarize yeduc, d
local nobs = r(N)
local p10 `r(p10)'
local p50 `r(p50)'
local p90 `r(p90)'
local nobs `r(N)'
qui: margins, at(desic=(-16(3)18) yeduc=(`p10' `p50' `p90')) atmeans post
margins, coeflegend
set scheme s1mono
marginsplot, recast(line)  ///
			title("") ///
			note("Source: Eurobarometer 87.1 and EC" ///
			"N{subscript:obs}=`nobs', N{subscript:countries}=28" ///
			, pos(5) size(small)) ///
			xtitle("DESI (centered)") ///
			name(c6, replace) ///
			plot(, label("10th" "Median" "90th")) ///
			plot1opts(lc(black) mc(black)) ///
			ci1opts(color(black)) /// 
			plot2opts(lc(gs8) mc(gs8)) ///
			ci2opts(color(gs8)) /// 
			plot3opts(lc(gs5) mc(gs5)) ///
			ci3opts(color(gs5)) ///
			legend(row(1) pos(6) size(small) ///
				title("Years in education: percentile", size(msmall)))	

graph export "$save/figure_a2.pdf", replace
graph export "$save/figure_a2.eps", replace
graph export "$save/figure_a2.emf", replace
graph export "$save/figure_a2.tif", replace
graph export "$save/figure_a2.png", replace
	
	
		
xtmelogit nointernet yeduc female ib3.agecat unemployed student i.settlement ///
		|| cntry: ,var 

		
		
		
* alternative reflexive mindset

u datadd.dta, replace

fre qa10_1 qa10_2 qa10_3 qa10r_1 qa10r_2 qa10r_3 qa10r_4 qa10r_5 qa10r_6 qa10r_7

gen instknow=.


replace instknow=0 if qa10r_2 ==1 // no correct answer
replace instknow=1 if qa10r_3 ==1 //  1 correct
replace instknow=2 if qa10r_4 ==1 //  2 correct
replace instknow=3 if qa10r_5 ==1 //  3 correct
fre instknow

lab var instknow "Institutional knowledge"
sum instknow

pwcorr yedut instknow, sig

mixed sumprivacy yeduc female ib3.agecat student unemployed i.settlement instknow  || cntry:,var cov(unstructured)
est store m4alt
di -2*e(ll)

mixed sumprivacy yeduc female ib3.agecat student unemployed i.settlement intusec skillsc instknow  || cntry:,var cov(unstructured)
est store m5alt
di -2*e(ll)
estadd beta
matrix list e(beta)

* LR test
 lrtest m1 m4alt
 lrtest m1 m5alt
 
* pseudo r2
{
local vc0 0.371
local vc4 0.364
local vc5 0.270
foreach n of numlist 4 5 {
di ((`vc0' - `vc`n'')/`vc0') *100
}
}
* pseudo r2 individual level
{ 
local vi0 3.232
local vi4 3.094	
local vi5 2.895 
foreach n of numlist  4 5  {
di ((`vi0' - `vi`n'')/`vi0') *100
}
}
*