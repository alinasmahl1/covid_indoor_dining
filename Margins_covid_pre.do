
*load file
use "/Users/2/Downloads/county_cases2.dta"

*turn outcome into rate 


*run final model
xtset FIPS time

* make pop offset logged

gen pop1 = log(pop/100000)
gen pop2=log(pop)

nbreg daily_count i.treat1##i.pre_post i.at_home i.mask_mandate i.evict_end, offset(pop1) irr vce(robust) 


*marignal effects at means 
*get hte marginal effects and calculate dif in dif by hand 
margins treat1#pre_post, atmeans

* to estimate the "interaction effect" at the means, w. CI"s, or the marginal effect at the mean 
margins r.treat1#r.pre_post, atmeans
*caculuate the toatl number over 4 weeks as -87* 4(treatment cities) * 28 (#of post days)=10,416
display 87*4*28

*average marginal effects (treat all obs as treatment, then all obs as control, and then the difference in the two probabilities just computed is the marginal effect

* coefficent for treat1. is our average marginal effect. see (https://www3.nd.edu/~rwilliam/stats/Margins01.pdf)

margins pre_post, dydx(treat1)
* 163.234-72.06= 91.17 *11 cities *28 days (study period)= 28081.592 (round up to 28,082)
display 163.234-72.06
display 91.174*11
display 1002.914 *28

*alternative way to calculate 
margins, dydx(pre_post) at(treat=(1 0))
 display -70.96434-20.20651 =-91.17085
 
 *for per 100,000 calculation take 39384/pop total pop=20,963,027 *100,000=187.877
 di -39384.737/20963027
di -.00187877*100000
