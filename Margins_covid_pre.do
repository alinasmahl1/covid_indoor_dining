use "county_cases2.dta", clear
*turn outcome into rate
xtset FIPS time
egen double_cluster=group(FIPS Province_State)
*run final model
nbreg daily_count i.treat1##i.pre_post i.at_home i.mask_mandate i.evict_end, exposure(pop) irr vce(cluster double_cluster)
* get average marginal effect in the average city=average daily cases averted
margins r.treat1#r.pre_post
* get the total effect in all cities
* first, replace pop by total pop
replace pop=20963027
* re-run margins
margins r.treat1#r.pre_post
matrix b=r(b)
matrix V=r(V)
local est=b[1,1]*28
local lci=(b[1,1]+1.96*sqrt(V[1,1]))*28
local uci=(b[1,1]-1.96*sqrt(V[1,1]))*28
di `est' " (" `lci' " to " `uci' ")"
* calculate per 100,000
 di -39384.737/20963027
di -.00187877*100000
