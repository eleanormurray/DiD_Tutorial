/*************************************************************/
/**********DID Tutorial: Rothbard et al, 2022*****************/
/**********Main Tutorial CODE*********************************/
/**********Stata Code by Sarah Rothbard & Eleanor J Murray****/
/*************************************************************/

/******************************************/
/*Code Section 0: Data set up & formatting*/
/******************************************/


use "C:\Users\ejmurray\Dropbox\ProjectManagement\Collaborations\AriadneLabs\SafeSurgery-DBT\MethodsPaper\Final\did_sim.dta", clear

label define postfmt 0 "Pre-DBT"  1 "Post-DBT"
label define dbtfmt 0 "Control" 1 "Intervention"
label define timefmt -12 "Pre-COVID" 0 "Baseline" 1 "Follow-up"
label define stratafmt 11 "Sealer, Very Challenging"        12 "Sealer, Somewhat challenging"       13  "Sealer, Not challenging"   21 "Stapler, Very challenging"  22 "Stapler, Somewhat challenging"      23  "Stapler, Not challenging"  31  "Other device, Very challenging"  32 "Other device,Somewhat challenging"	33 "Other device,Not challenging"
label define covidfmt 0 "Pre-COVID" 1 "Post-COVID"
					  
label values post_dbt postfmt
label values dbt_grp dbtfmt
label values time timefmt
label values post_covid covidfmt

/*Get familiarized with the data*/
describe
summarize case

summarize tot_notechs
tab post_dbt 
tab dbt_grp


/******************************************/
/*Code Section 1: Checking Assumptions*****/
/******************************************/
/*Calculate average outcome value in control & intervention departments in each baseline time*/
by dbt_grp post_covid, sort: summarize tot_notechs if post_dbt==0

/*Calculate the change in average value between baseline timepoints & control / intervention departments*/
reg tot_notechs i.post_covid##i.dbt_grp if time <= 0


/*****************************************************/
/*CODE SECTION 2: Unadjusted models******************/
/***************************************************/
/*Code 2.1: Unadjusted linear regression, excluding pre-COVID data*/
reg tot_notechs time i.post_dbt##i.dbt_grp if time >=0, cluster(dept)

/*Optional Code 2.1.2: Create table of predicted outcome values & DiD*/
/*Note, that the margins command performs many different linear predictor tasks. */
/*The code below is correct for standardization of our DiD estimates for this example but may not be appropriate for other examples. */
/*The code below also does not output the robust standard errors*/
/*Use margins with caution!*/
quietly margins post_dbt#dbt_grp if time >=0, at(time = (0 1)) post
margins, coeflegend
matrix input lin_unadj = (.,. ,. ,. ,. )
matrix lin_unadj = (_b[1bn._at#0bn.post_dbt#1.dbt_grp], _b[2._at#1.post_dbt#1.dbt_grp],.)
matrix lin_unadj = (lin_unadj  \  _b[1bn._at#0bn.post_dbt#0bn.dbt_grp], _b[2._at#1.post_dbt#0bn.dbt_grp],  ((_b[2bn._at#1bn.post_dbt#1.dbt_grp]-_b[1bn._at#0bn.post_dbt#1.dbt_grp])-(_b[2bn._at#1bn.post_dbt#0bn.dbt_grp]- _b[1bn._at#0bn.post_dbt#0bn.dbt_grp])))
matrix rownames lin_unadj = Intervention Control 
matrix colnames lin_unadj = Pre-DBT Post-DBT DiD
matrix list lin_unadj


/*Code 2.2: Unadjusted linear regression, using all data*/
reg tot_notechs time i.post_dbt##i.dbt_grp, cluster(dept)
/*Optional Code 2.2.2: Create table of predicted outcome values & DiD*/
/*Note that the addition of time to the model requires a modified margins command*/
margins post_dbt#dbt_grp if time >=0, at(time = (0 1)) post
margins, coeflegend
matrix input lin_unadj2 = (.,. ,. ,. ,. )
matrix lin_unadj2 = (_b[1bn._at#0bn.post_dbt#1.dbt_grp], _b[2._at#1.post_dbt#1.dbt_grp],.)
matrix lin_unadj2 = (lin_unadj2  \  _b[1bn._at#0bn.post_dbt#0bn.dbt_grp], _b[2._at#1.post_dbt#0bn.dbt_grp],  ((_b[2bn._at#1bn.post_dbt#1.dbt_grp]-_b[1bn._at#0bn.post_dbt#1.dbt_grp])-(_b[2bn._at#1bn.post_dbt#0bn.dbt_grp]- _b[1bn._at#0bn.post_dbt#0bn.dbt_grp])))
matrix rownames lin_unadj2 = Intervention Control 
matrix colnames lin_unadj2 = Pre-DBT Post-DBT DiD
matrix list lin_unadj2


/*Code 2.3: Unadjusted Poisson model using all timepoints*/
 poisson tot_notechs time i.post_dbt##i.dbt_grp, irr
/*Code 2.3.2: Create table of predicted outcome values & DiD*/
/*Note, we cannot get the DiD directly from the regression coefficents*/
/*This code is required*/
margins  post_dbt#dbt_grp if time >=0, at(time = (0 1)) post
margins, coeflegend
matrix input pois_unadj = (. ,. ,. ,. )
matrix pois_unadj = (_b[1bn._at#0bn.post_dbt#1.dbt_grp], _b[2._at#1.post_dbt#1.dbt_grp],.)
matrix  pois_unadj = ( pois_unadj \  _b[1bn._at#0bn.post_dbt#0bn.dbt_grp], _b[2._at#1.post_dbt#0bn.dbt_grp],  ((_b[2bn._at#1bn.post_dbt#1.dbt_grp]-_b[1bn._at#0bn.post_dbt#1.dbt_grp])-(_b[2bn._at#1bn.post_dbt#0bn.dbt_grp]- _b[1bn._at#0bn.post_dbt#0bn.dbt_grp])))
matrix rownames  pois_unadj= Intervention Control 
matrix colnames  pois_unadj = Pre-DBT Post-DBT DiD
matrix list  pois_unadj


/***************************************************/
/*Code Section 3: Adjusted Linear Models***********/ 
/*************************************************/
/*Code 3.1: adjusted linear regression model*/
/*Conditional Difference-in-Difference, holding confounders constant*/
reg tot_notechs time ib(last).post_dbt##ib(last).dbt_grp ib(last).case_challenge##ib(last).device, cluster(dept)

/*Optional Code 3.1.1: Output conditional DiD by strata*/
gen strata = device*10 + case_challenge
label values strata stratafmt
tab strata

reg tot_notechs time ib(last).post_dbt##ib(last).dbt_grp ib(last).case_challenge##ib(last).device, cluster(dept)
predict e_notechs if time >=0
local j 1
matrix input cDiD = (., ., ., ., ., ., ., .)
matrix colnames  cDiD= Stratum Pre-Cntrl Pre-Interv Post-Cntrl Post-Interv dInterv dControl cDiD
forvalues i = 1/3{
	forvalues k = 1/3{
		local j = `j'+1
		local s = `i'*10+`k'
		summarize e_notechs if (post_dbt == 0 & dbt_grp ==0 & strata == `s')
		matrix  cDiD = (cDiD \ `i'*10+`k', `r(mean)', ., ., ., ., ., .)
		summarize e_notechs if (post_dbt == 0 & dbt_grp ==1 & strata == `s')
		matrix  cDiD[`j',3] = `r(mean)'
		summarize e_notechs if (post_dbt == 1 & dbt_grp ==0 & strata == `s')
		matrix  cDiD[`j',4] = `r(mean)'
		summarize e_notechs if (post_dbt == 1 & dbt_grp ==1 & strata == `s')
		matrix  cDiD[`j',5] = `r(mean)'
		matrix cDiD[`j',6] = cDiD[`j',5]- cDiD[`j',4] 
		matrix cDiD[`j',7] = cDiD[`j',3]- cDiD[`j',2] 
		matrix cDiD[`j',8] = cDiD[`j',6]- cDiD[`j',7] 
	}
}
matrix list cDiD


/*Code 3.2: Standardizing the estimated outcome to obtain a Marginal DiD estimator*/
/*Option 1: the long way! Calculating the stratum-specific probabilities & standardizing by hand*/
*run 3.1.1 to create stratum-specific estimates, then run the code below to get strata distributions*
matrix list cDiD
tab case_challenge device if post_covid ==1, cell

/*Code 3.3: Standardizing the estimated outcome to obtain a Marginal DiD estimator*/
/*Option 2: the short way! Using Stata's built in margins command*/
/*Note that we need to fix time and post_covid to match the stratum-assignment of the copy*/
reg tot_notechs time ib(last).post_dbt##ib(last).dbt_grp ib(last).case_challenge##ib(last).device, cluster(dept)
margins  post_dbt#dbt_grp if time >=0, at(time = (0 1)) post
margins, coeflegend
matrix input lin_adj = (.,. ,. ,. ,. )
matrix lin_adj = (_b[1bn._at#0bn.post_dbt#1.dbt_grp], _b[2._at#1.post_dbt#1.dbt_grp],.)
matrix lin_adj = (lin_adj \  _b[1bn._at#0bn.post_dbt#0bn.dbt_grp], _b[2._at#1.post_dbt#0bn.dbt_grp],  ((_b[2bn._at#1bn.post_dbt#1.dbt_grp]-_b[1bn._at#0bn.post_dbt#1.dbt_grp])-(_b[2bn._at#1bn.post_dbt#0bn.dbt_grp]- _b[1bn._at#0bn.post_dbt#0bn.dbt_grp])))
matrix rownames lin_adj = Intervention Control
matrix colnames lin_adj = Pre-DBT Post-DBT DiD
matrix list lin_adj

/*Code 3.4: Standardizing the estimated outcome to obtain a Marginal DiD estimator*/
/*Option 3: the super short way! Using copies of the data*/
use "C:\Users\ejmurray\Dropbox\ProjectManagement\Collaborations\AriadneLabs\SafeSurgery-DBT\MethodsPaper\Final\did_sim.dta", clear

*i.Data set up for standardization: create 6 copies of each subject*
*first, duplicate the dataset and create a variable 'interv' which indicates which copy is the duplicate (interv =1)
expand 2 if time >=0, generate(interv)
*next, duplicate the original copy (interv = 0) again, and create another variable 'interv2' to indicate the copy
expand 2 if interv == 0 & time >=0, generate(interv2)
replace interv = -1 if interv == 0 & interv2 == 0
expand 2 if interv == 0 & time >=0, generate(interv3)
replace interv = 2 if interv == 0 & interv3 == 1
expand 2 if interv == 0 & time >=0, generate(interv4)
replace interv =3 if interv == 0 & interv4 == 1


tab interv time
drop interv2 interv3 interv4 
*6 of the copies will be for computing the standardized result*
*interv = -1 is the original data, all other values will have new exposure & missing outcome values*/
*you may need to edit this part of the code for your outcome and exposure variables*
replace tot_notechs = . if interv != -1
replace post_dbt = 0 if inlist(interv, 0, 1)  
replace post_dbt = 1 if inlist(interv, 2, 3)
replace dbt_grp = 0 if inlist(interv,0,2)
replace dbt_grp = 1 if inlist(interv,1,3)
replace time = 0 if   inlist(interv, 0,1)
replace time = 1 if   inlist(interv, 2,3)
*check that the data has the structure you want: for interv = -1 exposures & time should be as distributed in the data*/
/*For all other interv, frequency should be the total sample size*/
by post_dbt dbt_grp time, sort: tab interv
 
/*Adjusted linear regression*/
reg tot_notechs time ib(last).post_dbt##ib(last).dbt_grp ib(last).case_challenge##ib(last).device, cluster(dept)
predict predY_adj if time >=0, xb
*Output standardized point estimates and difference*
quietly summarize predY_adj if(interv == 1)
matrix input lin_stdz = (`r(mean)', ., .)
quietly summarize predY_adj if(interv == 3)
matrix lin_stdz[1,2] = `r(mean)'
quietly summarize predY_adj if(interv == 0)
matrix lin_stdz= (lin_stdz \ `r(mean)' , ., .)
quietly summarize predY_adj if(interv == 2)
matrix lin_stdz[2,2] = `r(mean)'
matrix lin_stdz[1,3] = ((lin_stdz[1,2]-lin_stdz[1,1])-(lin_stdz[2,2]-lin_stdz[2,1]))
matrix rownames  lin_stdz=  Intervention Control
matrix colnames  lin_stdz =  Pre-DBT Post-DBT DiD
matrix list lin_stdz




/********************************************************************************/
/********Code Section A3: Adjusted Poisson regression****************************/
/********************************************************************************/
use "C:\Users\ejmurray\Dropbox\ProjectManagement\Collaborations\AriadneLabs\SafeSurgery-DBT\MethodsPaper\Final\did_sim.dta", clear
/*Code Section A3.1: Adjusted Poisson regression with standaridzation using margins command*/
poisson tot_notechs time ib(last).post_dbt##ib(last).dbt_grp ib(last).case_challenge##ib(last).device, cluster(dept) irr
margins  post_dbt#dbt_grp if time >=0, at(time = (0 1)) post
margins, coeflegend
matrix input pois_adj = (.,. ,. ,. ,. )
matrix pois_adj = (_b[1bn._at#0bn.post_dbt#1.dbt_grp], _b[2._at#1.post_dbt#1.dbt_grp],.)
matrix  pois_adj = ( pois_adj \  _b[1bn._at#0bn.post_dbt#0bn.dbt_grp], _b[2._at#1.post_dbt#0bn.dbt_grp],  ((_b[2bn._at#1bn.post_dbt#1.dbt_grp]-_b[1bn._at#0bn.post_dbt#1.dbt_grp])-(_b[2bn._at#1bn.post_dbt#0bn.dbt_grp]- _b[1bn._at#0bn.post_dbt#0bn.dbt_grp])))
matrix rownames  pois_adj= Intervention Control
matrix colnames  pois_adj =  Pre-DBT Post-DBT DiD
matrix list  pois_adj

/*Code Section A3.2: Conditional DiD from Poisson regression model*/
/*Conditional Difference-in-Difference, holding confounders constant*/
gen strata = device*10 + case_challenge
label values strata stratafmt
tab strata
quietly poisson tot_notechs time ib(last).post_dbt##ib(last).dbt_grp ib(last).case_challenge##ib(last).device, cluster(dept) irr
predict e_notechs if time >=0
local j 1
matrix input cDiD_pois = (., ., ., ., ., ., ., .)
matrix colnames  cDiD_pois= Stratum Pre-Cntrl Pre-Interv Post-Cntrl Post-Interv dInterv dControl cDiD
forvalues i = 1/3{
	forvalues k = 1/3{
		local j = `j'+1
		local s = `i'*10+`k'
		summarize e_notechs if (post_dbt == 0 & dbt_grp ==0 & strata == `s')
		matrix  cDiD_pois = (cDiD_pois \ `i'*10+`k', `r(mean)', ., ., ., ., ., .)
		summarize e_notechs if (post_dbt == 0 & dbt_grp ==1 & strata == `s')
		matrix  cDiD_pois[`j',3] = `r(mean)'
		summarize e_notechs if (post_dbt == 1 & dbt_grp ==0 & strata == `s')
		matrix  cDiD_pois[`j',4] = `r(mean)'
		summarize e_notechs if (post_dbt == 1 & dbt_grp ==1 & strata == `s')
		matrix  cDiD_pois[`j',5] = `r(mean)'
		matrix cDiD_pois[`j',6] = cDiD_pois[`j',5]- cDiD_pois[`j',4] 
		matrix cDiD_pois[`j',7] = cDiD_pois[`j',3]- cDiD_pois[`j',2] 
		matrix cDiD_pois[`j',8] = cDiD_pois[`j',6]- cDiD_pois[`j',7] 
	}
}
matrix list cDiD_pois



/*Code A3.3: Standardizing the estimated outcome to obtain a Marginal DiD estimator*/
use "C:\Users\ejmurray\Dropbox\ProjectManagement\Collaborations\AriadneLabs\SafeSurgery-DBT\MethodsPaper\Final\did_sim.dta", clear

*i.Data set up for standardization: create 6 copies of each subject*
*first, duplicate the dataset and create a variable 'interv' which indicates which copy is the duplicate (interv =1)
expand 2 if time >=0, generate(interv)
*next, duplicate the original copy (interv = 0) again, and create another variable 'interv2' to indicate the copy
expand 2 if interv == 0 & time >=0, generate(interv2)
replace interv = -1 if interv == 0 & interv2 == 0
expand 2 if interv == 0 & time >=0, generate(interv3)
replace interv = 2 if interv == 0 & interv3 == 1
expand 2 if interv == 0 & time >=0, generate(interv4)
replace interv =3 if interv == 0 & interv4 == 1

tab interv time
drop interv2 interv3 interv4 
*6 of the copies will be for computing the standardized result*
*interv = -1 is the original data, all other values will have new exposure & missing outcome values*/
*you may need to edit this part of the code for your outcome and exposure variables*
replace tot_notechs = . if interv != -1
replace post_dbt = 0 if inlist(interv, 0, 1)  
replace post_dbt = 1 if inlist(interv, 2, 3)
replace dbt_grp = 0 if inlist(interv,0,2)
replace dbt_grp = 1 if inlist(interv,1,3)
replace time = 0 if   inlist(interv, 0,1)
replace time = 1 if   inlist(interv, 2,3)
*check that the data has the structure you want: for interv = -1 exposures & time should be as distributed in the data*/
/*For all other interv, frequency should be the total sample size*/
by post_dbt dbt_grp time, sort: tab interv
 
 
/*Adjusted linear regression*/
poisson tot_notechs time ib(last).post_dbt##ib(last).dbt_grp ib(last).case_challenge##ib(last).device, cluster(dept) irr
predict predY_p if time >=0
*Output standardized point estimates and difference*
quietly summarize predY_p if(interv == 1)
matrix input pois_stdz = (`r(mean)', ., .)
quietly summarize predY_p if(interv == 3)
matrix pois_stdz[1,2] = `r(mean)'
quietly summarize predY_p if(interv == 0)
matrix pois_stdz= (pois_stdz \ `r(mean)' , ., .)
quietly summarize predY_p if(interv == 2)
matrix pois_stdz[2,2] = `r(mean)'
matrix pois_stdz[1,3] = ((pois_stdz[1,2]-pois_stdz[1,1])-(pois_stdz[2,2]-pois_stdz[2,1]))
matrix rownames  pois_stdz=  Intervention Control
matrix colnames  pois_stdz=  Pre-DBT Post-DBT DiD
matrix list pois_stdz
