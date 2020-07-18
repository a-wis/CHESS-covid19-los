
/////    
/////    
/////  Shryane et al. (2020). Length of Stay in ICU of Covid-19 Patients in England     
/////                            March - May 2020
/////    
/////   
// Authors: Nick Shryane & Maria Pampaka

////////////////////////////////////////////////////////////////////////////////
//     Data
//
//   The CHESS data used in the study above are not publicly available. 
//   The code below is made available to allow readers to evaluate the way
//    the data was coded and analysed. 

////////////////////////////////////////////////////////////////////////////////
//     Covariates

// sex
tab female, miss // Dummy variable for female

//// age
tab ageyear4     // 4-category variable for age

//// ICU policy change/admission period 
gen icucat = icupolicychange  // 3-category variable for admission period

/// ECMO dummy variable
gen ecmo = respiratorysupportecmo  
recode ecmo (2=1) (missing=0)      // dummy variable for ECMO ventilation

//// Health Risk scores (continuous variable, high = many comorbid conditions)
sum RaschComor
gen risk_c = RaschComor - r(mean) // mean-centre the risk scores 

//// ethnicity
tab ethnicity5 // 5-category variable for ethnicity


////////////////////////////////////////////////////////////////////////////////
//     Outcomes

tab dateadmittedicu   // The date the patient was admitted to ICU (if known). 
                      // Used to computethe ICU duration.
tab dateleavingicu    // The date the patient left ICU (if known).	
					  // Used to computethe ICU duration.
tab finaloutcomedate  // The date of patient's final outcome (if known): 
                      //  death/discharge/still_on_ICU. Used to compute end of 
					  //  icu stay if dateleavingicu is unavailable.
					
////////////////////////////////////////////////////////////////////////////////
////   NHS Trust completeness
////  
////  Data were obtained from 96 English NHS trusts. The code below computes 
////  summary information for each trust, to evaluate how much of the key data
////  were missing. We intended to include data from NHS trusts with 'good' data
////  quality, i.e. low missingness. 

// create numerical trustid
encode trustcode, gen(trustid)
egen trustidtag = tag(trustid)
tab trustid if trustidtag
// 96 trusts

// generate trust data completeness variables 

// generate total observations per trust: trust_tot = ICU patients per trust.
sort trustid caseid obsid
bysort trustid: gen trust_n = _n
bysort trustid: egen trust_tot = max(trust_n)
tab trust_tot if trustidtag == 1
// three trusts with only one case. 90%ile is 96 cases, max is 173.

// completeness of data by trust for: 	dateadmittedicu
sort trustid caseid obsid
bysort trustid: egen icuadmit_obs = count(dateadmittedicu)
gen icuadmit_prop = icuadmit_obs / trust_tot
sum icuadmit_prop if trustidtag == 1, det
// 5%ile = 0.106. 10%ile = 0.316, median = 1

// completeness of data by trust for:  	dateleavingicu
sort trustid caseid obsid
bysort trustid: egen iculeave_obs = count(dateleavingicu)
gen iculeave_prop = iculeave_obs / trust_tot
sum iculeave_prop if trustidtag == 1, det
// 5%ile = 0, 10%ile = 0, median = 0.5

/// completeness of data by trust for:  finaloutcomedate
sort trustid caseid obsid
bysort trustid: egen icuoutcome_obs = count(finaloutcomedate)
gen icuoutcome_prop = icuoutcome_obs / trust_tot
sum icuoutcome_prop if trustidtag == 1, det
// 5%ile = 0, 10%ile = 0.041, median = 0.815

// Label trusts as having great, good, or poor data completeness. 
// poor  = icuadmit_prop < .5 & icuoutcome_prop < .5 & iculeave_prop < .5 
// good  = as above but > .5 and < .75
// great = as above but > .75

gen trust_good = cond(icuadmit_prop < .5 & icuoutcome_prop < .5 & iculeave_prop < .5, 0, 1)
tab trust_good if trustidtag == 1  // 59 trusts are good 

gen trust_great = cond(icuadmit_prop > .75 & icuoutcome_prop > .75 & iculeave_prop > .75, 1, 0)
tab trust_great if trustidtag == 1 // 25 trusts are great, therefore 12 trusts are not good. 

gen trust_poor = 1 - trust_good
tab trust_poor if trustidtag == 1

/// Ternery variable for trust quality (0 poor, 1 good, 2 great)
gen trustqualcat = 0
replace trustqualcat = 1 if trust_good == 1 
replace trustqualcat = 2 if trust_great == 1 


////////////////////////////////////////////////////////////////////////////////
//      
//							AFT Models
//
////////////////////////////////////////////////////////////////////////////////

//Models with no ethnicity

// Declare data as survival durations (failure = observed to leave icu (through
//  death or discharge). Only cases from trusts with at least 'good' data quality,
//  and patients aged 18 or over, were included.
 
stset durationiculeavingicu, failure(dummy_outcome_icu = 1) if(trust_good==1 & ageyear > 17)
stdescribe
// N = 3406, censored = 162; Mean LoS 13.2

/////// Models assuming WEIBULL baseline hazard

// 1_1 Main effects only (no interactions)
streg female b3.icucat b2.ageyear4 risk_c ecmo, distribution(weibull) time vce(cluster trustid)
estat ic 
est store w11
predict los11w_mean, mean
predict los11w_median

// 1_2 Main effects + interactions
streg ecmo female b3.icucat##b2.ageyear4 b3.icucat##c.risk_c, distribution(weibull) time vce(cluster trustid)
estat ic 
est store w12
predict los12w_mean, mean
predict los12w_median

/////// Models assuming LOG-NORMAL baseline hazard

// 1_1 Main effects 
streg female b3.icucat b2.ageyear4 risk_c  ecmo, distribution(lognormal) time vce(cluster trustid)
estat ic 
est store ln11
predict los11logn_mean, mean
predict los11logn_median

// 1_2 Main effects + interactions
streg ecmo female b3.icucat##b2.ageyear4 b3.icucat##c.risk_c, distribution(lognormal) time vce(cluster trustid)
estat ic 
est store ln12
predict los12ln_mean, mean
predict los12ln_median

////////////////////////////////////////////////////////////////////////////////
//   
//						Model results
//
////////////////////////////////////////////////////////////////////////////////

// Comparison of model AIC
est table w11 w12 ln11 ln12, stats(aic)

// comparison of predictions from the Weibull and log-normal models
sum los11w_mean los11w_median los11logn_mean los11logn_median, det  // log-normal about a day longer

//  The Main effects, log-normal model was preferred according to the AIC
//  The results below show the effects of the significant predictors for the 
//   preferred, log-normal, main effects model
 
table ageyear4 icucat,  c(mean los11logn_mean sd los11logn_mean n los11logn_mean) col row
table ageyear4 icucat,  c(mean los11logn_median sd los11logn_median n los11logn_median) col row

// for comparison, the results from the Weibull model
table ageyear4 icucat,  c(mean los11w_mean sd los11w_mean n los11w_mean) col row
table ageyear4 icucat,  c(mean los11w_median sd los11w_median n los11w_median) col row


// Mean LoS by age group
graph twoway (kdensity los11logn_mean if ageyear4 == 1, bwidth(.8) ) ///
             (kdensity los11logn_mean if ageyear4 == 2, bwidth(.8) ) ///
			 (kdensity los11logn_mean if ageyear4 == 3, bwidth(.8) ) ///
			 (kdensity los11logn_mean if ageyear4 == 4, bwidth(.8) ) ///
			 , scheme(sj) xtitle("LoS (Days)") ytitle(Density)       ///
			 legend(label(1 "Age: <50") label(2 "Age: 50-64")        ///
			 label(3 "Age: 65-74") label(4 "Age: 75+")) 			 ///
			 xlabel(0 10 20 30) ylabel(0 .1 .2 .3)

// Mean LoS by admission period
graph twoway (kdensity los11logn_mean if icucat == 1, bwidth(.8) ) ///
             (kdensity los11logn_mean if icucat == 2, bwidth(.8) ) ///
			 (kdensity los11logn_mean if icucat == 3, bwidth(.8) ) ///
			 , scheme(sj) xtitle("LoS (Days)") ytitle(Density)     ///
			 legend(label(1 "Admission before March 25") label(2 "Admission March 25 to April 7") ///
			 label(3 "Admission after April 7")) xlabel(0 10 20 30) ylabel(0 .1 .2 .3)

// Mean LoS by ecmo
graph twoway (kdensity los11logn_mean if ecmo == 0, bwidth(.8) ) ///
             (kdensity los11logn_mean if ecmo == 1, bwidth(.8) ) ///
			 , scheme(sj) xtitle("LoS (Days)") ytitle(Density)       ///
			 legend(label(1 "non ECMO") label(2 "ECMO"))        ///
			  xlabel(0 10 20 30) ylabel(0 .1 .2 .3)		 
			 
// Mean LoS by Models with and without ethnicity
graph twoway (kdensity los11logn_mean , bwidth(.8) ) ///
             (kdensity loslnE_mean, bwidth(.8) ) ///
			 , scheme(sj) xtitle("LoS (Days)") ytitle(Density)       ///
			 legend(label(1 "Model without ethnicity") label(2 "Model with ethnicity"))        ///
			  xlabel(0 10 20 30) ylabel(0 .05 .1 0.15 0.2)			 
//////////////////////////////////////////////////////////////////////////////    
// 	   Check for the effects of ethnicity and potential sample bias 

/////////// Sample is those cases with no missing ethnicity

stset durationiculeavingicu, failure(dummy_outcome_icu = 1) if(trust_good==1 & ageyear > 17 & ethnicity5 != .)

// Model without ethnicity as predictor 
streg female b3.icucat b2.ageyear4 risk_c ecmo				, distribution(logn) time vce(cluster trustid)
estat ic
// Model with ethnicity as predictor
streg female b3.icucat b2.ageyear4 risk_c ecmo b1.ethnicity5, distribution(logn) time vce(cluster trustid)
estat ic
predict loslnE_mean, mean
predict los1nE_median

