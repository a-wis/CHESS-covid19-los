
/////    
/////    
/////  Shryane et al. (2020). Length of Stay in ICU of Covid-19 Patients in England     
/////                            March - May 2020
/////    
/////   

////////////////////////////////////////////////////////////////////////////////
//     Data
//
//   The CHESS data used in the study above are not publicly available. 
//   The code below is made available to allow readers to evaluate the way
//    the data was coded and analysed. 


// FOR CHECKING RESULTS
use "\\10.2.82.9\humrss$\snapped\replicated\Research\SOSS\SOST\SOST-RAMP-PROJECT\Data\Data - Processed\26th May data processed\SOST paper\CHESS_Case_Report_MASTERNEW_2020-05-26.dtanumeric", clear


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

//// ethnicity
tab ethnicity5 // 5-category variable for ethnicity


//////////////// 
//  Health risk Rasch scores

rename chronicrespiratory resp 
rename asthmarequiring asth
rename chronicheart heart
rename chronicrenal renal
rename chronicliver liver
rename chronicneurological neuro
rename isdiabetes diab
rename immunosuppressiontreatment immt 
rename immunosuppressiondisease immd
rename obesityclinical obc 
rename hypertension hyp

// Respiratory dummy
tab resp, miss nol 
recode resp (2=0)(3=.)(4=1)
rename resp co_respi
tab co_respi, miss

//Asthma dummy
tab asth, miss nol
recode asth (2=0)(3=.)(4=1)
rename asth co_asthm
tab co_asthm

// Heart dummy
tab heart, miss nol
recode heart (2=0)(3=.)(4=1)
rename heart co_heart
tab co_heart

// hypertension dummy
tab hyp, miss nol
recode hyp (2=0)(3=.)(4=1)
rename hyp co_hyper
tab co_hyper

// conditions that supress the immune system
tab immd, miss nol
tab immt
recode immd immt (2=0)(3=.)(4=1)
rename immd co_immud
rename immt co_immut
tab co_immud co_immut

// diabetes dummy
tab diab, miss nol
recode diab (2=0)(3=.)(4=1)
rename diab co_diab
tab co_diab

// Kidney disease dummy
tab renal, miss nol
recode renal (2=0)(3=.)(4=1)
rename renal co_renal
tab co_renal

// Liver disease dummy
tab liver, miss nol
recode liver (2=0)(3=.)(4=1)
rename liver co_liver
tab co_liver

// neurological dummy
tab neuro, miss nol
recode neuro (2=0)(3=.)(4=1)
rename neuro co_neuro
tab co_neuro

// Obesity ternary 
tab obc, miss
tab obc, miss nol
recode obc (2=1)(3=0)(4=.)(5=2) 
rename obc co_obcli
tab co_obcli, nol miss

// How many cases have non-missing comorbidity data?
egen cocount = rownonmiss(co_*)
tab cocount
// N = 504 have no comorbidity data
// N = 3,086 have at least some comorbidity data

///////// COMPUTE irt partial-credit model, only cases with some non-missing (N = 3,086)
//irt pcm co_* if cocount > 0
//predict irt0 if cocount > 0, latent


///Compute Rasch PCM
irt pcm co_* if cocount > 0, cns(a@1)
predict irt0 if cocount > 0, latent


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
//						Descriptive stats
//
////////////////////

sum female ageyear4 icucat ecmo irt0 ethnicity5

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

sum co_* 
sum co_* if _st == 1


/////// Models assuming WEIBULL baseline hazard

// 1_1 Main effects only (no interactions)
streg female b3.icucat b2.ageyear4 irt0 ecmo, distribution(weibull) time vce(cluster trustid)
estat ic 
est store w11
predict los11w_mean, mean
predict los11w_median

// 1_2 Main effects + interactions
streg ecmo female b3.icucat##b2.ageyear4 b3.icucat##c.irt0, distribution(weibull) time vce(cluster trustid)
estat ic 
est store w12
predict los12w_mean, mean
predict los12w_median

/////// Models assuming LOG-NORMAL baseline hazard

// 1_1 Main effects 
streg female b3.icucat b2.ageyear4 irt0 ecmo, distribution(lognormal) time vce(cluster trustid)
estat ic 
est store ln11
predict los11logn_mean, mean
predict los11logn_median

// 1_2 Main effects + interactions
streg ecmo female b3.icucat##b2.ageyear4 b3.icucat##c.irt0, distribution(lognormal) time vce(cluster trustid)
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
 
table ageyear4 icucat,  c(mean los11logn_mean mean los11logn_median sd los11logn_mean n los11logn_mean) col row
table ageyear4 icucat,  c(mean los11logn_median sd los11logn_median n los11logn_median) col row

// for comparison, the results from the Weibull model
table ageyear4 icucat,  c(mean los11w_mean mean los11w_median sd los11w_mean n los11w_mean) col row
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
streg female b3.icucat b2.ageyear4 irt0 ecmo , distribution(logn) time vce(cluster trustid)
estat ic
// Model with ethnicity as predictor
streg female b3.icucat b2.ageyear4 irt0 ecmo b1.ethnicity5, distribution(logn) time vce(cluster trustid)
estat ic
predict loslnE_mean, mean
predict los1nE_median

