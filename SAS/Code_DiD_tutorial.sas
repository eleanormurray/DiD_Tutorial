/**********DID Tutorial: Rothbard et al, 2022****************/
/**********Main Tutorial CODE*********************************/
/**********SAS Code by Eleanor J Murray***********************/
/*************************************************************/

/******************************************/
/*Code Section 0: Data set up & formatting*/
/******************************************/

/* Set directory to your location */
libname tutorial "<path>";

/*Read in data and count the number of observations*/
data did_sim;
set tutorial.did_sim end = _end_  ;
  retain _id ;
  if _n_ = 1 then _id = 0;
  _id = _id + 1 ;
  if _end_ then do ;
     call symput("nids",trim(left(_id)));
  end;
run;

/*Optional: Format to make output easier to read*/
filename style "<path>";
%include style(tutorial_style_template);

/*Required formats: run if not using style file*/
proc format;
	value Postfmt   0 = "Pre_DBT"
				    1 = "Post_DBT";
    value DBTfmt    0 = "Control"
				    1 = "Intervention";
	value timefmt -12 = "Pre_COVID"
				   	0 = "Baseline"
				    1 = "Follow_up";

	value stratafmt 11 = "Sealer, Very challenging"
					12 = "Sealer, Somewhat challenging"
					13 = "Sealer, Not challenging"
					21 = "Stapler, Very challenging"
					22 = "Stapler, Somewhat challenging"
					23 = "Stapler, Not challenging"
					31 = "Other device, Very challenging"
					32 = "Other device,Somewhat challenging"
					33 = "Other device,Not challenging" ;
run;

/*Get familiarized with the data*/
proc contents data = did_sim;
run;

proc freq data = did_sim;
tables tot_notechs post_dbt dbt_grp /missing;
run;

/******************************************/
/*Code Section 1: Checking Assumptions*****/
/******************************************/

/*Calculate average outcome value in control & intervention departments in each baseline time*/
proc sort data = did_sim; by post_covid dbt_grp; run;
proc means data = did_sim;
where post_dbt = 0;
by post_covid dbt_grp;
var tot_notechs;
format dbt_grp DBTfmt.;
run;
/*Calculate the change in average value between baseline timepoints & control / intervention departments*/
PROC GENMOD DATA=did_sim (where = (time le 0));
CLASS Dept;
	MODEL Tot_NOTECHS = post_covid DBT_Grp post_covid*DBT_Grp  ; 
REPEATED SUBJECT = DEPT / TYPE=IND; /*clustering by department*/
estimate "Pre-Intervention trend difference" post_covid*DBT_Grp 1; /*output pre-covid vs post-covid DID*/
title1 'Assumption check'; 
RUN;


/*****************************************************/
/*CODE SECTION 2: Unadjusted models******************/
/***************************************************/

/*Code 2.1: unadjusted linear regression model, excluding pre-COVID data*/
PROC GENMOD DATA=did_sim (where = (post_covid = 1));
CLASS Dept;
	MODEL Tot_NOTECHS = Post_DBT DBT_Grp Post_DBT*DBT_Grp  ; 
REPEATED SUBJECT = DEPT / TYPE=IND; /*clustering by department*/
estimate "Unconditional DiD" Post_DBT*DBT_Grp 1; /*output unconditional DiD*/
OUTPUT OUT = lm_unadj P = E_NOTECHS;/*store predicted outcome values*/
title1 'Unadjusted Linear Model'; 
RUN;

/*Optional Code 2.1.2: Create table of predicted outcome values & DiD*/
PROC MEANS DATA=lm_unadj mean maxdec=3 nway nonobs noprint;  
	where post_covid = 1;
	VAR E_NOTECHS;  
	class DBT_grp  / order = fmt descending;
	class post_dbt;
	format post_dbt Postfmt. Dbt_grp DBTfmt.;
	output out = lm_unadj_results (keep = dbt_grp post_dbt mean) mean = mean;
run;
proc sort data = lm_unadj_results; by dbt_grp;
run;
proc transpose data = lm_unadj_results out=lm_unadj_diff (drop = _name_ _label_); 
	by dbt_grp;
	var mean;
	id post_dbt;
run;
data lm_unadj_diff;
	set lm_unadj_diff;
	difference = post_dbt - pre_dbt;
	if dbt_grp = 0 then do;
		call symput("diff_0", difference);
	end;
run;
data lm_unadj_diff;
	set lm_unadj_diff;
	DiD = difference - %sysevalf(&diff_0);
	if dbt_grp = 0 then DiD = 0;
run;
proc sort data = lm_unadj_diff; by descending dbt_grp ; run;
proc print data = lm_unadj_diff;
	title1 'Unadjusted Linear Model';
	title2 'Predicted NoTECHS & DiD estimate';
run;

/*Code 2.2: Unadjusted linear model using all timepoints*/
PROC GENMOD DATA=did_sim ;
CLASS Dept;
	MODEL Tot_NOTECHS = time Post_DBT DBT_Grp Post_DBT*DBT_Grp  ; 
REPEATED SUBJECT = DEPT / TYPE=IND; /*clustering by department*/
estimate "Unconditional DiD" Post_DBT*DBT_Grp 1; /*output unconditional DiD*/
OUTPUT OUT = lm_unadj2 P = E_NOTECHS;/*store predicted outcome values*/
title1 'Unadjusted Linear Model'; 
RUN;

/*Optional Code 2.2.2: Create table of predicted outcome values & DiD*/
PROC MEANS DATA=lm_unadj2 mean maxdec=3 nway nonobs noprint;  
	where post_covid = 1;
	VAR E_NOTECHS;  
	class DBT_grp  / order = fmt descending;
	class post_dbt;
	format post_dbt Postfmt. Dbt_grp DBTfmt.;
	output out = lm_unadj_results2 (keep = dbt_grp post_dbt mean) mean = mean;
run;
proc sort data = lm_unadj_results2; by dbt_grp;
run;
proc transpose data = lm_unadj_results2 out=lm_unadj_diff2 (drop = _name_ _label_); 
	by dbt_grp;
	var mean;
	id post_dbt;
run;
data lm_unadj_diff2;
	set lm_unadj_diff2;
	difference = post_dbt - pre_dbt;
	if dbt_grp = 0 then do;
		call symput("diff_0", difference);
	end;
run;
data lm_unadj_diff2;
	set lm_unadj_diff2;
	DiD = difference - %sysevalf(&diff_0);
	if dbt_grp = 0 then DiD = 0;
run;
proc sort data = lm_unadj_diff2; by descending dbt_grp ; run;
proc print data = lm_unadj_diff2;
	title1 'Unadjusted Linear Model';
	title2 'Predicted NoTECHS & DiD estimate';
run;

/*Code 2.3: Unadjusted Poisson regression model, all time periods*/
PROC GENMOD DATA=did_sim;  
CLASS Dept;  
MODEL Tot_NOTECHS = TIME Post_DBT DBT_Grp Post_DBT*DBT_Grp  / DIST=POISSON LINK=LOG; 
REPEATED SUBJECT = DEPT / TYPE=IND;  
OUTPUT OUT = poisson_unadj P = E_NOTECHS;  
title1 'Unadjusted Poisson Model'; 
RUN;  

/*Code 2.3.2: Create table of predicted outcome values & DiD*/
/*Note, we cannot get the DiD directly from the regression coefficents*/
/*This code is required*/
PROC MEANS DATA=poisson_unadj mean  maxdec=3 nway nonobs noprint;  
	where post_covid = 1;
	VAR E_NOTECHS;  
	class DBT_grp  / order = fmt descending;
	class post_dbt;
	format post_dbt Postfmt. Dbt_grp DBTfmt.;
	output out = poisson_unadj_results (keep = dbt_grp post_dbt mean) mean = mean;
run;
proc sort data = poisson_unadj_results; by dbt_grp;
run;
proc transpose data = poisson_unadj_results out=poisson_unadj_diff (drop = _name_ _label_); 
	by dbt_grp;
	var mean;
	id post_dbt;
run;
data poisson_unadj_diff;
	set poisson_unadj_diff;
	difference = post_dbt - pre_dbt;
	if dbt_grp = 0 then do;
		call symput("diff_0", difference);
	end;
run;
data poisson_unadj_diff;
	set poisson_unadj_diff;
	DiD = difference - %sysevalf(&diff_0);
	if dbt_grp = 0 then DiD = 0;
run;
proc sort data = poisson_unadj_diff; by descending dbt_grp ; run;
proc print data = poisson_unadj_diff;
	title1 'Unadjusted Poisson Model';
	title2 'Predicted NoTECHS & DiD estimate';
run;


/***************************************************/
/*Code Section 3: Adjusted Linear Models***********/ 
/*************************************************/
/*Code 3.1: adjusted linear regression model*/
/*Difference-in-Difference, conditional on confounder levels*/
PROC GENMOD DATA=did_sim;  
CLASS Dept Device Case_Challenge;  
MODEL Tot_NOTECHS = TIME Post_DBT DBT_Grp Post_DBT*DBT_Grp  
Case_Challenge DEVICE DEVICE*Case_Challenge ; 
REPEATED SUBJECT = DEPT / TYPE=IND; /*Cluster standard errors by department*/
estimate "Conditional DiD" Post_DBT*DBT_Grp 1; /*outputs conditional DiD -- i.e. if all confounders held constant*/
OUTPUT OUT = lm_adj P = E_NOTECHS;/*store predicted outcome values*/
title1 'Adjusted Linear Model'; 
RUN;   

/*Optional Code 3.1.1: Output conditional DiD by strata*/
PROC MEANS DATA=lm_adj mean maxdec=3 nway nonobs noprint;  
	where post_covid = 1;
	VAR E_NOTECHS;  
	class DBT_grp  / order = fmt descending;
	class post_dbt ;
	class case_challenge;
	class device; 
	format post_dbt Postfmt. Dbt_grp DBTfmt.;
	output out = lm_adj_results1 (keep = dbt_grp post_dbt case_challenge device mean) mean = mean;
run;
data lm_adj_results1; set lm_adj_results1; 
stratum = device*10+case_challenge;
run;
proc sort data = lm_adj_results1; by stratum dbt_grp; run;
proc transpose data = lm_adj_results1 out=lm_adj_diff (drop = _name_ _label_); 
	by stratum dbt_grp;
	var mean;
	id  post_dbt;
run;
data lm_adj_diff1;
	set lm_adj_diff;
	difference = post_dbt - pre_dbt;
run;
proc transpose data = lm_adj_diff1 out = lm_adj_diff2 (drop = _name_ _label_);
	by stratum;
	var difference;
	id dbt_grp;
run;
data lm_adj_diff3;
	set lm_adj_diff2;
	cDiD = intervention - control;
run;
proc print data =  lm_adj_diff3;
	title1 'Adjusted Linear Model';
	title2 'Conditional DiD estimates, by joint strata';
	format stratum stratafmt.;
run;


/*************************************************/
/*Code 3.2: Standardizing the estimated outcome to obtain a Marginal DiD estimator*/
/*Option 1: the long way! Calculating the stratum-specific probabilities & standardizing by hand*/
/*************************************************/
PROC MEANS DATA=lm_adj mean maxdec=3 nway nonobs;  
	where post_covid = 1;
	VAR E_NOTECHS;  
	class DBT_grp  / order = fmt descending;
	class post_dbt ;
	class case_challenge;
	class device; 
	format post_dbt Postfmt. Dbt_grp DBTfmt.;
run;
proc freq data = did_sim;
where post_covid = 1;
tables Case_Challenge*Device  / norow nocol;
run;

/*************************************************/
/*Code 3.3: Standardizing the estimated outcome to obtain a Marginal DiD estimator*/
/*Option 2: the short way! Using copies of the data*/
/*Note that we need to fix time and post_covid to match the stratum-assignment of the copy*/
/*************************************************/
data onesample ;
  set did_sim end = _end_  ;
  retain _id ;
  if _n_ = 1 then _id = 0;
  _id = _id + 1 ;
  if _end_ then do ;
     call symput("nids",trim(left(_id)));
  end;
   
  interv = -1 ;    /* 1st copy: equal to original one */
    output ; 
  if time ge 0 then do; /*The copies should only be made from surgeries in times 0 and 1!*/
  	interv = 0 ;     /* 2nd copy: Post_DBT = 0, DBT_group = 0, outcome = missing */
   	 	post_dbt = 0 ;
		dbt_grp = 0;
		time = 0;
		post_covid = 1;
   	 	tot_notechs = . ;
   	 output ;  
  	interv = 1 ;     /* 2nd copy: Post_DBT = 0, DBT_group = 1, outcome = missing */
   	 	post_dbt = 0 ;
		dbt_grp = 1;
		time = 0;
		post_covid = 1;
   		tot_notechs = . ;
     output ;    
   	interv = 3 ;     /* 3rd copy: Post_DBT = 1, DBT_group = 0, outcome = missing */
    	post_dbt = 1 ;
		dbt_grp = 0;
		time = 1;
		post_covid = 1;
    	tot_notechs = . ;
     output ;  
 	 interv = 4 ;     /* 4th copy: Post_DBT = 1, DBT_group = 1, outcome = missing */
    	post_dbt = 1 ;
		dbt_grp = 1;
		time = 1;
		post_covid = 1;
    	tot_notechs = . ;
     output ;  
 end;
run;

/*Re-run regression code to generate stratum-specific predicted values in all copies*/
/*Since new data copies has missing outcome, only original data is used to fit the model*/
ods select none;
PROC GENMOD DATA=onesample;  
CLASS Dept Device Case_Challenge;  
MODEL Tot_NOTECHS = TIME Post_DBT DBT_Grp Post_DBT*DBT_Grp  
Case_Challenge DEVICE Case_Challenge*DEVICE  ; 
REPEATED SUBJECT = DEPT / TYPE=IND; 
OUTPUT OUT = lm_adj_stdz P = E_NOTECHS;/*store predicted outcome values*/
RUN;  

/*Optional Code 3.2.2: Create table of predicted outcome values & DiD*/
PROC MEANS DATA=lm_adj_stdz mean  maxdec=3 nway nonobs noprint;  
	where post_covid = 1 and interv >-1;
	VAR E_NOTECHS;  
	class DBT_grp  / order = fmt descending;
	class Post_DBT ;
	format Post_DBT Postfmt. Dbt_grp DBTfmt.;
	output out = lm_adj_stdz_results (keep = dbt_grp Post_DBT mean) mean = mean;
run;
proc sort data = lm_adj_stdz_results; by dbt_grp;
run;
proc transpose data = lm_adj_stdz_results out=lm_adj_stdz_diff (drop = _name_ _label_); 
	by dbt_grp;
	var mean;
	id Post_DBT;
run;
data lm_adj_stdz_diff;
	set lm_adj_stdz_diff;
	difference = post_dbt - pre_dbt;
	if dbt_grp = 0 then do;
		call symput("diff_0", difference);
	end;
run;
data lm_adj_stdz_diff;
	set lm_adj_stdz_diff;
	DiD = difference - %sysevalf(&diff_0);
	if dbt_grp = 0 then DiD = 0;
run;
ods select all;
proc sort data = lm_adj_stdz_diff; by descending dbt_grp ; run;
proc print data = lm_adj_stdz_diff;
	title1 'Adjusted Linear Model';
	title2 'Predicted NoTECHS & Marginal DiD estimate';
run;
