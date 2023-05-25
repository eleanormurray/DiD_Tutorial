/*************************************************************/
/**********DID Tutorial: Rothbard et al, 2022****************/
/**********CODE SECTION A3: ADJUSTED POISSON MODELS***********/
/**********SAS Code by Eleanor J Murray***********************/
/*************************************************************/

/*See main code for data set up*/

/*Code A2.1: adjusted Poisson regression model*/
PROC GENMOD DATA=did_sim;  
CLASS Dept Device Case_Challenge;  
MODEL Tot_NOTECHS = TIME Post_DBT DBT_Grp Post_DBT*DBT_Grp  
Case_Challenge DEVICE Case_Challenge*DEVICE / DIST=POISSON LINK=LOG; 
REPEATED SUBJECT = DEPT / TYPE=IND;
OUTPUT OUT = poisson_adj P = E_NOTECHS;/*store predicted outcome values*/
run;


/**************************************************************************/
/*Code A2.2: Stratum-specific conditional DiD*/
/**************************************************************************/
PROC MEANS DATA=poisson_adj mean maxdec=3 nway nonobs;  
	where post_covid = 1;
	VAR E_NOTECHS;  
	class DBT_grp  / order = fmt descending;
	class post_dbt ;
	class case_challenge;
	class device; 
	format post_dbt Postfmt. Dbt_grp DBTfmt.;
	output out = poisson_adj_results1 (keep = dbt_grp post_dbt case_challenge device mean) mean = mean;
run;
data poisson_adj_results1; set poisson_adj_results1; 
stratum = device*10+case_challenge;
run;
proc sort data = poisson_adj_results1; by stratum dbt_grp; run;
proc transpose data = poisson_adj_results1 out=poisson_adj_diff (drop = _name_ _label_); 
	by stratum dbt_grp;
	var mean;
	id  post_dbt;
run;
data poisson_adj_diff1;
	set poisson_adj_diff;
	difference = post_dbt - pre_dbt;
run;
proc transpose data = poisson_adj_diff1 out = poisson_adj_diff2 (drop = _name_ _label_);
	by stratum;
	var difference;
	id dbt_grp;
run;
data poisson_adj_diff3;
	set poisson_adj_diff2;
	cDiD = intervention - control;
run;
proc print data =  poisson_adj_diff3;
	title1 'Adjusted Poisson Model';
	title2 'Conditional DiD estimates, by joint strata';
	format stratum stratafmt.;
run;

proc means data = poisson_adj_diff3;
var cDiD; 
run;
data  poisson_adj_diff4;set poisson_adj_diff3;
if stratum in (11, 12, 13) then device = 1;
if stratum in (21, 22, 23) then device = 2;
if stratum in (31, 32, 33) then device = 3;
run;
proc sort data = poisson_adj_diff4; by device;run;
proc means data = poisson_adj_diff4;
by device; 
var cDiD; 
run;



/**************************************************************************/
/*Code A2.3: Standardizing to get predicted NoTeCHs values & Marginal DiD*/
/**************************************************************************/
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

ods select none;
PROC GENMOD DATA=onesample;  
CLASS Dept Device Case_Challenge;  
MODEL Tot_NOTECHS = TIME Post_DBT DBT_Grp Post_DBT*DBT_Grp  
Case_Challenge DEVICE Case_Challenge*DEVICE   / DIST=POISSON LINK=LOG;  
REPEATED SUBJECT = DEPT / TYPE=IND; 
OUTPUT OUT = poisson_adj_stdz P = E_NOTECHS;/*store predicted outcome values*/
RUN;  

PROC MEANS DATA=poisson_adj_stdz mean  maxdec=3 nway nonobs noprint;  
	where post_covid = 1 and interv >-1;
	VAR E_NOTECHS;  
	class DBT_grp  / order = fmt descending;
	class time ;
	format time Postfmt. Dbt_grp DBTfmt.;
	output out = poisson_adj_stdz_results (keep = dbt_grp time mean) mean = mean;
run;
proc sort data = poisson_adj_stdz_results; by dbt_grp;
run;
proc transpose data = poisson_adj_stdz_results out=poisson_adj_stdz_diff (drop = _name_ _label_); 
	by dbt_grp;
	var mean;
	id time;
run;
data poisson_adj_stdz_diff;
	set poisson_adj_stdz_diff;
	difference = post_dbt - pre_dbt;
	if dbt_grp = 0 then do;
		call symput("diff_0", difference);
	end;
run;
data poisson_adj_stdz_diff;
	set poisson_adj_stdz_diff;
	DiD = difference - %sysevalf(&diff_0);
	if dbt_grp = 0 then DiD = 0;
run;
ods select all;
proc sort data =poisson_adj_stdz_diff; by descending dbt_grp ; run;
proc print data = poisson_adj_stdz_diff;
	title1 'Adjusted Poisson Model';
	title2 'Predicted NoTECHS & Marginal DiD estimate';
run;


