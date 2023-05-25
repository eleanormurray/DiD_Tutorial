/*************************************************************/
/**********DID Tutorial: Rothbard et al, 2022****************/
/**********CODE SECTION A2: Bootstraps************************/
/**********SAS Code by Eleanor J Murray***********************/
/*************************************************************/

/*When running >100 bootstraps, run the code below to store  the log as external file so that bootstraps don't overwhelm display memory*/
proc printto log = "<path>\log.rtf";
run;

/**********************************************************/
/*Code 5: Bootstrap code for standardized estimates & DiD*/ 
/********************************************************/

%macro didBoot(nboot = , datain = , model = , adjusted =  , covs = , troubleshoot = 0);
%if &troubleshoot = 0 %then %do;
	ods select none;
%end;
%else %if &troubleshoot = 1 %then %do;
	ods select all;
%end;

/*Count number of unique surgeries*/
data bootset ;
  set &datain end = _end_ ;
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

/*Create a list of surgeries to bootstrap*/
data ids ;
   do bsample = 1 to &nboot;
       do _id = 1 to &nids ;
           output ;
       end;
   end;
run;
proc surveyselect data= ids 
         method = urs
         n= &nids
         seed = 1232  
         out = _idsamples (keep = bsample _id  numberhits  ) 
         outall  noprint  ;       
      strata bsample ;
run;

/*Create blank results dataset*/
data out_boots;
	bsample = .;
run;

/*Step through main analysis and bootstrap samples*/
%do bsample = 0 %to &nboot;
 title "Bootstrap number &bsample";

 	%if %eval(&bsample) = 0 %then %do;
		data bootsample;
			set bootset;
			numberhits = 1;
		run;
	%end;
	%else %do;
		/*Pull new dataset for bootstrap sample*/
		proc sort data = bootset ;
			by _id;
		run;
		data bootsample;
			merge bootset _idsamples (where = (bsample = &bsample));
			by _id;
		run;
	%end;
		/*Run the model*/
		%if &model = Linear %then %do;
		PROC GENMOD DATA=bootsample;  
			CLASS Dept Device Case_Challenge;  
			MODEL Tot_NOTECHS =Post_DBT DBT_Grp Post_DBT*DBT_Grp  
			&covs; 
			freq numberhits;
			OUTPUT OUT = out_boot P = E_NOTECHS;/*store predicted outcome values*/
		RUN;  
		%end;
		%else %if &model = Poisson %then %do;
		PROC GENMOD DATA=bootsample;  
			CLASS Dept Device Case_Challenge;  
			MODEL Tot_NOTECHS =Post_DBT DBT_Grp Post_DBT*DBT_Grp  
			&covs / DIST=POISSON LINK=LOG; 
			freq numberhits;
			OUTPUT OUT = out_boot P = E_NOTECHS;/*store predicted outcome values*/
		RUN; 
		%end;

		proc sort data = out_boot; by interv; run; 

		PROC MEANS DATA=out_boot (where = (interv >-1)) mean maxdec=3 nway noprint;  
			by interv;
			where post_covid = 1;
			VAR E_NOTECHS;  
			class DBT_grp   / order = fmt descending;
			class post_dbt;
			freq numberhits;
			format post_dbt Postfmt. Dbt_grp DBTfmt.;
			output out = out2_boot (keep = DBT_grp post_dbt interv  mean) mean = mean;
		run;

		proc sort data = out2_boot; by dbt_grp;
			run;
		proc transpose data = out2_boot out=out2_boot_diff (drop = _name_ _label_); 
			by dbt_grp;
			var mean;
			id post_dbt;
		run;
		data out2_boot_diff_2 ;
			set out2_boot_diff ;
			pre_post = post_dbt - pre_dbt;
			if dbt_grp = 0 then do;
				call symput("diff_0", pre_post);
			end;
		run;
		data  out2_boot_diff_3;
			set  out2_boot_diff_2 ;
			DiD = pre_post - %sysevalf(&diff_0);
			if dbt_grp = 0 then DiD = 0;
		run;
	
		proc transpose data = out2_boot_diff_3 out = out2_boot_2 ;
			by dbt_grp;
				var Pre_DBT Post_DBT pre_post DiD ;
		run;
		proc transpose data = out2_boot_2 out = out2_boot3;
			var col1;
			id dbt_grp _NAME_;
		run;

		data out2_boot_last;
			set out2_boot3; 
			bsample = &bsample;
			drop _NAME_ ControlDiD;
		run;

		/*Merges current results into total results file*/
		data out_boots;
			set out_boots out2_boot_last; 
			by bsample;
		run;
%end;	
ods select all;

data out_boots;set out_boots;
where bsample ne .;
run;

/*Extract point estimates*/
data sample0;
	set out_boots (where = (bsample = 0));
	keep ControlPre_DBT ControlPost_DBT 
		InterventionPre_DBT InterventionPost_DBT InterventionDiD ;
run;
data sample0;
set sample0;
	call symput("Ctrlpre", ControlPre_DBT);
	call symput("Ctrlpost", ControlPost_DBT);
	call symput("intvpre",InterventionPre_DBT);
	call symput("intvpost", InterventionPost_DBT);
	call symput("DiD", InterventionDiD);
run;

/*Calculate 2.5th and 97.5th percentiles of the beta and RR estimates from bootstrap samples only (not using main analysis)*/
proc univariate data = out_boots (where = (bsample >0)) noprint;
	VAR ControlPre_DBT ControlPost_DBT 
		InterventionPre_DBT InterventionPost_DBT InterventionDiD  ;  
	output out = for_CIs pctlpre =  Control0Pre_DBT0  Control0Post_DBT0  Intervention0Pre_DBT0 
			Intervention0Post_DBT0 DiD0x0 pctlname = lb ub pctlpts = 2.5, 97.5;
run;

proc transpose data = for_CIs out=for_CIs_2;
run;
data for_CIs_3;
set for_CIs_2;
	Period = scan(_NAME_,2,'0');
	Group = scan(_NAME_,1,'0');
	Bound = scan(_NAME_,3,'0');
	if Group = "DiD" then  do;
		Group = "_DiD"; Period = .;
		if Bound = "lb" then do;
			call symput("DiDlb",col1);
		end;
		else if Bound = "ub" then do;
			call symput("DiDub",col1);
		end;
	end;
	Estimate = col1;
	drop _NAME_ col1 ;
run;
proc sort data = for_CIs_3; by Group Period; run;
proc transpose data = for_CIs_3 out= for_CIs_4 (drop = _NAME_);
	var Estimate;
	by Group Period;
	id Bound;
run;
data for_CIs_5;
set for_CIs_4;
	where Group not in ("_DiD");
run;

proc transpose data = for_CIs_5 out=for_CIs_6 suffix=_lb;
	by Group;
	var Lb;
	id Period;
run;
proc transpose data = for_CIs_5 out=for_CIs_7 suffix=_ub;
	by Group;
	var Ub;
	id Period;
run;
data final;
	merge for_CIs_6(drop = _name_) for_CIs_7(drop=_name_);
	by Group;
run;

proc sql;
	ALTER TABLE final
	ADD Pre_DBT NUM(8), Post_DBT NUM(8), DiD NUM(8), DiD_lb NUM(8), DiD_ub NUM(8);

	UPDATE final
		SET	Pre_DBT =  %sysevalf(&Ctrlpre), Post_DBT = %sysevalf(&Ctrlpost)		
			WHERE Group ="Control";
	UPDATE final
		SET Pre_DBT = %sysevalf(&intvpre), Post_DBT = %sysevalf(&intvpost), 
		DiD = %sysevalf(&DiD), DiD_lb = %sysevalf(&DiDlb), DiD_ub = %sysevalf(&DiDub)
			WHERE Group = "Intervention";
quit;


data final;
set final;
label Pre_DBT = 'E[Ya|Baseline]' Post_DBT = 'E[Ya|Follow-up]' 
	Pre_DBT_lb = 'E[Ya|Baseline] lb' Pre_DBT_ub = 'E[Ya|Baseline] ub' 
	Post_DBT_lb= 'E[Ya|Follow-up] lb'  Post_DBT_ub= 'E[Ya|Follow-up] ub' ;
run;
proc sort data = final;
by descending Group;
run;

/*Print final results*/
ods select all;
proc print data = final label;
	var Group Pre_DBT Pre_DBT_lb Pre_DBT_ub Post_DBT Post_DBT_lb Post_DBT_ub  DiD DiD_lb DiD_ub;
	title1 "Model: &model, Adjusted = &adjusted";
	title2 "Predicted NoTECHS & Marginal DiD estimate";
	title3 "95% Confidence intervals using &nboot bootstrap samples";
run;

%mend;

%let nboot = 500;

%didBoot(nboot = &nboot, datain = did_sim, model = Linear, adjusted = no , covs = TIME);
%didBoot(nboot = &nboot, datain =  did_sim, model = Linear, adjusted = yes, covs = TIME Case_Challenge DEVICE Case_Challenge*DEVICE);

%didBoot(nboot = &nboot, datain = did_sim, model = Poisson, adjusted = no, covs = TIME ); 
%didBoot(nboot = &nboot, datain = did_sim, model = Poisson, adjusted = yes , covs =TIME Case_Challenge DEVICE Case_Challenge*DEVICE);


proc printto;
run;
