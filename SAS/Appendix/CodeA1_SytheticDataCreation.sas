/*************************************************************/
/**********DID Tutorial: Rothbard et al, 2022****************/
/**********CODE SECTION A1: SYNTHETIC DATA CREATION***********/
/**********SAS Code by Eleanor J Murray***********************/
/*************************************************************/

/*Initialize data frame*/
data did;
do case = 1 to 20000;
	time = .;
	post_DBT =.;
	dept = .;
	DBT_grp =.;
	case_challenge = .;
	device = .;
	tot_notechs = .;
output;
end;
run;

data did;
set did;
by case;
e_dept = ranuni(75);
if 0 < e_dept le 0.476 then dept = 1;
else if 0.476 < e_dept le 0.505 then dept = 8;
else if 0.505 < e_dept le 0.638 then dept = 4;
else if 0.638 < e_dept le 0.757 then dept = 3;

else if 0.757 < e_dept le 0.805 then dept = 5;
else if 0.805 < e_dept le 0.845 then dept = 6;
else if 0.845 < e_dept le 0.948 then dept = 2;
else if 0.948 < e_dept le 1 then dept = 7;

e_interv = ranuni(99);
if 0< e_interv le 0.5 then do; post_dbt = 1; time = 1; end;
else post_dbt = 0;

if dept in (1, 8, 4, 3) then dbt_grp = 1;
else if dept in (5, 6, 2, 7) then dbt_grp = 0;

if post_dbt = 0 then do;
	e_time = ranuni(51); 
	if 0< e_time le 0.5 then time = 0;
	else if 0.5 < e_time le 1 then time = -12;
end;

if time >-12 then post_covid = 1;
else post_covid = 0;

run;


data did;
set did;
by case;
if post_DBT = 1 and DBT_grp= 1 then do;
	e_challenge = ranuni(8);
	if 0 le e_challenge le 0.24 then case_challenge = 1;
	else if 0.24 lt e_challenge le 0.98 then case_challenge = 2;
	else if 0.9876 lt e_challenge then case_challenge = 3;
	
end;

else if post_DBT = 1 and DBT_grp= 0 then do;
	e_challenge = ranuni(4);
	if 0 le e_challenge le 0.28 then case_challenge = 1;
	else if 0.28 lt e_challenge le 0.96 then case_challenge = 2;
	else if 0.96 lt e_challenge then case_challenge = 3;
	
end;

else if post_DBT = 0 and DBT_grp= 1 then do;
	e_challenge = ranuni(144);
	if 0 le e_challenge le 0.18 then case_challenge = 1;
	else if 0.18 lt e_challenge le 0.8 then case_challenge = 2;
	else if 0.8 lt e_challenge then case_challenge = 3;
	
end;

else if post_DBT = 0 and DBT_grp= 0 then do;
	e_challenge = ranuni(856);
	if 0 le e_challenge le 0.15 then case_challenge = 1;
	else if 0.15 lt e_challenge le 0.76 then case_challenge = 2;
	else if 0.76 lt e_challenge then case_challenge = 3;

end;
run;

data did;
set did;
by case;
if post_DBT = 1 and DBT_grp= 1 then do;
	e_type = ranuni(8);
	if 0 le e_type le 0.20 then device = 1;
	else if 0.20 lt e_type le 0.97 then device = 2;
	else if 0.97 lt e_type then device = 3;
	
end;
else if post_DBT = 0 and DBT_grp= 1 then do;
	e_type = ranuni(8);
	if 0 le e_type le 0.08 then device = 1;
	else if 0.08 lt e_type le 0.99 then device = 2;
	else if 0.99 lt e_type then device = 3;
		
end;

else if post_DBT = 1 and DBT_grp= 0 then do;
	e_type = ranuni(8);
	if 0 le e_type le 0.95 then device = 1;
	else if 0.95 lt e_type le 0.99 then device = 2;
	else if 0.99 lt e_type then device = 3;
		
end;

else if post_DBT = 0 and DBT_grp= 0 then do;
	e_type = ranuni(8);
	if 0 le e_type le 0.78 then device = 1;
	else if 0.78 lt e_type le 0.92 then device = 2;
	else if 0.92 lt e_type then device = 3;

end;
run;

data did;
set did;

/*DEPARTMENT DUMMIES*/
if dept = 1 then dept_1 = 1; else dept_1 = 0;
if dept = 2 then dept_2 = 1; else dept_2 = 0;
if dept = 3 then dept_3 = 1; else dept_3 = 0;
if dept = 4 then dept_4 = 1; else dept_4 = 0;
if dept = 5 then dept_5 = 1; else dept_5 = 0;
if dept = 6 then dept_6 = 1; else dept_6 = 0;
if dept = 7 then dept_7 = 1; else dept_7 = 0;
if dept = 8 then dept_8 = 1; else dept_8 = 0;

/*CASE CHALLENGE DUMMIES*/
if case_challenge = 1 then case_challenge_1 = 1; else case_challenge_1 = 0;
if case_challenge = 2 then case_challenge_2 = 1; else case_challenge_2 = 0;
if case_challenge = 3 then case_challenge_3 = 1; else case_challenge_3 = 0;

/*DEPT*CASE_CHALLENGE DUMMIES*/
if case_challenge = 1 then do;
	if dept = 1 then dept_casech_11 = 1; else dept_casech_11 = 0;
	if dept = 2 then dept_casech_21 = 1; else dept_casech_21 = 0;
	if dept = 3 then dept_casech_31 = 1; else dept_casech_31 = 0;
	if dept = 4 then dept_casech_41 = 1; else dept_casech_41 = 0;
	if dept = 5 then dept_casech_51 = 1; else dept_casech_51 = 0;
	if dept = 6 then dept_casech_61 = 1; else dept_casech_61 = 0;
	if dept = 7 then dept_casech_71 = 1; else dept_casech_71 = 0;
	if dept = 8 then dept_casech_81 = 1; else dept_casech_81 = 0;

end;
if case_challenge = 2 then do;
	if dept = 1 then dept_casech_12 = 1; else dept_casech_12 = 0;
	if dept = 2 then dept_casech_22 = 1; else dept_casech_22 = 0;
	if dept = 3 then dept_casech_32 = 1; else dept_casech_32 = 0;
	if dept = 4 then dept_casech_42 = 1; else dept_casech_42 = 0;
	if dept = 5 then dept_casech_52 = 1; else dept_casech_52 = 0;
	if dept = 6 then dept_casech_62 = 1; else dept_casech_62 = 0;
	if dept = 7 then dept_casech_72 = 1; else dept_casech_72 = 0;
	if dept = 8 then dept_casech_82 = 1; else dept_casech_82 = 0;

end;
if case_challenge = 3 then do;
	if dept = 1 then dept_casech_13 = 1; else dept_casech_13 = 0;
	if dept = 2 then dept_casech_23 = 1; else dept_casech_23 = 0;
	if dept = 3 then dept_casech_33 = 1; else dept_casech_33 = 0;
	if dept = 4 then dept_casech_43 = 1; else dept_casech_43 = 0;
	if dept = 5 then dept_casech_53 = 1; else dept_casech_53 = 0;
	if dept = 6 then dept_casech_63 = 1; else dept_casech_63 = 0;
	if dept = 7 then dept_casech_73 = 1; else dept_casech_73 = 0;
	if dept = 8 then dept_casech_83 = 1; else dept_casech_83 = 0;

end;

if dept_casech_11 = . then dept_casech_11 = 0;
if dept_casech_12 = . then dept_casech_12 = 0;
if dept_casech_13 = . then dept_casech_13 = 0;
if dept_casech_21 = . then dept_casech_21 = 0;
if dept_casech_22 = . then dept_casech_22 = 0;
if dept_casech_23 = . then dept_casech_23 = 0;
if dept_casech_31 = . then dept_casech_31 = 0;
if dept_casech_32 = . then dept_casech_32 = 0;
if dept_casech_33 = . then dept_casech_33 = 0;
if dept_casech_41 = . then dept_casech_41 = 0;
if dept_casech_42 = . then dept_casech_42 = 0;
if dept_casech_43 = . then dept_casech_43 = 0;
if dept_casech_51 = . then dept_casech_51 = 0;
if dept_casech_52 = . then dept_casech_52 = 0;
if dept_casech_53 = . then dept_casech_53 = 0;
if dept_casech_61 = . then dept_casech_61 = 0;
if dept_casech_62 = . then dept_casech_62 = 0;
if dept_casech_63 = . then dept_casech_63 = 0;
if dept_casech_71 = . then dept_casech_71 = 0;
if dept_casech_72 = . then dept_casech_72 = 0;
if dept_casech_73 = . then dept_casech_73 = 0;
if dept_casech_81 = . then dept_casech_81 = 0;
if dept_casech_82 = . then dept_casech_82 = 0;
if dept_casech_83 = . then dept_casech_83 = 0;

/*DEVICE DUMMIES*/
if device =1 then device_1 =1; else device_1 = 0;
if device =2 then device_2 = 1; else device_2 = 0;
if device =3 then device_3 = 1; else device_3 = 0;

run;
 

data did;
set did;

/*estimated logNOTECHS*/
log_TOT_NOTECHS = 3.5055 + 0.0039*time +0.0528*Post_DBT + 0.0190*DBT_grp
				+ 0.0001*TIME*DBT_Grp + (-0.0294)*Post_dbt*DBT_Grp 
			+ 0.0494*dept_1 +(0.0325)*dept_2 + 0.0316*dept_3 + 0.0994*dept_4
			+ 0.0190*dept_5 + 0.0073*dept_6 + (0)*dept_7 + 0*dept_8
			+ (-0.0583)*case_challenge_1 + (0.0285)*case_challenge_2 + 0*case_challenge_3
			+ 0.0316*dept_casech_11 + (-0.0582)*dept_casech_12 + 0*dept_casech_13 
			+ (0.0530)*dept_casech_21 + 0.0118*dept_casech_22 + 0*dept_casech_23
			+ (0.1502)*dept_casech_31  + (-0.0159)*dept_casech_32 + (0)*dept_casech_33
			+ (-0.0462)*dept_casech_41 + (-0.1179)*dept_casech_42 + (0)*dept_casech_43
			+ 0*dept_casech_52 + 0*dept_casech_53 
			+ 0.1323*dept_casech_61 + 0*dept_casech_62
			+ (-0.0091)*dept_casech_71 
			+ 0.0416*device_1 + 0.0414*device_2;
run;

data did;
set did;
TOT_NOTECHS = round(exp(log_TOT_NOTECHS));
if TOT_NOTECHS < 4 then TOT_NOTECHS = 4;
if TOT_NOTECHS > 48 then TOT_NOTECHS = 48;
run;

data did;
set did;
drop e_challenge e_type log_tot_notechs e_dept e_interv e_time;
run;

/*export simulated data*/
libname methods "<path>";
data methods.did_sim;
set did;
run;

/*Convert to stata file*/
libname did "<path>";
PROC EXPORT DATA= did.did_sim 
            OUTFILE= "<path>\did_sim.dta" 
            DBMS=STATA REPLACE;
RUN;

/*Convert to csv file*/
libname did "<path>";
PROC EXPORT DATA= did.did_sim 
            OUTFILE= "<path>\did_sim.csv" 
            DBMS=CSV REPLACE;
RUN;
