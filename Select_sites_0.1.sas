/******************************************************************************************
Program:		Select_sites.sas


Description:	This program defines select_sites, a SAS macro for producing rank-ordered lists of districts and 
				schools in education as implemented in Litwok et al. (2022):

				select_sites	Produces rank-ordered lists of districts and schools for
								users to recruit to participate in an impact study.


Authors:		Azim Shivji, Daniel Litwok, and Robert B. Olsen


References: 	Shivji, A., Litwok, D., & Olsen, Robert B. (2023). Select_sites: SAS Macro 
					for	Selecting Districts and Schools for Impact Studies in Education.
					Documentation. Available at: <https://github.com/select-sites/exval>.

				Litwok, D., Nichols, A., Shivji, A., & Olsen, Robert B. (2022). Selecting 
					districts and schools for impact studies in education: A simulation 
					study of different strategies. Journal of Research on Educational 
					Effectiveness. DOI: https://doi.org/10.1080/19345747.2022.2128952.

				Tipton, E. (2013). Stratified sampling using cluster analysis: A sample 
					selection strategy for improved generalizations from experiments. 
					Evaluation Review, 37(2), 109-139.


GitHub Repo:	<https://github.com/select-sites/exval>


Support:		If you have questions or bugs to report, you may open an issue on the Github 
				repository.


History:		Version				Date				Notes
				-------				----				-----------------------------------
				0.1					02/07/23			Preliminary release of the select_sites macro


SAS Version:	These macros were programmed and tested in SAS 9.4.


Notes:			*	The macro creates various intermediate datasets (temporary working 
					datasets that are used in the processing of the macros and are deleted 
					after they are no longer needed). To reduce the likelihood that these 
					datasets conflict with (and overwrite) existing datasets in the user's 
					work library, the macros use SAS's "DATAn naming convention."
						With this feature, SAS defines a set of potential dataset names 
					(DATA1, DATA2, etc., all the way to DATA9999) and sequentially searches 
					for unused dataset names among that group, in the work library. When it 
					finds an unused name, it will assign that to the intermediate dataset 
					the macros create.
						This *significantly reduces the likelihood of* (rather than 
					categorically prevents) conflicts because there is still a possibility 
					(however remote) that the user may have existing datasets in their work 
					library that cover all of the potential names that SAS could assign 
					(from DATA1 to DATA9999). If all 9,999 potential dataset names are in 
					use, SAS will start from the beginning of the list and assign the name 
					DATA1 to the new dataset (and overwrite the existing DATA1 dataset). In 
					that unlikely scenario, the user should beware that their data may be 
					overwritten by the macro.

				*	As mentioned above, the macros delete the intermediate datasets 
					when they are done with them. However, if the macros terminate early 
					because of errors, there may be leftover intermediate datasets that 
					were not yet deleted (they should be easy to identify because they all 
					follow the DATAn naming convention, as in DATA1, DATA2, etc.). The user 
					should feel free to delete those intermediate datasets, if they want to 
					clean up their work library, but there is no need to do so. Whether 
					those datasets remain in the work library will not interfere with the 
					processing of the macros.



License:		Select_sites: SAS Macro for Selecting Districts and Schools for Impact 
					Studies	in Education. Copyright (c) 2023 Azim Shivji, Daniel Litwok, 
					and Robert B. Olsen.

				This program is free software: you can redistribute it and/or modify it
					under the terms of the GNU General Public License as published by the
					Free Software Foundation, either version 3 of the License, or (at your
					option) any later version.

				This program is distributed in the hope that it will be useful, but 
					WITHOUT ANY WARRANTY; without even the implied warranty of
					MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU 
					General Public License for more details.

				You should have received a copy of the GNU General Public License along
					with this program. If not, see <https://www.gnu.org/licenses>.

******************************************************************************************/


/*----------------------------------
BEGIN MACRO DEFINITION
----------------------------------*/
%macro select_sites(
	input_data =,
	sample_size_school =,
	district_id =,
	school_id =,
	seed =,
	max_schools_per_district =,
	max_type = OVERALL,
	
	strata_d =,
	method_d =,
	rand_size_d =,
	bal_distance_d =,
	
	strata_s =,
	method_s =,
	rand_size_s =,
	bal_distance_s =,
	
	targets_data =,	
	out_district = _out_district,
	out_school = _out_school,
	out_summary = _out_summary
);



/*----------------------------------
PREPARATORY WORK
----------------------------------*/
%local i var strat n_err;
%let n_err = 0;

%let max_type = %upcase(&max_type.);
%let method_d = %upcase(&method_d.);
%let method_s = %upcase(&method_s.);

%local strata_d_comma strata_s_comma;
%let strata_d_comma = %sysfunc(transtrn(&strata_d., %str( ), %str(, )));
%let strata_s_comma = %sysfunc(transtrn(&strata_s., %str( ), %str(, )));



/*----------------------------------
DEFINE AUXILIARY, INTERNAL MACROS
----------------------------------*/
%macro varexist(ds, var);
	%local dsid rc;
	
	%let dsid = %sysfunc(open(&ds.));
	%if (&dsid.) %then %do;
		%if %sysfunc(varnum(&dsid.,&var.)) %then 1;
		%else 0;
		%let rc = %sysfunc(close(&dsid.));
	%end; %else 0;
%mend varexist;
/* ^ varnum function is not case sensitive */



/*----------------------------------
CHECK REQUIRED PARAMETERS
----------------------------------*/
%if %length(&input_data.)=0 %then %do;
	%put ERROR: Missing input_data macro parameter;
	%let n_err = %eval(&n_err. + 1);
%end;
%if %length(&sample_size_school.)=0 %then %do;
	%put ERROR: Missing sample_size_school macro parameter;
	%let n_err = %eval(&n_err. + 1);
%end;
%if %length(&district_id.)=0 %then %do;
	%put ERROR: Missing district_id macro parameter;
	%let n_err = %eval(&n_err. + 1);
%end;
%if %length(&school_id.)=0 %then %do;
	%put ERROR: Missing school_id macro parameter;
	%let n_err = %eval(&n_err. + 1);
%end;

%if %length(&method_d.)=0 %then %do;
	%put ERROR: Missing method_d macro parameter;
	%let n_err = %eval(&n_err. + 1);
%end;
%if %length(&method_s.)=0 %then %do;
	%put ERROR: Missing method_s macro parameter;
	%let n_err = %eval(&n_err. + 1);
%end;

/* terminate macro if any errors were triggered */
%if &n_err.>0 %then %put ERROR: Macro terminated because of errors;
%if &n_err.>0 %then %return;



/*----------------------------------
CHECK FOR VALID PARAMETER VALUES
----------------------------------*/
/* all of these are simple checks for whether the specified parameter value is in the list 
of possible values */

/* max_type */
%if %length(&max_type.)>0 & &max_type.~=OVERALL & &max_type.~=STRATUM %then %do;
	%put ERROR: Invalid max_type parameter. The only supported values are OVERALL and STRATUM.;
	%let n_err = %eval(&n_err. + 1);
%end;

/* method_d */
%if %length(&method_d.)>0 & &method_d.~=%str(RANDOM EQUAL) & &method_d.~=%str(RANDOM UNEQUAL) & &method_d.~=BALANCED %then %do;
	%put ERROR: Invalid method_d parameter. The only supported values are RANDOM EQUAL, RANDOM UNEQUAL, and BALANCED.;
	%let n_err = %eval(&n_err. + 1);
%end;

/* method_s */
%if %length(&method_s.)>0 & &method_s.~=%str(RANDOM EQUAL) & &method_s.~=%str(RANDOM UNEQUAL) & &method_s.~=BALANCED %then %do;
	%put ERROR: Invalid method_s parameter. The only supported values are RANDOM EQUAL, RANDOM UNEQUAL, and BALANCED.;
	%let n_err = %eval(&n_err. + 1);
%end;

/* terminate macro if any errors were triggered */
%if &n_err.>0 %then %put ERROR: Macro terminated because of errors;
%if &n_err.>0 %then %return;



/*----------------------------------
CHECK NUMERIC PARAMETERS
----------------------------------*/
/*
These checks are specifically for the sample_size_school and max_schools_per_district parameters. 
We are not checking whether the seed parameter is a number. We will let SAS's own error capture handle 
that, when we set the seed.
*/

/* first check for characters other than digits */
data _null_;
	flag_samp = prxmatch("/[^0-9]/", "&sample_size_school.")>0;
	if flag_samp then put "ERROR: Invalid characters in sample_size_school parameter. The only allowable characters are digits. (And since the number cannot be negative, hyphens are not allowed either.)";
	
	%if %length(&max_schools_per_district.)>0 %then %do;
		flag_cap = prxmatch("/[^0-9]/", "&max_schools_per_district.")>0;
		if flag_cap then put "ERROR: Invalid characters in max_schools_per_district parameter. The only allowable characters are digits. (And since the number cannot be negative, hyphens are not allowed either.)";
	%end; %else %do;
		flag_cap = 0;
	%end;

	any_flag = max(of flag_:);
	
	call symputx("any_flag_mnum_chars", any_flag, "L");
	/* ^ note that there is only one observation in this dataset */
run;

%if &any_flag_mnum_chars.=1 %then %let n_err = %eval(&n_err. + 1);


/* terminate macro if any errors were triggered */
%if &n_err.>0 %then %put ERROR: Macro terminated because of errors;
%if &n_err.>0 %then %return;


/* check that the numbers are positive integers */
data _null_;
	flag_samp = &sample_size_school.~=int(&sample_size_school.) | &sample_size_school.<=0;
	if flag_samp then put "ERROR: Invalid sample_size_school parameter. Must be a positive integer.";
	
	%if %length(&max_schools_per_district.)>0 %then %do;
		flag_cap = &max_schools_per_district.~=int(&max_schools_per_district.) | &max_schools_per_district.<=0;
		if flag_cap then put "ERROR: Invalid max_schools_per_district parameter. Must be a positive integer.";
	%end; %else %do;
		flag_cap = 0;
	%end;

	any_flag = max(of flag_:);
	
	call symputx("any_flag_mnum_pint", any_flag, "L");
	/* ^ note that there is only one observation in this dataset */
run;

%if &any_flag_mnum_pint.=1 %then %let n_err = %eval(&n_err. + 1);


/* terminate macro if any errors were triggered */
%if &n_err.>0 %then %put ERROR: Macro terminated because of errors;
%if &n_err.>0 %then %return;



/*----------------------------------
ASSORTED PARAMETER CHECKS
----------------------------------*/
/* max_schools_per_district and max_type */
%if %length(&max_schools_per_district.)>0 & %length(&max_type.)=0 %then %do;
	%put ERROR: Since you specified the max_schools_per_district macro parameter, the max_type macro parameter cannot be blank.;
	%let n_err = %eval(&n_err. + 1);
%end;

/* if there is no stratification at all (no district and no school), there should not be a targets dataset */
%if %length(&strata_d.)=0 & %length(&strata_s.)=0 & %length(&targets_data.)>0 %then %do;
	%put ERROR: You did not specify any stratification at either the district or school levels (in the strata_d and strata_s macro parameters), yet you did specify a targets dataset (in the targets_data parameter). The targets_data parameter only applies if there is stratification at the district and/or school levels.;
	%let n_err = %eval(&n_err. + 1);
%end;


/* terminate macro if any errors were triggered */
%if &n_err.>0 %then %put ERROR: Macro terminated because of errors;
%if &n_err.>0 %then %return;



/*----------------------------------
CREATE MINIMAL VERSION OF INPUT DATA
----------------------------------*/
%local minimal;
data; run;
%let minimal = &syslast.;

%local other_vars;
%let other_vars = 
	&strata_d.
	&strata_s.
	&rand_size_d.
	&rand_size_s.
	&bal_distance_d.
	&bal_distance_s.
	;

data &minimal.;
	set &input_data.(keep=
			&district_id.
			&school_id.
			&other_vars.
		);
run;



/*----------------------------------
CHECK FOR VARIABLE CONFLICTS
----------------------------------*/
/* check if variables the macro will create already exist */
%if %varexist(ds=&minimal., var=_temp_varname)=1 %then %do;
	%put ERROR: The macro needs to create a variable called "_temp_varname", but you already have a variable with the same name in your dataset. Please rename this variable, so that it does not conflict with the macro.;
	%let n_err = %eval(&n_err. + 1);
%end;

%if %varexist(ds=&minimal., var=_temp_found)=1 %then %do;
	%put ERROR: The macro needs to create a variable called "_temp_found", but you already have a variable with the same name in your dataset. Please rename this variable, so that it does not conflict with the macro.;
	%let n_err = %eval(&n_err. + 1);
%end;


/* terminate macro if any errors were triggered */
%if &n_err.>0 %then %put ERROR: Macro terminated because of errors;
%if &n_err.>0 %then %return;


/* check if any variables beginning with _x_ exist */
data _null_;
	if 0=1 then set &minimal.;
	length _temp_varname $32;
	_temp_found = 0;
	do until (_temp_found=1 | missing(_temp_varname));
		call vnext(_temp_varname);
		if find(_temp_varname, "_x_", "i")=1 then _temp_found=1;
	end;
	call symputx("_x_found", _temp_found, "L");
	stop;
run;

%if &_x_found.=1 %then %do;
	%put ERROR: The macro needs to create a series of variables whose names begin with the string "_x_", but you already have at least one variable whose name starts with the same string, in your dataset. Please rename any such variables, so that they do not conflict with the macro.;
	%let n_err = %eval(&n_err. + 1);
%end;


/* terminate macro if any errors were triggered */
%if &n_err.>0 %then %put ERROR: Macro terminated because of errors;
%if &n_err.>0 %then %return;



/*----------------------------------
CHECK FOR MISSING DATA
----------------------------------*/
/* check for missing IDs */
data _null_;
	set &minimal. end=last;
	_x_flag_miss_id_d = missing(&district_id.);
	if _x_flag_miss_id_d then put "ERROR: Observation missing district ID (&district_id.)";
	_x_flag_miss_id_s = missing(&school_id.);
	if _x_flag_miss_id_s then put "ERROR: Observation missing school ID (&school_id.)";
	
	_x_any_flag_row = max(of _x_flag_:);
	
	retain _x_any_flag_miss_id 0;
	if _x_any_flag_row then _x_any_flag_miss_id = 1;
	if last then call symputx("any_flag_miss_id", _x_any_flag_miss_id, "L");
run;

%if &any_flag_miss_id.=1 %then %let n_err = %eval(&n_err. + 1);


/* check for missing data among other variables */
data _null_;
	set &minimal.(drop=&district_id. &school_id.) end=last;
	_x_n_miss = sum(
		nmiss(of _numeric_)-1,
		cmiss(of _character_, "dummy string")
	);
	/* ^ a few notes:
	** _x_nmiss(of _numeric_)-1 because _x_n_miss is, itself, a numeric variable (created in 
	the PDV before SAS determines the _numeric_ list) and will be missing before 
	evaluation (and we do not have to worry about the first argument of sum evaluating 
	to -1 since the missingness in _x_n_miss ensures at least 1-1=0).
	** Because _x_n_miss is, itself, a numeric variable, we also do not have to worry about 
	cases where nmiss() would not have enough arguments because there are no numeric 
	variables.
	** However, we do have to address cases where there are no character variables. 
	To prevent an error in cmiss() for not enough arguments if there are no character 
	variables, we add a dummy string.
	*/
	_x_flag_miss_data = _x_n_miss>0;
	if _x_flag_miss_data then put "ERROR: You have missing data in one or more of the following cluster variables: &other_vars..";
	
	retain _x_any_flag_miss_data 0;
	if _x_flag_miss_data then _x_any_flag_miss_data = 1;
	if last then call symputx("any_flag_miss_data", _x_any_flag_miss_data, "L");
run;

%if &any_flag_miss_data.=1 %then %let n_err = %eval(&n_err. + 1);


/* terminate macro if any errors were triggered */
%if &n_err.>0 %then %put ERROR: Macro terminated because of errors;
%if &n_err.>0 %then %return;



/*----------------------------------
CHECK OBSERVATIONS
----------------------------------*/
/*
We will do a couple different kinds of checks here. 
We will check that there is only one observation per school ID. 
We will also check that there are enough observations to sample.
*/
%local check_structure;
data; run;
%let check_structure = &syslast.;

/* collapse the data so that there is only one observation per school_id */
proc sort data=&minimal. out=&check_structure. nodupkey;
	by &school_id.;
run;

/* run checks */
data _null_;
	if 0=1 then do;
		set &minimal. nobs=_x_n_orig;
		set &check_structure. nobs=_x_n_collapsed;
	end;
	
	/** compare the number of observations to see if the collapsed dataset is smaller **/
	_x_flag_obs_struc = _x_n_orig>_x_n_collapsed;
	if _x_flag_obs_struc then put "ERROR: There must only be one observation per unique school ID (&school_id.) in the input_data.";
	
	/** check observations vs. sample size **/
	_x_flag_obs_fewer = _x_n_orig<&sample_size_school.;
	if _x_flag_obs_fewer then put "ERROR: There are not enough schools in the input_data to fill your requested sample size. The requested school sample size (&sample_size_school.) is greater than the total number of observations in the input_data: " _x_n_orig;
	_x_flag_obs_equal = _x_n_orig=&sample_size_school.;
	if _x_flag_obs_equal then put "ERROR: Your requested sample size (&sample_size_school.) equals the total number of observations in the input_data. There is no sampling for the macro to perform.";
	/* ^ using _x_n_orig here rather than _x_n_collapsed, but it does not matter either way 
	because if the two do not match, it will be flagged anyways */
	
	_x_any_flag = max(of _x_flag_:);
	
	call symputx("flag_obs", _x_any_flag, "L");
	
	stop;
run;

%if &flag_obs.=1 %then %let n_err = %eval(&n_err. + 1);

proc datasets library=work nolist;
	delete 	%scan(&check_structure.,2)
			;
quit;


/* terminate macro if any errors were triggered */
%if &n_err.>0 %then %put ERROR: Macro terminated because of errors;
%if &n_err.>0 %then %return;



/*----------------------------------
CHECK CONSISTENCY OF DISTRICT-LEVEL DATA
----------------------------------*/
%local check_consistency;
data; run;
%let check_consistency = &syslast.;

%local district_vars;
%let district_vars = 
	&strata_d.
	&rand_size_d.
	&bal_distance_d.
	;

%if %length(&district_vars.)>0 %then %do;
	proc sql;
		create table &check_consistency. as 
			select		&district_id.
						%do i=1 %to %sysfunc(countw(&district_vars.));
							%let var = %scan(&district_vars., &i.);
							,count(distinct &var.) as _x_n_v&i.
						%end;
			from		&minimal.
			group by	&district_id.;
	quit;

	data _null_;
		set &check_consistency. end=last;
		length _x_var $32;
		array nvars{*} _x_n_v:;
		do i=1 to dim(nvars);
			_x_var = scan("&district_vars.", i);
			if nvars{i}>1 then put "ERROR: Multiple values detected within a single district ID (&district_id.) for district-level variable " _x_var ", where &district_id. = " &district_id.;
		end;
		drop i;
		_x_any_flag = max(of _x_n_v:)>1;
		
		retain _x_any_flag_consistency 0;
		if _x_any_flag then _x_any_flag_consistency = 1;
		if last then call symputx("any_flag_consistency", _x_any_flag_consistency, "L");
	run;

	%if &any_flag_consistency.=1 %then %let n_err = %eval(&n_err. + 1);
%end;

proc datasets library=work nolist;
	delete 	%scan(&check_consistency.,2)
			;
quit;

/* terminate macro if any errors were triggered */
%if &n_err.>0 %then %put ERROR: Macro terminated because of errors;
%if &n_err.>0 %then %return;



/*----------------------------------
CREATE STRATUM IDENTIFIERS
----------------------------------*/
/* datasets */
%local stratid_prep stratid_prep_final stratum_xwalk;
data; run;
%let stratid_prep = &syslast.;
data; run;
%let stratid_prep_final = &syslast.;
data; run;
%let stratum_xwalk = &syslast.;


/* generate code for first.[stratum variables] */
%local strata_d_first strata_s_first;
%let strata_d_first = first.%sysfunc(transtrn(&strata_d., %str( ), %str(, first.)));
%let strata_s_first = first.%sysfunc(transtrn(&strata_s., %str( ), %str(, first.)));


/* create stratum IDs */
/** district stratum **/
%if %length(&strata_d.)>0 %then %do;
	proc sort data=&minimal.; by &strata_d.; run;
	data &stratid_prep.;
		set &minimal. end=last;
		by &strata_d.;
		retain _x_stratum_d 0;
		if max(&strata_d_first.) then _x_stratum_d = sum(_x_stratum_d, 1);
		
		if last then call symputx("n_strat_d", _x_stratum_d, "L");
	run;
%end; %else %do;
	data &stratid_prep.;
		set &minimal.;
		_x_stratum_d = 1;
		if _n_=1 then call symputx("n_strat_d", _x_stratum_d, "L");
	run;
%end;

/** school stratum **/
%if %length(&strata_s.)>0 %then %do;
	proc sort data=&stratid_prep.; by &strata_s.; run;
	data &stratid_prep_final.;
		set &stratid_prep. end=last;
		by &strata_s.;
		retain _x_stratum_s 0;
		if max(&strata_s_first.) then _x_stratum_s = sum(_x_stratum_s, 1);
		
		if last then call symputx("n_strat_s", _x_stratum_s, "L");
	run;
%end; %else %do;
	data &stratid_prep_final.;
		set &stratid_prep.;
		_x_stratum_s = 1;
		if _n_=1 then call symputx("n_strat_s", _x_stratum_s, "L");
	run;
%end;

proc datasets library=work nolist;
	delete 	%scan(&minimal.,2)
			%scan(&stratid_prep.,2)
			;
quit;


/* create stratum crosswalk */
proc sort data=&stratid_prep_final.(keep=&strata_d. &strata_s. _x_stratum_d _x_stratum_s) out=&stratum_xwalk. nodupkey;
	by _all_;
run;



/*----------------------------------
CHECK STRATUM TARGETS DATASET: Part 1, Variable Attributes
----------------------------------*/
%if %length(&targets_data.)>0 %then %do;
	
	
	/* datasets */
	%local contents_strat_xwalk0 contents_user_targ0 contents_strat_xwalk contents_user_targ contents_merged;
	data; run;
	%let contents_strat_xwalk0 = &syslast.;
	data; run;
	%let contents_user_targ0 = &syslast.;
	data; run;
	%let contents_strat_xwalk = &syslast.;
	data; run;
	%let contents_user_targ = &syslast.;
	data; run;
	%let contents_merged = &syslast.;


	/* contents */
	proc contents data=&stratum_xwalk.(drop=_x_stratum_d _x_stratum_s) out=&contents_strat_xwalk0.(keep=name type length) noprint;
	run;

	proc contents data=&targets_data. out=&contents_user_targ0.(keep=name type length) noprint;
	run;

	data &contents_strat_xwalk.;
		set &contents_strat_xwalk0.;
		length lowvar $32;
		lowvar = strip(lowcase(name));
		drop name;
		rename	type=type_data
				length=length_data
				;
	run;

	data &contents_user_targ.;
		set &contents_user_targ0.;
		length lowvar $32;
		lowvar = strip(lowcase(name));
		drop name;
		rename	type=type_targ
				length=length_targ
				;
	run;

	proc sort data=&contents_strat_xwalk.; by lowvar; run;
	proc sort data=&contents_user_targ.; by lowvar; run;
	data &contents_merged.;
		merge &contents_strat_xwalk.(in=indata) &contents_user_targ.(in=intarg);
		by lowvar;
		error_miss = indata & not intarg;
		if error_miss then put "ERROR: Your targets dataset (specified in the targets_data macro parameter) must have the exact same stratification variables that you specified in strata_d and strata_s. The following variable was specified in the strata parameters but not found in your targets_data: " lowvar;
		error_type = indata & intarg & type_data~=type_targ;
		if error_type then put "ERROR: The stratification variables in your targets dataset (as specified in the targets_data macro parameter) must be of the same type (character/numeric) as their counterparts in your input data. You have a mismatch between the two sources for the following variable: " lowvar= type_data= type_targ=;
		warn_length = indata & intarg & type_data=type_targ & length_data~=length_targ;
		if warn_length then put "WARNING: You specified different lengths for the stratification variables, between your targets dataset (as specified in the targets_data macro parameter) and your input data. This may produce unexpected results. You have a mismatch in the length for the following variable: " lowvar= length_data= length_targ=;
		error_target_type = intarg & lowvar="target" & type_targ~=1; /* 1=numeric, 2=character */
		if error_target_type then put "ERROR: The target variable in the targets_data must be numeric.";
		
		any_error = max(of error_:);
	run;
	
	%local any_flag_targ_attr;
	proc sql noprint;
		select	max(any_error)
		into	:any_flag_targ_attr trimmed
		from	&contents_merged.;
	quit;
	
	%if &any_flag_targ_attr.=1 %then %let n_err = %eval(&n_err. + 1);


	/* clean up */
	proc datasets library=work nolist;
		delete 	%scan(&contents_strat_xwalk0.,2) %scan(&contents_user_targ0.,2)
				%scan(&contents_strat_xwalk.,2) %scan(&contents_user_targ.,2)
				%scan(&contents_merged.,2)
				;
	quit;
	
	
%end;


/* terminate macro if any errors were triggered */
%if &n_err.>0 %then %put ERROR: Macro terminated because of errors;
%if &n_err.>0 %then %return;



/*----------------------------------
CHECK STRATUM TARGETS DATASET: Part 2, Assorted Checks
----------------------------------*/
/* check for target variable */
%if %length(&targets_data.)>0 %then %do;
	%if %varexist(ds=&targets_data., var=target)=0 %then %do;
		%put ERROR: Missing target variable in the targets_data.;
		%let n_err = %eval(&n_err. + 1);
	%end;
%end;


/* terminate macro if any errors were triggered */
%if &n_err.>0 %then %put ERROR: Macro terminated because of errors;
%if &n_err.>0 %then %return;


/* checking the values of the target variable */
%if %length(&targets_data.)>0 %then %do;
	data _null_;
		set &targets_data. end=last;
		_x_flag_miss = missing(target);
		if _x_flag_miss then put "ERROR: Missing data for target variable in the targets_data, in the following observation: " _n_=;
		_x_flag_low = target<0 & not missing(target);
		if _x_flag_low then put "ERROR: In the targets_data, the target cannot be less than zero. The following observation in the targets_data violates that: " _n_= target=;
		_x_flag_int = target~=int(target);
		if _x_flag_int then put "ERROR: In the targets_data, the target must be an integer. The following observation in the targets_data violates that: " _n_= target=;
		
		_x_any_flag = max(of _x_flag_:);
		
		retain _x_any_flag_targ_val 0;
		if _x_any_flag then _x_any_flag_targ_val = 1;
		if last then call symputx("any_flag_targ_val", _x_any_flag_targ_val, "L");
	run;
	
	%if &any_flag_targ_val.=1 %then %let n_err = %eval(&n_err. + 1);
%end;


/* terminate macro if any errors were triggered */
%if &n_err.>0 %then %put ERROR: Macro terminated because of errors;
%if &n_err.>0 %then %return;


/* check to make sure there is only one observation per stratum */
%if %length(&targets_data.)>0 %then %do;
	%local check_targets_obs;
	data; run;
	%let check_targets_obs = &syslast.;
	
	proc sort data=&targets_data. out=&check_targets_obs. nodupkey;
		by &strata_d. &strata_s.;
	run;
	
	data _null_;
		if 0=1 then do;
			set &targets_data. nobs=_x_n_orig;
			set &check_targets_obs. nobs=_x_n_collapsed;
		end;
		_x_flag = _x_n_orig>_x_n_collapsed;
		if _x_flag then put "ERROR: There must only be one observation per unique combination of stratification variables in the targets_data.";
		call symputx("flag_targ_obs", _x_flag, "L");
		stop;
	run;
	
	%if &flag_targ_obs.=1 %then %let n_err = %eval(&n_err. + 1);
	
	proc datasets library=work nolist;
		delete 	%scan(&check_targets_obs.,2)
				;
	quit;
%end;


/* terminate macro if any errors were triggered */
%if &n_err.>0 %then %put ERROR: Macro terminated because of errors;
%if &n_err.>0 %then %return;


/* check whether targets add up to sample size */
%if %length(&targets_data.)>0 %then %do;
	%local user_targ_sum;
	proc sql noprint;
		select	sum(target)
		into	:user_targ_sum trimmed
		from	&targets_data.;
	quit;
	
	%if &user_targ_sum.~=&sample_size_school. %then %do;
		%put ERROR: The sum of the target variable in the targets_data (&user_targ_sum.) does not equal your requested sample size (&sample_size_school., from the sample_size_school parameter).;
		%let n_err = %eval(&n_err. + 1);
	%end;
%end;


/* terminate macro if any errors were triggered */
%if &n_err.>0 %then %put ERROR: Macro terminated because of errors;
%if &n_err.>0 %then %return;



/*----------------------------------
CHECK STRATUM TARGETS DATASET: Part 3, Merge to Stratum Crosswalk
----------------------------------*/
/* generate code for PUT statement below */
%local strata_equal;
%let strata_equal = %sysfunc(transtrn(%trim(&strata_d. &strata_s.), %str( ), %str(= )))%str(=);


/* merge user targets with stratum crosswalk and run checks */
%if %length(&targets_data.)>0 %then %do;
	%local user_targets1 user_targets2;
	data; run;
	%let user_targets1 = &syslast.;
	data; run;
	%let user_targets2 = &syslast.;

	proc sort data=&targets_data. out=&user_targets1.; by &strata_d. &strata_s.; run;
	proc sort data=&stratum_xwalk.; by &strata_d. &strata_s.; run;
	data &user_targets2.;
		merge &user_targets1.(in=_x_inuser keep=&strata_d. &strata_s. target) &stratum_xwalk.(in=_x_inxwalk);
		by &strata_d. &strata_s.;
		_x_flag_mismatch1 = _x_inuser & not _x_inxwalk & target>0;
		if _x_flag_mismatch1 then put "ERROR: The following stratum was found in your targets_data, but not in your input_data: " &strata_equal.;
		if not _x_inuser & _x_inxwalk then do;
			target = 0;
			put "NOTE: The following stratum was found in your input_data, but not in your targets_data. The macro will assume that you want the target sample size for this combination to be zero. " &strata_equal.;
		end;
	run;
	
	%local any_flag_targets_merge;
	proc sql noprint;
		select	max(_x_flag_mismatch1)
		into	:any_flag_targets_merge trimmed
		from	&user_targets2.;
	quit;

	%if &any_flag_targets_merge.=1 %then %let n_err = %eval(&n_err. + 1);
	
	proc datasets library=work nolist;
		delete 	%scan(&user_targets1.,2)
				;
	quit;
	/* ^ we will use user_targets2 later in the macro, so not deleting it yet */
%end;


/* terminate macro if any errors were triggered */
%if &n_err.>0 %then %put ERROR: Macro terminated because of errors;
%if &n_err.>0 %then %return;



/*----------------------------------
GENERATE SEED IF NOT SPECIFIED
----------------------------------*/
data _null_;
	call streaminit('pcg', 0);
	/* ^ the zero makes SAS generate a default seed value and store it in the SYSRANDOM macro 
	variable */
run;

%local seed_use;
%if %length(&seed.)=0 %then %let seed_use = &sysrandom.;
%else %let seed_use = &seed.;



/*----------------------------------
CREATE RANDOM VARIABLES
----------------------------------*/
/*
Note that even if the user has not specified random selection, the random 
variables created in this section will still be useful. If the user specifies 
balanced selection, we will use these random variables to break ties (e.g., 
when two districts or schools have the same distance value).
*/


/* datasets */
%local ds_rand_sch ds_rand_dist1 ds_rand_dist2 ds_rand;
data; run;
%let ds_rand_sch = &syslast.;
data; run;
%let ds_rand_dist1 = &syslast.;
data; run;
%let ds_rand_dist2 = &syslast.;
data; run;
%let ds_rand = &syslast.;


/* school-level random variable */
proc sort data=&stratid_prep_final.; by &district_id. &school_id.; run;

data &ds_rand_sch.;
	set &stratid_prep_final.;
	call streaminit('pcg', &seed_use.);
	call stream(0);
	_x_rand_school = rand('uniform');
run;

proc datasets library=work nolist;
	delete 	%scan(&stratid_prep_final.,2)
			;
quit;


/* district-level random variable */
proc sort data=&ds_rand_sch.(keep=&district_id. &rand_size_d.) out=&ds_rand_dist1. nodupkey;
	by &district_id.;
run;
/* ^ keeping rand_size_d for use later */

data &ds_rand_dist2.;
	set &ds_rand_dist1.;
	call streaminit('pcg', &seed_use.);
	call stream(1);
	_x_rand_district = rand('uniform');
run;

proc datasets library=work nolist;
	delete 	%scan(&ds_rand_dist1.,2)
			;
quit;


/* combine */
proc sql;
	create table &ds_rand. as 
		select	a.*,
				b._x_rand_district
		from	&ds_rand_sch. as a left join
				&ds_rand_dist2. as b
				on a.&district_id.=b.&district_id.
				;
quit;

proc datasets library=work nolist;
	delete 	%scan(&ds_rand_sch.,2)
			;
quit;
/* ^ we will use the ds_rand_dist2 dataset in a subsequent step, so we will hold off on deleting 
that dataset until later */



/*----------------------------------
CREATE RANDOM LOTTERY VARIABLE
----------------------------------*/
/*
This section creates the variable we will use to operationalize PPS sampling as a rank-ordered 
list. The variable will be randomly drawn, and it will account for unequal selection probabilities 
(with probabilities proportionate to size). The random variable will be negatively associated with 
size, such that when we sort by the random variable in ascending order, districts/schools that are 
ranked toward the top of the list will tend to have larger size values.

The following summarizes the steps we take below to create this random variable:
1.	Randomly order the districts/schools and assign numeric ranges to each district/school, with the 
	range based on the district/school's size measure.
	(Think of this as assigning a varying number of balls to the district/school for a lottery, with 
	the number of balls in proportion to our size measure; then, when we select the balls sequentially, 
	the districts/schools with more balls have a greater probability of being selected earlier.)
	For instance, if District 1 has a size measure of 5, its range is 1-5; if District 2 has 10, its 
	range is 6-15; and if District 3 has 1, its range is 16-16.
3.	Create a SAS format, associating each district/school's numeric range with the district/school's ID 
	(as its formatted value label).
4.	Select the lottery draws:
		a.	Create a dataset where we generate lottery draw numbers, from 1 to the cumulative total of 
			the size measure (i.e., the max end point of the district/school numeric ranges).
			These draw numbers are the lottery "balls" we will be selecting.
		b.	Randomly order the lottery draw numbers. This random order is the order in which we will be 
			drawing the lottery numbers/balls.
5.	In this dataset of lottery draws, use our lottery format to identify the district/school associated 
	with a particular lottery draw number (i.e., the district/school whose numeric range encompasses 
	that draw number).
6.	Collapse the lottery draw dataset, and compute the minimum draw order for each district/school. This 
	minimum draw order is what we will sort by in sampling (in ascending order).
*/



/* datasets */
%local out_lottery_d out_lottery_s merged_lottery0 merged_lottery;
data; run;
%let out_lottery_d = &syslast.;
data; run;
%let out_lottery_s = &syslast.;
data; run;
%let merged_lottery0 = &syslast.;
data; run;
%let merged_lottery = &syslast.;



/* define an internal macro to create the lottery measure */
%macro lottery(in_data, out_data, fmt, level, size, id, random, stream);
	/** **/
	%local level_fl;
	%let level_fl = %substr(&level., 1, 1);


	/** datasets **/
	%local lottery_ranges lottery_formats lottery_draws1 lottery_draws2 lottery_draws3 lottery_draws_min;
	data; run;
	%let lottery_ranges = &syslast.;
	data; run;
	%let lottery_formats = &syslast.;
	data; run;
	%let lottery_draws1 = &syslast.;
	data; run;
	%let lottery_draws2 = &syslast.;
	data; run;
	%let lottery_draws3 = &syslast.;
	data; run;
	%let lottery_draws_min = &syslast.;
	
	
	/** randomly sort districts/schools **/
	proc sort data=&in_data.; by &random.; run;


	/** assign numeric ranges to each district/school **/
	data &lottery_ranges.;
		set &in_data.(keep=&id. &size.) end=last;
		_x_size = ceil(&size.);

		/*** flags ***/
		_x_flag_neg = &size.<0;
		if _x_flag_neg then put "ERROR: Negative values are prohibited in the rand_size_&level_fl. variable (&size.): " &id.= &size.=;
		_x_flag_zero = &size.=0;
		if _x_flag_zero then put "ERROR: The rand_size_&level_fl. variable (&size.) cannot equal zero for any observation (if you want an observation to have a selection probability of zero, please exclude it from the input dataset): " &id.= &size.=;
		_x_any_flag_error = max(of _x_flag_:);
		_x_warn_flag_int = &size.~=_x_size;

		/*** continue processing ***/
		retain _x_end;
		if _n_=1 then do;
			_x_start = 1;
			_x_end = _x_size;
		end; else do;
			_x_start = _x_end + 1;
			_x_end = sum(_x_end, _x_size);
		end;
		/* ^ _x_end - _x_start + 1 will equal _x_size */
		
		_x_temp_id = _n_;
		/* ^ We are using this temporary ID rather than the existing district or school ID in 
		the dataset for two main reasons:
			1) so that we do not have to account for whether the ID is character/numeric
			2) so that we do not have to worry about how the original ID treated leading and trailing 
			blanks if it was character (which would be important when we recreate the ID from the 
			numeric format we are creating below)
		*/
		
		if last then do;
			call symputx("lab_len_lotto", max(length(strip(_x_temp_id)), 11), "L");
			/* ^ the 11 is so that it is long enough to accommodate the string ***ERROR*** */
			call symputx("nobs_lotto", _x_end, "L");
		end;
		
		drop &size.;
		rename &id.=_x_orig_id;
		/* ^ so that the ID does not conflict with the variables we will create in 
		the lottery_formats dataset (which must have specific names) */
	run;

	%local any_error_flag_lotto any_warn_flag_int_lotto lotto_min_size lotto_max_size;
	proc sql noprint;
		select	max(_x_any_flag_error),
				max(_x_warn_flag_int),
				min(_x_size),
				max(_x_size)
		into	:any_error_flag_lotto trimmed,
				:any_warn_flag_int_lotto trimmed,
				:lotto_min_size trimmed,
				:lotto_max_size trimmed
		from	&lottery_ranges.;
	quit;

	%if &any_warn_flag_int_lotto.=1 %then %put WARNING: Non-integer values detected in the rand_size_&level_fl. variable (&size.). The macro will take the ceiling of these values.;
	%if %sysevalf(&lotto_max_size. - &lotto_min_size.)=0 %then %put WARNING: There is no variation in the rand_size_&level_fl. variable (&size.) after applying the CEIL function. Effectively, that means each unit within a stratum will have the same selection probability.;
	%if &any_error_flag_lotto.=1 %then %let n_err = %eval(&n_err. + 1);
	%if &any_error_flag_lotto.=1 %then %return;
	
	
	/** determine the minimum length we can use to store the draw number and order **/
	%local len_lotto;
	%if 		&nobs_lotto.<=8192 				%then %let len_lotto = 3;
	%else %if 	&nobs_lotto.<=2097152 			%then %let len_lotto = 4;
	%else %if 	&nobs_lotto.<=536870912 		%then %let len_lotto = 5;
	%else %if 	&nobs_lotto.<=137438953472 		%then %let len_lotto = 6;
	%else %if 	&nobs_lotto.<=35184372088832 	%then %let len_lotto = 7;
	%else 											  %let len_lotto = 8;
	
	
	/** extract existing fmtsearch option **/
	%local orig_fmtsearch;
	%let orig_fmtsearch = %sysfunc(getoption(fmtsearch));
	
	
	/** search for an unused format catalog name, in the work library **/
	/* This will roughly mimic the DATAn naming convention, but for format catalogs. However, 
	we will not search as long as the DATAn convention does (from 1 to 9999). We will 
	only search from 1 to 99. */
	%local unused_cat_name cat_iter last_cat_num final_cat_name;
	%let unused_cat_name = 0;
	%let cat_iter = 1;
	%do %until(&cat_iter.=100 | &unused_cat_name.=1);
		%let last_cat_num = &cat_iter.;
		%if %sysfunc(cexist(work._fmt&cat_iter.))=0 %then %let unused_cat_name = 1;
		%let cat_iter = %eval(&cat_iter. + 1);
	%end;
	%let final_cat_name = work._fmt&last_cat_num.;
	
	
	/** create lottery formats, based on the assigned numeric ranges **/
	data &lottery_formats.;
		set &lottery_ranges.;
		length fmtname $32 start end label $&lab_len_lotto. type $1;
		fmtname = "&fmt.";
		start = strip(_x_start);
		end = strip(_x_end);
		label = strip(_x_temp_id);
		type = "N";
	run;

	proc format library=&final_cat_name. cntlin=&lottery_formats.;
	run;
	options fmtsearch=(&final_cat_name.);
	
	proc datasets library=work nolist;
		delete 	%scan(&lottery_formats.,2)
				;
	quit;
	
	
	/** select random draws **/
	data &lottery_draws1.;
		length _x_draw_number &len_lotto.;
		do _x_draw_number=1 to &nobs_lotto.;
			output;
		end;
	run;

	data &lottery_draws2.;
		set &lottery_draws1.;
		call streaminit('pcg', &seed_use.);
		call stream(&stream.);
		_x_random = rand('uniform');
	run;
	
	proc datasets library=work nolist;
		delete 	%scan(&lottery_draws1.,2)
				;
	quit;

	proc sort data=&lottery_draws2.; by _x_random; run;
	
	
	/** identify the district/school associated with each draw **/
	data &lottery_draws3.;
		set &lottery_draws2.;
		length _x_draw_order &len_lotto.;
		_x_draw_order = _n_;
		_x_temp_id = input(put(_x_draw_number,&fmt..),32.);
	run;
	
	proc datasets library=work nolist;
		delete 	%scan(&lottery_draws2.,2)
				;
	quit;
	
	
	/** reset fmtsearch option back to original **/
	options fmtsearch=&orig_fmtsearch.;
	/* ^ no parentheses since the parentheses are already in the macro variable */
	
	
	/** take the minimum draw order for each district/school **/
	proc means data=&lottery_draws3. noprint nway;
		class _x_temp_id;
		var _x_draw_order;
		output out=&lottery_draws_min.(drop=_type_) min(_x_draw_order)=_x_lotto_&level.;
	run;
	
	proc datasets library=work nolist;
		delete 	%scan(&lottery_draws3.,2)
				;
	quit;
	
	
	/** attach the original district/school ID **/
	proc sql;
		create table &out_data. as 
			select	a._x_orig_id as &id.,
					b._x_lotto_&level.
			from	&lottery_ranges. as a left join
					&lottery_draws_min. as b
					on a._x_temp_id=b._x_temp_id
					;
	quit;
	
	proc datasets library=work nolist;
		delete 	%scan(&lottery_ranges.,2)
				%scan(&lottery_draws_min.,2)
				;
	quit;

%mend lottery;



/* district call */
%if &method_d.=%str(RANDOM UNEQUAL) %then %do;
	%lottery(in_data=&ds_rand_dist2., out_data=&out_lottery_d., fmt=lottery_d_f, level=district, size=&rand_size_d., id=&district_id., random=_x_rand_district, stream=2);
%end; %else %do;
	data &out_lottery_d.;
		set &ds_rand_dist2.(keep=&district_id.);
		_x_lotto_district = .;
	run;
%end;

/* terminate macro if any errors were triggered */
%if &n_err.>0 %then %put ERROR: Macro terminated because of errors;
%if &n_err.>0 %then %return;

proc datasets library=work nolist;
	delete 	%scan(&ds_rand_dist2.,2)
			;
quit;



/* school call */
%if &method_s.=%str(RANDOM UNEQUAL) %then %do;
	%lottery(in_data=&ds_rand., out_data=&out_lottery_s., fmt=lottery_s_f, level=school, size=&rand_size_s., id=&school_id., random=_x_rand_school, stream=3);
%end; %else %do;
	data &out_lottery_s.;
		set &ds_rand.(keep=&school_id.);
		_x_lotto_school = .;
	run;
%end;

/* terminate macro if any errors were triggered */
%if &n_err.>0 %then %put ERROR: Macro terminated because of errors;
%if &n_err.>0 %then %return;



/* assemble */
proc sql;
	create table &merged_lottery0. as
		select	a.*,
				b._x_lotto_district
		from	&ds_rand. as a left join
				&out_lottery_d. as b
				on a.&district_id.=b.&district_id.
				;
	create table &merged_lottery. as
		select	a.*,
				b._x_lotto_school
		from	&merged_lottery0. as a left join
				&out_lottery_s. as b
				on a.&school_id.=b.&school_id.
				;
quit;

proc datasets library=work nolist;
	delete 	%scan(&ds_rand.,2)
			%scan(&merged_lottery0.,2)
			%scan(&out_lottery_d.,2)
			%scan(&out_lottery_s.,2)
			;
quit;



/*----------------------------------
COUNT SCHOOLS BY DISTRICT
----------------------------------*/
/* datasets */
%local dist_sch_counts0 dist_sch_counts;
data; run;
%let dist_sch_counts0 = &syslast.;
data; run;
%let dist_sch_counts = &syslast.;


/* total schools by district */
proc sql;
	create table &dist_sch_counts0. as 
		select		_x_stratum_d,
					%if %length(&strata_d.)>0 %then %do;
						&strata_d_comma.,
					%end;
					&district_id.,
					%if %length(&bal_distance_d.)>0 & &method_d.=BALANCED %then %do;
						&bal_distance_d.,
					%end;
					_x_rand_district,
					_x_lotto_district,
					count(*) as _x_tot_nsch
					%do strat_s=1 %to &n_strat_s.;
						,sum(case when _x_stratum_s=&strat_s. then 1 else 0 end) as _x_nsch_strat_s_&strat_s.
					%end;
		from		&merged_lottery.
		group by	_x_stratum_d,
					%if %length(&strata_d.)>0 %then %do;
						&strata_d_comma.,
					%end;
					&district_id.,
					%if %length(&bal_distance_d.)>0 & &method_d.=BALANCED %then %do;
						&bal_distance_d.,
					%end;
					_x_rand_district,
					_x_lotto_district
					;
quit;


/* if the user specified a cap, apply that cap */
data &dist_sch_counts.;
	set &dist_sch_counts0.;
	%if %length(&max_schools_per_district.)>0 & &max_type.=OVERALL & &n_strat_s.>1 %then %do;
		/* Scenario #1: overall cap & 2+ school strata - allocate the cap proportionally, across school strata */
		/*
		In this scenario, there is a maximum total number of schools we can sample per district. And since there is more than 
		one school stratum, we need to allocate that cap across school strata.
		*/
		
		/** capped count of the total number of schools per district **/
		_x_tot_nsch_c = min(_x_tot_nsch,&max_schools_per_district.);
		
		/** loop over school strata **/
		%do strat_s=1 %to &n_strat_s.;
			if _x_tot_nsch>0 then _x_temp_strat_s_&strat_s. = _x_tot_nsch_c * _x_nsch_strat_s_&strat_s. / _x_tot_nsch;
			else _x_temp_strat_s_&strat_s. = 0;
			
			_x_round_strat_s_&strat_s. = round(_x_temp_strat_s_&strat_s.);
			_x_floor_strat_s_&strat_s. = floor(_x_temp_strat_s_&strat_s.);
			_x_ceil_strat_s_&strat_s. = ceil(_x_temp_strat_s_&strat_s.);
			
			_x_nsch_c_strat_s_&strat_s. = _x_round_strat_s_&strat_s.;
			if _x_nsch_c_strat_s_&strat_s.=0 & _x_ceil_strat_s_&strat_s.=1 then _x_nsch_c_strat_s_&strat_s. = _x_ceil_strat_s_&strat_s.;
			/* ^ this modification means the sum would be larger than it should be since i have not compensated by lowering 
			the count in another stratum, but that will be taken care of when we adjust the capped counts below */
		%end;
		
		/** adjust the capped school stratum counts to ensure they add up to the capped total **/
		if sum(of _x_nsch_c_strat_s_:)~=_x_tot_nsch_c then do;
			%do strat_s=1 %to &n_strat_s.;
				if 		sum(of _x_nsch_c_strat_s_:)<_x_tot_nsch_c then _x_nsch_c_strat_s_&strat_s. = _x_ceil_strat_s_&strat_s.;
				else if sum(of _x_nsch_c_strat_s_:)>_x_tot_nsch_c then _x_nsch_c_strat_s_&strat_s. = _x_floor_strat_s_&strat_s.;
			%end;
		end;
		_x_flag = sum(of _x_nsch_c_strat_s_:)~=_x_tot_nsch_c;
		if _x_flag then put "ERROR: Unexpected error (either an issue in the macro logic or macro inputs). sum(of _x_nsch_c_strat_s_:)~=_x_tot_nsch_c";
		/* ^ we are not terminating the macro for this error */
		
		/** clean up **/
		drop _x_temp_: _x_round_: _x_floor_: _x_ceil_:;
	%end; %else %if %length(&max_schools_per_district.)>0 & &max_type.=STRATUM & &n_strat_s.>1 %then %do;
		/* Scenario #2: stratum-specific cap - apply the cap separately, to each school stratum (and no need to apportion it) */
		/*
		In this scenario, there is a maximum number of schools we can sample per school stratum per district. Therefore, we apply 
		the maximum to each school stratum separately. We do not need to apportion the cap across school strata.
		*/
		
		/** loop over school strata **/
		%do strat_s=1 %to &n_strat_s.;
			_x_nsch_c_strat_s_&strat_s. = min(_x_nsch_strat_s_&strat_s.,&max_schools_per_district.);
		%end;
		
		/** capped count of the total number of schools per district **/
		_x_tot_nsch_c = sum(of _x_nsch_c_strat_s_:);
		
		_x_flag = 0;
	%end; %else %if %length(&max_schools_per_district.)>0 & &n_strat_s.=1 %then %do;
		/* Scenario #3: there is a cap (either overall or stratum-specific), but only one school stratum */
		
		/** capped count of the total number of schools per district **/
		_x_tot_nsch_c = min(_x_tot_nsch,&max_schools_per_district.);
		
		/** only one school stratum **/
		_x_nsch_c_strat_s_1 = _x_tot_nsch_c;
		
		_x_flag = 0;
	%end; %else %do;
		/* Scenario #4: no cap */
		
		/** UNcapped count of the total number of schools per district **/
		/*** uncapped same as capped in this case ***/
		_x_tot_nsch_c = _x_tot_nsch;
		
		/** loop over school strata **/
		/*** uncapped same as capped in this case ***/
		%do strat_s=1 %to &n_strat_s.;
			_x_nsch_c_strat_s_&strat_s. = _x_nsch_strat_s_&strat_s.;
		%end;
		
		_x_flag = 0;
	%end;
	drop _x_flag;
run;

proc datasets library=work nolist;
	delete 	%scan(&dist_sch_counts0.,2)
			;
quit;



/*----------------------------------
COMPUTE TARGETS BY STRATUM: Part 1, Default Allocation
----------------------------------*/
/* datasets */
%local targets_prep1 targets_prep2 targets_prep3 targets_wide targets_strat_d targets_strat_all;
data; run;
%let targets_prep1 = &syslast.;
data; run;
%let targets_prep2 = &syslast.;
data; run;
%let targets_prep3 = &syslast.;
data; run;
%let targets_wide = &syslast.;
data; run;
%let targets_strat_d = &syslast.;
data; run;
%let targets_strat_all = &syslast.;


/* collapse school counts (by school stratum) to the district stratum level */
proc means data=&dist_sch_counts. noprint nway;
	class _x_stratum_d;
	var _x_nsch:;
	output out=&targets_prep1.(drop=_type_ _freq_) sum=;
run;


/* transpose counts so that all are in one observation */
data &targets_prep2.;
	set &targets_prep1.;
	length vname $32;
	array vars _x_nsch:;
	do over vars;
		vname = vname(vars);
		vvalue = vars;
		output;
	end;
	keep _x_stratum_d vname vvalue;
run;

proc datasets library=work nolist;
	delete 	%scan(&targets_prep1.,2)
			;
quit;

proc transpose data=&targets_prep2. out=&targets_prep3.(drop=_name_) delim=_d_;
	id vname _x_stratum_d;
	var vvalue;
run;

proc datasets library=work nolist;
	delete 	%scan(&targets_prep2.,2)
			;
quit;


/* determine targets */
data &targets_wide.;
	set &targets_prep3. end=last;
	/* count total number of schools */
	_x_tot_nsch = sum(of _x_nsch_strat_:);
	_x_tot_nsch_c = sum(of _x_nsch_c_strat_:);
	/* ^ the capped total (_x_tot_nsch_c) is the overall maximum number of schools we can sample from the sampling frame */
	
	/* flags */
	_x_flag_size_c = _x_tot_nsch_c<&sample_size_school. & _x_tot_nsch>=&sample_size_school.;
	if _x_flag_size_c then put "ERROR: There are not enough schools in the sampling frame to fill your requested sample size, after taking into account the maximum number of schools to be sampled per district (as specified in the max_schools_per_district and max_type macro parameters). The requested school sample size (&sample_size_school.) is greater than the overall maximum number of schools that could be sampled from the sampling frame: " _x_tot_nsch_c;
	
	retain _x_any_flag 0;
	if _x_flag_size_c then _x_any_flag = 1;
	if last then call symputx("any_flag_tw", _x_any_flag, "L");
	
	
	
	/* process targets */
	/**************************************************
	BLOCK 1
	**************************************************/
	/* begin loop over district strata */
	%do strat_d=1 %to &n_strat_d.;
		
		/* begin loop over school strata */
		%do strat_s=1 %to &n_strat_s.;
			/* simple count based on total sample size target multiplied by stratum percentage */
			_x_temp1_strat_s_&strat_s._d_&strat_d. = &sample_size_school. * _x_nsch_strat_s_&strat_s._d_&strat_d. / _x_tot_nsch;
			
			/* check whether we have enough capped schools to meet that target and adjust */
			_x_temp2_strat_s_&strat_s._d_&strat_d. = min(_x_temp1_strat_s_&strat_s._d_&strat_d.,_x_nsch_c_strat_s_&strat_s._d_&strat_d.);
			
			/* calculate the remainder, between what we would sample (temp1) and what we can (temp2) */
			_x_remainder_strat_s_&strat_s._d_&strat_d. = _x_temp1_strat_s_&strat_s._d_&strat_d. - _x_temp2_strat_s_&strat_s._d_&strat_d.;
			
			/* if we have enough to sample in a particular stratum (remainder=temp1-temp2=0), then we may have a 
			reserve in that stratum, which we can use to fill the sample for strata where we did not have enough 
			capped schools. This variable counts how much of that reserve we have. */
			_x_reserve_strat_s_&strat_s._d_&strat_d. = ifn(
				_x_remainder_strat_s_&strat_s._d_&strat_d. = 0,
				_x_nsch_c_strat_s_&strat_s._d_&strat_d. - _x_temp2_strat_s_&strat_s._d_&strat_d.,
				0
			);
			
			/* the overall frequency of the stratum in the population, but specifically for those strata where 
			we have a reserve */
			_x_nsch_adj_strat_s_&strat_s._d_&strat_d. = ifn(
				_x_reserve_strat_s_&strat_s._d_&strat_d. > 0,
				_x_nsch_strat_s_&strat_s._d_&strat_d.,
				0
			);
		%end;
		/* end loop over school strata */
		
	%end;
	/* end loop over district strata */
	
	
	/**************************************************
	BLOCK 2
	**************************************************/
	_x_tot_remainder = sum(of _x_remainder_:);
	_x_tot_reserve = sum(of _x_reserve_:);
	_x_tot_nsch_adj = sum(of _x_nsch_adj_strat_:);
	
	/* begin loop over district strata */
	%do strat_d=1 %to &n_strat_d.;
	
		/* begin loop over school strata */
		%do strat_s=1 %to &n_strat_s.;
			/* temp3 = tot_remainder*nsch_adj/tot_nsch_adj */
			/* multiplying the total remainder by the stratum percentage among strata where we have a reserve */
			_x_temp3_strat_s_&strat_s._d_&strat_d. = _x_tot_remainder * _x_nsch_adj_strat_s_&strat_s._d_&strat_d. / _x_tot_nsch_adj;
			
			/* temp4 = min(temp3,reserve) */
			/* check whether we have enough schools in reserve to meet that and adjust */
			_x_temp4_strat_s_&strat_s._d_&strat_d. = min(_x_temp3_strat_s_&strat_s._d_&strat_d.,_x_reserve_strat_s_&strat_s._d_&strat_d.);
			
			/* temp = temp2 + temp4 */
			/* add up the two counts of schools to sample in the stratum (temp2 from Block 1 and temp4 from Block 2) */
			_x_temp_strat_s_&strat_s._d_&strat_d. = _x_temp2_strat_s_&strat_s._d_&strat_d. + _x_temp4_strat_s_&strat_s._d_&strat_d.;
			
			/* create round, floor, ceil, and target variables (the target variables will be updated soon) */
			_x_round_strat_s_&strat_s._d_&strat_d. = round(_x_temp_strat_s_&strat_s._d_&strat_d.);
			_x_floor_strat_s_&strat_s._d_&strat_d. = floor(_x_temp_strat_s_&strat_s._d_&strat_d.);
			_x_ceil_strat_s_&strat_s._d_&strat_d. = ceil(_x_temp_strat_s_&strat_s._d_&strat_d.);
			
			_x_target_strat_s_&strat_s._d_&strat_d. = _x_round_strat_s_&strat_s._d_&strat_d.;
		%end;
		/* end loop over school strata */
	
	%end;
	/* end loop over district strata */
	
	
	/**************************************************
	BLOCK 3
	**************************************************/
	/* check that the computed temp variables add up to the desired sample size */
	if round(sum(of _x_temp_strat_:),1)~=round(&sample_size_school.,1) then put "ERROR: Unexpected error (either an issue in the macro logic or macro inputs). sum(of _x_temp_strat_:)~=school sample size.";
	/* ^ unlike most of the other error checks in this program, we are not terminating the program after this one is triggered */
	
	/* adjust the rounded targets to ensure they add up to the total sample size */
	if sum(of _x_target_:)~=&sample_size_school. then do;
		%do strat_d=1 %to &n_strat_d.;
			%do strat_s=1 %to &n_strat_s.;
				if 		sum(of _x_target_:)<&sample_size_school. then _x_target_strat_s_&strat_s._d_&strat_d.=_x_ceil_strat_s_&strat_s._d_&strat_d.;
				else if sum(of _x_target_:)>&sample_size_school. then _x_target_strat_s_&strat_s._d_&strat_d.=_x_floor_strat_s_&strat_s._d_&strat_d.;
			%end;
		%end;
	end;
	if sum(of _x_target_:)~=&sample_size_school. then put "ERROR: Unexpected error (either an issue in the macro logic or macro inputs). sum(of _x_target_:)~=school sample size.";
	/* ^ unlike most of the other error checks in this program, we are not terminating the program after this one is triggered */
run;

%if &any_flag_tw.=1 %then %let n_err = %eval(&n_err. + 1);


/* terminate macro if any errors were triggered */
%if &n_err.>0 %then %put ERROR: Macro terminated because of errors;
%if &n_err.>0 %then %return;

proc datasets library=work nolist;
	delete 	%scan(&targets_prep3.,2)
			;
quit;


/* reshape targets data */
/** version 1: district stratum level **/
/** this is the main version, which we will attach to the district-level data, by district stratum, before sampling **/
data &targets_strat_d.;
	set &targets_wide.;
	%do strat_d=1 %to &n_strat_d.;
		_x_stratum_d = &strat_d.;
		%do strat_s=1 %to &n_strat_s.;
			_x_mx_target_strat_s_&strat_s. = _x_target_strat_s_&strat_s._d_&strat_d.;
			_x_fx_nsch_strat_s_&strat_s. = _x_nsch_strat_s_&strat_s._d_&strat_d.;
			_x_fx_nsch_c_strat_s_&strat_s. = _x_nsch_c_strat_s_&strat_s._d_&strat_d.;
		%end;
		output;
	%end;
	keep _x_stratum_d _x_mx_: _x_fx_:;
run;

proc datasets library=work nolist;
	delete 	%scan(&targets_wide.,2)
			;
quit;

/** version 2: district and school stratum level **/
/** this version is for merging to the user-supplied targets data and to the final sample results **/
data &targets_strat_all.;
	set &targets_strat_d.;
	%do strat_s=1 %to &n_strat_s.;
		_x_stratum_s = &strat_s.;
		_x_mx_target = _x_mx_target_strat_s_&strat_s.;
		_x_fx_nsch = _x_fx_nsch_strat_s_&strat_s.;
		_x_fx_nsch_c = _x_fx_nsch_c_strat_s_&strat_s.;
		output;
	%end;
	keep _x_stratum_d _x_stratum_s _x_mx_target _x_fx_nsch _x_fx_nsch_c;
run;



/*----------------------------------
COMPUTE TARGETS BY STRATUM: Part 2, Incorporate User-Supplied Targets
----------------------------------*/
%local targets_final_strat_d targets_final_strat_all;
data; run;
%let targets_final_strat_d = &syslast.;
data; run;
%let targets_final_strat_all = &syslast.;


%if %length(&targets_data.)>0 %then %do;
	/* datasets */
	%local user_targets3;
	data; run;
	%let user_targets3 = &syslast.;
	
	
	/* merge macro targets with user targets */
	proc sort data=&user_targets2.; by _x_stratum_d _x_stratum_s; run;
	proc sort data=&targets_strat_all.; by _x_stratum_d _x_stratum_s; run;
	data &user_targets3.;
		merge &user_targets2.(in=inuser) &targets_strat_all.(in=inmac);
		by _x_stratum_d _x_stratum_s;
		_x_flag_overall = target>_x_fx_nsch;
		if _x_flag_overall then do;
			put "ERROR: There are not enough schools available to sample in the following stratum to fill your requested target (from the targets_data).";
			put "~Stratum: " &strata_equal.;
			put "~Number of schools available to sample in the stratum: " _x_fx_nsch;
			%if %length(&max_schools_per_district.)>0 %then %do;
				put "~Adjusted number of schools available to sample in the stratum (after accounting for the max number of schools to sample per district): " _x_fx_nsch_c;
			%end;
			put "~Requested sampling target: " target;
		end;
		_x_flag_capped = target<=_x_fx_nsch & target>_x_fx_nsch_c;
		if _x_flag_capped then do;
			put "ERROR: There are not enough schools available to sample in the following stratum (after accounting for the max number of schools to sample per district) to fill your requested target (from the targets_data).";
			put "~Stratum: " &strata_equal.;
			put "~Number of schools available to sample in the stratum (after accounting for the max number of schools to sample per district): " _x_fx_nsch_c;
			put "~Requested sampling target: " target;
		end;
		
		_x_any_flag = max(of _x_flag_:);
	run;
	
	%local any_flag_targ_frame;
	proc sql noprint;
		select	max(_x_any_flag)
		into	:any_flag_targ_frame trimmed
		from	&user_targets3.;
	quit;
	
	%if &any_flag_targ_frame.=1 %then %let n_err = %eval(&n_err. + 1);
	
	
	/* exit block if any errors were triggered */
	%if &n_err.>0 %then %put ERROR: Macro terminated because of errors;
	%if &n_err.>0 %then %return;
	/* ^ return will exit this conditional block, and then the return statement outside of this block will terminate the macro */
	
	
	/* finalize targets */
	/** we will create two versions, as we did with the macro targets **/
	data &targets_final_strat_all.;
		set &user_targets3.;
		rename target=_x_fx_target;
		drop _x_mx_target;
	run;
	
	data &targets_final_strat_d.;
		set &targets_final_strat_all.;
		by _x_stratum_d;
		%do strat_s=1 %to &n_strat_s.;
			retain	_x_fx_target_strat_s_&strat_s.
					_x_fx_nsch_strat_s_&strat_s.
					_x_fx_nsch_c_strat_s_&strat_s.
					;
			if first._x_stratum_d then do;
				_x_fx_target_strat_s_&strat_s. = .;
				_x_fx_nsch_strat_s_&strat_s. = .;
				_x_fx_nsch_c_strat_s_&strat_s. = .;
			end;
			if _x_stratum_s=&strat_s. then do;
				_x_fx_target_strat_s_&strat_s. = _x_fx_target;
				_x_fx_nsch_strat_s_&strat_s. = _x_fx_nsch;
				_x_fx_nsch_c_strat_s_&strat_s. = _x_fx_nsch_c;
			end;
		%end;
		if last._x_stratum_d then output;
		keep _x_stratum_d _x_fx_target_strat_: _x_fx_nsch_strat_: _x_fx_nsch_c_strat_:;
	run;
	
	proc datasets library=work nolist;
		delete 	%scan(&user_targets2.,2)
				%scan(&user_targets3.,2)
				;
	quit;
%end; %else %do;
	/* finalize targets */
	/** we will create two versions, as we did with the macro targets **/
	/** for the strat_all version, we will also attach the original user 
	stratification variables from the stratum_xwalk we created earlier 
	(and note that those variables are attached to the strat_all dataset 
	created in the above conditional block--via the merge with the user 
	targets) **/
	proc sort data=&targets_strat_all.; by _x_stratum_d _x_stratum_s; run;
	proc sort data=&stratum_xwalk.; by _x_stratum_d _x_stratum_s; run;
	data &targets_final_strat_all.;
		merge &targets_strat_all. &stratum_xwalk.;
		by _x_stratum_d _x_stratum_s;
		/* ^ there should be no mismatches in this merge, and each of 
		the two datasets should have the exact same number of obs */
		rename _x_mx_target=_x_fx_target;
	run;
	
	data &targets_final_strat_d.;
		set &targets_strat_d.;
		%do strat_s=1 %to &n_strat_s.;
			rename _x_mx_target_strat_s_&strat_s.=_x_fx_target_strat_s_&strat_s.;
		%end;
	run;
%end;
	
proc datasets library=work nolist;
	delete 	%scan(&targets_strat_d.,2)
			%scan(&targets_strat_all.,2)
			%scan(&stratum_xwalk.,2)
			;
quit;

/* terminate macro if any errors were triggered */
%if &n_err.>0 %then %put ERROR: Macro terminated because of errors;
%if &n_err.>0 %then %return;



/*----------------------------------
ATTACH TARGETS TO DISTRICT-LEVEL DATA
----------------------------------*/
%local district_presamp;
data; run;
%let district_presamp = &syslast.;

proc sql;
	create table &district_presamp. as
		select	a.*
				%do strat_s=1 %to &n_strat_s.;
					,b._x_fx_target_strat_s_&strat_s.
				%end;
		from	&dist_sch_counts. as a left join
				&targets_final_strat_d. as b
				on a._x_stratum_d=b._x_stratum_d
				;
quit;

proc datasets library=work nolist;
	delete 	%scan(&dist_sch_counts.,2)
			%scan(&targets_final_strat_d.,2)
			;
quit;



/*----------------------------------
SAMPLE DISTRICTS
----------------------------------*/
/*
The basic concept here is:
1)	We sort the districts within their district strata.
2)	For each school stratum, we proceed down the list of districts 
	and keep a running tally of how many schools in the stratum 
	we have selected into the sample.
3)	We continue selecting schools until we reach the target for 
	that school stratum.
*/


/* datasets */
%local district_samp;
data; run;
%let district_samp = &syslast.;


/* determine district sort order */
%local sort_d;
%if 		&method_d.=%str(RANDOM EQUAL) %then %let sort_d = _x_rand_district;
%else %if 	&method_d.=%str(RANDOM UNEQUAL) %then %let sort_d = _x_lotto_district;
%else %if 	&method_d.=BALANCED %then %let sort_d = &bal_distance_d.;


/* sort districts, within district strata */
proc sort data=&district_presamp.; by _x_stratum_d &sort_d.; run;


/* sample districts */
data &district_samp.;
	set &district_presamp.;
	by _x_stratum_d;
	
	/** Block 1: create the selection order and running, cumulative counts **/
	/*
	We will be creating the following variables in this block:
	variable								description
	--------								-----------
	_x_order_district						the district recruitment order

	_x_cum_strat_s_&strat_s.				a running count of schools selected, separately in each school stratum

	_x_min_order_strat_s_&strat_s.			the minimum value of _x_order_district where the running count of schools 
											selected reaches the target value
	*/
	retain	_x_order_district
			%do strat_s=1 %to &n_strat_s.;
				_x_cum_strat_s_&strat_s.
				_x_min_order_strat_s_&strat_s.
			%end;
			;
	if first._x_stratum_d then do;
		_x_order_district = 1;
		%do strat_s=1 %to &n_strat_s.;
			_x_cum_strat_s_&strat_s. = _x_nsch_c_strat_s_&strat_s.;
			if _x_cum_strat_s_&strat_s.>=_x_fx_target_strat_s_&strat_s. then _x_min_order_strat_s_&strat_s. = _x_order_district;
			else _x_min_order_strat_s_&strat_s. = .;
		%end;
	end; else do;
		_x_order_district = sum(_x_order_district,1);
		%do strat_s=1 %to &n_strat_s.;
			_x_cum_strat_s_&strat_s. = sum(_x_cum_strat_s_&strat_s., _x_nsch_c_strat_s_&strat_s.);
			if missing(_x_min_order_strat_s_&strat_s.) then do;
				if _x_cum_strat_s_&strat_s.>=_x_fx_target_strat_s_&strat_s. then _x_min_order_strat_s_&strat_s. = _x_order_district;
			end;/* else do;
				if _x_cum_strat_s_&strat_s.>=_x_fx_target_strat_s_&strat_s. & _x_order_district<_x_min_order_strat_s_&strat_s. then _x_min_order_strat_s_&strat_s. = _x_order_district;
			end;
			^ I don't see why we would need this commented out piece and cannot remember why it was there in the first place, 
			but, Dan, can you double-check? ***********************************************************************************************************
			*/
		%end;
	end;
	
	/** Block 2: create school-stratum specific sampling variables **/
	/*
	We will be creating the following variables in this block:
	variable								description
	--------								-----------
	_x_ranthru_strat_s_&strat_s.			indicator for whether we passed through this district as we accumulated 
											the initial sample for this particular school stratum
											
	_x_initial_sample_strat_s_&strat_s.		indicator for whether the district contributes to the initial sample 
											for this particular school stratum
											(The difference between this variable and the "ranthru" variant is that 
											the latter can include districts that did not have any schools to 
											contribute to the sample in this particular school stratum.)
											
	_x_n_select_strat_s_&strat_s.			the number of schools to be selected from a district for this particular 
											school stratum
	*/
	%do strat_s=1 %to &n_strat_s.;
		length _x_ranthru_strat_s_&strat_s. _x_initial_sample_strat_s_&strat_s. 3;
		
		_x_ranthru_strat_s_&strat_s. = (_x_order_district<=_x_min_order_strat_s_&strat_s. | missing(_x_min_order_strat_s_&strat_s.)) & _x_fx_target_strat_s_&strat_s.>0;
		/* ^ a couple notes about this:
			1. we are accounting for missing(min_order) in case the target was never reached
			2. some targets may be 0 (hence the part of the code specifying that the target be > 0)
		*/
		
		_x_initial_sample_strat_s_&strat_s. = _x_ranthru_strat_s_&strat_s. * (_x_nsch_c_strat_s_&strat_s.>0);
		
		if _x_cum_strat_s_&strat_s.>_x_fx_target_strat_s_&strat_s. & _x_initial_sample_strat_s_&strat_s.=1 
			then _x_n_select_strat_s_&strat_s. = _x_nsch_c_strat_s_&strat_s. - (_x_cum_strat_s_&strat_s. - _x_fx_target_strat_s_&strat_s.);
		else _x_n_select_strat_s_&strat_s. = _x_initial_sample_strat_s_&strat_s. * _x_nsch_c_strat_s_&strat_s.;
	%end;
	
	/** Block 3: create cross-school-stratum sampling variables **/
	/*
	We will be creating the following variables in this block:
	variable								description
	--------								-----------
	_x_initial_sample						indicator for whether the district is in the initial sample
	_x_n_select_total						the total number of schools to be selected from a district
	*/
	length _x_initial_sample 3;
	_x_initial_sample = max(of _x_initial_sample_strat_s_:);
	_x_n_select_total = sum(of _x_n_select_strat_s_:);
	
	
	/** labeling **/
	label	_x_order_district="District Recruitment Order"
			_x_initial_sample="District Selected"
			_x_n_select_total="Number of Schools Selected in This District Overall"
			_x_tot_nsch="Total Schools Available in This District Overall"
			_x_stratum_d="District Stratum"
			;
	%do strat_s=1 %to &n_strat_s.;
		label	_x_n_select_strat_s_&strat_s.="Number of Schools Selected in This District, in School Stratum &strat_s."
				_x_nsch_strat_s_&strat_s.="Total Schools Available in This District, in School Stratum &strat_s."
				;
	%end;
run;

proc datasets library=work nolist;
	delete 	%scan(&district_presamp.,2)
			;
quit;


/* finalize district output data */
/*
Note that the output contains the FULL district list, not just the initial sample. 
Users need the full list so they can select replacements if sites decline to participate, 
using the recruitment order specified in _x_order_district.
*/
%local vars_out_district;
%let vars_out_district = 
	_x_stratum_d
	&strata_d.
	_x_order_district
	&district_id.
	_x_initial_sample
	_x_n_select_total
	_x_tot_nsch
	;
%do strat_s=1 %to &n_strat_s.;
	%let vars_out_district = 
		&vars_out_district.
		_x_n_select_strat_s_&strat_s.
		_x_nsch_strat_s_&strat_s.
		;
%end;
data &out_district.;
	retain &vars_out_district.;
	set &district_samp.;
	keep &vars_out_district.;
run;



/*----------------------------------
PREPARE FOR SCHOOL SAMPLING
----------------------------------*/
/* datasets */
%local school_targets school_presamp;
data; run;
%let school_targets = &syslast.;
data; run;
%let school_presamp = &syslast.;

/* reshape the district sampling data */
data &school_targets.;
	set &district_samp.(keep=&district_id. _x_order_district _x_n_select_: _x_nsch_strat_:);
	%do strat_s=1 %to &n_strat_s.;
		/* if there are any schools available in the stratum (note AVAILABLE, not SELECTED, 
		since we want to merge the number of selections AND the district order--and we need 
		the latter for both those districts with any selections and those without) */
		if _x_nsch_strat_s_&strat_s.>0 then do;
			_x_stratum_s = &strat_s.;
			_x_n_select = _x_n_select_strat_s_&strat_s.;
			output;
		end;
	%end;
	keep &district_id. _x_order_district _x_stratum_s _x_n_select;
run;

proc datasets library=work nolist;
	delete 	%scan(&district_samp.,2)
			;
quit;

/* merge to the school-level data */
proc sql;
	create table &school_presamp. as
		select	a.*,
				b._x_order_district,
				coalesce(b._x_n_select, 0) as _x_n_select /* this is precautionary */
		from	&merged_lottery. as a left join
				&school_targets. as b
				on a.&district_id.=b.&district_id. & a._x_stratum_s=b._x_stratum_s
				;
quit;

proc datasets library=work nolist;
	delete 	%scan(&merged_lottery.,2)
			%scan(&school_targets.,2)
			;
quit;



/*----------------------------------
SAMPLE SCHOOLS
----------------------------------*/
/*
This step is considerably simpler than the district sampling. All we are doing here is 
sorting the schools within districts and within school strata, according to the sampling 
method's sort order. Then we are identifying the "initial sample" of schools--i.e., the 
sample assuming 100% participation.
*/

/* datasets */
%local school_samp;
data; run;
%let school_samp = &syslast.;


/* determine schools sort order */
%local sort_s;
%if 		&method_s.=%str(RANDOM EQUAL) %then %let sort_s = _x_rand_school;
%else %if 	&method_s.=%str(RANDOM UNEQUAL) %then %let sort_s = _x_lotto_school;
%else %if 	&method_s.=BALANCED %then %let sort_s = &bal_distance_s.;


/* sort schools, within districts and within school strata */
proc sort data=&school_presamp.; by &district_id. _x_stratum_s &sort_s.; run;


/* school sampling */
data &school_samp.;
	set &school_presamp.;
	by &district_id. _x_stratum_s;
	
	retain _x_order_school;
	if max(first.&district_id., first._x_stratum_s) then _x_order_school = 1;
	else _x_order_school = sum(_x_order_school,1);
	
	_x_initial_sample = _x_order_school<=_x_n_select & _x_n_select>0;

	label	_x_order_school="School Recruitment Order"
			_x_initial_sample="School Selected"
			_x_stratum_s="School Stratum"
			_x_stratum_d="District Stratum"
			;
run;

proc datasets library=work nolist;
	delete 	%scan(&school_presamp.,2)
			;
quit;


/* finalize school output data */
/*
Note that the output contains the FULL school list, not just the initial sample. 
Users need the full list so they can select replacements if sites decline to participate, 
using the recruitment order specified in _x_order_district and _x_order_school.
*/
%local vars_out_school;
%let vars_out_school = 
	_x_stratum_d
	&strata_d.
	_x_order_district
	&district_id.
	_x_stratum_s
	&strata_s.
	_x_order_school
	&school_id.
	_x_initial_sample
	;
data &out_school.;
	retain &vars_out_school.;
	set &school_samp.;
	keep &vars_out_school.;
run;

proc sort data=&out_school.;
	by _x_stratum_d _x_order_district _x_stratum_s _x_order_school;
run;



/*----------------------------------
VERIFY SAMPLE TARGETS WERE MET
----------------------------------*/
/* datasets */
%local school_samp_sum check_sampling;
data; run;
%let school_samp_sum = &syslast.;
data; run;
%let check_sampling = &syslast.;


/* count the sampled schools */
proc means data=&school_samp. noprint nway;
	class _x_stratum_d _x_stratum_s;
	var _x_initial_sample;
	output out=&school_samp_sum.(drop=_type_ _freq_) sum=;
run;

proc datasets library=work nolist;
	delete 	%scan(&school_samp.,2)
			;
quit;


/* compare to the targets */
proc sort data=&targets_final_strat_all.; by _x_stratum_d _x_stratum_s; run;
data &check_sampling.;
	merge &school_samp_sum.(in=insamp) &targets_final_strat_all.(in=intarg);
	by _x_stratum_d _x_stratum_s;
	if missing(_x_initial_sample) then _x_initial_sample=0;
	if missing(_x_fx_target) then _x_fx_target=0;
	_x_flag = _x_initial_sample~=_x_fx_target;
	if _x_flag then put "ERROR: Unexpected error (either an issue in the macro logic or macro inputs). Actual schools sampled does not align with the target for this stratum: " 
		_x_stratum_d= _x_stratum_s= _x_initial_sample= _x_fx_target=;
	/* ^ not terminating the macro for this flag because this is the end of the macro anyways */
run;

%local total_sampled;
proc sql noprint;
	select	sum(_x_initial_sample)
	into	:total_sampled trimmed
	from	&check_sampling.;
quit;

%if &total_sampled.~=&sample_size_school. %then %put ERROR: Unexpected error (either an issue in the macro logic or macro inputs). Total schools sampled (&total_sampled.) does not equal the requested sample size (&sample_size_school.);

proc datasets library=work nolist;
	delete 	%scan(&school_samp_sum.,2)
			%scan(&targets_final_strat_all.,2)
			;
quit;



/*----------------------------------
CREATE SUMMARY OUTPUT DATASET
----------------------------------*/
%local vars_out_summary;
%let vars_out_summary = 
	_x_stratum_d
	&strata_d.
	_x_stratum_s
	&strata_s.
	_x_initial_sample
	_x_fx_target
	_x_fx_nsch
	_x_fx_nsch_c
	;
data &out_summary.;
	retain &vars_out_summary.;
	set &check_sampling.;
	keep &vars_out_summary.;
	label	_x_initial_sample="Sampled Schools"
			_x_fx_target="Sampling Target"
			_x_fx_nsch="Total Schools Available"
			_x_fx_nsch_c="Adjusted Total Schools Available (after accounting for the max number of schools to sample per district--if applicable)"
			;
	rename 	_x_initial_sample=_x_sampled
			_x_fx_target=_x_targeted
			_x_fx_nsch=_x_tot_nsch
			_x_fx_nsch_c=_x_adj_nsch_c
			;
run;

proc datasets library=work nolist;
	delete 	%scan(&check_sampling.,2)
			;
quit;



/*----------------------------------
END MACRO DEFINITION
----------------------------------*/
%mend select_sites;

