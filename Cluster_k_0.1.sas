/******************************************************************************************
Program:		Cluster_k.sas


Description:	This program defines cluster_k, a SAS macro for clustering districts and 
				schools in education as implemented in Litwok et al. (2022):

				cluster_k		Performs k-means or k-medians clustering to partition 
								data into groups.


Authors:		Azim Shivji, Daniel Litwok, and Robert B. Olsen


References: 	Shivji, A., Litwok, D., & Olsen, Robert B. (2022). Cluster_k: SAS Macro for
					Clustering Districts and Schools for Impact Studies in Education.
					Documentation. Available at: <github.com/select-sites/exval>.

				Litwok, D., Nichols, A., Shivji, A., & Olsen, Robert B. (2022). Selecting 
					districts and schools for impact studies in education: A simulation 
					study of different strategies. Journal of Research on Educational 
					Effectiveness. DOI: https://doi.org/10.1080/19345747.2022.2128952.

				Tipton, E. (2013). Stratified sampling using cluster analysis: A sample 
					selection strategy for improved generalizations from experiments. 
					Evaluation Review, 37(2), 109-139.


GitHub Repo:	<github.com/select-sites/exval>


Support:		If you have questions or bugs to report, you may open an issue on the Github 
				repository.


History:		Version				Date				Notes
				-------				----				-----------------------------------
				0.1					11/18/22			Preliminary release of the cluster_k macro


SAS Version:	These macros were programmed and tested in SAS 9.4.


Requirements:	The SAS/STAT product is required for the cluster_k macro.


Notes:			*	Both of the macros in this program create various intermediate 
					datasets (temporary working datasets that are used in the processing 
					of the macros and are deleted after they are no longer needed). To 
					reduce the likelihood that these datasets conflict with (and overwrite) 
					existing datasets in the user's work library, the macros use SAS's 
					"DATAn naming convention."
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


License:		Cluster_k: SAS Macro for Clustering Districts and Schools for Impact 
					Studies	in Education. Copyright (c) 2022 Azim Shivji, Daniel Litwok, 
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




/******************************************************************************************
Macro: Cluster_k

Performs k-means or k-medians clustering to partition data into groups.
******************************************************************************************/
/*----------------------------------
MANDATORY/OPTIONAL PARAMETERS
----------------------------------*/
/*
Mandatory Parameters: The following parameters must be specified in each 
macro call. They are the minimum parameters required for the macro to run.
	input_data
	unit_id
	<at least one of the three cluster_vars_[type] parameters>
		cluster_vars_interval
		cluster_vars_binary
		cluster_vars_nominal
	k_clusters_list


Optional Parameters: You may choose to specify the following parameters 
or let the macro choose default values for you.
	centroid_measure
	distance_measure
	scaling
	cluster_var_weights_data
	seed
	maxiter
	converge
	output_data
	output_var_cluster
	output_var_distance
	output_centroids
	output_summary
*/


/*----------------------------------
BEGIN MACRO DEFINITION
----------------------------------*/
%macro cluster_k(
	input_data =,
	unit_id =,
	cluster_vars_interval =,
	cluster_vars_binary =,
	cluster_vars_nominal =,
	k_clusters_list =,
	centroid_measure =,
	distance_measure =,
	scaling =,
	cluster_var_weights_data =,
	seed =,
	maxiter =,
	converge = 0.0001,
	output_data = _out_data,
	output_var_cluster = _cluster,
	output_var_distance = _distance,
	output_centroids = _out_centroids,
	output_summary = _out_summary
);



/*----------------------------------
PREPARATORY WORK
----------------------------------*/
%local i var k n_err;
%let n_err = 0;

%let centroid_measure = %upcase(&centroid_measure.);
%let distance_measure = %upcase(&distance_measure.);
%let scaling = %upcase(&scaling.);

%local cluster_vars_all n_cluster_vars_all;
%let cluster_vars_all = &cluster_vars_interval. &cluster_vars_binary. &cluster_vars_nominal.;
%if %length(&cluster_vars_all.)>0 %then %let n_cluster_vars_all = %sysfunc(countw(&cluster_vars_all.));
%else %let n_cluster_vars_all = 0;



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
%if %length(&unit_id.)=0 %then %do;
	%put ERROR: Missing unit_id macro parameter;
	%let n_err = %eval(&n_err. + 1);
%end;
%if %length(&cluster_vars_all.)=0 %then %do;
	%put ERROR: You must specify at least one variable to cluster, in at least one of the cluster_vars_interval, cluster_vars_binary, and cluster_vars_nominal parameters.;
	%let n_err = %eval(&n_err. + 1);
%end;
%if %length(&k_clusters_list.)=0 %then %do;
	%put ERROR: Missing k_clusters_list macro parameter;
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

/* centroid_measure */
%if %length(&centroid_measure.)>0 & &centroid_measure.~=MEAN & &centroid_measure.~=MEDIAN %then %do;
	%put ERROR: Invalid centroid_measure parameter. The only supported values are MEAN and MEDIAN. (You may also leave this parameter blank and have the macro choose a default value.);
	%let n_err = %eval(&n_err. + 1);
%end;

/* distance_measure */
%if %length(&distance_measure.)>0 & &distance_measure.~=EUCLIDEAN & &distance_measure.~=MANHATTAN & &distance_measure.~=GOWER %then %do;
	%put ERROR: Invalid distance_measure parameter. The only supported values are EUCLIDEAN, MANHATTAN, and GOWER. (You may also leave this parameter blank and have the macro choose a default value.);
	%let n_err = %eval(&n_err. + 1);
%end;

/* scaling */
%if %length(&scaling.)>0 & &scaling.~=STD & &scaling.~=RANGE & &scaling.~=GOWER & &scaling.~=EUCLEN & &scaling.~=NONE %then %do;
	%put ERROR: Invalid scaling parameter. The only supported values are STD, RANGE, GOWER, EUCLEN, and NONE. (You may also leave this parameter blank, in which case NONE is assumed--or GOWER is assumed, if the macro uses the GOWER distance measure.);
	%let n_err = %eval(&n_err. + 1);
%end;


/* terminate macro if any errors were triggered */
%if &n_err.>0 %then %put ERROR: Macro terminated because of errors;
%if &n_err.>0 %then %return;



/*----------------------------------
CHECK FOR VARIABLE CONFLICTS
----------------------------------*/
/* check if variables the macro will create already exist */
%if %varexist(ds=&input_data., var=_temp_varname)=1 %then %do;
	%put ERROR: The macro needs to create a variable called "_temp_varname", but you already have a variable with the same name in your dataset. Please rename this variable, so that it does not conflict with the macro.;
	%let n_err = %eval(&n_err. + 1);
%end;

%if %varexist(ds=&input_data., var=_temp_found)=1 %then %do;
	%put ERROR: The macro needs to create a variable called "_temp_found", but you already have a variable with the same name in your dataset. Please rename this variable, so that it does not conflict with the macro.;
	%let n_err = %eval(&n_err. + 1);
%end;


/* terminate macro if any errors were triggered */
%if &n_err.>0 %then %put ERROR: Macro terminated because of errors;
%if &n_err.>0 %then %return;


/* check if any variables beginning with _xyz_ exist */
data _null_;
	if 0=1 then set &input_data.;
	length _temp_varname $32;
	_temp_found = 0;
	do until (_temp_found=1 | missing(_temp_varname));
		call vnext(_temp_varname);
		if find(_temp_varname, "_xyz_", "i")=1 then _temp_found=1;
	end;
	call symputx("_xyz_found", _temp_found, "L");
	stop;
run;

%if &_xyz_found.=1 %then %do;
	%put ERROR: The macro needs to create a series of variables whose names begin with the string "_xyz_", but you already have at least one variable whose name starts with the same string, in your dataset. Please rename any such variables, so that they do not conflict with the macro.;
	%let n_err = %eval(&n_err. + 1);
%end;


/* terminate macro if any errors were triggered */
%if &n_err.>0 %then %put ERROR: Macro terminated because of errors;
%if &n_err.>0 %then %return;



/*----------------------------------
DETERMINE DATA TYPE & KEY DEFAULT 
PARAMETER VALUES
----------------------------------*/
/* determine the data type of the cluster variables */
%local data_type;
%if 		%length(&cluster_vars_interval.)>0 & %length(&cluster_vars_binary.)=0 & %length(&cluster_vars_nominal.)=0 %then %let data_type = INTERVAL;
%else %if 	%length(&cluster_vars_interval.)=0 & (%length(&cluster_vars_binary.)>0 | %length(&cluster_vars_nominal.)>0) %then %let data_type = NOMINAL;
%else %if 	%length(&cluster_vars_interval.)>0 & (%length(&cluster_vars_binary.)>0 | %length(&cluster_vars_nominal.)>0) %then %let data_type = MIXED;


/* populate a table of compatible combinations and default combinations of key parameters */
%local combos_compatible combos_choice;
data; run;
%let combos_compatible = &syslast.;
data; run;
%let combos_choice = &syslast.;

data &combos_compatible.;
	length	centroid_measure	$20
			distance_measure	$20
			data_type			$20
			default_naive		3
			extra_pref			3
			;
	centroid_measure="MEAN";	distance_measure="EUCLIDEAN";	data_type="INTERVAL";	default_naive=1;	extra_pref=0;	output;
	centroid_measure="MEDIAN";	distance_measure="MANHATTAN";	data_type="INTERVAL";	default_naive=0;	extra_pref=1;	output;
	centroid_measure="MEDIAN";	distance_measure="GOWER";		data_type="MIXED";		default_naive=1;	extra_pref=0;	output;
	centroid_measure="MEDIAN";	distance_measure="GOWER";		data_type="INTERVAL";	default_naive=0;	extra_pref=0;	output;
	centroid_measure="MEDIAN";	distance_measure="GOWER";		data_type="NOMINAL";	default_naive=1;	extra_pref=0;	output;
	centroid_measure="MEAN";	distance_measure="EUCLIDEAN";	data_type="MIXED";		default_naive=0;	extra_pref=0;	output;
run;
/*
This dataset serves two purposes:
	(1)	We will use it to check whether the parameter values the user specified are 
		compatible. This is complicated by the fact that users do not have to specify 
		each of these parameters. So when we check the parameters that the user DID 
		specify, we will search for matches in this master dataset solely by those 
		specified parameters. For instance, if we know the data_type, and the user 
		additionally specified the centroid_measure and distance_measure, we will 
		search for matches exclusively on those three variables. If there is more than 
		one matching observation, then the user specification is compatible. If there 
		are zero matching observations, then the user specification is not compatible.
	(2)	We will also use it to choose and assign default values for parameters the user 
		did not specify. Our goal is to determine the default combination given any 
		acceptable user input. (And recall that the user must specify which variables 
		to cluster, so at minimum, we will know the data_type.) This is how we will make 
		our choices:
			(a)	Subset this dataset, based on the data_type and whichever parameters the 
				user specified.
			(b)	The default_naive variable identifies the default values for 
				centroid_measure and distance_measure when the only information we have 
				is the data_type.
			(c)	In the subsetted dataset, if there is only one row where default_naive 
				equals 1, we choose the values from that row as our defaults.
			(d)	If default_naive equals 0, but there is only one row in the subsetted 
				dataset, we choose the values from that row.
			(e)	If default_naive=0 and there is more than one row in the subsetted 
				dataset, we use the extra_pref variable to make our final choice. If 
				there is only one row where extra_pref=1, then we choose that row.
			(f)	We accomplish this by sorting, in descending order, by default_naive, 
				and extra_pref, and selecting the top row from the sorted data. And we 
				also check the sums of these variables in the subsetted dataset (prior 
				to taking the top row), to verify that there are not multiple rows 
				meeting our selection criteria.
*/


/* process the user inputs */
data _null_;
	arg_data_type = "data_type='&data_type.'";
	if "&centroid_measure."~="" then arg_centroid_measure = "centroid_measure='&centroid_measure.'";
	if "&distance_measure."~="" then arg_distance_measure = "distance_measure='&distance_measure.'";
	logic = catx(" & ", of arg_:);
	nmiss = cmiss(of arg_:);
	call symputx("user_specs_logic", logic, "L");
	call symputx("user_specs_nmiss", nmiss, "L");
run;

%local combos_user_n combos_user_sum_def combos_user_sum_ex;
proc sql noprint;
	select	count(*),
			sum(default_naive),
			sum(extra_pref)
	into	:combos_user_n trimmed,
			:combos_user_sum_def trimmed,
			:combos_user_sum_ex trimmed
	from	&combos_compatible.
	where	&user_specs_logic.;
quit;


/* check whether the user-specified parameter values are compatible */
%if &combos_user_n.=0 %then %do;
	%put ERROR: You have specified an incompatible combination of two or more of the following macro parameters: centroid_measure, distance_measure, and data type (based on the cluster_vars_[type] parameters). See the table in the documentation for the distance_measure macro parameter for the list of all compatible combinations.;
	%let n_err = %eval(&n_err. + 1);
%end;


/* internal check */
%if &combos_user_n.>1 & &combos_user_sum_def.~=1 & &combos_user_sum_ex.~=1 %then %do;
	%put ERROR: Internal issue in the macro logic for choosing default parameter values. This may not necessarily be a user issue. It may be an issue in the macro programming.;
	%let n_err = %eval(&n_err. + 1);
%end;


/* terminate macro if any errors were triggered */
%if &n_err.>0 %then %put ERROR: Macro terminated because of errors;
%if &n_err.>0 %then %return;


/* sort our choice for the default combination to the top */
proc sort data=&combos_compatible.(where=(&user_specs_logic.)) out=&combos_choice.;
	by descending default_naive descending extra_pref;
run;

/* select our choice and assign its values to local macro variables */
data _null_;
	set &combos_choice.(obs=1);
	call symputx("centroid_choice", centroid_measure, "L");
	call symputx("distance_choice", distance_measure, "L");
run;

/* create final macro variables, where we assign defaults for unspecified parameter values */
/** note that we are not overriding any specified parameter values here **/
%local centroid_use distance_use;
%if %length(&centroid_measure.)=0 %then %let centroid_use = &centroid_choice.;
%else %let centroid_use = &centroid_measure.;
%if %length(&distance_measure.)=0 %then %let distance_use = &distance_choice.;
%else %let distance_use = &distance_measure.;


/* set distance_sas macro variable */
%local distance_sas;
%if &distance_use.=GOWER %then %let distance_sas = MANHATTAN;
%else %let distance_sas = &distance_use.;
/* ^ We operationalize the Gower distance by applying the Manhattan distance to 
transformed and rescaled variables. The distance_sas macro variable is used to 
determine the value for the least macro variable, which is plugged into the 
LEAST option of PROC FASTCLUS. */


/* determine default scaling */
%local scaling_use scaling_sas;
%if &distance_use.=GOWER %then %let scaling_use = GOWER;
%else %if %length(&scaling.)=0 %then %let scaling_use = NONE;
%else %let scaling_use = &scaling.;

%if &scaling_use.=GOWER %then %let scaling_sas = RANGE;
%else %let scaling_sas = &scaling_use.;
/* ^ scaling_sas represents the actual value for the METHOD option we will feed to 
PROC STDIZE. In the case of GOWER, we are using METHOD=RANGE on transformed 
variables, to implement the Gower scaling. */


/* determine default maxiter */
%local maxiter_use;
%if %length(&maxiter.)=0 %then %do;
	%if 		&distance_sas.=EUCLIDEAN %then %let maxiter_use = 10;
	%else %if 	&distance_sas.=MANHATTAN %then %let maxiter_use = 20;
	/* ^ note that it is distance_sas, not distance_use, so EUCLIDEAN and MANHATTAN 
	cover all supported options */
%end; %else %let maxiter_use = &maxiter.;


/* clean up */
proc datasets library=work nolist;
	delete 	%scan(&combos_compatible.,2)
			%scan(&combos_choice.,2)
			;
quit;



/*----------------------------------
ASSORTED PARAMETER CHECKS
----------------------------------*/
/* scaling vs. distance for gower */
%if &distance_use.=GOWER & %length(&distance_measure.)=0 & (&scaling.=STD | &scaling.=RANGE | &scaling.=EUCLEN | &scaling.=NONE) 
	%then %put WARNING: Since you have mixed data (interval and nominal) and did not specify a distance measure, the macro will use the Gower distance, by default. That also means that the macro will scale the cluster variables according to the Gower measure and will ignore whichever value you specified for the scaling parameter.;
%else %if &distance_measure.=GOWER & (&scaling.=STD | &scaling.=RANGE | &scaling.=EUCLEN | &scaling.=NONE) 
	%then %put WARNING: Since you specified the Gower distance measure, the macro will scale the cluster variables according to that measure and will ignore whichever value you specified for the scaling parameter.;

/* lengths of the output variable names */
/** will check these after parsing the cluster numbers below, to determine if there is more than one k value **/



/*----------------------------------
PARSE & CHECK CLUSTER NUMBERS
----------------------------------*/
/* We checked above whether k_clusters_list is empty. Here, we will parse the parameter and 
check whether it is syntactically and logically correct. */

/*
Examples of acceptable values for k_clusters_list:
4
2:8
2 4 9:13 20
*/

/* first check for unacceptable characters */
data _null_;
	/* check for characters other than digits, colons, and spaces */
	flag_chars = prxmatch("/[^0-9: ]/", "&k_clusters_list.")>0;
	if flag_chars then put "ERROR: Invalid characters in k_clusters_list parameter. The only allowable characters are digits, colons, and spaces.";
	/* check for non-digit characters on either side of a colon, and check for colons at the very beginning or end of the string */
	flag_colon = prxmatch("/([^0-9]:)|(:[^0-9])|(^:)|(:$)/", "&k_clusters_list.")>0;
	if flag_colon then put "ERROR: Invalid k_clusters_list parameter. Colons must be between two numbers. And do not include spaces (or any character other than numeric digits) around the colons.";

	/* FYI, we are using two different types of ^ metacharacters in the code above, and they have 
	different meanings. [^...] matches a character not in the brackets. ^ outside of brackets, and 
	at the beginning of a pattern, matches the beginning of a line. */
	
	any_flag = max(of flag_:);
	
	call symputx("any_cluster_flag_chars", any_flag, "L");
	/* ^ note that there is only one observation in this dataset */
run;

%if &any_cluster_flag_chars.=1 %then %let n_err = %eval(&n_err. + 1);


/* terminate macro if any errors were triggered */
%if &n_err.>0 %then %put ERROR: Macro terminated because of errors;
%if &n_err.>0 %then %return;


/* parse cluster number list */
%local parse_clusters_list1 parse_clusters_list2 parse_clusters_list3;
data; run;
%let parse_clusters_list1 = &syslast.;
data; run;
%let parse_clusters_list2 = &syslast.;
data; run;
%let parse_clusters_list3 = &syslast.;

data &parse_clusters_list1.;
	length word $%length(&k_clusters_list.);
	do word_num=1 to countw("&k_clusters_list.", " ");
		/* find start and stop */
		word = scan("&k_clusters_list.", word_num, " ");
		if find(word, ":")=0 then do;
			start = input(word, 8.);
			stop = start;
		end; else do;
			start = input(scan(word, 1, ":"), 8.);
			stop = input(scan(word, 2, ":"), 8.);
		end;
		
		/* flags (part 1) */
		flag_zero = start=0 | stop=0;
		if flag_zero then put "ERROR: Invalid k_clusters_list parameter. The number of clusters cannot be zero.";
		flag_seq = start>stop;
		if flag_seq then put "ERROR: Invalid k_clusters_list parameter. If you specify a sequence of cluster numbers as #:# (as in 2:10), the first number cannot be greater than the second number (e.g., 10:2 is not allowed, but 2:10 is).";
		/* do not need to flag for negative numbers because those would already be captured above when flagging characters 
		that are not digits, colons, or spaces */
		any_flag = max(of flag_:);
		
		/* output */
		if any_flag=0 then do k_clusters=start to stop;
			output;
		end; else output;
		/* ^ the IF-ELSE logic is needed because we need to capture the flag output, and if flag_seq=1, 
		no observations would have been output through the DO LOOP without the IF-ELSE conditioning */
	end;
run;

proc sql;
	create table &parse_clusters_list2. as 
		select		k_clusters,
					max(any_flag) as any_flag_part1,
					count(k_clusters) as count
		from		&parse_clusters_list1.
		group by	k_clusters;
quit;

data &parse_clusters_list3.;
	set &parse_clusters_list2. end=last;
	/* flags (part 2) */
	flag_dup = count>1;
	if flag_dup then put "ERROR: Invalid k_clusters_list parameter. At least one value in the list of cluster numbers was repeated.";
	any_flag = max(any_flag_part1, flag_dup);
	
	retain any_cluster_flag_parsed 0;
	if any_flag then any_cluster_flag_parsed = 1;
	if last then call symputx("any_cluster_flag_parsed", any_cluster_flag_parsed, "L");
run;

%if &any_cluster_flag_parsed.=1 %then %let n_err = %eval(&n_err. + 1);


/* based on the parsed cluster list, determine the number of distinct (and unflagged) cluster number values, 
and insert the k values into a space-separated macro variable */
%local k_values n_k_values max_k;
proc sql noprint;
	select	k_clusters,
			count(k_clusters),
			max(k_clusters)
	into	:k_values separated by ' ',
			:n_k_values trimmed,
			:max_k trimmed
	from	&parse_clusters_list3.
	where	any_flag=0;
quit;


/* terminate macro if any errors were triggered */
%if &n_err.>0 %then %put ERROR: Macro terminated because of errors;
%if &n_err.>0 %then %return;


/* clean up */
proc datasets library=work nolist;
	delete 	%scan(&parse_clusters_list1.,2)
			%scan(&parse_clusters_list2.,2)
			%scan(&parse_clusters_list3.,2)
			;
quit;



/*----------------------------------
CHECK OUTPUT VARIABLE NAME LENGTHS
----------------------------------*/
/*
Only checking lengths here--not more sophisticated checks for whether the variable names are acceptable. We 
will rely on SAS for that. We are specifically checking for lengths here because we may add a suffix to the 
variable names, and a tailored message to the user will be helpful. (Also, we are not checking the names of 
the output datasets.)
If the user specified more than one cluster number (i.e., n_k_values>1), then the macro will add a suffix to 
the end of the output variable names, as "_k#" where "#" is the cluster number.
Therefore, the maximum length of the specified output variable names is...
	if n_k_values=1.......32						
	if n_k_values>1.......32 - (2 + the digits of the largest k value)
*/

/* output_var_cluster */
%if %length(&output_var_cluster.)>32 %then %do;
	%put ERROR: The specified variable name for output_var_cluster is too large.;
	%let n_err = %eval(&n_err. + 1);
%end;
%if &n_k_values.>1 & %length(&output_var_cluster.)>(32 - 2 - %length(&max_k.)) %then %do;
	%put ERROR: The specified variable name for output_var_cluster is too large. (Note that, since you specified more than one value for the number of clusters in the k_clusters_list, this macro will add a suffix to the variable name, as "_k#" where # is the number of clusters. So the variable name must be short enough to fit this suffix.);
	%let n_err = %eval(&n_err. + 1);
%end;

/* output_var_distance */
%if %length(&output_var_distance.)>32 %then %do;
	%put ERROR: The specified variable name for output_var_distance is too large.;
	%let n_err = %eval(&n_err. + 1);
%end;
%if &n_k_values.>1 & %length(&output_var_distance.)>(32 - 2 - %length(&max_k.)) %then %do;
	%put ERROR: The specified variable name for output_var_distance is too large. (Note that, since you specified more than one value for the number of clusters in the k_clusters_list, this macro will add a suffix to the variable name, as "_k#" where # is the number of clusters. So the variable name must be short enough to fit this suffix.);
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
	set &input_data. end=last;
	_xyz_flag_miss_id = missing(&unit_id.);
	if _xyz_flag_miss_id then put "ERROR: Observation missing &unit_id.";
	
	retain _xyz_any_flag_miss_id 0;
	if _xyz_flag_miss_id then _xyz_any_flag_miss_id = 1;
	if last then call symputx("any_flag_miss_id", _xyz_any_flag_miss_id, "L");
run;

%if &any_flag_miss_id.=1 %then %let n_err = %eval(&n_err. + 1);


/* check for missing data among cluster variables */
data _null_;
	set &input_data.(keep=&cluster_vars_all.) end=last;
	_xyz_n_miss = sum(
		nmiss(of _numeric_)-1,
		cmiss(of _character_, "dummy string")
	);
	/* ^ a few notes:
	** _xyz_nmiss(of _numeric_)-1 because _xyz_n_miss is, itself, a numeric variable (created in 
	the PDV before SAS determines the _numeric_ list) and will be missing before 
	evaluation (and we do not have to worry about the first argument of sum evaluating 
	to -1 since the missingness in _xyz_n_miss ensures at least 1-1=0).
	** Because _xyz_n_miss is, itself, a numeric variable, we also do not have to worry about 
	cases where nmiss() would not have enough arguments because there are no numeric 
	variables.
	** However, we do have to address cases where there are no character variables. 
	To prevent an error in cmiss() for not enough arguments if there are no character 
	variables, we add a dummy string.
	*/
	_xyz_flag_miss_data = _xyz_n_miss>0;
	if _xyz_flag_miss_data then put "ERROR: You have missing data in one or more of the following cluster variables: &cluster_vars_all..";
	
	retain _xyz_any_flag_miss_data 0;
	if _xyz_flag_miss_data then _xyz_any_flag_miss_data = 1;
	if last then call symputx("any_flag_miss_data", _xyz_any_flag_miss_data, "L");
run;

%if &any_flag_miss_data.=1 %then %let n_err = %eval(&n_err. + 1);


/* terminate macro if any errors were triggered */
%if &n_err.>0 %then %put ERROR: Macro terminated because of errors;
%if &n_err.>0 %then %return;



/*----------------------------------
CHECK THAT BINARY VARS ARE 0/1
----------------------------------*/
data _null_;
	set &input_data. end=last;
	%if %length(&cluster_vars_binary.)>0 %then %do;
		%do i=1 %to %sysfunc(countw(&cluster_vars_binary.));
			%let var = %scan(&cluster_vars_binary., &i.);
			_xyz_flag_binary_&i. = (strip(&var.) not in("0", "1") & not missing(&var.)) | vtype(&var.)="C";
			/* ^ using strip() and quoted numbers so that that part of the flag logic is type-agnostic (character or numeric) */
			if _xyz_flag_binary_&i. then put "ERROR: Binary variables must be numeric variables with 0 or 1 as values. Invalid value found for variable &var..: " &var.;
		%end;
		_xyz_any_flag = max(of _xyz_flag_:);
	%end; %else %do;
		_xyz_any_flag = 0;
	%end;
	
	retain _xyz_any_flag_binary 0;
	if _xyz_any_flag then _xyz_any_flag_binary = 1;
	if last then call symputx("any_flag_binary", _xyz_any_flag_binary, "L");
run;

%if &any_flag_binary.=1 %then %let n_err = %eval(&n_err. + 1);

/* terminate macro if any errors were triggered */
%if &n_err.>0 %then %put ERROR: Macro terminated because of errors;
%if &n_err.>0 %then %return;



/*----------------------------------
CHECK THAT CLUSTER VARS HAVE AT 
LEAST TWO LEVELS
----------------------------------*/
%local check_levels;
data; run;
%let check_levels = &syslast.;

proc sql;
	create table &check_levels. as 
		select	1 as dummy
				%do i=1 %to &n_cluster_vars_all.;
					%let var = %scan(&cluster_vars_all., &i.);
					,count(distinct &var.) as n_v&i.
				%end;
		from	&input_data.;
quit;
/* ^ Missing values will be treated as distinct values here, but that is okay since we also check 
for missingness and conditionally terminate the macro earlier. */

data _null_;
	set &check_levels.(drop=dummy);
	length var $32;
	array nvars{*} n_v:;
	do i=1 to dim(nvars);
		var = scan("&cluster_vars_all.", i);
		if nvars{i}=1 then put "ERROR: Only one distinct value found in cluster variable " var ". Cluster variables should have at least two distinct values.";
	end;
	drop i;
	any_flag = 1 in nvars;
	
	call symputx("any_flag_levels", any_flag, "L");
run;

%if &any_flag_levels.=1 %then %let n_err = %eval(&n_err. + 1);

/* terminate macro if any errors were triggered */
%if &n_err.>0 %then %put ERROR: Macro terminated because of errors;
%if &n_err.>0 %then %return;


/* clean up */
proc datasets library=work nolist;
	delete 	%scan(&check_levels.,2)
			;
quit;



/*----------------------------------
CHECK THAT CLUSTER VARS DO NOT VARY 
WITHIN EACH UNIT_ID
----------------------------------*/
%local check_consistency;
data; run;
%let check_consistency = &syslast.;

proc sql;
	create table &check_consistency. as 
		select		&unit_id.
					%do i=1 %to &n_cluster_vars_all.;
						%let var = %scan(&cluster_vars_all., &i.);
						,count(distinct &var.) as _xyz_n_v&i.
					%end;
		from		&input_data.
		group by	&unit_id.;
quit;

data _null_;
	set &check_consistency. end=last;
	length _xyz_var $32;
	array nvars{*} _xyz_n_v:;
	do i=1 to dim(nvars);
		_xyz_var = scan("&cluster_vars_all.", i);
		if nvars{i}>1 then put "ERROR: Multiple values detected within a single &unit_id. for cluster variable " _xyz_var ", where &unit_id. = " &unit_id.;
	end;
	drop i;
	_xyz_any_flag = max(of _xyz_n_v:)>1;
	
	retain _xyz_any_flag_consistency 0;
	if _xyz_any_flag then _xyz_any_flag_consistency = 1;
	if last then call symputx("any_flag_consistency", _xyz_any_flag_consistency, "L");
run;

%if &any_flag_consistency.=1 %then %let n_err = %eval(&n_err. + 1);

/* terminate macro if any errors were triggered */
%if &n_err.>0 %then %put ERROR: Macro terminated because of errors;
%if &n_err.>0 %then %return;


/* clean up */
proc datasets library=work nolist;
	delete 	%scan(&check_consistency.,2)
			;
quit;



/*----------------------------------
PROCESS CLUSTER VARIABLE WEIGHTS
----------------------------------*/
%local weights_ds_wide cluster_vars_long weights_ds_long1 weights_ds_long2 weights_ds_merged;
data; run;
%let weights_ds_wide = &syslast.;
data; run;
%let cluster_vars_long = &syslast.;
data; run;
%let weights_ds_long1 = &syslast.;
data; run;
%let weights_ds_long2 = &syslast.;
data; run;
%let weights_ds_merged = &syslast.;


/* if the user did not specify a weights dataset, create one, and equally weight 
each cluster variable */
%if %length(&cluster_var_weights_data.)=0 %then %do;
	data &weights_ds_wide.;
		%do i=1 %to &n_cluster_vars_all.;
			%let var = %scan(&cluster_vars_all., &i.);
			&var. = 1;
		%end;
	run;
%end; %else %do;
	data &weights_ds_wide.;
		set &cluster_var_weights_data.;
	run;
%end;


/* check that there is one and only one observation in the weights data */
data _null_;
	if 0=1 then set &weights_ds_wide. nobs=_xyz_n;
	call symputx("weights_nobs", _xyz_n, "L");
	%if %length(&cluster_var_weights_data.)=0 %then %do;
		if _xyz_n~=1 then put "ERROR: There must be one and only one observation in the cluster weights dataset specified in the cluster_var_weights_data macro parameter.";
	%end;
	stop;
run;

%if &weights_nobs.~=1 %then %let n_err = %eval(&n_err. + 1);


/* terminate macro if any errors were triggered */
%if &n_err.>0 %then %put ERROR: Macro terminated because of errors;
%if &n_err.>0 %then %return;


/* create a long dataset with each cluster variable name */
data &cluster_vars_long.;
	length lowvar $32 type $10;
	/* interval variables */
	%if %length(&cluster_vars_interval.)>0 %then %do i=1 %to %sysfunc(countw(&cluster_vars_interval.));
		%let var = %scan(&cluster_vars_interval., &i.);
		lowvar = lowcase("&var.");
		type = "interval";
		output;
	%end;
	/* binary variables */
	%if %length(&cluster_vars_binary.)>0 %then %do i=1 %to %sysfunc(countw(&cluster_vars_binary.));
		%let var = %scan(&cluster_vars_binary., &i.);
		lowvar = lowcase("&var.");
		type = "binary";
		output;
	%end;
	/* nominal variables */
	%if %length(&cluster_vars_nominal.)>0 %then %do i=1 %to %sysfunc(countw(&cluster_vars_nominal.));
		%let var = %scan(&cluster_vars_nominal., &i.);
		lowvar = lowcase("&var.");
		type = "nominal";
		output;
	%end;
run;


/* transpose the weights data */
proc transpose data=&weights_ds_wide. out=&weights_ds_long1.(rename=(col1=weight));
run;

data &weights_ds_long2.;
	set &weights_ds_long1.;
	length lowvar $32;
	lowvar = strip(lowcase(_name_));
	keep lowvar weight;
run;


/* merge together */
proc sort data=&cluster_vars_long.; by lowvar; run;
proc sort data=&weights_ds_long2.; by lowvar; run;
data &weights_ds_merged.;
	merge	&cluster_vars_long.(in=inbase)
			&weights_ds_long2.(in=inwts)
			;
	by lowvar;
	
	if inbase & not inwts then do;
		weight = 1;
		put "NOTE: A weight of one will be assumed for the following cluster variable, which was not found in the cluster weights dataset (specified in the cluster_var_weights_data parameter): " lowvar;
	end;
	if inbase & inwts & missing(weight) then do;
		weight = 1;
		put "NOTE: A weight of one will be assumed for the following cluster variable, which had a missing value in the cluster weights dataset (specified in the cluster_var_weights_data parameter): " lowvar;
	end;
	
	flag_unrecog = inwts & not inbase;
	if flag_unrecog then put "ERROR: The following cluster variable was found in the cluster weights dataset (specified in the cluster_var_weights_data parameter) but not in any of the cluster_vars parameters: " lowvar;
	flag_zero = inwts & inbase & weight=0;
	if flag_zero then put "ERROR: You specified a weight of zero for the following cluster variable. If that is what you intended, please simply remove the variable from both the cluster_vars parameters and the cluster weights dataset (specified in the cluster_var_weights_data parameter). " lowvar;
	flag_neg = inwts & inbase & not missing(weight) & weight<0;
	if flag_neg then put "ERROR: Negative cluster variable weights are not valid for this macro. You specified a negative weight for the following cluster variable in the cluster weights dataset (specified in the cluster_var_weights_data parameter): " lowvar;
	
	any_flag = max(of flag_:);
	
	if "&distance_use."="EUCLIDEAN" then adj_weight = sqrt(weight);
	else adj_weight = weight;
run;

%local any_flag_weights;
proc sql noprint;
	select	max(any_flag)
	into	:any_flag_weights trimmed
	from	&weights_ds_merged.;
quit;

%if &any_flag_weights.=1 %then %let n_err = %eval(&n_err. + 1);


/* terminate macro if any errors were triggered */
%if &n_err.>0 %then %put ERROR: Macro terminated because of errors;
%if &n_err.>0 %then %return;


/* clean up */
proc datasets library=work nolist;
	delete 	%scan(&weights_ds_wide.,2)
			%scan(&cluster_vars_long.,2)
			%scan(&weights_ds_long1.,2)
			%scan(&weights_ds_long2.,2)
			;
quit;



/*----------------------------------
COLLAPSE DATA
----------------------------------*/
%local collapsed;
data; run;
%let collapsed = &syslast.;

/* collapse the data so that there is only one observation per unit_id */
/*
We are explicitly doing this in the macro (rather than relying on the user to provide a dataset at the 
appropriate unit level for clustering) for the user's convenience. For instance, if the user has a dataset 
at the school level, with district and school identifiers, and with district- and school-level variables 
for clustering, then the user could feed this same dataset into the clustering macro for both district and 
school clustering (they would not need to create a separate district-level offshoot--because the macro will 
create that offshoot itself and then merge the cluster membership and distance variables back onto the 
original dataset, with its original structure).
*/
proc sort data=&input_data.(keep=&unit_id. &cluster_vars_all.) out=&collapsed. nodupkey;
	by &unit_id.;
run;

/* compare the number of observations to see if the collapsed dataset is smaller, 
and issue note if it is */
data _null_;
	if 0=1 then do;
		set &input_data. nobs=_xyz_n_orig;
		set &collapsed. nobs=_xyz_n_collapsed;
	end;
	if _xyz_n_orig > _xyz_n_collapsed then put "NOTE: Because there was more than one observation per unit_id in the input_data, the macro has collapsed the data before clustering. The clustering will be run on the collapsed data (with one observation per unit_id), and then the output_data returned by the macro will attach the cluster assignments to your original (pre-collapsed) data.";
	call symputx("nobs_orig", _xyz_n_orig, "L");
	call symputx("nobs_collapsed", _xyz_n_collapsed, "L");
	stop;
run;



/*----------------------------------
CREATE DUMMY VARIABLES FOR NOMINAL 
CLUSTER VARIABLE LEVELS
----------------------------------*/
%local ds_binary_prep ds_binary param_guide1 param_guide2;
data; run;
%let ds_binary_prep = &syslast.;
data; run;
%let ds_binary = &syslast.;
data; run;
%let param_guide1 = &syslast.;
data; run;
%let param_guide2 = &syslast.;


/* determine whether we need to create the dummy variables */
%local create_binaries;
%if %length(&cluster_vars_nominal.)>0 %then %let create_binaries = 1;
%else %let create_binaries = 0;


/* create dummy variables for each level of nominal variables */
/** We will use GLMSELECT to automate the dummy variable creation. The output design matrix will 
contain dummy variables for each level of the nominal variables we feed into the CLASS statement. **/
%if &create_binaries.=1 %then %do;
	data &ds_binary_prep.;
		set &collapsed.;
		_xyz_proxy = 1;
	run;

	proc glmselect data=&ds_binary_prep. outdesign(addinputvars prefix=_xyz_bin_ names)=&ds_binary.(drop=_xyz_proxy) namelen=200;
		class &cluster_vars_nominal. / order=internal;
		model _xyz_proxy = &cluster_vars_nominal. / noint selection=none;
		ods output ParameterNames=&param_guide1.;
	run;
	ods output close;
	
	data &param_guide2.;
		set &param_guide1.;
		length lowvar binvar $32;
		lowvar = strip(lowcase(scan(parameter, 1)));
		binvar = strip(lowcase(name));
		/* ^ technically, it should already be lowcase because we specified the prefix */
		parameter = strip(substr(parameter, length(lowvar)+1));
		keep lowvar binvar parameter;
		rename parameter=level;
	run;
%end; %else %do;
	data &ds_binary.;
		set &collapsed.;
	run;
	
	data &param_guide2.;
		length lowvar binvar $32 level $50;
		lowvar = "";
		binvar = "";
		level = "";
		if 0=1;
		/* ^ to create a zero-observation dataset, for merging later */
	run;
%end;


/* clean up */
proc datasets library=work nolist;
	delete 	%scan(&ds_binary_prep.,2)
			%scan(&param_guide1.,2)
			;
quit;



/*----------------------------------
FINALIZE CLUSTER VARIABLES META-DATA
----------------------------------*/
%local cluster_vars_meta;
data; run;
%let cluster_vars_meta = &syslast.;

proc sort data=&weights_ds_merged.; by lowvar; run;
proc sort data=&param_guide2.; by lowvar; run;
data &cluster_vars_meta.;
	merge &weights_ds_merged.(keep=lowvar type adj_weight) &param_guide2.;
	by lowvar;
	length finalvar $32;
	if not missing(binvar) then finalvar = strip(binvar);
	else finalvar = strip(lowvar);
run;

proc sort data=&cluster_vars_meta.; by lowvar binvar; run;

data _null_;
	set &cluster_vars_meta. end=last;
	by lowvar binvar;
	
	retain sum_weights 0;
	if first.lowvar then sum_weights = sum(sum_weights, adj_weight);
	/* ^ the sum of the weights is the denominator for the Gower measure, but for 
	nominal variables, we only want to count each nominal variable's weight once, 
	not repeatedly for each binary dummy we constructed for the nominal levels */
	
	call symputx(cats("final_cluster_var_name",_n_), finalvar, "L");
	call symputx(cats("final_cluster_var_type",_n_), type, "L");
	call symputx(cats("final_cluster_var_weight",_n_), adj_weight, "L");
	if last then do;
		call symputx("n_final_cluster_vars", _n_, "L");
		call symputx("sum_weights", sum_weights, "L");
	end;
run;

%local final_cluster_vars;
proc sql noprint;
	select	finalvar
	into	:final_cluster_vars separated by ' '
	from	&cluster_vars_meta.;
quit;


/* clean up */
proc datasets library=work nolist;
	delete 	%scan(&weights_ds_merged.,2)
			%scan(&param_guide2.,2)
			;
quit;



/*----------------------------------
RESCALE THE CLUSTER VARIABLES & 
APPLY VARIABLE WEIGHTS
----------------------------------*/
%local rescaled0 rescaled;
data; run;
%let rescaled0 = &syslast.;
data; run;
%let rescaled = &syslast.;

/* rescale the cluster variables */
%local stdize_mult stdize_options;
%let stdize_mult = %sysevalf(1/&sum_weights.);
%if &scaling_use.=GOWER %then %let stdize_options = %str(mult=&stdize_mult.);
%else %let stdize_options = ;

%if &scaling_use.~=NONE %then %do;
	proc stdize data=&ds_binary. out=&rescaled0. method=&scaling_sas. &stdize_options.;
		var &final_cluster_vars.;
	run;
%end; %else %do;
	data &rescaled0.;
		set &ds_binary.;
	run;
%end;

/* apply cluster variable weights */
proc sql;
	create table &rescaled. as 
		select	&unit_id.
				%do i=1 %to &n_final_cluster_vars.;
					,&&final_cluster_var_weight&i.. * &&final_cluster_var_name&i.. 
					as &&final_cluster_var_name&i..
				%end;
		from	&rescaled0.;
quit;


/* clean up */
proc datasets library=work nolist;
	delete 	%scan(&rescaled0.,2)
			;
quit;



/*----------------------------------
GENERATE SEED IF NOT SPECIFIED
----------------------------------*/
data _null_;
	call streaminit(0);
	/* ^ the zero makes SAS generate a default seed value and store it in the SYSRANDOM macro 
	variable */
run;

%local seed_use;
%if %length(&seed.)=0 %then %let seed_use = &sysrandom.;
%else %let seed_use = &seed.;



/*----------------------------------
RUN CLUSTERING PROCEDURE
----------------------------------*/
/* datasets */
%local clus_mem_master clus_mem_temp;

data;
	k = .;
	if 0=1;
	/* ^ the macro needs this to be a zero-observation dataset */
run;
%let clus_mem_master = &syslast.;

data; run;
%let clus_mem_temp = &syslast.;

data &output_summary.;
	k = .;
	if 0=1;
	/* ^ the macro needs this to be a zero-observation dataset */
run;

data; run;
%let clus_stat_temp = &syslast.;


/* least option for FASTCLUS */
%local least;
%if 		&distance_sas.=EUCLIDEAN %then %let least = 2;
%else %if 	&distance_sas.=MANHATTAN %then %let least = 1;
/* ^ note that this is distance_sas, not distance_use */


/* before running the cluster procedure, document the specifications in the log */
%put Clustering Specifications;
%put -------------------------;
%put ~Procedure:             	FASTCLUS;
%put ~Data type:             	&data_type.;
%put ~Centroid measure:      	&centroid_use.;
%put ~Distance measure:      	&distance_use.;
%put ~Scaling:               	&scaling_use.;
%put ~Weights dataset (if any):	&cluster_var_weights_data.;
%put ~k clusters value(s):   	&k_values.;
%put ~Seed:                  	&seed_use.;
%put ~Max iterations:        	&maxiter_use.;
%put ~Convergence criterion: 	&converge.;
%put;

%if &distance_use.=GOWER %then %do;
	%put NOTE: This procedure will implement clustering with the Gower distance, but the output will refer to the L1 (or Manhattan) distance. This is expected, and it is not an issue. We operationalized the Gower distance by applying the Manhattan distance to transformed and rescaled variables.;
%end;


/* begin loop through k values */
%do i=1 %to &n_k_values.;
	%let k = %scan(&k_values., &i.);
	/* Note that i is the index for the k_values list, and k is the actual number of 
	clusters. i does not necessarily (and likely would not) equal k. */
	
	proc fastclus data=&rescaled. maxclusters=&k. least=&least.
			maxiter=&maxiter_use. converge=&converge.
			replace=random random=&seed_use.
			out=&clus_mem_temp.(keep=&unit_id. cluster distance rename=(
				cluster=&output_var_cluster. 
				distance=&output_var_distance.
				))
			outstat=&clus_stat_temp.
			;
		/* ^ If the input data already has a variable named cluster or distance, it will be 
		overwritten by FASTCLUS in the output dataset. In the case of the cluster membership 
		output dataset (clus_mem_temp), that is okay for our purposes because we would not be 
		using that version of the input variable anyways (we will be merging to a different 
		version, pre-scaling and pre-weighting). As for the outstat dataset (clus_stat_temp), 
		we will leave it as is, and not attempt to reconcile any possible variable name 
		conflict. */
		var &final_cluster_vars.;
	run;
	
	data &clus_mem_master.;
		set &clus_mem_master. &clus_mem_temp.(in=innew);
		if innew then k=&k.;
	run;
	
	data &clus_mem_temp.;
	run;
	
	data &output_summary.;
		set &output_summary. &clus_stat_temp.(in=innew);
		if innew then k=&k.;
		%if &i.=&n_k_values. %then %do;
			length	_xyz_spec_data_type
					_xyz_spec_centroid_measure
					_xyz_spec_distance_measure
					_xyz_spec_scaling
					$20
					_xyz_spec_weights_ds
					$32
					;
			_xyz_spec_data_type = "&data_type.";
			_xyz_spec_centroid_measure = "&centroid_use.";
			_xyz_spec_distance_measure = "&distance_use.";
			_xyz_spec_scaling = "&scaling_use.";
			_xyz_spec_weights_ds = "&cluster_var_weights_data.";
			_xyz_spec_seed = &seed_use.;
			_xyz_spec_maxiter = &maxiter_use.;
			_xyz_spec_converge = &converge.;
			label	_xyz_spec_data_type="Data Type"
					_xyz_spec_centroid_measure="Centroid Measure"
					_xyz_spec_distance_measure="Distance Measure"
					_xyz_spec_scaling="Scaling"
					_xyz_spec_weights_ds="Weights Dataset (if any)"
					_xyz_spec_seed="Seed"
					_xyz_spec_maxiter="Max Iterations"
					_xyz_spec_converge="Convergence Criterion"
					;
		%end;
	run;
	
	data &clus_stat_temp.;
	run;
%end;
/* end loop through k values */


/* check observations */
data _null_;
	if 0=1 then set &clus_mem_master. nobs=_xyz_nobs;
	_xyz_expected = &nobs_collapsed.*&n_k_values.;
	if _xyz_nobs~=_xyz_expected then do;
		put "ERROR: Unexpected observation count in dataset &clus_mem_master. (either an issue in the macro logic or macro inputs).";
		put "Expected observations: " _xyz_expected;
		put "Actual observations: " _xyz_nobs;
	end;
	/* ^ we are not terminating the macro for this error */
	stop;
run;


/* clean up */
proc datasets library=work nolist;
	delete 	%scan(&rescaled.,2)
			%scan(&clus_mem_temp.,2)
			%scan(&clus_stat_temp.,2)
			;
quit;



/*----------------------------------
SUMMARIZE CLUSTER CENTROIDS
----------------------------------*/
/* datasets */
%local centroids_prep centroids_mean centroids_mean_xp centroids_med centroids_med_xp centroids_merged centroids_final;
data; run;
%let centroids_prep = &syslast.;
data; run;
%let centroids_mean = &syslast.;
data; run;
%let centroids_mean_xp = &syslast.;
data; run;
%let centroids_med = &syslast.;
data; run;
%let centroids_med_xp = &syslast.;
data; run;
%let centroids_merged = &syslast.;
data; run;
%let centroids_final = &syslast.;


/* attach cluster membership to the pre-scaled and pre-weighted (but post-binary-creation) 
cluster variables */
/** If there is more than one k value, this will be a many:one merge, joining the cluster 
membership data to the pre-scaled/pre-weighted cluster variable data. If there is only one 
k value, it will be a one:one merge.
If the cluster variable data has X observations (for X units to cluster), then the cluster 
membership data has X * n_k observations, where n_k is the number of k values. **/
proc sql;
	create table &centroids_prep. as 
		select		a.k,
					a.&output_var_cluster. as _xyz_cluster,
					b.*
		from		&clus_mem_master. as a left join
					&ds_binary. as b
					on a.&unit_id.=b.&unit_id.
					;
quit;


/* check observations */
data _null_;
	if 0=1 then set &centroids_prep. nobs=_xyz_nobs;
	_xyz_expected = &nobs_collapsed.*&n_k_values.;
	if _xyz_nobs~=_xyz_expected then do;
		put "ERROR: Unexpected observation count in dataset &centroids_prep. (either an issue in the macro logic or macro inputs).";
		put "Expected observations: " _xyz_expected;
		put "Actual observations: " _xyz_nobs;
	end;
	/* ^ we are not terminating the macro for this error */
	stop;
run;


/* compute unweighted cluster means */
proc means data=&centroids_prep. noprint nway;
	class k _xyz_cluster;
	var &final_cluster_vars.;
	output out=&centroids_mean.(drop=_type_ _freq_) mean=;
run;

proc transpose data=&centroids_mean. out=&centroids_mean_xp.(keep=k _xyz_cluster _name_ col1 rename=(col1=mean));
	by k _xyz_cluster;
run;


/* compute unweighted cluster medians */
proc means data=&centroids_prep. noprint nway;
	class k _xyz_cluster;
	var &final_cluster_vars.;
	output out=&centroids_med.(drop=_type_ _freq_) median=;
run;

proc transpose data=&centroids_med. out=&centroids_med_xp.(
			keep=k _xyz_cluster _name_ col1 rename=(col1=median)
			where=(find(_name_, "_xyz_bin_")~=1) /* excluding nominal variables */
		);
	by k _xyz_cluster;
run;


/* delete centroids prep dataset to clear space (this may be a large dataset if there 
were many different k values) */
proc datasets library=work nolist;
	delete 	%scan(&centroids_prep.,2)
			;
quit;


/* merge */
/** combine the mean and median data **/
proc sort data=&centroids_mean_xp.; by k _xyz_cluster _name_; run;
proc sort data=&centroids_med_xp.; by k _xyz_cluster _name_; run;
data &centroids_merged.;
	merge &centroids_mean_xp. &centroids_med_xp.;
	by k _xyz_cluster _name_;
	length finalvar $32;
	finalvar = strip(lowcase(_name_));
	drop _name_;
run;

/** attach the level for nominal variables **/
proc sort data=&centroids_merged.; by finalvar; run;
proc sort data=&cluster_vars_meta.; by finalvar; run;
data &centroids_final.;
	merge &centroids_merged. &cluster_vars_meta.(keep=finalvar lowvar level);
	by finalvar;
	if missing(level) then level="N/A";
	drop finalvar;
	rename lowvar=variable;
run;

proc sort data=&centroids_final.; by k _xyz_cluster variable level; run;


/* create output dataset */
data &output_centroids.;
	retain k _xyz_cluster variable level;
	set &centroids_final.;
	%if &create_binaries.=0 %then %do;
		drop level;
	%end;
	rename _xyz_cluster=cluster;
run;


/* clean up */
proc datasets library=work nolist;
	delete 	%scan(&centroids_mean.,2)
			%scan(&centroids_mean_xp.,2)
			%scan(&centroids_med.,2)
			%scan(&centroids_med_xp.,2)
			%scan(&centroids_merged.,2)
			%scan(&centroids_final.,2)
			;
quit;



/*----------------------------------
CREATE THE OUTPUT DATA
----------------------------------*/
/* in this section, we will attach the cluster membership and distance data to 
the original input data */


/* datasets */
%local orig_w_order xp_cluster xp_distance merged_output_data;
data; run;
%let orig_w_order = &syslast.;
data; run;
%let xp_cluster = &syslast.;
data; run;
%let xp_distance = &syslast.;
data; run;
%let merged_output_data = &syslast.;


/* create a duplicate of the input data, but with a variable identifying the original 
order of the observations, so that we can return the output data in the same order 
after merging */
data &orig_w_order.;
	set &input_data.;
	_xyz_temp_order = _n_;
run;


/* merge the data */
%if &n_k_values.>1 %then %do;
	/* if there is more than one k value, we need to first transpose the cluster and 
	distance data and then attach it to the original data */
	proc sort data=&clus_mem_master.; by &unit_id. k; run;

	proc transpose data=&clus_mem_master. out=&xp_cluster.(drop=_name_ _label_) prefix=&output_var_cluster._k;
		by &unit_id.;
		id k;
		var &output_var_cluster.;
	run;
	
	proc transpose data=&clus_mem_master. out=&xp_distance.(drop=_name_ _label_) prefix=&output_var_distance._k;
		by &unit_id.;
		id k;
		var &output_var_distance.;
	run;
	
	proc sql;
		create table &merged_output_data.(drop=_xyz_temp_id1 _xyz_temp_id2) as 
			select		*
			from		&orig_w_order. as a
						left join &xp_cluster.(rename=(&unit_id.=_xyz_temp_id1)) as b
						on a.&unit_id.=b._xyz_temp_id1
						left join &xp_distance.(rename=(&unit_id.=_xyz_temp_id2)) as c
						on a.&unit_id.=c._xyz_temp_id2
			order by 	a._xyz_temp_order
						;
	quit;
%end; %else %do;
	/* if there is only one k value, we can skip the transpose and directly attach the 
	cluster and distance data to the original data */
	proc sql;
		create table &merged_output_data.(drop=_xyz_temp_id1) as 
			select		*
			from		&orig_w_order. as a
						left join &clus_mem_master.(rename=(&unit_id.=_xyz_temp_id1)) as b
						on a.&unit_id.=b._xyz_temp_id1
			order by 	a._xyz_temp_order
						;
	quit;
%end;


/* check observations */
data _null_;
	if 0=1 then set &merged_output_data. nobs=_xyz_nobs;
	_xyz_expected = &nobs_orig.;
	if _xyz_nobs~=_xyz_expected then do;
		put "ERROR: Unexpected observation count in dataset &merged_output_data. (either an issue in the macro logic or macro inputs).";
		put "Expected observations: " _xyz_expected;
		put "Actual observations: " _xyz_nobs;
	end;
	/* ^ we are not terminating the macro for this error */
	stop;
run;


/* finalize and clean up */
data &output_data.;
	set &merged_output_data.;
	drop _xyz_temp_order;
run;


/* clean up */
proc datasets library=work nolist;
	delete 	%scan(&orig_w_order.,2)
			%scan(&xp_cluster.,2)
			%scan(&xp_distance.,2)
			%scan(&merged_output_data.,2)
			;
quit;



/*----------------------------------
FINAL CLEAN UP
----------------------------------*/
proc datasets library=work nolist;
	delete 	%scan(&collapsed.,2)
			%scan(&ds_binary.,2)
			%scan(&cluster_vars_meta.,2)
			%scan(&clus_mem_master.,2)
			;
quit;



/*----------------------------------
END MACRO DEFINITION
----------------------------------*/
%mend cluster_k;

