/******************************************************************************************
Program:		Cluster_k.sas


Description:	This program defines cluster_k, a SAS macro for clustering districts and 
				schools in education as implemented in Litwok et al. (2022):

				cluster_k		Performs k-means or k-medians clustering to partition 
								data into groups.


Authors:		Azim Shivji, Daniel Litwok, and Robert B. Olsen


References: 	Shivji, A., Litwok, D., & Olsen, Robert B. (2022). Cluster_k: SAS Macro for
					Clustering Districts and Schools for Impact Studies in Education.
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
PARAMETER GUIDE
----------------------------------*/
/*
+------------+
| input_data |
+------------+
~~~Mandatory/Optional to Specify: Mandatory

~~~Description: The name of the dataset containing the data you wish to cluster.

~~~Acceptable Values: Any valid one- or two-level dataset name (note that two-level names 
including the libref are compatible). Examples:
	input_data = mydata
	input_data = mylib.mydata

~~~Default Value: None

~~~Notes: If there is more than one observation per unique unit_id in input_data, then the 
macro will do the following, for the user's convenience:
a)	Collapse the data before clustering, so that there is only one observation per unique 
	unit_id. (Before doing this, the macro will also check whether the specified cluster 
	variables are consistent among observations with the same unit_id.)
b)	Attach the final results (the cluster assignments variable and the distance variable) 
	to the original data structure (with multiple observations per unique unit_id).
This may be useful for users who have a nested data structure. See the Examples section for 
a sample.


+---------+
| unit_id |
+---------+
~~~Mandatory/Optional to Specify: Mandatory

~~~Description: The name of a variable in your input_data that uniquely identifies the 
units you wish to cluster (e.g., a district ID if you want to cluster districts, a school 
ID if you want to cluster schools, etc.)

~~~Accetapable Values: Any valid variable name in input_data that uniquely identifies the
units you wish to cluster

~~~Default Value: None


+-----------------------+
| cluster_vars_interval |
+-----------------------+
~~~Mandatory/Optional to Specify: Optional (but the three cluster_vars_[type] parameters 
cannot all be blank; you must specify at least one variable to cluster in at least one 
of the three cluster_vars_[type] parameters)

~~~Description: The names of any interval variables you want to use in the clustering. 
This is one of the three parameters where you specify cluster variables (also known as 
features in the clustering literature): cluster_vars_interval, cluster_vars_binary, 
and cluster_vars_nominal.
Interval variables must be numeric variables.

~~~Acceptable Values: A space-delimited list of variable names. Abbreviated variable lists 
(such as name prefix lists or numbered range lists) are not supported. You may get either errors 
or incorrect results if you use abbreviated variable lists. Variables identified as cluster
variables cannot be missing for any observations.
For instance, the following space-delimited variable list is acceptable:
	cluster_vars_interval = var1 var2 var3
Abbreviated variable lists such as the following are NOT acceptable:
	cluster_vars_interval = var:
	cluster_vars_interval = var1-var3

~~~Default Value: None


+---------------------+
| cluster_vars_binary |
+---------------------+
~~~Mandatory/Optional to Specify: Optional (but the three cluster_vars_[type] parameters 
cannot all be blank; you must specify at least one variable to cluster in at least one 
of the three cluster_vars_[type] parameters)

~~~Description: The names of any binary variables you want to use in the clustering. 
This is one of the three parameters where you specify cluster variables (also known as 
features in the clustering literature): cluster_vars_interval, cluster_vars_binary, 
and cluster_vars_nominal.
Binary variables must be numeric variables, and the only acceptable non-missing values are 
0 and 1.

~~~Acceptable Values: A space-delimited list of variable names. Abbreviated variable lists 
(such as name prefix lists or numbered range lists) are not supported. You may get either errors 
or incorrect results if you use abbreviated variable lists. Variables identified as cluster
variables cannot be missing for any observations.
For instance, the following space-delimited variable list is acceptable:
	cluster_vars_binary = var1 var2 var3
Abbreviated variable lists such as the following are NOT acceptable:
	cluster_vars_binary = var:
	cluster_vars_binary = var1-var3

~~~Default Value: None


+----------------------+
| cluster_vars_nominal |
+----------------------+
~~~Mandatory/Optional to Specify: Optional (but the three cluster_vars_[type] parameters 
cannot all be blank; you must specify at least one variable to cluster in at least one 
of the three cluster_vars_[type] parameters)

~~~Description: The names of any nominal variables you want to use in the clustering. 
This is one of the three parameters where you specify cluster variables (also known as 
features in the clustering literature): cluster_vars_interval, cluster_vars_binary, 
and cluster_vars_nominal.
Nominal variables may be either numeric or character variables.

~~~Acceptable Values: A space-delimited list of variable names. Abbreviated variable lists 
(such as name prefix lists or numbered range lists) are not supported. You may get either errors 
or incorrect results if you use abbreviated variable lists. Variables identified as cluster
variables cannot be missing for any observations.
For instance, the following space-delimited variable list is acceptable:
	cluster_vars_nominal = var1 var2 var3
Abbreviated variable lists such as the following are NOT acceptable:
	cluster_vars_nominal = var:
	cluster_vars_nominal = var1-var3

~~~Default Value: None


+-----------------+
| k_clusters_list |
+-----------------+
~~~Mandatory/Optional to Specify: Mandatory

~~~Description: A number or list of numbers specifying the maximum number of clusters (i.e., 
the "k" value in k-means or k-medians clustering). (This value corresponds to the MAXCLUSTERS 
option in PROC FASTCLUS, the clustering procedure used by this macro.)

If this parameter is a single value (e.g., k_clusters_list=4), then the clustering procedure 
will be run once, with this value as the maximum number of clusters.

However, you can also supply a list of k values to the k_clusters_lists parameter, in which 
case the macro will run the clustering procedure multiple times, once for each k value. You 
can use this feature to try out multiple values of k and search for your preferred value.

~~~Acceptable Values: There are several different ways to specify values for k_clusters_list.

If you only want to specify a single k value, such as 4:
	k_clusters_list = 4

If you want the macro to run the clustering procedure for multiple values of k, you can specify 
a space-delimited list of k values:
	k_clusters_list = 4 5 6

Alternatively, you can use a colon (:) to specify a range of k values. 4:6 means the numbers 
4, 5, and 6. E.g.:
	k_clusters_list = 4:6

And you can use both ranges and space-delimited lists together:
	k_clusters_list = 2 5 7:10 15 20:22
The above example would tell the macro to run the clustering for each of the following k values: 
2, 5, 7, 8, 9, 10, 15, 20, 21, and 22.

~~~Default Value: None

~~~Notes: The k value represents the maximum number of clusters permitted. The actual clusters 
that the procedure finds may be fewer than the maximum specified.


+------------------+
| centroid_measure |
+------------------+
~~~Mandatory/Optional to Specify: Optional, because there is a default value

~~~Description: Specifies whether the macro will perform k-means or k-medians clustering. 
More precisely, this parameter specifies how the cluster centroids (or cluster centers) 
are computed. See Notes for more details.

~~~Acceptable Values: MEAN or MEDIAN

The values are case-insensitive.

~~~Default Value: The default values for the centroid_measure and distance_measure macro 
parameters all vary based on what you specify in the macro. See the documentation for the 
distance_measure for a table of default values.

~~~Notes: To understand the purpose of the centroid_measure parameter, we first briefly 
sketch the clustering algorithm. The clustering procedure begins by randomly choosing a set of 
initial cluster centroids from the data. The procedure assigns each observation to the cluster of 
the nearest centroid. After each observation has been assigned to a cluster, the procedure 
recomputes the centroids. The assignment and centroid recomputation steps repeat until convergence 
or until the maximum number of iterations is reached. The centroid_measure parameter determines 
whether the procedure will compute the centroids using the mean or median of observations 
assigned to the cluster.


+------------------+
| distance_measure |
+------------------+
~~~Mandatory/Optional to Specify: Optional, because there is a default value

~~~Description: Specifies how the distance of an observation from a cluster centroid is 
measured.

~~~Acceptable Values:

	Value*			Description
	------			-----------
	EUCLIDEAN		Euclidean distance (also known as the L2 norm)

					Supported for k-means clustering of interval data.

					When distance_measure=EUCLIDEAN, the clustering procedure 
					will attempt "to minimize the root mean squared difference 
					between the data and the corresponding cluster means" (as stated 
					in the SAS documentation for PROC FASTCLUS).

	MANHATTAN		Manhattan distance (also known as the L1 norm, or the 
					absolute-value distance)

					Supported for k-medians clustering of interval data.

					When distance_measure=MANHATTAN, the clustering procedure 
					will attempt "to minimize the mean absolute difference 
					between the data and the corresponding cluster medians" (as stated 
					in the SAS documentation for PROC FASTCLUS).

	GOWER			Gower's dissimilarity coefficient
					
					Supported for k-medians clustering of mixed data (including 
					interval and binary/nominal data).

					PROC FASTCLUS (the SAS clustering procedure this macro uses) does 
					not natively support the Gower measure. The macro operationalizes 
					the Gower distance by applying the Manhattan distance to 
					transformed and rescaled versions of the cluster variables. 
					The transformation and rescaling involve the following steps:
						1)	Nominal cluster variables are transformed into a series of 
							binary dummy variables. A binary variable is constructed 
							for each level of the nominal variable (e.g., a nominal 
							variable for Census region would be transformed into 
							four binary variables for the four regions--West, Midwest, 
							Northeast, and South).
						2)	All cluster variables (including the binaries constructed 
							from nominal variables) are rescaled, as described in the 
							documentation for the macro's scaling parameter (see the 
							description of the scaling when scaling=GOWER).
					Once the cluster variables are transformed and rescaled, the macro 
					can simply calculate the Gower distance between an observation and 
					a cluster centroid using the Manhattan distance in PROC FASTCLUS.

* The value is case-insensitive.

~~~Notes: Only specific combinations of the centroid_measure, distance_measure, and 
data type (as specified in the cluster_vars_[type] parameters) are supported by the 
macro. The table below lists all compatible combinations of these parameters:

	centroid_measure	distance_measure		Supported		
	Macro Parameter		Macro Parameter			Data Types		
	----------------	----------------    	-------------	
	MEAN				EUCLIDEAN				interval
	MEDIAN				MANHATTAN				interval
	MEDIAN				GOWER					interval
	MEDIAN				GOWER					binary/nominal
	MEDIAN				GOWER					mixed (interval and binary/nominal)

Additionally, the following combination is technically supported (meaning the macro will 
not return an error if you specify this combination), but is not recommended:

	centroid_measure	distance_measure		Supported
	Macro Parameter		Macro Parameter			Data Types
	----------------	----------------    	-------------
	MEAN				EUCLIDEAN				mixed

~~~Default Value: The default values for the centroid_measure and distance_measure 
macro parameters vary based on what you specify in the macro.

If you only specify the type of data you want to cluster (in the cluster_vars_[type] parameters), 
then these are the default values used by the macro:

	User-Specified Data Type 		Default				Default
	(from cluster_vars_[type])		centroid_measure	distance_measure
	--------------------------		----------------	----------------
	interval						MEAN				EUCLIDEAN
	binary/nominal					MEDIAN				GOWER
	mixed							MEDIAN				GOWER

Defaults will vary if you specify additional information. If you specify interval-only data and the 
MEDIAN centroid_measure, then the default distance_measure will be MANHATTAN. Likewise, if you specify 
interval-only data and the MANHATTAN distance_measure, then the default centroid_measure will be 
MEDIAN.

The macro documents the final specifications (including any default values assigned) in two places:
	(1) the log, right before PROC FASTCLUS runs
	(2) the output_summary dataset (see the output_summary parameter)


+---------+
| scaling |
+---------+
~~~Mandatory/Optional to Specify: Optional, because there is a default value

~~~Description: Specifies whether and how the cluster variables should be rescaled 
before performing the clustering.

~~~Acceptable Values:

	Value*		Location	Scale				Formula (in PROC SQL pseudo-code)
	------		--------	-----				---------------------------------
	STD			Mean		Standard deviation	(x - mean(x)) / std(x)
	RANGE		Minimum		Range				(x - min(x)) / range(x)
	GOWER		Minimum		Range*v				(x - min(x)) / (range(x) * v)
	EUCLEN		0			Euclidean length	x / sqrt(sum(x**2))
	NONE		N/A			N/A					N/A
	
* The value is case-insensitive.

For the Gower measure:
(1)	v = the sum of the cluster variable weights (or simply the number of cluster 
variables, if there are no weights--see the documentation for the cluster_var_weights_data 
macro parameter)
(2) Gower here refers not to the distance measure, itself, but rather the scaling 
done in support of that measure. This scaling allows the macro to compute the Gower 
distance simply as the absolute value of the difference between an observation in the 
data and a cluster centroid.

~~~Default Value: If the distance_measure equals GOWER, then the macro uses GOWER as the default 
scaling. This is not merely a default; it also overrides any value the user specified (i.e., 
if the user specifies distance_measure=GOWER, then the user specification for scaling will be 
ignored, and the macro will use the default GOWER scaling). For all other cases (where the 
distance_measure is not GOWER), there is no default value for the scaling parameter.

~~~Notes: While the macro supports several common scaling options, you may wish to use 
a different or customized scaling. In that case, we recommend you rescale your data 
(with SAS's PROC STDIZE or other methods) before running the macro. Then, you can 
feed the rescaled data into the macro and specify scaling=NONE, so that no further 
scaling will be done by the macro.
	Also note that the rescaled versions of the cluster variables, produced by this 
scaling parameter, will be discarded after clustering. The output_data returned by 
the macro will contain the original versions of these variables, not the rescaled 
versions.


+--------------------------+
| cluster_var_weights_data |
+--------------------------+
~~~Mandatory/Optional to Specify: Optional, because there is a default value

~~~Description: The name of a dataset containing weights for the cluster variables.

Note that these are VARIABLE weights, not OBSERVATION weights. The dataset should have 
ONLY ONE observation, and the variables in the dataset should have the values of the 
cluster variable weights. See the Examples section below for sample datasets.

This parameter is ONLY supported for the EUCLIDEAN, MANHATTAN, and GOWER distance_measure 
values. Differential weights are not supported for the BINARY, GLOBALFREQ, and RELATIVEFREQ
distance_measure values.

~~~Acceptable Values: Any valid dataset name that contains weights for the cluster variables.

~~~Default Value: Each cluster variable is equally weighted, with a weight of 1. 
Additionally, if the user does specify a dataset for the cluster_var_weights_data 
parameter, then any cluster variables that are named in the cluster_vars_[type] macro 
parameters but do not appear in the cluster_var_weights_data dataset will be assigned 
a weight of 1.

~~~Discussion: The scaling macro parameter and this cluster_var_weights_data parameter are 
tied closely together. The scaling is a form of weighting, but it equalizes the importance of 
all cluster variables, regardless of their original measurement units. However, a researcher 
may have a prior basis to distinguish the relative weights of different variables. 
The researcher may wish to adjust the relative importance of variables in the clustering, to 
for instance, inflate one variable's importance and deflate another's. Tipton (2013) notes 
that these weights can take advantage of information the researcher has on "the importance 
of particular covariates in explaining potential treatment effect heterogeneity."
	If you do want to apply different weights to different cluster variables, then you can 
supply a dataset to this macro parameter (the cluster_var_weights_data parameter). Note that 
the scaling parameter and the cluster_var_weights_data parameter are not mutually exclusive. 
You may use both if you wish. In fact, it may be useful to use both--to first adjust the 
cluster variables to a common scale (since the macro always performs the scaling before the 
weighting) and then distinguish their importance according to the weights you supply.
	It is important to understand that the macro will apply your given weights to the 
variables, themselves (after performing any scaling requested in the scaling parameter), not 
to the distances. Therefore, the macro tailors your weights according to the distance_measure 
you are using.
	For instance, if you want to cluster two variables and double the weight of one variable, 
x1, and halve the weight of the other variable, x2, then...

	(1)	If you are using the Manhattan or Gower distances, the macro uses the weights you 
		specified, as is:
						User-Specified 		Weight the Macro
			Variable	Weight				Uses
			--------	--------------		----------------
			x1			2					2
			x2			0.5					0.5
		
	(2)	However, if you are using the Euclidean distance measure, and you want a weighted 
		Euclidean distance for variables x1 and x2, the equation for that distance is...
			d(i, j) = sqrt(w1*((x1_i - x1_j)**2) + w2*((x2_i - x2_j)**2))
		where...
			...d(i, j) is the distance between observations i and j
			...x1 and x2 are two cluster variables
			...w1 is 2, the desired weight for variable x1
			...w2 is 0.5, the desired weight for variable x2
			...we use the SAS syntax of **2 to indicate square and sqrt() to indicate a square root
		In this equation, the weights are applied to the squared distances.
		That equation is equivalent with directly weighting x1 and x2 by the square root of w1 and w2, 
		respectively, instead of weighting the squared distances, as in...
			d(i, j) = sqrt((sqrt(w1)*(x1_i - x1_j))**2 + (sqrt(w2)*(x2_i - x2_j))**2)
		Since this macro applies the weights to the variables (x1 and x2) directly, and not to the 
		distances, the macro uses this second, equivalent form of the equation and takes the square 
		roots of the weights you specified for variables x1 and x2:
						User-Specified 		Weight the Macro
			Variable	Weight				Uses
			--------	--------------		----------------
			x1			2					sqrt(2)
			x2			0.5					sqrt(0.5)
			
	(3)	The macro does not support weights for the BINARY, GLOBALFREQ, or RELATIVEFREQ distance 
		measures.

~~~Technical Notes:
(1) You do not need to specify weights for all cluster variables in the cluster_var_weights_data 
dataset. Any cluster variables that you do not specify in this dataset will be assigned a 
weight of 1.
(2) It doesn't matter if the order of the variables in the cluster_var_weights_data 
dataset does not match the order of the variables, either in your input_data or in 
the cluster_vars_[type] macro parameters. But the variable names must match. If you specified 
cluster_vars_interval=x1, but your cluster_var_weights_data dataset misnamed the variable 
as x_1, then the macro will not be able to match the weight with the appropriate variable. 
An error will trigger if the macro finds a variable in the cluster_var_weights_data dataset 
that does not appear in the cluster_vars_[type] macro parameters.

~~~Examples:
Example #1:
----------

* create the dataset with the cluster variable weights;
data weights;
	x1 = 2;
	x2 = 0.5;
	x3 = 1;
run;

* run the clustering macro;
%cluster_k(
	input_data = mydata,
	unit_id = district_id,
	cluster_vars_interval = x1 x3,
	cluster_vars_binary = x2,
	cluster_vars_nominal = x4,
	k_clusters_list = 6,
	centroid_measure = MEAN,
	distance_measure = GOWER,
	cluster_var_weights_data = weights
);
* ^ note that the macro will assign a weight of 1 to cluster variable x4 because it 
will not be able to find that variable in the weights dataset;
* also note that the scaling parameter was not specified here because, since distance_measure=GOWER, 
the GOWER scaling is implied;

Example #2:
----------
In describing the weighted Euclidean distance, Tipton (2013) writes, "...if there is no 
information regarding which covariate is a more important or better predictor of treatment 
effect heterogeneity, then the obvious solution is to use inverse-variance weights..."

As we showed above, weighting the squared distance for a variable by weight w1, in the weighted 
Euclidean distance equation, is equivalent with directly weighting that variable's values by 
the square root of w1. Therefore, to apply inverse-variance weights, we can simply standardize 
the variable, dividing it by its standard deviation (since the inverse of the standard deviation 
is the square root of the inverse of the variance).

The best way to implement that approach in this macro is to use the scaling parameter, as in:
%cluster_k(
	input_data = mydata,
	unit_id = district_id,
	cluster_vars_interval = x1 x2 x3,
	cluster_vars_binary = ,
	cluster_vars_nominal = ,
	k_clusters_list = 4,
	centroid_measure = MEAN,
	distance_measure = EUCLIDEAN,
	scaling = STD,
	cluster_var_weights_data = ,
);

Note that, in addition to dividing by the standard deviation, when you specify scaling=STD, the 
macro will also center the cluster variables around their means.

However, if you prefer, you can use the cluster_var_weights_data parameter to apply inverse-variance 
weights, instead of using the scaling parameter. The example below shows how (but note that this example 
below would not center the cluster variables around their means):

* create the dataset with the cluster variable weights;
proc sql;
	create table weights as 
		select	1/var(x1) as x1,
				1/var(x2) as x2,
				1/var(x3) as x3
		from	mydata;
quit;

* run the clustering macro;
%cluster_k(
	input_data = mydata,
	unit_id = district_id,
	cluster_vars_interval = x1 x2 x3,
	cluster_vars_binary = ,
	cluster_vars_nominal = ,
	k_clusters_list = 4,
	centroid_measure = MEAN,
	distance_measure = EUCLIDEAN,
	scaling = NONE,
	cluster_var_weights_data = weights
);


+------+
| seed |
+------+
~~~Mandatory/Optional to Specify: Optional, because there is a default value

~~~Description: A positive integer specifying the seed the clustering procedure will use.

~~~Acceptable Values: Positive integers

~~~Default Value: Automatically generated in SAS by running: CALL STREAMINIT(0)


+---------+
| maxiter |
+---------+
~~~Mandatory/Optional to Specify: Optional, because there is a default value

~~~Description: This parameter corresponds to the MAXITER option in PROC FASTCLUS and (as 
stated in the SAS documentation) "specifies the maximum number of iterations for recomputing 
cluster [centroids]."

~~~Acceptable Values: Positive integers

~~~Default Value: Depends on the value of the distance_measure parameter:

	distance_measure	default maxiter
	----------------	---------------
	EUCLIDEAN			10
	MANHATTAN			20
	GOWER				20

~~~Notes: We chose to use the same default values as PROC FASTCLUS uses for the MAXITER 
option. In practice, you may find that the default is relatively low, and you may see 
this message after PROC FASTCLUS runs: "WARNING: Iteration limit reached without convergence." 
In that case, you may consider increasing the maxiter parameter value and/or increasing the 
converge parameter value.


+----------+
| converge |
+----------+
~~~Mandatory/Optional to Specify: Optional, because there is a default value

~~~Description: The convergence criterion for the clustering algorithm. This parameter 
corresponds to the CONVERGE option in PROC FASTCLUS.

~~~Acceptable Values: Any non-negative numeric value

~~~Default Value: 0.0001


+-------------+
| output_data |
+-------------+
~~~Mandatory/Optional to Specify: Optional, because there is a default value

~~~Description: The name of the output dataset that will be produced by the macro, and 
which will attach to your original data (supplied in the input_data parameter) the cluster 
assignments and the distances from the cluster centroids.

~~~Acceptable Values: Any valid one- or two-level dataset name (note that two-level names 
including the libref are compatible). Examples:
	output_data = outdata
	output_data = mylib.outdata

~~~Default Value: _out_data

~~~Notes: The output_data will maintain the structure of the input_data. If the input_data 
had multiple observations per unique unit_id, then the output_data will as well. See the 
Notes for the input_data parameter.


+--------------------+
| output_var_cluster |
+--------------------+
~~~Mandatory/Optional to Specify: Optional, because there is a default value

~~~Description: The name of the variable in the output dataset containing the cluster 
assignments.

~~~Default Value: _cluster


+---------------------+
| output_var_distance |
+---------------------+
~~~Mandatory/Optional to Specify: Optional, because there is a default value

~~~Description: The name of the variable in the output dataset containing the distance 
of each observation from its cluster centroid.

~~~Default Value: _distance


+------------------+
| output_centroids |
+------------------+
~~~Mandatory/Optional to Specify: Optional, because there is a default value

~~~Description: The name of the output dataset that will be produced by the macro, and 
which will report the mean and median of each cluster variable for each cluster (and each 
value of k, if multiple values were specified in the k_clusters_list parameter).

Note that the means and medians are unweighted, and they are reported at the unique 
unit_id level. If your input_data contained multiple observations per unique unit_id, 
the means and medians will be computed from a deduplicated dataset, with only one 
observation per unique unit_id.

~~~Acceptable Values: Any valid one- or two-level dataset name (note that two-level names 
including the libref are compatible). Examples:
	output_centroids = outcentroids
	output_centroids = mylib.outcentroids

~~~Default Value: _out_centroids


+----------------+
| output_summary |
+----------------+
~~~Mandatory/Optional to Specify: Optional, because there is a default value

~~~Description: The name of the output dataset that will be produced by the macro, and 
which will collect any summary statistics reported by PROC FASTCLUS, in the procedure's 
OUTSTAT option. Which statistics are reported will vary based on whether the centroid_measure 
equals MEAN or MEDIAN. The pseudo-F statistic, for instance, is only reported when the 
centroid_measure equals MEAN. If you specified multiple values of k in the k_clusters_list 
parameter, the output_summary dataset will collect the summary statistics for each k value 
(i.e., for each clustering run) and stack them together.

This output dataset will also include variables documenting the final specifications of 
the clustering, including any default values the macro assigned. These variables all begin 
with the prefix "_xyz_spec_" and report specifications such as the centroid measure, distance 
measure, scaling, and seed.

~~~Acceptable Values: Any valid one- or two-level dataset name (note that two-level names 
including the libref are compatible). Examples:
	output_summary = outsummary
	output_summary = mylib.outsummary

~~~Default Value: _out_summary

~~~Notes: Summary statistics on the cluster variables reported in this dataset are based 
on the adjusted versions of those variables. If applicable, these would be the versions 
of the variables that were rescaled (according to the scaling parameter), transformed 
(into binaries if the variable was nominal), and weighted (according to your specifications 
in the cluster_var_weights_data). If you want to report on the mean/median of the clustered 
variables in each cluster, in their original scale, we recommend using the mean/median reported 
in the output_centroids dataset, rather than the statistics reported in this output_summary 
dataset.
*/



/*----------------------------------
EXAMPLES
----------------------------------*/
/*

* PREP
-----------------------;
* run macro definition program;
%include "[your file path to the program]\Site Selection Macros.sas" / lrecl=32767;

* load the sample data from the macro GitHub repository;
libname dr "[your file path to the data]";
data base_school;
	set dr.base_school;
run;

* This is a school-level dataset containing both district-level 
and school-level variables. In this example, we will first cluster 
the districts, then cluster the schools.;


* CLUSTER DISTRICTS
-----------------------;
* If you want to apply variable weights, create a weights dataset.;
data weights_district;
	nschools_elig = 5;
	epp_d_imp = 0.25;
	region_d = 1;
run;

* run clustering macro for districts;
%cluster_k(
	input_data = base_school,
	unit_id = leaid,

	cluster_vars_interval = nschools_elig epp_d_imp,
	cluster_vars_binary =,
	cluster_vars_nominal = region_d,

	k_clusters_list = 2:4,
	centroid_measure = MEDIAN,
	distance_measure = GOWER,
	scaling = GOWER,
	cluster_var_weights_data = weights_district,

	seed = 43290,

	output_data = clust_district,
	output_var_cluster = _cluster_d,
	output_var_distance = _distance_d,
	output_centroids = cent_district,
	output_summary = summary_district
);
** Specifying scaling=GOWER is unnecessary if you already specified 
distance_measure=GOWER, but we explicitly included it here for comparison with 
the second cluster_k call below.;
** Also, you could have left each of the following three parameters blank 
and have gotten the same results: centroid_measure, distance_measure, and 
scaling. If you had left those blank (or not included them in the macro call 
at all), the macro would have determined default values for those parameters 
based on the type of data you are clustering. And since the macro call above 
specifies mixed data (interval and nominal variables), the macro would have 
chosen, as its defaults, centroid_measure=MEDIAN, distance_measure=GOWER, and 
scaling=GOWER. (See the documentation for the distance_measure parameter for 
more information about how the macro chooses defaults for centroid_measure and 
distance_measure.);

* finalize the district clustering by choosing one of the k values and discarding 
the rest;
** In this example, we will choose the results from k=4. We will rename the k4 
versions of the cluster and distance variables and discard the versions from 
the other values of k.;
data post_district;
	set clust_district(rename=(_cluster_d_k4=cluster_district _distance_d_k4=distance_district));
	drop _cluster_d: _distance_d:;
run;


* CLUSTER SCHOOLS
-----------------------;
* If you do not want to apply variable weights, there is no need to 
create a weights dataset.;

* run clustering macro for schools;
%cluster_k(
	input_data = post_district,
	unit_id = ncessch,

	cluster_vars_interval = enr_tot_imp sme_pct_frp_tc_imp,
	cluster_vars_binary =,
	cluster_vars_nominal =,

	k_clusters_list = 2:4,
	centroid_measure = MEAN,
	distance_measure = EUCLIDEAN,
	scaling = STD,
	cluster_var_weights_data =,

	seed = 54908,

	output_data = clust_school,
	output_var_cluster = _cluster_s,
	output_var_distance = _distance_s,
	output_centroids = cent_school,
	output_summary = summary_school,

	maxiter = 50
);
* In this example, we increased the maximum number of iterations (maxiter) 
to 50. In practice, you may want to tweak the maxiter and/or 
converge parameters if the clustering procedure reaches the iteration 
limit before convergence.;

* The output_summary dataset has valuable information you can use to 
assess the clustering and the different values of k.;
** The pseudo-F statistic, which we view below, is only available when 
the centroid_measure=MEAN.;
proc print data=summary_school noobs;
	where _type_="PSEUDO_F";
	var k over_all;
run;

* finalize the school clustering by choosing one of the k values and discarding 
the rest;
** In this example, we will choose the results from k=2. We will rename the k2 
versions of the cluster and distance variables and discard the versions from 
the other values of k.;
data post_school;
	set clust_school(rename=(_cluster_s_k2=cluster_school _distance_s_k2=distance_school));
	drop _cluster_s: _distance_s:;
run;
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

