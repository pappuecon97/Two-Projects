/******************************************************************************************************************
*Title: Measurement incentive experiment
*Created by: Johirul Islam
*Created on: STATA17
*Last Modified on: 09/08/24
*Last Modified by: - MJI
*Purpose : All the regression and descriptive analysis  
*Edits 
	- [MM/DD/YY] - [editor's initials]: Added / Changed....
	
*****************************************************************************************************************/


	set more off
	clear all
	clear matrix


*Directories
	**Appropriate directory based on user
	
	if "`c(username)'"== "Lenovo" {
		global base_dir "I:\My Drive\BIGD\Misperception measurement" //johir's dir 
	}
	if "`c(username)'"== "h" {
		global base_dir "G:\My Drive\BIGD\Misperception measurement" //18th floor pc
	}	
	
	
	global data_dir				${base_dir}/02_data 
	global raw_data_dir 		${data_dir}/01_raw
	global clean_data_dir		${data_dir}/02_clean
	global analysis_dir   		${base_dir}/03_analysis
	global analysis_do_dir  	${analysis_dir}/01_do file 
	global analysis_log_dir 	${analysis_dir}/02_log
	global analysis_output_dir 	${analysis_dir}/03_output 
	global paper 				${base_dir}/04_paper\02_Manuscript
	global table 				${paper}/01_tables
	global figure 				${paper}/02_figure
	global table_data			${data_dir}/01_raw/table_data
	
	
	*load data 
	use "$clean_data_dir/SELP_Norm_Survey_Clean.dta", clear 
	
	
	
	**#Variable clening for analysis 
	
	
	*incentive experiment variable
	gen exp_inc = 1 if consent_belief_g1==1
	replace exp_inc=0 if consent_belief_g2==1
	
	la define incentive 1 "Incentive" 0 "No Incentive"
	la value exp_inc incentive
	
	la var incentive_group "Incentive"
	
	
	*control vars 
	
	*father's age: winsorize by 1 and 99%
	fre e_2
	
	*father's edu 
	fre e_3
	recode e_3 (0/15 50 51 555 = 1 "Formal education") (52/53=0 "No formal education"), gen(father_edu)
	fre father_edu

	
	*poverty index 
	fre ppi_likelihood
	
	
	*religion 
	fre religion
	recode religion (1=1 "Muslim") (2/3=0 "Non-Muslim"), gen(relg)
	
	*number of daughters 
	fre s1_e
	
	*father edu aspiration for the daughters
	fre parents_want_edu
	
	*hh size 
	fre hh_size
	
	*district 
	fre district_1
	
	*adolescents age
	fre adolescent_age
	
	*adolescent academic performance
	fre last_exm_cgpa
	
	*have at least one daughhter married
	gen marr_tot = 0 
	
	forvalues i = 1/8{
		replace marr_tot = marr_tot + 1 if s2_m_status_`i'==1
	}
	
	recode marr_tot (0=0 "No") (1/6=1 "Yes"), gen(daughhter_marr)
	la var daughhter_marr "Have at least one daughter married [Yes=1]"
	
	*any child marr in hh in the last 2 years
	fre marr_early
	la var marr_early "Any child marriage in HH in last 2 years"
	
	
	*label some vars
	la var father_edu "Respondent's highest class passed [1=Formal education]"
	la var relg "Religion [1=Muslim]"
	la var parents_want_edu "Up to what class do you want your daughter to study?[1=Graduate+]"
	la var e_2 "Respondent's age (in years)"
	
	tab district_1, gen(dist_)
	la var dist_1 "Barishal"
	la var dist_2 "Kishoreganj"
	la var dist_3 "Kurigram"
	la var dist_4 "Mymensingh"
	la var dist_5 "Rajshahi"
	
	*father age cat 
	winsor2 e_2, cut(1 99)
	
	sum e_2_w, det
	local med = r(p50)
	
	gen fage_cat = cond(e_2_w<`med', 0,1)
	la define fage_cat_la 1 "Older" 0 "Younger"
	la value fage_cat fage_cat_la
	
	la var fage_cat "Respondent's age category"
	
	*poverty cat 
	fre poverty_cat
	la define pov_cat 1 "Poor" 0 "Non-Poor", modify
	
	*father education
	fre father_edu
	
	
	
	*Incentive age interaction 
	gen incentive_age = incentive_group*fage_cat
	la var incentive_age "Additional Impact on Older"
	
	
	
	
	
	**# Balance table 
	
	des adolescent_age e_2_w father_edu ppi_likelihood relg s1_e parents_want_edu daughhter_marr marr_early hh_size dist_1 dist_2 dist_3 dist_4 dist_5 
	

	texdoc init "$paper/balance_table.tex", replace force
	
	

	global vars "adolescent_age e_2_w father_edu ppi_likelihood relg s1_e parents_want_edu daughhter_marr marr_early hh_size"

	//test for joint orthogonality
	reghdfe exp_inc $vars, absorb(district_1 enu_code) vce(robust)
	
	local Fstat = e(F)
	local df_m = e(df_m)
	local df_r = e(df_r)

	global p_val_ortho = Ftail(`df_m', `df_r', `Fstat')
	scalar pval = Ftail(`df_m', `df_r', `Fstat')
	local p_val_ortho : display %9.4f pval
	
	
	local N_no_incentive ""  // Store No Incentive observations
	local N_incentive ""      // Store Incentive observations
	local N_total ""          // Store total observations

	texdoc write \begin{table}[h]
	texdoc write \centering
	texdoc write \caption{Balance table}
	texdoc write \label{tab:balance}
	texdoc write \resizebox{\textwidth}{!}{\begin{tabular}{lccc}
	texdoc write \hline
	texdoc write \hline
	texdoc write Variable & No Incentive & Incentive & Difference \\
	texdoc write \hline

	foreach var in $vars {
		local var_label : variable label `var'
		reghdfe `var' exp_inc, absorb(district_1 enu_code) vce(robust)
		
		// Store means for groups
		local mean_no_incentive : di %6.3fc _b[_cons]
		local mean_incentive : di %6.3fc (_b[_cons] + _b[exp_inc])
		
		// Store standard errors
		local se_no_incentive = trim(string(_se[_cons], "%6.3f"))
		local se_incentive = trim(string(_se[_cons] + _se[exp_inc], "%6.3f"))
		
		// Compute Difference
		local diff : di %6.3fc _b[exp_inc]
		local se_diff = trim(string(_se[exp_inc], "%6.3f"))

		// Significance stars
		qui test exp_inc = 0
		local star = cond(r(p)< 0.01 , "***" , cond(r(p)< 0.05 , "**", cond(r(p)< 0.10 , "*", "")))

		// Store group-specific observations once
		if "`N_no_incentive'" == "" {
			qui count if exp_inc == 0
			local N_no_incentive : di %6.0fc r(N)
		}
		if "`N_incentive'" == "" {
			qui count if exp_inc == 1
			local N_incentive : di %6.0fc r(N)
		}
		if "`N_total'" == "" {
			local N_total : di %6.0fc e(N)
		}

		// Append results to table
		texdoc write `var_label' & `mean_no_incentive' & `mean_incentive' & `diff'`star' \\
		texdoc write  & (`se_no_incentive') & (`se_incentive') & (`se_diff') \\
	}

	texdoc write \hline
	texdoc write P-value from joint orthogonality test & \multicolumn{3}{c}{`p_val_ortho'} \\
	texdoc write Observations & `N_no_incentive' & `N_incentive' & `N_total' \\
	texdoc write \hline
	texdoc write \hline
	texdoc write \end{tabular}}
	texdoc write \begin{flushleft}
	texdoc write \scriptsize{Note: Robust standard deviations in parentheses. *** p $<$ 0.01, ** p $<$ 0.05, * p $<$ 0.1. District and Enumerator fixed effects used.}
	texdoc write \end{flushleft}
	texdoc write \end{table}

	texdoc close

	

	
	**# Robustness: Different measure of dependent variable 
	

	// Define independent variables
	global ind_coeff "adolescent_age e_2 father_edu ppi_likelihood relg s1_e parents_want_edu daughhter_marr marr_early hh_size"

	// Define dependent variables
	global dep "f2 f4 f6 f8 f10 f12"

	preserve

	* Run regression and store estimates for each dependent variable
	 // Create a local macro to store model names
	local i = 1
	foreach y in $dep {
		reghdfe `y' incentive_group $ind_coeff, absorb(district_1 enu_code) vce(robust)
		est store reg`i'  // Store estimates under the dependent variable name
		
		local i = `i' + 1
	}


	gen depvar = ""   // Variable to store dependent variable names
	gen coef = .      // Coefficient estimates
	gen pval = .      // P-values
	gen coef_label = "" // Coefficient labels with significance stars

	local i = 1
	foreach y in $dep {
		* Load stored estimates
		est restore reg`i'

		* Extract coefficient and variance-covariance matrix
		matrix b = e(b)
		matrix V = e(V)

		* Store variable name
		replace depvar = "`y'" in `i'

		* Extract coefficient and standard error
		local coef = _b[incentive_group]
		local se = sqrt(V[1,1])  // First row, first column for incentive_group

		* Compute t-stat and p-value
		local tstat = abs(`coef' / `se')
		local df = e(df_r)  // Degrees of freedom
		local p = 2 * ttail(`df', `tstat')  // Two-tailed p-value

		replace coef = `coef' in `i'
		replace pval = `p' in `i'

		* Assign significance stars
		if `p' < 0.01 {
			local star = "***"
		}
		else if `p' < 0.05 {
			local star = "**"
		}
		else if `p' < 0.1 {
			local star = "*"
		}
		else {
			local star = ""
		}

		* Store coefficient with significance stars
		replace coef_label = string(`coef', "%9.3f") + " " + "`star'" in `i'

		local ++i
	}


	keep depvar coef pval coef_label
	drop if coef==.

	* Generate coefficient plot with coefficient values and significance stars
	coefplot (reg1, label("Think of 100 fathers: How many accept financial support from the daughter?") msymbol(O)) ///
			(reg2, label("Think of 100 fathers: How many find financial support from daughter acceptable?") msymbol(O)) ///
			(reg3, label("Think of 100 fathers: How many say higher dowry requires for girls above 18") msymbol(P)) ///
			(reg4, label("Think of 100 fathers: How many think higher dowry require for girl with more education") msymbol(M)) ///
			(reg5, label("Think of 100 fathers: How many think early school girl have better marr prospect") msymbol(Q)) ///
			(reg6, label("Think of 100 fathers: How many think girls below 18 have better marr prospect?") msymbol(W)), ///
			vertical keep(incentive_group) ///
			coeflabels(incentive_group = "Incentive") ///
			yline(0, lcolor(black)) ///
			ytitle("Coefficient Estimate") ///
			title("", ///
				  justification(center) margin(b+5) size(small) color(black)) ///
			xsize(5) ysize(4) ///
			msymbol(O) ///
			ciopts(recast(rcap) lcolor(black)) ///
			xlabel(, angle(0)) ///
			yscale(range(-4 4)) /// Adjusts the y-axis range
			ylabel(-4(2)4)  ///
				legend( ///
			   pos(6) row(6) size(vsmall)) ///
			scheme(white_tableau) ///
			yline(-4, lcolor(black) lpattern(solid)) ///
			name(coeff_incentive, replace) ///
			mlab(coef_label) mlabposition(3) /// Add coefficient values and significance stars
			graphregion(margin(r=15 l=15 t=10 b=10))

	* Export the graph
	graph export "$paper/02_figure/robust_coeff_main.pdf", replace

	restore

	
	
	
	
	/////////////////////////////////////////////
	**# cleaning vars for dependent variables
	
	// Renaming the guess variables for putting them in the loop
	
	ren f2 f1_1
	ren f4 f3_1
	ren f6 f5_1
	ren f8 f7_1
	ren f10 f9_1
	ren f12 f11_1




	//differences from the mean


	foreach i in 1 3 5 7 9 11 {
	bys cluster_id: egen f`i'_mean= mean(f`i')
	replace f`i'_mean= f`i'_mean*100
	g dif_f`i'= (f`i'_1-f`i'_mean)
	}


	sum dif_f*


	foreach i in 1 3 5 7 9 11 {
	ttest dif_f`i'=0
	}

		
		
		********************des

	g res_dec= (f27==1)
	g res_dec_j= (f27==3|f27==4)

	for var res_dec res_dec_j: replace X=. if f27==.

	g res_dec_f= (f28==1)
	g res_dec_j_f= (f28==3|f28==4)

	for var res_dec res_dec_* : recode X (1=100)

	replace s1_c=0 if s1_c>14


	egen mean_exp= rowmean(f23_1 f23_2 f23_3)

	label var res_dec "Respondent decision-maker(past)"
	label var res_dec_j "Respondent jointly decision-maker(past)"
	label var res_dec_f "Respondent decision-maker(future)"
	label var res_dec_j_f "Respondent jointly decision-maker(future)"
	label var mean_exp "Annual expenditure (edu)"
	label var s1_b "Age"
	label var s1_c "Year (schooling)"

	g saving= (f25==1)
	recode saving (1=100)
	label var saving "Opened savings account for daughters"




	label var f1_mean "Acceptability of financial support" 
	label var f3_mean "Socail acceptability of financial support" 
	label var f5_mean "Age and dowry" 
	label var f7_mean "Edu and dowry" 
	label var f9_mean "Edu and marriage proposal quality" 
	label var f11_mean "Age and marriage proposal quality" 

	
	
	**# Robustness: coeff plot without enum fixed effect 
	
	preserve

	global dep_main "dif_f1 dif_f3 dif_f5 dif_f7 dif_f9 dif_f11"
	
	
	* Run regression and store estimates for each dependent variable
	 // Create a local macro to store model names
	local i = 1
	foreach y in $dep_main {
		reghdfe `y' incentive_group $ind_coeff, absorb(district_1) vce(robust)
		est store reg`i'  // Store estimates under the dependent variable name
		
		local i = `i' + 1
	}


	gen depvar = ""   // Variable to store dependent variable names
	gen coef = .      // Coefficient estimates
	gen pval = .      // P-values
	gen coef_label = "" // Coefficient labels with significance stars

	local i = 1
	foreach y in $dep_main {
		* Load stored estimates
		est restore reg`i'

		* Extract coefficient and variance-covariance matrix
		matrix b = e(b)
		matrix V = e(V)

		* Store variable name
		replace depvar = "`y'" in `i'

		* Extract coefficient and standard error
		local coef = _b[incentive_group]
		local se = sqrt(V[1,1])  // First row, first column for incentive_group

		* Compute t-stat and p-value
		local tstat = abs(`coef' / `se')
		local df = e(df_r)  // Degrees of freedom
		local p = 2 * ttail(`df', `tstat')  // Two-tailed p-value

		replace coef = `coef' in `i'
		replace pval = `p' in `i'

		* Assign significance stars
		if `p' < 0.01 {
			local star = "***"
		}
		else if `p' < 0.05 {
			local star = "**"
		}
		else if `p' < 0.1 {
			local star = "*"
		}
		else {
			local star = ""
		}

		* Store coefficient with significance stars
		replace coef_label = string(`coef', "%9.3f") + " " + "`star'" in `i'

		local ++i
	}


	keep depvar coef pval coef_label
	drop if coef==.

	* Generate coefficient plot with coefficient values and significance stars
	coefplot (reg1, label("Misperception about accepting financial support from one's daughter when she is married") msymbol(O)) ///
			(reg2, label("Misperception about taking financial support from a married daughter") msymbol(O)) ///
			(reg3, label("Misperception about higher dowry being required if the girl is aged more than 18") msymbol(P)) ///
			(reg4, label("Misperception about needing to pay higher dowry if the girl completes more years of education") msymbol(M)) ///
			(reg5, label("Misperception about girls who leave school early having better prospects of getting married") msymbol(Q)) ///
			(reg6, label("Misperception about girls below 18 having better prospects of getting married") msymbol(W)), ///
			vertical keep(incentive_group) ///
			coeflabels(incentive_group = "Incentive") ///
			yline(0, lcolor(black)) ///
			ytitle("Coefficient Estimate") ///
			title("", ///
				  justification(center) margin(b+5) size(small) color(black)) ///
			xsize(5) ysize(4) ///
			msymbol(O) ///
			ciopts(recast(rcap) lcolor(black)) ///
			xlabel(, angle(0)) ///
			yscale(range(-6 6)) /// Adjusts the y-axis range
			ylabel(-6(3)6)  ///
				legend( ///
			   pos(6) row(6) size(vsmall)) ///
			scheme(white_tableau) ///
			yline(-6, lcolor(black) lpattern(solid)) ///
			name(coeff_incentive_enum, replace) ///
			mlab(coef_label) mlabposition(3) /// Add coefficient values and significance stars
			graphregion(margin(r=18 l=18 t=8 b=8))

	* Export the graph
	graph export "$paper/02_figure/robust_coeff_without_enum.pdf", replace

	restore	
		
		
		
		
	**# Robustness: coeff plot without enum and district fixed effect 
	
	preserve

	global dep_main "dif_f1 dif_f3 dif_f5 dif_f7 dif_f9 dif_f11"
	
	
	* Run regression and store estimates for each dependent variable
	 // Create a local macro to store model names
	local i = 1
	foreach y in $dep_main {
		reghdfe `y' incentive_group $ind_coeff, vce(robust)
		est store reg`i'  // Store estimates under the dependent variable name
		
		local i = `i' + 1
	}


	gen depvar = ""   // Variable to store dependent variable names
	gen coef = .      // Coefficient estimates
	gen pval = .      // P-values
	gen coef_label = "" // Coefficient labels with significance stars

	local i = 1
	foreach y in $dep_main {
		* Load stored estimates
		est restore reg`i'

		* Extract coefficient and variance-covariance matrix
		matrix b = e(b)
		matrix V = e(V)

		* Store variable name
		replace depvar = "`y'" in `i'

		* Extract coefficient and standard error
		local coef = _b[incentive_group]
		local se = sqrt(V[1,1])  // First row, first column for incentive_group

		* Compute t-stat and p-value
		local tstat = abs(`coef' / `se')
		local df = e(df_r)  // Degrees of freedom
		local p = 2 * ttail(`df', `tstat')  // Two-tailed p-value

		replace coef = `coef' in `i'
		replace pval = `p' in `i'

		* Assign significance stars
		if `p' < 0.01 {
			local star = "***"
		}
		else if `p' < 0.05 {
			local star = "**"
		}
		else if `p' < 0.1 {
			local star = "*"
		}
		else {
			local star = ""
		}

		* Store coefficient with significance stars
		replace coef_label = string(`coef', "%9.3f") + " " + "`star'" in `i'

		local ++i
	}


	keep depvar coef pval coef_label
	drop if coef==.

	* Generate coefficient plot with coefficient values and significance stars
	coefplot (reg1, label("Misperception about accepting financial support from one's daughter when she is married") msymbol(O)) ///
			(reg2, label("Misperception about taking financial support from a married daughter") msymbol(O)) ///
			(reg3, label("Misperception about higher dowry being required if the girl is aged more than 18") msymbol(P)) ///
			(reg4, label("Misperception about needing to pay higher dowry if the girl completes more years of education") msymbol(M)) ///
			(reg5, label("Misperception about girls who leave school early having better prospects of getting married") msymbol(Q)) ///
			(reg6, label("Misperception about girls below 18 having better prospects of getting married") msymbol(W)), ///
			vertical keep(incentive_group) ///
			coeflabels(incentive_group = "Incentive") ///
			yline(0, lcolor(black)) ///
			ytitle("Coefficient Estimate") ///
			title("", ///
				  justification(center) margin(b+5) size(small) color(black)) ///
			xsize(5) ysize(4) ///
			msymbol(O) ///
			ciopts(recast(rcap) lcolor(black)) ///
			xlabel(, angle(0)) ///
			yscale(range(-6 6)) /// Adjusts the y-axis range
			ylabel(-6(3)6)  ///
				legend( ///
			   pos(6) row(6) size(vsmall)) ///
			scheme(white_tableau) ///
			yline(-6, lcolor(black) lpattern(solid)) ///
			name(coeff_incentive_dist, replace) ///
			mlab(coef_label) mlabposition(3) /// Add coefficient values and significance stars
			graphregion(margin(r=18 l=18 t=10 b=10))

	* Export the graph
	graph export "$paper/02_figure/robust_coeff_without_enum_dist.pdf", replace

	restore			
		
		
		
		
		
		
		
		
		
		
		
	
	


	**# Regresison tables using mean dif var

	
	**# Overall regression: misperception

	local dep "dif_f1 dif_f3 dif_f5 dif_f7 dif_f9 dif_f11"
	
	local ind "adolescent_age e_2 father_edu ppi_likelihood relg s1_e parents_want_edu daughhter_marr marr_early hh_size"
	
	//Start clean and run models
	cap which esttab
	cap which eststo
	eststo clear

	local i = 1
	foreach x of local dep {
		capture noisily reghdfe `x' incentive_group `ind', absorb(district_1 enu_code) vce(robust)
		if (_rc==0) {
			eststo m`i'
			local ++i
		}
		else {
			di as error "Model for `x' failed (rc=_rc). Not stored."
		}
	}

	
	esttab using "$paper/new_overall.tex", replace label noconstant ///
		prehead("\begin{table}[htbp] \centering  \def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}  \caption{Regression on misperception belief} \resizebox{\textwidth}{!}{\begin{tabular}{l*{6}{c}} \hline\hline & \multicolumn{6}{c}{Dependent Variables} \\ \cmidrule{2-7}") ///
		nobaselevels ///
		drop(*`ind' _cons*) ///
		b(%8.3f) se(%8.3f) ///
		star(* 0.10 ** 0.05 *** 0.01) ///
		scalars("N Number of Observations") ///
		nonumbers mtitles("Accept FS" "Find FS Acceptable" "HDRFGA18" "HDRFGWME" "ESGHBMP" "GB18HBMP")  ///
		prefoot("\hline") ///
		postfoot("\hline\hline \end{tabular}} \begin{flushleft}{\scriptsize Note: Accept FS means misperception about acceptability of financial support from daughhters; Find FS Acceptable means misperception about finding financial support from daughhters acceptable; HDRFGA18 means misperception about higher dowry requires for girls above 18; HDRFGWME means misperception about higher dowry requires for girls with more education; ESGHBMP means misperception about early school girls have better marital prospects; GB18HBMP means misperception about girls below 18 have better marital prospects. Robust standard errors in parentheses. All the regressions include adolescent's age, respondent's age, education, poverty likelihood, religion, number of daughters, educational aspiration for the daughter, having at least one daughter married, having child marriage incident in the HH in last 2 years, and household size as the control variables. *** p $<$ 0.01, ** p $<$ 0.05, * p $<$ 0.1. All regressions are estimated using district and enumerators fixed effect.} \end{flushleft} \end{table}")

	eststo clear




	
	
	fre fage_cat poverty_cat father_edu
	
	
	**# Heterogeneity by poverty category
	
	gen incentive_poverty = incentive_group*poverty_cat
	
	texdoc init "$paper/new_hetero_poverty", replace force


	// Define dependent variables
	global depvars "dif_f1 dif_f3 dif_f5 dif_f7 dif_f9 dif_f11"

	// Initialize storage for LaTeX table
	global tex_younger ""
	global tex_se_younger ""
	global tex_add_older ""
	global tex_se_add_older ""
	global tex_total_older ""
	global tex_se_total_older ""
	global tex_obs ""
	global tex_r2 ""

	// Loop over each dependent variable
	foreach var in $depvars {
		reghdfe `var' incentive_group incentive_poverty poverty_cat $ind, absorb(district_1 enu_code) vce(robust)

		// Store Treatment Effect on Younger
		global younger_`var' : di %6.3fc _b[incentive_group]
		global se_younger_`var' = trim(string(_se[incentive_group], "%6.3f"))   // Trim + Format to 3 decimal places
		
		// Store Additional Impact on Older
		global add_older_`var' : di %6.3fc _b[incentive_poverty]
		global se_add_older_`var' = trim(string(_se[incentive_poverty], "%6.3f"))

		// Compute Total Impact on Older
		qui lincom incentive_group + incentive_poverty
		global total_older_`var' : di %6.3fc r(estimate)
		global se_total_older_`var' = trim(string(r(se), "%6.3f"))

		// Significance stars
		qui test incentive_group = 0
		global star_younger_`var' = cond(r(p)< 0.01 , "***" , cond(r(p)< 0.05 , "**", cond(r(p)< 0.10 , "*", "")))

		qui test incentive_poverty = 0
		global star_add_older_`var' = cond(r(p)< 0.01 , "***" , cond(r(p)< 0.05 , "**", cond(r(p)< 0.10 , "*", "")))

		qui test incentive_group + incentive_poverty = 0
		global star_total_older_`var' = cond(r(p)< 0.01 , "***" , cond(r(p)< 0.05 , "**", cond(r(p)< 0.10 , "*", "")))

		// Store sample size, R-squared, and Fstat
		global N_`var' : di %6.0fc e(N)
		global r2_`var' : di %6.3fc e(r2)
		global Fstat_`var' : di %6.3fc e(F)

		// Append to LaTeX storage
		global tex_younger "$tex_younger & ${younger_`var'}${star_younger_`var'}"
		global tex_se_younger "$tex_se_younger & (${se_younger_`var'})"
		
		global tex_add_older "$tex_add_older & ${add_older_`var'}${star_add_older_`var'}"
		global tex_se_add_older "$tex_se_add_older & (${se_add_older_`var'})"

		global tex_total_older "$tex_total_older & ${total_older_`var'}${star_total_older_`var'}"
		global tex_se_total_older "$tex_se_total_older & (${se_total_older_`var'})"

		global tex_obs "$tex_obs & ${N_`var'}"
		global tex_r2 "$tex_r2 & ${r2_`var'}"
		global tex_fstat "$tex_fstat & ${Fstat_`var'}"
	}


	// Write the LaTeX table
	texdoc write \begin{table}[htbp]
	texdoc write \centering
	texdoc write \caption{Regression on misperception belief: Heterogeneity by Povery}
	texdoc write \label{tab:misperception_poverty}
	texdoc write \resizebox{\textwidth}{!}{\begin{tabular}{lcccccc}
	texdoc write \hline
	texdoc write \hline
	texdoc write  & \multicolumn{6}{c}{Dependent Variables} \\
	texdoc write \cmidrule{2-7} 
	texdoc write  & Accept FS & Find FS Acceptable & HDFRGA18 & HDFRGWME & ESGHBMP & GB1BHBMP \\
	texdoc write \hline

	// Insert stored values
	texdoc write Impact on Poor $tex_total_older \\
	texdoc write $tex_se_total_older \\
	texdoc write Impact on Non-Poor $tex_younger \\
	texdoc write $tex_se_younger \\
	texdoc write Additional Impact on Poor $tex_add_older \\
	texdoc write $tex_se_add_older \\
	texdoc write \hline
	texdoc write Observations $tex_obs \\
	texdoc write \hline
	texdoc write \hline

	texdoc write \end{tabular}}
	texdoc write \begin{flushleft}
	texdoc write {\scriptsize Note: Accept FS means misperception about acceptability of financial support from daughhters; Find FS Acceptable means misperception about finding financial support from daughhters acceptable; HDRFGA18 means misperception about higher dowry requires for girls above 18; HDRFGWME means misperception about higher dowry requires for girls with more education; ESGHBMP means misperception about early school girls have better marital prospects; GB18HBMP means misperception about girls below 18 have better marital prospects. Poor and non-poor is categorised based on median value of the poverty likelihood index-Median and above is Poor. Robust standard errors in parentheses. All regressions include adolescent's age, respondent's age, education, religion, , poverty, number of daughters, educational aspiration for the daughter, having at least one daughter married, having child marriage incident in the household in the last 2 years, and household size as control variables. *** p $<$ 0.01, ** p $<$ 0.05, * p $<$ 0.1. All regressions use district and enumerator fixed effects.}
	texdoc write \end{flushleft}
	texdoc write \end{table}

	texdoc close
	

	



	**# Heterogeneity by age 
	
	texdoc init "$paper/new_hetero_age", replace force

	// Define independent variables
	global ind "adolescent_age father_edu ppi_likelihood relg s1_e parents_want_edu daughhter_marr marr_early hh_size"

	// Define dependent variables
	global depvars "dif_f1 dif_f3 dif_f5 dif_f7 dif_f9 dif_f11"

	// Initialize storage for LaTeX table
	global tex_younger ""
	global tex_se_younger ""
	global tex_add_older ""
	global tex_se_add_older ""
	global tex_total_older ""
	global tex_se_total_older ""
	global tex_obs ""
	global tex_r2 ""

	// Loop over each dependent variable
	foreach var in $depvars {
		reghdfe `var' incentive_group incentive_age fage_cat $ind, absorb(district_1 enu_code) vce(robust)

		// Store Treatment Effect on Younger
		global younger_`var' : di %6.3fc _b[incentive_group]
		global se_younger_`var' = trim(string(_se[incentive_group], "%6.3f"))   // Trim + Format to 3 decimal places
		
		// Store Additional Impact on Older
		global add_older_`var' : di %6.3fc _b[incentive_age]
		global se_add_older_`var' = trim(string(_se[incentive_age], "%6.3f"))

		// Compute Total Impact on Older
		qui lincom incentive_group + incentive_age
		global total_older_`var' : di %6.3fc r(estimate)
		global se_total_older_`var' = trim(string(r(se), "%6.3f"))

		// Significance stars
		qui test incentive_group = 0
		global star_younger_`var' = cond(r(p)< 0.01 , "***" , cond(r(p)< 0.05 , "**", cond(r(p)< 0.10 , "*", "")))

		qui test incentive_age = 0
		global star_add_older_`var' = cond(r(p)< 0.01 , "***" , cond(r(p)< 0.05 , "**", cond(r(p)< 0.10 , "*", "")))

		qui test incentive_group + incentive_age = 0
		global star_total_older_`var' = cond(r(p)< 0.01 , "***" , cond(r(p)< 0.05 , "**", cond(r(p)< 0.10 , "*", "")))

		// Store sample size, R-squared, and Fstat
		global N_`var' : di %6.0fc e(N)
		global r2_`var' : di %6.3fc e(r2)
		global Fstat_`var' : di %6.3fc e(F)

		// Append to LaTeX storage
		global tex_younger "$tex_younger & ${younger_`var'}${star_younger_`var'}"
		global tex_se_younger "$tex_se_younger & (${se_younger_`var'})"
		
		global tex_add_older "$tex_add_older & ${add_older_`var'}${star_add_older_`var'}"
		global tex_se_add_older "$tex_se_add_older & (${se_add_older_`var'})"

		global tex_total_older "$tex_total_older & ${total_older_`var'}${star_total_older_`var'}"
		global tex_se_total_older "$tex_se_total_older & (${se_total_older_`var'})"

		global tex_obs "$tex_obs & ${N_`var'}"
		global tex_r2 "$tex_r2 & ${r2_`var'}"
		global tex_fstat "$tex_fstat & ${Fstat_`var'}"
	}


	// Write the LaTeX table
	texdoc write \begin{table}[htbp]
	texdoc write \centering
	texdoc write \caption{Regression on misperception belief: Heterogeneity by age}
	texdoc write \label{tab:misperception_age}
	texdoc write \resizebox{\textwidth}{!}{\begin{tabular}{lcccccc}
	texdoc write \hline
	texdoc write \hline
	texdoc write  & \multicolumn{6}{c}{Dependent Variables} \\
	texdoc write \cmidrule{2-7} 
	texdoc write  & Accept FS & Find FS Acceptable & HDFRGA18 & HDFRGWME & ESGHBMP & GB1BHBMP \\
	texdoc write \hline

	// Insert stored values
	texdoc write Impact on Older $tex_total_older \\
	texdoc write $tex_se_total_older \\
	texdoc write Impact on Younger $tex_younger \\
	texdoc write $tex_se_younger \\
	texdoc write Additional Impact on Older $tex_add_older \\
	texdoc write $tex_se_add_older \\
	texdoc write \hline
	texdoc write Observations $tex_obs \\
	texdoc write \hline
	texdoc write \hline

	texdoc write \end{tabular}}
	texdoc write \begin{flushleft}
	texdoc write {\scriptsize Note: Accept FS means misperception about acceptability of financial support from daughhters; Find FS Acceptable means misperception about finding financial support from daughhters acceptable; HDRFGA18 means misperception about higher dowry requires for girls above 18; HDRFGWME means misperception about higher dowry requires for girls with more education; ESGHBMP means misperception about early school girls have better marital prospects; GB18HBMP means misperception about girls below 18 have better marital prospects. Older and younger is categorised based on median age of the sample-Median and above is Older. Robust standard errors in parentheses. All regressions include adolescent's age, respondent's age, education, poverty, religion, number of daughters, educational aspiration for the daughter, having at least one daughter married, having child marriage incident in the household in the last 2 years, and household size as control variables. *** p $<$ 0.01, ** p $<$ 0.05, * p $<$ 0.1. All regressions use district and enumerator fixed effects.}
	texdoc write \end{flushleft}
	texdoc write \end{table}

	texdoc close
	



	
	**# Heterogeneity by respondent's education
	
	gen incentive_edu = incentive_group*father_edu
	
	texdoc init "$paper/new_hetero_edu", replace force

	// Define independent variables
	global ind "adolescent_age father_edu ppi_likelihood relg s1_e parents_want_edu daughhter_marr marr_early hh_size"

	// Define dependent variables
	global depvars "dif_f1 dif_f3 dif_f5 dif_f7 dif_f9 dif_f11"

	// Initialize storage for LaTeX table
	global tex_younger ""
	global tex_se_younger ""
	global tex_add_older ""
	global tex_se_add_older ""
	global tex_total_older ""
	global tex_se_total_older ""
	global tex_obs ""
	global tex_r2 ""

	// Loop over each dependent variable
	foreach var in $depvars {
		reghdfe `var' incentive_group incentive_edu father_edu $ind, absorb(district_1 enu_code) vce(robust)

		// Store Treatment Effect on No Formal Edu
		global younger_`var' : di %6.3fc _b[incentive_group]
		global se_younger_`var' = trim(string(_se[incentive_group], "%6.3f"))   // Trim + Format to 3 decimal places
		
		// Store Additional Impact on Formal Edu 
		global add_older_`var' : di %6.3fc _b[incentive_edu]
		global se_add_older_`var' = trim(string(_se[incentive_edu], "%6.3f"))

		// Compute Total Impact on Older
		qui lincom incentive_group + incentive_edu
		global total_older_`var' : di %6.3fc r(estimate)
		global se_total_older_`var' = trim(string(r(se), "%6.3f"))

		// Significance stars
		qui test incentive_group = 0
		global star_younger_`var' = cond(r(p)< 0.01 , "***" , cond(r(p)< 0.05 , "**", cond(r(p)< 0.10 , "*", "")))

		qui test incentive_edu = 0
		global star_add_older_`var' = cond(r(p)< 0.01 , "***" , cond(r(p)< 0.05 , "**", cond(r(p)< 0.10 , "*", "")))

		qui test incentive_group + incentive_edu = 0
		global star_total_older_`var' = cond(r(p)< 0.01 , "***" , cond(r(p)< 0.05 , "**", cond(r(p)< 0.10 , "*", "")))

		// Store sample size, R-squared, and Fstat
		global N_`var' : di %6.0fc e(N)
		global r2_`var' : di %6.3fc e(r2)
		global Fstat_`var' : di %6.3fc e(F)

		// Append to LaTeX storage
		global tex_younger "$tex_younger & ${younger_`var'}${star_younger_`var'}"
		global tex_se_younger "$tex_se_younger & (${se_younger_`var'})"
		
		global tex_add_older "$tex_add_older & ${add_older_`var'}${star_add_older_`var'}"
		global tex_se_add_older "$tex_se_add_older & (${se_add_older_`var'})"

		global tex_total_older "$tex_total_older & ${total_older_`var'}${star_total_older_`var'}"
		global tex_se_total_older "$tex_se_total_older & (${se_total_older_`var'})"

		global tex_obs "$tex_obs & ${N_`var'}"
		global tex_r2 "$tex_r2 & ${r2_`var'}"
		global tex_fstat "$tex_fstat & ${Fstat_`var'}"
	}


	// Write the LaTeX table
	texdoc write \begin{table}[htbp]
	texdoc write \centering
	texdoc write \caption{Regression on misperception belief: Heterogeneity by Respondent's Education}
	texdoc write \label{tab:misperception_edu}
	texdoc write \resizebox{\textwidth}{!}{\begin{tabular}{lcccccc}
	texdoc write \hline
	texdoc write \hline
	texdoc write  & \multicolumn{6}{c}{Dependent Variables} \\
	texdoc write \cmidrule{2-7} 
	texdoc write  & Accept FS & Find FS Acceptable & HDFRGA18 & HDFRGWME & ESGHBMP & GB1BHBMP \\
	texdoc write \hline

	// Insert stored values
	texdoc write Impact on Respondent with Formal Education $tex_total_older \\
	texdoc write $tex_se_total_older \\	
	texdoc write Impact on Respondent with No Formal Education $tex_younger \\
	texdoc write $tex_se_younger \\
	texdoc write Additional Impact on Respondent with Formal Education $tex_add_older \\
	texdoc write $tex_se_add_older \\
	texdoc write \hline
	texdoc write Observations $tex_obs \\
	texdoc write \hline
	texdoc write \hline

	texdoc write \end{tabular}}
	texdoc write \begin{flushleft}
	texdoc write {\scriptsize Note: Accept FS means misperception about acceptability of financial support from daughhters; Find FS Acceptable means misperception about finding financial support from daughhters acceptable; HDRFGA18 means misperception about higher dowry requires for girls above 18; HDRFGWME means misperception about higher dowry requires for girls with more education; ESGHBMP means misperception about early school girls have better marital prospects; GB18HBMP means misperception about girls below 18 have better marital prospects. Robust standard errors in parentheses. All regressions include adolescent's age, respondent's age, education, poverty, religion, number of daughters, educational aspiration for the daughter, having at least one daughter married, having child marriage incident in the household in the last 2 years, and household size as control variables. *** p $<$ 0.01, ** p $<$ 0.05, * p $<$ 0.1. All regressions use district and enumerator fixed effects.}
	texdoc write \end{flushleft}
	texdoc write \end{table}

	texdoc close	



		
		
	**# Reverse formatted tables: dependent variables are in first row
	
	
	label variable dif_f1  "Misperception about accepting financial support from one's daughter when she is married"
	label variable dif_f3  "Misperception about taking financial support from a married daughter"
	label variable dif_f5  "Misperception about higher dowry being required if the girl is aged more than 18"
	label variable dif_f7  "Misperception about needing to pay higher dowry if the girl completes more years of schooling"
	label variable dif_f9  "Misperception about girls who leave school early having better prospects of getting married well"
	label variable dif_f11 "Misperception about girls below 18 having better prospects of getting married"


	
	
	
	
	**# Table: Overall impact 
	
	// Define variables
	local depvars "dif_f1 dif_f3 dif_f5 dif_f7 dif_f9 dif_f11"
	local controls "adolescent_age e_2 father_edu ppi_likelihood relg s1_e parents_want_edu daughhter_marr marr_early hh_size"

	// Start texdoc
	texdoc init "$paper/new_overall.tex", replace force
	texdoc write \begin{table}[htbp]
	texdoc write \centering
	texdoc write \def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}
	texdoc write \caption{Effect of Incentive on Misperception Beliefs}
	texdoc write \begin{tabular}{lc}
	texdoc write \hline\hline
	texdoc write & Incentive \\
	texdoc write Dependent Variable & (1) \\
	texdoc write \hline

	// Loop through dependent variables
	local col = 1
	foreach dep in `depvars' {
		// Get label from variable itself
		local label : variable label `dep'

		// Run regression
		reghdfe `dep' incentive_group `controls', absorb(district_1 enu_code) vce(robust)

		// Extract coefficient and SE
		local coef : di %8.3f _b[incentive_group]
		local se = string(_se[incentive_group], "%8.3f")

		// Significance stars
		test incentive_group = 0
		local p = r(p)
		local star = cond(`p'<0.01,"***",cond(`p'<0.05,"**",cond(`p'<0.10,"*","")))

		// Write to table
		texdoc write `label' & `coef'`star' \\
		texdoc write  & (`se') \\
		
		local ++col
	}

	texdoc write \hline\hline
	texdoc write \end{tabular}
	texdoc write \begin{flushleft}
	texdoc write \scriptsize Note: Each row reports coefficient and robust standard error (in parentheses) from a separate regression where the dependent variable is a belief indicator. All regressions include controls: adolescent age, respondent education, poverty likelihood, religion, number of daughters, educational aspiration, daughter married, recent child marriage, household size. District and enumerator fixed effects included. *** p $<$ 0.01, ** p $<$ 0.05, * p $<$ 0.1.
	texdoc write \end{flushleft}
	texdoc write \end{table}
	texdoc close

		
		
		
		
		
	**# Table: Heterogeneity by poverty	
		
	texdoc init "$paper/new_hetero_poverty", replace force

	// Define dependent variables
	global depvars "dif_f1 dif_f3 dif_f5 dif_f7 dif_f9 dif_f11"

	// Initialize storage for LaTeX table
	global tex_younger ""
	global tex_se_younger ""
	global tex_add_older ""
	global tex_se_add_older ""
	global tex_total_older ""
	global tex_se_total_older ""
	global tex_obs ""
	global tex_r2 ""
	global tex_fstat ""

	// Loop over each dependent variable
	foreach var in $depvars {
		reghdfe `var' incentive_group incentive_poverty poverty_cat $ind, absorb(district_1 enu_code) vce(robust)

		// Store Treatment Effect on Younger
		global younger_`var' : di %6.3fc _b[incentive_group]
		global se_younger_`var' = trim(string(_se[incentive_group], "%6.3f"))

		// Store Additional Impact on Older
		global add_older_`var' : di %6.3fc _b[incentive_poverty]
		global se_add_older_`var' = trim(string(_se[incentive_poverty], "%6.3f"))

		// Compute Total Impact on Older
		qui lincom incentive_group + incentive_poverty
		global total_older_`var' : di %6.3fc r(estimate)
		global se_total_older_`var' = trim(string(r(se), "%6.3f"))

		// Significance stars
		qui test incentive_group = 0
		global star_younger_`var' = cond(r(p)< 0.01 , "***" , cond(r(p)< 0.05 , "**", cond(r(p)< 0.10 , "*", "")))

		qui test incentive_poverty = 0
		global star_add_older_`var' = cond(r(p)< 0.01 , "***" , cond(r(p)< 0.05 , "**", cond(r(p)< 0.10 , "*", "")))

		qui test incentive_group + incentive_poverty = 0
		global star_total_older_`var' = cond(r(p)< 0.01 , "***" , cond(r(p)< 0.05 , "**", cond(r(p)< 0.10 , "*", "")))

		// Store sample size, R-squared, and Fstat
		global N_`var' : di %6.0fc e(N)
		global r2_`var' : di %6.3fc e(r2)
		global Fstat_`var' : di %6.3fc e(F)

		// Append to LaTeX storage
		global tex_younger "$tex_younger & ${younger_`var'}${star_younger_`var'}"
		global tex_se_younger "$tex_se_younger & (${se_younger_`var'})"

		global tex_add_older "$tex_add_older & ${add_older_`var'}${star_add_older_`var'}"
		global tex_se_add_older "$tex_se_add_older & (${se_add_older_`var'})"

		global tex_total_older "$tex_total_older & ${total_older_`var'}${star_total_older_`var'}"
		global tex_se_total_older "$tex_se_total_older & (${se_total_older_`var'})"

		global tex_obs "$tex_obs & ${N_`var'}"
		global tex_r2 "$tex_r2 & ${r2_`var'}"
		global tex_fstat "$tex_fstat & ${Fstat_`var'}"
	}

	// Write the LaTeX table (Transposed)
	texdoc write \begin{table}[htbp]
	texdoc write \centering
	texdoc write \caption{Regression on misperception belief: Heterogeneity by Povery}
	texdoc write \label{tab:misperception_poverty}
	texdoc write \resizebox{\textwidth}{!}{\begin{tabular}{lccc}
	texdoc write \hline
	texdoc write \hline
	texdoc write  & \multicolumn{1}{p{4.3em}}{Impact on Non-Poor} & \multicolumn{1}{p{4.4em}}{Additional Impact on Poor} & \multicolumn{1}{p{4.0em}}{Impact on Poor} \\
	texdoc write Dependent Variable & (1) & (2) & (3) \\
	texdoc write \hline

	// Fill rows with dependent variable labels and values
	foreach var in $depvars {
		local label : variable label `var'

		texdoc write `label' & ${younger_`var'}${star_younger_`var'} & ${add_older_`var'}${star_add_older_`var'} & ${total_older_`var'}${star_total_older_`var'} \\
		texdoc write  & (${se_younger_`var'}) & (${se_add_older_`var'}) & (${se_total_older_`var'}) \\
	}

	texdoc write \hline
	texdoc write Observations & 2,914 & 2,914 & 2,914 \\
	texdoc write \hline
	texdoc write \hline
	texdoc write \end{tabular}}
	texdoc write \begin{flushleft}
	texdoc write {\scriptsize Note: Poor and non-poor is categorised based on median value of the poverty likelihood index—Median and above is Poor. Robust standard errors in parentheses. All regressions include adolescent's age, respondent's age, education, religion, poverty, number of daughters, educational aspiration for the daughter, having at least one daughter married, having child marriage incident in the household in the last 2 years, and household size as control variables. *** p $<$ 0.01, ** p $<$ 0.05, * p $<$ 0.1. All regressions use district and enumerator fixed effects.}
	texdoc write \end{flushleft}
	texdoc write \end{table}

	texdoc close

			
			
			
		
		
	**# Table: Heterogeneity by age 
	
	texdoc init "$paper/new_hetero_age", replace force

	// Define independent variables
	global ind "adolescent_age father_edu ppi_likelihood relg s1_e parents_want_edu daughhter_marr marr_early hh_size"

	// Define dependent variables
	global depvars "dif_f1 dif_f3 dif_f5 dif_f7 dif_f9 dif_f11"

	// Initialize storage for LaTeX table
	global tex_younger ""
	global tex_se_younger ""
	global tex_add_older ""
	global tex_se_add_older ""
	global tex_total_older ""
	global tex_se_total_older ""
	global tex_obs ""
	global tex_r2 ""

	// Loop over each dependent variable
	foreach var in $depvars {
		reghdfe `var' incentive_group incentive_age fage_cat $ind, absorb(district_1 enu_code) vce(robust)

		// Store Treatment Effect on Younger
		global younger_`var' : di %6.3fc _b[incentive_group]
		global se_younger_`var' = trim(string(_se[incentive_group], "%6.3f"))   // Trim + Format to 3 decimal places
		
		// Store Additional Impact on Older
		global add_older_`var' : di %6.3fc _b[incentive_age]
		global se_add_older_`var' = trim(string(_se[incentive_age], "%6.3f"))

		// Compute Total Impact on Older
		qui lincom incentive_group + incentive_age
		global total_older_`var' : di %6.3fc r(estimate)
		global se_total_older_`var' = trim(string(r(se), "%6.3f"))

		// Significance stars
		qui test incentive_group = 0
		global star_younger_`var' = cond(r(p)< 0.01 , "***" , cond(r(p)< 0.05 , "**", cond(r(p)< 0.10 , "*", "")))

		qui test incentive_age = 0
		global star_add_older_`var' = cond(r(p)< 0.01 , "***" , cond(r(p)< 0.05 , "**", cond(r(p)< 0.10 , "*", "")))

		qui test incentive_group + incentive_age = 0
		global star_total_older_`var' = cond(r(p)< 0.01 , "***" , cond(r(p)< 0.05 , "**", cond(r(p)< 0.10 , "*", "")))

		// Store sample size, R-squared, and Fstat
		global N_`var' : di %6.0fc e(N)
		global r2_`var' : di %6.3fc e(r2)
		global Fstat_`var' : di %6.3fc e(F)

		// Append to LaTeX storage
		global tex_younger "$tex_younger & ${younger_`var'}${star_younger_`var'}"
		global tex_se_younger "$tex_se_younger & (${se_younger_`var'})"
		
		global tex_add_older "$tex_add_older & ${add_older_`var'}${star_add_older_`var'}"
		global tex_se_add_older "$tex_se_add_older & (${se_add_older_`var'})"

		global tex_total_older "$tex_total_older & ${total_older_`var'}${star_total_older_`var'}"
		global tex_se_total_older "$tex_se_total_older & (${se_total_older_`var'})"

		global tex_obs "$tex_obs & ${N_`var'}"
		global tex_r2 "$tex_r2 & ${r2_`var'}"
		global tex_fstat "$tex_fstat & ${Fstat_`var'}"
	}


	// Write the LaTeX table (Transposed)
	texdoc write \begin{table}[htbp]
	texdoc write \centering
	texdoc write \caption{Regression on misperception belief: Heterogeneity by age}
	texdoc write \label{tab:misperception_age}
	texdoc write \resizebox{\textwidth}{!}{\begin{tabular}{lccc}
	texdoc write \hline
	texdoc write \hline
	texdoc write  & \multicolumn{1}{p{4.3em}}{Impact on Younger} & \multicolumn{1}{p{4.4em}}{ Additional Impact on Older} & \multicolumn{1}{p{4.0em}}{Impact on Older} \\
	texdoc write Dependent Variable & (1) & (2) & (3) \\
	texdoc write \hline

	// Fill rows with dependent variable labels and values
	foreach var in $depvars {
		local label : variable label `var'

		texdoc write `label' & ${younger_`var'}${star_younger_`var'} & ${add_older_`var'}${star_add_older_`var'} & ${total_older_`var'}${star_total_older_`var'} \\
		texdoc write  & (${se_younger_`var'}) & (${se_add_older_`var'}) & (${se_total_older_`var'}) \\
	}

	texdoc write \hline
	texdoc write Observations & 2,914 & 2,914 & 2,914 \\
	texdoc write \hline
	texdoc write \hline
	texdoc write \end{tabular}}
	texdoc write \begin{flushleft}
	texdoc write {\scriptsize Note: Older and younger is categorised based on median age of the sample-Median and above is Older. Robust standard errors in parentheses. All regressions include adolescent's age, respondent's age, education, poverty, religion, number of daughters, educational aspiration for the daughter, having at least one daughter married, having child marriage incident in the household in the last 2 years, and household size as control variables. *** p $<$ 0.01, ** p $<$ 0.05, * p $<$ 0.1. All regressions use district and enumerator fixed effects.}
	texdoc write \end{flushleft}
	texdoc write \end{table}

	texdoc close
	



	
	**# Table: Heterogeneity by respondent's education

	
	texdoc init "$paper/new_hetero_edu", replace force

	// Define independent variables
	global ind "adolescent_age father_edu ppi_likelihood relg s1_e parents_want_edu daughhter_marr marr_early hh_size"

	// Define dependent variables
	global depvars "dif_f1 dif_f3 dif_f5 dif_f7 dif_f9 dif_f11"

	// Initialize storage for LaTeX table
	global tex_younger ""
	global tex_se_younger ""
	global tex_add_older ""
	global tex_se_add_older ""
	global tex_total_older ""
	global tex_se_total_older ""
	global tex_obs ""
	global tex_r2 ""

	
	
	// Loop over each dependent variable
	foreach var in $depvars {
		reghdfe `var' incentive_group incentive_edu father_edu $ind, absorb(district_1 enu_code) vce(robust)

		// Store Treatment Effect on No Formal Edu
		global younger_`var' : di %6.3fc _b[incentive_group]
		global se_younger_`var' = trim(string(_se[incentive_group], "%6.3f"))   // Trim + Format to 3 decimal places
		
		// Store Additional Impact on Formal Edu 
		global add_older_`var' : di %6.3fc _b[incentive_edu]
		global se_add_older_`var' = trim(string(_se[incentive_edu], "%6.3f"))

		// Compute Total Impact on Older
		qui lincom incentive_group + incentive_edu
		global total_older_`var' : di %6.3fc r(estimate)
		global se_total_older_`var' = trim(string(r(se), "%6.3f"))

		// Significance stars
		qui test incentive_group = 0
		global star_younger_`var' = cond(r(p)< 0.01 , "***" , cond(r(p)< 0.05 , "**", cond(r(p)< 0.10 , "*", "")))

		qui test incentive_edu = 0
		global star_add_older_`var' = cond(r(p)< 0.01 , "***" , cond(r(p)< 0.05 , "**", cond(r(p)< 0.10 , "*", "")))

		qui test incentive_group + incentive_edu = 0
		global star_total_older_`var' = cond(r(p)< 0.01 , "***" , cond(r(p)< 0.05 , "**", cond(r(p)< 0.10 , "*", "")))

		// Store sample size, R-squared, and Fstat
		global N_`var' : di %6.0fc e(N)
		global r2_`var' : di %6.3fc e(r2)
		global Fstat_`var' : di %6.3fc e(F)

		// Append to LaTeX storage
		global tex_younger "$tex_younger & ${younger_`var'}${star_younger_`var'}"
		global tex_se_younger "$tex_se_younger & (${se_younger_`var'})"
		
		global tex_add_older "$tex_add_older & ${add_older_`var'}${star_add_older_`var'}"
		global tex_se_add_older "$tex_se_add_older & (${se_add_older_`var'})"

		global tex_total_older "$tex_total_older & ${total_older_`var'}${star_total_older_`var'}"
		global tex_se_total_older "$tex_se_total_older & (${se_total_older_`var'})"

		global tex_obs "$tex_obs & ${N_`var'}"
		global tex_r2 "$tex_r2 & ${r2_`var'}"
		global tex_fstat "$tex_fstat & ${Fstat_`var'}"
	}


	// Write the LaTeX table (Transposed)
	texdoc write \begin{table}[!htbp]
	texdoc write \centering
	texdoc write \caption{Regression on misperception belief: Heterogeneity by Education}
	texdoc write \label{tab:misperception_poverty}
	texdoc write \resizebox{\textwidth}{!}{\begin{tabular}{lccc}
	texdoc write \hline
	texdoc write \hline
	texdoc write  & \multicolumn{1}{p{6.3em}}{\centering  Impact on Respondent with No Formal Education} & \multicolumn{1}{p{6.4em}}{Additional Impact on Respondent with Formal Education} & \multicolumn{1}{p{6.4em}}{Impact on Respondent with Formal Education} \\
	texdoc write Dependent Variables & (1) & (2) & (3) \\
	texdoc write \hline

	// Fill rows with dependent variable labels and values
	foreach var in $depvars {
		local label : variable label `var'

		texdoc write `label' & ${younger_`var'}${star_younger_`var'} & ${add_older_`var'}${star_add_older_`var'} & ${total_older_`var'}${star_total_older_`var'} \\
		texdoc write  & (${se_younger_`var'}) & (${se_add_older_`var'}) & (${se_total_older_`var'}) \\
	}

	texdoc write \hline
	texdoc write Observations & 2,914 & 2,914 & 2,914 \\
	texdoc write \hline
	texdoc write \hline
	texdoc write \end{tabular}}
	texdoc write \begin{flushleft}
	texdoc write {\scriptsize Note: Robust standard errors in parentheses. All regressions include adolescent's age, respondent's age, education, poverty, religion, number of daughters, educational aspiration for the daughter, having at least one daughter married, having child marriage incident in the household in the last 2 years, and household size as control variables. *** p $<$ 0.01, ** p $<$ 0.05, * p $<$ 0.1. All regressions used district and enumerator fixed effects.}
	texdoc write \end{flushleft}
	texdoc write \end{table}

	texdoc close	



	//save dataset for the tables 
	save "$table_data/selp_temp_data_for_tables.dta", replace 
	
	
	
	
	
	
	**# Tables with anderson's q-values and FWER p-values starts from here
	
	
	
	**# Table with q-values: by poverty
	
	
	set more off
	clear all 
	clear matrix

	use "$table_data/selp_temp_data_for_tables.dta", clear
	
	
	*Initialize texdoc and globals
	texdoc init "$paper/new_hetero_poverty", replace force

	* Dependent vars
	global depvars "dif_f1 dif_f3 dif_f5 dif_f7 dif_f9 dif_f11"

	* Controls (fill in your exact list)
	global ind "adolescent_age e_2 father_edu relg s1_e parents_want_edu daughhter_marr marr_early hh_size"

	*-------------------------------------------------------------------------------
	* 2.  Run regressions & collect raw results into a temporary file
	tempfile rawres
	tempname R
	postfile `R' str20 outcome ///
			double b_y se_y p_y ///
			double b_a se_a p_a ///
			double b_t se_t p_t ///
			double N r2 Fstat ///
		using "`rawres'", replace

	foreach var of global depvars {
		* run the cluster‐robust FE regression
		reghdfe `var' incentive_group incentive_poverty poverty_cat $ind, ///
			absorb(district_1 enu_code) vce(robust)

		* collect coefficients and SEs
		local b_y = _b[incentive_group]
		local se_y = _se[incentive_group]
		local b_a = _b[incentive_poverty]
		local se_a = _se[incentive_poverty]

		* p-values for individual effects
		quietly test incentive_group = 0
		local p_y = r(p)
		quietly test incentive_poverty = 0
		local p_a = r(p)

		* total effect and its p-value
		quietly lincom incentive_group + incentive_poverty
		local b_t = r(estimate)
		local se_t = r(se)
		local p_t = r(p)

		* store
		post `R' ("`var'") ///
			(`b_y') (`se_y') (`p_y') ///
			(`b_a') (`se_a') (`p_a') ///
			(`b_t') (`se_t') (`p_t') ///
			(e(N)) (e(r2)) (e(F_statistic))
	}
	postclose `R'

	use "`rawres'", clear
	save "$table_data/raw_results.dta", replace

	*-------------------------------------------------------------------------------
	* 3.  Prepare p-values for multiple testing
	use "$table_data/raw_results.dta", clear
	keep outcome p_y p_a p_t
	reshape long p_, i(outcome) j(effect) string
	rename p_ pval
	save "$table_data/Tablepvals_long.dta", replace
	
	*-------------------------------------------------------------------------------
	* 4.  Anderson's BKY (2006) sharpened q-values
	preserve
	use "$table_data/Tablepvals_long.dta", clear
	version 10
	set more off

	* count and rank
	quietly summarize pval
	local totalpvals = r(N)
	gen int original_order = _n
	sort pval
	gen int rank = _n if pval < .

	* run the BKY step-down

	gen bky06_qval = 1 if pval < .

	local qval = 1
	while `qval' > 0 {
		* First Stage
		* Generate the adjusted first stage q level we are testing: q' = q/1+q
		local qval_adj = `qval'/(1+`qval')
		* Generate value q'*r/M
		gen fdr_temp1 = `qval_adj'*rank/`totalpvals'
		* Generate binary variable checking condition p(r) <= q'*r/M
		gen reject_temp1 = (fdr_temp1>=pval) if pval~=.
		* Generate variable containing p-value ranks for all p-values that meet above condition
		gen reject_rank1 = reject_temp1*rank
		* Record the rank of the largest p-value that meets above condition
		egen total_rejected1 = max(reject_rank1)

		* Second Stage
		* Generate the second stage q level that accounts for hypotheses rejected in first stage: q_2st = q'*(M/m0)
		local qval_2st = `qval_adj'*(`totalpvals'/(`totalpvals'-total_rejected1[1]))
		* Generate value q_2st*r/M
		gen fdr_temp2 = `qval_2st'*rank/`totalpvals'
		* Generate binary variable checking condition p(r) <= q_2st*r/M
		gen reject_temp2 = (fdr_temp2>=pval) if pval~=.
		* Generate variable containing p-value ranks for all p-values that meet above condition
		gen reject_rank2 = reject_temp2*rank
		* Record the rank of the largest p-value that meets above condition
		egen total_rejected2 = max(reject_rank2)

		* A p-value has been rejected at level q if its rank is less than or equal to the rank of the max p-value that meets the above condition
		replace bky06_qval = `qval' if rank <= total_rejected2 & rank~=.
		* Reduce q by 0.001 and repeat loop
		drop fdr_temp* reject_temp* reject_rank* total_rejected*
		local qval = `qval' - .001
	}


	sort original_order
	drop original_order rank
	reshape wide bky06_qval pval, i(outcome) j(effect) string
	rename bky06_qvaly q_anderson_younger
	rename bky06_qvala     q_anderson_add
	rename bky06_qvalt   q_anderson_total
	save "$table_data/Tableqvals.dta", replace
	restore

	*-------------------------------------------------------------------------------
	* 5.  Merge q-values back into the raw results
	use "$table_data/raw_results.dta", clear
	
	merge 1:1 outcome using "$table_data/Tableqvals.dta"
	drop _merge
	save "$table_data/results_with_q.dta", replace

	*-------------------------------------------------------------------------------
	* 6.  Romano–Wolf FWER for the two treatment effects
	clear all
	clear matrix
	use "$table_data/selp_temp_data_for_tables.dta", clear

	rwolf2 ///
	  (reghdfe dif_f1  incentive_group incentive_poverty poverty_cat $ind, absorb(district_1 enu_code) vce(robust)) ///
	  (reghdfe dif_f3  incentive_group incentive_poverty poverty_cat $ind, absorb(district_1 enu_code) vce(robust)) ///
	  (reghdfe dif_f5  incentive_group incentive_poverty poverty_cat $ind, absorb(district_1 enu_code) vce(robust)) ///
	  (reghdfe dif_f7  incentive_group incentive_poverty poverty_cat $ind, absorb(district_1 enu_code) vce(robust)) ///
	  (reghdfe dif_f9  incentive_group incentive_poverty poverty_cat $ind, absorb(district_1 enu_code) vce(robust)) ///
	  (reghdfe dif_f11 incentive_group incentive_poverty poverty_cat $ind, absorb(district_1 enu_code) vce(robust)) ///
	, indepvars(  incentive_group incentive_poverty,  ///
				  incentive_group incentive_poverty,  ///
				  incentive_group incentive_poverty,  ///
				  incentive_group incentive_poverty,  ///
				  incentive_group incentive_poverty,  ///
				  incentive_group incentive_poverty ) ///
	  seed(12345) reps(3000) usevalid
	  
	matrix FWER = e(RW)


	//for total impact on poor
	fre poverty_cat
	clonevar poverty_cat1 = poverty_cat
	recode poverty_cat1 (1=0) (0=1)
	
	gen pov_incen = poverty_cat1*incentive_group
	
	
	rwolf2 ///
	  (reghdfe dif_f1  incentive_group pov_incen poverty_cat1 $ind, absorb(district_1 enu_code) vce(robust)) ///
	  (reghdfe dif_f3  incentive_group pov_incen poverty_cat1 $ind, absorb(district_1 enu_code) vce(robust)) ///
	  (reghdfe dif_f5  incentive_group pov_incen poverty_cat1 $ind, absorb(district_1 enu_code) vce(robust)) ///
	  (reghdfe dif_f7  incentive_group pov_incen poverty_cat1 $ind, absorb(district_1 enu_code) vce(robust)) ///
	  (reghdfe dif_f9  incentive_group pov_incen poverty_cat1 $ind, absorb(district_1 enu_code) vce(robust)) ///
	  (reghdfe dif_f11 incentive_group pov_incen poverty_cat1 $ind, absorb(district_1 enu_code) vce(robust)) ///
	, indepvars(  incentive_group pov_incen,  ///
				  incentive_group pov_incen,  ///
				  incentive_group pov_incen,  ///
				  incentive_group pov_incen,  ///
				  incentive_group pov_incen,  ///
				  incentive_group pov_incen ) ///
	  seed(12345) reps(3000) usevalid
	
	
	matrix tot_fwer = e(RW)
	
	
	
	
	*-------------------------------------------------------------------------------
	* 7.  Build all the LaTeX strings
	use "$table_data/results_with_q.dta", clear

	* Initialize storage for LaTeX table columns
	global tex_younger ""
	global tex_se_younger ""
	global tex_q_younger ""
	global tex_add_older ""
	global tex_se_add_older ""
	global tex_q_add_older ""
	global tex_total_older ""
	global tex_se_total_older ""
	global tex_q_total_older ""
	global tex_obs ""
	global tex_r2 ""
	global tex_fstat ""

	* List of dependent variables (these should be the same as in your original list)
	local depvars dif_f1 dif_f3 dif_f5 dif_f7 dif_f9 dif_f11

	* Manually define variable labels for the LaTeX table
	local labels ""Misperception about accepting financial support from one's daughter when she is married" "Misperception about taking financial support from a married daughter" "Misperception about higher dowry being required if the girl is aged more than 18" "Misperception about needing to pay higher dowry if the girl completes more years of education" "Misperception about girls who leave school early having better prospects of getting married" "Misperception about girls below 18 having better prospects of getting married""

	* Loop over each dependent variable (by name)
	local i = 1
	
	//set local macros for fwer
	local k = 1
	local h = 2
	
	foreach var of local depvars {

		* Collect necessary values for each dependent variable
		local b_y   = b_y[`i']
		local se_y  = se_y[`i']
		local p_y   = p_y[`i']
		local b_a   = b_a[`i']
		local se_a  = se_a[`i']
		local p_a   = p_a[`i']
		local b_t   = b_t[`i']
		local se_t  = se_t[`i']
		local p_t   = p_t[`i']
		local Nobs  = N[`i']
		local R2    = r2[`i']
		local Fst   = Fstat[`i']
		local q_y   = q_anderson_younger[`i']
		local q_a   = q_anderson_add[`i']
		local q_t   = q_anderson_total[`i']

		
		* FWER values for each outcome (just use saved variables from rwolf2)
		local fwer_y = FWER[`k', 3]
		di `fwer_y' 
		local fwer_a = FWER[`h', 3]
		di `fwer_a'
		local fwer_t = tot_fwer[`k', 3]  // Set this if you have the total effect stored

		* Significance stars for each p-value
		local star_y = cond(`p_y' < 0.01, "***", cond(`p_y' < 0.05, "**", cond(`p_y' < 0.10, "*", "")))
		local star_a = cond(`p_a' < 0.01, "***", cond(`p_a' < 0.05, "**", cond(`p_a' < 0.10, "*", "")))
		local star_t = cond(`p_t' < 0.01, "***", cond(`p_t' < 0.05, "**", cond(`p_t' < 0.10, "*", "")))

		* Append results to LaTeX strings
		global tex_younger_`var'    "$tex_younger & `=string(`b_y',"%6.3f")'`star_y'"
		global tex_se_younger_`var' "$tex_se_younger & (`=string(`se_y',"%6.3f")')"
		global tex_q_younger_`var'  "$tex_q_younger & [`=string(`q_y',"%6.3f")', `=string(`fwer_y',"%6.3f")']"

		global tex_add_older_`var'    "$tex_add_older & `=string(`b_a',"%6.3f")'`star_a'"
		global tex_se_add_older_`var' "$tex_se_add_older & (`=string(`se_a',"%6.3f")')"
		global tex_q_add_older_`var'  "$tex_q_add_older & [`=string(`q_a',"%6.3f")', `=string(`fwer_a',"%6.3f")']"

		global tex_total_older_`var'    "$tex_total_older & `=string(`b_t',"%6.3f")'`star_t'"
		global tex_se_total_older_`var' "$tex_se_total_older & (`=string(`se_t',"%6.3f")')"
		global tex_q_total_older_`var'  "$tex_q_total_older & [`=string(`q_t',"%6.3f")', `=string(`fwer_t', "%6.3f")']"

		//global tex_obs   "$tex_obs & `Nobs'"
		//global tex_r2    "$tex_r2 & `=string(`R2',"%6.3f")'"
		//global tex_fstat "$tex_fstat & `=string(`Fst',"%6.1f")'"

		local i = `i' + 1
		local k = `k' + 2
		local h = `h' + 2
	}

	*-------------------------------------------------------------------------------
	* 8.  Write out the final LaTeX table
	texdoc write \begin{table}[htbp]
	texdoc write \centering
	texdoc write \caption{Regression on misperception belief: Heterogeneity by Poverty}
	texdoc write \label{tab:misperception_poverty}
	texdoc write \resizebox{\textwidth}{!}{\begin{tabular}{lccc}
	texdoc write \hline
	texdoc write \hline
	texdoc write  & \multicolumn{1}{p{5.3em}}{\centering Impact on Non-Poor} & ///
					 \multicolumn{1}{p{7.4em}}{\centering Additional Impact on Poor} & ///
					 \multicolumn{1}{p{4.0em}}{\centering Impact on Poor} \\
	texdoc write Dependent Variable & (1) & (2) & (3) \\
	texdoc write \hline

	* Fill rows with dependent variable labels and values
	local j = 1
	local depvars dif_f1 dif_f3 dif_f5 dif_f7 dif_f9 dif_f11
	foreach var of local depvars {
		* Use the manually defined label from the `labels` local
		local lbl : word `j' of `labels'
		texdoc write `lbl' ${tex_younger_`var'} ${tex_add_older_`var'} ${tex_total_older_`var'} \\
		texdoc write   ${tex_se_younger_`var'}  ${tex_se_add_older_`var'} ${tex_se_total_older_`var'} \\
		texdoc write   ${tex_q_younger_`var'} ///
						 ${tex_q_add_older_`var'} ///
						 ${tex_q_total_older_`var'} \\
		local ++j
	}

	texdoc write \hline
	texdoc write Observations & 2,914 & 2,914 & 2,914 \\
	texdoc write \hline
	texdoc write \hline
	texdoc write \end{tabular}}
	texdoc write \begin{flushleft}
	texdoc write {\scriptsize Note: Poor and non-poor is categorised based on the median of the poverty index. Robust standard errors in parentheses. The additional row (in square brackets) shows Anderson's sharpened q-values and Romano–Wolf FWER estimates. *** p $<$ 0.01, ** p $<$ 0.05, * p $<$ 0.1. All regressions include ...}
	texdoc write \end{flushleft}
	texdoc write \end{table}

	texdoc close


	
	
	
	
	
	
	**# Table with q-values: by father's age 
	
	set more off
	clear all 
	clear matrix

	use "$table_data/selp_temp_data_for_tables.dta", clear
	
	
	*Initialize texdoc and globals
	texdoc init "$paper/new_hetero_age", replace force

	* Dependent vars
	global depvars "dif_f1 dif_f3 dif_f5 dif_f7 dif_f9 dif_f11"

	* Controls (fill in your exact list)
	global ind "adolescent_age father_edu ppi_likelihood relg s1_e parents_want_edu daughhter_marr marr_early hh_size"

	*-------------------------------------------------------------------------------
	* 2.  Run regressions & collect raw results into a temporary file
	tempfile rawres
	tempname R
	postfile `R' str20 outcome ///
			double b_y se_y p_y ///
			double b_a se_a p_a ///
			double b_t se_t p_t ///
			double N r2 Fstat ///
		using "`rawres'", replace

	foreach var of global depvars {
		* run the cluster‐robust FE regression
		reghdfe `var' incentive_group incentive_age fage_cat $ind, ///
			absorb(district_1 enu_code) vce(robust)

		* collect coefficients and SEs
		local b_y = _b[incentive_group]
		local se_y = _se[incentive_group]
		local b_a = _b[incentive_age]
		local se_a = _se[incentive_age]

		* p-values for individual effects
		quietly test incentive_group = 0
		local p_y = r(p)
		quietly test incentive_age = 0
		local p_a = r(p)

		* total effect and its p-value
		quietly lincom incentive_group + incentive_age
		local b_t = r(estimate)
		local se_t = r(se)
		local p_t = r(p)

		* store
		post `R' ("`var'") ///
			(`b_y') (`se_y') (`p_y') ///
			(`b_a') (`se_a') (`p_a') ///
			(`b_t') (`se_t') (`p_t') ///
			(e(N)) (e(r2)) (e(F_statistic))
	}
	postclose `R'

	use "`rawres'", clear
	save "$table_data/raw_results.dta", replace

	*-------------------------------------------------------------------------------
	* 3.  Prepare p-values for multiple testing
	use "$table_data/raw_results.dta", clear
	keep outcome p_y p_a p_t
	reshape long p_, i(outcome) j(effect) string
	rename p_ pval
	save "$table_data/Tablepvals_long.dta", replace
	
	*-------------------------------------------------------------------------------
	* 4.  Anderson's BKY (2006) sharpened q-values
	preserve
	use "$table_data/Tablepvals_long.dta", clear
	version 10
	set more off

	* count and rank
	quietly summarize pval
	local totalpvals = r(N)
	gen int original_order = _n
	sort pval
	gen int rank = _n if pval < .

	* run the BKY step-down

	gen bky06_qval = 1 if pval < .

	local qval = 1
	while `qval' > 0 {
		* First Stage
		* Generate the adjusted first stage q level we are testing: q' = q/1+q
		local qval_adj = `qval'/(1+`qval')
		* Generate value q'*r/M
		gen fdr_temp1 = `qval_adj'*rank/`totalpvals'
		* Generate binary variable checking condition p(r) <= q'*r/M
		gen reject_temp1 = (fdr_temp1>=pval) if pval~=.
		* Generate variable containing p-value ranks for all p-values that meet above condition
		gen reject_rank1 = reject_temp1*rank
		* Record the rank of the largest p-value that meets above condition
		egen total_rejected1 = max(reject_rank1)

		* Second Stage
		* Generate the second stage q level that accounts for hypotheses rejected in first stage: q_2st = q'*(M/m0)
		local qval_2st = `qval_adj'*(`totalpvals'/(`totalpvals'-total_rejected1[1]))
		* Generate value q_2st*r/M
		gen fdr_temp2 = `qval_2st'*rank/`totalpvals'
		* Generate binary variable checking condition p(r) <= q_2st*r/M
		gen reject_temp2 = (fdr_temp2>=pval) if pval~=.
		* Generate variable containing p-value ranks for all p-values that meet above condition
		gen reject_rank2 = reject_temp2*rank
		* Record the rank of the largest p-value that meets above condition
		egen total_rejected2 = max(reject_rank2)

		* A p-value has been rejected at level q if its rank is less than or equal to the rank of the max p-value that meets the above condition
		replace bky06_qval = `qval' if rank <= total_rejected2 & rank~=.
		* Reduce q by 0.001 and repeat loop
		drop fdr_temp* reject_temp* reject_rank* total_rejected*
		local qval = `qval' - .001
	}


	sort original_order
	drop original_order rank
	reshape wide bky06_qval pval, i(outcome) j(effect) string
	rename bky06_qvaly q_anderson_younger
	rename bky06_qvala     q_anderson_add
	rename bky06_qvalt   q_anderson_total
	save "$table_data/Tableqvals.dta", replace
	restore

	*-------------------------------------------------------------------------------
	* 5.  Merge q-values back into the raw results
	use "$table_data/raw_results.dta", clear
	
	merge 1:1 outcome using "$table_data/Tableqvals.dta"
	drop _merge
	save "$table_data/results_with_q.dta", replace

	*-------------------------------------------------------------------------------
	* 6.  Romano–Wolf FWER for the two treatment effects
	clear all
	clear matrix
	use "$table_data/selp_temp_data_for_tables.dta", clear

	rwolf2 ///
	  (reghdfe dif_f1  incentive_group incentive_age fage_cat $ind, absorb(district_1 enu_code) vce(robust)) ///
	  (reghdfe dif_f3  incentive_group incentive_age fage_cat $ind, absorb(district_1 enu_code) vce(robust)) ///
	  (reghdfe dif_f5  incentive_group incentive_age fage_cat $ind, absorb(district_1 enu_code) vce(robust)) ///
	  (reghdfe dif_f7  incentive_group incentive_age fage_cat $ind, absorb(district_1 enu_code) vce(robust)) ///
	  (reghdfe dif_f9  incentive_group incentive_age fage_cat $ind, absorb(district_1 enu_code) vce(robust)) ///
	  (reghdfe dif_f11 incentive_group incentive_age fage_cat $ind, absorb(district_1 enu_code) vce(robust)) ///
	, indepvars(  incentive_group incentive_age,  ///
				  incentive_group incentive_age,  ///
				  incentive_group incentive_age,  ///
				  incentive_group incentive_age,  ///
				  incentive_group incentive_age,  ///
				  incentive_group incentive_age ) ///
	  seed(12345) reps(3000) usevalid
	  
	matrix FWER = e(RW)


	//for total impact on older 
	fre fage_cat
	clonevar fage_cat1 = fage_cat
	recode fage_cat1 (1=0) (0=1)
	
	gen age_incen = fage_cat1*incentive_group
	
	
	rwolf2 ///
	  (reghdfe dif_f1  incentive_group age_incen fage_cat1 $ind, absorb(district_1 enu_code) vce(robust)) ///
	  (reghdfe dif_f3  incentive_group age_incen fage_cat1 $ind, absorb(district_1 enu_code) vce(robust)) ///
	  (reghdfe dif_f5  incentive_group age_incen fage_cat1 $ind, absorb(district_1 enu_code) vce(robust)) ///
	  (reghdfe dif_f7  incentive_group age_incen fage_cat1 $ind, absorb(district_1 enu_code) vce(robust)) ///
	  (reghdfe dif_f9  incentive_group age_incen fage_cat1 $ind, absorb(district_1 enu_code) vce(robust)) ///
	  (reghdfe dif_f11 incentive_group age_incen fage_cat1 $ind, absorb(district_1 enu_code) vce(robust)) ///
	, indepvars(  incentive_group age_incen,  ///
				  incentive_group age_incen,  ///
				  incentive_group age_incen,  ///
				  incentive_group age_incen,  ///
				  incentive_group age_incen,  ///
				  incentive_group age_incen ) ///
	  seed(12345) reps(3000) usevalid
	
	
	matrix tot_fwer = e(RW)
	
	
	
	
	*-------------------------------------------------------------------------------
	* 7.  Build all the LaTeX strings
	use "$table_data/results_with_q.dta", clear

	* Initialize storage for LaTeX table columns
	global tex_younger ""
	global tex_se_younger ""
	global tex_q_younger ""
	global tex_add_older ""
	global tex_se_add_older ""
	global tex_q_add_older ""
	global tex_total_older ""
	global tex_se_total_older ""
	global tex_q_total_older ""
	global tex_obs ""
	global tex_r2 ""
	global tex_fstat ""

	* List of dependent variables (these should be the same as in your original list)
	local depvars dif_f1 dif_f3 dif_f5 dif_f7 dif_f9 dif_f11

	* Manually define variable labels for the LaTeX table
	local labels ""Misperception about accepting financial support from one's daughter when she is married" "Misperception about taking financial support from a married daughter" "Misperception about higher dowry being required if the girl is aged more than 18" "Misperception about needing to pay higher dowry if the girl completes more years of education" "Misperception about girls who leave school early having better prospects of getting married" "Misperception about girls below 18 having better prospects of getting married""

	* Loop over each dependent variable (by name)
	local i = 1
	
	//set local macros for fwer
	local k = 1
	local h = 2
	
	foreach var of local depvars {

		* Collect necessary values for each dependent variable
		local b_y   = b_y[`i']
		local se_y  = se_y[`i']
		local p_y   = p_y[`i']
		local b_a   = b_a[`i']
		local se_a  = se_a[`i']
		local p_a   = p_a[`i']
		local b_t   = b_t[`i']
		local se_t  = se_t[`i']
		local p_t   = p_t[`i']
		local Nobs  = N[`i']
		local R2    = r2[`i']
		local Fst   = Fstat[`i']
		local q_y   = q_anderson_younger[`i']
		local q_a   = q_anderson_add[`i']
		local q_t   = q_anderson_total[`i']

		
		* FWER values for each outcome (just use saved variables from rwolf2)
		local fwer_y = FWER[`k', 3]
		di `fwer_y' 
		local fwer_a = FWER[`h', 3]
		di `fwer_a'
		local fwer_t = tot_fwer[`k', 3]  // Set this if you have the total effect stored

		* Significance stars for each p-value
		local star_y = cond(`p_y' < 0.01, "***", cond(`p_y' < 0.05, "**", cond(`p_y' < 0.10, "*", "")))
		local star_a = cond(`p_a' < 0.01, "***", cond(`p_a' < 0.05, "**", cond(`p_a' < 0.10, "*", "")))
		local star_t = cond(`p_t' < 0.01, "***", cond(`p_t' < 0.05, "**", cond(`p_t' < 0.10, "*", "")))

		* Append results to LaTeX strings
		global tex_younger_`var'    "$tex_younger & `=string(`b_y',"%6.3f")'`star_y'"
		global tex_se_younger_`var' "$tex_se_younger & (`=string(`se_y',"%6.3f")')"
		global tex_q_younger_`var'  "$tex_q_younger & [`=string(`q_y',"%6.3f")', `=string(`fwer_y',"%6.3f")']"

		global tex_add_older_`var'    "$tex_add_older & `=string(`b_a',"%6.3f")'`star_a'"
		global tex_se_add_older_`var' "$tex_se_add_older & (`=string(`se_a',"%6.3f")')"
		global tex_q_add_older_`var'  "$tex_q_add_older & [`=string(`q_a',"%6.3f")', `=string(`fwer_a',"%6.3f")']"

		global tex_total_older_`var'    "$tex_total_older & `=string(`b_t',"%6.3f")'`star_t'"
		global tex_se_total_older_`var' "$tex_se_total_older & (`=string(`se_t',"%6.3f")')"
		global tex_q_total_older_`var'  "$tex_q_total_older & [`=string(`q_t',"%6.3f")', `=string(`fwer_t', "%6.3f")']"

		//global tex_obs   "$tex_obs & `Nobs'"
		//global tex_r2    "$tex_r2 & `=string(`R2',"%6.3f")'"
		//global tex_fstat "$tex_fstat & `=string(`Fst',"%6.1f")'"

		local i = `i' + 1
		local k = `k' + 2
		local h = `h' + 2
	}

	*-------------------------------------------------------------------------------
	* 8.  Write out the final LaTeX table
	texdoc write \begin{table}[htbp]
	texdoc write \centering
	texdoc write \caption{Regression on misperception belief: Heterogeneity by respondent's age}
	texdoc write \label{tab:misperception_poverty}
	texdoc write \resizebox{\textwidth}{!}{\begin{tabular}{lccc}
	texdoc write \hline
	texdoc write \hline
	texdoc write  & \multicolumn{1}{p{5.3em}}{\centering Impact on Younger} & ///
					 \multicolumn{1}{p{7.4em}}{\centering Additional Impact on Older} & ///
					 \multicolumn{1}{p{4.0em}}{\centering Impact on Older} \\
	texdoc write Dependent Variables & (1) & (2) & (3) \\
	texdoc write \hline

	* Fill rows with dependent variable labels and values
	local j = 1
	local depvars dif_f1 dif_f3 dif_f5 dif_f7 dif_f9 dif_f11
	foreach var of local depvars {
		* Use the manually defined label from the `labels` local
		local lbl : word `j' of `labels'
		texdoc write `lbl' ${tex_younger_`var'} ${tex_add_older_`var'} ${tex_total_older_`var'} \\
		texdoc write   ${tex_se_younger_`var'}  ${tex_se_add_older_`var'} ${tex_se_total_older_`var'} \\
		texdoc write   ${tex_q_younger_`var'} ///
						 ${tex_q_add_older_`var'} ///
						 ${tex_q_total_older_`var'} \\
		local ++j
	}

	texdoc write \hline
	texdoc write Observations & 2,914 & 2,914 & 2,914 \\
	texdoc write \hline
	texdoc write \hline
	texdoc write \end{tabular}}
	texdoc write \begin{flushleft}
	texdoc write {\scriptsize Note: Older and younger is categorised based on median age of the sample-Median and above is Older. Robust standard errors in parentheses. All regressions include adolescent's age, respondent's age, education, poverty, religion, number of daughters, educational aspiration for the daughter, having at least one daughter married, having child marriage incident in the household in the last 2 years, and household size as control variables. *** p $<$ 0.01, ** p $<$ 0.05, * p $<$ 0.1. All regressions use district and enumerator fixed effects. Robust standard errors in parentheses. The additional row (in square brackets) shows Anderson's sharpened q-values and Romano–Wolf FWER estimates.}
	texdoc write \end{flushleft}
	texdoc write \end{table}

	texdoc close

	
	
	
	
	
	
	
	
	
	**# Table with q-values: by respondent's education 
	
	set more off
	clear all 
	clear matrix

	use "$table_data/selp_temp_data_for_tables.dta", clear
	
	
	*Initialize texdoc and globals
	texdoc init "$paper/new_hetero_edu", replace force

	* Dependent vars
	global depvars "dif_f1 dif_f3 dif_f5 dif_f7 dif_f9 dif_f11"

	* Controls (fill in your exact list)
	global ind "adolescent_age e_2 ppi_likelihood relg s1_e parents_want_edu daughhter_marr marr_early hh_size"

	*-------------------------------------------------------------------------------
	* 2.  Run regressions & collect raw results into a temporary file
	tempfile rawres
	tempname R
	postfile `R' str20 outcome ///
			double b_y se_y p_y ///
			double b_a se_a p_a ///
			double b_t se_t p_t ///
			double N r2 Fstat ///
		using "`rawres'", replace

	foreach var of global depvars {
		* run the cluster‐robust FE regression
		reghdfe `var' incentive_group incentive_edu father_edu $ind, ///
			absorb(district_1 enu_code) vce(robust)

		* collect coefficients and SEs
		local b_y = _b[incentive_group]
		local se_y = _se[incentive_group]
		local b_a = _b[incentive_edu]
		local se_a = _se[incentive_edu]

		* p-values for individual effects
		quietly test incentive_group = 0
		local p_y = r(p)
		quietly test incentive_edu = 0
		local p_a = r(p)

		* total effect and its p-value
		quietly lincom incentive_group + incentive_edu
		local b_t = r(estimate)
		local se_t = r(se)
		local p_t = r(p)

		* store
		post `R' ("`var'") ///
			(`b_y') (`se_y') (`p_y') ///
			(`b_a') (`se_a') (`p_a') ///
			(`b_t') (`se_t') (`p_t') ///
			(e(N)) (e(r2)) (e(F_statistic))
	}
	postclose `R'

	use "`rawres'", clear
	save "$table_data/raw_results.dta", replace

	*-------------------------------------------------------------------------------
	* 3.  Prepare p-values for multiple testing
	use "$table_data/raw_results.dta", clear
	keep outcome p_y p_a p_t
	reshape long p_, i(outcome) j(effect) string
	rename p_ pval
	save "$table_data/Tablepvals_long.dta", replace
	
	*-------------------------------------------------------------------------------
	* 4.  Anderson's BKY (2006) sharpened q-values
	preserve
	use "$table_data/Tablepvals_long.dta", clear
	version 10
	set more off

	* count and rank
	quietly summarize pval
	local totalpvals = r(N)
	gen int original_order = _n
	sort pval
	gen int rank = _n if pval < .

	* run the BKY step-down

	gen bky06_qval = 1 if pval < .

	local qval = 1
	while `qval' > 0 {
		* First Stage
		* Generate the adjusted first stage q level we are testing: q' = q/1+q
		local qval_adj = `qval'/(1+`qval')
		* Generate value q'*r/M
		gen fdr_temp1 = `qval_adj'*rank/`totalpvals'
		* Generate binary variable checking condition p(r) <= q'*r/M
		gen reject_temp1 = (fdr_temp1>=pval) if pval~=.
		* Generate variable containing p-value ranks for all p-values that meet above condition
		gen reject_rank1 = reject_temp1*rank
		* Record the rank of the largest p-value that meets above condition
		egen total_rejected1 = max(reject_rank1)

		* Second Stage
		* Generate the second stage q level that accounts for hypotheses rejected in first stage: q_2st = q'*(M/m0)
		local qval_2st = `qval_adj'*(`totalpvals'/(`totalpvals'-total_rejected1[1]))
		* Generate value q_2st*r/M
		gen fdr_temp2 = `qval_2st'*rank/`totalpvals'
		* Generate binary variable checking condition p(r) <= q_2st*r/M
		gen reject_temp2 = (fdr_temp2>=pval) if pval~=.
		* Generate variable containing p-value ranks for all p-values that meet above condition
		gen reject_rank2 = reject_temp2*rank
		* Record the rank of the largest p-value that meets above condition
		egen total_rejected2 = max(reject_rank2)

		* A p-value has been rejected at level q if its rank is less than or equal to the rank of the max p-value that meets the above condition
		replace bky06_qval = `qval' if rank <= total_rejected2 & rank~=.
		* Reduce q by 0.001 and repeat loop
		drop fdr_temp* reject_temp* reject_rank* total_rejected*
		local qval = `qval' - .001
	}


	sort original_order
	drop original_order rank
	reshape wide bky06_qval pval, i(outcome) j(effect) string
	rename bky06_qvaly q_anderson_younger
	rename bky06_qvala     q_anderson_add
	rename bky06_qvalt   q_anderson_total
	save "$table_data/Tableqvals.dta", replace
	restore

	*-------------------------------------------------------------------------------
	* 5.  Merge q-values back into the raw results
	use "$table_data/raw_results.dta", clear
	
	merge 1:1 outcome using "$table_data/Tableqvals.dta"
	drop _merge
	save "$table_data/results_with_q.dta", replace

	*-------------------------------------------------------------------------------
	* 6.  Romano–Wolf FWER for the two treatment effects
	clear all
	clear matrix
	use "$table_data/selp_temp_data_for_tables.dta", clear

	rwolf2 ///
	  (reghdfe dif_f1  incentive_group incentive_edu father_edu $ind, absorb(district_1 enu_code) vce(robust)) ///
	  (reghdfe dif_f3  incentive_group incentive_edu father_edu $ind, absorb(district_1 enu_code) vce(robust)) ///
	  (reghdfe dif_f5  incentive_group incentive_edu father_edu $ind, absorb(district_1 enu_code) vce(robust)) ///
	  (reghdfe dif_f7  incentive_group incentive_edu father_edu $ind, absorb(district_1 enu_code) vce(robust)) ///
	  (reghdfe dif_f9  incentive_group incentive_edu father_edu $ind, absorb(district_1 enu_code) vce(robust)) ///
	  (reghdfe dif_f11 incentive_group incentive_edu father_edu $ind, absorb(district_1 enu_code) vce(robust)) ///
	, indepvars(  incentive_group incentive_edu,  ///
				  incentive_group incentive_edu,  ///
				  incentive_group incentive_edu,  ///
				  incentive_group incentive_edu,  ///
				  incentive_group incentive_edu,  ///
				  incentive_group incentive_edu ) ///
	  seed(12345) reps(3000) usevalid
	  
	matrix FWER = e(RW)


	//for total impact on respondent with formal education
	fre father_edu
	clonevar father_edu1 = father_edu
	recode father_edu1 (1=0) (0=1)
	
	gen edu_incen = father_edu1*incentive_group
	
	
	rwolf2 ///
	  (reghdfe dif_f1  incentive_group edu_incen father_edu1 $ind, absorb(district_1 enu_code) vce(robust)) ///
	  (reghdfe dif_f3  incentive_group edu_incen father_edu1 $ind, absorb(district_1 enu_code) vce(robust)) ///
	  (reghdfe dif_f5  incentive_group edu_incen father_edu1 $ind, absorb(district_1 enu_code) vce(robust)) ///
	  (reghdfe dif_f7  incentive_group edu_incen father_edu1 $ind, absorb(district_1 enu_code) vce(robust)) ///
	  (reghdfe dif_f9  incentive_group edu_incen father_edu1 $ind, absorb(district_1 enu_code) vce(robust)) ///
	  (reghdfe dif_f11 incentive_group edu_incen father_edu1 $ind, absorb(district_1 enu_code) vce(robust)) ///
	, indepvars(  incentive_group edu_incen,  ///
				  incentive_group edu_incen,  ///
				  incentive_group edu_incen,  ///
				  incentive_group edu_incen,  ///
				  incentive_group edu_incen,  ///
				  incentive_group edu_incen ) ///
	  seed(12345) reps(3000) usevalid
	
	
	matrix tot_fwer = e(RW)
	
	
	
	
	*-------------------------------------------------------------------------------
	* 7.  Build all the LaTeX strings
	use "$table_data/results_with_q.dta", clear

	* Initialize storage for LaTeX table columns
	global tex_younger ""
	global tex_se_younger ""
	global tex_q_younger ""
	global tex_add_older ""
	global tex_se_add_older ""
	global tex_q_add_older ""
	global tex_total_older ""
	global tex_se_total_older ""
	global tex_q_total_older ""
	global tex_obs ""
	global tex_r2 ""
	global tex_fstat ""

	* List of dependent variables (these should be the same as in your original list)
	local depvars dif_f1 dif_f3 dif_f5 dif_f7 dif_f9 dif_f11

	* Manually define variable labels for the LaTeX table
	local labels ""Misperception about accepting financial support from one's daughter when she is married" "Misperception about taking financial support from a married daughter" "Misperception about higher dowry being required if the girl is aged more than 18" "Misperception about needing to pay higher dowry if the girl completes more years of education" "Misperception about girls who leave school early having better prospects of getting married" "Misperception about girls below 18 having better prospects of getting married""

	* Loop over each dependent variable (by name)
	local i = 1
	
	//set local macros for fwer
	local k = 1
	local h = 2
	
	foreach var of local depvars {

		* Collect necessary values for each dependent variable
		local b_y   = b_y[`i']
		local se_y  = se_y[`i']
		local p_y   = p_y[`i']
		local b_a   = b_a[`i']
		local se_a  = se_a[`i']
		local p_a   = p_a[`i']
		local b_t   = b_t[`i']
		local se_t  = se_t[`i']
		local p_t   = p_t[`i']
		local Nobs  = N[`i']
		local R2    = r2[`i']
		local Fst   = Fstat[`i']
		local q_y   = q_anderson_younger[`i']
		local q_a   = q_anderson_add[`i']
		local q_t   = q_anderson_total[`i']

		
		* FWER values for each outcome (just use saved variables from rwolf2)
		local fwer_y = FWER[`k', 3]
		di `fwer_y' 
		local fwer_a = FWER[`h', 3]
		di `fwer_a'
		local fwer_t = tot_fwer[`k', 3]  // Set this if you have the total effect stored

		* Significance stars for each p-value
		local star_y = cond(`p_y' < 0.01, "***", cond(`p_y' < 0.05, "**", cond(`p_y' < 0.10, "*", "")))
		local star_a = cond(`p_a' < 0.01, "***", cond(`p_a' < 0.05, "**", cond(`p_a' < 0.10, "*", "")))
		local star_t = cond(`p_t' < 0.01, "***", cond(`p_t' < 0.05, "**", cond(`p_t' < 0.10, "*", "")))

		* Append results to LaTeX strings
		global tex_younger_`var'    "$tex_younger & `=string(`b_y',"%6.3f")'`star_y'"
		global tex_se_younger_`var' "$tex_se_younger & (`=string(`se_y',"%6.3f")')"
		global tex_q_younger_`var'  "$tex_q_younger & [`=string(`q_y',"%6.3f")', `=string(`fwer_y',"%6.3f")']"

		global tex_add_older_`var'    "$tex_add_older & `=string(`b_a',"%6.3f")'`star_a'"
		global tex_se_add_older_`var' "$tex_se_add_older & (`=string(`se_a',"%6.3f")')"
		global tex_q_add_older_`var'  "$tex_q_add_older & [`=string(`q_a',"%6.3f")', `=string(`fwer_a',"%6.3f")']"

		global tex_total_older_`var'    "$tex_total_older & `=string(`b_t',"%6.3f")'`star_t'"
		global tex_se_total_older_`var' "$tex_se_total_older & (`=string(`se_t',"%6.3f")')"
		global tex_q_total_older_`var'  "$tex_q_total_older & [`=string(`q_t',"%6.3f")', `=string(`fwer_t', "%6.3f")']"

		//global tex_obs   "$tex_obs & `Nobs'"
		//global tex_r2    "$tex_r2 & `=string(`R2',"%6.3f")'"
		//global tex_fstat "$tex_fstat & `=string(`Fst',"%6.1f")'"

		local i = `i' + 1
		local k = `k' + 2
		local h = `h' + 2
	}

	*-------------------------------------------------------------------------------
	* 8.  Write out the final LaTeX table
	texdoc write \begin{table}[htbp]
	texdoc write \centering
	texdoc write \caption{Regression on misperception belief: Heterogeneity by respondent's education}
	texdoc write \label{tab:misperception_poverty}
	texdoc write \resizebox{\textwidth}{!}{\begin{tabular}{lccc}
	texdoc write \hline
	texdoc write \hline
	texdoc write  & \multicolumn{1}{p{7.3em}}{\centering  Impact on Respondent with No Formal Education} & ///
					 \multicolumn{1}{p{9.4em}}{\centering Additional Impact on Respondent with Formal Education} & ///
					 \multicolumn{1}{p{7.0em}}{\centering Impact on Respondent with Formal Education} \\
	texdoc write Dependent Variables & (1) & (2) & (3) \\
	texdoc write \hline

	* Fill rows with dependent variable labels and values
	local j = 1
	local depvars dif_f1 dif_f3 dif_f5 dif_f7 dif_f9 dif_f11
	foreach var of local depvars {
		* Use the manually defined label from the `labels` local
		local lbl : word `j' of `labels'
		texdoc write `lbl' ${tex_younger_`var'} ${tex_add_older_`var'} ${tex_total_older_`var'} \\
		texdoc write   ${tex_se_younger_`var'}  ${tex_se_add_older_`var'} ${tex_se_total_older_`var'} \\
		texdoc write   ${tex_q_younger_`var'} ///
						 ${tex_q_add_older_`var'} ///
						 ${tex_q_total_older_`var'} \\
		local ++j
	}

	texdoc write \hline
	texdoc write Observations & 2,914 & 2,914 & 2,914 \\
	texdoc write \hline
	texdoc write \hline
	texdoc write \end{tabular}}
	texdoc write \begin{flushleft}
	texdoc write {\scriptsize Note: All regressions include adolescent's age, respondent's age, education, poverty, religion, number of daughters, educational aspiration for the daughter, having at least one daughter married, having child marriage incident in the household in the last 2 years, and household size as control variables. *** p $<$ 0.01, ** p $<$ 0.05, * p $<$ 0.1. All regressions use district and enumerator fixed effects. Robust standard errors in parentheses. The additional row (in square brackets) shows Anderson's sharpened q-values and Romano–Wolf FWER estimates.}
	texdoc write \end{flushleft}
	texdoc write \end{table}

	texdoc close

	
	
	
	
	
	
	
	
	
	
	
	
	**# Table with q-values: Overall sample
	
	
	set more off
	clear all 
	clear matrix

	use "$table_data/selp_temp_data_for_tables.dta", clear
	
	*Initialize texdoc and globals
	texdoc init "$paper/new_overall", replace force

	* Dependent vars
	global depvars "dif_f1 dif_f3 dif_f5 dif_f7 dif_f9 dif_f11"

	* Controls (fill in your exact list)
	global ind "adolescent_age e_2 father_edu ppi_likelihood relg s1_e parents_want_edu daughhter_marr marr_early hh_size"

	*-------------------------------------------------------------------------------
	* 2.  Run regressions & collect raw results into a temporary file
	tempfile rawres
	tempname R
	postfile `R' str20 outcome ///
			double b_y se_y p_y ///
			double N r2 Fstat ///
		using "`rawres'", replace

	foreach var of global depvars {
		* run the cluster‐robust FE regression
		reghdfe `var' incentive_group $ind, ///
			absorb(district_1 enu_code) vce(robust)

		* collect coefficients and SEs
		local b_y = _b[incentive_group]
		local se_y = _se[incentive_group]

		* p-values for individual effects
		quietly test incentive_group = 0
		local p_y = r(p)
		

		* store
		post `R' ("`var'") ///
			(`b_y') (`se_y') (`p_y') ///
			(e(N)) (e(r2)) (e(F_statistic))
	}
	postclose `R'

	use "`rawres'", clear
	save "$table_data/raw_results.dta", replace

	*-------------------------------------------------------------------------------
	* 3.  Prepare p-values for multiple testing
	use "$table_data/raw_results.dta", clear
	keep outcome p_y
	reshape long p_, i(outcome) j(effect) string
	rename p_ pval
	save "$table_data/Tablepvals_long.dta", replace
	
	*-------------------------------------------------------------------------------
	* 4.  Anderson's BKY (2006) sharpened q-values
	preserve
	use "$table_data/Tablepvals_long.dta", clear
	version 10
	set more off

	* count and rank
	quietly summarize pval
	local totalpvals = r(N)
	gen int original_order = _n
	sort pval
	gen int rank = _n if pval < .

	* run the BKY step-down

	gen bky06_qval = 1 if pval < .

	local qval = 1
	while `qval' > 0 {
		* First Stage
		* Generate the adjusted first stage q level we are testing: q' = q/1+q
		local qval_adj = `qval'/(1+`qval')
		* Generate value q'*r/M
		gen fdr_temp1 = `qval_adj'*rank/`totalpvals'
		* Generate binary variable checking condition p(r) <= q'*r/M
		gen reject_temp1 = (fdr_temp1>=pval) if pval~=.
		* Generate variable containing p-value ranks for all p-values that meet above condition
		gen reject_rank1 = reject_temp1*rank
		* Record the rank of the largest p-value that meets above condition
		egen total_rejected1 = max(reject_rank1)

		* Second Stage
		* Generate the second stage q level that accounts for hypotheses rejected in first stage: q_2st = q'*(M/m0)
		local qval_2st = `qval_adj'*(`totalpvals'/(`totalpvals'-total_rejected1[1]))
		* Generate value q_2st*r/M
		gen fdr_temp2 = `qval_2st'*rank/`totalpvals'
		* Generate binary variable checking condition p(r) <= q_2st*r/M
		gen reject_temp2 = (fdr_temp2>=pval) if pval~=.
		* Generate variable containing p-value ranks for all p-values that meet above condition
		gen reject_rank2 = reject_temp2*rank
		* Record the rank of the largest p-value that meets above condition
		egen total_rejected2 = max(reject_rank2)

		* A p-value has been rejected at level q if its rank is less than or equal to the rank of the max p-value that meets the above condition
		replace bky06_qval = `qval' if rank <= total_rejected2 & rank~=.
		* Reduce q by 0.001 and repeat loop
		drop fdr_temp* reject_temp* reject_rank* total_rejected*
		local qval = `qval' - .001
	}


	sort original_order
	drop original_order rank
	reshape wide bky06_qval pval, i(outcome) j(effect) string
	rename bky06_qvaly q_anderson_younger

	save "$table_data/Tableqvals.dta", replace
	restore

	
	*-------------------------------------------------------------------------------
	* 5.  Merge q-values back into the raw results
	use "$table_data/raw_results.dta", clear
	
	merge 1:1 outcome using "$table_data/Tableqvals.dta"
	drop _merge
	save "$table_data/results_with_q.dta", replace

	*-------------------------------------------------------------------------------
	* 6.  Romano–Wolf FWER for the two treatment effects
	clear all
	clear matrix
	use "$table_data/selp_temp_data_for_tables.dta", clear

	rwolf2 ///
	  (reghdfe dif_f1  incentive_group $ind, absorb(district_1 enu_code) vce(robust)) ///
	  (reghdfe dif_f3  incentive_group $ind, absorb(district_1 enu_code) vce(robust)) ///
	  (reghdfe dif_f5  incentive_group $ind, absorb(district_1 enu_code) vce(robust)) ///
	  (reghdfe dif_f7  incentive_group $ind, absorb(district_1 enu_code) vce(robust)) ///
	  (reghdfe dif_f9  incentive_group $ind, absorb(district_1 enu_code) vce(robust)) ///
	  (reghdfe dif_f11 incentive_group $ind, absorb(district_1 enu_code) vce(robust)) ///
	, indepvars(  incentive_group,  ///
				  incentive_group,  ///
				  incentive_group,  ///
				  incentive_group,  ///
				  incentive_group,  ///
				  incentive_group ) ///
	  seed(12345) reps(3000) usevalid
	  
	matrix FWER = e(RW)


	*-------------------------------------------------------------------------------
	* 7.  Build all the LaTeX strings
	use "$table_data/results_with_q.dta", clear

	* Initialize storage for LaTeX table columns
	global tex_younger ""
	global tex_se_younger ""
	global tex_q_younger ""

	global tex_obs ""
	global tex_r2 ""
	global tex_fstat ""

	* List of dependent variables (these should be the same as in your original list)
	local depvars dif_f1 dif_f3 dif_f5 dif_f7 dif_f9 dif_f11

	* Manually define variable labels for the LaTeX table
	local labels ""Misperception about accepting financial support from one's daughter when she is married" "Misperception about taking financial support from a married daughter" "Misperception about higher dowry being required if the girl is aged more than 18" "Misperception about needing to pay higher dowry if the girl completes more years of education" "Misperception about girls who leave school early having better prospects of getting married" "Misperception about girls below 18 having better prospects of getting married""

	* Loop over each dependent variable (by name)
	local i = 1
	
	//set local macros for fwer
	local k = 1
	
	foreach var of local depvars {

		* Collect necessary values for each dependent variable
		local b_y   = b_y[`i']
		local se_y  = se_y[`i']
		local p_y   = p_y[`i']
		
		local Nobs  = N[`i']
		local R2    = r2[`i']
		local Fst   = Fstat[`i']
		local q_y   = q_anderson_younger[`i']
		

		
		* FWER values for each outcome (just use saved variables from rwolf2)
		local fwer_y = FWER[`k', 3]
		di `fwer_y' 

		* Significance stars for each p-value
		local star_y = cond(`p_y' < 0.01, "***", cond(`p_y' < 0.05, "**", cond(`p_y' < 0.10, "*", "")))


		* Append results to LaTeX strings
		global tex_younger_`var'    "$tex_younger & `=string(`b_y',"%6.3f")'`star_y'"
		global tex_se_younger_`var' "$tex_se_younger & (`=string(`se_y',"%6.3f")')"
		global tex_q_younger_`var'  "$tex_q_younger & [`=string(`q_y',"%6.3f")', `=string(`fwer_y',"%6.3f")']"


		//global tex_obs   "$tex_obs & `Nobs'"
		//global tex_r2    "$tex_r2 & `=string(`R2',"%6.3f")'"
		//global tex_fstat "$tex_fstat & `=string(`Fst',"%6.1f")'"

		local i = `i' + 1
		local k = `k' + 1
		
	}

	*-------------------------------------------------------------------------------
	* 8.  Write out the final LaTeX table
	texdoc write \begin{table}[htbp]
	texdoc write \centering
	texdoc write \caption{Regression on misperception belief: Overall sample}
	texdoc write \label{tab:misperception_poverty}
	texdoc write \resizebox{\textwidth}{!}{\begin{tabular}{lc}
	texdoc write \hline
	texdoc write \hline
	texdoc write  & \multicolumn{1}{p{5.3em}}{\centering Impact of Incentive} \\
	texdoc write Dependent Variable & (1) \\
	texdoc write \hline

	* Fill rows with dependent variable labels and values
	local j = 1
	local depvars dif_f1 dif_f3 dif_f5 dif_f7 dif_f9 dif_f11
	foreach var of local depvars {
		* Use the manually defined label from the `labels` local
		local lbl : word `j' of `labels'
		texdoc write `lbl' ${tex_younger_`var'} \\
		texdoc write   ${tex_se_younger_`var'} \\
		texdoc write   ${tex_q_younger_`var'} \\
		local ++j
	}

	texdoc write \hline
	texdoc write Observations & 2,914 \\
	texdoc write \hline
	texdoc write \hline
	texdoc write \end{tabular}}
	texdoc write \begin{flushleft}
	texdoc write {\scriptsize Note:  Each row reports coefficient and robust standard error (in parentheses) from a separate regression where the dependent variable is a belief indicator. All regressions include controls: adolescent age, respondent education, poverty likelihood, religion, number of daughters, educational aspiration, daughter married, recent child marriage, household size. District and enumerator fixed effects included. The additional row (in square brackets) shows Anderson's sharpened q-values and Romano–Wolf FWER estimates. *** p $<$ 0.01, ** p $<$ 0.05, * p $<$ 0.1.}
	texdoc write \end{flushleft}
	texdoc write \end{table}

	texdoc close

	
	
	
	
	
	
	
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		
		/*
		
**# Overall regression analysis 
	
	*selected dependent vars
	
	des f2 f4 f6 f8 f10 f12 
	
	local dep "f2 f4 f6 f8 f10 f12"
	
	local ind "e_2_w father_edu ppi_likelihood relg s1_e parents_want_edu hh_size adolescent_age"
	
	
	reghdfe f2 incentive_group `ind', absorb(district_1 enu_code) 
	est sto reg1 
	qui estadd local control "Yes"
	qui estadd local region "Yes"
	
	reghdfe f4 incentive_group `ind', absorb(district_1 enu_code) 
	est sto reg2 
	qui estadd local control "Yes"
	qui estadd local region "Yes"
	
	reghdfe f6 incentive_group `ind', absorb(district_1 enu_code) 
	est sto reg3 
	qui estadd local control "Yes"
	qui estadd local region "Yes"
	
	reghdfe f8 incentive_group `ind', absorb(district_1 enu_code) 
	est sto reg4 
	qui estadd local control "Yes"
	qui estadd local region "Yes"
	
	reghdfe f10 incentive_group `ind', absorb(district_1 enu_code)
	est sto reg5 
	qui estadd local control "Yes"
	qui estadd local region "Yes"
	
	reghdfe f12 incentive_group `ind', absorb(district_1 enu_code)
	est sto reg6 
	qui estadd local control "Yes"
	qui estadd local region "Yes"
	
	local regressions reg1 reg2 reg3 reg4 reg5 reg6
	
	esttab `regressions' using "$paper/overall.tex", replace label noconstant ///
		prehead("\begin{table}[htbp] \centering  \def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}  \caption{Regression with controls and district fixed effect} \resizebox{\textwidth}{!}{\begin{tabular}{l*{6}{c}} \hline\hline & \multicolumn{6}{c}{Dependent Variables} \\ \cmidrule{2-7}") ///
		nobaselevels ///
		drop(*`ind' _cons*) ///
		b(%8.3f) se(%8.3f) ///
		scalars("F F-statistics" "r2 R-squared" "N Number of Observations" "control Controls" "region District Fixed Effects") ///
		nonumbers mtitles("Accept FS" "Find FS Acceptable" "HDRFGA18" "HDRFGWME" "ESGHBMP" "GB18HBMP")  ///
		prefoot("\hline") ///
		postfoot("\hline\hline \multicolumn{6}{l}{\footnotesize Standard errors in parentheses}\\ \multicolumn{6}{l}{\footnotesize \sym{*} \(p<0.05\), \sym{**}  \(p<0.01\), \sym{***} \(p<0.001\)}\\ \end{tabular}} \end{table}")

	eststo clear
	

	
	**# Heterogeneity analysis table: age category
	

	
	
	**#Table codes start from here
	
	*storing title names
	local text1 ""Heterogeneity analysis by age category" "Heterogeneity analysis by wealth category" "Heterogeneity analysis by education category""
	
		
	local i = 1
	
	foreach y of local text1 {
		
		local title`i' "`y'"
		
		local i = `i' + 1
		
	}
	
	
	*store panel names 
	local text2 ""Older Father" "Younger Father" "Poor" "Non-poor" "Have formal education" "No formal education""
	
	local i = 1
	
	foreach y of local text2{
		
		local panel`i' "`y'"
		
		local i = `i' + 1
	}
	
	
	
	*store file name 
	local text3 ""hetero_age" "hetero_poverty" "hetero_edu""
	
	local i = 1
	
	foreach y of local text3{
		
		local file`i' "`y'"
		
		local i = `i' + 1
		
	}
		
	
	
	
	*panel A
	
	local dep "f2 f4 f6 f8 f10 f12"
	
	local ind "e_2 father_edu ppi_likelihood relg s1_e parents_want_edu hh_size adolescent_age last_exm_cgpa"
	
	local hetvars "fage_cat poverty_cat father_edu"
	
	
	
	local t = 1
	local x = 1
	local f = 1
	
	
	
	foreach i of local hetvars{
	
	reghdfe f2 incentive_group `ind' if `i'==1, absorb(district_1 enu_code) 
	est sto reg1 
	qui estadd local control "Yes"
	qui estadd local region "Yes"
	
	reghdfe f4 incentive_group `ind' if `i'==1, absorb(district_1 enu_code) 
	est sto reg2 
	qui estadd local control "Yes"
	qui estadd local region "Yes"
	
	reghdfe f6 incentive_group `ind' if `i'==1, absorb(district_1 enu_code) 
	est sto reg3 
	qui estadd local control "Yes"
	qui estadd local region "Yes"
	
	reghdfe f8 incentive_group `ind' if `i'==1, absorb(district_1 enu_code) 
	est sto reg4 
	qui estadd local control "Yes"
	qui estadd local region "Yes"
	
	reghdfe f10 incentive_group `ind' if `i'==1, absorb(district_1 enu_code)
	est sto reg5 
	qui estadd local control "Yes"
	qui estadd local region "Yes"
	
	reghdfe f12 incentive_group `ind' if `i'==1, absorb(district_1 enu_code)
	est sto reg6 
	qui estadd local control "Yes"
	qui estadd local region "Yes"
	
	local regressions reg1 reg2 reg3 reg4 reg5 reg6	
	dis `x'
	//top panel
	esttab `regressions' using "$paper/`file`f''.tex", replace label noconstant ///
		prehead("\begin{table}[htbp] \centering  \def\sym#1{\ifmmode^{#1}\else\(^{#1}\)\fi}  \caption{`title`t''} \resizebox{\textwidth}{!}{\begin{tabular}{l*{6}{c}} \hline\hline & \multicolumn{6}{c}{Dependent Variables} \\ \cmidrule{2-7}") ///
		posthead("\hline \multicolumn{6}{l}{\textbf{Panel A: `panel`x''}} \\\\[-1.5ex]") ///
		fragment ///
		drop(*`ind' _cons*) ///
		b(%8.3f) se(%8.3f) ///
		scalars("F F-statistics" "r2 R-squared" "N Number of Observations" "control Controls" "region District Fixed Effects") ///		
		nonumbers mtitles("Accept FS" "Find FS Acceptable" "HDRFGA18" "HDRFGWME" "ESGHBMP" "GB18HBMP")  
	
	
	eststo clear	
		
	*Panel B
	
	local x = `x' + 1
	
	reghdfe f2 incentive_group `ind' if `i'==0, absorb(district_1 enu_code) 
	est sto reg1 
	qui estadd local control "Yes"
	qui estadd local region "Yes"
	
	reghdfe f4 incentive_group `ind' if `i'==0, absorb(district_1 enu_code) 
	est sto reg2 
	qui estadd local control "Yes"
	qui estadd local region "Yes"
	
	reghdfe f6 incentive_group `ind' if `i'==0, absorb(district_1 enu_code) 
	est sto reg3 
	qui estadd local control "Yes"
	qui estadd local region "Yes"
	
	reghdfe f8 incentive_group `ind' if `i'==0, absorb(district_1 enu_code) 
	est sto reg4 
	qui estadd local control "Yes"
	qui estadd local region "Yes"
	
	reghdfe f10 incentive_group `ind' if `i'==0, absorb(district_1 enu_code)
	est sto reg5 
	qui estadd local control "Yes"
	qui estadd local region "Yes"
	
	reghdfe f12 incentive_group `ind' if `i'==0, absorb(district_1 enu_code)
	est sto reg6 
	qui estadd local control "Yes"
	qui estadd local region "Yes"
	
	local regressions reg1 reg2 reg3 reg4 reg5 reg6		
		
	//bottom panel
	esttab `regressions' using "$paper/`file`f''.tex", append label noconstant ///
		posthead("\hline \multicolumn{6}{l}{\textbf{Panel B: `panel`x''}} \\\\[-1.5ex]") ///
		fragment ///
		drop(*`ind' _cons*) ///
		b(%8.3f) se(%8.3f) ///
		scalars("F F-statistics" "r2 R-squared" "N Number of Observations" "control Controls" "region District Fixed Effects") ///
		nonumbers nomtitles ///
		prefoot("\hline") ///
		postfoot("\hline\hline \multicolumn{6}{l}{\footnotesize Standard errors in parentheses}\\ \multicolumn{6}{l}{\footnotesize \sym{*} \(p<0.05\), \sym{**}  \(p<0.01\), \sym{***} \(p<0.001\)}\\ \end{tabular}} \end{table}")

	eststo clear	
	
	local x = `x' + 1
	local t = `t' + 1
	local f = `f' + 1
	
	
	}
	
	
	*/
	
			
		
		
		
		
		
		
		
		
		
		
		