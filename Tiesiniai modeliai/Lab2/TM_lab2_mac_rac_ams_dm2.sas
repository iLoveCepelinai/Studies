proc import out = work.productivity_temp
			datafile = "/home/u45718890/sasuser.v94/garments_worker_productivity.csv" 
    replace;
    getnames = yes;
run;

data work.productivity;
	length day $ 12;
	set work.productivity_temp;

	where incentive > 0 and idle_men = 0 and actual_productivity ge targeted_productivity * 0.9
		  and wip < 2500 and smv < 40 and over_time < 25000 and no_of_workers < 70;
	if cmiss(of _all_) then delete;
	
	if day = "Monday" then day1 = 1;
	if day = "Tuesday" then day2 = 1;
	if day = "Wednesda" then day3 = 1;
	if day = "Thursday" then day4 = 1;
	if day = "Saturday" then day5 = 1;
	
	if day1 = . then day1 = 0;
	if day2 = . then day2 = 0;
	if day3 = . then day3 = 0;
	if day4 = . then day4 = 0;
	if day5 = . then day5 = 0;
	
	
	keep targeted_productivity smv wip over_time incentive no_of_workers actual_productivity
	day1 day2 day3 day4 day5;
run;


proc corr data = work.productivity plots=matrix(histogram);
	var actual_productivity targeted_productivity smv incentive 
		wip over_time no_of_workers;
quit;


proc reg data=work.productivity simple  plots=(diagnostics(stats=none) RStudentByLeverage(label)
             CooksD(label) Residuals(smooth)
             DFFITS(label) DFBETAS ObservedByPredicted(label));
	model actual_productivity = targeted_productivity
								smv wip over_time
								incentive no_of_workers
								day1 day2 day3 day4 day5
	/ stb vif clb pcorr2;  
run; quit;

data work.productivity2;
	set work.productivity;
	if _n_ in(409, 140, 275, 372, 88, 130, 286) then delete;
run;


proc reg data=work.productivity2 simple  plots=(diagnostics(stats=none) RStudentByLeverage(label)
             CooksD(label) Residuals(smooth)
             DFFITS(label) DFBETAS ObservedByPredicted(label));
	model actual_productivity = targeted_productivity
								smv wip over_time
								incentive no_of_workers
								day1 day2 day3 day4 day5
	/ stb vif clb pcorr2;  
run; quit;

data work.productivity3;
	set work.productivity2;
	if _n_ in(251, 341, 22, 436, 274) then delete;
run;

proc reg data=work.productivity3 simple  plots=(diagnostics(stats=none) RStudentByLeverage(label)
             CooksD(label) Residuals(smooth)
             DFFITS(label) DFBETAS ObservedByPredicted(label));
	model actual_productivity = targeted_productivity
								smv wip over_time
								incentive no_of_workers
								day1 day2 day3 day4 day5
	/ stb vif clb pcorr2;
	output out = productivity_res residual=residuals;  
run; quit;

proc univariate data=productivity_res normal;
	var residuals;
run;

proc model data = work.productivity3;
	parms beta0 beta1 beta2 beta3 beta4 beta5 beta6
		  beta7 beta8 beta9 beta10 beta11;
		  
	actual_productivity=beta0+
	beta1*targeted_productivity+
	beta2*smv+beta3*wip+beta4*over_time+
	beta5*incentive+beta6*no_of_workers+
	beta7*day1+beta8*day2+beta9*day3+
	beta10*day4+beta11*day5;

	fit actual_productivity / white

	breusch=(1 targeted_productivity smv wip
			 over_time incentive no_of_workers 
			 day1 day2 day3 day4 day5);
run; quit;


proc reg data=work.productivity3 simple  plots=(diagnostics(stats=none) RStudentByLeverage(label)
             CooksD(label) Residuals(smooth)
             DFFITS(label) DFBETAS ObservedByPredicted(label));
	model actual_productivity = targeted_productivity
								smv wip over_time
								incentive no_of_workers
								day1 day2 day3 day4 day5
	/ stb vif clb pcorr2 hccmethod=0;
	output out = productivity_res residual=residuals;  
run; quit;



proc reg data=work.productivity3 simple  plots=(diagnostics(stats=none) RStudentByLeverage(label)
             CooksD(label) Residuals(smooth)
             DFFITS(label) DFBETAS ObservedByPredicted(label));
	model actual_productivity = targeted_productivity
								smv wip over_time
								incentive no_of_workers
								day1 day2 day3 day4 day5
	/ stb vif clb pcorr2 hccmethod=0 selection = stepwise slstay = 0.05 slentry = 0.05;
	output out = productivity_res residual=residuals;  
run; quit;

proc reg data=work.productivity3 simple  plots=(diagnostics(stats=none) RStudentByLeverage(label)
             CooksD(label) Residuals(smooth)
             DFFITS(label) DFBETAS ObservedByPredicted(label));
	model actual_productivity = targeted_productivity
								wip over_time
								incentive no_of_workers
								day1 day2 day3 day4 day5
	/ stb vif clb pcorr2 hccmethod=0;
	output out = productivity_res residual=residuals;  
run; quit;

proc reg data=work.productivity3 simple  plots=(diagnostics(stats=none) RStudentByLeverage(label)
             CooksD(label) Residuals(smooth)
             DFFITS(label) DFBETAS ObservedByPredicted(label));
	model actual_productivity = targeted_productivity
								wip over_time
								incentive no_of_workers
	/ stb vif clb pcorr2 hccmethod=0;
	output out = productivity_res residual=residuals;  
run; quit;




	