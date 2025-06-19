* fit loon winter dive duration models using data;
* code created by Brian Gray, US Geological Survey. last updated 20 Nov 2020;


* HOUSEKEEPING;
options nocenter;
title1 'Loon time max depth (winter)';


* IMPORT LOON DIVE DATA;
***** enter user's filepath at "[enter filepath]" below *****;
proc import datafile="[enter filepath]\COLO winter dive data.csv" out=winterloondivedata;
		getnames=yes; 
		guessingrows=100;
run;


* MODIFY DIVE DATASET;
* as the mean response from geotag 2754 represents an outlier, this file should be run with and without data from that geotag (by running with and 
		without the "/*" and "*/" remark identifiers in the "set" line immediately below);
data timemaxdepth;
  set winterloondivedata(where=(gender ne "unkn" and winter_loc ne "SE" /*and geotag ne 2754*/) rename=(interval_at_max_depth = timemaxdepth) 
		keep = geotag gender radio gender_radio winter_loc year month dive_duration interval_at_max_depth);
  genradioloc=gender_radio||winter_loc;
  missing=0; if timemaxdepth = . then missing = 1;
 run;


* GENERATE DESCRIPTIVE STATS;
* no missing timemaxdepth measurements;
title2 "compare rel frequencies of missing obs"; proc freq data=timemaxdepth; tables missing*gender_radio*winter_loc / nocol; run;
title2 "descriptive stats"; proc means data=timemaxdepth min median p90 max n nmiss; var timemaxdepth; run;

title2 "sample size (n) by geotag > 100"; 
proc sort data=timemaxdepth out=timemaxdepthsort; by geotag; run;
proc means data=timemaxdepth noprint; id winter_loc gender_radio; var timemaxdepth; output out=nbird mean=mean stderr=SE n=n; by geotag; run;
proc means data=nbird min q1 median q3 max n; format _numeric_ 8.; var n; run;

* calculate sample sizes;
proc sort data=timemaxdepth out=geotags nodupkey; by gender_radio winter_loc geotag; run;
title2 "confirm no bird in more than one location"; proc freq data=geotags; tables geotag*genradioloc; run;
title2 "confirm multiple (3-18) birds per gender_radio*winter_loc cell"; proc freq data=geotags; tables gender_radio*winter_loc; run;

* evaluate distributions of subjects/birds;
ods graphics / reset width=1600px height=800px;
title2 'dive duration by gender_radio and geotag';
proc sgpanel data=timemaxdepth;
	panelby gender_radio winter_loc / uniscale=row columns=2;
	vbox timemaxdepth / group=geotag;
	refline 80 / lineattrs=(pattern=shortdash) label="Grand median";
run;


* FIT LOCATION AND GENDER-RADIO EFFECTS MODEL;
* study units = bird/geotag;
title2 'model output';
ods results off; ods select none; * turn off output to screen;
proc glimmix data=timemaxdepth method=rspl asycorr gradient plots=(boxplot(subject) PearsonPanel(marginal conditional) StudentPanel(marginal conditional));
  class winter_loc gender_radio geotag;
  model timemaxdepth = winter_loc|gender_radio / s d=normal corrb ddfm=betwithin cl;
  random int / sub=geotag s;
  lsmeans winter_loc gender_radio / cl diff adjust=scheffe lines;
  lsmeans winter_loc*gender_radio / cl diff adjust=scheffe lines;
  output out=outtimemaxdepth pred(noblup)=mumarg pred=mu_ss stderr(noblup)=SEmumarg stderr=SEmu_ss lcl(noblup)=lclmarg ucl(noblup)=uclmarg student=studentSS student(noblup)=studentmarg
		pearson=pearsonss pearson(noblup)=pearsonmarg resid=residSS resid(noblup)=residmarg;
  ods output modelinfo=modelinfo classlevels=classlevels nobs=nobs dimensions=dim optinfo=optinfo iterhistory=iterhist convergencestatus=conv fitstatistics=fitstats
        corrb=corrb covparms=covparms asycorr=asycorr parameterestimates=parms solutionr=reffects tests3=tests3 lsmeans=lsmeans diffs=lsmdiffs lsmlines=lsmlines;
run;
ods results on; ods select all;

* print model results;
title3 "Model info"; proc print data=modelinfo; run;
title3 "Class levels"; proc print data=classlevels; run;
title3 "Numbers of observations"; proc print data=nobs;   run;
title3 "Dimensions"; proc print data=dim; run;
title3 "Optimization info"; proc print data=optinfo; run;
title3 "Iteration history"; proc print data=iterhist; run;
title3 "Convergence info"; proc print data=conv; run;
title3 "Fit stats"; proc print data=fitstats; run;
title3 "Solution for fixed effects"; proc print data=parms; run;
title3 "Corr matrix fixed effects"; proc print data=corrb; run;
title3 "Covariance parameter estimates"; proc print data=covparms; run;
title3 "Asymptotic correlation matrix of covariance parameter estimates"; proc print data=asycorr; run;
title3 "Least-squared means"; proc print data=lsmeans; run;
title3 "Least-squared means differences"; proc print data=lsmdiffs; run;
title3 "Least-squared means lines"; proc print data=lsmlines; run;
title3 "Type III F-tests"; proc print data=tests3; run;
title3;


* PLOT GROUP-LEVEL RESIDUALS;
* retrieve geotag number from subject label;
data reffects2; set reffects(drop=tvalue probt); format geotag 15.; geotag = input(substr(subject,8),5.); run;

* merge random geotag effects file with geotag info;
proc sort data=geotags out=geotagssort; by geotag; run;
data reffectsplus;
	merge reffects2 geotagssort(keep=geotag gender winter_loc radio year month gender_radio); 
	id=_N_; 
	monthyear=month+year/100;
	by geotag; 
run;

title2 "group/bird/geotag-level estimates";
proc sgplot data=reffectsplus; 
	scatter x=effect y=estimate / datalabel=geotag markerattrs=(symbol=circlefilled) group=gender_radio; 
	refline 0; 
run;

* plot group-level residuals;
proc sgplot data=reffectsplus; vbox estimate / datalabel=geotag ; run;
proc univariate data=reffectsplus noprint; var estimate; qqplot; run;
title3 "Geotag with low estimated effects";
proc print data=reffectsplus(where=(estimate < -30)); run;
title3 "by gender_radio and winter loc";

proc sgpanel data=reffectsplus;
	panelby winter_loc gender_radio / columns=3; 
	scatter x=effect y=estimate / datalabel=geotag markerattrs=(symbol=circlefilled) group=gender_radio; 
	refline 0; 
run;
title3 "by winter loc";

proc sgpanel data=reffectsplus;
	panelby winter_loc / columns=2; 
	scatter x=effect y=estimate / datalabel=geotag markerattrs=(symbol=circlefilled) group=gender_radio; 
	refline 0; 
run;
run;
title3 "by gender radio";
proc sgpanel data=reffectsplus;
	panelby gender_radio / columns=3; 
	scatter x=effect y=estimate / datalabel=geotag markerattrs=(symbol=circlefilled) group=winter_loc; 
	refline 0; 
run;
title3 "by gender_radio, location and date";
proc sgpanel data=reffectsplus; 
	panelby gender_radio winter_loc / columns=3; 
	scatter x=monthyear y=estimate / datalabel=geotag markerattrs=(symbol=circlefilled) group=gender_radio; 
	refline 0; 
run;


* PLOT MEASUREMENT-LEVEL RESIDUALS;
title2 "dive-level subject-specific Studentized residuals";
title3 "proportion studentized resids that are missing";
data outtimemaxdepth2; set outtimemaxdepth; studentSSmiss=0; if studentSS=. then studentSSmiss=1; run;
proc freq data=outtimemaxdepth2; tables studentSSmiss; run;

title3 'winter_loc by gender_radio, year';
proc sgpanel data=outtimemaxdepth;
	panelby winter_loc gender_radio / uniscale=row columns=3 onepanel;
	vbox StudentSS / group=geotag;
	refline -2 0 2 / lineattrs=(pattern=shortdash);
run;
title3 'winter_loc by gender_radio, geotag';
proc sgpanel data=outtimemaxdepth;
	panelby winter_loc gender_radio year / uniscale=row columns=6 onepanel;
	vbox StudentSS / group=geotag;
	refline -2 0 2 / lineattrs=(pattern=shortdash); 
run;
