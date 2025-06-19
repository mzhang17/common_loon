* fit loon winter post dive surface interval models;
* code created by Brian Gray, US Geological Survey. last revised 19 Nov 2020;


* HOUSEKEEPING;
options nocenter;
title1 'Loon post dive surface interval models';


* IMPORT LOON DIVE DATA;
***** enter user's filepath at "[enter filepath]" below *****;
proc import datafile="[enter filepath]\COLO winter dive data.csv" out=winterloondivedata;
		getnames=yes; 
		guessingrows=100;
run;


* MODIFY DIVE DATASET;
data surfint;
  set winterloondivedata(where=(gender ne "unkn" and winter_loc ne "SE") rename=(post_dive_surface_interval = pdsurfint) 
		keep = geotag gender radio gender_radio winter_loc year month post_dive_surface_interval interval_at_max_depth max_depth dive_duration);
  genradioloc=gender_radio||winter_loc;
  missing=0; if pdsurfint = . then missing = 1;
  if not missing then logpdsurfint=log(pdsurfint);
run;


* GENERATE DESCRIPTIVE STATS;
* 383 missing observations in winterloondivedata;
title2 "compare rel frequencies of missing obs"; proc freq data=surfint; tables missing*gender_radio*winter_loc / nocol; run;
* evaluate distribution of missing observations;
proc sort data=surfint out=surfintsortmiss; by missing geotag; run;
title2 "descriptive stats (marginal over geotags, etc)"; 
proc means data=surfintsortmiss noprint; 
	var pdsurfint max_depth dive_duration; 
	output out=avbymiss min=minpdsurfint minmaxdepth mindivedur median=medpdsurfint medmaxdepth meddivedur p90=p90pdsurfint p90maxdepth p90divedur 
		max=maxpdsurfint maxmaxdepth maxdivedur nmiss=nmisspdsurfint nmissmaxdepth nmissdivedur; 
	by missing; 
run;
proc print data=avbymiss(drop=_type_) noobs; format _numeric_ 8.2 _freq_ missing; run;
title2 "Percent missing by bird"; 
proc sort data=surfint out=surfintsortgeotagmiss; by geotag missing; run;
proc means data=surfintsortgeotagmiss noprint; var missing; output out=missbygeotag mean=Pe; by geotag; run;
proc means data=missbygeotag min mean max n; var Pe; run;


* evaluate distribution of subjects/birds;
proc sort data=surfint out=geotags nodupkey; by gender_radio winter_loc geotag; run;
title2 "no bird in more than one location"; proc freq data=geotags; tables geotag*genradioloc; run;
title2 "multiple (3-18) birds per gender_radio*winter_loc cell";
proc freq data=geotags; tables gender_radio*winter_loc; run;


ods graphics / reset width=1600px height=800px;
title2 'post dive surface interval by gender_radio and geotag';
proc sgpanel data=surfint;
	panelby gender_radio winter_loc / uniscale=row columns=2;
	vbox pdsurfint / group=geotag;
	refline 45.9 / lineattrs=(pattern=shortdash) label="Grand median";
run;
title2 'log(post dive surface interval) by gender_radio and geotag';
proc sgpanel data=surfint;
	panelby gender_radio winter_loc / uniscale=row columns=2;
	vbox logpdsurfint / group=geotag;
run;


title2 'post-dive surface interval and max depth by location and gender-radio';
proc sgpanel data=surfint;
  panelby winter_loc /  rows=2 uniscale=column;
  pbspline y=pdsurfint x=max_depth / nknots=4 clm nomarkers group=gender_radio;
run;

title2 'post-dive surface interval and dive duration by location and gender-radio';
proc sgpanel data=surfint;
  panelby winter_loc /  rows=2 uniscale=column;
  pbspline y=pdsurfint x=dive_duration / nknots=4 clm nomarkers group=gender_radio;
run;



title2 'ln(post dive surface interval)';
* study units = bird/geotag;
ods results off; ods select none; * turn off output to screen;
proc glimmix data=surfint(where=(pdsurfint ne .)) method=rspl asycorr gradient plots=(boxplot(subject) PearsonPanel(marginal conditional) StudentPanel(marginal conditional));
  class winter_loc gender_radio geotag;
  model pdsurfint = winter_loc|gender_radio / s d=lognormal corrb ddfm=betwithin cl;
  random int / sub=geotag s;
  lsmeans winter_loc gender_radio / cl diff adjust=scheffe lines;
  lsmeans winter_loc*gender_radio / cl diff adjust=scheffe lines;
  output out=outdivedur pred(noblup)=mumarg pred=mu_ss stderr(noblup)=SEmumarg stderr=SEmu_ss lcl(noblup)=lclmarg ucl(noblup)=uclmarg student=studentSS student(noblup)=studentmarg
		pearson=pearsonss pearson(noblup)=pearsonmarg resid=residSS resid(noblup)=residmarg;
  ods output modelinfo=modelinfo classlevels=classlevels nobs=nobs dimensions=dim optinfo=optinfo iterhistory=iterhist convergencestatus=conv fitstatistics=fitstats
        corrb=corrb covparms=covparms asycorr=asycorr parameterestimates=parms solutionr=reffects tests3=tests3 lsmeans=lsmeans diffs=lsmdiffs lsmlines=lsmlines;
run;
ods results on; ods select all;

* print results to screen;
  title4 "Model info"; proc print data=modelinfo; run;
  title4 "Class levels"; proc print data=classlevels; run;
  title4 "Numbers of observations"; proc print data=nobs;   run;
  title4 "Dimensions"; proc print data=dim; run;
  title4 "Optimization info"; proc print data=optinfo; run;
  title4 "Iteration history"; proc print data=iterhist; run;
  title4 "Convergence info"; proc print data=conv; run;
  title4 "Fit stats"; proc print data=fitstats; run;
  title4 "Solution for fixed effects"; proc print data=parms; run;
  title4 "Corr matrix fixed effects"; proc print data=corrb; run;
  title4 "Covariance parameter estimates"; proc print data=covparms; run;
  title4 "Asymptotic correlation matrix of covariance parameter estimates"; proc print data=asycorr; run;
  title4 "Least-squared means"; proc print data=lsmeans; run;
  title4 "Least-squared means differences"; proc print data=lsmdiffs; run;
  title4 "Least-squared means lines"; proc print data=lsmlines; run;
  title4 "Type III F-tests"; proc print data=tests3; run;



* parse random effect labels;
title2 "group/bird/geotag-level estimates";
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

* no clear evidence that heterogeneity among predicted effects differs substantially among factors;
proc sgplot data=reffectsplus; scatter x=effect y=estimate /  datalabel=geotag markerattrs=(symbol=circlefilled) group=gender_radio; refline 0; run;
title3 "by gender_radio and winter loc";
proc sgpanel data=reffectsplus; 
	panelby gender_radio winter_loc / columns=3; 
	scatter x=effect y=estimate / datalabel=geotag markerattrs=(symbol=circlefilled) group=gender_radio; 
	refline 0; 
run;
title3 "by gender_radio, location and date";
proc sgpanel data=reffectsplus; 
	panelby gender_radio winter_loc / columns=3; 
	scatter x=monthyear y=estimate / datalabel=geotag markerattrs=(symbol=circlefilled) group=gender_radio; 
	refline 0; 
run;


title2 "dive-level subject-specific Pearson residuals";
title3 'winter_loc by location, gender_radio and geotag';
proc sgpanel data=outdivedur;
	panelby winter_loc gender_radio / uniscale=row columns=3 onepanel;
	vbox PearsonSS / group=geotag;
	refline -2 0 2 / lineattrs=(pattern=shortdash);
run;
title3 'winter_loc by location, gender_radio, year and geotag';
proc sgpanel data=outdivedur;
	panelby winter_loc gender_radio year / uniscale=row columns=6 onepanel;
	vbox PearsonSS / group=geotag;
	refline -2 0 2 / lineattrs=(pattern=shortdash); 
run;


title2 "dive-level subject-specific Studentized residuals";
title3 "proportion studentized resids that are missing";
data outdivedur2; set outdivedur; studentSSmiss=0; if studentSS=. then studentSSmiss=1; run;
proc freq data=outdivedur2; tables studentSSmiss; run;

title3 'by location, gender_radio and geotag';
proc sgpanel data=outdivedur;
	panelby winter_loc gender_radio / uniscale=row columns=3 onepanel;
	vbox StudentSS / group=geotag;
	refline -2 0 2 / lineattrs=(pattern=shortdash);
run;
title3 'by location, gender_radio, year and geotag';
proc sgpanel data=outdivedur;
	panelby winter_loc gender_radio year / uniscale=row columns=6 onepanel;
	vbox StudentSS / group=geotag;
	refline -2 0 2 / lineattrs=(pattern=shortdash); 
run;
