* purpose: analyze winter loon dive depth data;
* created by Brian Gray, US Geological Survey. last revised 18 Nov 2020;


* HOUSEKEEPING;
options nocenter;
title1 'Loon maximum dive depth';


* IMPORT LOON DIVE DATA;
***** enter user's filepath at "[enter filepath]" below *****;
proc import datafile="[enter filepath]\COLO winter dive data.csv" out=winterloondivedata;
		getnames=yes; 
		guessingrows=100;
run;


data maxdepth;
  set winterloondivedata(where=(gender ne "unkn" and winter_loc ne "SE") keep = geotag gender radio gender_radio winter_loc year month interval_at_max_depth max_depth);
  genradioloc=gender_radio||winter_loc;
  if max_depth then logmax_depth=log(max_depth);  
run;


* GENERATE DESCRIPTIVE STATS (and confirm no missing observations);
title2 "descriptive stats"; proc means data=maxdepth min median p90 max n nmiss; var max_depth; run;

* evaluate distribution of subjects/birds;
proc sort data=maxdepth out=geotags nodupkey; by gender_radio winter_loc geotag; run;
title2 "confirm that no bird in more than one location"; proc freq data=geotags; tables geotag*genradioloc; run;
title2 "confirm that multiple (3-18) birds per gender_radio*winter_loc cell"; proc freq data=geotags; tables gender_radio*winter_loc; run;


* PLOT MAX_DEPTH BY BIRD AND FACTOR;
ods graphics / reset width=1600px height=800px;
title2 'max depth by gender_radio and geotag';
proc sgpanel data=maxdepth;
	panelby gender_radio winter_loc / uniscale=row columns=2;
	vbox max_depth / group=geotag;
run;


* EVALUATE WHETHER SEs MOSTLY LOW AND SKEWNESS CONSISTENT ACROSS BIRDS;
title2 "Evaluate properties, including of sample means";
title3 "Relative SE <= 2%";
proc means data=maxdepth noprint alpha=0.025; 
	id gender_radio winter_loc genradioloc month; 
	var max_depth; 
	output out=birdstats mean=mean stddev=SD stderr=SE p10=p10 median=median p90=p90 max=max mode=mode skew=skew lclm=lclm uclm=uclm n=n; 
	by geotag; 
run;
data birdstats2; set birdstats; varsmean=SE**2; relSE = SE/mean; run;
proc means data=birdstats2 p10 p50 p90 max var; var mean sd SE relSE varsmean skew mode n; run;
title3;

* evaluate relative standard errors;
title2 "relSE and mean by geotag";
* relSEs not obviously associated with mean (within gender-radio-location cells, relSEs somewhat lower in ATL;
proc sgpanel data=birdstats2; panelby gender_radio winter_loc; scatter y=relSE x=mean / group = geotag; run;
* mild positive asso btwn SE and mean;
title2 "SE and mean by geotag";
proc sgplot data=birdstats2; scatter y=SE x=mean / group = genradioloc; run;


* fit linear model on sample means;
title2 'LMM _mean_ max depth';
proc glimmix data=birdstats method=rspl asycorr gradient infocrit=pq plots=(boxplot(subject) StudentPanel);
  class winter_loc gender_radio;
  model mean = winter_loc|gender_radio / ddfm=betwithin cl;
  random _residual_ / group=winter_loc;
  lsmeans winter_loc|gender_radio / cl diff adjust=scheffe lines;
  output out=outavmaxdepthav pred=mumarg stderr=SEmu lcl=lcl ucl=ucl student=student pearson=pearson resid=resid;
  ods output modelinfo=modelinfoav classlevels=classlevelsav nobs=nobsav dimensions=dimav parmsearch=parmsearchav optinfo=optinfoav iterhistory=iterhistav convergencestatus=convav 
		fitstatistics=fitstatsav parameterestimates=parmsav tests3=tests3av lsmeans=lsmeansav diffs=lsmdiffsav lsmlines=lsmlinesav;
run;

* plot studentized residuals;
title3 "Studentized resids by factor interactions";
proc sgpanel data=outavmaxdepthav;
	panelby gender_radio winter_loc;
	scatter y=student x=_type_ / datalabel=geotag;
	refline 0;
run;
title3;

* print sample statistics;
title2 "Sample statistics by geotag";
proc sort data=birdstats out=birdstatssort; by gender_radio winter_loc; run;
proc print data=birdstatssort; run;
