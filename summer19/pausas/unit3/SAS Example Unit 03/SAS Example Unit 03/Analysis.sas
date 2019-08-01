LIBNAME ClasData 'Class Data';

/* Create working dataset with additional variables */
data SalesExperiment;
 set ClasData.SalesExperiment;
 HiNeed = CustomerNeed > 45;
 HiInfoAdvice = SalesInfoAdvice > 45;
 HiOrder = CustomerDecision > 45;
run; 


/* Hypothesis Tests */

/* Contingency Table and Chi-Square Test of Independence */

/* Is HiInforAdvice related to HiNeed? */
proc freq data=SalesExperiment;
 tables HiNeed * HiInfoAdvice;
run;


ODS RTF FILE='Chi-Sq.rtf';
proc freq data=SalesExperiment;
 tables HiNeed * HiInfoAdvice/ChiSq;
run;


/* Hypothesis test for each treatment / experimental condition */
proc freq data=SalesExperiment;
 tables TreatmentId * HiNeed * HiInfoAdvice/ChiSq;
run;
ODS RTF CLOSE;

/* Correlation test */
/* Is SalesInfoAdvice related to CustomerNeed?... Customer Decision to SalesInfoAdvice? */

ODS RTF FILE='Correlation.rtf';

proc sort data=SalesExperiment;;
 by Repeated ReportType;
run;


proc corr data=SalesExperiment;
 var CustomerNeed SalesInfoAdvice CustomerDecision;
 by Repeated ReportType;
run;


/* By default SAS uses pearson correlation*/
/* Recall that pearson correlation is ideally meant to test for linear relationships */
/* It can give incorrect or inconsistent results if there is a non-linear relationship - especially non-monotonic relationship*/
/* Do exploratory analysis first */

/* Spearman correlation is less sensitive to departures from linear relation */
/* But is not a complete solution - can still have problems with non-monotonic relationship */

proc corr data=SalesExperiment spearman;
 var CustomerNeed SalesInfoAdvice CustomerDecision;
 by Repeated ReportType;
run;
ODS RTF CLOSE;

ODS RTF FILE='T-Tests.rtf';
proc sort data=SalesExperiment; 
by Repeated ReportType;
run;

/* Single Sample T-Tests */
/* Is (avg of) CustomerNeed = 45?... SalesInfoReport > 45?... CustomerDecision < 45 */
proc ttest data=SalesExperiment H0=45;
 var CustomerNeed SalesInfoAdvice CustomerDecision;
 by Repeated ReportType;
run;

/* Paired Sample T-Tests */
/* Is SalesInfoReport > CustomerNeed */
proc ttest data=SalesExperiment;
 paired CustomerNeed*SalesInfoAdvice;
 by Repeated ReportType;
run;

/* Independent Sample T-Tests */
/* Is SalesInfoReport for IS (Information Sharing) > SalesInfoReport for AP (Advice Provision) */
 
proc ttest data=SalesExperiment;
 class ReportType;
 var SalesInfoAdvice;
 by Repeated;
run;
ODS RTF CLOSE;
