LIBNAME ClasData 'Class Data';

PROC IMPORT OUT= ClasData.SALESEXPERIMENT 
            DATAFILE= "Sales Information Advice Experiment Data.csv" 
            DBMS=CSV REPLACE;
     GETNAMES=YES;
     DATAROW=2; 
	 GUESSINGROWS=600;
RUN;

/* working dataset */
data SalesExperiment;
 set ClasData.SalesExperiment;
run; 



/* Summary Statistics using PROC means */
proc means data= SalesExperiment n mean stddev min p25 median p75 max maxdec= 2;
 var CustomerNeed SalesInfoAdvice CustomerDecision;
 title 'Summary Statistics';
run;

proc means data= SalesExperiment n mean stddev min p25 median p75 max maxdec= 2;
 class ReportType Repeated;
 var CustomerNeed SalesInfoAdvice CustomerDecision;
 title 'Summary Statistics';
run;

proc means data= SalesExperiment n mean stddev min p25 median p75 max maxdec= 2;
 class ReportType Repeated Round;
 var CustomerNeed SalesInfoAdvice CustomerDecision;
 title 'Summary Statistics';
run;

proc corr data=SalesExperiment;
 var CustomerNeed SalesInfoAdvice CustomerDecision;
 by Repeated ReportType;
run;

/* data must be sorted appropriately before using a by statement in proc means or other procs */
proc sort data=SalesExperiment;;
 by Repeated;
run;
/* by statement creates separate tables for all levels (and combinations) of the by variables */ 
proc means data= SalesExperiment n mean stddev min p25 median p75 max maxdec= 2;
 by Repeated; 
 class ReportType;
 var CustomerNeed SalesInfoAdvice CustomerDecision;
 title 'Summary Statistics';
run;

/* You can store output from any SAS command or set of commands in a RTF file to copy to another report */
ODS RTF FILE='Summary Stats.rtf';

proc means data= SalesExperiment n mean stddev min p25 median p75 max maxdec= 2;
 by Repeated;
 class ReportType;
 var CustomerNeed SalesInfoAdvice CustomerDecision;
 title 'Summary Statistics';
run;

ODS RTF CLOSE; 

/* Summary Statistics using PROC tabulate */
proc tabulate data= SalesExperiment ;
 var CustomerDecision; *variables described by the table; 
 table CustomerDecision; 
 title 'Sum of CustomerDecision';
run;

proc tabulate data= SalesExperiment ;
 var CustomerDecision; *variables described by the table; 
 table CustomerDecision*(N Mean StdDev Min p25 Median p75 Max); 
 title 'Summary Statistics of CustomerDecision';
run;

proc tabulate data= SalesExperiment ;
 var CustomerDecision; *variables described by the table; 
 table CustomerDecision,N Mean StdDev Min p25 Median p75 Max; 
 title 'Summary Statistics of CustomerDecision';
run;

proc tabulate data= SalesExperiment ;
 var CustomerNeed SalesInfoAdvice CustomerDecision; *variables described by the table; 
 table CustomerNeed SalesInfoAdvice CustomerDecision,N Mean StdDev Min p25 Median p75 Max; 
 title 'Summary Statistics';
run;

proc tabulate data= SalesExperiment ;
 class Repeated ReportType; *categorical variables for breakdown of variable summaries;
 var CustomerNeed SalesInfoAdvice CustomerDecision; *variables described by the table; 
 table Repeated*ReportType*(CustomerNeed SalesInfoAdvice CustomerDecision),N Mean StdDev Min p25 Median p75 Max; 
 title 'Summary Statistics';
run;

proc tabulate data= SalesExperiment ;
 class Repeated ReportType; *categorical variables for breakdown of variable summaries;
 var CustomerNeed SalesInfoAdvice CustomerDecision; *variables described by the table; 
 table Repeated*(CustomerNeed SalesInfoAdvice CustomerDecision),ReportType*(N Mean StdDev Min p25 Median p75 Max); 
 title 'Summary Statistics';
run;

proc tabulate data= SalesExperiment ;
 class Repeated ReportType Round; *categorical variables for breakdown of variable summaries;
 var CustomerNeed SalesInfoAdvice CustomerDecision; *variables described by the table; 
 table Repeated*Round*(CustomerNeed SalesInfoAdvice CustomerDecision),ReportType*(N Mean StdDev Min p25 Median p75 Max); 
 title 'Summary Statistics';
run;


/* Other uses of PROC tabulate  */
proc tabulate data= SalesExperiment ;
 class TreatmentId Round; *categorical variables for rows and columns;
 table TreatmentId, Round; 
 title 'Frequencies of Observations by Treatment and Round';
run;

proc tabulate data= SalesExperiment ;
 class TreatmentId Round; *categorical variables for rows and columns;
 var CustomerDecision; *variables described by the table; 
 table TreatmentId, CustomerDecision*Round; 
 title 'Total of CustomerDecision by Treatment and Round ';
run;

/* Descriptive Analysis using PROC univariate */
proc univariate data= SalesExperiment;
 class Repeated ReportType;
 var CustomerDecision;
 histogram CustomerDecision;
 title 'Summary Statistics for Customer Decision';
run;

/* Descriptive Analysis using PROC sgplot and sgpanel; */

/* Box Plots */
proc sgplot data= SalesExperiment;
 hbox SalesInfoAdvice / category= Repeated group= ReportType ; 

 title 'Sales Report';
run;


/* Histograms */
proc sgplot data= SalesExperiment;
 histogram CustomerDecision / binstart = 10 binwidth = 5 ; 
 density CustomerDecision / type = kernel; 
 density CustomerDecision; 
 title 'Customer Decision';
run;

/* Series plots */
proc sort data= SalesExperiment; 
 by Repeated ReportType Round;
run;

proc means data= SalesExperiment mean maxdec= 2 noprint;
 by Repeated ReportType Round;
 var CustomerNeed SalesInfoAdvice CustomerDecision;
 output out=means 
  mean= AvgCustomerNeed AvgSalesInfoAdvice AvgCustomerDecision;
run;

proc means data= SalesExperiment mean maxdec= 2 noprint;
 by Repeated ReportType;
 class Round;
 var CustomerNeed SalesInfoAdvice CustomerDecision;
 output out=means 
  mean= AvgCustomerNeed AvgSalesInfoAdvice AvgCustomerDecision;
run;


proc sgpanel  data= means;
 panelby Repeated ReportType;
 series x=Round y=AvgCustomerDecision;
 title 'Customer Decision by Decision Round';
run;

/* Scatter Plots */
proc sgplot data = SalesExperiment;
 scatter X=CustomerNeed Y=SalesInfoAdvice; 
 title 'Customer Need and Sales Report';
 label SalesInfoAdvice = 'Sales Report' CustomerNeed = 'Customer Need'; 
run;

proc sgpanel data= SalesExperiment;
 panelby ReportType Repeated;
 
 scatter X=CustomerNeed Y=SalesInfoAdvice ; 
 title 'Customer Need and Sales Report';
 label CustomerNeed= 'Customer Need' SalesInfoAdvice = 'Sales Report'; 

run;
proc sgpanel data= SalesExperiment;
 panelby ReportType Repeated;
 
 scatter X=SalesInfoAdvice Y=CustomerDecision ; 
 title 'Sales Report and Customer Decision';
 label CustomerDecision = 'Customer Decision' SalesInfoAdvice = 'Sales Report'; 

run;

proc sgscatter data= SalesExperiment;
 matrix CustomerNeed SalesInfoAdvice CustomerDecision / diagonal= (histogram);
 title 'Relation between Customer Need, Sales Report and Customer Decision';
run;

proc corr data= SalesExperiment plots=matrix(histogram);
 var CustomerNeed SalesInfoAdvice CustomerDecision;
 title 'Relation between Customer Need, Sales Report and Customer Decision';
run;


/* Fitted Lines */
proc sgpanel data= means;
 panelby ReportType;
 scatter x=AvgCustomerNeed y=AvgCustomerDecision /group= Repeated;
 reg X=AvgCustomerNeed Y=AvgCustomerDecision /group= Repeated ; 
 title 'Average Customer Decision by Decision Round';
run;
