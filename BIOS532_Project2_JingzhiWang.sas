***********************************************************************************;
*** BIOS 532;
*** Project 2;
*** Student: Jingzhi Wang;
*** Emory ID: 1587727;
*** Submitted by 2014/04/01
***********************************************************************************;


***********************************************************************************;
*** Question A;
*** 1. Create a randomization list for a balanced study with 3 treatments(arms) 
*** 2. Do same for center #2. Patients should have ids 201-250. Either use another
***  seed for this center or do in same datastep as center#1.
***********************************************************************************;


***********************************************************************************;
*** Question A.1 Solution
***********************************************************************************;

/* Create 5 and 10 treatment for each block */
DATA shuffle1;
     seed=123456;
	 n=50;
	 block=0;
	 do until (n=0);   	 
        blocksize=rantbl(seed,0.5,0.5)*5;
        if n =5 then blocksize=5;  
		n=n-blocksize;
        if blocksize = 5 then do;
	      trt='A'; x=uniform(seed); output;
	      trt='A'; x=uniform(seed); output;
	      trt='B'; x=uniform(seed); output;
	      trt='B'; x=uniform(seed); output;
          trt='C'; x=uniform(seed); output;
	    end;
		else do;
	      trt='A'; x=uniform(seed); output;
	      trt='A'; x=uniform(seed); output;
	      trt='B'; x=uniform(seed); output;
	      trt='B'; x=uniform(seed); output;
          trt='C'; x=uniform(seed); output;
		  trt='A'; x=uniform(seed); output;
	      trt='A'; x=uniform(seed); output;
	      trt='B'; x=uniform(seed); output;
	      trt='B'; x=uniform(seed); output;
          trt='C'; x=uniform(seed); output;
	    end;
		block=block+1;
	 end;
run;

/* sort data by block number */
proc sort data=shuffle1; 
	by block x;
run;

/* Assign the ID number */
data center1;
	set shuffle1;
	if _n_=1 then ID=100;
	ID+1;  
run;

/* Print out the result */
proc print data=center1;
	TITLE 'Question A Center 1';
	var ID trt;
run;


***********************************************************************************;
*** Question A.2 Solution
***********************************************************************************;

/* Create 5 and 10 treatment for each block */
DATA shuffle2;
     seed=654321; ** USING A NEW SEED;
	 n=50;
	 block=0;
	 do until (n=0);   	 
        blocksize=rantbl(seed,0.5,0.5)*5;
        if n =5 then blocksize=5;  
		n=n-blocksize;
        if blocksize = 5 then do;
	      trt='A'; x=uniform(seed); output;
	      trt='A'; x=uniform(seed); output;
	      trt='B'; x=uniform(seed); output;
	      trt='B'; x=uniform(seed); output;
          trt='C'; x=uniform(seed); output;
	    end;
		else do;
	      trt='A'; x=uniform(seed); output;
	      trt='A'; x=uniform(seed); output;
	      trt='B'; x=uniform(seed); output;
	      trt='B'; x=uniform(seed); output;
          trt='C'; x=uniform(seed); output;
		  trt='A'; x=uniform(seed); output;
	      trt='A'; x=uniform(seed); output;
	      trt='B'; x=uniform(seed); output;
	      trt='B'; x=uniform(seed); output;
          trt='C'; x=uniform(seed); output;
	    end;
		block=block+1;
	 end;
run;

/* sort data by block number */
proc sort data=shuffle2; 
	by block x;
run;

/* Assign the ID number */
data center2;
	set shuffle2;
	if _n_=1 then ID=200; ** ASSIGNING NEW ID SERIES;
	ID+1;  
run;

/* Print out the result */
proc print data=center2;
	TITLE 'Question A Center 2';
	var ID trt;
run;



***********************************************************************************;
*** Question B;
*** Use simulation to address (do 1000 simulations)
*** Power for a paired t-test assuming: bivariate normality and ....(parameters)
***********************************************************************************;


***********************************************************************************;
*** Question B Solution
***********************************************************************************;

proc iml;
	mu={25,26.5};
	sig={9 10,
		10 16};
	n= 20;
	numloops = 1000;
	seed=123456;
	x= randnormal(n,mu,sig);
	rep= repeat(1,n,1);
	result=x||rep; /*create one sample in a matrix "result" */
	do i=1 to (numloops - 1); /* add the rest 999 samples to the matrix "result" */
		x= randnormal(n,mu,sig);
		rep= repeat(i,n,1);
		iresult=x||rep;
		result=result//iresult;
	end;
	create population from result[c={"pre","post","replicate"}]; 
	TITLE 'Question B';
	append from result;
	close population;
quit;


ods select none;

/* Paired t-test results */
proc ttest data=population; 
	paired pre*post;
	by replicate;
	ods output ttests = ttest;
run;

/* Determine rejection */
data testresult;
	set ttest;
	reject=(probt lt 0.05);
	*  if < 0.05 then reject=1 , else reject =0;
	keep reject;
run;

ods select all;

proc freq data=testresult;
	tables reject;
run;



***********************************************************************************;
*** Question C;
*** A random sample of children who came in to an ince cream shop were asked....
*** 1. Find the proportion of girls and boys who like chocolate ice creams
*** 2. Perform a permutation test...
***********************************************************************************;


***********************************************************************************;
*** Question C.1 Solution
***********************************************************************************;

data permu; 
	do i=1 to 95;
		if i le 50 then group="f"; 
		else group="m";

		if i le 40 		then y=1;
		else if i le 50 then y=0;
		else if i le 80 then y=1;
		else if i le 95 then y=0;
		output;
	end;
	title 'Question C.1';
run;

ods select none;

proc ttest data=permu;
	ods output ttests=ttests;
  	class group;
   	var y;
run;

ods select all;

proc print data=ttests;
run; 

***********************************************************************************;
*** Question C.2 Solution
***********************************************************************************;

/* reorder 1000 times*/
data permu2;
   	set permu;
   	do rep=1 to 1000;
      	x=ranuni(112233);
		output;
   	end;
	title 'Question C.2';
run;

proc sort data=permu2;
	by rep x;
run;

data permu3;
   	set permu2;
   	by rep x;

   	if first.rep then member=1; 
	else member+1;

   	if member le 50 then group="f"; 
	else group="m";
run;

ods select none;  

/*do 2 sample ttest for each replicate */
proc ttest data=permu3; 
  	by rep;
  	var y;
  	class group ;
  	ods output ttests=testresult;
run;

/* Pooled method */
data permu4;
   	set testresult;
   	if method="Pooled";
run;

/* get a distribution of tvalue */
proc univariate data=permu4 freq; 
   	var tvalue;
run;

/*find extreme tvalue compared to the original tvalue*/
data permu5;
   	set permu4;
   	if abs(tvalue) ge 1.47 then extreme=1; else extreme=0; 
   	run;

/*the percentage of extreme=1 is the pvalue */
proc means data=permu5 ; 
  	var extreme;
  	output out=meanout mean=pvalue; 
run;

ods select all;

data result;
  	set meanout;
  	if pvalue > 0.05 then significant= "NO";
  	else significant="YES";
  	keep pvalue significant;
run;

proc print data= result;
run;


***********************************************************************************;
*** Question D;
*** 1. Find the least-squares regression line for predicting batting average from...
*** 2. Boostrap the regression line, and give a 95% confidence interval for the..
***********************************************************************************;


***********************************************************************************;
*** Question D.1 Solution
***********************************************************************************;

 /* Import Data form S Drive */
PROC IMPORT OUT= originalData
            DATAFILE= "S:\course\Bios532\2014\ta18_002.csv" 
            DBMS=CSV REPLACE;
     		GETNAMES=YES;
     		DATAROW=2; 
			
RUN;


/*fit the regression line for aver from sal*/
proc reg data=originalData;  
	model aver=sal;
	output out=processedData r=e p=pred; 
	title 'Question D.1';
run;



***********************************************************************************;
*** Question D.2 Solution
***********************************************************************************;

/*get the original sample of residuals to bootstrap*/
data bootstrap; 
	set processedData;
	keep e;
	title 'Question D.2';
run;

/* bootstrap residuals of size n=50 with 1000 replicates */
proc surveyselect data=bootstrap 
	  out= bootstrapResult 
	  n=50 
	  method=urs 
	  seed=112233
	  rep=1000;
run;

/* List strapped residuals separetly */
data samples; 
	set bootstrapResult;
	do i=1 to numberhits;
 		output;
	end;
	drop numberhits i;
run;

/*get 1000 replicates of the original data of sal aver predictedaver (fixed sal) */
data boot;   
	set processedData;
	do replicate=1 to 1000;
 		output;
	end;
	drop e;
run;

proc sort data=boot;
	by replicate;
run;

/*Add the bootstraped residuals to the fitted values of the original */
data fullsamples; 
	set boot; 
	set samples;
	aver2=pred+e;
run;

ods select none;

/*using the aver2 to regress on sal to obtain the bootstrap estimate beta1*/
proc reg data=fullsamples; 
	model aver2=sal;
	by replicate;
	ods output parameterestimates=beta;
run;

data beta1;
 	set beta;
 	if variable="sal"; 
run;

ods select all;

ods graphics off;

/*get the boostrap percentile (2.5%,97.5% )interval of beta1*/
proc univariate data=beta1; 
	var estimate;
	histogram estimate / normal; 
	output out=stats pctlpre=beta1_ pctlpts=2.5 97.5 pctlname=lower upper;
run;

proc print data=stats; 
run;
