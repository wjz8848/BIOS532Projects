***********************************************************************************;
*** BIOS 532;
*** Project 1;
*** Student: Jingzhi Wang;
*** Emory ID: 1587727;
***********************************************************************************;

***********************************************************************************;
*** Question A;
*** Calculate AUC using the trapezoid rule for s:\bios532\2014\crc301.mdb and the 
*** table crcblood
***********************************************************************************;


*** Import Data from MS Access mdb file to a temporary dataset ;

PROC IMPORT OUT= Work.crcblood
			DATATABLE= "CRCBLOOD" 
			DBMS=ACCESS REPLACE;
     		DATABASE="S:\course\Bios532\2014\crc301.mdb"; 
		SCANMEMO=YES;
		USEDATE=NO;
		SCANTIME=YES;
RUN;

/* sort data by id and visitno */
proc sort data= Work.crcblood; 
	by id visitno;
run;

/* Calculate the AUC */
data Work.questionA;
	set Work.crcblood;

	array ins(5) ins0 ins30 ins60 ins90 ins120;
	array minutes(5) (0,30,60,90,120);
	auc=0;

	/* Loop thru the time period */
	do i=2 to 5;
		/* Apply the trapezoid rule */
		auc = auc+0.5*(minutes(i)-minutes(i-1))*(ins(i)+ins(i-1));
	end;

	keep id visitno ins0 ins30 ins60 ins90 ins120 auc;
run;

/* Print out the result */
proc print;
	title "Question A Output";
run;





***********************************************************************************;
*** Question B;
*** The corrected trapezoid rule function with a macro approach
***********************************************************************************;

*** macro method is created;
%macro correctedTrapezoidAUC (fx,fdx,c,d,n);
data Work.questionB;

	x = &c;
	y0 = &fx;
	dy0 = &fdx;
	diff=(&d-&c)/&n;

	array y(*) y0-y&n ;
	array dy(*) dy0-dy&n;

	auc = 0;
	/* Stepwise calculate the AUC and add to the sum */
  	do i= 1 to &n;
    	x = x + diff;
    	y(i+1) = &fx;
		dy(i+1) = &fdx;
		auc = auc + diff/2 * (y(i+1) + y(i)) + (diff**2) / 12 * (dy(i+1) + dy(i));
	end;
run;

proc print data= Work.questionB noobs;
	var auc;
	title "Question B Ouput - &fx with &n divides";
run;

%mend;

*** f(x) = exp(-x) is tested with 20 divides and 200 divides;
%correctedTrapezoidAUC(exp(-x), -exp(-x), 0, 1, 20);
%correctedTrapezoidAUC(exp(-x), -exp(-x), 0, 1, 200);

*** f(x) = log(x) is tested with 20 divides and 200 divides;
%correctedTrapezoidAUC(log(x), 1/x, 1, 3, 20);
%correctedTrapezoidAUC(log(x), 1/x, 1, 3, 200);

*** In theory, smaller divides we test, the better result we will get;






***********************************************************************************;
*** Question C;
*** ZAR on multiple comparisons for proportions
***********************************************************************************;

*** macro method is created;
%macro compare(ngroups);

data Work.questionC;
	/* need to be adjusted if input data is not named as 'input'*/
	set input;
	pi=constant('pi');
	p=0.5*(arsin(sqrt(x/(n+1)))*180/pi)+0.5*(arsin(sqrt((x+1)/(n+1)))*180/pi);
run;

proc print;
run;

data tukey;
 	array ns(&ngroups) n1-n&ngroups;
 	array ps(&ngroups) p1-p&ngroups;
  	do i=1 to &ngroups;
  		set questionC;
 		ns(i)=n;
		ps(i)=p;
	end;

	/* Calculate critical value at 95% confidence level */
	q0=probmc("range",.,0.95,.,&ngroups);

	/* Loop thru all the groups */
    do i=1 to (&ngroups-1);
		/* Loop thru the remaining groups */
    	do j=i+1 to &ngroups;
			diff=abs(ps(i)-ps(j));
			se=sqrt((410.35/(ns(i)+0.5))+(410.35/(ns(j)+0.5)));
			q=diff/se;

			if q>q0 then sig='***';else sig='  ';
			group1=i;
			group2=j;
			output;
	    end;
	end;

	/* Create labels for variables */
	label diff = 'Difference'
			se = 'Standard Error'
			q = 'Test Value qs'
			q0 = 'Critical Value q0'
			sig = 'Significance';
run;

proc print label noobs;
	var group1 group2 diff se q q0 sig;
	title "Question C Output";
run;

%mend;

*** Set the initial data in 'input';
data input;
input group x n;
cards;
1 32 87
2 43 108
3 16 80
4 9  25
;
run;

*** Use macro method to test the input dataset;
%compare (4);
