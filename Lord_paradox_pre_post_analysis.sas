
*************************************************************************
* Simulate data for pre-test with two groups                            *
* Mean score for group one: 160                                         *
* Mean score for group two: 150                                         *
*************************************************************************; 

data pre_one;
/*		call streaminit(123);*/
		do i = 1 to 10000;
		x = rand("Normal", 160, 20); 
		id=i;
		group=1;
		output;
		end;
run;

data pre_two;
/*		call streaminit(123);*/
		do i = 1 to 10000;
		x = rand("Normal", 150, 20); 
		id=i*2;
		group=2;
		output;
		end;
run;

data pre;
       set pre_one pre_two;
run;



*************************************************************************
* Simulate data for post-test with two groups                           *
* Mean score for group one: 120                                         *
* Mean score for group two: 110                                         *
*************************************************************************; 
data post_one;
/*		call streaminit(123);*/
		do i = 1 to 10000;
		x = rand("Normal", 120, 20); 
		id=i;
		group=1;
		output;
		end;
run;

data post_two;
/*		call streaminit(123);*/
		do i = 1 to 10000;
		x = rand("Normal", 110, 20); 
		id=i*2;
		group=2;
		output;
		end;
run;

data post;
		set post_one post_two;
run;


proc sql noprint;
      create table pre_post as select a.x as pre, a.id as id, a.group as group, b.x as post, post-pre as diff
	         from pre as a, post as b
			 where a.id=b.id and a.group=b.group;
quit; 

proc sort data=pre_post; by group; run;

proc summary data=pre_post;
      var post ;
	  by group;
	  output out=sum n=n mean=mean std=std;
run;




*************************************************************************
* Analysis by fisrt statistician                                        *
* t test                                                                *
*************************************************************************; 
proc ttest data=pre_post ;
   class group;
   var diff;
   title 't test for mean difference';
run;


*************************************************************************
* Analysis by second statistician                                       *
* ANCOVA                                                                *
*************************************************************************; 
proc glm data=pre_post;
      class group;
	  model diff=pre group / solution;
      lsmeans group / diff stderr pdiff cl cov;
run;




*************************************************************************
* Advanced method                                                       *
* MMRM                                                                  *
*************************************************************************; 
proc mixed data=pre_post method=reml covtest;
      class id group;
	  model diff=pre group  / noint cl ddfm=KR;
	  repeated / sub=id type=un;
	  lsmeans group /  diff pdiff cl e;
run;
