proc import out = work.wine_all
			datafile = "/home/u45718890/sasuser.v94/Regresine_analize/WineQT.csv" 
    replace;
    getnames = yes;
run;

data work.wine_all;
	set work.wine_all;
	drop Id;
run;

proc sgplot data = work.wine_all;
	histogram alcohol;
run;

/* Pradinė analizė */

proc corr data = work.wine_all plots(maxpoints = 15000)=matrix(histogram);
	var alcohol 'fixed acidity'n 'volatile acidity'n;
quit;

proc corr data = work.wine_all plots(maxpoints = 15000)=matrix(histogram);
	var alcohol 'citric acid'n 'residual sugar'n;
quit;

proc corr data = work.wine_all plots(maxpoints = 15000)=matrix(histogram);
	var alcohol 'chlorides'n 'free sulfur dioxide'n;
quit;

proc corr data = work.wine_all plots(maxpoints = 15000)=matrix(histogram);
	var alcohol 'total sulfur dioxide'n density;
quit;

proc corr data = work.wine_all plots(maxpoints = 15000)=matrix(histogram);
	var alcohol pH sulphates;
quit;

proc sort data = work.wine_all
			out = work.wine_all_srt;
	by quality;
run;quit;

proc boxplot data=work.wine_all_srt;
   plot alcohol*quality;
run;

/* Dalinimas į mokymo ir testinę aibę */

proc surveyselect data=work.wine_all rate=.2 outall
						out=split seed=100;
run;

data work.wine;
	set work.split;
	where selected = 0;
	drop selected;
run;

data work.wine_test;
	set work.split;
	where selected = 1;
	drop selected;
run;

proc sgplot data = work.wine;
	histogram alcohol;
run;


/* GAMMA modelis, skirtingos link funkcijos */

proc genmod data = work.wine plots=(cooksd stdreschi);
   class quality;
   model alcohol = 'fixed acidity'n 'volatile acidity'n  
   					'citric acid'n 'residual sugar'n 
   					'chlorides'n 'free sulfur dioxide'n
   					'total sulfur dioxide'n density
   					pH sulphates quality
   				/ dist=gamma
                  link=log
                  type3;
run;

proc genmod data = work.wine plots=(cooksd stdreschi);
   class quality;
   model alcohol = 'fixed acidity'n 'volatile acidity'n  
   					'citric acid'n 'residual sugar'n 
   					'chlorides'n 'free sulfur dioxide'n
   					'total sulfur dioxide'n density
   					pH sulphates quality
   				/ dist=gamma
                  link=power(-1)
                  type3;
run;

proc genmod data = work.wine plots=(cooksd stdreschi);
   class quality;
   model alcohol = 'fixed acidity'n 'volatile acidity'n  
   					'citric acid'n 'residual sugar'n 
   					'chlorides'n 'free sulfur dioxide'n
   					'total sulfur dioxide'n density
   					pH sulphates quality
   				/ dist=gamma
                  link=identity
                  type3;
    output out=wine_res stdreschi=residuals;
run;

/* Šalinam išskirtis */
data work.wine1;
	set work.wine_res;
	where residuals between -3.5 and 3.5;
	drop residuals;
run;

proc genmod data = work.wine1 plots=(cooksd stdreschi);
   class quality;
   model alcohol = 'fixed acidity'n 'volatile acidity'n  
   					'citric acid'n 'residual sugar'n 
   					'chlorides'n 'free sulfur dioxide'n
   					'total sulfur dioxide'n density
   					pH sulphates quality
   				/ dist=gamma
                  link=power(-1)
                  type3;
run;


/* Bandom siaurinti Y kitimo sritį */

data work.wine2;
	set work.wine;
	where alcohol < 13;
run;quit;

proc genmod data = work.wine2 plots=(cooksd stdreschi);
   class quality;
   model alcohol = 'fixed acidity'n 'volatile acidity'n  
   					'citric acid'n 'residual sugar'n 
   					'chlorides'n 'free sulfur dioxide'n
   					'total sulfur dioxide'n density
   					pH sulphates quality
   				/ dist=gamma
                  link=power(-1)
                  type3;
run;

/* Tikriname multikolinearuma */
proc corr data = work.wine;
quit;

/* O kaip čia vif skaičiuot??? */

/* Be fixed acidity */
proc genmod data = work.wine2 plots=(cooksd stdreschi);
   class quality;
   model alcohol = 'volatile acidity'n  
   					'citric acid'n 'residual sugar'n 
   					'chlorides'n 'free sulfur dioxide'n
   					'total sulfur dioxide'n density
   					pH sulphates quality
   				/ dist=gamma
                  link=power(-1)
                  type3;
    output out=wine_res2 stdreschi=residuals;
run;

/* Šalinam išskirtis modelyje be fixed acidity */

data work.wine3;
	set work.wine_res2;
	where residuals between -3.5 and 3.5;
	drop residuals;
run;

proc genmod data = work.wine3 plots=(cooksd stdreschi);
   class quality;
   model alcohol = 'volatile acidity'n  
   					'citric acid'n 'residual sugar'n 
   					'chlorides'n 'free sulfur dioxide'n
   					'total sulfur dioxide'n density
   					pH sulphates quality
   				/ dist=gamma
                  link=power(-1)
                  type3;
run;


/* IG modelis, skirtingos link funkcijos */

proc genmod data = work.wine plots=(cooksd stdreschi);
   class quality;
   model alcohol = 'fixed acidity'n 'volatile acidity'n  
   					'citric acid'n 'residual sugar'n 
   					'chlorides'n 'free sulfur dioxide'n
   					'total sulfur dioxide'n density
   					pH sulphates quality
   				/ dist=igaussian
                  link=log
                  type3;
run;

proc genmod data = work.wine plots=(cooksd stdreschi);
   class quality;
   model alcohol = 'fixed acidity'n 'volatile acidity'n  
   					'citric acid'n 'residual sugar'n 
   					'chlorides'n 'free sulfur dioxide'n
   					'total sulfur dioxide'n density
   					pH sulphates quality
   				/ dist=igaussian
                  link=power(-1)
                  type3;
run;

proc genmod data = work.wine plots=(cooksd stdreschi);
   class quality;
   model alcohol = 'fixed acidity'n 'volatile acidity'n  
   					'citric acid'n 'residual sugar'n 
   					'chlorides'n 'free sulfur dioxide'n
   					'total sulfur dioxide'n density
   					pH sulphates quality
   				/ dist=igaussian
                  link=identity
                  type3;
    output out=wine_res stdreschi=residuals;
run;

/* Šalinam išskirtis */
data work.wine1;
	set work.wine_res;
	where residuals between -3.5 and 3.5;
	drop residuals;
run;

proc genmod data = work.wine1 plots=(cooksd stdreschi);
   class quality;
   model alcohol = 'fixed acidity'n 'volatile acidity'n  
   					'citric acid'n 'residual sugar'n 
   					'chlorides'n 'free sulfur dioxide'n
   					'total sulfur dioxide'n density
   					pH sulphates quality
   				/ dist=igaussian
                  link=power(-1)
                  type3;
run;


/* Bandom siaurinti Y kitimo sritį */

data work.wine2;
	set work.wine;
	where alcohol < 13;
run;quit;

proc genmod data = work.wine2 plots=(cooksd stdreschi);
   class quality;
   model alcohol = 'fixed acidity'n 'volatile acidity'n  
   					'citric acid'n 'residual sugar'n 
   					'chlorides'n 'free sulfur dioxide'n
   					'total sulfur dioxide'n density
   					pH sulphates quality
   				/ dist=igaussian
                  link=power(-1)
                  type3;
run;

/* Tikriname multikolinearuma */
proc corr data = work.wine;
quit;

/* O kaip čia vif skaičiuot??? */

/* Be fixed acidity */
proc genmod data = work.wine2 plots=(cooksd stdreschi);
   class quality;
   model alcohol = 'volatile acidity'n  
   					'citric acid'n 'residual sugar'n 
   					'chlorides'n 'free sulfur dioxide'n
   					'total sulfur dioxide'n density
   					pH sulphates quality
   				/ dist=igaussian
                  link=power(-1)
                  type3;
    output out=wine_res2 stdreschi=residuals;
run;

/* Šalinam išskirtis modelyje be fixed acidity */

data work.wine3;
	set work.wine_res2;
	where residuals between -3.5 and 3.5;
	drop residuals;
run;

proc genmod data = work.wine3 plots=(cooksd stdreschi);
   class quality;
   model alcohol = 'volatile acidity'n  
   					'citric acid'n 'residual sugar'n 
   					'chlorides'n 'free sulfur dioxide'n
   					'total sulfur dioxide'n density
   					pH sulphates quality
   				/ dist=igaussian
                  link=power(-1)
                  type3;
run;

proc genmod data = work.wine3;
   class quality;
   model alcohol = 'volatile acidity'n  
   					'citric acid'n 'residual sugar'n 
   					'chlorides'n 'free sulfur dioxide'n
   					'total sulfur dioxide'n density
   					pH sulphates quality
   				/ dist=igaussian
                  link=power(-1)
                  type3;
run;












