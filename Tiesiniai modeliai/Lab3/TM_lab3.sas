proc import out = work.movies_temp
			datafile = "/home/u45718890/sasuser.v94/2014 and 2015 CSM dataset.xlsx"
			dbms = xlsx
    replace;
    getnames = yes;
run;

data work.movies;
	set work.movies_temp;
	length gen $15;
	format gross 10.5;
	if Genre in (4,6,7) then delete;
	if cmiss(of Genre Ratings Gross) then delete;
	if Genre = 1 then gen = "Veiksmo";
	if Genre = 2 then gen = "Nuotykių";
	if Genre = 3 then gen = "Drama";
	if Genre = 8 then gen = "Komedija";
	if Genre = 9 then gen = "Biografinis";
	if Genre = 10 then gen = "Kriminalinis";
	if Genre = 12 then gen = "Animacinis";
	if Genre = 15 then gen = "Siaubo";
	Gross = gross / 1000000;
	if gross > 400 then delete;
	keep Movie Ratings Gross Budget Screens Views Likes Dislikes	
		Comments gen Year;
run;

proc glm data = work.movies;
	class Gen;
	model Ratings = Gen Year Dislikes Likes Comments Gross Budget Screens/ ss3 solution;
quit;

/* Tikriname, ar lygus koeficientai */
proc glm data = work.movies;
	class Gen;
	model Ratings = Gen gross Gen * gross/ ss3 solution;
quit;

/* Kuriamas modelis */
proc glm data = work.movies;
	class Gen;
	model Ratings = Gen gross/ ss3 solution;
	lsmeans Gen / stderr pdiff cov out=adjmeans;
	output out=rez residual=liekanos;
quit;

/*Tikrinamas normalumas */
proc univariate data=rez normal;
	var liekanos;
run;


/* Box-Cox transformacija */
proc transreg details data=work.movies ss2
              plots=(transformation(dependent) obp);
   model BoxCox(ratings / convenient lambda=-2 to 2 by 0.05) =
         qpoint(gross);
run;

/* Transformuojamas ratings */
data movies_new;
	set movies;
	Ratings = Ratings * Ratings;
run;

proc glm data = work.movies_new;
	class Gen;
	model Ratings = Gen gross/ ss3 solution;
	lsmeans Gen / stderr pdiff cov out=adjmeans;
	output out=rez residual=liekanos;
quit;

/*Tikrinamas normalumas */
proc univariate data=rez normal;
	var liekanos;
run;

/* Tikrinama dispersiju lygybe */

proc means data = work.movies_new mean;
	var gross;
quit;

data work.movies2;
	set work.movies_new;
	Z = ratings - 0.05897080 * (gross  - 65.2244707);
run;

proc glm data = movies2; 
Title 'Homogeneity of variances'; 
	Class Gen; 
	Model Z = Gen; 
	Means Gen / hovtest = Levene;
quit;

proc glm data = movies2; 
Title 'Homogeneity of variances'; 
	Class Gen; 
	Model ratings = Gen; 
	Means Gen / hovtest = Levene;
quit;

/* Galutinis modelis */
proc glm data = work.movies2;
	class Gen;
	model Ratings = Gen gross/ ss3 solution;
	lsmeans Gen / stderr pdiff cov out=adjmeans;
quit;

/* Anova */
proc glm data = work.movies2;
	class Gen;
	model Ratings = Gen/ ss3 solution;
	means Gen /tukey hovtest = levene;
quit;














