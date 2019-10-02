use "C:\Users\sxm180029\Desktop\guns.dta",clear

*giving panel state and year id
xtset stateid year

xtdescribe

cor rob mur shall incarc_rate density avginc pop pm1029 pw1064 pb1064

*since the overall maximum population will consists of black and white so if one increases other will decrease so they are highly corelated thus we are keeping only 1 variable percentage of black

*corelation after generating rate for population percentage
cor rob mur shall incarc_rate density avginc pop pm1029  pb1064

*Analyzing different variables 

***********************************************************
*Crime Rate

*1.checking the distribution of crime rate
histogram vio

*Left Skewed
*Taking log to make it normal distribution

gen lnvio=ln(vio+1)
histogram lnvio


***********************************************************
*sentenced prisoner

*1.checking the distribution of sentenced prisoners in previous year
histogram incarc_rate
*left skewed data

*Taking log to make it normal distribution
gen lnincarc_rate=ln(incarc_rate+1)

histogram lnincarc_rate

*2. Linear prediction Plot
graph twoway (lfit lnvio lnincarc_rate) (scatter lnvio lnincarc_rate)

*3. Residual Plot
rvfplot, yline(0)
*4. Panel line plot 
xtline lnincarc_rate, t(year) i(stateid) overlay
*********************************************************

***********************************************************
*per capita income

*1.checking the distribution of per capita income
histogram avginc
gen lnavginc=ln(avginc+1)
histogram lnavginc

* approx normally distributed

*2. Linear prediction Plot
graph twoway (lfit vio lnavginc) (scatter vio lnavginc)
*3. Residual Plot
rvfplot, yline(0)
*4. Panel line plot 
xtline lnavginc, t(year) i(stateid) overlay
*********************************************************


***********************************************************
*robbery rate

*1.checking the distribution of per capita income
histogram rob

*left skewed data

*Taking log to make it normal distribution
gen lnrob=ln(rob+1)
histogram lnrob

*2. Linear prediction Plot
graph twoway (lfit lnvio lnrob) (scatter lnvio lnrob)
*3. Residual Plot
rvfplot, yline(0)
*4. Panel line plot 
xtline lnrob, t(year) i(stateid) overlay
*********************************************************

***********************************************************
*mur rate

*1.checking the distribution of per capita income
histogram mur

*left skewed data

*Taking log to make it normal distribution
gen lnmur=ln(mur+1)
histogram lnmur

*2. Linear prediction Plot
graph twoway (lfit lnvio lnmur) (scatter lnvio lnmur)
*3. Residual Plot
rvfplot, yline(0)
*4. Panel line plot 
xtline lnmur, t(year) i(stateid) overlay
*********************************************************



***********************************************************
*density (population per square mile of land area, divided by 1000)

*1.checking the distribution of per capita income
histogram density

*left skewed data

*Taking log to make it normal distribution
gen lnden=ln(density+1)
histogram lnden

*2. Linear prediction Plot
graph twoway (lfit lnvio lnden) (scatter lnvio lnden)
*3. Residual Plot
rvfplot, yline(0)
*4. Panel line plot 
xtline lnden, t(year) i(stateid) overlay
*********************************************************

***********************************************************
*population (millions of people)

*1.checking the distribution of per capita income
histogram pop

*left skewed data

*Taking log to make it normal distribution
gen lnpop=ln(pop+1)
histogram lnpop

*2. Linear prediction Plot
graph twoway (lfit lnvio lnpop) (scatter lnvio lnpop)
*3. Residual Plot
rvfplot, yline(0)
*4. Panel line plot 
xtline lnpop, t(year) i(stateid) overlay
*********************************************************

***********************************************************
*pm1029 (millions of people)

*1.checking the distribution of per capita income
histogram pm1029

*approx. normal skewed data

*2. Linear prediction Plot
graph twoway (lfit lnvio pm1029) (scatter lnvio pm1029)
*3. Residual Plot
rvfplot, yline(0)
*4. Panel line plot 
xtline pm1029, t(year) i(stateid) overlay
*********************************************************

***********************************************************
*percentage of black 

*1.checking the distribution of per capita income
histogram pb1064

*left skewed data

*Taking log to make it normal distribution
gen lnpb1064=ln(pb1064+1)
histogram lnpb1064

*2. Linear prediction Plot
graph twoway (lfit lnvio lnpb1064) (scatter lnvio lnpb1064)
*3. Residual Plot
rvfplot, yline(0)
*4. Panel line plot 
xtline lnpb1064, t(year) i(stateid) overlay
*********************************************************

******HYPOTHESIS TESTING*********
1. shall law 

1.1
anova lnvio shall

1.2
anova lnden shall

1.3
anova lnavginc shall
*Not significant

1.4
anova lnpb1064 shall


**********REGRESSION ANALYSIS(PANEL DATA)****************
*MODEL1.1: with all the variables
reg lnvio lnincarc_rate shall lnpb1064 pm1029 lnpop lnden lnmur lnrob lnavginc
estat ic

*insignificant variables
*lnavginc

*MODEL1.2: with robust standard error since running pooled in panel data is heteroescdacity
reg lnvio lnincarc_rate shall lnpb1064 pm1029 lnpop lnden lnmur lnrob lnavginc, vce(cluster stateid)
estat ic

*We see that coefficients are same but for most of the variables the satndard errors and p values changed
*thus proceeding with robust standard error method
*insignificant Variables: shall pm1029 lnpop lnden lnavginc

*MODEL1.3 : AFTER REMOVING INSIGNIFICANT VARIABLES
reg lnvio lnincarc_rate lnpb1064 lnmur lnrob , vce (cluster stateid)
estat ic

*MODEL1.4 : AFTER REMOVING INSIGNIFICANT VARIABLES BUT WITH SHALL SINCE WE HAVE TO ANALYZE ITS EFFECT
reg lnvio shall lnincarc_rate lnpb1064 lnmur lnrob , vce (cluster stateid)
estat ic

************************************************************************************
*MODEL2.1: FIXED AFFECT MODEL ON ALL THE VRIABLES(TO CAPTURE THE EFECTS OF DIFFERENT STATES)
xtreg lnvio lnincarc_rate shall lnpb1064 pm1029 lnpop lnden lnmur lnrob lnavginc, fe 
estat ic
estimates store fixed2_1

*INSIGNIFICANT VARIABLES: shall lnden lnavginc

*MODEL 2.2 : with robust standard error
xtreg lnvio lnincarc_rate shall lnpb1064 pm1029 lnpop lnden lnmur lnrob lnavginc, fe vce (cluster stateid)
estat ic
estimates store fixed2_2

*insignificant variables: shall lnpb1064 lnincarc_rate lnpop lnden lnavginc

*MODEL 2.3 : REMOVING ALL INSIGNIFICANT VARIABLES
xtreg lnvio lnpm1029 lnmur lnrob , fe vce (cluster stateid)
estat ic
estimates store fixed2_3

*MODEL 2.4: Using shall and REMOVING ALL other INSIGNIFICANT VARIABLES
xtreg lnvio shall lnpm1029 lnmur lnrob , fe vce (cluster stateid)
estat ic
estimates store fixed2_4

*MODEL 3.1: MODEL WITH ENTITY AND TIME FIXED EFFECT
xtreg lnvio lnincarc_rate shall lnpb1064 pm1029 lnpop lnden lnmur lnrob lnavginc i.year, fe 
estat ic
estimates store fixed3_1

*insignificant variables: lnincarc_rate lnpb1064 lnavginc

*FTEST TO CHECK THE SIGNIFICANCE OF YEAR
testparm i.year
*yes significant 

*MODEL 3.2: MODEL WITH ENTITY AND TIME FIXED EFFECT with robust standard error
xtreg lnvio lnincarc_rate shall lnpb1064 pm1029 lnpop lnden lnmur lnrob lnavginc i.year, fe vce(cluster stateid)
estat ic
estimates store fixedtime3_2
*insignificant variables: lnincarc_rate shall lnpb1064 lnpop lnden lnmur  lnavginc

*MODEL 3.3: MODEL WITH ENTITY AND TIME FIXED EFFECT and removing insignificant variables
xtreg lnvio pm1029 lnrob i.year, fe vce (cluster stateid)
estat ic
estimates store fixed3_3

*MODEL 3.4: MODEL WITH ENTITY AND TIME FIXED EFFECT with shall and removing other insignificant variables
xtreg lnvio pm1029 lnrob shall i.year, fe vce (cluster stateid)
estat ic
estimates store fixed3_4


*RUNNING RANDOM EFFECTS MODEL
*MODEL 4.1: MODEL WITH RANDOM EFFECTS
xtreg lnvio lnincarc_rate shall lnpb1064 pm1029 lnpop lnden lnmur lnrob lnavginc , re
estat ic
estimates store random4_1

*HAUSMAN TEST ON FIXED AND RANDOM MODEL FOR ALL THE VARIABLES

hausman fixed2_1 random4_1
*significant (we reject the null hypothesis thus will use fixed effect only)

*HAUSMAN TEST ON FIXED TIME AND RANDOM MODEL FOR ALL THE VARIABLES

hausman fixed3_1 random4_1
*significant (we reject the null hypothesis thus will use fixed effect only)















