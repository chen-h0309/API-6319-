/* # This do file is to be sumbitted for: API 6319 Marked Lab 
   # Should install packages: coefplot*/

*****************************Data Cleaning**************************************
use "/Users/erin/Downloads/GSS 2013 social networks v 11.dta",clear
recode incm (1=0)(2=2500)(3=7500)(4=12500)(5=17500)(6=25000)(7=35000)(8=45000)(9=55000)(10=65000)(11=85000)(12=100000),gen(income)
tab income,m
drop if income==.b 
drop if income==.c
drop if income==.d
tab wet_110,m
drop if wet_110==.a
drop if wet_110==.c
drop if wet_110==.b
drop if wet_110==.d
tab wkwehrc,m
drop if wkwehrc==.a
drop if wkwehrc==.b
drop if wkwehrc==.c 
drop if wkwehrc==.d 
sum income wet_110 wkwehrc 

*************************Recode and Descriptive Statistics*************************

/*Recode,gen new variables*/
recode incm (1=0)(2=2500)(3=7500)(4=12500)(5=17500)(6=25000)(7=35000)(8=45000)(9=55000)(10=65000)(11=85000)(12=100000),gen(income)
gen weekly_income=income/wet_110
gen hourly_wage=weekly_income/wkwehrc
gen lnhwage=log(hourly_wage)
codebook lnhwage
hist lnhwage
recode brthregc (1/3=1 "Born in Canada")(4/6=2 "Born outside Canda"),gen(Immigrant_Status) 
codebook ehg_all
recode ehg_all (1=9)(2=12)(3/4=13)(5=14)(6=16)(7=18),gen(years_of_schooling)
recode yrarri(.=0 "canborn")(1/5=1 "pre 1974")(6/8=2 "1975-1989")(9/10=3 "1990-1999")(11=4 "2000-2004")(12=5 "2005-2009")(13=6 "2010-2013"),gen(years_of_residency)
recode lanhsdc (1/2=1 "Official Language")(3/4=2 "Other Language"),gen(language) 

/*Descriptive Statistics*/
tab years_of_residency if Immigrant_Status==2 [aw=wght_per]
tab ethnic7 if Immigrant_Status==2[aw=wght_per],row nofreq
tab lip_10 if Immigrant_Status==2 [aw=wght_per]
tab years_of_schooling if Immigrant_Status==2 [aw=wght_per]
summarize lnhwage if Immigrant_Status==1,detail
summarize lnhwage if Immigrant_Status==2,detail
ttest lnhwage, by(Immigrant_Status)

/*Scatterplot*/
gen randunm=uniform()
twoway(scatter lnhwage years_of_schooling if (Immigrant_Status==1&randunm<.01))(scatter lnhwage years_of_schooling if (Immigrant_Status==2&randunm<.01))(lfit lnhwage years_of_schooling if (Immigrant_Status==1&randunm<.01))(lfit lnhwage years_of_schooling if (Immigrant_Status==2&randunm<.01))

/*Dotplot: distribution*/
gen rand=uniform()
dotplot lnhwage if rand<.01,over(Immigrant_Status) nx(25) ny(10)center median bar 


*****************Regression Analysis************************************************

/*Restrict the sample that include those who report work more than 40 weeks last year */
drop if wet_110<40 
/*Basic Regression*/
reg lnhwage years_of_schooling if Immigrant_Status==1
reg lnhwage years_of_schooling if Immigrant_Status==2
reg lnhwage years_of_schooling if years_of_residency==1
reg lnhwage years_of_schooling if years_of_residency==2
reg lnhwage years_of_schooling if years_of_residency==3
reg lnhwage years_of_schooling if years_of_residency==4
reg lnhwage years_of_schooling if years_of_residency==5
reg lnhwage years_of_schooling if years_of_residency==6
reg lnhwage years_of_schooling if brthregc==4
reg lnhwage years_of_schooling if brthregc==5


***************Intepretation and Post-estimation************************************

/*Coefplot: Rate of return to education*/
reg lnhwage years_of_schooling if years_of_residency==1
estimate store a
reg lnhwage years_of_schooling if years_of_residency==2
estimate store b
reg lnhwage years_of_schooling if years_of_residency==3
estimate store c
reg lnhwage years_of_schooling if years_of_residency==4
estimate store d
reg lnhwage years_of_schooling if years_of_residency==5
estimate store e
reg lnhwage years_of_schooling if years_of_residency==6
estimate store f
coefplot a b c d e f,eform vertical drop(_cons)recast(bar)barwidth(.1)fcolor(*.5)ciopts(recast(rcap))citopcitype(logit)

/*Prediction: marginplot*/

/*Prediction:by native born and foreign born*/
reg lnhwage years_of_schooling if Immigrant_Status==1
margins, at(years_of_schooling=(9(2)18 ))
marginsplot,saving(plot_a)
reg lnhwage years_of_schooling if Immigrant_Status==2
margins, at(years_of_schooling=(9(2)18 ))
marginsplot,saving(plot_b)
gr combine plot_a.gph plot_b.gph
margins, expression(exp(predict(xb))*exp((`e(rmse)'^2)/2))at(years_of_schooling=(9(2)18 ))
margins, expression(exp(predict(xb))*exp((`e(rmse)'^2)/2))at(years_of_schooling=(9(2)18 ))

/*Prediction:by years of residency*/
reg lnhwage years_of_schooling if years_of_residency==1
margins, at(years_of_schooling=(9(2)18 ))
marginsplot,saving(plot_X)
reg lnhwage years_of_schooling if years_of_residency==2
margins, at(years_of_schooling=(9(2)18 ))
marginsplot,saving(plot_Y)
reg lnhwage years_of_schooling if years_of_residency==3
margins, at(years_of_schooling=(9(2)18 ))
marginsplot,saving(plot_Z)
reg lnhwage years_of_schooling if years_of_residency==4
margins, at(years_of_schooling=(9(2)18 ))
marginsplot,saving(plot_O)
reg lnhwage years_of_schooling if years_of_residency==5
margins, at(years_of_schooling=(9(2)18 ))
marginsplot,saving(plot_P)
reg lnhwage years_of_schooling if years_of_residency==6
margins, at(years_of_schooling=(9(2)18 ))
marginsplot,saving(plot_Q)
gr combine plot_X.gph plot_Y.gph plot_Z.gph plot_O.gph plot_P.gph plot_Q.gph 
