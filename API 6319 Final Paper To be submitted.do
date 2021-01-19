/*Read me file
#Date Created:4 November,2020
               #Date last modified:
			   #Should Install: coefplot package
			   Note: This do fils is for explore and practice 2017-18 Community Health Survey*/	   
***Data Cleaning***
use "/Users/erin/Desktop/December 3 Final Paper 32%/Data/cchs-82M0013-E-2017-2018-Annual-component/cchs-82M0013-E-2017-2018-Annual-component_F1.dta"
tab INCG015,nol 
recode FSCDVHF2 (0=0 "Food Secure") (1/3=1 "Food Insecure"),gen(FS)
recode INCG015 (1=1 "Wages and Employment Income")(2=2"EI Work Compensation and Provincial Social Assistance")(3=3"Pension")(4=4"Child Benefit"),gen(Income_source)
tab GEO_PRV,nol
tab GEO_PRV
recode GEO_PRV (10/13=1 "Atlantic Canada") (24=2 "Quebec")(35=3 "Ontario")(46/48=4 "Western Canada")(59=5 "B.C.")(60/62=6 "Territorities"),gen(region) 
drop if FS==.d 
recode DHHDGLVG (1/3=1 "No Children") (4=2 "Married&Children")(5/6=3 "lone-Parent")(7=2 "Married&Children")(8=4 "Others"),gen (household)
drop if household==.d
/*Missing Value*/
recode variable (.=0), gen(newvar)
/*Recode*/
gen gender=household
replace gender=5 if household==3&DHH_SEX==1
tab household DHH_SEX
label var gender "gender"
label define label 1 "No Children" 2 "Married and Children" 3 "Female lone Parent" 4 "Others" 5 "Male lone parent" 
label values gender label 
rename gender household_structure
/*Recode Health Region See Attachment*/
***Descriptive Statistics***
tab region FSCDVHFS [aw=WTS_M],row nofreq 
tab Income_source FSCDVHFS[aw=WTS_M],row nofreq
tab household FSCDVHFS[aw=WTS_M],row nofreq
tab household FSCDVHFS[aw=WTS_M] if DHH_SEX==2,row nofreq
tab household FSCDVHFS[aw=WTS_M] if DHH_SEX==1,row nofreq
tab GEODGHR4 FS [aw=WTS_M],row nofreq
tab GEODGHR4 INCDVRRS [aw=WTS_M],row nofreq
tab GEODGHR4 SDCDGCGT [aw=WTS_M],row nofreq
tab GEODGHR4 household [aw=WTS_M],row nofreq
tab GEODGHR4 INCG015 [aw=WTS_M],row nofreq
tab GEODGHR4 INCDGHH [aw=WTS_M],row nofreq
tab peer_group FS[aw=WTS_M],row nofreq 
tab peer_group INCDGHH [aw=WTS_M],row nofreq
tab peer_group household [aw=WTS_M],row nofreq
tab region FS[aw=WTS_M]
tab peer_group FS[aw=WTS_M]
tab Income_source if FS==1&LBFDVWSS>2 [aw=WTS_M]
tab Income_source if FS==1&LBFDVWSS<2 [aw=WTS_M]
***Regression***
/*Model 1 */
logit FS ib5.INCDGHH ib2.region ib3.EHG2DVH3 i.DHH_OWN ib7.peer_group i.Income_source ib2.gender_loneparent ib2.SDC_015 i.SDCDGCB,allbaselevel 
coefplot,xline(0)drop(_cons)keep(*.INCDGHH *.Income_source *.DHH_OWN *.EHG2DVH3 *.gender_loneparent *.SDC_015)omitted baselevels recast(bar)ciopts(recast(rcap))citop barwidt(0.5)color(*.6)
coefplot,xline(0)drop(_cons)keep(*.peer_group *.region)omitted baselevels recast(bar)ciopts(recast(rcap))citop barwidt(0.5)color(*.6)
/*Model 2*/
logit FS ib5.INCDGHH ib3.EHG2DVH3 i.Income_source i.DHH_OWN ib2.gender_loneparent ib3.SMK_005 ib3.ALCDVTTM ib2.SDC_ 015 i.SDCDGCB if INCDGHH>2,allbaselevel 
coefplot,xline(0)drop(_cons) keep(*.INCDGHH *.Income_source *.EHG2DVH3 *.DHH_OWN *.gender_loneparent *.SMK_005)omitted baselevels recast(bar)ciopts(recast(rcap))citop barwidt(0.5)


/*Model Test*/
logistic FS i.INCDGHH i.region i.Income_source i.DHH_SEX i.household,allbaselevel
logistic FS i.INCDGHH i.region i.Income_source i.DHH_SEX i.household ib7.peer_group,allbaselevel
logistic FS i.INCDGHH i.region i.Income_source i.DHH_SEX i.household ib7.peer_group
logistic FS i.INCDGHH i.region i.Income_source i.DHH_SEX i.household ib8.peer_group
logistic FS i.INCDGHH i.region i.Income_source i.DHH_SEX i.household ib8.peer_group,allbaselevel
logistic FS i.INCDGHH ib2.region i.Income_source i.DHH_SEX i.household ib8.peer_group,allbaselevel
logistic FS i.INCDGHH ib2.region i.Income_source i.DHH_SEX i.household ib8.peer_group,allbaselevel
logistic FS i.INCDGHH ib2.region i.Income_source i.DHHDGL12 i.LBFG10 LBFDGHPW i.DHH_OWN i.household ib7.peer_group,allbaselevel
logistic FS ib3.INCDGHH ib2.region i.Income_source i.DHHDGL12 i.LBFG10 LBFDGHPW i.DHH_OWN i.household ib7.peer_group,allbaselevel
logistic FS ib5.INCDGHH ib2.region i.Income_source i.DHHDGL12 i.LBFG10 LBFDGHPW i.DHH_OWN i.household ib7.peer_group,allbaselevel
logistic FS ib5.INCDGHH ib2.region i.Income_source i.DHHDGL12 i.LBFG10 LBFDGHPW i.DHH_OWN i.household ib5.peer_group,allbaselevel
logistic FS ib5.INCDGHH ib2.region i.Income_source i.DHHDGL12 i.LBFG10 LBFDGHPW i.DHH_OWN i.household ib6.peer_group,allbaselevel
logistic FS ib2.region ib3.INCDGHH ib3.EHG2DVH3 i.Income_source i.LBFG10 LBFDGHPW i.DHH_OWN i.household,allbaselevel
logistic FS ib3.INCDGHH ib3.EHG2DVH3 i.Income_source i.LBFG10 LBFDGHPW i.DHH_OWN i.household ib3.SMK_005 ib3.ALCDVTTM ib2.DRGDVLAC ib2.DRGDVLCM,allbaselevel
logistic FS ib3.INCDGHH ib2.EHG2DVH3 i.Income_source i.LBFG10 LBFDGHPW i.DHH_OWN i.household ib3.SMK_005 ib3.ALCDVTTM ib2.DRGDVLAC ib2.DRGDVLCM,allbaselevel
char region [omit]3
xi:logistic FS i.region 
corr region INCDGHH EHG2DVH3 DHH_OWN household SMK_005 ALCDVTTM DRGDVLA
***Post-Estimation***
margins Income_source,atmeans predict (outcome(1))
margins Income_source,atmeans predict (outcome(2))
margins SMK_005#household, plot
margins DRGDVLAC#household, plot
margins DRGDVLAC#Income_source, plot
margins Income_source#DRGDVLAC, plot
margins INCDGHH#DRGDVLAC, plot
ssc install fitstat
fitstat 
***Graph***
logit FS ib3.INCDGHH ib2.region ib3.EHG2DVH3 i.DHH_OWN ib2.urban_rural i.Income_source ib2.gender_loneparent ib2.SDC_015 i.SDCDGCB,allbaselevel 
margins,dydx(gender_loneparent DHH_OWN) atmeans
margins,dydx(gender_loneparent DHH_OWN)
margins,dydx(gender_loneparent)
margins INCDGHH##gender_loneparent
marginsplot,recast(line)
marginsplot,recast(scatter)
marginsplot,recast(line)recastci(rarea)
coefplot a, eform drop(_cons)xscale(log)xline(1,lwidth(vthin))omitted levels(99 95) msym(s)mfcolor(white)legend(order(1 "99% CI" 2 "95% CI")) ciopts(lwidth(*1 *5))
coefplot a, drop(_cons)xscale(log)xline(1,lwidth(vthin))omitted levels(99 95) msym(s)mfcolor(white)legend(order(1 "99% CI" 2 "95% CI")) ciopts(lwidth(*1 *5))
coefplot a,eform drop(_cons) xscale(log) xline(1, lwidth(vthin)) omitted levels(99 95) legend(order(1 "99% CI" 2 "95% CI"))ciopts(recast(. rcap))
coefplot a, eform drop(_cons) xscale(log) xline(1, lwidth(vthin)) omitted mlabel format(%9.2f) mlabposition(12) mlabsize(medium)
coefplot a, drop(_cons) xscale(log) xline(0, lwidth(vthin)) omitted mlabel format(%9.2f) mlabposition(12) mlabsize(medium)recast(bar)
coefplot a, drop(_cons) xscale(log) xline(0, lwidth(vthin)) omitted mlabel format(%9.2f) mlabposition(12) mlabsize(medium)
coefplot b, drop(_cons) xscale(log) xline(0, lwidth(vthin)) omitted mlabel format(%9.2f) mlabposition(12) mlabsize(medium)recast(bar)
coefplot a, drop(_cons) xscale(log) xline(0, lwidth(vthin)) omitted mlabel format(%9.2f) mlabposition(12) mlabsize(medium)recast(bar)
coefplot a, drop(_cons) xscale(log) xline(0, lwidth(vthin)) omitted mlabel format(%9.2f) mlabposition(12) mlabsize(medium)
coefplot a, drop(_cons) xscale(log) xline(1, lwidth(vthin)) omitted mlabel format(%9.2f) mlabposition(12) mlabsize(medium)
coefplot,ciopts(recast(rcap))
/*Coefplot Command*/
coefplot, xline(0, lwidth(vthin)) omitted mlabel format(%9.2f) mlabposition(12) 
