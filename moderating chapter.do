Second Empirical chapter 
Moderating impact of Institutional Qauality and Financial development

0. Original Variables based on theoretical model
0.1 Dependent variable
    Economic Growth= RYPC RYPH DGDP CGDP
0.2 Expalaintory Variables
    External debt= TDPY TDPH TDPX 
    Interaction terms= TDPY*IQ1 TDPY*IQ2 TDPYFD PTO*DCI*REIR
    Growth determinants = PFIN or CPIN FAPG HD DPOP or POPN 
0.3 Control variables= REIR DCPI PTO

1.  Cleaning data
1.1.check missing data for all data
    npresent 
1.2.for one variable 
    nmissing

	How to check the average values of a variable yearly or by each country 
	
egen avTDPY =mean(TDPY), by(year)
tabstat avTDPY, stats(count mean sd p50 min max) by(year)
egen avcTDPY =mean(TDPY), by( id_c )
tabstat avcTDPY, stats(count mean sd p50 min max) by( id_c )
	

1.3 Interaction term if needed
    gen TDPYIQ1=TDPY*IQ1
    gen TDPYIQ2=TDPY*IQ2
    gen TDPYFD=TDPY*FD
	
  gen TDPYVOA=TDPY*VOA
  gen TDPYPS =TDPY*PS
  gen TDPYGE =TDPY*GE
  gen TDPYRQ =TDPY*RQ
  gen TDPYROL =TDPY*ROL
  gen TDPYRCOC =TDPY*COC
	
   
	gen MEP= REIR*PTO*DCPI
1.4. generate year dummies
     1.4.1
     tab year, gen(yd)
	 1.4.2
	 generate A_1997G_2008C_19 = 0
     replace A_1997G_2008C_19 = 1 if tccode==1997 | tccode==2009 | tccode==2020

1.5 Principal component analysis see stata statistics then multivariate analysis then pca and other things and see postestiamtion for prediction of the index from two estimated components 
     pca VOA  PS GE RQ ROL COC
     Based on egien value greater then 1, predict them as follow
     predict pc1 pc2, score
     composit index by commulative value comp1 and comp2
     (comp1/comp2)*pc1+[(comp2-comp1)/comp2]*pc2  
     gen Q0I=(0.6632/0.8332)*pc1+[(0.8332-0.6632)/0.8332]*pc2
	 
	 Macroeconomic policy indicator by pca 
	 Monetary policy, Fiscal policy and Trade policy 
	 pca REIR DCPI PTO 
     predict pca pcb, score
gen MEP =(0.4374/0.7732)*pca+[(0.7732-0.4374)/0.7732]*pcb

shortcut for interaction terms 
    pca TDPY FD
    predict comp1 comp2, score
    use comp1 if egeine value is greater than 1
    pca TDPY IQ2
    predict compA compB, score
    use compA

2. Taking log 
2.1
Variables in log to avoid from colinearity and multiclinearity
gen lnRYPH   =log(RYPH)
gen lnCGDP   =log(CGDP)
gen lnTDPY   =log(TDPY)
gen lnTDPH   = log(TDPH)
gen lnTDPX   =log(TDPX)
gen lnTDPYFD =log(TDPYFD)
gen lnPFIN   =log(PFIN)
gen lnCPFIN  =log(CFIN)
gen lnHD     =log(HD)
gen lnFD     =log(FD)
gen lnDPOP   =log(DPOP+2)
gen lnPOPN   =ln(POPN)
gen lnREIR   =ln(REIR)
gen lnDCPI   =ln(DCPI+2)
gen lnPTO    =ln(PTO)
gen lnTDPYOC =ln(TDPYOC)
gen lnGFDTYFD=ln(GFDTYFD)
gen lnPFDTYFD=ln(PFDTYFD)
gen lnSTDTYFD=ln(STDTYFD)






2.2 in case high colinearity or multicolinearity first normal the variable then take log or take log first and then normal variable

Normalize the variables 

     norm RYPH,      by(year) method(mmx)
     norm PFIN,      by(year) method(mmx)
	 norm PTO,       by(year) method(mmx)
     norm HD,        by(year) method(mmx)
     norm FD,        by(year) method(mmx)
     norm POPN,      by(year) method(mmx)
     norm DCPI,      by(year) method(mmx)
     norm TDPY,      by(year) method(mmx)
     norm DPOP,      by(year) method(mmx)
	 norm TDPYFD,    by(year) method(mmx)
	 norm TDPYOC     by(year) method(mmx)
     norm lnRYPH,      by(year) method(mmx)
     norm lnPFIN,      by(year) method(mmx)
	 norm lnPTO,       by(year) method(mmx)
     norm lnHD,        by(year) method(mmx)
     norm lnFD,        by(year) method(mmx)
     norm lnPOPN,      by(year) method(mmx)
     norm lnDCPI,      by(year) method(mmx)
     norm lnTDPY,      by(year) method(mmx)
     norm lnDPOP,      by(year) method(mmx)
	 norm lnTDPYFD,    by(year) method(mmx)
	 norm TDPYOC       by(year) method(mmx)
	 
	 
Variables which cant express in log,can take log after transformation norm, demean, centered 
RYPC
DGDP
IQ1 
IQ2
FAPG
     These variables already in norm above just gen log below
	 norm RYPC, by(year) method(mmx)
	 gen lnRYPC =ln(mmx_RYPC+2)
	 
	 norm IQ1, by (year) method (mmx)
	 gen lnIQ1 =ln(mmx_IQ+2)
	 
     norm IQ2, by (year) method (mmx)
	 gen lnIQ2 =ln(mmx_IQ2+2) 
	 
	 norm TDPYIQ1, by (year) method (mmx)
	 gen lnTDPYIQ1 =ln(mmx_TDPYIQ1+2)	 
	 norm TDPYIQ2, by (year) method (mmx)
	 gen lnTDPYIQ2 =ln(mmx_TDPYIQ2+2)
	 
	 norm GFPYIQ1, by (year) method (mmx)
	 gen lnGFPYIQ1 =ln(mmx_GFPYIQ1+2)
	 norm GFDTYIQ2, by (year) method (mmx)
	 gen lnGFDTYIQ2 =ln(mmx_GFDTYIQ2+2)
	 
	 norm PFDTYIQ1, by (year) method (mmx)
	 gen lnPFDTYIQ1 =ln(mmx_PFDTYIQ1+2)
	 norm PFDTYIQ2, by (year) method (mmx)
	 gen lnPFDTYIQ2 =ln(mmx_PFDTYIQ2+2)
	 
	 norm STDTYIQ1, by (year) method (mmx)
	 gen lnPFDTYIQ1 =ln(mmx_STDTYIQ1+2)
	 norm STDTYIQ2, by (year) method (mmx)
	 gen lnSTDTYIQ2 =ln(mmx_STDTYIQ2+2)
	 
	 
	 norm FAPG, by(year) method(mmx)
	 gen lnFAPG =ln(mmx_FAPG+2)	 
	 
norm TDPYVOA , by (year) method (mmx)
 gen lnTDPYVOA =ln(mmx_TDPYVOA +2)
norm TDPYPS , by (year) method (mmx)
 gen lnTDPYPS =ln(mmx_TDPYPS +2)
norm TDPYGE , by (year) method (mmx)
 gen lnTDPYGE =ln(mmx_TDPYGE +2)
norm TDPYRQ , by (year) method (mmx)
 gen lnTDPYRQ =ln(mmx_TDPYRQ +2)
norm TDPYROL , by (year) method (mmx)
 gen lnTDPYROL =ln(mmx_TDPYROL +2)
norm TDPYRCOC , by (year) method (mmx)
 gen lnTDPYRCOC =ln(mmx_TDPYRCOC +2)
	 

3.Basics check
3.1 Summary statistics
    sum RYPC TDPY TDPYIQ2 TDPYFD PFIN FAPG DPOP PTO
    sum RYPC lnmmx_TDPY compA comp1 lnmmx_PFIN FAPG DPOP lnmmx_PTO
3.2 No high colinearity
    corr TDPY TDPYIQ2 TDPYFD PFIN FAPG DPOP PTO
    corr lnmmx_TDPY compA comp1 lnmmx_PFIN FAPG DPOP lnmmx_PTO
3.3 No multicolinearity variables
    xtreg RYPC TDPY TDPYIQ2 TDPYFD PFIN FAPG DPOP PTO
    vif,uncentered
    xtreg RYPC lnmmx_TDPY compA comp1 lnmmx_PFIN FAPG DPOP lnmmx_PTO
    vif, uncentered

4. Investigate cross-section dependence in all production function variables and residuals 
4.1 Individual variables pre estimation OF Pesaran (2004)
	xtcd RYPC TDPY TDPYIQ2 TDPYFD PFIN FAPG 
	xtcd DPOP PTO
	or 
	xtcdf RYPC TDPY TDPYIQ2 TDPYFD PFIN FAPG DPOP PTO
	
    xtcd RYPC lnmmx_TDPY compA comp1 lnmmx_PFIN FAPG 
	xtcd DPOP lnmmx_PTO
	or 
	xtcdf RYPC lnmmx_TDPY compA comp1 lnmmx_PFIN FAPG DPOP lnmmx_PTO
	
4.2. Pre estimation of residuals
	4.2.1. Compute the residuals from an OLS production function with time fixed effects and test the residuals for cross-section dependence.
	
	reg RYPC lnmmx_TDPY compA comp1 lnmmx_PFIN FAPG DPOP lnmmx_PTO i.year
    predict ols_res if e(sample), res
    xtcd ols_res, resid
	4.2.2. Compute the residuals from a heterogeneous parameter production function using the Pesaran & Smith (1995) Mean Group estimator (xtmg if installed) with a
    country-specific linear trend. Then test the residuals for cross-section independence
	
	xtmg RYPC lnmmx_TDPY compA comp1 lnmmx_PFIN FAPG DPOP lnmmx_PTO, trend robust res(mg_res)
    xtcd mg_res, resid
	
	4.2.3.Compute the residuals from a heterogeneous parameter production function using the Pesaran (2006) CCE Mean Group estimator (xtmg if installed) and then test the
    residuals
	
	xtmg  RYPC lnmmx_TDPY compA comp1 lnmmx_PFIN FAPG DPOP lnmmx_PTO, cce robust res(cce_res)
    asdoc xtcd cce_res, resid
	
    4.2.4 Other way Pesaran CD tests (2015) of residuals and variables 

        reg d.log_rgdpo log_hc log_ck log_ngd
        xtcd2
		Predicting the error terms after reg, leads to the same result:
        reg d.log_rgdpo log_hc log_ck log_ngd
        predict res, residuals
        xtcd2 res
		The test statistic is 36.34 and the p-value is 0, therefore rejecting the null hypothesis of weak cross sectional dependence.
        To draw a density plot with the cross correlations the kdensity option is used:
        xtcd2 res, kdensity
		Testing the variable log_rgdpo for cross sectional dependence reads:
        xtcd2 log_rgdpo, noestimation


5. 1.Second unit root test, first generation if there is no csd, otherwise second unit root test
    Seven variables togather with multipurt 
	but variables should not have gaps 
    multipurt RYPC lnmmx_TDPY compA comp1 lnmmx_PFIN FAPG DPOP lnmmx_PTO , lags(2)
	then use original variables 
	multipurt RYPC TDPY TDPYIQ2 TDPYFD PFIN FAPG DPOP PTO
	or use 
	asdoc pescadf RYPC , lags(2)
    asdoc pescadf d.RYPC , lags(2)
	asdoc pescadf RYPH , lags(2)
    asdoc pescadf d.RYPH , lags(2)
    asdoc pescadf PFIN , lags(2)
    asdoc pescadf d.PFIN , lags(2)
    asdoc pescadf PTO , lags(2)
    asdoc pescadf d.PTO , lags(2)
    asdoc pescadf FAPG , lags(2)
    asdoc pescadf d.FAPG , lags(2)
    asdoc pescadf HD , lags(2)
    asdoc pescadf d.HD , lags(2)
    asdoc pescadf FD , lags(2)
    asdoc pescadf d.FD , lags(2)
    asdoc pescadf IQ2 , lags(2)
    asdoc pescadf d.IQ2 , lags(2)	
    asdoc pescadf DCPI , lags(2)
    asdoc pescadf d.DCPI , lags(2)
    asdoc pescadf TDPY , lags(2)
    asdoc pescadf d.TDPY , lags(2)
    asdoc pescadf DPOP , lags(2)
    asdoc pescadf d.DPOP , lags(2)
    asdoc pescadf TDPYIQ2 , lags(2)
    asdoc pescadf d.TDPYIQ2 , lags(2)
    asdoc pescadf TDPYFD , lags(2)
    asdoc pescadf d.TDPYFD , lags(2)	
	
6. If Need cointegration test, use this command with replce with your variables with changing boostrap value or others like 100
   xtwest RYPC lnmmx_TDPY compA comp1 lnmmx_PFIN FAPG DPOP lnmmx_PTO, constant trend lags(1) leads(1) lrwindow(3) bootstrap(800)
   Above variables have missing values, so use below one
   xtwest RYPC TDPY TDPYIQ2 TDPYFD PFIN FAPG, constant trend lags(0) leads(0) lrwindow(1) bootstrap(100)
   xtwest RYPC DPOP PTO
Note: not more than 6 variables at a time, and no missing values, try with 0 lags and leads because sometimes lack of observaions problem.
   
7. Heterogeneity slope coefficients
Note:To adjust heteroscedasticity and auto correlation following Blomquist, Westerlund (2013) use hac, to compare the standard delta test and the hac robust version use ,compare
   xthst RYPC lnmmx_TDPY compA comp1 lnmmx_PFIN FAPG DPOP lnmmx_PTO
   xthst RYPC l.RYPC lnmmx_TDPY compA comp1 lnmmx_PFIN FAPG DPOP lnmmx_PTO
   xthst RYPC l.RYPC lnmmx_TDPY compA comp1 lnmmx_PFIN FAPG DPOP lnmmx_PTO, hac
   xthst RYPC l.RYPC lnmmx_TDPY compA comp1 lnmmx_PFIN FAPG DPOP lnmmx_PTO, compare
   
   xthst RYPC TDPY TDPYIQ2 TDPYFD PFIN FAPG DPOP PTO	
   xthst RYPC l.RYPC TDPY TDPYIQ2 TDPYFD PFIN FAPG DPOP PTO
   xthst RYPC l.RYPC TDPY TDPYIQ2 TDPYFD PFIN FAPG DPOP PTO, hac
   xthst RYPC l.RYPC TDPY TDPYIQ2 TDPYFD PFIN FAPG DPOP PTO, comparehac
   
   
   
Nonlinearity test 
scatter RYPC TDPY 
generate TDPY_sq = TDPY^ 2
regress RYPC TDPY TDPY_sq
regress RYPC TDPY
estimates store linear
regress RYPC TDPY TDPY_sq
lrtest linear
  
   
 Use the models which can solve the cross sectional dependence, endogeneity and heterogeneity
 
 DCCE
 linear 
asdoc xtdcce2 RYPC l.RYPC TDPY PFIN FAPG DPOP PTO , reportc cr(RYPC l.RYPC TDPY PFIN FAPG DPOP PTO)cr_lags(1)fullsample
 Interaction  Regression
 xtdcce2 RYPC l.RYPC TDPY TDPYIQ2 PFIN FAPG DPOP PTO , reportc cr(RYPC l.RYPC TDPY TDPYIQ2 PFIN FAPG DPOP PTO))cr_lags(1)fullsample  jackknife
 xtdcce2 RYPC l.RYPC TDPY TDPYIQ2 PFIN FAPG DPOP PTO , reportc cr(RYPC l.RYPC TDPY TDPYIQ2 PFIN FAPG DPOP PTO)cr_lags(0)fullsample
 xtdcce2 RYPC l.RYPC lnmmx_TDPY compA lnmmx_PFIN FAPG DPOP lnmmx_PTO , reportc cr(RYPC l.RYPC lnmmx_TDPY compA lnmmx_PFIN FAPG DPOP lnmmx_PTO)cr_lags(0) fullsample   
 xtdcce2 RYPC l.RYPC lnmmx_TDPY compA lnmmx_PFIN FAPG DPOP lnmmx_PTO , reportc cr(RYPC l.RYPC lnmmx_TDPY compA lnmmx_PFIN FAPG DPOP lnmmx_PTO)cr_lags(0) fullsample
 
 asdoc xtdcce2 RYPC l.RYPC TDPY TDPYIQ2 PFIN FAPG DPOP PTO , reportc cr(RYPC l.RYPC TDPY TDPYIQ2 PFIN FAPG DPOP PTO)cr_lags(0)fullsample
 
 xtdcce2 RYPC l.RYPC TDPY TDPYFD PFIN PTO FAPG DPOP , reportc cr(RYPC l.RYPC TDPY TDPYFD PFIN PTO FAPG DPOP)cr_lags(1) fullsample
 xtdcce2 RYPC l.RYPC TDPY TDPYFD PFIN PTO FAPG DPOP , reportc cr(RYPC l.RYPC TDPY TDPYFD PFIN PTO FAPG DPOP)cr_lags(0) fullsample
 xtdcce2 RYPC l.RYPC lnmmx_TDPY comp1 lnmmx_PFIN FAPG DPOP lnmmx_PTO , reportc cr(RYPC l.RYPC lnmmx_TDPY comp1 lnmmx_PFIN FAPG DPOP lnmmx_PTO)cr_lags(1)fullsample
 xtdcce2 RYPC l.RYPC lnmmx_TDPY comp1 lnmmx_PFIN FAPG DPOP lnmmx_PTO , reportc cr(RYPC l.RYPC lnmmx_TDPY comp1 lnmmx_PFIN FAPG DPOP lnmmx_PTO)cr_lags(0)fullsample
 
 asdoc xtdcce2 RYPC l.RYPC TDPY TDPYFD PFIN PTO FAPG DPOP , reportc cr(RYPC l.RYPC TDPY TDPYFD PFIN PTO FAPG DPOP)cr_lags(0) fullsample
 
 xtdcce2 RYPC l.RYPC TDPY MEPT_pca PFIN FAPG DPOP PTO , reportc cr(RYPC l.RYPC TDPY MEPT_pca PFIN FAPG DPOP PTO)cr_lags(1)fullsample
 xtdcce2 RYPC l.RYPC TDPY MEPT_pca PFIN FAPG DPOP PTO , reportc cr(RYPC l.RYPC TDPY MEPT_pca PFIN FAPG DPOP PTO)cr_lags(0)fullsample
 xtdcce2 RYPC l.RYPC lnmmx_TDPY MEPT_pca lnmmx_PFIN FAPG DPOP lnmmx_PTO , reportc cr(RYPC l.RYPC lnmmx_TDPY MEPT_pca lnmmx_PFIN FAPG DPOP lnmmx_PTO)cr_lags(0) fullsample   
 xtdcce2 RYPC l.RYPC lnmmx_TDPY MEPT_pca lnmmx_PFIN FAPG DPOP lnmmx_PTO , reportc cr(RYPC l.RYPC lnmmx_TDPY MEPT_pca lnmmx_PFIN FAPG DPOP lnmmx_PTO)cr_lags(0) fullsample

 xtdcce2 RYPC l.RYPC TDPY MEPT_pca PFIN FAPG DPOP PTO , reportc cr(RYPC l.RYPC TDPY MEPT_pca PFIN FAPG DPOP PTO)cr_lags(0)fullsample
 
26 03 2020
asdoc xtdcce2 lnRYPC l.lnRYPC lnTDPY TDPYFD lnPFIN lnPTO FAPG DPOP , reportc cr(lnRYPC lnTDPY TDPYFD lnPFIN lnPTO FAPG DPOP)cr_lags(1)
asdoc xtdcce2 lnRYPC l.lnRYPC lnTDPY TDPYIQ1 lnPFIN lnPTO FAPG DPOP , reportc cr(lnRYPC lnTDPY TDPYIQ1 lnPFIN lnPTO FAPG DPOP)cr_lags(1)
asdoc xtdcce2 lnRYPC l.lnRYPC lnTDPY lnPFIN lnPTO FAPG DPOP , reportc cr(lnRYPC lnTDPY lnPFIN lnPTO FAPG DPOP)cr_lags(1)



26 03 2020
GMM
asdoc xtabond2 lnRYPC l.lnRYPC lnTDPY lnPFIN lnPTO FAPG DPOP, gmm(l.lnRYPC, lag(2 1) collapse eq(level)) iv(lnTDPY lnPFIN lnPTO FAPG DPOP, eq(diff)) gmm(l.lnRYPC, lag(2 1) collapse eq(diff)) twostep robust
asdoc xtabond2 lnRYPC l.lnRYPC lnTDPY TDPYIQ2 lnPFIN lnPTO FAPG DPOP, gmm(l.lnRYPC, lag(2 1) collapse eq(level)) iv(lnTDPY lnPFIN lnPTO FAPG DPOP, eq(diff)) gmm(l.lnRYPC, lag(2 1) collapse eq(diff)) twostep robust
asdoc xtabond2 lnRYPC l.lnRYPC lnTDPY TDPYFD lnPFIN lnPTO FAPG DPOP, gmm(l.lnRYPC, lag(2 1) collapse eq(level)) iv(lnTDPY lnPFIN lnPTO FAPG DPOP, eq(diff)) gmm(l.lnRYPC, lag(2 1) collapse eq(diff)) twostep robust
 
Robust check by Kremer Threshold
DPTM
xtendothresdpd RYPC L.RYPC PFIN PTO FAPG DPOP, thresv( IQ2 ) stub(enr) pivar(TDPY) dgmmiv(PFIN PTO FAPG DPOP)	
xtendothresdpd RYPC L.RYPC PFIN PTO FAPG DPOP, thresv( FD ) stub(enr) pivar(TDPY) dgmmiv(PFIN PTO FAPG DPOP)
xtendothresdpd RYPC L.RYPC PFIN PTO FAPG DPOP, thresv( MEPI_pca ) stub(enr) pivar(TDPY) dgmmiv(PFIN PTO FAPG DPOP)
	
xtendothresdpd RYPC L.RYPC lnmmx_PFIN FAPG DPOP lnmmx_PTO, thresv( IQ2 ) stub(enr) pivar(lnmmx_TDPY) dgmmiv(lnmmx_PFIN FAPG DPOP lnmmx_PTO)	
xtendothresdpd RYPC L.RYPC lnmmx_PFIN FAPG DPOP lnmmx_PTO, thresv( FD ) stub(enr) pivar(lnmmx_TDPY) dgmmiv(lnmmx_PFIN FAPG DPOP lnmmx_PTO)
xtendothresdpd RYPC L.RYPC lnmmx_PFIN FAPG DPOP lnmmx_PTO, thresv( MEPI_pca ) stub(enr) pivar(lnmmx_TDPY) dgmmiv(lnmmx_PFIN FAPG DPOP lnmmx_PTO)
	
xtendothresdpd RYPC L.RYPC lnmmx_PFIN FAPG DPOP lnmmx_PTO, thresv( FD ) stub(enr) pivar(lnmmx_TDPY) dgmmiv(lnmmx_PFIN FAPG DPOP lnmmx_PTO)
xtendothresdpd RYPC L.RYPC lnmmx_PFIN FAPG DPOP lnmmx_PTO, thresv( IQ2 ) stub(enr) pivar(lnmmx_TDPY) dgmmiv(lnmmx_PFIN FAPG DPOP lnmmx_PTO)	
xtendothresdpd RYPC L.RYPC lnmmx_PFIN FAPG DPOP lnmmx_PTO, thresv( MEPT_pca ) stub(enr) pivar(lnmmx_TDPY) dgmmiv(lnmmx_PFIN FAPG DPOP lnmmx_PTO)

 26 3 2020
asdoc xtendothresdpd lnRYPC l.lnRYPC lnPFIN lnPTO FAPG DPOP, thresv(FD) stub(enr) pivar(lnTDPY) dgmmiv(yd2-yd25)
asdoc xtendothresdpd lnRYPC l.lnRYPC lnPFIN lnPTO FAPG DPOP, thresv(IQ1) stub(enr) pivar(lnTDPY) dgmmiv(yd2-yd25) not good threshold 
asdoc xtendothresdpd lnRYPC l.lnRYPC lnPFIN lnPTO FAPG DPOP, thresv(IQ2) stub(enr) pivar(TDPY) dgmmiv(yd2-yd25)   applied one


Robust by DCCE with two controls
asdoc xtdcce2 lnRYPC l.lnRYPC lnTDPY lnPFIN lnPTO FAPG DPOP DCPI REIR , reportc cr(lnRYPC lnTDPY lnPFIN lnPTO FAPG DPOP DCPI REIR )cr_lags(0)
asdoc xtdcce2 lnRYPC l.lnRYPC lnTDPY TDPYFD lnPFIN lnPTO FAPG DPOP DCPI REIR, reportc cr(lnRYPC lnTDPY TDPYFD lnPFIN lnPTO FAPG DPOP DCPI REIR)cr_lags(0)
asdoc xtdcce2 lnRYPC l.lnRYPC lnTDPY TDPYIQ1 lnPFIN lnPTO FAPG DPOP DCPI REIR, reportc cr(lnRYPC lnTDPY TDPYIQ1 lnPFIN lnPTO FAPG DPOP DCPI REIR)cr_lags(0)



asdoc xtdcce2 lnRYPC l.lnRYPC lnTDPY TDPYVOA lnPFIN lnPTO FAPG DPOP , reportc cr(lnRYPC lnTDPY TDPYVOA lnPFIN lnPTO FAPG DPOP)cr_lags(1)jackknife
asdoc xtdcce2 lnRYPC l.lnRYPC lnTDPY TDPYPS lnPFIN lnPTO FAPG DPOP , reportc cr(lnRYPC lnTDPY TDPYPS lnPFIN lnPTO FAPG DPOP)cr_lags(1)jackknife
asdoc xtdcce2 lnRYPC l.lnRYPC lnTDPY TDPYGE lnPFIN lnPTO FAPG DPOP , reportc cr(lnRYPC lnTDPY TDPYGE lnPFIN lnPTO FAPG DPOP)cr_lags(1)jackknife
asdoc xtdcce2 lnRYPC l.lnRYPC lnTDPY TDPYRQ lnPFIN lnPTO FAPG DPOP , reportc cr(lnRYPC lnTDPY TDPYRQ lnPFIN lnPTO FAPG DPOP)cr_lags(1)jackknife
asdoc xtdcce2 lnRYPC l.lnRYPC lnTDPY TDPYROL lnPFIN lnPTO FAPG DPOP , reportc cr(lnRYPC lnTDPY TDPYROL lnPFIN lnPTO FAPG DPOP)cr_lags(1)jackknife
asdoc xtdcce2 lnRYPC l.lnRYPC lnTDPY TDPYRCOC lnPFIN lnPTO FAPG DPOP , reportc cr(lnRYPC lnTDPY TDPYRCOC lnPFIN lnPTO FAPG DPOP)cr_lags(1)jackknife




