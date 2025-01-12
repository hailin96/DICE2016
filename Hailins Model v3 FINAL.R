#2016 DICE Model

install.packages("NlcOptim")
library(NlcOptim)
library(tidyr)
install.packages("tidyverse")
library(tibble)
install.packages("numDeriv")
install.packages("BB")
install.packages("nloptr")
install.packages("DEoptim")
install.packages("DEoptimR")

library(BB)

t <- 1:100 #assume 5 time periods of 5 years each
tfirst <- t== 1 #if t is equal to 1 exactly, then it is tfirst.
tlast <- t==length(t) #if t is equal to NT exactly, then it is tlast.
tstep <- 5 #5 years per period
NT <- length(t)

#=========
#SCENARIO: OPTIMAL POLICY
#Control Variables 
#Defining S(t)= savings rate (CONTROL VARIABLE)
dk = 0.1 #depreciation rate on capital per year (PARAMETER)
elasmu = 1.45 #elasticity of marginal utility of consumption (PARAMETER)
prstp = 0.015 #initial rate of social time preference (PARAMETER)
gamma = 0.3 #elasticity of capital (PARAMETER)
optlrsav <- (dk + 0.004)/(dk + 0.004*elasmu + prstp)*gamma #optimal long-run savings rate (PARAMETER)
lag10 <- t > NT - 10 #instructs R to make last 10 positions of the vectors as follows:
S.lo <- rep(1e-1, NT)
S.lo[lag10] <- optlrsav
S.up <- rep(0.9, NT)
S.up[lag10] <- optlrsav
S_start <- rep(0.2, NT)
S_start[S_start < S.lo] <- S.lo[S_start < S.lo]
S_start[S_start > S.up] <- S.lo[S_start > S.up]
S <- S_start

#Defining MIU(t)= emissions reduction rate (CONTROL VARIABLE)
miu0 = 0.03 #Initial emissions control rate for base case in 2015
limmiu = 1.2 #Upper limit of control rate used for after t=30
MIU.up <- rep(limmiu, NT) #This is copying the original model
MIU.up[t < 30] <- 1
MIU.up[1] <- miu0
MIU.lo <- rep(0.01, NT)
MIU.lo[1] <- miu0
MIU_start <- 0.99*MIU.up
MIU_start[MIU_start < MIU.lo] <- MIU.lo[MIU_start < MIU.lo]
MIU_start[MIU_start > MIU.up] <- MIU.up[MIU_start > MIU.up]
MIU <- MIU_start

#==============

#PARAMETERS

#ga(t)= ga0 * exp(-dela*5*(t-1))
#Inputs: ga0 = initial growth rate of productivity per 5 years(PARAMETER)
#dela= decline rate of total factor productivity per 5 years(PARAMETER)
#Output: ga(t) = growth rate of productivity per 5 years(PARAMETER)
ga0= 0.076 #you can change the parameter
dela= 0.005 #you can change the parameter
ga <- rep(NA, NT)
ga[1] <- ga0

#al(t)= loop(t, al(t))=al(t-1)/((1-ga(t-1)))
#Inputs: ga(t)= growth rate of productivity (PARAMETER)
#Output: al(t)= level of total factor productivity (PARAMETER)
a0=5.115 #initial level of total factor productivity and you can change the parameter
al <- rep(NA, NT)
al[1] <- a0

#l(t)= loop(t, l(t)=l(t-1)*(popasym/l(t))^popadj)
#Inputs: popasym= asymptotic population in millions (PARAMETER); 
#popadj= growth rate to calibrate to 2050 population projection (PARAMETER)
#Output: l(t)= level of population and labour (PARAMETER) 
pop0= 7403 #initial world population in 2015 and you can change the parameter
l<- rep(NA, NT)
popasym= 11500 #you can change the parameter
popadj= 0.134 #you can change the parameter
l[1] <- pop0

#forcoth(t)= fex0 + (1/5)*(fex1-fex0)*(t-1) OR (t.val lt 18)+(fex1-fex0)
#Inputs: fex0= 2015 forcing of other GHGs (PARAMETER); 
#fex1= 2100 forcing of other GHGs (PARAMETER);
#Output: forcoth= exogenous forcing for other GHGs(PARAMETER)
fex0= 0.5 #you can change the parameter
fex1= 1.0 #you can change the parameter
forcoth <- rep(fex0, NT)
forcoth[1] <- fex0
forcoth[1]

#etree(t)=eland0*(1-deland)^(t-1)
#Inputs: eland = carbon emissions from land in 2015 (PARAMETER); 
#deland = decline rate of land emissions per period (PARAMETER)
#Output: etree(t) = emissions from deforestation (PARAMETER)
eland0 = 2.6 #you can change the parameter
deland = 0.115 #you can change the parameter
etree <- rep(NA, NT)
etree[1] <- eland0*(1-deland)^(t[1]-1)
etree[1]

#sig0 = e0/(q0*(1-miu0))
#Inputs: e0 = industrial emissions in 2015 in trillions of USD (PARAMETER); 
#q0 = gross world output in 2015 in trillions of USD (PARAMETER);
#miu0 = initial emissions control rate for base case (PARAMETER)
#Outputs: sig0 = initial emissions to output ratio (PARAMETER)
e0 = 35.85 #you can change the parameter
q0 = 105.5 #you can change the parameter
miu0 = 0.03 #you can change the parameter
sig0 <- e0/(q0*(1-miu0))

#Defining gsig = change in sigma (PARAMETER)
dsig = -0.001 #you can change the parameter
gsigma1 = -0.0152 #initial growth of sigma (can change)
gsig <- rep(NA, NT)
gsig[1] <- gsigma1
gsig[1]

#Defining sigma = emissions-to-output ratio (PARAMETER)
sigma <- rep(NA, NT)
sigma[1] <- sig0

#pbacktime(t)= pback*(1-gback)^(t-1)
#Inputs: pback= cost of backstop in 2010 USD per tons of CO2 in 2015 (PARAMETER);
#gback= initial cost decline of of backstop cost (PARAMETER)
#Output: pbacktime(t)= backstop price (PARAMETER)
pback= 550 #you can change this parameter
gback= 0.025 #you can change this parameter
pbacktime <- rep(NA, NT)
pbacktime[1] <- pback*(1-gback)^(t[1]-1)

#cost1(t)= pbacktime(t)*sigma(t)/expcost2/1000
#Inputs: pbacktime(t)= backstop price (PARAMETER); sigma(t)= CO2 equivalent emissions output ratio (PARAMETER); 
#expcost2= exponent of control cost function (PARAMETER)
#Output: cost1(t)= adjusted cost for backstop (PARAMETER)
expcost2 = 2.6 #you can change the parameter
cost1 <- rep(NA, NT)
cost1[1] <- pbacktime[1]*sigma[1]/expcost2/1000

#rr(t)= 1/((1+prstp)**(tstep*(t-1)))
#Inputs: prstp= initial rate of social time preference (PARAMETER)
#Output: rr(t)= average utility social discount rate (PARAMETER)
prstp= 0.015
rr <- rep(NA, NT)
rr[1] <- 1/((1+prstp)^(tstep*(t[1]-1))) 

#cumetree= loop(t,cumetree(t)=cumetree(t-1)+etree(t-1)*(5/3.666)
#Inputs:etree(t)= emissions from deforestation in GtC (PARAMETER);
#Output: cumetree(t)= cumulative emissions from land in GtC (PARAMETER);
cumetree <- rep(NA, NT)
cumetree0= 100 #initial land emissions
cumetree[1] <- cumetree0

#lam= fco22x/ t2xco2
#Inputs: fco22x= radiative forcing of doubling equilibrium CO2 (PARAMETER);
#t2xco2= equilibrium temperature impact (PARAMETER);
fco22x = 3.6813 
t2xco2= 3.1 
lam <- fco22x/t2xco2

#===============

#VARIABLES

#K(t)= ((1-dk)^tstep)*K(t-1) + tstep*I(t-1)
#Inputs: dk = depreciation rate on capital (PARAMETER); I(t) = investments in trillions of 2005 USD (VARIABLE)
#Output: K(t)= capital stock (VARIABLE)
k0= 223 #initial capital value and you can change the parameter
K <- rep(NA, NT)
K[1] <- k0

#YGROSS(t) = al(t)*((l(t)/1000)^(1-gamma))*(K(t)^gamma)
#Inputs: al(t)= level of total factor productivity (VARIABLE); l(t)= level of population and labour (VARIABLE);
#K(t)= capital stock (VARIABLE); gamma= elasticity of capital (PARAMETER)
#Output: YGROSS(t)= gross world output including abatement and damages in trillions of 2005 USD (VARIABLE)
gamma= 0.3 #you can change the parameter
YGROSS <- rep(NA, NT)
YGROSS[1] <- al[1]*{(l[1]/1000)^(1-gamma)}*(K[1]^gamma)

#EIND(t) = sigma(t)*(1-MIU(t))*YGROSS(t)
#Inputs: sigma(t) = emissions to output ratio (PARAMETER); MIU(t) = emissions reduction rate (CONTROL VARIABLE);
#YGROSS(t) = Gross World Product including abatement and damages in trillions USD (VARIABLE)
#Output: EIND(t) = industrial emissions (VARIABLE) 
EIND <- rep(NA, NT)
EIND[1] <- sigma[1]*(1-MIU[1])*YGROSS[1]
EIND[1]

#E(t) = EIND(t) + etree(t)
#Inputs: EIND(t) = industrial emissions (VARIABLE); etree(t) = emissions from deforestation (PARAMETER);
#dE= emissions addition (PARAMETER)
#Output: E(t) = total CO2 emissions at time t (VARIABLE)
fosslim= 6000 #max cumulative extraction of of fossil fuels in GtC
dE <- rep(0, NT)
E <- rep(NA, NT)
E[1] <- EIND[1] + etree[1] + dE[1]


#CCA(t)= CCA(t-1) + EIND(t-1)*5/3.666
#Inputs: EIND(t)= industrial emissions (VARIABLE);
#Output: CCA(t)= cumulative industrial emissions in GtC (VARIABLE)
CCA <- rep(NA, NT)
cca0= 400 #initial industrial emissions
CCA[1] <- cca0

#CCATOT(t)= CCA(t)+cumetree(t)
#Inputs: CCA(t)= cumulative industrial emissions in GtC (VARIABLE);
#cumetree(t)= cumulative emissions from land in GtC (PARAMETER);
#Output: CCATOT(t)= total carbon emissions (VARIABLE)
CCATOT <- rep(NA, NT)
CCATOT[1] <- CCA[1] + cumetree[1]

#MAT(t) = MAT(t-1)*b11 + MU(t-1)*b21 + E(t-1)*(5/3.666)
#Inputs: MU(t) = carbon concentration increase in upper ocean in GtC from 1750 (VARIABLE);
#E(t) = total CO2 emissions at time t (VARIABLE);
#b11, b21 = carbon cycle transition matrix (PARAMETER);
#Output: MAT(t)= carbon concentration increase in atmosphere in GtC from 1750 (VARIABLE)
b12 = 0.12 #you can change this parameter
b11 = 1 - b12
mateq = 588 #Equilibrium CO2 concentration in the atmosphere (GtC)
mueq = 360 #Equilibrium CO2 concentration in upper ocean (GtC)
b21 = b12*mateq/mueq
mat0 = 851 #Initial equilibrium CO2 concentration in the atmosphere (GtC)
MAT <- rep(NA, NT)
MAT[1] <- mat0

#MU(t) = MAT(t-1)*b12 + MU(t-1)*b22 + ML(t-1)*b32
#Inputs: MAT(t) = carbon concentration increase in atmosphere in GtC from 1750 (VARIABLE);
#ML(t) = carbon concentration increase in lower ocean in GtC from 1750 (VARIABLE);
#b22, b32, b33 = carbon cycle transition matrix (PARAMETER)
#Output:
#MU(t) = carbon concentration increase in upper ocean in GtC from 1750 (VARIABLE)
mleq <- 1720
b23 = 0.007
b22 = 1 - b21 - b23;
b32 = b23*mueq/mleq;
b33 = 1 - b32
mu0 = 460 #Initial equilibrium CO2 concentration in upper ocean (GtC)
MU <- rep(NA, NT)
MU[1] <- mu0

#ML(t) = ML(t-1)*b33 + MU(t-1)*b23
#Inputs: MU(t) = carbon concentration increase in upper ocean in GtC from 1750 (VARIABLE)
#b33, b23 = carbon cycle transition matrix (PARAMETER)
#Output: ML(t) = carbon concentration increase in lower ocean in GtC from 1750 (VARIABLE)
ml0 = 1740 #Initial equilibrium CO2 concentration in lower ocean (GtC)
ML <- rep(NA, NT)
ML[1] <- ml0

#FORC(t)= fco22x * ((log((MAT(t)/588.000))/log(2))) + forcoth(t)
#Inputs: fco22x= radiative forcing of doubling equilibrium CO2 (PARAMETER);
#MAT(t)= carbon concentration increase in atmosphere since 1750 (VARIABLE);
#forcoth(t)= exogenous forcing for other GHGs (PARAMETER)
#Output: FORC(t)= increase in radiative forcing since 1900 in watts per m2 (VARIABLE)
FORC <- rep(NA, NT)
FORC[1] <- fco22x * ((log((MAT[1]/588.000))/log(2))) + forcoth[1]
FORC[1]

#TATM(t)= TATM(t-1)*c1*((FORC(t)-(fco22x/t2xco2)*TATM(t-1))-(c3*(TATM(t-1)-TOCEAN(t-1))))
#Inputs: c1= coefficient for upper ocean (PARAMETER); 
#fco22x= radiative forcing of doubling equilibrium CO2 (PARAMETER); 
#t2xco2= equilibrium temperature impact (PARAMETER);
#c3= transfer coefficient from upper to lower stratum of ocean (PARAMETER)
#FORC(t)= increase in radiative forcing since 1900 in watts per m2 (VARIABLE)
#TOCEAN(t)= temperature increase in lower ocean since 1900 in Celsius (VARIABLE)
#Output: TATM(t)= temperature increase in atmosphere since 1900 in Celsius (VARIABLE)
c1= 0.1005 #you can change the parameter
c3= 0.088 #you can change the parameter
c4= 0.025 #you can change the parameter
tatm0 = 0.85 #Initial temp change in atmosphere in Celsius from 1900 
TATM <- rep(NA, NT)
TATM[1] <- tatm0

#TOCEAN(t)= TOCEAN(t-1) + c4*(TATM(t-1) - TOCEAN(t-1))
#Inputs: c4= transfer coefficient for lower level (PARAMETER);
#TATM(t)= temperature increase in atmosphere since 1900 in Celsius (VARIABLE)
#Output: TOCEAN(t)= temperature increase in lower ocean since 1900 in Celsius (VARIABLE)
c4= 0.025 #you can change the parameter
tocean0 = 0.0068 #Initial temp change in Celsius in lower oceans since 1900
TOCEAN <- rep(NA, NT)
TOCEAN[1] <- tocean0

#DAMFRAC(t)= a1*TATM(t) + a2*TATM(t)^a3
#Inputs: a1= damage intercept (PARAMETER); TATM(t)= temperature increase in the atmosphere from 1900 (VARIABLE)
#a2= damage quadratic term (PARAMETER); a3= damage exponent (PARAMETER)
#Output: DAMFRAC(t)= percent of GWP spent on damages (VARIABLE)
a10 = 0 #initial damage intercept
a20 = NULL #initial damage quadratic term
a1 = 0 #damage intercept
a2 = 0.00236 #you can change this parameter
a3 = 2.00 #you can change this parameter
DAMFRAC <- rep(NA, NT)
DAMFRAC[1] <- a1*TATM[1] + a2*(TATM[1]^a3)

#DAMAGES(t)= YGROSS(t)*DAMFRAC(t)
#Inputs: YGROSS(t)= GWP including abatement costs and damages (VARIABLE);
#DAMFRAC(t)= percent of GWP spent on damages (VARIABLE)
#Output: DAMAGES(t)= GWP spent on damages in trillions of 2010 USD (VARIABLE)
DAMAGES <- rep(NA, NT)
DAMAGES[1] <- YGROSS[1]*DAMFRAC[1]

#ABATECOST(t)= YGROSS(t) * cost1(t) * MIU(t)^expcost2
#Inputs: YGROSS(t)= GWP including spending on abatement and damages (VARIABLE);
#cost1(t)= adjusted cost for backstop (PARAMETER); #MIU(t)= emissions reduction rate (CONTROL VARIABLE);
#expcost2= exponent of control cost function (PARAMETER); 
#Output: ABATECOST(t)= cost of emissions abatement (PARAMETER)
tnopol = 45 #period with no emissions control base (PARAMETER)
ABATECOST <- rep(NA, NT)
ABATECOST[1] <- YGROSS[1]*cost1[1]*MIU[1]^expcost2

#MCABATE(t)= pbacktime(t)*MIU(t)^(expcost2-1)
#Inputs: pbacktime(t)= backstop price (PARAMETER);
#MIU(t)= emissions control rate (CONTROL VARIABLE);
#expcost2= exponent of control cost function (PARAMETER);
#Output: MCABATE(t)= marginal cost of abatement in 2010 USD per ton (VARIABLE) 
MCABATE <- rep(NA, NT)
MCABATE[1] <- pbacktime[1]*MIU[1]^(expcost2 - 1)

#CPRICE(t)= pbacktime(t)*MIU(t)^(expcost2-1)
#Inputs: pbacktime(t)= backstop price (PARAMETER);
#MIU(t)= emissions control rate (CONTROL VARIABLE);
#expcost2= exponent of control cost function (PARAMETER);
#Output: CPRICE(t)= carbon price in 2010 USD per ton (VARIABLE) 
cprice0= 2 #initial base carbon price (PARAMETER)
gcprice= 0.02 #growth rate of base carbon price per year (PARAMETER)
cpricebase <- rep(NA, NT)
cpricebase[1] <- cprice0
CPRICE <- rep(NA, NT)
CPRICE[1] <- pbacktime[1]*MIU[1]^(expcost2 - 1)

#YNET(t)= YGROSS(t)*(1-damfrac(t))
#Inputs: YGROSS(t)= GWP including spending on abatement and damages in trillions of 2010 USD (VARIABLE); 
#damfrac(t)= percent of GWP spent on damages (VARIABLE);
#Output: YNET(t)= GWP excluding damages in trillions of 2010 USD (PARAMETER)
YNET <- rep(NA, NT)
YNET[1] <- YGROSS[1]*(1-DAMFRAC[1])

#Y(t) = YNET(t) - ABATECOST(t)
#Inputs: YNET(t)= GWP excluding damages (VARIABLE); ABATECOST(t)= cost of emissions abatement (VARIABLE)
#Output: Y(t)= GWP excluding spending on abatement and damages (VARIABLE)
Y <- rep(NA, NT)
Y[1] <- YNET[1] - ABATECOST[1]

#I(t)= S(t)*Y(t)
#Inputs: S(t) = savings rate (CONTROL VARIABLE); Y(t)= GWP without abatement and damages costs (VARIABLE);
#Output: I(t) = investments in trillions of 2005 USD (VARIABLE)
I <- rep(NA, NT)
I[1] <- S[1]*Y[1]

#C(t)= Y(t) - I(t)
#Inputs: Y(t)= GWP without abatement costs and damages in trillions of 2010 USD (VARIABLE);
#I(t)= investment in trillions of 2010 USD (VARIABLE);
#Output: C(t)= consumption per year in trillions of 2010 USD (VARIABLE)
C <- rep(NA, NT)
C[1] <- Y[1] - I[1]

#CPC(t) = 1000*C(t)/l(t)
#Inputs: C(t)= consumption per year in trillions of 2010 USD (VARIABLE);
#l(t)= level of population and labour (PARAMETER);
#Output: CPC(t)= capital stock in trillions of 2010 USD (VARIABLE)
CPC <- rep(NA, NT)
CPC[1] <- 1000*C[1]/l[1]

#RI(t)= (1+prstp)*(CPC(t+1)/CPC(t))^(elasmu/tstep)-1
#Inputs: prstp= initial rate of social time preference (PARAMETER);
#CPC(t)= capital stock in trillions of 2010 USD (VARIABLE);
#elasmu= elasticity of marginal utility consumption (PARAMETER);
#Output: RI(t)= real interest rate (VARIABLE)
RI <- rep(NA, NT)


#PERIODU(t)= ((C(t)*1000/l(t))^(1-elasmu)-1)/(1-elasmu)-1
#Inputs: C(t)= consumption per year in trillions of 2010 USD (VARIABLE);
#l(t)= level of population and labour (PARAMETER);
#elasmu= elasticity of marginal utility consumption (PARAMETER);
#Output: PERIODU(t)= utility function for one period (VARIABLE)
PERIODU <- rep(NA, NT)
PERIODU[1] <- ((C[1]*1000/l[1])^(1-elasmu)-1)/(1-elasmu)-1

#CEMUTOTPER(t)= PERIODU(t)*l(t)*rr(t)
#Inputs: PERIODU(t)= utility function for one period (VARIABLE);
#l(t)= level of population and labour (PARAMETER);
#rr(t)= average utility discount rate (PARAMETER);
#Output: CEMUTOTPER(t)= period utility (VARIABLE)
CEMUTOTPER <- rep(NA, NT)
CEMUTOTPER[1] <- PERIODU[1]*l[1]*rr[1]

#UTILITY(t)= tstep*scale1*sum(t,CEMUTOTPER(t)) + scale2
#Inputs: scale1= a scaling coefficient to ensure that PV consumption = PV utility (PARAMETER);
#scale2= additional scaling coefficient 
#CEMUTOTPER(t)= period utility (VARIABLE);
#Output: UTILITY(t)= social welfare function (VARIABLE)
scale1= 0.0302455265681763
scale2= -10993.704
UTILITY <- rep(NA, NT)
UTILITY[1] <- tstep * scale1 * sum(CEMUTOTPER[1:1]) + scale2

#==========

#Loop for each period
forcoth[1:17] <- forcoth[1:17] + (1/17)*(fex1-fex0)*(t[1:17]-1)
forcoth[18:NT] <- forcoth[18:NT] + (fex1-fex0)

for(i in 2:NT) {
  #parameters#
  ga[i] <- ga0*exp(-dela*5*(t[i-1]))
  al[i] <- al[i-1]/(1-ga[i-1])
  
  l[i] <- l[i-1]*(popasym/l[i-1])^popadj
  etree[i] <- eland0*(1-deland)^(t[i-1])
  gsig[i] <- gsig[i-1]*((1+dsig)^tstep) 
  sigma[i] <- (sigma[i-1] * exp(gsig[i-1] * tstep)) 
  pbacktime[i] <- pback*(1-gback)^(t[i-1])
  cost1[i] <- pbacktime[i]*sigma[i]/expcost2/1000
  rr[i] <- 1/((1+prstp)**(tstep*(t[i-1]))) 
  cumetree[i] <- cumetree[i-1]+etree[i-1]*(5/3.666)
  cpricebase[i] <- cprice0*(1+gcprice)^(5*t[i-1])
  
  #variables#
  K[i] <- (1-dk)^tstep * K[i-1] + tstep * I[i-1]
  YGROSS[i] <- al[i]*((l[i]/1000)^(1-gamma))*(K[i]^gamma)
  EIND[i] <- sigma[i]*(1-MIU[i])*YGROSS[i]
  E[i] <- EIND[i] + etree[i] + dE[i]
  CCA[i] <- CCA[i-1] + EIND[i-1]*5/3.666
  CCATOT[i] <- CCA[i] + cumetree[i]
  MAT[i] <- MAT[i-1]*b11 + MU[i-1]*b21 + E[i-1]*(5/3.666)
  MU[i] <- MAT[i-1]*b12 + MU[i-1]*b22 + ML[i-1]*b32
  ML[i] <- ML[i-1]*b33 + MU[i-1]*b23
  FORC[i] <- fco22x * ((log((MAT[i]/588.000))/log(2))) + forcoth[i]
  TATM[i] <- TATM[i-1]+ c1*((FORC[i]-(fco22x/t2xco2)*TATM[i-1])-(c3*(TATM[i-1]-TOCEAN[i-1])))
  TOCEAN[i] <- TOCEAN[i-1] + c4*(TATM[i-1] - TOCEAN[i-1])
  DAMFRAC[i] <- a1*TATM[i] + a2*(TATM[i]^a3)
  DAMAGES[i] <- YGROSS[i]*DAMFRAC[i]
  ABATECOST[i] <- YGROSS[i]*cost1[i]*MIU[i]^expcost2
  MCABATE[i] <- pbacktime[i]*MIU[i]^(expcost2 - 1)
  CPRICE[i] <- pbacktime[i]*MIU[i]^(expcost2 - 1)
  YNET[i] <- YGROSS[i]*(1-DAMFRAC[i])
  Y[i] <- YNET[i] - ABATECOST[i]
  I[i] <- S[i] * Y[i]
  C[i] <- Y[i] - I[i]
  CPC[i] <- 1000*C[i]/l[i]
  
  PERIODU[i] <- ((C[i]*1000/l[i])^(1-elasmu)-1)/(1-elasmu)-1
  CEMUTOTPER[i] <- PERIODU[i]*l[i]*rr[i]
  UTILITY[i] <- tstep * scale1 * sum(CEMUTOTPER[1:i]) + scale2
}

for(i in 1: NT-1) {RI[i] <- (1+prstp)*(CPC[i+1]/CPC[i])^(elasmu/tstep) - 1}
  
pars <- c("a0", "a1", "a10", "a2", "a20", "a3", "al", "b11", "b12", "b21", "b22", "b23", "b32", "b33", 
          "c1", "c3", "c4", "cost1", "cprice0", "cpricebase", "cumetree", "dela", "deland", "dk", "dsig", 
          "e0", "eland0", "elasmu", "etree", "expcost2", "fco22x", "fex0", "fex1", "forcoth", "fosslim", 
          "ga", "ga0", "gamma", "gback", "gcprice", "gsig", "gsigma1", "ifopt", "k0", "l", "lam", "limmiu", 
          "mat0", "mateq", "miu0", "ml0", "mleq", "mu0", "mueq", "optlrsav", "pback", "pbacktime", "pop0", 
          "popadj", "popasym", "prstp", "q0", "rr", "scale1", "scale2", "sig0", "sigma", "t", "t2xco2", 
          "tatm0", "tfirst", "tlast", "tnopol", "tocean0", "tstep")
pars

if (F) {
  write.table(t(as.matrix(pars)), "pars.csv", row.names = FALSE, col.names = F, sep = ", ")
  
  prm <- sapply(pars, function(x) get(x, envir = as.environment(1)))  
  names(prm)
}
 

#=============

library(NlcOptim)

#Minimizing the objective function for the OPTIMAL POLICY SCENARIO
#setting up objective function
fOBJ <- function(x, minimize = TRUE, ALL = FALSE, asTibble = FALSE, pars=NULL, dE=NULL) {
  if(is.null(dE)) dE <- rep(0, NT)
  MIU <- x[1:NT]
  S <- x[(NT+1):(2*NT)]
  
  K[1] <- k0
  YGROSS[1] <- al[1]*{(l[1]/1000)^(1-gamma)}*(K[1]^gamma)
  EIND[1] <- sigma[1]*(1-MIU[1])*YGROSS[1]
  E[1] <- EIND[1] + etree[1] + dE[1]
  CCA[1] <- cca0
  CCATOT[1] <- CCA[1] + cumetree[1]
  MAT[1] <- mat0
  MU[1] <- mu0
  ML[1] <- ml0
  FORC[1] <- fco22x * ((log((MAT[1]/588.000))/log(2))) + forcoth[1]
  TATM[1] <- tatm0
  TOCEAN[1] <- tocean0
  DAMFRAC[1] <- a1*TATM[1] + a2*(TATM[1]^a3)
  DAMAGES[1] <- YGROSS[1]*DAMFRAC[1]
  ABATECOST[1] <- YGROSS[1]*cost1[1]*MIU[1]^expcost2
  MCABATE[1] <- pbacktime[1]*MIU[1]^(expcost2 - 1)
  CPRICE[1] <- pbacktime[1]*MIU[1]^(expcost2 - 1)
  YNET[1] <- YGROSS[1]*(1-DAMFRAC[1])
  Y[1] <- YNET[1] - ABATECOST[1]
  I[1] <- S[1]*Y[1]
  C[1] <- Y[1] - I[1]
  CPC[1] <- 1000*C[1]/l[1]
  RI[1] <- (1+prstp)*(CPC[1+1]/CPC[1])^(elasmu/tstep) - 1
  PERIODU[1] <- ((C[1]*1000/l[1])^(1-elasmu)-1)/(1-elasmu)-1
  CEMUTOTPER[1] <- PERIODU[1]*l[1]*rr[1]
  UTILITY[1] <- tstep * scale1 * sum(CEMUTOTPER[1:1]) + scale2
  
  
  for(i in 2:NT) {
    K[i] <- (1-dk)^tstep * K[i-1] + tstep * I[i-1]
    YGROSS[i] <- al[i]*((l[i]/1000)^(1-gamma))*(K[i]^gamma)
    EIND[i] <- sigma[i]*(1-MIU[i])*YGROSS[i]
    E[i] <- EIND[i] + etree[i] + dE[i]
    CCA[i] <- CCA[i-1] + EIND[i-1]*5/3.666
    CCATOT[i] <- CCA[i] + cumetree[i]
    MAT[i] <- MAT[i-1]*b11 + MU[i-1]*b21 + E[i-1]*(5/3.666)
    MU[i] <- MAT[i-1]*b12 + MU[i-1]*b22 + ML[i-1]*b32
    ML[i] <- ML[i-1]*b33 + MU[i-1]*b23
    FORC[i] <- fco22x * ((log((MAT[i]/588.000))/log(2))) + forcoth[i]
    TATM[i] <- TATM[i-1] + c1*((FORC[i]-(fco22x/t2xco2)*TATM[i-1])-(c3*(TATM[i-1]-TOCEAN[i-1])))
    TOCEAN[i] <- TOCEAN[i-1] + c4*(TATM[i-1] - TOCEAN[i-1])
    DAMFRAC[i] <- a1*TATM[i] + a2*(TATM[i]^a3)
    DAMAGES[i] <- YGROSS[i]*DAMFRAC[i]
    ABATECOST[i] <- YGROSS[i]*cost1[i]*MIU[i]^expcost2
    MCABATE[i] <- pbacktime[i]*MIU[i]^(expcost2 - 1)
    CPRICE[i] <- pbacktime[i]*MIU[i]^(expcost2 - 1)
    YNET[i] <- YGROSS[i]*(1-DAMFRAC[i])
    Y[i] <- YNET[i] - ABATECOST[i]
    I[i] <- S[i] * Y[i]
    C[i] <- Y[i] - I[i]
    CPC[i] <- 1000*C[i]/l[i]
    PERIODU[i] <- ((C[i]*1000/l[i])^(1-elasmu)-1)/(1-elasmu)-1
    CEMUTOTPER[i] <- PERIODU[i]*l[i]*rr[i]
    UTILITY[i] <- tstep * scale1 * sum(CEMUTOTPER[1:i]) + scale2
  }
  
  for(i in 1: NT-1) {
    RI[i] <- (1+prstp)*(CPC[i+1]/CPC[i])^(elasmu/tstep) - 1

  }
  
  if (ALL) {
    ll <- list(
      K = K,
      YGROSS = YGROSS,
      EIND = EIND,
      E = E,
      CCA= CCA,
      CCATOT = CCATOT,
      MAT = MAT,
      MU = MU,
      ML = ML,
      FORC = FORC,
      TATM = TATM,
      TOCEAN = TOCEAN,
      DAMFRAC = DAMFRAC,
      DAMAGES = DAMAGES,
      ABATECOST = ABATECOST,
      MCABATE = MCABATE,
      CPRICE = CPRICE,
      YNET = YNET,
      Y = Y,
      I = I,
      C = C,
      CPC = CPC,
      RI = RI,
      PERIODU = PERIODU,
      CEMUTOTPER = CEMUTOTPER,
      MIU = MIU,
      S = S,
      UTILITY = UTILITY)
    
    #the tibble helps us build a data frame
    #the sapply() returns a tibble (if length(v) == length(t) TRUE) as a matrix or data frame. 
    if(asTibble) {
      ll <- sapply(ll, function(v) {
        if(length(v) == length(t)) {
          tibble(t = t, l = v)
        } else {
          tibble(l = v)
        }
      })
    }
    #if the pars vector is NOT filled with null values, then create a vector "pr".
    #pr is a matrix where if every parameter in pars has the same number of elements as the time vector,
    #then we have a tibble of a time column and a parameter value over time column FOR EACH parameter in pars.
    #otherwise, if the elements of the said parameter is NOT the same as the time vector, then
    #we have a vector of just the parameter values over time.
    #if the pars vector is filled with null values, then pars the vector is renamed as pr.
    if(!is.null(pars)) {
      if(asTibble) {
        pr <- sapply(pars, function(v) {
          if(length(v) == length(t)) {
            tibble(t = t, value = v)
          } else {
            tibble(value = v)
          }
        })
      } else {
        pr <- pars
      }
      #new ll is a vector that combines ll list from above with the new pr vector made above.
      ll = c(ll, pr)
    }
    return(ll)
  } else {
    if (minimize) cf <- -1 else cf <- 1
    return(cf * UTILITY[NT])
  }
}


x_start <- c(MIU_start, S_start)
lb_nofx <- c(MIU.lo, S.lo) 
ub_nofx <- c(MIU.up, S.up) 

#Find all MIU and S values when upper bound is equal to lower bound, and
#multiply those values by 0.9999 to ensure that we don't have fixed variables.
ii <- lb_nofx == ub_nofx
summary(ii)
lb_nofx[ii] <- lb_nofx[ii] * 0.99999
ii <- lb_nofx == ub_nofx
summary(ii)

system.time(
  ss1 <- NlcOptim::solnl(
    X = x_start, 
    objfun = fOBJ, 
    lb = lb_nofx,
    ub = ub_nofx,
    maxIter = 1e3,
    tolX = 1e-3,
    tolFun = 1e-3,
    tolCon = 1e-3)
)
  
#Included in NlcOptim results are: 
#par= the optimum solution; 
#fn= the value of the objective function at the optimal point (minima);
#counts=number of function evaluations;
#lambda= Langrangian multiplier;
#grad= gradient of objective function at the optimal point;
#hessian= Hessian of the objective function at optimal point.


ss1 # the results come out as a split vector.
ss1$counts
ss1$fn
plot(ss1$par) #Fixed here from plt()
points(ss1$par, col = "blue", type = "l", lwd = 2)

fOBJ(ss1$par)
bb <- fOBJ(ss1$par, ALL = TRUE)
bb$MIU

maketibble <- function(x, asTibble=TRUE) { #formed a tibble data frame as the fOBJ asTibble=TRUE doesn't work!
  if(length(t) == length (x)) {tibble(t=t, l=x)}
  else {tibble(l=x)}
  }

library(tibble)

bb$MIU <- maketibble(bb$MIU)
bb$RI <- maketibble(bb$RI)
bb$S <- maketibble(bb$S)
bb$DAMAGES <- maketibble(bb$DAMAGES)
bb$MAT <- maketibble(bb$MAT)
bb$TATM <- maketibble(bb$TATM)
bb$PERIODU <- maketibble(bb$PERIODU)

#========

#The SOCIAL COST OF CARBON
miu_scc <- bb$MIU$l
scc_dice <- bb$MIU
scc_dice$l <- NA

#With a variable discount rate
discount <- rep(1, NT)
for (i in 2:NT) discount[i] <- discount[i-1]*((1 + bb$RI$l[i-1])^tstep)

dE_marg <- 0.001 # Marginal emission
for(i in t) {
  dE <- rep(0, NT)
  dE[i] <- 0.001
  bb_scc <- fOBJ(c(miu_scc, bb$S$l), ALL = T, dE = dE)
  bb_scc$DAMAGES <- maketibble(bb_scc$DAMAGES)
  (dc <- bb_scc$DAMAGES$l - bb$DAMAGES$l)
  scc_dice$l[i] <- 1000 *sum(dc[i:NT]/discount[i:NT], na.rm = T)/dE_marg * discount[i]
}
scc_dice$l

#===========

#SCENARIO: NO CONTROL
#Control Variables

#Defining S(t)= savings rate 
S.lo <- rep(1e-1, NT)
S.up <- rep(0.9, NT)
S_start <- rep(0.2, NT)
S_start[S_start < S.lo] <- S.lo[S_start < S.lo]
S_start[S_start > S.up] <- S.lo[S_start > S.up]
S <- S_start

#Defining MIU(t)= emissions control rate
MIU <- rep(0.03, NT) #policymaker does not regulate emissions above 0.03%.

#TATM for this scenario
TATM2 <- rep(NA, NT)
TATM2[1] <- tatm0

#MAT for this scenario
MAT2 <- rep(NA, NT)
MAT2[1] <- mat0

#PERIODU for this scenario
PERIODU2 <- rep(NA, NT)

#Objective Function
fOBJ <- function(x, minimize = TRUE, ALL = FALSE, asTibble = FALSE, pars=NULL, dE=NULL) {
  if(is.null(dE)) dE <- rep(0, NT)
  MIU <- x[1:NT]
  S <- x[(NT+1):(2*NT)]
  
  K[1] <- k0
  YGROSS[1] <- al[1]*{(l[1]/1000)^(1-gamma)}*(K[1]^gamma)
  EIND[1] <- sigma[1]*(1-MIU[1])*YGROSS[1]
  E[1] <- EIND[1] + etree[1] + dE[1]
  CCA[1] <- cca0
  CCATOT[1] <- CCA[1] + cumetree[1]
  MAT2[1] <- mat0
  MU[1] <- mu0
  ML[1] <- ml0
  FORC[1] <- fco22x * ((log((MAT2[1]/588.000))/log(2))) + forcoth[1]
  TATM2[1] <- tatm0
  TOCEAN[1] <- tocean0
  DAMFRAC[1] <- a1*TATM2[1] + a2*(TATM2[1]^a3)
  DAMAGES[1] <- YGROSS[1]*DAMFRAC[1]
  ABATECOST[1] <- YGROSS[1]*cost1[1]*MIU[1]^expcost2
  MCABATE[1] <- pbacktime[1]*MIU[1]^(expcost2 - 1)
  CPRICE[1] <- pbacktime[1]*MIU[1]^(expcost2 - 1)
  YNET[1] <- YGROSS[1]*(1-DAMFRAC[1])
  Y[1] <- YNET[1] - ABATECOST[1]
  I[1] <- S[1]*Y[1]
  C[1] <- Y[1] - I[1]
  CPC[1] <- 1000*C[1]/l[1]
  RI[1] <- (1+prstp)*(CPC[1+1]/CPC[1])^(elasmu/tstep) - 1
  PERIODU2[1] <- ((C[1]*1000/l[1])^(1-elasmu)-1)/(1-elasmu)-1
  CEMUTOTPER[1] <- PERIODU2[1]*l[1]*rr[1]
  UTILITY[1] <- tstep * scale1 * sum(CEMUTOTPER[1:1]) + scale2
  
  
  for(i in 2:NT) {
    K[i] <- (1-dk)^tstep * K[i-1] + tstep * I[i-1]
    YGROSS[i] <- al[i]*((l[i]/1000)^(1-gamma))*(K[i]^gamma)
    EIND[i] <- sigma[i]*(1-MIU[i])*YGROSS[i]
    E[i] <- EIND[i] + etree[i] + dE[i]
    CCA[i] <- CCA[i-1] + EIND[i-1]*5/3.666
    CCATOT[i] <- CCA[i] + cumetree[i]
    MAT2[i] <- MAT2[i-1]*b11 + MU[i-1]*b21 + E[i-1]*(5/3.666)
    MU[i] <- MAT2[i-1]*b12 + MU[i-1]*b22 + ML[i-1]*b32
    ML[i] <- ML[i-1]*b33 + MU[i-1]*b23
    FORC[i] <- fco22x * ((log((MAT2[i]/588.000))/log(2))) + forcoth[i]
    TATM2[i] <- TATM2[i-1] + c1*((FORC[i]-(fco22x/t2xco2)*TATM2[i-1])-(c3*(TATM2[i-1]-TOCEAN[i-1])))
    TOCEAN[i] <- TOCEAN[i-1] + c4*(TATM2[i-1] - TOCEAN[i-1])
    DAMFRAC[i] <- a1*TATM2[i] + a2*(TATM2[i]^a3)
    DAMAGES[i] <- YGROSS[i]*DAMFRAC[i]
    ABATECOST[i] <- YGROSS[i]*cost1[i]*MIU[i]^expcost2
    MCABATE[i] <- pbacktime[i]*MIU[i]^(expcost2 - 1)
    CPRICE[i] <- pbacktime[i]*MIU[i]^(expcost2 - 1)
    YNET[i] <- YGROSS[i]*(1-DAMFRAC[i])
    Y[i] <- YNET[i] - ABATECOST[i]
    I[i] <- S[i] * Y[i]
    C[i] <- Y[i] - I[i]
    CPC[i] <- 1000*C[i]/l[i]
    PERIODU2[i] <- ((C[i]*1000/l[i])^(1-elasmu)-1)/(1-elasmu)-1
    CEMUTOTPER[i] <- PERIODU2[i]*l[i]*rr[i]
    UTILITY[i] <- tstep * scale1 * sum(CEMUTOTPER[1:i]) + scale2
  }
  
  for(i in 1: NT-1) {
    RI[i] <- (1+prstp)*(CPC[i+1]/CPC[i])^(elasmu/tstep) - 1
    
  }
  
  if (ALL) {
    ll <- list(
      K = K,
      YGROSS = YGROSS,
      EIND = EIND,
      E = E,
      CCA= CCA,
      CCATOT = CCATOT,
      MAT2 = MAT2,
      MU = MU,
      ML = ML,
      FORC = FORC,
      TATM2 = TATM2,
      TOCEAN = TOCEAN,
      DAMFRAC = DAMFRAC,
      DAMAGES = DAMAGES,
      ABATECOST = ABATECOST,
      MCABATE = MCABATE,
      CPRICE = CPRICE,
      YNET = YNET,
      Y = Y,
      I = I,
      C = C,
      CPC = CPC,
      RI = RI,
      PERIODU2 = PERIODU2,
      CEMUTOTPER = CEMUTOTPER,
      MIU = MIU,
      S = S,
      UTILITY = UTILITY)
    
    if(asTibble) {
      ll <- sapply(ll, function(v) {
        if(length(v) == length(t)) {
          tibble(t = t, l = v)
        } else {
          tibble(l = v)
        }
      })
    }

    if(!is.null(pars)) {
      if(asTibble) {
        pr <- sapply(pars, function(v) {
          if(length(v) == length(t)) {
            tibble(t = t, value = v)
          } else {
            tibble(value = v)
          }
        })
      } else {
        pr <- pars
      }
      ll = c(ll, pr)
    }
    return(ll)
  } else {
    if (minimize) cf <- -1 else cf <- 1
    return(cf * UTILITY[NT])
  }
}

#Maximized Social Welfare

ss2 <- c(MIU, S)
bb2<- fOBJ(ss2, ALL = TRUE)

library(tibble)
bb2$MIU <- maketibble(bb2$MIU)
bb2$RI <- maketibble(bb2$RI)
bb2$S <- maketibble(bb2$S)
bb2$DAMAGES <- maketibble(bb2$DAMAGES)
bb2$MAT2 <- maketibble(bb2$MAT2)
bb2$TATM2 <- maketibble(bb2$TATM2)
bb2$PERIODU2 <- maketibble(bb2$PERIODU2)

#The SOCIAL COST OF CARBON
miu_scc2 <- bb2$MIU$l
scc2_dice <- bb2$MIU
scc2_dice$l <- NA

#With a variable discount rate
discount2 <- rep(1, NT)
for (i in 2:NT) discount2[i] <- discount2[i-1]*((1 + bb2$RI$l[i-1])^tstep)

dE_marg <- 0.001 # Marginal emission
for(i in t) {
  dE <- rep(0, NT)
  dE[i] <- 0.001
  bb_scc2 <- fOBJ(c(miu_scc2, bb2$S$l), ALL = T, dE = dE)
  bb_scc2$DAMAGES <- maketibble(bb_scc2$DAMAGES)
  (dc2 <- bb_scc2$DAMAGES$l - bb2$DAMAGES$l)
  scc2_dice$l[i] <- 1000 *sum(dc2[i:NT]/discount2[i:NT], na.rm = T)/dE_marg * discount2[i]
}
scc2_dice$l

#==============
#SCENARIO: PARIS AGREEMENT

#Control Variables

#Defining S(t)= savings rate 
S.lo <- rep(1e-1, NT)
S.up <- rep(0.9, NT)
S_start <- rep(0.2, NT)
S_start[S_start < S.lo] <- S.lo[S_start < S.lo]
S_start[S_start > S.up] <- S.lo[S_start > S.up]
S <- S_start

#Defining MIU(t)= emissions control rate
MIU <- rep(0.03, NT) #initial point for policymakers is 0.03%
for(i in 2:3) {MIU[i] <- MIU[i-1]+0.01}
for(i in 4:NT) {MIU[i]<- MIU[i-1]+0.005}

#TATM for this scenario
TATM3 <- rep(NA, NT)
TATM3[1] <- tatm0

#MAT for this scenario
MAT3 <- rep(NA, NT)
MAT3[1] <- mat0

#PERIODU for this scenario
PERIODU3 <- rep(NA, NT)

#Objective Function
fOBJ <- function(x, minimize = TRUE, ALL = FALSE, asTibble = FALSE, pars=NULL, dE=NULL) {
  if(is.null(dE)) dE <- rep(0, NT)
  MIU <- x[1:NT]
  S <- x[(NT+1):(2*NT)]
  
  K[1] <- k0
  YGROSS[1] <- al[1]*{(l[1]/1000)^(1-gamma)}*(K[1]^gamma)
  EIND[1] <- sigma[1]*(1-MIU[1])*YGROSS[1]
  E[1] <- EIND[1] + etree[1] + dE[1]
  CCA[1] <- cca0
  CCATOT[1] <- CCA[1] + cumetree[1]
  MAT3[1] <- mat0
  MU[1] <- mu0
  ML[1] <- ml0
  FORC[1] <- fco22x * ((log((MAT3[1]/588.000))/log(2))) + forcoth[1]
  TATM3[1] <- tatm0
  TOCEAN[1] <- tocean0
  DAMFRAC[1] <- a1*TATM3[1] + a2*(TATM3[1]^a3)
  DAMAGES[1] <- YGROSS[1]*DAMFRAC[1]
  ABATECOST[1] <- YGROSS[1]*cost1[1]*MIU[1]^expcost2
  MCABATE[1] <- pbacktime[1]*MIU[1]^(expcost2 - 1)
  CPRICE[1] <- pbacktime[1]*MIU[1]^(expcost2 - 1)
  YNET[1] <- YGROSS[1]*(1-DAMFRAC[1])
  Y[1] <- YNET[1] - ABATECOST[1]
  I[1] <- S[1]*Y[1]
  C[1] <- Y[1] - I[1]
  CPC[1] <- 1000*C[1]/l[1]
  RI[1] <- (1+prstp)*(CPC[1+1]/CPC[1])^(elasmu/tstep) - 1
  PERIODU3[1] <- ((C[1]*1000/l[1])^(1-elasmu)-1)/(1-elasmu)-1
  CEMUTOTPER[1] <- PERIODU3[1]*l[1]*rr[1]
  UTILITY[1] <- tstep * scale1 * sum(CEMUTOTPER[1:1]) + scale2
  
  
  for(i in 2:NT) {
    K[i] <- (1-dk)^tstep * K[i-1] + tstep * I[i-1]
    YGROSS[i] <- al[i]*((l[i]/1000)^(1-gamma))*(K[i]^gamma)
    EIND[i] <- sigma[i]*(1-MIU[i])*YGROSS[i]
    E[i] <- EIND[i] + etree[i] + dE[i]
    CCA[i] <- CCA[i-1] + EIND[i-1]*5/3.666
    CCATOT[i] <- CCA[i] + cumetree[i]
    MAT3[i] <- MAT3[i-1]*b11 + MU[i-1]*b21 + E[i-1]*(5/3.666)
    MU[i] <- MAT3[i-1]*b12 + MU[i-1]*b22 + ML[i-1]*b32
    ML[i] <- ML[i-1]*b33 + MU[i-1]*b23
    FORC[i] <- fco22x * ((log((MAT3[i]/588.000))/log(2))) + forcoth[i]
    TATM3[i] <- TATM3[i-1] + c1*((FORC[i]-(fco22x/t2xco2)*TATM3[i-1])-(c3*(TATM3[i-1]-TOCEAN[i-1])))
    TOCEAN[i] <- TOCEAN[i-1] + c4*(TATM3[i-1] - TOCEAN[i-1])
    DAMFRAC[i] <- a1*TATM3[i] + a2*(TATM3[i]^a3)
    DAMAGES[i] <- YGROSS[i]*DAMFRAC[i]
    ABATECOST[i] <- YGROSS[i]*cost1[i]*MIU[i]^expcost2
    MCABATE[i] <- pbacktime[i]*MIU[i]^(expcost2 - 1)
    CPRICE[i] <- pbacktime[i]*MIU[i]^(expcost2 - 1)
    YNET[i] <- YGROSS[i]*(1-DAMFRAC[i])
    Y[i] <- YNET[i] - ABATECOST[i]
    I[i] <- S[i] * Y[i]
    C[i] <- Y[i] - I[i]
    CPC[i] <- 1000*C[i]/l[i]
    PERIODU3[i] <- ((C[i]*1000/l[i])^(1-elasmu)-1)/(1-elasmu)-1
    CEMUTOTPER[i] <- PERIODU3[i]*l[i]*rr[i]
    UTILITY[i] <- tstep * scale1 * sum(CEMUTOTPER[1:i]) + scale2
  }
  
  for(i in 1: NT-1) {
    RI[i] <- (1+prstp)*(CPC[i+1]/CPC[i])^(elasmu/tstep) - 1
    
  }
  
  if (ALL) {
    ll <- list(
      K = K,
      YGROSS = YGROSS,
      EIND = EIND,
      E = E,
      CCA= CCA,
      CCATOT = CCATOT,
      MAT3 = MAT3,
      MU = MU,
      ML = ML,
      FORC = FORC,
      TATM3 = TATM3,
      TOCEAN = TOCEAN,
      DAMFRAC = DAMFRAC,
      DAMAGES = DAMAGES,
      ABATECOST = ABATECOST,
      MCABATE = MCABATE,
      CPRICE = CPRICE,
      YNET = YNET,
      Y = Y,
      I = I,
      C = C,
      CPC = CPC,
      RI = RI,
      PERIODU3 = PERIODU3,
      CEMUTOTPER = CEMUTOTPER,
      MIU = MIU,
      S = S,
      UTILITY = UTILITY)
    
    if(asTibble) {
      ll <- sapply(ll, function(v) {
        if(length(v) == length(t)) {
          tibble(t = t, l = v)
        } else {
          tibble(l = v)
        }
      })
    }
    
    if(!is.null(pars)) {
      if(asTibble) {
        pr <- sapply(pars, function(v) {
          if(length(v) == length(t)) {
            tibble(t = t, value = v)
          } else {
            tibble(value = v)
          }
        })
      } else {
        pr <- pars
      }
      ll = c(ll, pr)
    }
    return(ll)
  } else {
    if (minimize) cf <- -1 else cf <- 1
    return(cf * UTILITY[NT])
  }
}

#Maximized Social Welfare

ss3 <- c(MIU, S)
bb3 <- fOBJ(ss3, ALL = TRUE)

library(tibble)
bb3$MIU <- maketibble(bb3$MIU)
bb3$RI <- maketibble(bb3$RI)
bb3$S <- maketibble(bb3$S)
bb3$DAMAGES <- maketibble(bb3$DAMAGES)
bb3$MAT3 <- maketibble(bb3$MAT3)
bb3$TATM3 <- maketibble(bb3$TATM3)
bb3$PERIODU3 <- maketibble(bb3$PERIODU3)

#The SOCIAL COST OF CARBON
miu_scc3 <- bb3$MIU$l
scc3_dice <- bb3$MIU
scc3_dice$l <- NA

#With a variable discount rate
discount3 <- rep(1, NT)
for (i in 2:NT) discount3[i] <- discount3[i-1]*((1 + bb3$RI$l[i-1])^tstep)

dE_marg <- 0.001 # Marginal emission
for(i in t) {
  dE <- rep(0, NT)
  dE[i] <- 0.001
  bb_scc3 <- fOBJ(c(miu_scc3, bb3$S$l), ALL = T, dE = dE)
  bb_scc3$DAMAGES <- maketibble(bb_scc3$DAMAGES)
  (dc3 <- bb_scc3$DAMAGES$l - bb3$DAMAGES$l)
  scc3_dice$l[i] <- 1000 *sum(dc3[i:NT]/discount3[i:NT], na.rm = T)/dE_marg * discount3[i]
}
scc3_dice$l

#=============
#Temperature graph
TATM.op <- bb$TATM$l #of Optimized Policy scenario
TATM.nc <- bb2$TATM2$l #of No Controls policy scenario
TATM.pa <- bb3$TATM3$l #of Paris Agreement policy scenario

TATM.df <- data.frame(t, TATM.op, TATM.nc, TATM.pa)
colnames(TATM.df) <- c("Time Period", "Optimal Policy", "No Controls", "Paris Agreement")

library(ggplot2)

legend1 <- c("Optimal Policy"="twodash", "No Controls"="dotted", "Paris Agreement"="solid")

temp <- ggplot(TATM.df, aes(x=t)) +
  geom_line(aes(y = TATM.op, linetype="Optimal Policy", group=1, ))+
  geom_line(aes(y = TATM.nc, linetype="No Controls", group=1, ))+
  geom_line(aes(y = TATM.pa, linetype="Paris Agreement", group=1, ))+
  labs(y = "Temperature Increase in Atmosphere since 1900 in degrees Celsius", x = "Time Period", lintype = "Legend") +
  scale_linetype_manual(values=unname(legend1))

temp

#=============
#Carbon concentration increase graph
MAT.op <- bb$MAT$l #of Optimized Policy scenario
MAT.nc <- bb2$MAT2$l #of No Controls scenario
MAT.pa <- bb3$MAT3$l #of Paris Agreement scenario

MAT.df <- data.frame(t, MAT.op, MAT.nc, MAT.pa)
colnames(MAT.df) <- c("Time Period", "Optimal Policy", "No Controls", "Paris Agreement")

carbconc <- ggplot(MAT.df, aes(x=t)) +
  geom_line(aes(y = MAT.op, linetype="Optimal Policy", group=1, ))+
  geom_line(aes(y = MAT.nc, linetype="No Controls", group=1, ))+
  geom_line(aes(y = MAT.pa, linetype="Paris Agreement", group=1, ))+
  labs(y = "Carbon Concentration Increase since 1750 in GtC", x = "Time Period", lintype = "Legend") +
  scale_linetype_manual(values=unname(legend1))

carbconc

#=============
#Period Utility graph
PERIODU.op <- bb$PERIODU$l #of Optimized Policy scenario
PERIODU.nc <- bb2$PERIODU2$l #of No Controls scenario
PERIODU.pa <- bb3$PERIODU3$l #of Paris Agreement scenario

PERIODU.df <- data.frame(t, PERIODU.op, PERIODU.nc, PERIODU.pa)
colnames(PERIODU.df) <- c("Time Period", "Optimal Policy", "No Controls", "Paris Agreement")

putility <- ggplot(PERIODU.df, aes(x=t)) +
  geom_line(aes(y = PERIODU.op, linetype="Optimal Policy", group=1, ))+
  geom_line(aes(y = PERIODU.nc, linetype="No Controls", group=1, ))+
  geom_line(aes(y = PERIODU.pa, linetype="Paris Agreement", group=1, ))+
  labs(y = "Utility of Society", x = "Time Period", lintype = "Legend") +
  scale_linetype_manual(values=unname(legend1))

putility

