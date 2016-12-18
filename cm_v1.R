library(glmnet)

##Read Test Data
mytestdata<-read.csv(file="/Users/leighm888/Desktop/Test_set_v2.csv", header=TRUE, sep=",")
#turn categorical variables into factors
date<-mytestdata[,1]
RETMONTH_SPX<-mytestdata[,2]
compid<-mytestdata[,3]
Close<-mytestdata[,4]
Adj_Close<-mytestdata[,5]
Volume<-mytestdata[,6]
Adj_Volume<-mytestdata[,7]
Industry<-mytestdata[,8]
RETMONTH<-mytestdata[,9]
MARKETCAP<-mytestdata[,10]
REVENUEUSD<-mytestdata[,11]
COR<-mytestdata[,12]
GP<-mytestdata[,13]
RND<-mytestdata[,14]
SGNA<-mytestdata[,15]
OPEX<-mytestdata[,16]
OPINC<-mytestdata[,17]
EBITUSD<-mytestdata[,18]
INTEXP<-mytestdata[,19]
TAXEXP<-mytestdata[,20]
CONSOLINC<-mytestdata[,21]
NETINCNCI<-mytestdata[,22]
NETINC<-mytestdata[,23]
PREFDIVIS<-mytestdata[,24]
NETINCCMNUSD<-mytestdata[,25]
EPSUSD<-mytestdata[,26]
SHARESWA<-mytestdata[,27]
DPS<-mytestdata[,28]
DEPAMOR<-mytestdata[,29]
SBCOMP<-mytestdata[,30]
CAPEX<-mytestdata[,31]
NCF<-mytestdata[,32]
ASSETS<-mytestdata[,33]
CASHNEQUSD<-mytestdata[,34]
INVENTORY<-mytestdata[,35]
LIABILITIES<-mytestdata[,36]
DEBTUSD<-mytestdata[,37]
INVESTMENTS<-mytestdata[,38]
EQUITY<-mytestdata[,39]
BM<-mytestdata[,40]
SHARESBAS<-mytestdata[,41]
SHAREFACTOR<-mytestdata[,42]
TAXASSETS<-mytestdata[,43]
TAXLIABILITIES<-mytestdata[,44]
DIVYIELD<-mytestdata[,45]
EBITDAUSD<-mytestdata[,46]
EBITDAMARGIN<-mytestdata[,47]
DE<-mytestdata[,48]
EVEBIT<-mytestdata[,49]
EVEBITDA<-mytestdata[,50]
FCFPS<-mytestdata[,51]
GROSSMARGIN<-mytestdata[,52]
NETMARGIN<-mytestdata[,53]
PE<-mytestdata[,54]
PS<-mytestdata[,55]
PB<-mytestdata[,56]
ROIC<-mytestdata[,57]
SPS<-mytestdata[,58]
PAYOUTRATIO<-mytestdata[,59]
ROA<-mytestdata[,60]
ROE<-mytestdata[,61]
ROS<-mytestdata[,62]

mytestdata$Industry=as.factor(mytestdata$Industry)
contrasts(mytestdata$Industry) = contr.treatment(213)
mytestdata$industry.f[1:213]

linear_v1<-lm(RETMONTH ~ RETMONTH_SPX + Close + Adj_Close + Volume  + 
Adj_Volume + Industry +	MARKETCAP +	REVENUEUSD + COR +GP + RND+SGNA + OPEX + OPINC + 
EBITUSD + INTEXP + TAXEXP +	CONSOLINC +	NETINCNCI	+ NETINC + PREFDIVIS+ NETINCCMNUSD +	
EPSUSD	+ SHARESWA +	DPS + 	DEPAMOR +	SBCOMP + CAPEX + NCF + ASSETS + CASHNEQUSD + 
INVENTORY + LIABILITIES + DEBTUSD + INVESTMENTS + EQUITY + BM + SHARESBAS + SHAREFACTOR + 
TAXASSETS + TAXLIABILITIES + DIVYIELD	+ EBITDAUSD	+ EBITDAMARGIN + DE + EVEBIT + EVEBITDA + 
FCFPS + GROSSMARGIN + NETMARGIN	+ PE +PS	+ PB +ROIC + SPS +PAYOUTRATIO	+ ROA	+ ROE	+ ROS)

summary(linear_v1)
##Close, Adj_Close, Volume, Adj_Volume, Industry 48, 83,138,139,149,EPSUSD,
##SHARESWA,SHARESBAS,DIVYIELD,DE,NETMARGIN,PB,ROIC,SPS,ROA
linear_sig<-lm(RETMONTH ~ RETMONTH_SPX + Close + Adj_Close + Volume + Adj_Volume + Industry + EPSUSD + 
                 SHARESWA + SHARESBAS + DIVYIELD + DE + NETMARGIN + PB + ROIC + 
                 SPS + ROA)

#define matrix of explanatory variables
exp_var<-mytestdata[,c(2,4:8,10:62)]
fit<-glmnet(exp_var,RETMONTH)
testdata_matrix<-data.matrix(exp_var)
fit=glmnet(testdata_matrix,RETMONTH)

x = model.matrix(RETMONTH~ RETMONTH_SPX + Close + Adj_Close + Volume  + 
                   Adj_Volume + Industry +  MARKETCAP +	REVENUEUSD + COR +GP + RND+SGNA + OPEX + OPINC + 
                   EBITUSD + INTEXP + TAXEXP +	CONSOLINC +	NETINCNCI	+ NETINC + PREFDIVIS+ NETINCCMNUSD +	
                   EPSUSD	+ SHARESWA +	DPS + 	DEPAMOR +	SBCOMP + CAPEX + NCF + ASSETS + CASHNEQUSD + 
                   INVENTORY + LIABILITIES + DEBTUSD + INVESTMENTS + EQUITY + BM + SHARESBAS + SHAREFACTOR + 
                   TAXASSETS + TAXLIABILITIES + DIVYIELD	+ EBITDAUSD	+ EBITDAMARGIN + DE + EVEBIT + EVEBITDA + 
                   FCFPS + GROSSMARGIN + NETMARGIN	+ PE + PS	+ PB +ROIC + SPS +PAYOUTRATIO	+ ROA	+ ROE	+ ROS,
                 data = mytestdata)
lasso<-glmnet(x, RETMONTH,alpha=1)
ridge<-glmnet(x, RETMONTH,alpha=0)
 
##Industry, Close, Volume, Industry,opinc, netincnci,epsusd,ncf,bm,sharesbas,divyield,
## ebitdamargin, de, roic, sps, payoutratio, roa

linear_lasso<-lm(RETMONTH~Close + Volume + Industry + OPINC + NETINCNCI + EPSUSD
                 + NCF + BM + SHARESBAS + DIVYIELD + EBITDAMARGIN + DE + ROIC +
                   SPS + PAYOUTRATIO + ROA)
test_set_cat<-read.csv(file="/Users/leighm888/Desktop/Test_set_cat.csv", header=TRUE, sep=",")
test_set_cat$Industry=as.factor(test_set_cat$Industry)
contrasts(test_set_cat$Industry) = contr.treatment(213)
y<-test_set_cat$RETMONTHCAT
linear_cat<-glm(y~.,data = test_set_cat)
                   

