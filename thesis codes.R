
#Call the following libraries

library(zoo)

library(xts)

library(devtools)

library(visdat)

library(dplyr)

library(psych)

library(factoextra)

library(missMDA)

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("pcaMethods")

library(BiocManager)

library(pcaMethods)

library(rrcov)



##FINDEX

#import dataset
project.final.data. <- read.csv("~/project final data..csv", header=TRUE)


#removing unwanted columns/variables i.e. variables that are beyond the scope of the study
project.final.data.<-project.final.data.[, -c(2,4,5,7:29,32,34,36,38:40,42,44:98,100:110,112:170,172:242,244:695, 697:733,735:781)]

#remove unwanted rows i.e. countries that do not belong to any of the regional blocs in Africa as well as those not present in FAS data 
project.final.data.<-project.final.data.[-c(1:3, 6:29, 32:34, 41:63, 69:79, 91:93, 95:106, 108:113, 117:131, 135:143, 146:151, 155:160, 167:222, 226:245, 249:263, 266:274, 276:278, 282:292, 296:308, 319:324, 333:381, 385:390, 396:398, 402:411, 413:424, 426, 433:442, 445:450, 457:485),]


#renaming columns originally saved as 'X' and 'X.2'
colnames(project.final.data.)[1]<-"Year"
colnames(project.final.data.)[2]<-"Economy"


#re-arrange index number. removal of the unwanted rows altered the index numbering
rownames(project.final.data.) <- 1:nrow(project.final.data.)

#Remove % symbols
project.final.data. <- as.data.frame(lapply(project.final.data., function(x) {
  gsub("%", "", x)
}))

#check and change the structure of the data where appropriate. Convert all the numeric variables from character to numeric
str(project.final.data.)
project.final.data.[3:16] = lapply(project.final.data.[3:16], as.numeric)

#converting values to decimal to reflect percentages
project.final.data.[3:16]<-project.final.data.[3:16]/100


#renaming the countries to appear the same way as they are in FAS data to ensure uniformity
project.final.data.$Economy[project.final.data.$Economy == "Swaziland"] <-"Eswatini, Kingdom of"
project.final.data.$Economy[project.final.data.$Economy == "Egypt, Arab Rep."] <-"Egypt"
project.final.data.$Economy[project.final.data.$Economy == "Congo, Dem. Rep."] <-"Congo, Democratic Republic of"
project.final.data.$Economy[project.final.data.$Economy == "Congo, Rep."] <-"Congo, Republic of"


#examine the pattern and degree of missingness
vis_miss(project.final.data.)


#Identify if there are outliers
boxplot(project.final.data.[3:16])




##FAS data
#Import dataset
project.final.data.2. <- read.csv("~/project final data 2..csv", header=TRUE)


#remove unwanted columns i.e. variables that are beyond the scope of the study
project.final.data.2.<-project.final.data.2.[, -c(3:124, 126, 127, 129:130, 133, 134, 136:145, 147:160, 162:181, 183:188)]


#remove unwanted rows  i.e. countries that do not belong to any of the regional blocs in Africa as well as those not present in FAS data 
project.final.data.2.<-project.final.data.2.[-c(1:37,39,40,42,43,45:52,54,55,57,58,60:277,279,280,282,283,285:337,339,340,342,343,345:397,399,400,402,403,405:412,414,415,417,418,420:442,444,445,447,448,450:487,489,490,492,493, 495:502,504,505,507,508,510:592,594,595,597,598, 600:607,609,610,612,613,615:622,624,625,627,628,630:652,654,655,657,658,660:727,729,730,732,733,735:787,789,790,792,793,795:847,849,850,852,853,855:907,909,910,912,913,915:967,969,970,972,973,975:1027,1029,1030,1032,1033,1035:1297,1299,1300,1302,1303,1305:1432,1434,1435,1437,1438,1440:1447,1449,1450,1452,1453, 1455:1462,1464,1465,1467,1468,1470:1507,1509,1510,1512,1513,1515:1522,1524,1525,1527,1528,1530:1567,1569,1570,1572,1573,1575:1612,1614,1615,1617,1618, 1620:1627,1629,1630,1632,1633,1635:1732,1734,1735,1737,1738,1740:1747, 1749,1750,1752,1753,1755:1777,1779,1780,1782,1783,1785:1852,1854,1855,1857,1858,1860:1867,1869,1870,1872,1873,1875:2107,2109,2110,2112,2113,2115:2182,2184,2185,2187,2188,2190:2227,2229,2230,2232,2233,2235:2302,2304,2305,2307,2308,2310:2317,2319,2320,2322,2323,2325:2407,2409,2410,2412,2413,2415:2437,2439,2440,2442,2443,2445:2512,2514,2515,2517,2518,2520:2557,2559,2560,2562,2563,2565:2602,2604,2605,2607,2608,2610:2632,2634,2635,2637,2638,2640:2812,2814,2815,2817,2818,2820:2827,2829,2830,2832,2833,2835),]


#Reorder columns so that year would come before economy, just like Findex data
project.final.data.2. <- project.final.data.2.[c(2,1,3:10)]


#re-arrange index number. removal of the unwanted rows altered the index numbering
rownames(project.final.data.2.) <- 1:nrow(project.final.data.2.)


#Check the structure of the data. Change the structure of variable called year, from integer to character
str(project.final.data.2.)
project.final.data.2.[1] = lapply(project.final.data.2.[1], as.character)


#change NAs in mobile money to zero, with the exclusion of those countries that have started mobile money services but have NAs during the year of commencement and after
          #check percentage of missingness for the mobile money variables
sapply(project.final.data.2., FUN = function(x) {sum(is.na(x))})
p<-function(x){sum(is.na(x))/length(x)*100}
apply(project.final.data.2., 2, p)


for (i in c(1:6,8:9,11:15,19:26,28:38,40:43,46:51,53:63,65:80,82,84:87,89:96,98:108,110:122,124:135,137:nrow(project.final.data.2.))){
  print
  if (is.na(project.final.data.2.[i,7])==TRUE){
    project.final.data.2.[i,7]=0
  }
}



for (j in c(1:6,8:12,14,15,19:26,28:36,38:43,45:51,53:63,65:75,77:79,82,84:87,89:93,95,96,98:102,104:108,110:122,124:nrow(project.final.data.2.))){
  print
  if (is.na(project.final.data.2.[j,10])==TRUE){
    project.final.data.2.[j,10]=0
  }
}


#Check the degree and pattern of missingness
vis_miss(project.final.data.2.)

#Check for outliers
boxplot(project.final.data.2.[3:10])


##MERGED data

#Bind both datasets by year and economy
New.data<-merge(project.final.data.2., project.final.data., by= c("Year", "Economy"), all = TRUE)


#identify the degree and pattern of missingness
vis_miss(New.data)


#check for outliers
boxplot(New.data[3:24])


# imputation with Probabilistic principal component analysis 

        #determine number of PCs
estim_ncpPCA(New.data[3:24],method.cv = "kfold",scale = TRUE)

       #impute
imp<-pcaMethods::pca(as.matrix(New.data[-98,3:24]),nPcs = 5, method = "ppca",center = TRUE, scale = "vector", set.seed=123)

       #obtain the completed dataset i.e. observed and imputed values
imp_data<-completeObs(imp)

       #Bind the completed data with year and economy in the intial data, with the exclusion of the 98th row (Burundi, 2017)
comp_data = cbind(imp_data, New.data[-98, 1:2])


#reorder data so that Year and Economy will be the first two variables
completedData <- comp_data[c(23,24,1:22)]


#re-arrange index number 
rownames(completedData) <- 1:nrow(completedData)

  
#Create a new variable and call it 'Region'
completedData$region<-NA


#re-order the columns so that the new variable 'Region' will be the third
completedData<-completedData[c(1,2,25,3:24)]


#Create new variables (Account, Savings and Loan) from existing variables
completedData$Account<-completedData$Withdrawal.in.the.past.year....with.a.financial.institution.account..age.15.. + completedData$Deposit.in.the.past.year....with.a.financial.institution.account..age.15.. +completedData$Number.of.deposit.accounts.with.commercial.banks.per.1.000.adults +
  completedData$Number.of.mobile.money.transactions..during.the.reference.year..per.1.000.adults +completedData$Account....age.15.. + completedData$No.account.because.someone.in.the.family.has.an.account....age.15.. +
  completedData$Made.or.received.digital.payments.in.the.past.year....age.15..

completedData$Savings<-completedData$Saved.for.old.age....age.15.. + completedData$Saved.at.a.financial.institution....age.15..

completedData$Loan<-completedData$Number.of.loan.accounts.with.commercial.banks.per.1.000.adults + completedData$Borrowed.from.a.financial.institution.or.used.a.credit.card....age.15..


#Remove the variables used to create new ones from the dataset to avoid duplicate
completedData<-completedData[,-c(9:13,19:21,23,24,25)]


#create regional blocs. Specify the countries that belong to each bloc

AMU = list(Algeria='')
'AMU_CEN-SAD'=list(Mauritania='',Morocco='',Tunisia='')
'AMU_CEN-SAD_COMESA'=list(Libya='')
'CEN-SAD_ECOWAS'=list(Benin='','Burkina Faso'='',"Cote d'Ivoire"='',Ghana='',Mali='', Niger='',Nigeria='',Senegal='',Togo='','Sierra Leone'='')
'CEN-SAD_ECCAS'=list('Central African Republic'='',Chad='')
'CEN-SAD_COMESA_SADC'=list(Comoros='')
'CEN-SAD_COMESA_IGAD'=list(Djibouti='',Sudan='')
'CEN-SAD_COMESA'=list(Egypt='')
'COMESA_EAC_ECCAS'=list(Burundi='', Rwanda='')
'COMESA_ECCAS_SADC'=list('Congo, Democratic Republic of'='')
'COMESA_IGAD'=list(Ethiopia='')
'COMESA_EAC_IGAD'=list(Kenya='',Uganda='')
'COMESA_SADC'=list(	Madagascar='',Malawi='',Zimbabwe='',Mauritius='',"Eswatini, Kingdom of"='',Zambia='')
'EAC_IGAD'=list('South Sudan'='')
'SADC_EAC'=list(Tanzania='')
'SADC_ECCAS'=list(Angola='')
ECCAS=list(Cameroon='','Congo, Republic of'='',Gabon='')
ECOWAS=list(Liberia='',Guinea='')
SADC=list(Botswana='',Lesotho='',Mozambique='',Namibia='','South Africa'='')


completedData$region[completedData$Economy %in% names(AMU)] <- "AMU"
completedData$region[completedData$Economy %in% names(`AMU_CEN-SAD`)] <- "AMU,CEN-SAD"
completedData$region[completedData$Economy %in% names(`AMU_CEN-SAD_COMESA`)] <- "AMU,CEN-SAD,COMESA"
completedData$region[completedData$Economy %in% names(`CEN-SAD_ECOWAS`)] <- "CEN-SAD,ECOWAS"
completedData$region[completedData$Economy %in% names(`CEN-SAD_ECCAS`)] <- "CEN-SAD,ECCAS"
completedData$region[completedData$Economy %in% names(`CEN-SAD_COMESA_SADC`)] <- "CEN-SAD,COMESA,SADC"
completedData$region[completedData$Economy %in% names(`CEN-SAD_COMESA_IGAD`)] <- "CEN-SAD,COMESA,IGAD"
completedData$region[completedData$Economy %in% names(`CEN-SAD_COMESA`)] <- "CEN-SAD,COMESA"
completedData$region[completedData$Economy %in% names(`COMESA_EAC_ECCAS`)] <- "COMESA,EAC,ECCAS"
completedData$region[completedData$Economy %in% names(`COMESA_ECCAS_SADC`)] <- "COMESA,ECCAS,SADC"
completedData$region[completedData$Economy %in% names(`COMESA_IGAD`)] <- "COMESA,IGAD"
completedData$region[completedData$Economy %in% names(`COMESA_EAC_IGAD`)] <- "COMESA,EAC,IGAD"
completedData$region[completedData$Economy %in% names(`COMESA_SADC`)] <- "COMESA,SADC"
completedData$region[completedData$Economy %in% names(`EAC_IGAD`)] <- "EAC,IGAD"
completedData$region[completedData$Economy %in% names(`SADC_EAC`)] <- "SADC,EAC"
completedData$region[completedData$Economy %in% names(`SADC_ECCAS`)] <- "SADC,ECCAS"
completedData$region[completedData$Economy %in% names(ECCAS)] <- "ECCAS"
completedData$region[completedData$Economy %in% names(ECOWAS)] <- "ECOWAS"
completedData$region[completedData$Economy %in% names(SADC)] <- "SADC"


#create subset of dataframe for each regional blocs because some countries belong to more than one regional blocs. Each subset will contain only member states of each bloc
#with the variables, over the three years period.

AMU_data <- completedData[c(1,24,28,30,43,47,70,74,76,89,93,115,119,121,134),c(1,3:17)]

SADC_data<-completedData[c(2,4,10,11,16,22,25,26,29,31,32,38,41,45,46,48,50,56,57,62,68,71,72,75,77,78,84,87,91,92,94,96,101,102,107,113,
                        116,117,120,122,123,129,132,136,137), c(1,3:17)]

EAC_data<-completedData[c(6,21,35,39,41,44,52,67,81,85,87,90,112,126,130,132,135), c(1,3:17)]

ECCAS_data<-completedData[c(2,6:9,11,12,18,35,48,52:55,57,58,64,81,94,98:100,102,103,109,126), c(1,3:17)]

ECOWAS_data<-completedData[c(3,5,13,19,20,23,27,33,34,36,37,42,49,51,59,65,66,69,73,79,80,82,83,88,95,97,104,110,111,114,118,124,125,127,
                             128,133), c(1,3:17)]

CEN_SAD_data<-completedData[c(3,5,8:10,13:15,19,24,27,28,30,33,34,36,37,40,42,43,49,51,54:56,59:61,65,70,73,74,76,79,80,82,83,86,88,
                              89,95,97,99:101,104:106,110,115,118,119,121,124,125,127,128,131,133,134), c(1,3:17)]

COMESA_data<-completedData[c(6,10,11,14:17,21,24:26,29,35,40,44:46,52,56,57,60:63,67,70:72,75,81,86,90:92,101,102,105:108,112,115:117,
                             120,126,131,135:137), c(1,3:17)]

IGAD_data<-completedData[c(14,17,21,39,40,44,60,63,67,85,86,90,105,108,112,130,131,135), c(1,3:17)]


#sum up each regional bloc by year

AMU_data$region='AMU'
AMU_sum <- AMU_data %>%
  group_by(Year, region) %>% 
  summarise(across(.cols=everything(),.fns=sum))

SADC_data$region='SADC'
SADC_sum <- SADC_data %>%
  group_by(Year, region) %>% 
  summarise(across(.cols=everything(),.fns=sum))


EAC_data$region='EAC'
EAC_sum <- EAC_data %>%
  group_by(Year, region) %>% 
  summarise(across(.cols=everything(),.fns=sum))

ECCAS_data$region='ECCAS'
ECCAS_sum <- ECCAS_data %>%
  group_by(Year, region) %>% 
  summarise(across(.cols=everything(),.fns=sum))

ECOWAS_data$region='ECOWAS'
ECOWAS_sum <- ECOWAS_data %>%
  group_by(Year, region) %>% 
  summarise(across(.cols=everything(),.fns=sum))

CEN_SAD_data$region='CEN-SAD'
CEN_SAD_sum <- CEN_SAD_data %>%
  group_by(Year, region) %>% 
  summarise(across(.cols=everything(),.fns=sum))

COMESA_data$region='COMESA'
COMESA_sum <- COMESA_data %>%
  group_by(Year, region) %>% 
  summarise(across(.cols=everything(),.fns=sum))

IGAD_data$region='IGAD'
IGAD_sum <- IGAD_data %>%
  group_by(Year, region) %>% 
  summarise(across(.cols=everything(),.fns=sum))


#bind the regional blocs dataframe
Final_data <- rbind(AMU_sum,SADC_sum,EAC_sum,ECCAS_sum,ECOWAS_sum,CEN_SAD_sum,COMESA_sum,IGAD_sum)


##DATA MINING
#Bartlett test
cor.d <-cor(Final_data[,3:16])
View(cor.d)
cortest.bartlett(cor.d,n =nrow(Final_data))

#KMO test
KMO(cor.d) 
                              
                     #First stage RPCA
#RPCA for access dimension. Specify only the indicators that make up the access dimension
pca1 <- PcaCov(Final_data[, c(3:7, 13)], scale = TRUE,center = TRUE)
summary(pca1)

#Obtain the scores for access
x<-pca1$scores

#Obtain the eigenvalues for access
variance_a = pca1$eigenvalues


#RPCA for usage dimension. Specify only the indicators that make up the usage dimension
pca12 <- PcaCov(Final_data[, c(14:16)], scale = TRUE, center=TRUE)
summary(pca12)

#obtain the scores for usage
y<-pca12$scores

#obtain the eigenvalues for usage
variance_u = pca12$eigenvalues


#RPCA for quality dimension. Specify only the indicators that make up the quality dimension
pca13 <- PcaCov(Final_data[, c(8:12)], scale = TRUE, center = TRUE)
summary(pca13)

#obtain the scores for quality
z<-pca13$scores

#obtain the eigenvalues for quality
variance_q = pca13$eigenvalues


##Calculate the sub-indices for each of the regions over the three years period. e.g. 'access_amu_2011' means the access index for AMU region in 2011

             #AMU
access_amu_2011 = ((variance_a[1]*x[1,1]) + (variance_a[2]*x[1,2]) + (variance_a[3]*x[1,3]) + (variance_a[4]*x[1,4]) + (variance_a[5]*x[1,5]) + (variance_a[6]*x[1,6]))/(variance_a[1] + variance_a[2] + variance_a[3] + variance_a[4] + variance_a[5] + variance_a[6])

access_amu_2014 = ((variance_a[1]*x[2,1]) + (variance_a[2]*x[2,2]) + (variance_a[3]*x[2,3]) + (variance_a[4]*x[2,4]) + (variance_a[5]*x[2,5]) + (variance_a[6]*x[2,6]))/(variance_a[1] + variance_a[2] + variance_a[3] + variance_a[4] + variance_a[5] + variance_a[6])

access_amu_2017 = ((variance_a[1]*x[3,1]) + (variance_a[2]*x[3,2]) + (variance_a[3]*x[3,3]) + (variance_a[4]*x[3,4]) + (variance_a[5]*x[3,5]) + (variance_a[6]*x[3,6]))/(variance_a[1] + variance_a[2] + variance_a[3] + variance_a[4] + variance_a[5] + variance_a[6])


usage_amu_2011 = ((variance_u[1]*y[1,1]) + (variance_u[2]*y[1,2]) + (variance_u[3]*y[1,3]))/(variance_u[1] + variance_u[2] + variance_u[3])

usage_amu_2014 = ((variance_u[1]*y[2,1]) + (variance_u[2]*y[2,2]) + (variance_u[3]*y[2,3]))/(variance_u[1] + variance_u[2] + variance_u[3])

usage_amu_2017 = ((variance_u[1]*y[3,1]) + (variance_u[2]*y[3,2]) + (variance_u[3]*y[3,3]))/(variance_u[1] + variance_u[2] + variance_u[3])


quality_amu_2011 = ((variance_q[1]*z[1,1]) + (variance_q[2]*z[1,2]) + (variance_q[3]*z[1,3]) + (variance_q[4]*z[1,4]) + (variance_q[5]*z[1,5]))/(variance_q[1] + variance_q[2] + variance_q[3] + variance_q[4] + variance_q[5])

quality_amu_2014 = ((variance_q[1]*z[2,1]) + (variance_q[2]*z[2,2]) + (variance_q[3]*z[2,3]) + (variance_q[4]*z[2,4]) + (variance_q[5]*z[2,5]))/(variance_q[1] + variance_q[2] + variance_q[3] + variance_q[4] + variance_q[5])

quality_amu_2017 = ((variance_q[1]*z[3,1]) + (variance_q[2]*z[3,2]) + (variance_q[3]*z[3,3]) + (variance_q[4]*z[3,4]) + (variance_q[5]*z[3,5]))/(variance_q[1] + variance_q[2] + variance_q[3] + variance_q[4] + variance_q[5])



               #SADC
#Access

access_sadc_2011 = ((variance_a[1]*x[4,1]) + (variance_a[2]*x[4,2]) + (variance_a[3]*x[4,3]) + (variance_a[4]*x[4,4]) + (variance_a[5]*x[4,5]) + (variance_a[6]*x[4,6]))/(variance_a[1] + variance_a[2] + variance_a[3] + variance_a[4] + variance_a[5] + variance_a[6])

access_sadc_2014 = ((variance_a[1]*x[5,1]) + (variance_a[2]*x[5,2]) + (variance_a[3]*x[5,3]) + (variance_a[4]*x[5,4]) + (variance_a[5]*x[5,5]) + (variance_a[6]*x[5,6]))/(variance_a[1] + variance_a[2] + variance_a[3] + variance_a[4] + variance_a[5] + variance_a[6])

access_sadc_2017 = ((variance_a[1]*x[6,1]) + (variance_a[2]*x[6,2]) + (variance_a[3]*x[6,3]) + (variance_a[4]*x[6,4]) + (variance_a[5]*x[6,5]) + (variance_a[6]*x[6,6]))/(variance_a[1] + variance_a[2] + variance_a[3] + variance_a[4] + variance_a[5] + variance_a[6])

#Usage

usage_sadc_2011 = ((variance_u[1]*y[4,1]) + (variance_u[2]*y[4,2]) + (variance_u[3]*y[4,3]))/(variance_u[1] + variance_u[2] + variance_u[3])

usage_sadc_2014 = ((variance_u[1]*y[5,1]) + (variance_u[2]*y[5,2]) + (variance_u[3]*y[5,3]))/(variance_u[1] + variance_u[2] + variance_u[3])

usage_sadc_2017 = ((variance_u[1]*y[6,1]) + (variance_u[2]*y[6,2]) + (variance_u[3]*y[6,3]))/(variance_u[1] + variance_u[2] + variance_u[3])

#quality

quality_sadc_2011 = ((variance_q[1]*z[4,1]) + (variance_q[2]*z[4,2]) + (variance_q[3]*z[4,3]) + (variance_q[4]*z[4,4]) + (variance_q[5]*z[4,5]))/(variance_q[1] + variance_q[2] + variance_q[3] + variance_q[4] + variance_q[5])

quality_sadc_2014 = ((variance_q[1]*z[5,1]) + (variance_q[2]*z[5,2]) + (variance_q[3]*z[5,3]) + (variance_q[4]*z[5,4]) + (variance_q[5]*z[5,5]))/(variance_q[1] + variance_q[2] + variance_q[3] + variance_q[4] + variance_q[5])

quality_sadc_2017 = ((variance_q[1]*z[6,1]) + (variance_q[2]*z[6,2]) + (variance_q[3]*z[6,3]) + (variance_q[4]*z[6,4]) + (variance_q[5]*z[6,5]))/(variance_q[1] + variance_q[2] + variance_q[3] + variance_q[4] + variance_q[5])


            #EAC
#Access

access_eac_2011 = ((variance_a[1]*x[7,1]) + (variance_a[2]*x[7,2]) + (variance_a[3]*x[7,3]) + (variance_a[4]*x[7,4]) + (variance_a[5]*x[7,5]) + (variance_a[6]*x[7,6]))/(variance_a[1] + variance_a[2] + variance_a[3] + variance_a[4] + variance_a[5] + variance_a[6])

access_eac_2014 = ((variance_a[1]*x[8,1]) + (variance_a[2]*x[8,2]) + (variance_a[3]*x[8,3]) + (variance_a[4]*x[8,4]) + (variance_a[5]*x[8,5]) + (variance_a[6]*x[8,6]))/(variance_a[1] + variance_a[2] + variance_a[3] + variance_a[4] + variance_a[5] + variance_a[6])

access_eac_2017 = ((variance_a[1]*x[9,1]) + (variance_a[2]*x[9,2]) + (variance_a[3]*x[9,3]) + (variance_a[4]*x[9,4]) + (variance_a[5]*x[9,5]) + (variance_a[6]*x[9,6]))/(variance_a[1] + variance_a[2] + variance_a[3] + variance_a[4] + variance_a[5] + variance_a[6])

#Usage

usage_eac_2011 = ((variance_u[1]*y[7,1]) + (variance_u[2]*y[7,2]) + (variance_u[3]*y[7,3]))/(variance_u[1] + variance_u[2] + variance_u[3])

usage_eac_2014 = ((variance_u[1]*y[8,1]) + (variance_u[2]*y[8,2]) + (variance_u[3]*y[8,3]))/(variance_u[1] + variance_u[2] + variance_u[3])

usage_eac_2017 = ((variance_u[1]*y[9,1]) + (variance_u[2]*y[9,2]) + (variance_u[3]*y[9,3]))/(variance_u[1] + variance_u[2] + variance_u[3])

#quality

quality_eac_2011 = ((variance_q[1]*z[7,1]) + (variance_q[2]*z[7,2]) + (variance_q[3]*z[7,3]) + (variance_q[4]*z[7,4]) + (variance_q[5]*z[7,5]))/(variance_q[1] + variance_q[2] + variance_q[3] + variance_q[4] + variance_q[5])

quality_eac_2014 = ((variance_q[1]*z[8,1]) + (variance_q[2]*z[8,2]) + (variance_q[3]*z[8,3]) + (variance_q[4]*z[8,4]) + (variance_q[5]*z[8,5]))/(variance_q[1] + variance_q[2] + variance_q[3] + variance_q[4] + variance_q[5])

quality_eac_2017 = ((variance_q[1]*z[9,1]) + (variance_q[2]*z[9,2]) + (variance_q[3]*z[9,3]) + (variance_q[4]*z[9,4]) + (variance_q[5]*z[9,5]))/(variance_q[1] + variance_q[2] + variance_q[3] + variance_q[4] + variance_q[5])


          #ECCAS
#Access

access_eccas_2011 = ((variance_a[1]*x[10,1]) + (variance_a[2]*x[10,2]) + (variance_a[3]*x[10,3]) + (variance_a[4]*x[10,4]) + (variance_a[5]*x[10,5]) + (variance_a[6]*x[10,6]))/(variance_a[1] + variance_a[2] + variance_a[3] + variance_a[4] + variance_a[5] + variance_a[6])

access_eccas_2014 = ((variance_a[1]*x[11,1]) + (variance_a[2]*x[11,2]) + (variance_a[3]*x[11,3]) + (variance_a[4]*x[11,4]) + (variance_a[5]*x[11,5]) + (variance_a[6]*x[11,6]))/(variance_a[1] + variance_a[2] + variance_a[3] + variance_a[4] + variance_a[5] + variance_a[6])

access_eccas_2017 = ((variance_a[1]*x[12,1]) + (variance_a[2]*x[12,2]) + (variance_a[3]*x[12,3]) + (variance_a[4]*x[12,4]) + (variance_a[5]*x[12,5]) + (variance_a[6]*x[12,6]))/(variance_a[1] + variance_a[2] + variance_a[3] + variance_a[4] + variance_a[5] + variance_a[6])

#Usage

usage_eccas_2011 = ((variance_u[1]*y[10,1]) + (variance_u[2]*y[10,2]) + (variance_u[3]*y[10,3]))/(variance_u[1] + variance_u[2] + variance_u[3])

usage_eccas_2014 = ((variance_u[1]*y[11,1]) + (variance_u[2]*y[11,2]) + (variance_u[3]*y[11,3]))/(variance_u[1] + variance_u[2] + variance_u[3])

usage_eccas_2017 = ((variance_u[1]*y[12,1]) + (variance_u[2]*y[12,2]) + (variance_u[3]*y[12,3]))/(variance_u[1] + variance_u[2] + variance_u[3])

#quality

quality_eccas_2011 = ((variance_q[1]*z[10,1]) + (variance_q[2]*z[10,2]) + (variance_q[3]*z[10,3]) + (variance_q[4]*z[10,4]) + (variance_q[5]*z[10,5]))/(variance_q[1] + variance_q[2] + variance_q[3] + variance_q[4] + variance_q[5])

quality_eccas_2014 = ((variance_q[1]*z[11,1]) + (variance_q[2]*z[11,2]) + (variance_q[3]*z[11,3]) + (variance_q[4]*z[11,4]) + (variance_q[5]*z[11,5]))/(variance_q[1] + variance_q[2] + variance_q[3] + variance_q[4] + variance_q[5])

quality_eccas_2017 = ((variance_q[1]*z[12,1]) + (variance_q[2]*z[12,2]) + (variance_q[3]*z[12,3]) + (variance_q[4]*z[12,4]) + (variance_q[5]*z[12,5]))/(variance_q[1] + variance_q[2] + variance_q[3] + variance_q[4] + variance_q[5])


       #ECOWAS

#Access

access_ecowas_2011 = ((variance_a[1]*x[13,1]) + (variance_a[2]*x[13,2]) + (variance_a[3]*x[13,3]) + (variance_a[4]*x[13,4]) + (variance_a[5]*x[13,5]) + (variance_a[6]*x[13,6]))/(variance_a[1] + variance_a[2] + variance_a[3] + variance_a[4] + variance_a[5] + variance_a[6])

access_ecowas_2014 = ((variance_a[1]*x[14,1]) + (variance_a[2]*x[14,2]) + (variance_a[3]*x[14,3]) + (variance_a[4]*x[14,4]) + (variance_a[5]*x[14,5]) + (variance_a[6]*x[14,6]))/(variance_a[1] + variance_a[2] + variance_a[3] + variance_a[4] + variance_a[5] + variance_a[6])

access_ecowas_2017 = ((variance_a[1]*x[15,1]) + (variance_a[2]*x[15,2]) + (variance_a[3]*x[15,3]) + (variance_a[4]*x[15,4]) + (variance_a[5]*x[15,5]) + (variance_a[6]*x[15,6]))/(variance_a[1] + variance_a[2] + variance_a[3] + variance_a[4] + variance_a[5] + variance_a[6])

#Usage

usage_ecowas_2011 = ((variance_u[1]*y[13,1]) + (variance_u[2]*y[13,2]) + (variance_u[3]*y[13,3]))/(variance_u[1] + variance_u[2] + variance_u[3])

usage_ecowas_2014 = ((variance_u[1]*y[14,1]) + (variance_u[2]*y[14,2]) + (variance_u[3]*y[14,3]))/(variance_u[1] + variance_u[2] + variance_u[3])

usage_ecowas_2017 = ((variance_u[1]*y[15,1]) + (variance_u[2]*y[15,2]) + (variance_u[3]*y[15,3]))/(variance_u[1] + variance_u[2] + variance_u[3])

#quality

quality_ecowas_2011 = ((variance_q[1]*z[13,1]) + (variance_q[2]*z[13,2]) + (variance_q[3]*z[13,3]) + (variance_q[4]*z[13,4]) + (variance_q[5]*z[13,5]))/(variance_q[1] + variance_q[2] + variance_q[3] + variance_q[4] + variance_q[5])

quality_ecowas_2014 = ((variance_q[1]*z[14,1]) + (variance_q[2]*z[14,2]) + (variance_q[3]*z[14,3]) + (variance_q[4]*z[14,4]) + (variance_q[5]*z[14,5]))/(variance_q[1] + variance_q[2] + variance_q[3] + variance_q[4] + variance_q[5])

quality_ecowas_2017 = ((variance_q[1]*z[15,1]) + (variance_q[2]*z[15,2]) + (variance_q[3]*z[15,3]) + (variance_q[4]*z[15,4]) + (variance_q[5]*z[15,5]))/(variance_q[1] + variance_q[2] + variance_q[3] + variance_q[4] + variance_q[5])


            #CEN-SAD

#Access

access_censad_2011 = ((variance_a[1]*x[16,1]) + (variance_a[2]*x[16,2]) + (variance_a[3]*x[16,3]) + (variance_a[4]*x[16,4]) + (variance_a[5]*x[16,5]) + (variance_a[6]*x[16,6]))/(variance_a[1] + variance_a[2] + variance_a[3] + variance_a[4] + variance_a[5] + variance_a[6])

access_censad_2014 = ((variance_a[1]*x[17,1]) + (variance_a[2]*x[17,2]) + (variance_a[3]*x[17,3]) + (variance_a[4]*x[17,4]) + (variance_a[5]*x[17,5]) + (variance_a[6]*x[17,6]))/(variance_a[1] + variance_a[2] + variance_a[3] + variance_a[4] + variance_a[5] + variance_a[6])

access_censad_2017 = ((variance_a[1]*x[18,1]) + (variance_a[2]*x[18,2]) + (variance_a[3]*x[18,3]) + (variance_a[4]*x[18,4]) + (variance_a[5]*x[18,5]) + (variance_a[6]*x[18,6]))/(variance_a[1] + variance_a[2] + variance_a[3] + variance_a[4] + variance_a[5] + variance_a[6])

#Usage

usage_censad_2011 = ((variance_u[1]*y[16,1]) + (variance_u[2]*y[16,2]) + (variance_u[3]*y[16,3]))/(variance_u[1] + variance_u[2] + variance_u[3])

usage_censad_2014 = ((variance_u[1]*y[17,1]) + (variance_u[2]*y[17,2]) + (variance_u[3]*y[17,3]))/(variance_u[1] + variance_u[2] + variance_u[3])

usage_censad_2017 = ((variance_u[1]*y[18,1]) + (variance_u[2]*y[18,2]) + (variance_u[3]*y[18,3]))/(variance_u[1] + variance_u[2] + variance_u[3])

#quality

quality_censad_2011 = ((variance_q[1]*z[16,1]) + (variance_q[2]*z[16,2]) + (variance_q[3]*z[16,3]) + (variance_q[4]*z[16,4]) + (variance_q[5]*z[16,5]))/(variance_q[1] + variance_q[2] + variance_q[3] + variance_q[4] + variance_q[5])

quality_censad_2014 = ((variance_q[1]*z[17,1]) + (variance_q[2]*z[17,2]) + (variance_q[3]*z[17,3]) + (variance_q[4]*z[17,4]) + (variance_q[5]*z[17,5]))/(variance_q[1] + variance_q[2] + variance_q[3] + variance_q[4] + variance_q[5])

quality_censad_2017 = ((variance_q[1]*z[18,1]) + (variance_q[2]*z[18,2]) + (variance_q[3]*z[18,3]) + (variance_q[4]*z[18,4]) + (variance_q[5]*z[18,5]))/(variance_q[1] + variance_q[2] + variance_q[3] + variance_q[4] + variance_q[5])



   #COMESA

#Access

access_comesa_2011 = ((variance_a[1]*x[19,1]) + (variance_a[2]*x[19,2]) + (variance_a[3]*x[19,3]) + (variance_a[4]*x[19,4]) + (variance_a[5]*x[19,5]) + (variance_a[6]*x[19,6]))/(variance_a[1] + variance_a[2] + variance_a[3] + variance_a[4] + variance_a[5] + variance_a[6])

access_comesa_2014 = ((variance_a[1]*x[20,1]) + (variance_a[2]*x[20,2]) + (variance_a[3]*x[20,3]) + (variance_a[4]*x[20,4]) + (variance_a[5]*x[20,5]) + (variance_a[6]*x[20,6]))/(variance_a[1] + variance_a[2] + variance_a[3] + variance_a[4] + variance_a[5] + variance_a[6])

access_comesa_2017 = ((variance_a[1]*x[21,1]) + (variance_a[2]*x[21,2]) + (variance_a[3]*x[21,3]) + (variance_a[4]*x[21,4]) + (variance_a[5]*x[21,5]) + (variance_a[6]*x[21,6]))/(variance_a[1] + variance_a[2] + variance_a[3] + variance_a[4] + variance_a[5] + variance_a[6])

#Usage

usage_comesa_2011 = ((variance_u[1]*y[19,1]) + (variance_u[2]*y[19,2]) + (variance_u[3]*y[19,3]))/(variance_u[1] + variance_u[2] + variance_u[3])

usage_comesa_2014 = ((variance_u[1]*y[20,1]) + (variance_u[2]*y[20,2]) + (variance_u[3]*y[20,3]))/(variance_u[1] + variance_u[2] + variance_u[3])

usage_comesa_2017 = ((variance_u[1]*y[21,1]) + (variance_u[2]*y[21,2]) + (variance_u[3]*y[21,3]))/(variance_u[1] + variance_u[2] + variance_u[3])

#quality

quality_comesa_2011 = ((variance_q[1]*z[19,1]) + (variance_q[2]*z[19,2]) + (variance_q[3]*z[19,3]) + (variance_q[4]*z[19,4]) + (variance_q[5]*z[19,5]))/(variance_q[1] + variance_q[2] + variance_q[3] + variance_q[4] + variance_q[5])

quality_comesa_2014 = ((variance_q[1]*z[20,1]) + (variance_q[2]*z[20,2]) + (variance_q[3]*z[20,3]) + (variance_q[4]*z[20,4]) + (variance_q[5]*z[20,5]))/(variance_q[1] + variance_q[2] + variance_q[3] + variance_q[4] + variance_q[5])

quality_comesa_2017 = ((variance_q[1]*z[21,1]) + (variance_q[2]*z[21,2]) + (variance_q[3]*z[21,3]) + (variance_q[4]*z[21,4]) + (variance_q[5]*z[21,5]))/(variance_q[1] + variance_q[2] + variance_q[3] + variance_q[4] + variance_q[5])


    #IGAD

#Access

access_igad_2011 = ((variance_a[1]*x[22,1]) + (variance_a[2]*x[22,2]) + (variance_a[3]*x[22,3]) + (variance_a[4]*x[22,4]) + (variance_a[5]*x[22,5]) + (variance_a[6]*x[22,6]))/(variance_a[1] + variance_a[2] + variance_a[3] + variance_a[4] + variance_a[5] + variance_a[6])

access_igad_2014 = ((variance_a[1]*x[23,1]) + (variance_a[2]*x[23,2]) + (variance_a[3]*x[23,3]) + (variance_a[4]*x[23,4]) + (variance_a[5]*x[23,5]) + (variance_a[6]*x[23,6]))/(variance_a[1] + variance_a[2] + variance_a[3] + variance_a[4] + variance_a[5] + variance_a[6])

access_igad_2017 = ((variance_a[1]*x[24,1]) + (variance_a[2]*x[24,2]) + (variance_a[3]*x[24,3]) + (variance_a[4]*x[24,4]) + (variance_a[5]*x[24,5]) + (variance_a[6]*x[24,6]))/(variance_a[1] + variance_a[2] + variance_a[3] + variance_a[4] + variance_a[5] + variance_a[6])

#Usage

usage_igad_2011 = ((variance_u[1]*y[22,1]) + (variance_u[2]*y[22,2]) + (variance_u[3]*y[22,3]))/(variance_u[1] + variance_u[2] + variance_u[3])

usage_igad_2014 = ((variance_u[1]*y[23,1]) + (variance_u[2]*y[23,2]) + (variance_u[3]*y[23,3]))/(variance_u[1] + variance_u[2] + variance_u[3])

usage_igad_2017 = ((variance_u[1]*y[24,1]) + (variance_u[2]*y[24,2]) + (variance_u[3]*y[24,3]))/(variance_u[1] + variance_u[2] + variance_u[3])

#quality

quality_igad_2011 = ((variance_q[1]*z[22,1]) + (variance_q[2]*z[22,2]) + (variance_q[3]*z[22,3]) + (variance_q[4]*z[22,4]) + (variance_q[5]*z[22,5]))/(variance_q[1] + variance_q[2] + variance_q[3] + variance_q[4] + variance_q[5])

quality_igad_2014 = ((variance_q[1]*z[23,1]) + (variance_q[2]*z[23,2]) + (variance_q[3]*z[23,3]) + (variance_q[4]*z[23,4]) + (variance_q[5]*z[23,5]))/(variance_q[1] + variance_q[2] + variance_q[3] + variance_q[4] + variance_q[5])

quality_igad_2017 = ((variance_q[1]*z[24,1]) + (variance_q[2]*z[24,2]) + (variance_q[3]*z[24,3]) + (variance_q[4]*z[24,4]) + (variance_q[5]*z[24,5]))/(variance_q[1] + variance_q[2] + variance_q[3] + variance_q[4] + variance_q[5])





#Second stage RPCA
#Construct a new data frame with the values obtained in the first stage RPCA

dimensions<-data.frame(Region=c("AMU", "AMU", "AMU", "SADC", "SADC", "SADC", "EAC", "EAC", "EAC", "ECCAS", "ECCAS", "ECCAS", "ECOWAS", "ECOWAS", "ECOWAS", "CEN-SAD",
                                "CEN-SAD", "CEN-SAD", "COMESA", "COMESA", "COMESA", "IGAD", "IGAD", "IGAD"), Year=c(2011,2014,2017,2011,2014,2017,2011,2014,2017,2011,
                                                                                                                    2014,2017,2011,2014,2017,2011,2014,2017,2011,2014,2017,2011,2014,2017), 
                       Access=c(-0.3772514, -0.1963769,-0.03171932,4.983152,6.03537,6.572411,-0.8018109,-0.3309457,-0.1659542,-0.8693233,-0.07802531,-0.3811233,-0.6081268,0.3090875,1.447674,
                                1.19314,2.329114,3.834482,4.217723,5.475329,6.870455,-1.153137,-0.6663451,0.2487494),
                       
                       Usage=c(-0.7258889,-0.7272084,-0.5803466,0.7773274,1.227597,1.752235,-0.8257315,-0.1284268, 0.07153594,-0.8495394,-0.728553,-0.4363077,-0.4728939,-0.2569936, 0.7600474,
                               0.2741932,0.5061777,1.649305,0.2196731,0.7641939,1.917447,-0.9002182,-0.519438,-0.1192123), 
                       
                       Quality=c(-1.723847,-1.749739,-1.928579,0.6538879,0.2439152,0.9414272,-1.280153,-1.517817,-1.62059,-0.3011773,-0.6523402,-0.5597136,0.5847649,0.3390339,0.3759769,
                                 2.821685,2.432977,2.499219,1.660683,1.092067,1.106844,-1.22018,-1.383762,-1.36899))

#Perform RPCA on the new variables (Access, Usage, and Quality)
pca_final <- PcaCov(dimensions[, c(3:5)])
summary(pca_final)

#Obtain the eigenvalues
variance_final<-pca_final$eigenvalues

#Obtain the loadings or eigenvector
vec<-pca_final$loadings

#Substitute the values obtained in equation 9, 10, and 11 in the report. e.g. 'p1_amu_2011', 'p12_amu_2011' and 'p13_amu_2011' means the principal component for AMU region in 2011

#AMU
#2011
p1_amu_2011<-(vec[1,1]*access_amu_2011)+(vec[1,2]*usage_amu_2011)+(vec[1,3]*quality_amu_2011)
p12_amu_2011<-(vec[2,1]*access_amu_2011)+(vec[2,2]*usage_amu_2011)+(vec[2,3]*quality_amu_2011)
p13_amu_2011<-(vec[3,1]*access_amu_2011)+(vec[3,2]*usage_amu_2011)+(vec[3,3]*quality_amu_2011)

#2014
p1_amu_2014<-(vec[1,1]*access_amu_2014)+(vec[1,2]*usage_amu_2014)+(vec[1,3]*quality_amu_2014)
p12_amu_2014<-(vec[2,1]*access_amu_2014)+(vec[2,2]*usage_amu_2014)+(vec[2,3]*quality_amu_2014)
p13_amu_2014<-(vec[3,1]*access_amu_2014)+(vec[3,2]*usage_amu_2014)+(vec[3,3]*quality_amu_2014)

#2017
p1_amu_2017<-(vec[1,1]*access_amu_2017)+(vec[1,2]*usage_amu_2017)+(vec[1,3]*quality_amu_2017)
p12_amu_2017<-(vec[2,1]*access_amu_2017)+(vec[2,2]*usage_amu_2017)+(vec[2,3]*quality_amu_2017)
p13_amu_2017<-(vec[3,1]*access_amu_2017)+(vec[3,2]*usage_amu_2017)+(vec[3,3]*quality_amu_2017)


#SADC
#2011
p1_sadc_2011<-(vec[1,1]*access_sadc_2011)+(vec[1,2]*usage_sadc_2011)+(vec[1,3]*quality_sadc_2011)
p12_sadc_2011<-(vec[2,1]*access_sadc_2011)+(vec[2,2]*usage_sadc_2011)+(vec[2,3]*quality_sadc_2011)
p13_sadc_2011<-(vec[3,1]*access_sadc_2011)+(vec[3,2]*usage_sadc_2011)+(vec[3,3]*quality_sadc_2011)

#2014
p1_sadc_2014<-(vec[1,1]*access_sadc_2014)+(vec[1,2]*usage_sadc_2014)+(vec[1,3]*quality_sadc_2014)
p12_sadc_2014<-(vec[2,1]*access_sadc_2014)+(vec[2,2]*usage_sadc_2014)+(vec[2,3]*quality_sadc_2014)
p13_sadc_2014<-(vec[3,1]*access_sadc_2014)+(vec[3,2]*usage_sadc_2014)+(vec[3,3]*quality_sadc_2014)

#2017
p1_sadc_2017<-(vec[1,1]*access_sadc_2017)+(vec[1,2]*usage_sadc_2017)+(vec[1,3]*quality_sadc_2017)
p12_sadc_2017<-(vec[2,1]*access_sadc_2017)+(vec[2,2]*usage_sadc_2017)+(vec[2,3]*quality_sadc_2017)
p13_sadc_2017<-(vec[3,1]*access_sadc_2017)+(vec[3,2]*usage_sadc_2017)+(vec[3,3]*quality_sadc_2017)


#EAC
#2011
p1_eac_2011<-(vec[1,1]*access_eac_2011)+(vec[1,2]*usage_eac_2011)+(vec[1,3]*quality_eac_2011)
p12_eac_2011<-(vec[2,1]*access_eac_2011)+(vec[2,2]*usage_eac_2011)+(vec[2,3]*quality_eac_2011)
p13_eac_2011<-(vec[3,1]*access_eac_2011)+(vec[3,2]*usage_eac_2011)+(vec[3,3]*quality_eac_2011)

#2014
p1_eac_2014<-(vec[1,1]*access_eac_2014)+(vec[1,2]*usage_eac_2014)+(vec[1,3]*quality_eac_2014)
p12_eac_2014<-(vec[2,1]*access_eac_2014)+(vec[2,2]*usage_eac_2014)+(vec[2,3]*quality_eac_2014)
p13_eac_2014<-(vec[3,1]*access_eac_2014)+(vec[3,2]*usage_eac_2014)+(vec[3,3]*quality_eac_2014)

#2017
p1_eac_2017<-(vec[1,1]*access_eac_2017)+(vec[1,2]*usage_eac_2017)+(vec[1,3]*quality_eac_2017)
p12_eac_2017<-(vec[2,1]*access_eac_2017)+(vec[2,2]*usage_eac_2017)+(vec[2,3]*quality_eac_2017)
p13_eac_2017<-(vec[3,1]*access_eac_2017)+(vec[3,2]*usage_eac_2017)+(vec[3,3]*quality_eac_2017)


#ECCAS
#2011
p1_eccas_2011<-(vec[1,1]*access_eccas_2011)+(vec[1,2]*usage_eccas_2011)+(vec[1,3]*quality_eccas_2011)
p12_eccas_2011<-(vec[2,1]*access_eccas_2011)+(vec[2,2]*usage_eccas_2011)+(vec[2,3]*quality_eccas_2011)
p13_eccas_2011<-(vec[3,1]*access_eccas_2011)+(vec[3,2]*usage_eccas_2011)+(vec[3,3]*quality_eccas_2011)

#2014
p1_eccas_2014<-(vec[1,1]*access_eccas_2014)+(vec[1,2]*usage_eccas_2014)+(vec[1,3]*quality_eccas_2014)
p12_eccas_2014<-(vec[2,1]*access_eccas_2014)+(vec[2,2]*usage_eccas_2014)+(vec[2,3]*quality_eccas_2014)
p13_eccas_2014<-(vec[3,1]*access_eccas_2014)+(vec[3,2]*usage_eccas_2014)+(vec[3,3]*quality_eccas_2014)

#2017
p1_eccas_2017<-(vec[1,1]*access_eccas_2017)+(vec[1,2]*usage_eccas_2017)+(vec[1,3]*quality_eccas_2017)
p12_eccas_2017<-(vec[2,1]*access_eccas_2017)+(vec[2,2]*usage_eccas_2017)+(vec[2,3]*quality_eccas_2017)
p13_eccas_2017<-(vec[3,1]*access_eccas_2017)+(vec[3,2]*usage_eccas_2017)+(vec[3,3]*quality_eccas_2017)


#ECOWAS
#2011
p1_ecowas_2011<-(vec[1,1]*access_ecowas_2011)+(vec[1,2]*usage_ecowas_2011)+(vec[1,3]*quality_ecowas_2011)
p12_ecowas_2011<-(vec[2,1]*access_ecowas_2011)+(vec[2,2]*usage_ecowas_2011)+(vec[2,3]*quality_ecowas_2011)
p13_ecowas_2011<-(vec[3,1]*access_ecowas_2011)+(vec[3,2]*usage_ecowas_2011)+(vec[3,3]*quality_ecowas_2011)

#2014
p1_ecowas_2014<-(vec[1,1]*access_ecowas_2014)+(vec[1,2]*usage_ecowas_2014)+(vec[1,3]*quality_ecowas_2014)
p12_ecowas_2014<-(vec[2,1]*access_ecowas_2014)+(vec[2,2]*usage_ecowas_2014)+(vec[2,3]*quality_ecowas_2014)
p13_ecowas_2014<-(vec[3,1]*access_ecowas_2014)+(vec[3,2]*usage_ecowas_2014)+(vec[3,3]*quality_ecowas_2014)

#2017
p1_ecowas_2017<-(vec[1,1]*access_ecowas_2017)+(vec[1,2]*usage_ecowas_2017)+(vec[1,3]*quality_ecowas_2017)
p12_ecowas_2017<-(vec[2,1]*access_ecowas_2017)+(vec[2,2]*usage_ecowas_2017)+(vec[2,3]*quality_ecowas_2017)
p13_ecowas_2017<-(vec[3,1]*access_ecowas_2017)+(vec[3,2]*usage_ecowas_2017)+(vec[3,3]*quality_ecowas_2017)


#CEN-SAD
#2011
p1_censad_2011<-(vec[1,1]*access_censad_2011)+(vec[1,2]*usage_censad_2011)+(vec[1,3]*quality_censad_2011)
p12_censad_2011<-(vec[2,1]*access_censad_2011)+(vec[2,2]*usage_censad_2011)+(vec[2,3]*quality_censad_2011)
p13_censad_2011<-(vec[3,1]*access_censad_2011)+(vec[3,2]*usage_censad_2011)+(vec[3,3]*quality_censad_2011)

#2014
p1_censad_2014<-(vec[1,1]*access_censad_2014)+(vec[1,2]*usage_censad_2014)+(vec[1,3]*quality_censad_2014)
p12_censad_2014<-(vec[2,1]*access_censad_2014)+(vec[2,2]*usage_censad_2014)+(vec[2,3]*quality_censad_2014)
p13_censad_2014<-(vec[3,1]*access_censad_2014)+(vec[3,2]*usage_censad_2014)+(vec[3,3]*quality_censad_2014)

#2017
p1_censad_2017<-(vec[1,1]*access_censad_2017)+(vec[1,2]*usage_censad_2017)+(vec[1,3]*quality_censad_2017)
p12_censad_2017<-(vec[2,1]*access_censad_2017)+(vec[2,2]*usage_censad_2017)+(vec[2,3]*quality_censad_2017)
p13_censad_2017<-(vec[3,1]*access_censad_2017)+(vec[3,2]*usage_censad_2017)+(vec[3,3]*quality_censad_2017)


#COMESA
#2011
p1_comesa_2011<-(vec[1,1]*access_comesa_2011)+(vec[1,2]*usage_comesa_2011)+(vec[1,3]*quality_comesa_2011)
p12_comesa_2011<-(vec[2,1]*access_comesa_2011)+(vec[2,2]*usage_comesa_2011)+(vec[2,3]*quality_comesa_2011)
p13_comesa_2011<-(vec[3,1]*access_comesa_2011)+(vec[3,2]*usage_comesa_2011)+(vec[3,3]*quality_comesa_2011)

#2014
p1_comesa_2014<-(vec[1,1]*access_comesa_2014)+(vec[1,2]*usage_comesa_2014)+(vec[1,3]*quality_comesa_2014)
p12_comesa_2014<-(vec[2,1]*access_comesa_2014)+(vec[2,2]*usage_comesa_2014)+(vec[2,3]*quality_comesa_2014)
p13_comesa_2014<-(vec[3,1]*access_comesa_2014)+(vec[3,2]*usage_comesa_2014)+(vec[3,3]*quality_comesa_2014)

#2017
p1_comesa_2017<-(vec[1,1]*access_comesa_2017)+(vec[1,2]*usage_comesa_2017)+(vec[1,3]*quality_comesa_2017)
p12_comesa_2017<-(vec[2,1]*access_comesa_2017)+(vec[2,2]*usage_comesa_2017)+(vec[2,3]*quality_comesa_2017)
p13_comesa_2017<-(vec[3,1]*access_comesa_2017)+(vec[3,2]*usage_comesa_2017)+(vec[3,3]*quality_comesa_2017)


#IGAD
#2011
p1_igad_2011<-(vec[1,1]*access_igad_2011)+(vec[1,2]*usage_igad_2011)+(vec[1,3]*quality_igad_2011)
p12_igad_2011<-(vec[2,1]*access_igad_2011)+(vec[2,2]*usage_igad_2011)+(vec[2,3]*quality_igad_2011)
p13_igad_2011<-(vec[3,1]*access_igad_2011)+(vec[3,2]*usage_igad_2011)+(vec[3,3]*quality_igad_2011)

#2014
p1_igad_2014<-(vec[1,1]*access_igad_2014)+(vec[1,2]*usage_igad_2014)+(vec[1,3]*quality_igad_2014)
p12_igad_2014<-(vec[2,1]*access_igad_2014)+(vec[2,2]*usage_igad_2014)+(vec[2,3]*quality_igad_2014)
p13_igad_2014<-(vec[3,1]*access_igad_2014)+(vec[3,2]*usage_igad_2014)+(vec[3,3]*quality_igad_2014)

#2017
p1_igad_2017<-(vec[1,1]*access_igad_2017)+(vec[1,2]*usage_igad_2017)+(vec[1,3]*quality_igad_2017)
p12_igad_2017<-(vec[2,1]*access_igad_2017)+(vec[2,2]*usage_igad_2017)+(vec[2,3]*quality_igad_2017)
p13_igad_2017<-(vec[3,1]*access_igad_2017)+(vec[3,2]*usage_igad_2017)+(vec[3,3]*quality_igad_2017)


# Estimate the Financial inclusion index scores for each regional bloc over the three years period, by substituting the values in equation 12
#FI_amu_2011 means financial index for AMU region in 2011.

#AMU
FI_amu_2011<-((variance_final[1]*p1_amu_2011)+(variance_final[2]*p12_amu_2011)+(variance_final[3]*p13_amu_2011))/(variance_final[1]+variance_final[2]+variance_final[3])

FI_amu_2014<-((variance_final[1]*p1_amu_2014)+(variance_final[2]*p12_amu_2014)+(variance_final[3]*p13_amu_2014))/(variance_final[1]+variance_final[2]+variance_final[3])

FI_amu_2017<-((variance_final[1]*p1_amu_2017)+(variance_final[2]*p12_amu_2017)+(variance_final[3]*p13_amu_2017))/(variance_final[1]+variance_final[2]+variance_final[3])


#SADC
FI_sadc_2011<-((variance_final[1]*p1_sadc_2011)+(variance_final[2]*p12_sadc_2011)+(variance_final[3]*p13_sadc_2011))/(variance_final[1]+variance_final[2]+variance_final[3])

FI_sadc_2014<-((variance_final[1]*p1_sadc_2014)+(variance_final[2]*p12_sadc_2014)+(variance_final[3]*p13_sadc_2014))/(variance_final[1]+variance_final[2]+variance_final[3])

FI_sadc_2017<-((variance_final[1]*p1_sadc_2017)+(variance_final[2]*p12_sadc_2017)+(variance_final[3]*p13_sadc_2017))/(variance_final[1]+variance_final[2]+variance_final[3])


#EAC
FI_eac_2011<-((variance_final[1]*p1_eac_2011)+(variance_final[2]*p12_eac_2011)+(variance_final[3]*p13_eac_2011))/(variance_final[1]+variance_final[2]+variance_final[3])

FI_eac_2014<-((variance_final[1]*p1_eac_2014)+(variance_final[2]*p12_eac_2014)+(variance_final[3]*p13_eac_2014))/(variance_final[1]+variance_final[2]+variance_final[3])

FI_eac_2017<-((variance_final[1]*p1_eac_2017)+(variance_final[2]*p12_eac_2017)+(variance_final[3]*p13_eac_2017))/(variance_final[1]+variance_final[2]+variance_final[3])


#ECCAS
FI_eccas_2011<-((variance_final[1]*p1_eccas_2011)+(variance_final[2]*p12_eccas_2011)+(variance_final[3]*p13_eccas_2011))/(variance_final[1]+variance_final[2]+variance_final[3])

FI_eccas_2014<-((variance_final[1]*p1_eccas_2014)+(variance_final[2]*p12_eccas_2014)+(variance_final[3]*p13_eccas_2014))/(variance_final[1]+variance_final[2]+variance_final[3])

FI_eccas_2017<-((variance_final[1]*p1_eccas_2017)+(variance_final[2]*p12_eccas_2017)+(variance_final[3]*p13_eccas_2017))/(variance_final[1]+variance_final[2]+variance_final[3])


#ECOWAS
FI_ecowas_2011<-((variance_final[1]*p1_ecowas_2011)+(variance_final[2]*p12_ecowas_2011)+(variance_final[3]*p13_ecowas_2011))/(variance_final[1]+variance_final[2]+variance_final[3])

FI_ecowas_2014<-((variance_final[1]*p1_ecowas_2014)+(variance_final[2]*p12_ecowas_2014)+(variance_final[3]*p13_ecowas_2014))/(variance_final[1]+variance_final[2]+variance_final[3])

FI_ecowas_2017<-((variance_final[1]*p1_ecowas_2017)+(variance_final[2]*p12_ecowas_2017)+(variance_final[3]*p13_ecowas_2017))/(variance_final[1]+variance_final[2]+variance_final[3])


#CEN-SAD
FI_censad_2011<-((variance_final[1]*p1_censad_2011)+(variance_final[2]*p12_censad_2011)+(variance_final[3]*p13_censad_2011))/(variance_final[1]+variance_final[2]+variance_final[3])

FI_censad_2014<-((variance_final[1]*p1_censad_2014)+(variance_final[2]*p12_censad_2014)+(variance_final[3]*p13_censad_2014))/(variance_final[1]+variance_final[2]+variance_final[3])

FI_censad_2017<-((variance_final[1]*p1_censad_2017)+(variance_final[2]*p12_censad_2017)+(variance_final[3]*p13_censad_2017))/(variance_final[1]+variance_final[2]+variance_final[3])


#COMESA
FI_comesa_2011<-((variance_final[1]*p1_comesa_2011)+(variance_final[2]*p12_comesa_2011)+(variance_final[3]*p13_comesa_2011))/(variance_final[1]+variance_final[2]+variance_final[3])

FI_comesa_2014<-((variance_final[1]*p1_comesa_2014)+(variance_final[2]*p12_comesa_2014)+(variance_final[3]*p13_comesa_2014))/(variance_final[1]+variance_final[2]+variance_final[3])

FI_comesa_2017<-((variance_final[1]*p1_comesa_2017)+(variance_final[2]*p12_comesa_2017)+(variance_final[3]*p13_comesa_2017))/(variance_final[1]+variance_final[2]+variance_final[3])


#IGAD
FI_igad_2011<-((variance_final[1]*p1_igad_2011)+(variance_final[2]*p12_igad_2011)+(variance_final[3]*p13_igad_2011))/(variance_final[1]+variance_final[2]+variance_final[3])

FI_igad_2014<-((variance_final[1]*p1_igad_2014)+(variance_final[2]*p12_igad_2014)+(variance_final[3]*p13_igad_2014))/(variance_final[1]+variance_final[2]+variance_final[3])

FI_igad_2017<-((variance_final[1]*p1_igad_2017)+(variance_final[2]*p12_igad_2017)+(variance_final[3]*p13_igad_2017))/(variance_final[1]+variance_final[2]+variance_final[3])



