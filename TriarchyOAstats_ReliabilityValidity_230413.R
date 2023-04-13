library(boot)
library(psych)
library(tidyverse)


volData<-read_csv("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Data/TriarchyOA_cs_FS_aseg_vol_230321.csv")
lhThickData<-read_csv("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy//Data/TriarchyOA_cs_FS_aparc_thk_lh_230321.csv")
rhThickData<-read_csv("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy//Data/TriarchyOA_cs_FS_aparc_thk_rh_230321.csv")

##Grab data from scan names
volData<-volData %>% separate(c("Date", "ID","scanID"),col = "Measure:volume",sep = "_",extra = "merge",convert = T)
lhThickData <- lhThickData %>% separate(c("Date", "ID","scanID"),col = "lh.aparc.thickness",sep = "_",extra = "merge",convert = T)
rhThickData <- rhThickData %>% separate(c("Date", "ID","scanID"),col = "rh.aparc.thickness",sep = "_",extra = "merge",convert = T)

###Make dataframes longer
volDataLong<-volData %>% pivot_longer(!(Date:scanID),names_to = "region", values_to = "volume")
lhThickLong<-lhThickData %>% pivot_longer(!(Date:scanID),names_to = "region", values_to = "thickness")
rhThickLong<-rhThickData %>% pivot_longer(!(Date:scanID),names_to = "region", values_to = "thickness")
##Add in session variable
volDataLong<-volDataLong %>% arrange(Date,ID) %>% group_by(ID) %>% mutate(session=dense_rank(Date))
lhThickLong<-lhThickLong %>% arrange(Date,ID) %>% group_by(ID) %>% mutate(session=dense_rank(Date))
rhThickLong<-rhThickLong %>% arrange(Date,ID) %>% group_by(ID) %>% mutate(session=dense_rank(Date))

##Can run this to check if dense_rank work properly there should only be 3-4 sessions, 
table(volDataLong$session)

###Keep only the older adults
volDataLong<-volDataLong %>% filter(str_detect(ID,"^P"))
lhThickLong<-lhThickLong %>% filter(str_detect(ID,"^P"))
rhThickLong<-rhThickLong %>% filter(str_detect(ID,"^P"))

##Fix naming so combining data and plotting is easier
volDataLong<-volDataLong %>% separate(c("Vox", "scanName","Repeat"),col = "scanID",sep = "_",extra = "merge") %>% mutate(Repeat=replace_na(Repeat,"a")) %>% unite("scanID",scanName,Vox,Repeat)
lhThickLong<-lhThickLong %>% separate(c("Vox", "scanName","Repeat"),col = "scanID",sep = "_",extra = "merge") %>% mutate(Repeat=replace_na(Repeat,"a")) %>% unite("scanID",scanName,Vox,Repeat)
rhThickLong<-rhThickLong %>% separate(c("Vox", "scanName","Repeat"),col = "scanID",sep = "_",extra = "merge") %>% mutate(Repeat=replace_na(Repeat,"a")) %>% unite("scanID",scanName,Vox,Repeat)

volDataLong <- rename(volDataLong,value = volume)
lhThickLong <- rename(lhThickLong,value = thickness)
rhThickLong <- rename(rhThickLong,value = thickness)

lhThickLong<-lhThickLong %>% mutate(region = gsub("lh_", "Left-", region))
rhThickLong<-rhThickLong %>% mutate(region = gsub("rh_", "Right-", region))

###Combine all data into big DF
allFSdataLong<-bind_rows(list(volData=volDataLong,lhThick=lhThickLong,rhThick=rhThickLong), .id = "origData")
allFSdataLong<- allFSdataLong %>% mutate(ID = gsub(".ANAT", "", ID))
allFSdataLong<- allFSdataLong %>% mutate(region = gsub("_thickness", "", region))



###################################
################## Stats
###################################


#######Test-retest Reliability stats for all brain regions
  
allBrainVars<- unique(allFSdataLong$region)
scanIDs<-names(table(allFSdataLong$scanID)[table(allFSdataLong$scanID) > 10000])

####Get a list of subjects that have both ADNI and Left-Hippocampus. then enforce that on the rest of the estimates so you have the same subs in all comparisons

brainVar="Left-Hippocampus"
scan<-"ADNI_1.0_a"
statsDataLong <- allFSdataLong %>% filter(scanID == scan & region == brainVar & session < 3)
statsDataLong <- statsDataLong %>% select(!Date) %>% distinct()    ###Had to add in distinct because of case of incorrect notes (P19823 CSx6_0.8_a) labeling leading to multiple rows with the same value
statsDataWide <- statsDataLong %>% pivot_wider(names_from = c(region,session), names_sep = "_Session",values_from = value) %>% drop_na() 
subIDs<-statsDataWide$ID

testRetestStats<-tibble(region=character(),scanID=character(),ICC=numeric(),Rsquared=numeric(),lowerCI=numeric(),upperCI=numeric())
for(brainVar in allBrainVars){
  for(scan in scanIDs){
    stats<-{}
    ICC<-NA
    Rsquared<-NA
    lowerCI<-NA
    upperCI<-NA
    statsDataLong <- allFSdataLong %>% filter(scanID == scan & region == brainVar & session < 3)
    statsDataLong <- statsDataLong %>% select(!Date) %>% distinct()    ###Had to add in distinct because of case of incorrect notes (P19823 CSx6_0.8_a) labeling leading to multiple rows with the same value
    statsDataWide <- statsDataLong %>% pivot_wider(names_from = c(region,session), names_sep = "_Session",values_from = value) %>% drop_na() %>% filter(ID %in% subIDs)
    numSubs<-nrow(statsDataWide)
    print(paste(scan,brainVar,numSubs))
    
    session1data<-sym(paste0(brainVar,"_Session1"))
    session2data<-sym(paste0(brainVar,"_Session2"))
    
    rSQ<-summary(lm(eval(session1data) ~ eval(session2data), data=statsDataWide))$r.squared
    slope<-as.numeric(coef(lm(eval(session1data) ~ eval(session2data), data=statsDataWide))[2])
    ICC <- ICC(statsDataWide[,4:5])$results[6,2]
    foo <- boot(as.data.frame(statsDataWide),function(data,indices)
      summary(lm(eval(session1data)~eval(session2data),data[indices,]))$r.squared,R=10000,parallel="multicore",ncpus=6)
    CI95<-quantile(foo$t,c(0.025,0.975),na.rm = T)
    
    stats<-tibble(region=brainVar,scanID=scan,ICC=ICC,Rsquared=rSQ,lowerCI=as.numeric(CI95)[1],upperCI=as.numeric(CI95)[2])
    testRetestStats<-bind_rows(testRetestStats,stats)  
  }
}
write_csv(testRetestStats,"/Users/maxwellelliott/myFiles/Manuscripts/precisionRapidMRI_Triarchy//Results/testRetestStats_230321.csv")


testRetestStats<-read_csv("/Users/maxwellelliott/myFiles/Manuscripts/precisionRapidMRI_Triarchy//Results/testRetestStats_230321.csv")
#######Validity stats for all brain regions and scan combinations
allFSdataLong<-bind_rows(list(volData=volDataLong,lhThick=lhThickLong,rhThick=rhThickLong), .id = "origData")
allFSdataLong<- allFSdataLong %>% mutate(ID = gsub(".ANAT", "", ID))

allBrainVars<- unique(allFSdataLong$region)
scanIDs<-names(table(allFSdataLong$scanID)[table(allFSdataLong$scanID) >1000])

badBrainVars<-c(allBrainVars[35:45],'SurfaceHoles', 'Ventricle','Optic-Chiasm','rhSurfaceHoles','lhSurfaceHoles','MaskVol-to-eTIV','BrainSegVol-to-eTIV','MaskVol','3rd-Ventricle','4th-Ventricle','5th-Ventricle','Brain-Stem','Left-vessel','Right-vessel','Left-choroid-plexus','Right-choroid-plexus','Left-WM-hypointensities')
goodBrainVars<- allBrainVars[!allBrainVars %in% badBrainVars]

scanIDsShort<-c("ADNI_1.0_a","CSx6_1.0_a","CSx6_1.2_a","WAVE_1.0_a","CSx6_0.8_a")
###Pull out the intercept, slope and R-squared for each model and make sure to save out the X and Y variable name so slope is interpretable

#brainVar="Left-Hippocampus"
validityStats<-tibble(region=character(),scanIDy=character(),scanIDx=character(),day=numeric(),ICC=numeric(),Rsquared=numeric(),lowerCI=numeric(),upperCI=numeric(),intercept=numeric(),slope=numeric(),maxValidity=numeric())

for(brainVar in goodBrainVars){
  for(day in c(1,2)){
    for(scanIndex1 in 1){ # currently only validity of everything to ADNI. use this for everything else - :21){
      for(scanIndex2 in scanIndex1:5){ ##Only doing for scanIDsShort for now, swith from 5 to 21 for everything
        if(scanIndex1 == scanIndex2){
          print(paste("Skipping",scanIndex1,scanIndex2))
        }else{
          scanIDy<-scanIDsShort[scanIndex1]
          scanIDx<-scanIDsShort[scanIndex2]
          stats<-{}
          ICC<-NA
          Rsquared<-NA
          lowerCI<-NA
          upperCI<-NA
          statsDataLong <- allFSdataLong %>% filter(scanID %in% c(scanIDx,scanIDy) & region == brainVar & session == day)
          statsDataLong <- statsDataLong %>% select(!Date) %>% distinct()    ###Had to add in distinct because of case of incorrect notes (P19823 CSx6_0.8_a) labeling leading to multiple rows with the same value
          statsDataWide <- statsDataLong %>% pivot_wider(names_from = c(region,scanID), names_sep = "_",values_from = value) %>% drop_na()  %>% filter(ID %in% subIDs)
          numSubs<-nrow(statsDataWide)
          print(paste(brainVar,day,scanIDy,scanIDx,numSubs))
          
          scanXdata<-sym(paste0(brainVar,"_",scanIDx))
          scanYdata<-sym(paste0(brainVar,"_",scanIDy))
          
          rSQ<-summary(lm(eval(scanYdata) ~ eval(scanXdata), data=statsDataWide))$r.squared
          intercept<-as.numeric(coef(lm(eval(scanXdata) ~ eval(scanYdata), data=statsDataWide))[1])
          slope<-as.numeric(coef(lm(eval(scanXdata) ~ eval(scanYdata), data=statsDataWide))[2])
          ICC <- ICC(statsDataWide[,4:5])$results[6,2]
          foo <- boot(as.data.frame(statsDataWide),function(data,indices)
            summary(lm(eval(scanXdata)~eval(scanYdata),data[indices,]))$r.squared,R=10000,parallel="multicore",ncpus=6)
          CI95<-quantile(foo$t,c(0.025,0.975),na.rm = T)
          
          ####If the true validity correlation was 1, what is the correlation you would observe given observed reliability of each variable
          brainVar2<-gsub("_thickness","",brainVar)
          relX <- testRetestStats %>% filter(region == brainVar2 & scanID == scanIDx) %>% select(Rsquared)
          relY <- testRetestStats %>% filter(region == brainVar2 & scanID == scanIDy) %>% select(Rsquared)
          maxValidity<-sqrt(relX)*sqrt(relY)
          
          stats<-tibble(region=brainVar,scanIDy=scanIDy,scanIDx=scanIDx,day=day,ICC=ICC,Rsquared=rSQ,intercept=intercept,slope=slope,maxValidity=as.numeric(maxValidity),lowerCI=as.numeric(CI95)[1],upperCI=as.numeric(CI95)[2])
          validityStats<-bind_rows(validityStats,stats) 
        }
      }
    }
  }
}

write_csv(validityStats,"/Users/maxwellelliott/myFiles/Manuscripts/precisionRapidMRI_Triarchy//Results/validityStats_230321.csv")



#############
#############!!!!!!!!!!!!!!!!Everything Below is exploratory and can be removed !!!!!!!!!!!!!!!!!!
#############




#####ADNI Validity for combinations of Rapid scans
###Analyses that you could do
######Save out R-squared, slope and intercept combinations of scans predicting ADNI and save out AIC/BIC compared to single rapid and compared to ADNI so you can say at what point the model is saturated and how much better/worse it is than ADNI test-retest
######Eventuall do for test-retest as well for all brain areas with rapid scan combinations as well



#####Work in progress
nFilter<-c("n=1","n=2","n=4","n=8","n=16")

combScans1<-tibble(scans=c("CSx6_1.0_a","CSx6_1.0_b","CSx6_1.0_c","CSx6_1.0_d","CSx6_1.0_e","CSx6_1.0_f","CSx6_1.0_g","CSx6_1.0_h"),xlab="CSx6")
combScans2<-tibble(scans=c("WAVE_1.0_a","WAVE_1.0_b","WAVE_1.0_c","WAVE_1.0_d"),xlab="WAVE")
combScans3<-tibble(scans=c("CSx6_1.2_a","CSx6_0.8_a","CSx6_1.0_a","CSx6_1.1_a","CSx6_0.9_a","CSx6_1.2_b","CSx6_0.8_b","CSx6_1.0_b","CSx6_1.1_b","CSx6_0.9_b"),xlab="CSx6")
combScans4<-tibble(scans=c("CSx6_1.2_a","CSx6_0.8_a","CSx6_1.0_a","CSx6_1.0_b","CSx6_1.0_c","CSx6_1.0_d","CSx6_1.1_a","CSx6_0.9_a","CSx6_1.2_b","CSx6_0.8_b","CSx6_1.0_e","CSx6_1.0_f","CSx6_1.0_g","CSx6_1.0_h","CSx6_1.1_b","CSx6_0.9_b"),xlab="CSx6")
combScans5<-tibble(scans=c("ADNI_1.0_a"),xlab="ADNI")
combScans6<-tibble(scans=c("CSx6_1.0_a","CSx6_1.0_e","CSx6_1.0_b","CSx6_1.0_f","CSx6_1.0_c","CSx6_1.0_g","CSx6_1.0_d","CSx6_1.0_h"),xlab="CSx6")
combScans7<-tibble(scans=c("CSx6_1.2_a","CSx6_1.2_b","CSx6_1.1_a","CSx6_1.1_b","CSx6_1.0_a","CSx6_1.0_b","CSx6_1.0_c","CSx6_1.0_d","CSx6_1.0_e","CSx6_1.0_f","CSx6_1.0_g","CSx6_1.0_h","CSx6_0.9_a","CSx6_0.9_b","CSx6_0.8_a","CSx6_0.8_b"),xlab="CSx6")
allScanLists<-list(CSx6_1mm=combScans1,WAVE_1mm=combScans2,CSx6_allVox=combScans3,CSx6_scanOrder=combScans4,ADNI=combScans5,CSx6_breakOrder=combScans6,CSx6_FastToSlow=combScans7)

validityStats<-tibble(region=character(),scanIDy=character(),scanIDx=character(),day=numeric(),ICC=numeric(),Rsquared=numeric(),lowerCI=numeric(),upperCI=numeric(),intercept=numeric(),slope=numeric(),maxValidity=numeric())

for(brainVar in goodBrainVars){
  for(day in c(1,2)){
    for(scanIndex1 in 2:21){
      for(scanIndex2 in scanIndex1:21){
        if(scanIndex1 == scanIndex2){
          print(paste("Skipping",scanIndex1,scanIndex2))
        }else{
          scanIDy<-scanIDs[1] ###Adni
          scanIDx1<-scanIDs[scanIndex1]
          scanIDx2<-scanIDs[scanIndex2]
          stats<-{}
          ICC<-NA
          Rsquared<-NA
          lowerCI<-NA
          upperCI<-NA
          statsDataLong <- allFSdataLong %>% filter(scanID %in% c(scanIDx1,scanIDx2,scanIDy) & region == brainVar & session == day)
          statsDataLong <- statsDataLong %>% select(!Date) %>% distinct()    ###Had to add in distinct because of case of incorrect notes (P19823 CSx6_0.8_a) labeling leading to multiple rows with the same value
          statsDataWide <- statsDataLong %>% pivot_wider(names_from = c(region,scanID), names_sep = "_",values_from = value) %>% drop_na()  %>% filter(ID %in% subIDs)
          numSubs<-nrow(statsDataWide)
          print(paste(brainVar,day,scanIDy,scanIDx1,scanIDx2,numSubs))
          
          scanX1data<-sym(paste0(brainVar,"_",scanIDx1))
          scanX2data<-sym(paste0(brainVar,"_",scanIDx2))
          scanYdata<-sym(paste0(brainVar,"_",scanIDy))
          
          rSQ<-summary(lm(eval(scanYdata) ~ eval(scanX1data), data=statsDataWide))$r.squared
          rSQcomb<-summary(lm(eval(scanYdata) ~ eval(scanX1data) + eval(scanX2data), data=statsDataWide))$r.squared
          intercept<-as.numeric(coef(lm(eval(scanXdata) ~ eval(scanYdata), data=statsDataWide))[1])
          slope<-as.numeric(coef(lm(eval(scanXdata) ~ eval(scanYdata), data=statsDataWide))[2])
          ICC <- ICC(statsDataWide[,4:5])$results[6,2]
          #foo <- boot(as.data.frame(statsDataWide),function(data,indices)
          #  summary(lm(eval(scanXdata)~eval(scanYdata),data[indices,]))$r.squared,R=10000,parallel="multicore",ncpus=6)
          #CI95<-quantile(foo$t,c(0.025,0.975),na.rm = T)
          
          ####If the true validity correlation was 1, what is the correlation you would observe given observed reliability of each variable
          relX <- testRetestStats %>% filter(region == brainVar & scanID == scanIDx) %>% select(Rsquared)
          relY <- testRetestStats %>% filter(region == brainVar & scanID == scanIDy) %>% select(Rsquared)
          maxValidity<-sqrt(relX)*sqrt(relY)
          
          stats<-tibble(region=brainVar,scanIDy=scanIDy,scanIDx=scanIDx,day=day,ICC=ICC,Rsquared=rSQ,intercept=intercept,slope=slope,maxValidity=as.numeric(maxValidity)) #lowerCI=as.numeric(CI95)[1],upperCI=as.numeric(CI95)[2],
          validityStats<-bind_rows(validityStats,stats) 
        }
      }
    }
  }
}

write_csv(validityStats,"/Users/maxwellelliott/myFiles/Manuscripts/precisionRapidMRI_Triarchy//Results/validityStats_230321.csv")





