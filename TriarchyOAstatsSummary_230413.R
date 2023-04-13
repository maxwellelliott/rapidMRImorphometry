
library(tidyverse)

####Summaries and stats for the manuscript

TRstats<-read_csv("/Users/maxwellelliott/myFiles/Manuscripts/precisionRapidMRI_Triarchy/Results/testRetestStats_230321.csv")#read in test-retest stats
VALstats <- read_csv("/Users/maxwellelliott/myFiles/Manuscripts/precisionRapidMRI_Triarchy/Results/validityStats_230321.csv")
mriQCstats <- read_csv("/Users/maxwellelliott/myFiles/Manuscripts/precisionRapidMRI_Triarchy/Data/mriQC_230328.csv") 
mriQCstats <-mriQCstats %>% mutate(across('scanID', str_replace, 'ADNI_1.0_', 'ADNI_1.0_a'))


##Get all thickness regions
lhThickData <- read_csv("/Users/maxwellelliott/myFiles/Manuscripts/precisionRapidMRI_Triarchy/Data/TriarchyOA_cs_FS_aparc_thk_lh_230321.csv")
lhThickData <- lhThickData %>% separate(c("Date", "ID","scanID"),col = "lh.aparc.thickness",sep = "_",extra = "merge",convert = T)
lhThickLong <- lhThickData %>% pivot_longer(!(Date:scanID),names_to = "region", values_to = "thickness")
thickRegions <- unique(lhThickLong$region) %>% gsub("lh_","",.) %>% gsub("_thickness","",.)
thickRegions <- thickRegions[1:34]

##Select TR and Val for 68 thickness and 16 subcortical volumes
volTRstats<-TRstats %>% filter(str_detect(scanID, "_1.0_a")) %>% filter(str_detect(region,"Amygdala|Accumbens|Pallidum|Caudate|Hippocampus|Putamen|Thalamus|VentralDC"))
volVALstats<-VALstats %>% filter(str_detect(scanIDx, "_1.0_a")) %>% filter(str_detect(region,"Amygdala|Accumbens|Pallidum|Caudate|Hippocampus|Putamen|Thalamus|VentralDC"))

thickTRstats <- TRstats %>% filter(str_detect(scanID, "_1.0_a")) %>% filter(str_detect(region, paste(thickRegions, collapse="|")))
thickVALstats <- VALstats %>% filter(str_detect(scanIDx, "_1.0_a")) %>% filter(str_detect(region, paste(thickRegions, collapse="|")))

##get mean and SD for paper
volTRstats %>% group_by(scanID) %>% summarise(mean=mean(Rsquared),se=sd(Rsquared)/sqrt(length(Rsquared)),relGT75=sum(Rsquared>.75)/length(Rsquared),relGT90=sum(Rsquared>.90)/length(Rsquared))
thickTRstats %>% group_by(scanID) %>% summarise(mean=mean(Rsquared),se=sd(Rsquared)/sqrt(length(Rsquared)),relGT75=sum(Rsquared>.75)/length(Rsquared),relGT90=sum(Rsquared>.90)/length(Rsquared))

bind_rows(volTRstats,thickTRstats) %>% group_by(scanID) %>% summarise(mean=mean(Rsquared),se=sd(Rsquared)/sqrt(length(Rsquared)),relGT75=sum(Rsquared>.75)/length(Rsquared),relGT90=sum(Rsquared>.90)/length(Rsquared))

p1<-ggplot(thickTRstats, aes(x=scanID, y=Rsquared,fill=scanID)) +
  geom_violin() + geom_boxplot(width=0.1,outlier.alpha = .3) + theme_classic() +
  scale_colour_manual(values = c("cyan", "chartreuse", "dark orange"))

###Validity summary
volVALstats %>% group_by(scanIDx) %>% summarise(mean=mean(Rsquared),se=sd(Rsquared)/sqrt(length(Rsquared)),relGT75=sum(Rsquared>.75)/length(Rsquared),relGT90=sum(Rsquared>.90)/length(Rsquared))
thickVALstats %>% group_by(scanIDx) %>% summarise(mean=mean(Rsquared),se=sd(Rsquared)/sqrt(length(Rsquared)),relGT75=sum(Rsquared>.75)/length(Rsquared),relGT90=sum(Rsquared>.90)/length(Rsquared))

bind_rows(volVALstats,thickVALstats) %>% group_by(scanIDx) %>% summarise(mean=mean(Rsquared),se=sd(Rsquared)/sqrt(length(Rsquared)),relGT75=sum(Rsquared>.75)/length(Rsquared),relGT50=sum(Rsquared>.50)/length(Rsquared))

####Max validity summary
volVALstats %>% group_by(scanIDx) %>% summarise(mean=mean(Rsquared/maxValidity),se=sd(Rsquared/maxValidity)/sqrt(length(Rsquared)),relGT75=sum((Rsquared/maxValidity)>.75)/length(Rsquared),relGT50=sum((Rsquared/maxValidity)>.50)/length(Rsquared))
thickVALstats %>% group_by(scanIDx) %>% summarise(mean=mean(Rsquared/maxValidity),se=sd(Rsquared/maxValidity)/sqrt(length(Rsquared)),relGT75=sum((Rsquared/maxValidity)>.75)/length(Rsquared),relGT50=sum((Rsquared/maxValidity)>.50)/length(Rsquared))

bind_rows(volVALstats,thickVALstats) %>% group_by(scanIDx) %>% summarise(mean=mean(Rsquared/maxValidity),se=sd(Rsquared)/sqrt(length(Rsquared)),relGT75=sum((Rsquared/maxValidity)>.75)/length(Rsquared),relGT50=sum((Rsquared/maxValidity)>.50)/length(Rsquared))

###Slope summary
volVALstats %>% group_by(scanIDx) %>% summarise(mean=mean(slope),se=sd(slope)/sqrt(length(slope)))
thickVALstats %>% group_by(scanIDx) %>% summarise(mean=mean(slope),se=sd(slope)/sqrt(length(slope)))

bind_rows(volVALstats,thickVALstats) %>% group_by(scanIDx) %>% summarise(mean=mean(slope),se=sd(Rsquared)/sqrt(length(slope)),gt90=sum(slope>.90)/length(slope))


##Summarize error for paper

allFSdataLong<-read_csv("/Users/maxwellelliott/myFiles/Manuscripts/precisionRapidMRI_Triarchy/Results/allFSdataLong_230321.csv")

allFSdataLong <- allFSdataLong %>% filter(session < 3)
allFSdataLong <- allFSdataLong %>% select(!Date)  %>% distinct()  
allFSdataWide <- allFSdataLong %>% pivot_wider(names_from = c(session), names_prefix ="Session",values_from = value) %>% drop_na()
allFSdataLong<- allFSdataLong %>% filter(!str_detect("P10395",ID))
allFSdataLong %>% select(ID) %>% unique() %>% nrow()

allFSdataWide <- allFSdataWide %>% mutate(percentError = abs(Session1 - Session2)/(0.5*(Session1 + Session2))*100,absError = abs(Session1 - Session2))

volFSdataWide <- allFSdataWide %>% filter(str_detect(scanID, "_1.0_a|CSx6_1.2_a|CSx6_0.8_a")) %>% filter(str_detect(region,"Amygdala|Accumbens|Pallidum|Caudate|Hippocampus|Putamen|Thalamus|VentralDC"))
thickFSdataWide <- allFSdataWide %>% filter(str_detect(scanID, "_1.0_a|CSx6_1.2_a|CSx6_0.8_a")) %>% filter(str_detect(region, paste(thickRegions, collapse="|")))

volFSdataWide %>% group_by(scanID,region) %>% summarise(mean=mean(percentError),meanAbs=mean(absError)) %>% group_by(scanID) %>% summarise(perLT150=sum(meanAbs<150)/length(meanAbs),mean=mean(meanAbs),sd=sd(meanAbs))
thickFSdataWide %>% group_by(scanID,region) %>% summarise(mean=mean(percentError),meanAbs=mean(absError)) %>% group_by(scanID) %>% summarise(perLT10=sum(meanAbs<.10)/length(meanAbs),mean=mean(meanAbs),sd=sd(meanAbs))

volFSdataWide %>% group_by(scanID) %>% summarise(meanPer=mean(percentError),meanAbs=mean(absError),sd=sd(absError)) 
thickFSdataWide %>% group_by(scanID) %>% summarise(meanPer=mean(percentError),meanAbs=mean(absError),sd=sd(absError))

bind_rows(volFSdataWide,thickFSdataWide) %>% group_by(scanID)  %>% summarise(meanPer=mean(percentError),sdPer=sd(percentError),perLT3=sum(percentError<3)/length(percentError)) 

volFSdataWide %>% group_by(scanID,region) %>% summarise(mean=mean(Session1)) %>% group_by(scanID) %>% summarise(median=median(mean))
thickFSdataWide %>% group_by(scanID,region) %>% summarise(mean=mean(Session1)) %>% group_by(scanID) %>% summarise(median=median(mean))

meanADNIVols<-volFSdataWide %>% group_by(scanID,region) %>% filter(scanID == "ADNI_1.0_a") %>% summarise(mean=mean(Session1))
meanADNIthick<-thickFSdataWide %>% group_by(scanID,region) %>% filter(scanID == "ADNI_1.0_a") %>% summarise(mean=mean(Session1))


ggplot(volFSdataWide, aes(x=scanID, y=absError,fill=scanID)) +
  geom_violin() + geom_boxplot(width=0.1,outlier.alpha = .3) + theme_classic() +
  scale_colour_manual(values = c("cyan", "chartreuse", "dark orange"))


###Summarize MRIQC for the paper

##Filter out sessions that you won't use
mriQCstats<-mriQCstats %>% separate(c("Date", "ID"),col = "sesh",sep = "_",extra = "merge",convert = T)
mriQCstatsLong<-mriQCstats %>% pivot_longer(!(Date:scanID),names_to = "QCmetric", values_to = "value")
mriQCstatsLong<-mriQCstatsLong %>% arrange(Date,ID) %>% group_by(ID) %>% mutate(session=dense_rank(Date))
mriQCstatsLong <- mriQCstatsLong %>% filter(session < 3 & ID != "P10395") %>% filter(QCmetric != "session")

metrics<-unique(mriQCstatsLong$QCmetric)
  
for(metric in metrics){
  plotData<- mriQCstatsLong %>% filter(QCmetric == metric) %>% separate(c("scan", "vox"),col = scanID,sep = "_")  %>% unite("scanType",scan,vox)
  ylab<-plotData$QCmetric[1]
  xlab<- "Scan Type"
  p<-ggplot(plotData,aes(scanType,value)) + geom_boxplot() + theme_classic() +
    labs(title=" ", x=xlab, y= ylab)
  ggsave(paste0("/Users/maxwellelliott/myFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/IQMboxplot_",metric,".png"), plot= p,width = 5.2, height =4.0, unit="in", dpi=500)
  
}


QCSumTable<-mriQCstatsLong %>% select(-ID,-Date,-session) %>% filter(str_detect(scanID, "_a")) %>% group_by(scanID,QCmetric)  %>% 
  summarise(mean=mean(value),se=sd(value)/sqrt(length(value)),sd=sd(value)) %>% mutate(lowCI=mean-1.96*se,highCI=mean+1.96*se)


###Organize for Supplemental

##Select TR and Val for 68 thickness and 16 subcortical volumes and mean thickness, WBV and Etiv

volTRstats<-TRstats %>% filter(str_detect(scanID, "_1.0_a|CSx6_1.2_a|CSx6_0.8_a")) %>% filter(str_detect(region,"BrainSegVolNotVentSurf|EstimatedTotalIntraCranialVol|MeanThickness|Amygdala|Accumbens|Pallidum|Caudate|Hippocampus|Putamen|Thalamus|VentralDC"))
volVALstats<-VALstats %>% filter(str_detect(scanIDx, "_1.0_a|CSx6_1.2_a|CSx6_0.8_a")) %>% filter(str_detect(region,"BrainSegVolNotVentSurf|EstimatedTotalIntraCranialVol|MeanThickness|Amygdala|Accumbens|Pallidum|Caudate|Hippocampus|Putamen|Thalamus|VentralDC"))

thickTRstats <- TRstats %>% filter(str_detect(scanID, "_1.0_a|CSx6_1.2_a|CSx6_0.8_a")) %>% filter(str_detect(region, paste(thickRegions, collapse="|")))
thickVALstats <- VALstats %>% filter(str_detect(scanIDx, "_1.0_a|CSx6_1.2_a|CSx6_0.8_a")) %>% filter(str_detect(region, paste(thickRegions, collapse="|")))

#Reliability
thickTRstatsWide<-thickTRstats %>% select(!ICC) 
thickTRstatsWide_1mm <- thickTRstatsWide %>% filter(str_detect(scanID, "_1.0_a")) %>% pivot_wider(names_from = c(scanID), names_sep = "_Reliability_R2_",values_from = c(Rsquared,lowerCI,upperCI))
thickTRstatsWide_1mm <- thickTRstatsWide_1mm %>% select(c(1,2,5,8,3,6,9,4,7,10))
thickTRstatsWide_sub <- thickTRstatsWide %>% filter(str_detect(scanID, "ADNI_1.0_a|CSx6_1.2_a|CSx6_0.8_a")) %>% pivot_wider(names_from = c(scanID), names_sep = "_Reliability_R2_",values_from = c(Rsquared,lowerCI,upperCI))
thickTRstatsWide_sub <- thickTRstatsWide_sub %>% select(c(1,2,5,8,3,6,9,4,7,10))

volTRstatsWide<-volTRstats %>% select(!ICC)
volTRstatsWide_1mm <- volTRstatsWide %>% filter(str_detect(scanID, "_1.0_a")) %>% pivot_wider(names_from = c(scanID), names_sep = "_Reliability_R2_",values_from = c(Rsquared,lowerCI,upperCI))
volTRstatsWide_1mm <- volTRstatsWide_1mm %>% select(c(1,2,5,8,3,6,9,4,7,10))
volTRstatsWide_sub <- volTRstatsWide %>% filter(str_detect(scanID, "ADNI_1.0_a|CSx6_1.2_a|CSx6_0.8_a")) %>% pivot_wider(names_from = c(scanID), names_sep = "_Reliability_R2_",values_from = c(Rsquared,lowerCI,upperCI))
volTRstatsWide_sub <- volTRstatsWide_sub %>% select(c(1,2,5,8,3,6,9,4,7,10))

allTRstatsWide_1mm <- bind_rows(volTRstatsWide_1mm, thickTRstatsWide_1mm)
allTRstatsWide_sub <- bind_rows(volTRstatsWide_sub, thickTRstatsWide_sub)

#Validity
thickVALstatsWide_1mm<-thickVALstats %>% filter(str_detect(scanIDx, "_1.0_a")) %>% unite("label",c(scanIDx,day),sep = "_Day") %>% select(region,label,Rsquared,lowerCI,upperCI) %>% pivot_wider(names_from = label, names_sep = "_Validity_R2_",values_from = c(Rsquared,lowerCI,upperCI))
thickVALstatsWide_1mm <- thickVALstatsWide_1mm %>% select(c(1,2,6,10,4,8,12,3,7,11,5,9,13))
thickVALstatsWide_sub<-thickVALstats %>% filter(str_detect(scanIDx, "CSx6_1.2_a|CSx6_0.8_a")) %>% unite("label",c(scanIDx,day),sep = "_Day") %>% select(region,label,Rsquared,lowerCI,upperCI) %>% pivot_wider(names_from = label, names_sep = "_Validity_R2_",values_from = c(Rsquared,lowerCI,upperCI))
thickVALstatsWide_sub <- thickVALstatsWide_sub %>% select(c(1,2,6,10,4,8,12,3,7,11,5,9,13))

volVALstatsWide_1mm<-volVALstats %>% filter(str_detect(scanIDx, "_1.0_a")) %>% unite("label",c(scanIDx,day),sep = "_Day") %>% select(region,label,Rsquared,lowerCI,upperCI) %>% pivot_wider(names_from = label, names_sep = "_Validity_R2_",values_from = c(Rsquared,lowerCI,upperCI))
volVALstatsWide_1mm <- volVALstatsWide_1mm %>% select(c(1,2,6,10,4,8,12,3,7,11,5,9,13))
volVALstatsWide_sub<- volVALstats %>% filter(str_detect(scanIDx, "CSx6_1.2_a|CSx6_0.8_a")) %>% unite("label",c(scanIDx,day),sep = "_Day") %>% select(region,label,Rsquared,lowerCI,upperCI) %>% pivot_wider(names_from = label, names_sep = "_Validity_R2_",values_from = c(Rsquared,lowerCI,upperCI))
volVALstatsWide_sub <- volVALstatsWide_sub %>% select(c(1,2,6,10,4,8,12,3,7,11,5,9,13))

allVALstatsWide_1mm <- bind_rows(volVALstatsWide_1mm,thickVALstatsWide_1mm) #%>% select(!region)
allVALstatsWide_1mm$region <- allVALstatsWide_1mm$region %>% gsub("_thickness","",.)
allVALstatsWide_sub <- bind_rows(volVALstatsWide_sub,thickVALstatsWide_sub) #%>% select(!region)
allVALstatsWide_sub$region <- allVALstatsWide_sub$region %>% gsub("_thickness","",.)

##Error
allFSdataLong <- allFSdataLong %>% filter(session < 3)
allFSdataWide <- allFSdataLong %>% pivot_wider(names_from = c(session), names_prefix ="Session",values_from = value) %>% drop_na()
allFSdataWide <- allFSdataWide %>% mutate(absError = abs(Session1 - Session2))
sampSize<-length(unique(allFSdataWide$ID))

volFSdataWide <- allFSdataWide %>% filter(str_detect(scanID, "_1.0_a|CSx6_1.2_a|CSx6_0.8_a")) %>% filter(str_detect(region,"BrainSegVolNotVentSurf|EstimatedTotalIntraCranialVol|MeanThickness|Amygdala|Accumbens|Pallidum|Caudate|Hippocampus|Putamen|Thalamus|VentralDC") )
volFSdataWide <- volFSdataWide %>% group_by(scanID,region) %>% summarise(meanSize=mean(Session1+Session2)/2,absoluteError=mean(absError),se=sd(absError)/sampSize)
volFSdataWide <- volFSdataWide %>% mutate(absoluteError_lowerCI=absoluteError - 1.96*se,absoluteError_upperCI=absoluteError + 1.96*se) %>% mutate(percentError=absoluteError/meanSize*100,percentError_lowerCI=absoluteError_lowerCI/meanSize*100,percentError_upperCI=absoluteError_upperCI/meanSize*100)
volErrorWide_1mm <- volFSdataWide %>% filter(str_detect(scanID, "_1.0_a")) %>% select(!se) %>% pivot_wider(names_from = scanID, names_sep = "_mm_",values_from = c(meanSize,absoluteError,absoluteError_lowerCI,absoluteError_upperCI,percentError,percentError_lowerCI,percentError_upperCI))
volErrorWide_sub <- volFSdataWide %>% filter(str_detect(scanID, "ADNI_1.0_a|CSx6_1.2_a|CSx6_0.8_a")) %>% select(!se) %>% pivot_wider(names_from = scanID, names_sep = "_mm_",values_from = c(meanSize,absoluteError,absoluteError_lowerCI,absoluteError_upperCI,percentError,percentError_lowerCI,percentError_upperCI))

thickFSdataWide <- allFSdataWide %>% filter(str_detect(scanID, "_1.0_a|CSx6_1.2_a|CSx6_0.8_a")) %>% filter(str_detect(region, paste(thickRegions, collapse="|")))
thickFSdataWide <- thickFSdataWide %>% group_by(scanID,region) %>% summarise(meanSize=mean(Session1+Session2)/2,absoluteError=mean(absError),se=sd(absError)/sampSize)
thickFSdataWide <- thickFSdataWide %>% mutate(absoluteError_lowerCI=absoluteError - 1.96*se,absoluteError_upperCI=absoluteError + 1.96*se) %>% mutate(percentError=absoluteError/meanSize*100,percentError_lowerCI=absoluteError_lowerCI/meanSize*100,percentError_upperCI=absoluteError_upperCI/meanSize*100)
thickErrorWide_1mm <- thickFSdataWide %>% filter(str_detect(scanID, "_1.0_a")) %>% select(!se) %>% pivot_wider(names_from = scanID, names_sep = "_mm_",values_from = c(meanSize,absoluteError,absoluteError_lowerCI,absoluteError_upperCI,percentError,percentError_lowerCI,percentError_upperCI))
thickErrorWide_sub <- thickFSdataWide %>% filter(str_detect(scanID, "ADNI_1.0_a|CSx6_1.2_a|CSx6_0.8_a")) %>% select(!se) %>% pivot_wider(names_from = scanID, names_sep = "_mm_",values_from = c(meanSize,absoluteError,absoluteError_lowerCI,absoluteError_upperCI,percentError,percentError_lowerCI,percentError_upperCI))

allErrorStatsWide_1mm <- bind_rows(volErrorWide_1mm,thickErrorWide_1mm) %>% select(c(1,2,5,8,11,14,17,20,3,6,9,12,15,18,21,4,7,10,13,16,19,22)) #%>% select(!region)
allErrorStatsWide_sub <- bind_rows(volErrorWide_sub,thickErrorWide_sub) %>% select(c(1,2,5,8,11,14,17,20,3,6,9,12,15,18,21,4,7,10,13,16,19,22)) #%>% select(!region)

##Write out full supplementary tables
###If you do this again you will have to fix the column names further because I couldn't figure out a way to start the name with the scan name

#allStatsWide_1mm <- bind_cols(allTRstatsWide_1mm,allVALstatsWide_1mm,allErrorStatsWide_1mm)
allStatsWide_1mm <- full_join(allTRstatsWide_1mm,allVALstatsWide_1mm,by="region") %>% full_join(.,allErrorStatsWide_1mm,by="region") 
colnames(allStatsWide_1mm) <- c("region", "ADNI_Reliability_R2", "ADNI_Reliability_R2_lowerCI", "ADNI_Reliability_R2_upperCI", "CSx6_Reliability_R2", "CSx6_Reliability_R2_lowerCI", "CSx6_Reliability_R2_upperCI", "WAVE_Reliability_R2", "WAVE_Reliability_R2_lowerCI", "WAVE_Reliability_R2_upperCI", "CSx6_Validity_R2_Day1", "CSx6_Validity_R2_Day1_lowerCI", "CSx6_Validity_R2_Day1_upperCI", "CSx6_Validity_R2_Day2", "CSx6_Validity_R2_Day2_lowerCI", "CSx6_Validity_R2_Day2_upperCI", "WAVE_Validity_R2_Day1", "WAVE_Validity_R2_Day1_lowerCI", "WAVE_Validity_R2_Day1_upperCI", "WAVE_Validity_R2_Day2", "WAVE_Validity_R2_Day2_lowerCI", "WAVE_Validity_R2_Day2_upperCI", "ADNI_meanSize_mm", "ADNI_absoluteError_mm", "ADNI_absoluteError_mm_lowerCI", "ADNI_absoluteError_mm_upperCI","ADNI_percentError", "ADNI_percentError_lowerCI", "ADNI_percentError_upperCI", "CSx6_meanSize_mm","CSx6_absoluteError_mm", "CSx6_absoluteError_mm_lowerCI", "CSx6_absoluteError_mm_upperCI","CSx6_percentError", "CSx6_percentError_lowerCI", "CSx6_percentError_upperCI", "WAVE_meanSize_mm", "WAVE_absoluteError_mm", "WAVE_absoluteError_mm_lowerCI", "WAVE_absoluteError_mm_upperCI","WAVE_percentError", "WAVE_percentError_lowerCI", "WAVE_percentError_upperCI")
#write_csv(allStatsWide_1mm, "/Users/maxwellelliott/myFiles/Manuscripts/precisionRapidMRI_Triarchy/Data/allStatsWide_1mm_230401.csv")  

#allStatsWide_sub <- bind_cols(allTRstatsWide_sub,allVALstatsWide_sub,allErrorStatsWide_sub)
allStatsWide_sub <- full_join(allTRstatsWide_sub,allVALstatsWide_sub,by="region") %>% full_join(.,allErrorStatsWide_sub,by="region") #, allErrorStatsWide_sub)
colnames(allStatsWide_sub) <- c("region", "ADNI_Reliability_R2", "ADNI_Reliability_R2_lowerCI", "ADNI_Reliability_R2_upperCI", "CSx6_0.8mm_Reliability_R2", "CSx6_0.8mm_Reliability_R2_lowerCI", "CSx6_0.8mm_Reliability_R2_upperCI", "CSx6_1.2mm_Reliability_R2", "CSx6_1.2mm_Reliability_R2_lowerCI", "CSx6_1.2mm_Reliability_R2_upperCI", "CSx6_1.2mm_Validity_R2_Day1", "CSx6_1.2mm_Validity_R2_Day1_lowerCI", "CSx6_1.2mm_Validity_R2_Day1_upperCI", "CSx6_1.2mm_Validity_R2_Day2", "CSx6_1.2mm_Validity_R2_Day2_lowerCI", "CSx6_1.2mm_Validity_R2_Day2_upperCI", "CSx6_0.8mm_Validity_R2_Day1", "CSx6_0.8mm_Validity_R2_Day1_lowerCI", "CSx6_0.8mm_Validity_R2_Day1_upperCI", "CSx6_0.8mm_Validity_R2_Day2", "CSx6_0.8mm_Validity_R2_Day2_lowerCI", "CSx6_0.8mm_Validity_R2_Day2_upperCI",  "ADNI_meanSize_mm", "ADNI_absoluteError_mm", "ADNI_absoluteError_mm_lowerCI", "ADNI_absoluteError_mm_upperCI","ADNI_percentError", "ADNI_percentError_lowerCI", "ADNI_percentError_upperCI", "CSx6_0.8mm_meanSize_mm","CSx6_0.8mm_absoluteError_mm", "CSx6_0.8mm_absoluteError_mm_lowerCI", "CSx6_0.8mm_absoluteError_mm_upperCI","CSx6_0.8mm_percentError", "CSx6_0.8mm_percentError_lowerCI", "CSx6_0.8mm_percentError_upperCI", "CSx6_1.2mm_meanSize_mm", "CSx6_1.2mm_absoluteError_mm", "CSx6_1.2mm_absoluteError_mm_lowerCI", "CSx6_1.2mm_absoluteError_mm_upperCI","CSx6_1.2mm_percentError", "CSx6_1.2mm_percentError_lowerCI", "CSx6_1.2mm_percentError_upperCI")
#write_csv(allStatsWide_sub, "/Users/maxwellelliott/myFiles/Manuscripts/precisionRapidMRI_Triarchy/Data/allStatsWide_sub_230401.csv")  
  

