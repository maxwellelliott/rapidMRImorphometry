library(RColorBrewer)
library(boot)
library(corrplot)
library(psych)
library(tidyverse)
library(svglite)
library(magick)
library(png)
library(ggpubr)
library(ggnewscale)

 
volData<-read_csv("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Data/TriarchyOA_cs_FS_aseg_vol_230321.csv")
lhThickData<-read_csv("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Data/TriarchyOA_cs_FS_aparc_thk_lh_230321.csv")
rhThickData<-read_csv("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Data/TriarchyOA_cs_FS_aparc_thk_rh_230321.csv")
lhAreaData<-read_csv("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Data/TriarchyOA_cs_FS_aparc_area_lh_230321.csv")
rhAreaData<-read_csv("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Data/TriarchyOA_cs_FS_aparc_area_rh_230321.csv")

validityStats <- read_csv("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Results/validityStats_230321.csv")
testRetestStats <- read_csv("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Results/testRetestStats_230321.csv")

atlasSizeDataVol<-read_csv("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Data/Triarchy_CS_Reliability_Validity_R2_n15_ASEG_211110.csv")
atlasSizeDataThick<-read_csv("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Data/Triarchy_CS_Reliability_Validity_R2_n15_APARC_211110.csv")

##Grab data from scan names
volData<-volData %>% separate(c("Date", "ID","scanID"),col = "Measure:volume",sep = "_",extra = "merge",convert = T)
lhThickData <- lhThickData %>% separate(c("Date", "ID","scanID"),col = "lh.aparc.thickness",sep = "_",extra = "merge",convert = T)
rhThickData <- rhThickData %>% separate(c("Date", "ID","scanID"),col = "rh.aparc.thickness",sep = "_",extra = "merge",convert = T)
lhAreaData <- lhAreaData %>% separate(c("Date", "ID","scanID"),col = "lh.aparc.area",sep = "_",extra = "merge",convert = T)
rhAreaData <- rhAreaData %>% separate(c("Date", "ID","scanID"),col = "rh.aparc.area",sep = "_",extra = "merge",convert = T)

###Make dataframes longer
volDataLong<-volData %>% pivot_longer(!(Date:scanID),names_to = "region", values_to = "value")
lhThickLong<-lhThickData %>% pivot_longer(!(Date:scanID),names_to = "region", values_to = "value")
rhThickLong<-rhThickData %>% pivot_longer(!(Date:scanID),names_to = "region", values_to = "value")
lhAreaLong<-lhAreaData %>% pivot_longer(!(Date:scanID),names_to = "region", values_to = "area")
rhAreaLong<-rhAreaData %>% pivot_longer(!(Date:scanID),names_to = "region", values_to = "area")

##Add in session variable
volDataLong<-volDataLong %>% arrange(Date,ID) %>% group_by(ID) %>% mutate(session=dense_rank(Date))
lhThickLong<-lhThickLong %>% arrange(Date,ID) %>% group_by(ID) %>% mutate(session=dense_rank(Date))
rhThickLong<-rhThickLong %>% arrange(Date,ID) %>% group_by(ID) %>% mutate(session=dense_rank(Date))
lhThickLong<-lhThickLong %>% arrange(Date,ID) %>% group_by(ID) %>% mutate(session=dense_rank(Date))
rhThickLong<-rhThickLong %>% arrange(Date,ID) %>% group_by(ID) %>% mutate(session=dense_rank(Date))

##Can run this to check if dense_rank work properly there should only be 3-4 sessions, 
table(volDataLong$session)

###Keep only the older adults
volDataLong<-volDataLong %>% filter(str_detect(ID,"^P"))
lhThickLong<-lhThickLong %>% filter(str_detect(ID,"^P"))
rhThickLong<-rhThickLong %>% filter(str_detect(ID,"^P"))
lhAreaLong<-lhAreaLong %>% filter(str_detect(ID,"^P"))
rhAreaLong<-rhAreaLong %>% filter(str_detect(ID,"^P"))

##Fix naming so combining data and plotting is easier
volDataLong<-volDataLong %>% separate(c("Vox", "scanName","Repeat"),col = "scanID",sep = "_",extra = "merge") %>% mutate(Repeat=replace_na(Repeat,"a")) %>% unite("scanID",scanName,Vox,Repeat)
lhThickLong<-lhThickLong %>% separate(c("Vox", "scanName","Repeat"),col = "scanID",sep = "_",extra = "merge") %>% mutate(Repeat=replace_na(Repeat,"a")) %>% unite("scanID",scanName,Vox,Repeat)
rhThickLong<-rhThickLong %>% separate(c("Vox", "scanName","Repeat"),col = "scanID",sep = "_",extra = "merge") %>% mutate(Repeat=replace_na(Repeat,"a")) %>% unite("scanID",scanName,Vox,Repeat)
lhAreaLong<-lhAreaLong %>% separate(c("Vox", "scanName","Repeat"),col = "scanID",sep = "_",extra = "merge") %>% mutate(Repeat=replace_na(Repeat,"a")) %>% unite("scanID",scanName,Vox,Repeat)
rhAreaLong<-rhAreaLong %>% separate(c("Vox", "scanName","Repeat"),col = "scanID",sep = "_",extra = "merge") %>% mutate(Repeat=replace_na(Repeat,"a")) %>% unite("scanID",scanName,Vox,Repeat)

lhThickLong<-lhThickLong %>% mutate(region = gsub("lh_", "Left-", region))
rhThickLong<-rhThickLong %>% mutate(region = gsub("rh_", "Right-", region))
lhAreaLong<-lhAreaLong %>% mutate(region = gsub("lh_", "Left-", region))
rhAreaLong<-rhAreaLong %>% mutate(region = gsub("rh_", "Right-", region))

###Combine all data into big DF
allFSdataLong<-bind_rows(list(volData=volDataLong,lhThick=lhThickLong,rhThick=rhThickLong), .id = "origData")
allFSdataLong<- allFSdataLong %>% mutate(ID = gsub(".ANAT", "", ID))
allFSdataLong<- allFSdataLong %>% mutate(region = gsub("_thickness", "", region))

###Remove IDs with bad or missing data
allFSdataLong<- allFSdataLong %>% filter(!str_detect("P10395|P19644",ID))
allFSdataLong %>% select(ID) %>% unique() %>% nrow()

##Add in column for Dx
dx <- allFSdataLong$ID %>% recode(.,P09993 = "CU", P09453 = "CU", P19820 = "FTLD", P17035 = "CU", P19822 = "FTLD", P10116 = "CU", P19821 = "FTLD", P19823 =  "FTLD", P10295 = "MCI", P19434 = "CU", P19779 = "MCI", P09686 = "CU", P10507 = "CU", P19834 = "CU", P19863 = "FTLD", P10020 = "CU", P09684 = "CU", P10522 = "CU", P10452 = "CU", P17162 = "CU", P10454 = "CU", P19881 = "FTLD", P19897 = "MCI", P10090 = "CU", P19920 = "MCI", P09811 = "CU", P10091 = "MCI", P19923 = "FTLD", P10512 = "MCI", P17001 = "MCI", P10529 = "CU", P19919 = "MCI", P19931 = "FTLD", P19507 = "MCI", P19935 = "FTLD", P19562 = "MCI", P10410 = "CU", P10141 = "CU", P19644 = "MCI")
allFSdataLong <- allFSdataLong %>% add_column(dx)

#write_csv(allFSdataLong,"/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Results/allFSdataLong_230321.csv")


##Grab TR and Validity Stats made with TriarchyOAstats_ReliabilityValidity script
TRstats<-read_csv("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Results/testRetestStats_230321.csv")#read in test-retest stats
VALstats <- read_csv("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Results/validityStats_230321.csv")
VALstats<-rename(VALstats,valRsquared = Rsquared)
VALstats<-VALstats %>% mutate(region = gsub("_thickness","",region))
TRstats<-rename(TRstats,trRsquared = Rsquared)




###################################
################## Plots
###################################

###Test-retest plots for each scan type. Adapted from Lindsays plots

###Violin plots for distributions of reliability by scan type
#make sure to remove all the weird volumes that you don't want
allBrainVars<- unique(allFSdataLong$region)
scanIDs<-names(table(allFSdataLong$scanID)[table(allFSdataLong$scanID) >1000])
badBrainVars<-c(allBrainVars[35:45],"WM-hypointensities","CSF",str_subset(unique(allFSdataLong$region),c("Inf-Lat-Vent")),str_subset(unique(allFSdataLong$region),c("Ventricle")),str_subset(unique(allFSdataLong$region),c("Vol"))[-3],str_subset(unique(allFSdataLong$region),c("Mean")),str_subset(unique(allFSdataLong$region),"Ventricle"),str_subset(unique(allFSdataLong$region),c("Cortex")),'Left-Cerebellum-White-Matter','Right-Cerebellum-White-Matter','SurfaceHoles', 'Ventricle','Optic-Chiasm','rhSurfaceHoles','lhSurfaceHoles','MaskVol-to-eTIV','BrainSegVol-to-eTIV','MaskVol','3rd-Ventricle','4th-Ventricle','5th-Ventricle','Brain-Stem','Left-vessel','Right-vessel','Left-choroid-plexus','Right-choroid-plexus','Left-WM-hypointensities')

#########################################################
####### Make Bilateral test-retest plots
#########################################################

##Start with ADNI, CSx6_a and WAVE_a

biPlotVars<-c("Hippocampus","Amygdala","rostralanteriorcingulate","superiorfrontal","lateraloccipital","parahippocampal","entorhinal","MeanThickness")
allBrainVars<- unique(allFSdataLong$region)
scanIDs<-names(table(allFSdataLong$scanID)[table(allFSdataLong$scanID) >1000])
scanIDsShort<-c("ADNI_1.0_a","CSx6_1.0_a","CSx6_1.2_a","WAVE_1.0_a")
#
#########    Make a tibble with plot settings with each brain region row that can be looked up
#########    Add in parameters for axis labels (volume mm^3 or thickness inches)
#

breakVals<-c(1400,900,.6,.4,.4,.7,1.1,.3)
totMins<-c(800,0,1.9,1.8,1.5,1.4,0.8,1.8)
totMaxs<-c(5000,2700,3.7,3,2.8,3.5,4.1,2.7)
axisLabels<-c("Hipp. Vol.","Amyg. Vol.","rACC Thk.","SFG Thk.","LOC Thk.","PHG Thk.","EC Thk.","Mean Thk.")
units<-c(expr(mm^3),expr(mm^3),"mm","mm","mm","mm","mm","mm")
biPlotParams<-tibble(region=biPlotVars,breakval=breakVals,totMin=totMins,totMAX=totMaxs,axisLabel=axisLabels,unit=units)

paramCheck<-allFSdataLong %>% filter(scanID %in% scanIDsShort) %>% filter(str_detect(region,paste(biPlotVars,collapse = "|"))) %>% filter(session < 3) %>% mutate(region = gsub("Left-", "", region)) %>% mutate(region = gsub("Right-", "", region))
paramCheck<-paramCheck %>% group_by(region) %>% summarise(min=min(value),max=max(value)) %>% full_join(biPlotParams) %>% select(region,min,max,totMin,totMAX) 
paramCheck %>% mutate(minGood = min > totMin,maxGood=max<totMAX)


Palette <-c("#006600", "red")
Palette2 <- c("#0072B2", "#D55E00")
pallette3 <- brewer.pal(n = 3, name = "Dark2")[1:3]

brainVar="Left-Hippocampus"
scan<-"ADNI_1.0_a"
statsDataLong <- allFSdataLong %>% filter(scanID == scan & region == brainVar & session < 3)
statsDataLong <- statsDataLong %>% select(!Date) %>% distinct()    ###Had to add in distinct because of case of incorrect notes (P19823 CSx6_0.8_a) labeling leading to multiple rows with the same value
statsDataWide <- statsDataLong %>% pivot_wider(names_from = c(region,session), names_sep = "_Session",values_from = value) %>% drop_na() 
subIDs<-statsDataWide$ID

for(scan in scanIDsShort){
  for(index in 1:nrow(biPlotParams)){
    print(scan)
    
    brainVar <-  biPlotParams$region[index]
    breakval <- biPlotParams$breakval[index]
    totMIN <- biPlotParams$totMin[index]
    totMAX <- biPlotParams$totMAX[index]
    axisLabel<-biPlotParams$axisLabel[index]
    unit<-biPlotParams$unit[[index]]
    buffer<-breakval/4
    
    xlab <- as.expression(bquote(Session~1~ .(axisLabel)~(.(unit))))
    ylab <- as.expression(bquote(Session~2~ .(axisLabel)~(.(unit))))
    
    plotDataLong <- allFSdataLong %>% filter(scanID == scan & str_detect(region, brainVar) & session < 3)
    plotDataLong <- plotDataLong %>% separate(c("Hemi", "region"),col = "region",sep = "-") %>% select(!Date)  %>% distinct()  
    plotDataWide <- plotDataLong %>% pivot_wider(names_from = c(region,session), names_sep = "_Session",values_from = value) %>% drop_na() %>% filter(ID %in% subIDs)
    
    session1data<-sym(paste0(brainVar,"_Session1"))
    session2data<-sym(paste0(brainVar,"_Session2"))
    plotDataWide_L <- plotDataWide %>% filter(Hemi == "Left")
    plotDataWide_R <- plotDataWide %>% filter(Hemi == "Right")
    rSQ_L<-sprintf("%02.2f",summary(lm(eval(session1data) ~ eval(session2data), data=plotDataWide_L))$r.squared)
    rSQ_R<-sprintf("%02.2f",summary(lm(eval(session1data) ~ eval(session2data), data=plotDataWide_R))$r.squared)
    
    print(plotDataWide %>% select(ID) %>% unique() %>% nrow())
    
    p1<-ggplot(data= plotDataWide) +
      scale_colour_manual(values=Palette, name= " ",labels= c(as.expression(bquote(Left~(R^2 == .(rSQ_L)))),as.expression(bquote(Right~ (R^2 == .(rSQ_R)))))) + 
      scale_fill_manual(name=" ", values=Palette,labels=c(as.expression(bquote(Left~ (R^2 == .(rSQ_L)))),as.expression(bquote(Right~ (R^2 == .(rSQ_R)))))) + 
      scale_shape_manual(name=" ", values=c("\u25C4","\u25BA"),c(as.expression(bquote(Left~ (R^2 == .(rSQ_L)))),as.expression(bquote(Right~ (R^2 == .(rSQ_R))))))+
      geom_point(aes(x= eval(session1data), y= eval(session2data), shape= Hemi, colour=Hemi),  size=4) + 
      theme_classic() +  
      geom_smooth(aes(x= eval(session1data), y= eval(session2data), colour= Hemi), method="lm", se=F, size=0.75,fullrange=TRUE) + 
      scale_x_continuous(limits=c(totMIN-buffer, totMAX+buffer), breaks=c(seq(totMIN, totMAX, breakval)))+
      scale_y_continuous(limits=c(totMIN-buffer, totMAX+buffer), breaks=c(seq(totMIN, totMAX, breakval)))+
      coord_equal(expand=F, xlim=c(totMIN-buffer, totMAX+buffer),ylim=c(totMIN-buffer, totMAX+buffer)) +  
      geom_segment(x= totMIN-buffer, y= totMIN-buffer, xend= totMAX+buffer, yend= totMAX+buffer, linetype=2, size=0.25, color="black") + 
      theme(text = element_text(family = "Geneva"),
            axis.line = element_line(size = 0.75),
            axis.ticks = element_line(colour = "black", size = 0.75),
            plot.title=element_text(size=16, face="bold"),		
            plot.margin = margin(0.2,0.1,0.1,0.1, "in"),
            axis.title.x=element_text(size=12, face="bold", margin = margin(t = 5, r = 0, b = 0, l = 0)), 		
            axis.title.y=element_text(size=12, face="bold", margin = margin(t = 0, r = 5, b = 0, l = 0)), 
            axis.text.y=element_text(size=11, colour="black", face="bold"), 
            axis.text.x=element_text( size=11, colour="black", face="bold"), 
            legend.title=element_text(size=11, face="bold"), 
            legend.text=element_text(size=11, face="bold",),
            legend.position=c(0.01,1.05), 
            legend.justification=c(0,1),
            legend.text.align = 0) + 
      labs(x=xlab, y=ylab) #+ 
    #plot(p1)
    ggsave(paste0("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/testRetest_",brainVar,"_",scan,".png"), plot= p1, width = 4.6, height =4, unit="in", dpi=500)
    assign(paste("testRetest_",brainVar,"_",scan,sep = ""),p1)
  }
}


###

#########################################################
####### Make unilateral test-retest plots
#########################################################

uniPlotVars<-c("eTIV","BrainSegVolNotVentSurf")
breakVals<-c(250,250)
totMins<-c(1150,750)
totMaxs<-c(1900,1500)
axisLabels<-c("eTIV","WBV")
units<-c(expr(cm^3),expr(cm^3))
uniPlotParams<-tibble(region=uniPlotVars,breakval=breakVals,totMin=totMins,totMAX=totMaxs,axisLabel=axisLabels,unit=units)

paramCheck<-allFSdataLong %>% filter(scanID %in% scanIDs) %>% filter(region %in% uniPlotVars) %>% filter(session < 3) %>% mutate(region = gsub("Left-", "", region)) %>% mutate(region = gsub("Right-", "", region)) %>% mutate(value = value/1000)
paramCheck<-paramCheck %>% group_by(region) %>% summarise(min=min(value),max=max(value)) %>% full_join(uniPlotParams) %>% select(region,min,max,totMin,totMAX) 
paramCheck %>% mutate(minGood = min > totMin,maxGood=max<totMAX)

for(scan in scanIDsShort){
  for(index in 1:nrow(uniPlotParams)){
    print(scan)
    
    brainVar <-  uniPlotParams$region[index]
    breakval <- uniPlotParams$breakval[index]
    totMIN <- uniPlotParams$totMin[index]
    totMAX <- uniPlotParams$totMAX[index]
    axisLabel<-uniPlotParams$axisLabel[index]
    unit<-uniPlotParams$unit[[index]]
    buffer<-breakval/4
    
    xlab <- as.expression(bquote(Session~1~ .(axisLabel)~(.(unit))))
    ylab <- as.expression(bquote(Session~2~ .(axisLabel)~(.(unit))))
    
    plotDataLong <- allFSdataLong %>% filter(scanID == scan & region == brainVar & session < 3)
    plotDataLong <- plotDataLong %>% select(!Date)  %>% distinct() %>% mutate(value=value/1000)
    plotDataWide <- plotDataLong %>% pivot_wider(names_from = c(region,session), names_sep = "_Session",values_from = value) %>% drop_na()
    
    session1data<-sym(paste0(brainVar,"_Session1"))
    session2data<-sym(paste0(brainVar,"_Session2"))
    rSQ<-sprintf("%02.2f",summary(lm(eval(session1data) ~ eval(session2data), data=plotDataWide))$r.squared)
    
    print(plotDataWide %>% select(ID) %>% unique() %>% nrow())
    
    p1<-ggplot(data= plotDataWide) +
      scale_colour_manual(values="black", name= " ", labels= c(as.expression(bquote(R^2 == .(rSQ))))) + 
      geom_point(aes(x= eval(session1data), y= eval(session2data)),size=4,show.legend = T) + scale_size(guide = 'none') + #Show.legend=T Adds the dot to the line in the legend
      theme_classic() +  
      geom_smooth(aes(x= eval(session1data), y= eval(session2data), colour= "black"), method="lm", se=F, size=0.75,fullrange=TRUE) + 
      scale_x_continuous(limits=c(totMIN-buffer, totMAX+buffer), breaks=c(seq(totMIN, totMAX, breakval)))+
      scale_y_continuous(limits=c(totMIN-buffer, totMAX+buffer), breaks=c(seq(totMIN, totMAX, breakval)))+
      coord_equal(expand=F, xlim=c(totMIN-buffer, totMAX+buffer),ylim=c(totMIN-buffer, totMAX+buffer)) +  
      geom_segment(x= totMIN-buffer, y= totMIN-buffer, xend= totMAX+buffer, yend= totMAX+buffer, linetype=2, size=0.25, color="black") + 
      theme(text = element_text(family = "Geneva"),
            axis.line = element_line(size = 0.75),
            axis.ticks = element_line(colour = "black", size = 0.75),
            plot.title=element_text(size=16, face="bold"),		
            plot.margin = margin(0.2,0.1,0.1,0.1, "in"),
            axis.title.x=element_text(size=12, face="bold", margin = margin(t = 5, r = 0, b = 0, l = 0)), 		
            axis.title.y=element_text(size=12, face="bold", margin = margin(t = 0, r = 5, b = 0, l = 0)), 
            axis.text.y=element_text(size=11, colour="black", face="bold"), 
            axis.text.x=element_text( size=11, colour="black", face="bold"), 
            legend.title=element_text(size=11, face="bold"), 
            legend.text=element_text(size=11, face="bold",),
            legend.position=c(0.01,1.05), 
            legend.justification=c(0,1),
            legend.text.align = 0) + #guides(color = guide_legend(override.aes=list(shape = 18))) + #color = guide_legend(override.aes = list(linetype = c(0) ) ) ) + if you want to remove line from legend
      labs(x=xlab, y=ylab) + scale_shape_manual(name=" ", values=c("\u25C4")) 
    #plot(p1)
    ggsave(paste0("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/testRetest_",brainVar,"_",scan,".png"), plot= p1, width = 4.6, height =4, unit="in", dpi=500)
    assign(paste("testRetest_",brainVar,"_",scan,sep = ""),p1)
  }
}

############################
#######Make Test-retest Grid out of plots to save time In building powerpoint figures
############################

p1<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/testRetest_eTIV_ADNI_1.0_a.png") %>% image_ggplot(interpolate = F)
p2<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/testRetest_eTIV_CSx6_1.0_a.png") %>% image_ggplot(interpolate = F)
p3<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/testRetest_eTIV_WAVE_1.0_a.png") %>% image_ggplot(interpolate = F)
p4<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/testRetest_BrainSegVolNotVentSurf_ADNI_1.0_a.png") %>% image_ggplot(interpolate = F)
p5<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/testRetest_BrainSegVolNotVentSurf_CSx6_1.0_a.png") %>% image_ggplot(interpolate = F)
p6<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/testRetest_BrainSegVolNotVentSurf_WAVE_1.0_a.png") %>% image_ggplot(interpolate = F)
p7<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/testRetest_Hippocampus_ADNI_1.0_a.png") %>% image_ggplot(interpolate = F)
p8<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/testRetest_Hippocampus_CSx6_1.0_a.png") %>% image_ggplot(interpolate = F)
p9<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/testRetest_Hippocampus_WAVE_1.0_a.png") %>% image_ggplot(interpolate = F)
p10<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/testRetest_Amygdala_ADNI_1.0_a.png") %>% image_ggplot(interpolate = F)
p11<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/testRetest_Amygdala_CSx6_1.0_a.png") %>% image_ggplot(interpolate = F)
p12<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/testRetest_Amygdala_WAVE_1.0_a.png") %>% image_ggplot(interpolate = F)

TRgrid1<-ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,ncol = 3, nrow=4)
ggsave("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/testRetestGrid1.png",plot=TRgrid1,width = 13.8, height =16, unit="in", dpi=500)

p1<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/testRetest_MeanThickness_ADNI_1.0_a.png") %>% image_ggplot(interpolate = F)
p2<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/testRetest_MeanThickness_CSx6_1.0_a.png") %>% image_ggplot(interpolate = F)
p3<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/testRetest_MeanThickness_WAVE_1.0_a.png") %>% image_ggplot(interpolate = F)
p4<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/testRetest_superiorfrontal_ADNI_1.0_a.png") %>% image_ggplot(interpolate = F)
p5<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/testRetest_superiorfrontal_CSx6_1.0_a.png") %>% image_ggplot(interpolate = F)
p6<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/testRetest_superiorfrontal_WAVE_1.0_a.png") %>% image_ggplot(interpolate = F)
#p4<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/testRetest_lateraloccipital_ADNI_1.0_a.png") %>% image_ggplot(interpolate = F)
#p5<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/testRetest_lateraloccipital_CSx6_1.0_a.png") %>% image_ggplot(interpolate = F)
#p6<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/testRetest_lateraloccipital_WAVE_1.0_a.png") %>% image_ggplot(interpolate = F)
p7<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/testRetest_parahippocampal_ADNI_1.0_a.png") %>% image_ggplot(interpolate = F)
p8<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/testRetest_parahippocampal_CSx6_1.0_a.png") %>% image_ggplot(interpolate = F)
p9<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/testRetest_parahippocampal_WAVE_1.0_a.png") %>% image_ggplot(interpolate = F)
p10<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/testRetest_rostralanteriorcingulate_ADNI_1.0_a.png") %>% image_ggplot(interpolate = F)
p11<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/testRetest_rostralanteriorcingulate_CSx6_1.0_a.png") %>% image_ggplot(interpolate = F)
p12<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/testRetest_rostralanteriorcingulate_WAVE_1.0_a.png") %>% image_ggplot(interpolate = F)

TRgrid2<-ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,ncol = 3, nrow=4)
ggsave("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/testRetestGrid2.png",plot=TRgrid2,width = 13.8, height =16, unit="in", dpi=500)

#TRgrid2<-ggarrange(p4,p5,p6,p1,p2,p3,p7,p8,p9,p10,p11,p12,ncol = 3, nrow=4)
#ggsave("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/testRetestGrid4LabTalk.png",plot=TRgrid2,width = 13.8, height =16, unit="in", dpi=500)


#########################################################
####### Make Bilateral Validity plots
#########################################################

#To do
## Validity Plots to make sure each scan type captures regional variation in size and thickness

scanIDsShort<-c("CSx6_1.0_a","CSx6_1.2_a","WAVE_1.0_a")

for(day in c(1,2)){ 
  for(scan in scanIDsShort){ #scanIDs[2:21]){  ##Compare everything to ADNI
    for(index in 1:nrow(biPlotParams)){
      print(scan)
      
      brainVar <-  biPlotParams$region[index]
      breakval <- biPlotParams$breakval[index]
      totMIN <- biPlotParams$totMin[index]
      totMAX <- biPlotParams$totMAX[index]
      axisLabel<-biPlotParams$axisLabel[index]
      unit<-biPlotParams$unit[[index]]
      buffer<-breakval/4
      
      scanNameVars<-str_split(scan,pattern = "_")[[1]]
      yAxisLabel<-paste(scanNameVars[1]) %>% gsub("WAVE","WAVEx9",.)
      #yAxisLabel<-"CSx6 1.2mm"
      AdniAxisLabel<-paste("ADNI")
      
      xlab <- as.expression(bquote(.(AdniAxisLabel) ~ .(axisLabel)~(.(unit))))
      ylab <- as.expression(bquote(.(yAxisLabel) ~ .(axisLabel)~(.(unit))))
        
      print(paste(scan,day,brainVar))
      plotDataLong <- allFSdataLong %>% filter(scanID %in% c(scan, "ADNI_1.0_a") & str_detect(region, brainVar) & session < 3 & session == day)
      plotDataLong <- plotDataLong %>% separate(c("Hemi", "region"),col = "region",sep = "-") %>% select(!Date)  %>% distinct()  
      plotDataWide <- plotDataLong %>% pivot_wider(names_from = c(region,scanID), names_sep = "_",values_from = value) %>% drop_na()
      
      scan1data<-sym(paste(brainVar,"ADNI_1.0_a",sep="_"))
      scan2data<-sym(paste(brainVar,scan,sep="_"))
      plotDataWide_L <- plotDataWide %>% filter(Hemi == "Left")
      plotDataWide_R <- plotDataWide %>% filter(Hemi == "Right")
      rSQ_L<-sprintf("%02.2f",summary(lm(eval(scan1data) ~ eval(scan2data), data=plotDataWide_L))$r.squared)
      rSQ_R<-sprintf("%02.2f",summary(lm(eval(scan1data) ~ eval(scan2data), data=plotDataWide_R))$r.squared)
      
      print(plotDataWide %>% select(ID) %>% unique() %>% nrow())
      
      p1<-ggplot(data= plotDataWide) +
        scale_colour_manual(values=Palette, name= " ",labels=c(as.expression(bquote(Left~(R^2 == .(rSQ_L)))),as.expression(bquote(Right~ (R^2 == .(rSQ_R)))))) + 
        scale_fill_manual(name=" ", values=Palette,labels=c(as.expression(bquote(Left~(R^2 == .(rSQ_L)))),as.expression(bquote(Right~ (R^2 == .(rSQ_R)))))) + 
        scale_shape_manual(name=" ", values=c("\u25C4","\u25BA"),labels=c(as.expression(bquote(Left~(R^2 == .(rSQ_L)))),as.expression(bquote(Right~ (R^2 == .(rSQ_R))))))+
        geom_point(aes(x= eval(scan1data), y= eval(scan2data), shape= Hemi, colour=Hemi),  size=4) + 
        theme_classic() +  
        geom_smooth(aes(x= eval(scan1data), y= eval(scan2data), colour= Hemi), method="lm", se=F, size=0.75,fullrange=TRUE) + 
        scale_x_continuous(limits=c(totMIN-buffer, totMAX+buffer), breaks=c(seq(totMIN, totMAX, breakval)))+
        scale_y_continuous(limits=c(totMIN-buffer, totMAX+buffer), breaks=c(seq(totMIN, totMAX, breakval)))+
        coord_equal(expand=F, xlim=c(totMIN-buffer, totMAX+buffer),ylim=c(totMIN-buffer, totMAX+buffer)) +  
        geom_segment(x= totMIN-buffer, y= totMIN-buffer, xend= totMAX+buffer, yend= totMAX+buffer, linetype=2, size=0.25, color="black") + 
        theme(text = element_text(family = "Geneva"),
              axis.line = element_line(size = 0.75),
              axis.ticks = element_line(colour = "black", size = 0.75),
              plot.title=element_text(size=16, face="bold"),		
              plot.margin = margin(0.2,0.1,0.1,0.1, "in"),
              axis.title.x=element_text(size=12, face="bold", margin = margin(t = 5, r = 0, b = 0, l = 0)), 		
              axis.title.y=element_text(size=12, face="bold", margin = margin(t = 0, r = 5, b = 0, l = 0)), 
              axis.text.y=element_text(size=11, colour="black", face="bold"), 
              axis.text.x=element_text( size=11, colour="black", face="bold"), 
              legend.title=element_text(size=11, face="bold"), 
              legend.text=element_text(size=11, face="bold",),
              legend.position=c(0.01,1.05), 
              legend.justification=c(0,1),
              legend.background = element_rect(fill = "transparent"),
              legend.text.align = 0) + 
        labs(x=xlab, y=ylab) #+ 
      #  ggtitle(paste("Day ",day," Validity"),subtitle = paste("ADNI_1.0_a and",scan,sep=" ")) + theme(plot.title = element_text(hjust = 0.5),plot.subtitle = element_text(hjust = 0.5))
      #plot(p1)
      ggsave(paste0("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/Validity_ADNIto",scan,"_",brainVar,"_Day",day,".png"), plot= p1, width = 4.6, height =4, unit="in", dpi=500)
    }
  }
}


#########################################################
####### Make Unilateral validity plots
########################################################

for(day in c(1,2)){
  for(scan in scanIDsShort){ #scanIDs[2:21]){  ##Compare everything to ADNI
    for(index in 1:nrow(uniPlotParams)){
      print(scan)
      
      brainVar <-  uniPlotParams$region[index]
      breakval <- uniPlotParams$breakval[index]
      totMIN <- uniPlotParams$totMin[index]
      totMAX <- uniPlotParams$totMAX[index]
      axisLabel<-uniPlotParams$axisLabel[index]
      unit<-uniPlotParams$unit[[index]]
      buffer<-breakval/4
      
      scanNameVars<-str_split(scan,pattern = "_")[[1]]
      yAxisLabel<-paste(scanNameVars[1]) %>% gsub("WAVE","WAVEx9",.)
      #yAxisLabel<-"CSx6 1.2mm"
      AdniAxisLabel<-paste("ADNI")
      
      xlab <- as.expression(bquote(.(AdniAxisLabel) ~ .(axisLabel)~(.(unit))))
      ylab <- as.expression(bquote(.(yAxisLabel) ~ .(axisLabel)~(.(unit))))
      
      print(paste(scan,day,brainVar))
      plotDataLong <- allFSdataLong %>% filter(scanID %in% c(scan, "ADNI_1.0_a") & region == brainVar & session < 3 & session == day)
      plotDataLong <- plotDataLong %>% select(!Date) %>% select(!origData) %>% distinct() %>% mutate(value=value/1000)
      plotDataWide <- plotDataLong %>% pivot_wider(names_from = c(region,scanID), names_sep = "_",values_from = value) %>% drop_na()
      
      scan1data<-sym(paste(brainVar,"ADNI_1.0_a",sep="_"))
      scan2data<-sym(paste(brainVar,scan,sep="_"))
      rSQ<-sprintf("%02.2f",summary(lm(eval(scan1data) ~ eval(scan2data), data=plotDataWide))$r.squared)

      print(plotDataWide %>% select(ID) %>% unique() %>% nrow())
      
      p1<-ggplot(data= plotDataWide) +
        scale_colour_manual(values="black", name= " ", labels= c(as.expression(bquote(R^2 == .(rSQ))))) + 
        geom_point(aes(x= eval(scan1data), y= eval(scan2data)),size=4,show.legend = T) + scale_size(guide = 'none') +
        theme_classic() +  
        geom_smooth(aes(x= eval(scan1data), y= eval(scan2data), colour= "black"), method="lm", se=F, size=0.75,fullrange=TRUE) + 
        scale_x_continuous(limits=c(totMIN-buffer, totMAX+buffer), breaks=c(seq(totMIN, totMAX, breakval)))+
        scale_y_continuous(limits=c(totMIN-buffer, totMAX+buffer), breaks=c(seq(totMIN, totMAX, breakval)))+
        coord_equal(expand=F, xlim=c(totMIN-buffer, totMAX+buffer),ylim=c(totMIN-buffer, totMAX+buffer)) +  
        geom_segment(x= totMIN-buffer, y= totMIN-buffer, xend= totMAX+buffer, yend= totMAX+buffer, linetype=2, size=0.25, color="black") + 
        theme(text = element_text(family = "Geneva"),
              axis.line = element_line(size = 0.75),
              axis.ticks = element_line(colour = "black", size = 0.75),
              plot.title=element_text(size=16, face="bold"),		
              plot.margin = margin(0.2,0.1,0.1,0.1, "in"),
              axis.title.x=element_text(size=12, face="bold", margin = margin(t = 5, r = 0, b = 0, l = 0)), 		
              axis.title.y=element_text(size=12, face="bold", margin = margin(t = 0, r = 5, b = 0, l = 0)), 
              axis.text.y=element_text(size=11, colour="black", face="bold"), 
              axis.text.x=element_text( size=11, colour="black", face="bold"), 
              legend.title=element_text(size=11, face="bold"), 
              legend.text=element_text(size=11, face="bold",),
              legend.position=c(0.005,1.07), 
              legend.justification=c(0,1),
              legend.text.align = 0) + #guides(color = guide_legend(override.aes = list(linetype = c(0) ) ) ) +
        scale_shape_manual(name=" ", values=c("\u25C4")) +
        labs(x=xlab, y=ylab) #+ 
      #plot(p1)
      ggsave(paste0("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/Validity_ADNIto",scan,"_",brainVar,"_Day",day,".png"), plot= p1, width = 4.6, height =4, unit="in", dpi=500)
    }
  }
}

############################
#######Make Validity Grids out of plots to save time In building powerpoint figures
############################

#CSx6 to ADNI
p1<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/Validity_ADNItoCSx6_1.0_a_eTIV_Day1.png") %>% image_ggplot(interpolate = F)
p2<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/Validity_ADNItoCSx6_1.0_a_eTIV_Day2.png") %>% image_ggplot(interpolate = F)
p3<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/Validity_ADNItoCSx6_1.0_a_BrainSegVolNotVentSurf_Day1.png") %>% image_ggplot(interpolate = F)
p4<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/Validity_ADNItoCSx6_1.0_a_BrainSegVolNotVentSurf_Day2.png") %>% image_ggplot(interpolate = F)
p5<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/Validity_ADNItoCSx6_1.0_a_Hippocampus_Day1.png") %>% image_ggplot(interpolate = F)
p6<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/Validity_ADNItoCSx6_1.0_a_Hippocampus_Day2.png") %>% image_ggplot(interpolate = F)
p7<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/Validity_ADNItoCSx6_1.0_a_Amygdala_Day1.png") %>% image_ggplot(interpolate = F)
p8<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/Validity_ADNItoCSx6_1.0_a_Amygdala_Day2.png") %>% image_ggplot(interpolate = F)

Valgrid1<-ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,ncol = 2, nrow=4)
ggsave("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/validityGrid1_ADNItoCSx6.png",plot=Valgrid1,width = 9.2, height =16, unit="in", dpi=500)

p11<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/Validity_ADNItoCSx6_1.0_a_MeanThickness_Day1.png") %>% image_ggplot(interpolate = F)
p12<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/Validity_ADNItoCSx6_1.0_a_MeanThickness_Day2.png") %>% image_ggplot(interpolate = F)
p13<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/Validity_ADNItoCSx6_1.0_a_superiorfrontal_Day1.png") %>% image_ggplot(interpolate = F)
p14<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/Validity_ADNItoCSx6_1.0_a_superiorfrontal_Day2.png") %>% image_ggplot(interpolate = F)
#p3<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/Validity_ADNItoCSx6_1.0_a_lateraloccipital_Day1.png") %>% image_ggplot(interpolate = F)
#p4<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/Validity_ADNItoCSx6_1.0_a_lateraloccipital_Day2.png") %>% image_ggplot(interpolate = F)
p15<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/Validity_ADNItoCSx6_1.0_a_parahippocampal_Day1.png") %>% image_ggplot(interpolate = F)
p16<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/Validity_ADNItoCSx6_1.0_a_parahippocampal_Day2.png") %>% image_ggplot(interpolate = F)
p17<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/Validity_ADNItoCSx6_1.0_a_rostralanteriorcingulate_Day1.png") %>% image_ggplot(interpolate = F)
p18<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/Validity_ADNItoCSx6_1.0_a_rostralanteriorcingulate_Day2.png") %>% image_ggplot(interpolate = F)

Valgrid2<-ggarrange(p11,p12,p13,p14,p15,p16,p17,p18,ncol = 2, nrow=4)
ggsave("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/validityGrid2_ADNItoCSx6.png",plot=Valgrid2,width = 9.2, height =16, unit="in", dpi=500)

####New Grid for Revise and Resubmit - Session 1
Valgrid3<-ggarrange(p1,p3,p5,p7,p11,p13,p15,p17,ncol = 2, nrow=4)
ggsave("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/validityGrid3_ADNItoCSx6.png",plot=Valgrid3,width = 9.2, height =16, unit="in", dpi=500)

####New Grid for Revise and Resubmit Supplemental - Session 2
Valgrid4<-ggarrange(p2,p4,p6,p8,p12,p14,p16,p18,ncol = 2, nrow=4)
ggsave("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/validityGrid4_ADNItoCSx6.png",plot=Valgrid4,width = 9.2, height =16, unit="in", dpi=500)

#WAVE to ADNI
p1<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/Validity_ADNItoWAVE_1.0_a_eTIV_Day1.png") %>% image_ggplot(interpolate = F)
p2<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/Validity_ADNItoWAVE_1.0_a_eTIV_Day2.png") %>% image_ggplot(interpolate = F)
p3<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/Validity_ADNItoWAVE_1.0_a_BrainSegVolNotVentSurf_Day1.png") %>% image_ggplot(interpolate = F)
p4<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/Validity_ADNItoWAVE_1.0_a_BrainSegVolNotVentSurf_Day2.png") %>% image_ggplot(interpolate = F)
p5<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/Validity_ADNItoWAVE_1.0_a_Hippocampus_Day1.png") %>% image_ggplot(interpolate = F)
p6<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/Validity_ADNItoWAVE_1.0_a_Hippocampus_Day2.png") %>% image_ggplot(interpolate = F)
p7<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/Validity_ADNItoWAVE_1.0_a_Amygdala_Day1.png") %>% image_ggplot(interpolate = F)
p8<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/Validity_ADNItoWAVE_1.0_a_Amygdala_Day2.png") %>% image_ggplot(interpolate = F)

Valgrid1<-ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,ncol = 2, nrow=4)
ggsave("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/validityGrid1_ADNItoWAVE.png",plot=Valgrid1,width = 9.2, height =16, unit="in", dpi=500)

p11<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/Validity_ADNItoWAVE_1.0_a_MeanThickness_Day1.png") %>% image_ggplot(interpolate = F)
p12<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/Validity_ADNItoWAVE_1.0_a_MeanThickness_Day2.png") %>% image_ggplot(interpolate = F)
p13<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/Validity_ADNItoWAVE_1.0_a_superiorfrontal_Day1.png") %>% image_ggplot(interpolate = F)
p14<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/Validity_ADNItoWAVE_1.0_a_superiorfrontal_Day2.png") %>% image_ggplot(interpolate = F)
#p3<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/Validity_ADNItoWAVE_1.0_a_lateraloccipital_Day1.png") %>% image_ggplot(interpolate = F)
#p4<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/Validity_ADNItoWAVE_1.0_a_lateraloccipital_Day2.png") %>% image_ggplot(interpolate = F)
p15<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/Validity_ADNItoWAVE_1.0_a_parahippocampal_Day1.png") %>% image_ggplot(interpolate = F)
p16<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/Validity_ADNItoWAVE_1.0_a_parahippocampal_Day2.png") %>% image_ggplot(interpolate = F)
p17<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/Validity_ADNItoWAVE_1.0_a_rostralanteriorcingulate_Day1.png") %>% image_ggplot(interpolate = F)
p18<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/Validity_ADNItoWAVE_1.0_a_rostralanteriorcingulate_Day2.png") %>% image_ggplot(interpolate = F)

Valgrid2<-ggarrange(p11,p12,p13,p14,p15,p16,p17,p18,ncol = 2, nrow=4)
ggsave("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/validityGrid2_ADNItoWAVE.png",plot=Valgrid2,width = 9.2, height =16, unit="in", dpi=500)

####New Grid for Revise and Resubmit - Session 1
Valgrid3<-ggarrange(p1,p3,p5,p7,p11,p13,p15,p17,ncol = 2, nrow=4)
ggsave("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/validityGrid3_ADNItoWAVE.png",plot=Valgrid3,width = 9.2, height =16, unit="in", dpi=500)

####New Grid for Revise and Resubmit Supplemental - Session 2
Valgrid4<-ggarrange(p2,p4,p6,p8,p12,p14,p16,p18,ncol = 2, nrow=4)
ggsave("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/validityGrid4_ADNItoWAVE.png",plot=Valgrid4,width = 9.2, height =16, unit="in", dpi=500)

#Valgrid4<-ggarrange(p3,p23,p11,p211,p15,p215,p17,p217,ncol = 2, nrow=4)
#ggsave("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/validityGrid4LabTalk.png",plot=Valgrid4,width = 9.2, height =16, unit="in", dpi=500)





#######################################
################1.2mm Reliability + Validity Grids
#######################################

p1<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/testRetest_eTIV_CSx6_1.2_a.png") %>% image_ggplot(interpolate = F)
p2<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/Validity_ADNItoCSx6_1.2_a_eTIV_Day1.png") %>% image_ggplot(interpolate = F)
p3<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/Validity_ADNItoCSx6_1.2_a_eTIV_Day2.png") %>% image_ggplot(interpolate = F)
p4<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/testRetest_BrainSegVolNotVentSurf_CSx6_1.2_a.png") %>% image_ggplot(interpolate = F)
p5<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/Validity_ADNItoCSx6_1.2_a_BrainSegVolNotVentSurf_Day1.png") %>% image_ggplot(interpolate = F)
p6<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/Validity_ADNItoCSx6_1.2_a_BrainSegVolNotVentSurf_Day2.png") %>% image_ggplot(interpolate = F)
p7<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/testRetest_Hippocampus_CSx6_1.2_a.png") %>% image_ggplot(interpolate = F)
p8<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/Validity_ADNItoCSx6_1.2_a_Hippocampus_Day1.png") %>% image_ggplot(interpolate = F)
p9<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/Validity_ADNItoCSx6_1.2_a_Hippocampus_Day2.png") %>% image_ggplot(interpolate = F)
p10<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/testRetest_Amygdala_CSx6_1.2_a.png") %>% image_ggplot(interpolate = F)
p11<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/Validity_ADNItoCSx6_1.2_a_Amygdala_Day1.png") %>% image_ggplot(interpolate = F)
p12<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/Validity_ADNItoCSx6_1.2_a_Amygdala_Day2.png") %>% image_ggplot(interpolate = F)

#CS1.2grid1<-ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,ncol = 3, nrow=4)
#ggsave("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/reliabilityPlusValidityGrid1_CSx6_1.2mm.png",plot=CS1.2grid1,width = 13.8, height =16, unit="in", dpi=500)

p21<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/testRetest_MeanThickness_CSx6_1.2_a.png") %>% image_ggplot(interpolate = F)
p22<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/Validity_ADNItoCSx6_1.2_a_MeanThickness_Day1.png") %>% image_ggplot(interpolate = F)
p23<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/Validity_ADNItoCSx6_1.2_a_MeanThickness_Day2.png") %>% image_ggplot(interpolate = F)
p24<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/testRetest_superiorfrontal_CSx6_1.2_a.png") %>% image_ggplot(interpolate = F)
p25<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/Validity_ADNItoCSx6_1.2_a_superiorfrontal_Day1.png") %>% image_ggplot(interpolate = F)
p26<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/Validity_ADNItoCSx6_1.2_a_superiorfrontal_Day2.png") %>% image_ggplot(interpolate = F)
#p4<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/testRetest_lateraloccipital_CSx6_1.2_a.png") %>% image_ggplot(interpolate = F)
#p5<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/Validity_ADNItoCSx6_1.2_a_lateraloccipital_Day1.png") %>% image_ggplot(interpolate = F)
#p6<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/Validity_ADNItoCSx6_1.2_a_lateraloccipital_Day2.png") %>% image_ggplot(interpolate = F)
p27<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/testRetest_parahippocampal_CSx6_1.2_a.png") %>% image_ggplot(interpolate = F)
p28<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/Validity_ADNItoCSx6_1.2_a_parahippocampal_Day1.png") %>% image_ggplot(interpolate = F)
p29<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/Validity_ADNItoCSx6_1.2_a_parahippocampal_Day2.png") %>% image_ggplot(interpolate = F)
p30<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/testRetest_rostralanteriorcingulate_CSx6_1.2_a.png") %>% image_ggplot(interpolate = F)
p31<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/Validity_ADNItoCSx6_1.2_a_rostralanteriorcingulate_Day1.png") %>% image_ggplot(interpolate = F)
p32<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/Validity_ADNItoCSx6_1.2_a_rostralanteriorcingulate_Day2.png") %>% image_ggplot(interpolate = F)

#CS1.2grid2<-ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,ncol = 3, nrow=4)
#ggsave("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/reliabilityPlusValidityGrid2_CSx6_1.2mm.png",plot=CS1.2grid2,width = 13.8, height =16, unit="in", dpi=500)
  
CS1.2relGrid<-ggarrange(p1,p4,p7,p10,p21,p24,p27,p30,ncol = 2, nrow=4)
ggsave("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/reliabilityGrid_CSx6_1.2mm.png",plot=CS1.2relGrid,width = 9.2, height =16, unit="in", dpi=500)

CS1.2valGrid1<-ggarrange(p2,p5,p8,p11,p22,p25,p28,p31,ncol = 2, nrow=4)
ggsave("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/validityGrid1_CSx6_1.2mm.png",plot=CS1.2valGrid1,width = 9.2, height =16, unit="in", dpi=500)

CS1.2valGrid2<-ggarrange(p3,p6,p9,p12,p23,p26,p29,p32,ncol = 2, nrow=4)
ggsave("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/validityGrid2_CSx6_1.2mm.png",plot=CS1.2valGrid2,width = 9.2, height =16, unit="in", dpi=500)

#########################################################
####### Make bilateral Error plots
#########################################################

biPlotVars<-c("Hippocampus","Amygdala","rostralanteriorcingulate","superiorfrontal","lateraloccipital","parahippocampal","entorhinal", "MeanThickness")
allBrainVars<- unique(allFSdataLong$region)
scanIDs<-names(table(allFSdataLong$scanID)[table(allFSdataLong$scanID) >1000])
#
#########    Make a tibble with plot settings with each brain region row that can be looked up
#########    Add in parameters for axis labels (volume mm^3 or thickness inches)
#

breakVals<-c(2,4,2,1,1,1.5,4,.5)
totMins<-c(0,0,0,0,0,0,0,0)
totMaxs<-c(10,20,8,4,4,6,16,2)
axisLabels<-c("Hipp. Vol.","Amyg. Vol.","rACC Thk.","SFG Thk.","LOC Thk.","PHG Thk.","EC Thk.","Mean Thk.")
units<-c(expr(mm^3),expr(mm^3),"mm","mm","mm","mm","mm","mm")
yMargin<-c(18.7,18.7,28,28,28,14.5,18.8,18.7)
biPlotParams<-tibble(region=biPlotVars,breakval=breakVals,totMin=totMins,totMAX=totMaxs,axisLabel=axisLabels,unit=units,yMargin=yMargin)

breakValsAbs<-c(40,40,.05,.02,.02,.04,.07,.02)
totMinsAbs<-c(0,0,0,0,0,0,0,0)
totMaxsAbs<-c(200,200,.25,.08,.08,.16,.28,.08)
axisLabels<-c("Hipp. Vol.","Amyg. Vol.","rACC Thk.","SFG Thk.","LOC Thk.","PHG Thk.","EC Thk.","Mean Thk.")
units<-c(expr(mm^3),expr(mm^3),"mm","mm","mm","mm","mm","mm")
#yMarginAbs<-c(18.7,18.7,28,28,28,14.5,18.8)
yMarginAbs<-c(8,6,4,6,5,6,14.5,4)
biPlotParamsAbs<-tibble(region=biPlotVars,breakval=breakValsAbs,totMin=totMinsAbs,totMAX=totMaxsAbs,axisLabel=axisLabels,unit=units,yMarginAbs=yMarginAbs)


Palette <-c("#006600", "red")
Palette2 <- c("#0072B2", "#D55E00")

for(index in 1:nrow(biPlotParams)){
    
  #####Percent error
    brainVar <-  biPlotParams$region[index]
    breakval <- biPlotParams$breakval[index]
    totMIN <- biPlotParams$totMin[index]
    totMAX <- biPlotParams$totMAX[index]
    axisLabel<-biPlotParams$axisLabel[index]
    unit<-biPlotParams$unit[[index]]
    yMargin<-biPlotParams$yMargin[[index]]-5
    buffer<-breakval/4
    
    xlab <- "Scan Type"
    ylab <- paste("Error in",axisLabel,"(%)")
    
    plotDataLong <- allFSdataLong %>% filter(str_detect(region, brainVar) & session < 3)
    plotDataLong <- plotDataLong %>% separate(c("Hemi", "region"),col = "region",sep = "-") %>% select(!Date)  %>% distinct()  
    plotDataWide <- plotDataLong %>% pivot_wider(names_from = c(region,session), names_sep = "_Session",values_from = value) %>% drop_na()
    
    session1data<-sym(paste0(brainVar,"_Session1"))
    session2data<-sym(paste0(brainVar,"_Session2"))
    plotDataWide <- plotDataWide %>% mutate(percentError = abs(eval(session1data) - eval(session2data))/(0.5*(eval(session1data) + eval(session2data)))*100)
    plotSummaryData<-plotDataWide %>% group_by(scanID, Hemi) %>% summarise(mean=mean(percentError),se=sd(percentError)/sqrt(length(percentError)))
    plotSummaryData <- plotSummaryData %>% filter(str_detect(scanID, "_1.0_a"))
    plotSummaryData <- plotSummaryData %>% mutate(scanID = gsub("_1.0_a", "", scanID)) %>% mutate(scanID = gsub("WAVE", "WAVEx9", scanID))
    
    print(plotDataWide %>% select(ID) %>% unique() %>% nrow())
    
    p1<-ggplot(data= plotSummaryData, aes(y= mean, x= scanID,fill = Hemi)) +
      geom_bar(position=position_dodge(.9), stat="identity", color="black", alpha=0.6, size=.75) +
      scale_fill_manual(name=" ", values=Palette) +
      geom_errorbar(aes(ymin=mean, ymax=mean+se),position=position_dodge(.9), colour="black",size=0.75, width = .15) +
      theme_classic() +
      scale_y_continuous(expand = c(0.0,0),limits=c(totMIN, totMAX*1.0035), breaks=c(seq(totMIN, totMAX, breakval)),labels = c("0",format(seq(totMIN, totMAX, breakval)[-1]))) +
      theme(text = element_text(family = "Geneva"),
            plot.title=element_text(size=16, face="bold"),
            plot.margin = margin(0.02,0.1,0.04,0.1, "in"),
            axis.line = element_line(size = 0.75),
            axis.ticks = element_line(colour = "black", size = 0.75),
            axis.title.x=element_text(size=16, face="bold"), 
            axis.title.y=element_text(size=16, face="bold", margin = margin(t = 0, r = 5, b = 0, l = yMargin)),  		
            axis.text.y=element_text(size=14, colour="black", face="bold"), 
            axis.text.x=element_text( size=14, colour="black", face="bold"), 
            legend.title=element_text(size=14, face="bold"), 
            legend.text=element_text(size=14, face="bold"),
            legend.key.size = unit(1.0, 'lines'),
            legend.position=c(0.01,1),
            legend.justification=c(0,1),
            legend.direction = "horizontal") + 	
      guides(colour="none") +
      labs(title=" ", x=xlab, y= ylab)
      #plot(p1)
      ggsave(paste0("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/perError_",brainVar,".png"), plot= p1,width = 4.6, height =4.0, unit="in", dpi=500)
    
      #Absolute Error
      brainVar <-  biPlotParamsAbs$region[index]
      breakval <- biPlotParamsAbs$breakval[index]
      totMIN <- biPlotParamsAbs$totMin[index]
      totMAX <- biPlotParamsAbs$totMAX[index]
      axisLabel<-biPlotParamsAbs$axisLabel[index]
      unit<-biPlotParamsAbs$unit[[index]]
      yMarginAbs<-biPlotParamsAbs$yMarginAbs[[index]]-5
      buffer<-breakval/4
      
      yStart<-"Error in"
      xlab <- "Scan Type"
      ylab <- as.expression(bquote(.(yStart) ~ .(axisLabel)~(.(unit))))
      
      plotDataWide <- plotDataLong %>% pivot_wider(names_from = c(region,session), names_sep = "_Session",values_from = value) %>% drop_na()
      plotDataWide <- plotDataWide %>% mutate(absError = abs(eval(session1data) - eval(session2data)))
      plotSummaryData <- plotDataWide %>% group_by(scanID,Hemi) %>% summarise(mean=mean(absError),se=sd(absError)/sqrt(length(absError)))
      plotSummaryData <- plotSummaryData %>% filter(str_detect(scanID, "_1.0_a"))
      plotSummaryData <- plotSummaryData %>% mutate(scanID = gsub("_1.0_a", "", scanID)) %>% mutate(scanID = gsub("WAVE", "WAVEx9", scanID))
      
      p1<-ggplot(data= plotSummaryData, aes(y= mean, x= scanID,fill = Hemi)) +
        geom_bar(position=position_dodge(.9), stat="identity", color="black", alpha=0.6, size=.75) +
        scale_fill_manual(name=" ", values=Palette) +
        geom_errorbar(aes(ymin=mean, ymax=mean+se),position=position_dodge(.9), colour="black",size=0.75, width = .15) +
        theme_classic() +
        scale_y_continuous(expand = c(0.0,0),limits=c(totMIN, totMAX*1.0035), breaks=c(seq(totMIN, totMAX, breakval)),labels = c("0",format(seq(totMIN, totMAX, breakval)[-1]))) +
        theme(text = element_text(family = "Geneva"),
              plot.title=element_text(size=16, face="bold"),
              plot.margin = margin(0.02,0.1,0.04,0.1, "in"),
              axis.line = element_line(size = 0.75),
              axis.ticks = element_line(colour = "black", size = 0.75),
              axis.title.x=element_text(size=16, face="bold"), 
              axis.title.y=element_text(size=16, face="bold", margin = margin(t = 0, r = 5, b = 0, l = yMarginAbs)), 		
              axis.text.y=element_text(size=14, colour="black", face="bold"), 
              axis.text.x=element_text( size=14, colour="black", face="bold"), 
              legend.title=element_text(size=14, face="bold"), 
              legend.text=element_text(size=14, face="bold"),
              legend.key.size = unit(1.0, 'lines'),
              legend.position=c(0.01,1),
              legend.justification=c(0,1),
              legend.direction = "horizontal") + 	
        guides(colour="none") +
        labs(title=" ", x=xlab, y= ylab)
      #plot(p1)
      ggsave(paste0("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/absError_",brainVar,".png"), plot= p1,width = 4.6, height =4.0, unit="in", dpi=500)
      
}

#########################################################
####### Make unilateral %error plots
#########################################################

uniPlotVars<-c("eTIV","BrainSegVolNotVentSurf")
breakVals<-c(.15,.5)
totMins<-c(0,0)
totMaxs<-c(.6,2)
axisLabels<-c("eTIV","WBV")
units<-c(expr(cm^3),expr(cm^3))
yMargin<-c(5,14.5)     ###This variable has to be manually adjusted so the y-axes line up when the figures are combined in a grid
uniPlotParams<-tibble(region=uniPlotVars,breakval=breakVals,totMin=totMins,totMAX=totMaxs,axisLabel=axisLabels,unit=units,yMargin=yMargin)

uniPlotVarsAbs<-c("eTIV","BrainSegVolNotVentSurf")
breakValsAbs<-c(2.5,4)
totMinsAbs<-c(0,0)
totMaxsAbs<-c(10,16)
axisLabels<-c("eTIV","WBV")
units<-c(expr(cm^3),expr(cm^3))
yMarginAbs<-c(0,5) 
uniPlotParamsAbs<-tibble(region=uniPlotVarsAbs,breakval=breakValsAbs,totMin=totMinsAbs,totMAX=totMaxsAbs,axisLabel=axisLabels,unit=units,yMarginAbs=yMarginAbs)

for(index in 1:nrow(uniPlotParams)){
    
  ####Percent Error
    brainVar <-  uniPlotParams$region[index]
    breakval <- uniPlotParams$breakval[index]
    totMIN <- uniPlotParams$totMin[index]
    totMAX <- uniPlotParams$totMAX[index]
    axisLabel<-uniPlotParams$axisLabel[index]
    unit<-uniPlotParams$unit[[index]]
    buffer<-breakval/4
    yMargin<-uniPlotParams$yMargin[[index]]-5
    
    xlab <- "Scan Type"
    ylab <- paste("Error in",axisLabel,"(%)")
    
    plotDataLong <- allFSdataLong %>% filter(region == brainVar & session < 3)
    plotDataLong <- plotDataLong %>% select(!Date)  %>% distinct()  
    plotDataWide <- plotDataLong %>% pivot_wider(names_from = c(region,session), names_sep = "_Session",values_from = value) %>% drop_na()
    
    session1data<-sym(paste0(brainVar,"_Session1"))
    session2data<-sym(paste0(brainVar,"_Session2"))
    plotDataWide <- plotDataWide %>% mutate(percentError = abs(eval(session1data) - eval(session2data))/(0.5*(eval(session1data) + eval(session2data)))*100)
    plotSummaryData<-plotDataWide %>% group_by(scanID) %>% summarise(mean=mean(percentError),se=sd(percentError)/sqrt(length(percentError)))
    plotSummaryData <- plotSummaryData %>% filter(str_detect(scanID, "_1.0_a"))
    plotSummaryData <- plotSummaryData %>% mutate(scanID = gsub("_1.0_a", "", scanID)) %>% mutate(scanID = gsub("WAVE", "WAVEx9", scanID))
    
    p1<-ggplot(data= plotSummaryData, aes(y= mean, x= scanID)) +
      geom_bar(position=position_dodge(.9), stat="identity", color="black", alpha=0.6, size=.75) +
      scale_fill_manual(name=" ", values=Palette) +
      geom_errorbar(aes(ymin=mean, ymax=mean+se),position=position_dodge(.9), colour="black",size=0.75, width = .15) +
      theme_classic() +
      scale_y_continuous(expand = c(0.0,0),limits=c(totMIN, totMAX*1.0035), breaks=c(seq(totMIN, totMAX, breakval)),labels = c("0",format(seq(totMIN, totMAX, breakval)[-1]))) +
      theme(text = element_text(family = "Geneva"),
            plot.title=element_text(size=16, face="bold"),
            plot.margin = margin(0.02,0.1,0.04,0.1, "in"),
            axis.line = element_line(size = 0.75),
            axis.ticks = element_line(colour = "black", size = 0.75),
            axis.title.x=element_text(size=16, face="bold"), 
            axis.title.y=element_text(size=16, face="bold", margin = margin(t = 0, r = 5, b = 0, l = yMargin)), 	
            axis.text.y=element_text(size=14, colour="black", face="bold"), 
            axis.text.x=element_text( size=14, colour="black", face="bold"), 
            legend.title=element_text(size=14, face="bold"), 
            legend.text=element_text(size=14, face="bold"),
            legend.key.size = unit(1.0, 'lines'),
            legend.position=c(0.01,1),
            legend.justification=c(0,1),
            legend.direction = "horizontal") + 	
      guides(colour="none") +
      labs(title=" ", x=xlab, y= ylab)
    #plot(p1)
    ggsave(paste0("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/perError_",brainVar,".png"), plot= p1,width = 4.6, height =4.0, unit="in", dpi=500)
    
    
    #Absolute Error
    brainVar <-  uniPlotParamsAbs$region[index]
    breakval <- uniPlotParamsAbs$breakval[index]
    totMIN <- uniPlotParamsAbs$totMin[index]
    totMAX <- uniPlotParamsAbs$totMAX[index]
    axisLabel<-uniPlotParamsAbs$axisLabel[index]
    unit<-uniPlotParamsAbs$unit[[index]]
    yMarginAbs<-uniPlotParamsAbs$yMarginAbs[[index]]-5
    buffer<-breakval/4
    
    yStart<-"Error in"
    xlab <- "Scan Type"
    ylab <- as.expression(bquote(.(yStart) ~ .(axisLabel)~(.(unit))))
    
    plotDataWide <- plotDataLong %>% pivot_wider(names_from = c(region,session), names_sep = "_Session",values_from = value) %>% drop_na()
    plotDataWide <- plotDataWide %>% mutate(absError = abs(eval(session1data) - eval(session2data))/1000)
    plotSummaryData <- plotDataWide %>% group_by(scanID) %>% summarise(mean=mean(absError),se=sd(absError)/sqrt(length(absError)))
    plotSummaryData <- plotSummaryData %>% filter(str_detect(scanID, "_1.0_a"))
    plotSummaryData <- plotSummaryData %>% mutate(scanID = gsub("_1.0_a", "", scanID)) %>% mutate(scanID = gsub("WAVE", "WAVEx9", scanID))
   
    p1<-ggplot(data= plotSummaryData, aes(y= mean, x= scanID)) +
    geom_bar(position=position_dodge(.9), stat="identity", color="black", alpha=0.6, size=.75) +
    scale_fill_manual(name=" ", values=Palette) +
    geom_errorbar(aes(ymin=mean, ymax=mean+se),position=position_dodge(.9), colour="black",size=0.75, width = .15) +
    theme_classic() +
    scale_y_continuous(expand = c(0.0,0),limits=c(totMIN, totMAX*1.0035), breaks=c(seq(totMIN, totMAX, breakval)),labels = c("0",format(seq(totMIN, totMAX, breakval)[-1])))+
    theme(text = element_text(family = "Geneva"),
          plot.title=element_text(size=16, face="bold"),
          plot.margin = margin(0.02,0.1,0.04,0.1, "in"),
          axis.line = element_line(size = 0.75),
          axis.ticks = element_line(colour = "black", size = 0.75),
          axis.title.x=element_text(size=16, face="bold"), 
          axis.title.y=element_text(size=16, face="bold", margin = margin(t = 0, r = 5, b = 0, l = yMargin)), 		
          axis.text.y=element_text(size=14, colour="black", face="bold"), 
          axis.text.x=element_text( size=14, colour="black", face="bold"), 
          legend.title=element_text(size=14, face="bold"), 
          legend.text=element_text(size=14, face="bold"),
          legend.key.size = unit(1.0, 'lines'),
          legend.position=c(0.01,1),
          legend.justification=c(0,1),
          legend.direction = "horizontal") + 	
    guides(colour="none") +
    labs(title=" ", x=xlab, y= ylab)
    #plot(p1)
    ggsave(paste0("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/absError_",brainVar,".png"), plot= p1,width = 4.6, height =4.0, unit="in", dpi=500)
    
}

############################
#######Make Percent Error Grid out of plots to save time In building powerpoint figures
############################

p1<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/perError_eTIV.png") %>% image_ggplot(interpolate = F)
p2<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/perError_BrainSegVolNotVentSurf.png") %>% image_ggplot(interpolate = F)
p3<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/perError_Hippocampus.png") %>% image_ggplot(interpolate = F)
p4<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/perError_Amygdala.png") %>% image_ggplot(interpolate = F)
p5<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/perError_MeanThickness.png") %>% image_ggplot(interpolate = F)
p6<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/perError_superiorfrontal.png") %>% image_ggplot(interpolate = F)
#p6<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/perError_lateraloccipital.png") %>% image_ggplot(interpolate = F)
p7<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/perError_parahippocampal.png") %>% image_ggplot(interpolate = F)
p8<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/perError_rostralanteriorcingulate.png") %>% image_ggplot(interpolate = F)

perErrorGrid<-ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,ncol = 2, nrow=4)
ggsave("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/perErrorGrid.png",plot=perErrorGrid,width = 9.2, height =16, unit="in", dpi=500)

#TRgrid2<-ggarrange(p2,p5,p7,p8,ncol = 2, nrow=2)
#ggsave("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/perErrorGrid4LabTalk.png",plot=TRgrid2,width = 9.2, height =8, unit="in", dpi=500)


p1<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/absError_eTIV.png") %>% image_ggplot(interpolate = F)
p2<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/absError_BrainSegVolNotVentSurf.png") %>% image_ggplot(interpolate = F)
p3<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/absError_Hippocampus.png") %>% image_ggplot(interpolate = F)
p4<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/absError_Amygdala.png") %>% image_ggplot(interpolate = F)
p5<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/absError_MeanThickness.png") %>% image_ggplot(interpolate = F)
p6<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/absError_superiorfrontal.png") %>% image_ggplot(interpolate = F)
#p6<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/absError_lateraloccipital.png") %>% image_ggplot(interpolate = F)
p7<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/absError_parahippocampal.png") %>% image_ggplot(interpolate = F)
p8<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/absError_rostralanteriorcingulate.png") %>% image_ggplot(interpolate = F)

absErrorGrid<-ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,ncol = 2, nrow=4)
ggsave("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/absErrorGrid.png",plot=absErrorGrid,width = 9.2, height =16, unit="in", dpi=500)

#########################################################
###### Error x Error Scatter plots
#########################################################

Palette3 <- c("red4","steelblue4")

allThickDataLong<-bind_rows(list(lhThick=lhThickLong,rhThick=rhThickLong), .id = "origData")
allAreaDataLong<-bind_rows(list(lhArea=lhAreaLong,rhArea=rhAreaLong), .id = "origData")
regionSelect<-allThickDataLong %>% filter(str_detect(region,"_thickness")) 
thickRegions<-unique(regionSelect$region) %>% str_replace("_thickness","")

###Percent error
plotDataVolLong <- allFSdataLong %>% filter(str_detect(region,"Amygdala|Accumbens|Pallidum|Caudate|Hippocampus|Putamen|Thalamus|VentralDC")) %>% filter(session < 3) %>% select(!Date)  %>% mutate(origData = "Volume")
plotDataThickLong <- allFSdataLong %>% filter(region %in% thickRegions) %>% filter(session < 3) %>% select(!Date) %>% mutate(origData = "Thickness")
plotDataLong <-bind_rows(plotDataThickLong,plotDataVolLong)

plotDataWide <- plotDataLong %>% pivot_wider(names_from = c(session),names_prefix = "Session",values_from = value) %>% drop_na()

plotDataWide <- plotDataWide %>% mutate(percentError = abs(Session1 - Session2)/(0.5*(Session1 + Session2))*100,absError = abs(Session1 - Session2))
plotSummaryData<-plotDataWide %>% group_by(scanID,region,origData) %>% summarise(meanPer=mean(percentError),meanAbs=mean(absError))
plotSummaryData <- plotSummaryData %>% filter(str_detect(scanID, "_1.0_a"))
plotSummaryData <- plotSummaryData %>% mutate(scanID = gsub("_1.0_a", "", scanID)) %>% mutate(scanID = gsub("WAVE", "WAVEx9", scanID))

plotSummaryData %>% group_by(scanID) %>% summarise(meanPer=mean(meanPer),sdPer=sd(meanPer))

for(scan in c("CSx6","WAVEx9")){
  ADNI<-"ADNI"
  plotDataPerWide<- plotSummaryData %>% select(-meanAbs) %>% pivot_wider(names_from = scanID, values_from = meanPer) 
  plotDataPerWide$origData <- factor(plotDataPerWide$origData, levels = c("Volume","Thickness"))
  plotDataAbsWide<- plotSummaryData %>% select(-meanPer) %>% pivot_wider(names_from = scanID, values_from = meanAbs)
  
  xlab <- paste("Error in",ADNI,"(%)")
  ylab <- paste("Error in",scan,"(%)")
  p1<-ggplot(data= plotDataPerWide) +
    scale_colour_manual(values=Palette3, name= " ") + 
    geom_point(aes(x= ADNI, y= eval(sym(scan)),size=4,colour=origData),alpha=.6) + scale_size(guide = 'none') +
    theme_classic() + 
    scale_x_continuous(limits=c(0,16), breaks=seq(0,16,4),expand=c(0,0),labels=c("0",seq(4,16,4)))+
    scale_y_continuous(limits=c(0,16), breaks=seq(0,16,4),expand=c(0,0),labels=c("0",seq(4,16,4)))+
    coord_equal(expand=F, xlim=c(0,1),ylim=c(0,1)) +  
    geom_segment(x= 0, y= 0, xend= 20, yend= 20, linetype=2, size=0.25, color="black") + 
    theme(text = element_text(family = "Geneva"),
          axis.line = element_line(size = 0.75),
          axis.ticks = element_line(colour = "black", size = 0.75),
          plot.title=element_text(size=16, face="bold"),		
          plot.margin = margin(0.2,0.25,0.1,0.1, "in"),
          axis.title.x=element_text(size=12, face="bold", margin = margin(t = 5, r = 0, b = 0, l = 0)), 		
          axis.title.y=element_text(size=12, face="bold", margin = margin(t = 0, r = 5, b = 0, l = 0)), 
          axis.text.y=element_text(size=12, colour="black", face="bold"), 
          axis.text.x=element_text( size=12, colour="black", face="bold"), 
          legend.title=element_text(size=12, face="bold"), 
          legend.text=element_text(size=12, face="bold"),
          legend.position=c(0.01,1.1), 
          legend.justification=c(0,1),
          legend.text.align = 0) + 
    labs(x=xlab, y=ylab) + guides(color = guide_legend(override.aes = list(size=4))) +
    coord_cartesian(clip = "off") #+ 
  plot(p1)
  ggsave(paste0("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/PerErrorbyPerError_ADNIto",scan,".png"), plot= p1, width = 4.6, height =4, unit="in", dpi=500)

  
  plotDataAbsVol <- plotDataAbsWide %>% filter(origData == "Volume")
  plotDataAbsThick <- plotDataAbsWide %>% filter(origData == "Thickness") %>% filter(str_detect(region,"MeanThickness",negate = T))
  
  Start <-"Error in"
  unit=expr(mm^3)
  xlabAbs <- as.expression(bquote(.(Start) ~ .(ADNI) ~(.(unit))))
  ylabAbs <- as.expression(bquote(.(Start) ~ .(scan) ~(.(unit))))
  
  p1<-ggplot(data= plotDataAbsVol) +
    scale_colour_manual(values=Palette2[2], name= " ") + 
    geom_point(aes(x= ADNI, y= eval(sym(scan)),size=4,colour=origData),alpha=.6) + scale_size(guide = 'none') +
    theme_classic() + 
    scale_x_continuous(limits=c(0,240*1.0031), breaks=seq(0,240,60),expand=c(0,0),labels=c("0",seq(60,240,60)))+
    scale_y_continuous(limits=c(0,240*1.0035), breaks=seq(0,240,60),expand=c(0,0),labels=c("0",seq(60,240,60)))+
    coord_equal(expand=F, xlim=c(0,1),ylim=c(0,1)) +  
    geom_segment(x= 0, y= 0, xend= 240, yend= 240, linetype=2, size=0.25, color="black") + 
    theme(text = element_text(family = "Geneva"),
          axis.line = element_line(size = 0.75),
          axis.ticks = element_line(colour = "black", size = 0.75),
          plot.title=element_text(size=16, face="bold"),		
          plot.margin = margin(0.2,0.25,0.1,0.1, "in"),
          axis.title.x=element_text(size=12, face="bold", margin = margin(t = 5, r = 0, b = 0, l = 0)), 		
          axis.title.y=element_text(size=12, face="bold", margin = margin(t = 0, r = 5, b = 0, l = 0)), 
          axis.text.y=element_text(size=12, colour="black", face="bold"), 
          axis.text.x=element_text( size=12, colour="black", face="bold"), 
          legend.title=element_text(size=12, face="bold"), 
          legend.text=element_text(size=12, face="bold"),
          legend.position=c(0.01,1.11),
          legend.spacing.x = unit(.05,"cm"),
          legend.justification=c(0,1),
          legend.text.align = 0) + 
    guides(color = guide_legend(override.aes = list(size = 4))) + #Makes the size of legend dots match the size of graph dots
    labs(x=xlabAbs, y=ylabAbs) +
    coord_cartesian(clip = "off")
  plot(p1)
  ggsave(paste0("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/AbsErrorbyAbsErrorVol_ADNIto",scan,".png"), plot= p1, width = 4.6, height =4, unit="in", dpi=500)
 
  Start <-"Error in"
  unit=expr(mm)
  xlabAbs <- as.expression(bquote(.(Start) ~ .(ADNI) ~(.(unit))))
  ylabAbs <- as.expression(bquote(.(Start) ~ .(scan) ~(.(unit))))
  
   p1<-ggplot(data= plotDataAbsThick) +
    scale_colour_manual(values=Palette2, name= " ") + 
    geom_point(aes(x= ADNI, y= eval(sym(scan)),size=4,colour=origData),alpha=.6) + scale_size(guide = 'none') +
    theme_classic() + 
    scale_x_continuous(limits=c(0,.240*1.0031), breaks=seq(0,.240,.06),expand=c(0,0),labels=c("0",seq(.06,.240,.06)))+
    scale_y_continuous(limits=c(0,.240*1.0035), breaks=seq(0,.240,.06),expand=c(0,0),labels=c("0",seq(.06,.240,.06)))+
    coord_equal(expand=F, xlim=c(0,1),ylim=c(0,1)) +  
    geom_segment(x= 0, y= 0, xend= .24, yend= .24, linetype=2, size=0.25, color="black") + 
    theme(text = element_text(family = "Geneva"),
          axis.line = element_line(size = 0.75),
          axis.ticks = element_line(colour = "black", size = 0.75),
          plot.title=element_text(size=16, face="bold"),		
          plot.margin = margin(0.2,0.25,0.1,0.1, "in"),
          axis.title.x=element_text(size=12, face="bold", margin = margin(t = 5, r = 0, b = 0, l = 0)), 		
          axis.title.y=element_text(size=12, face="bold", margin = margin(t = 0, r = 5, b = 0, l = 0)), 
          axis.text.y=element_text(size=12, colour="black", face="bold"), 
          axis.text.x=element_text( size=12, colour="black", face="bold"), 
          legend.title=element_text(size=12, face="bold"), 
          legend.text=element_text(size=12, face="bold"),
          legend.position=c(0.01,1.11),
          legend.spacing.x = unit(.05,"cm"),
          legend.justification=c(0,1),
          legend.text.align = 0) + 
    guides(color = guide_legend(override.aes = list(size = 4))) + #Makes the size of legend dots match the size of graph dots
    labs(x=xlabAbs, y=ylabAbs) +
    coord_cartesian(clip = "off")
  plot(p1)
  ggsave(paste0("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/AbsErrorbyAbsErrorThick_ADNIto",scan,".png"), plot= p1, width = 4.6, height =4, unit="in", dpi=500)
  
  
  
  
}

#Per
p1<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/PerErrorbyPerError_ADNItoCSx6.png") %>% image_ggplot(interpolate = F)
p2<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/PerErrorbyPerError_ADNItoWAVEx9.png") %>% image_ggplot(interpolate = F)

PerErrorbyPerErrorGrid<-ggarrange(p1,p2,ncol = 2, nrow=1)
ggsave("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/PerErrorbyPerErrorGrid.png",plot=PerErrorbyPerErrorGrid,width = 9.2, height =4, unit="in", dpi=500)

#Abs
p1<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/AbsErrorbyAbsErrorVol_ADNItoCSx6.png") %>% image_ggplot(interpolate = F)
p2<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/AbsErrorbyAbsErrorVol_ADNItoWAVEx9.png") %>% image_ggplot(interpolate = F)
p3<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/AbsErrorbyAbsErrorThick_ADNItoCSx6.png") %>% image_ggplot(interpolate = F)
p4<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/AbsErrorbyAbsErrorThick_ADNItoWAVEx9.png") %>% image_ggplot(interpolate = F)

AbsErrorbyAbsErrorGrid<-ggarrange(p1,p2,p3,p4,ncol = 2, nrow=2)
ggsave("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/AbsErrorbyAbsErrorGrid.png",plot=AbsErrorbyAbsErrorGrid,width = 9.2, height =8, unit="in", dpi=500)



#########################################################
####### ADNI vs 1.2 vs 0.8mm  bilateral Error plots
#########################################################

biPlotVars<-c("Hippocampus","Amygdala","rostralanteriorcingulate","superiorfrontal","lateraloccipital","parahippocampal","entorhinal","MeanThickness")
allBrainVars<- unique(allFSdataLong$region)
scanIDs<-names(table(allFSdataLong$scanID)[table(allFSdataLong$scanID) >1000])
#
#########    Make a tibble with plot settings with each brain region row that can be looked up
#########    Add in parameters for axis labels (volume mm^3 or thickness inches)
#

breakVals<-c(2,4,2,1,1,1.5,4,0.5)
totMins<-c(0,0,0,0,0,0,0,0)
totMaxs<-c(10,20,8,4,4,6,16,2)
axisLabels<-c("Hipp. Vol.","Amyg. Vol.","rACC Thk.","SFG Thk.","LOC Thk.","PHG Thk.","EC Thk.", "Mean Thk.")
units<-c(expr(mm^3),expr(mm^3),"mm","mm","mm","mm","mm","mm")
yMargin<-c(18.7,18.7,28,28,28,14.5,18.8,18.7)
biPlotParams<-tibble(region=biPlotVars,breakval=breakVals,totMin=totMins,totMAX=totMaxs,axisLabel=axisLabels,unit=units,yMargin=yMargin)

breakValsAbs<-c(40,40,.05,.02,.02,.04,.07,.02)
totMinsAbs<-c(0,0,0,0,0,0,0,0)
totMaxsAbs<-c(200,200,.25,.08,.08,.16,.28,.08)
axisLabels<-c("Hipp. Vol.","Amyg. Vol.","rACC Thk.","SFG Thk.","LOC Thk.","PHG Thk.","EC Thk.", "Mean Thk.")
units<-c(expr(mm^3),expr(mm^3),"mm","mm","mm","mm","mm","mm")
yMarginAbs<-c(5,5,3,3,3,3,14.5,3)
biPlotParamsAbs<-tibble(region=biPlotVars,breakval=breakValsAbs,totMin=totMinsAbs,totMAX=totMaxsAbs,axisLabel=axisLabels,unit=units,yMarginAbs=yMarginAbs)

Palette <-c("#006600", "red")
Palette2 <- c("#0072B2", "#D55E00")

for(index in 1:nrow(biPlotParams)){
  
  #####Percent error
  brainVar <-  biPlotParams$region[index]
  breakval <- biPlotParams$breakval[index]
  totMIN <- biPlotParams$totMin[index]
  totMAX <- biPlotParams$totMAX[index]
  axisLabel<-biPlotParams$axisLabel[index]
  unit<-biPlotParams$unit[[index]]
  yMargin<-biPlotParams$yMargin[[index]]-5
  buffer<-breakval/4
  
  xlab <- "Scan Type"
  ylab <- paste("Error in",axisLabel,"(%)")
  
  plotDataLong <- allFSdataLong %>% filter(str_detect(region, brainVar) & session < 3)
  plotDataLong <- plotDataLong %>% separate(c("Hemi", "region"),col = "region",sep = "-") %>% select(!Date)  %>% distinct()  
  plotDataWide <- plotDataLong %>% pivot_wider(names_from = c(region,session), names_sep = "_Session",values_from = value) %>% drop_na()
  
  session1data<-sym(paste0(brainVar,"_Session1"))
  session2data<-sym(paste0(brainVar,"_Session2"))
  plotDataWide <- plotDataWide %>% mutate(percentError = abs(eval(session1data) - eval(session2data))/(0.5*(eval(session1data) + eval(session2data)))*100)
  plotSummaryData<-plotDataWide %>% group_by(scanID, Hemi) %>% summarise(mean=mean(percentError),se=sd(percentError)/sqrt(length(percentError)))
  plotSummaryData <- plotSummaryData %>% filter(scanID %in% c("ADNI_1.0_a","CSx6_0.8_a","CSx6_1.2_a"))
  
  print(plotDataWide %>% select(ID) %>% unique() %>% nrow())
  
  p1<-ggplot(data= plotSummaryData, aes(y= mean, x= scanID,fill = Hemi)) +
    geom_bar(position=position_dodge(.9), stat="identity", color="black", alpha=0.6, size=.75) +
    scale_fill_manual(name=" ", values=Palette) +
    geom_errorbar(aes(ymin=mean, ymax=mean+se),position=position_dodge(.9), colour="black",size=0.75, width = .15) +
    theme_classic() + scale_x_discrete(labels=c("ADNI_1.0_a" = "ADNI","CSx6_0.8_a" = "CSx6 0.8mm","CSx6_1.2_a" = "CSx6 1.2mm")) +
    scale_y_continuous(expand = c(0.0,0),limits=c(totMIN, totMAX*1.0035), breaks=c(seq(totMIN, totMAX, breakval)),labels = c("0",format(seq(totMIN, totMAX, breakval)[-1]))) +
    theme(text = element_text(family = "Geneva"),
          plot.title=element_text(size=16, face="bold"),
          plot.margin = margin(0.02,0.1,0.04,0.1, "in"),
          axis.line = element_line(size = 0.75),
          axis.ticks = element_line(colour = "black", size = 0.75),
          axis.title.x=element_text(size=16, face="bold"), 
          axis.title.y=element_text(size=16, face="bold", margin = margin(t = 0, r = 5, b = 0, l = yMargin)),  		
          axis.text.y=element_text(size=14, colour="black", face="bold"), 
          axis.text.x=element_text( size=12, colour="black", face="bold"), 
          legend.title=element_text(size=14, face="bold"), 
          legend.text=element_text(size=14, face="bold"),
          legend.key.size = unit(1.0, 'lines'),
          legend.position=c(0.01,1),
          legend.justification=c(0,1),
          legend.direction = "horizontal") + 	
    guides(colour="none") +
    labs(title=" ", x=xlab, y= ylab)
  #plot(p1)
  ggsave(paste0("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/perError_SubMillimeterVsSubMinute_",brainVar,".png"), plot= p1,width = 5.0, height =4.0, unit="in", dpi=500)

  #####Absolute error
  brainVarAbs <-  biPlotParamsAbs$region[index]
  breakvalAbs <- biPlotParamsAbs$breakval[index]
  totMINAbs <- biPlotParamsAbs$totMin[index]
  totMAXAbs <- biPlotParamsAbs$totMAX[index]
  axisLabelAbs<-biPlotParamsAbs$axisLabel[index]
  unitAbs<-biPlotParamsAbs$unit[[index]]
  yMarginAbs<-biPlotParamsAbs$yMarginAbs[[index]]-5
  
  xlabAbs <- "Scan Type"
  ylabAbs <- as.expression(bquote(.(yStart) ~ .(axisLabelAbs)~(.(unit))))
  
  plotDataLong <- allFSdataLong %>% filter(str_detect(region, brainVar) & session < 3)
  plotDataLong <- plotDataLong %>% separate(c("Hemi", "region"),col = "region",sep = "-") %>% select(!Date)  %>% distinct()  
  plotDataWide <- plotDataLong %>% pivot_wider(names_from = c(region,session), names_sep = "_Session",values_from = value) %>% drop_na()
  
  session1data<-sym(paste0(brainVar,"_Session1"))
  session2data<-sym(paste0(brainVar,"_Session2"))
  plotDataWide <- plotDataWide %>% mutate(absError = abs(eval(session1data) - eval(session2data)))
  plotSummaryData<-plotDataWide %>% group_by(scanID, Hemi) %>% summarise(mean=mean(absError),se=sd(absError)/sqrt(length(absError)))
  plotSummaryData <- plotSummaryData %>% filter(scanID %in% c("ADNI_1.0_a","CSx6_0.8_a","CSx6_1.2_a"))
  
  plotDataWide %>% select(ID) %>% unique() %>% nrow()
  
  p1<-ggplot(data= plotSummaryData, aes(y= mean, x= scanID,fill = Hemi)) +
    geom_bar(position=position_dodge(.9), stat="identity", color="black", alpha=0.6, size=.75) +
    scale_fill_manual(name=" ", values=Palette) +
    geom_errorbar(aes(ymin=mean, ymax=mean+se),position=position_dodge(.9), colour="black",size=0.75, width = .15) +
    theme_classic() + scale_x_discrete(labels=c("ADNI_1.0_a" = "ADNI","CSx6_0.8_a" = "CSx6 0.8mm","CSx6_1.2_a" = "CSx6 1.2mm")) +
    scale_y_continuous(expand = c(0.0,0),limits=c(totMINAbs, totMAXAbs*1.0035), breaks=c(seq(totMINAbs, totMAXAbs, breakvalAbs)),labels = c("0",format(seq(totMINAbs, totMAXAbs, breakvalAbs)[-1]))) +
    theme(text = element_text(family = "Geneva"),
          plot.title=element_text(size=16, face="bold"),
          plot.margin = margin(0.02,0.1,0.04,0.1, "in"),
          axis.line = element_line(size = 0.75),
          axis.ticks = element_line(colour = "black", size = 0.75),
          axis.title.x=element_text(size=16, face="bold"), 
          axis.title.y=element_text(size=16, face="bold", margin = margin(t = 0, r = 5, b = 0, l = yMarginAbs)),  		
          axis.text.y=element_text(size=14, colour="black", face="bold"), 
          axis.text.x=element_text( size=12, colour="black", face="bold"), 
          legend.title=element_text(size=14, face="bold"), 
          legend.text=element_text(size=14, face="bold"),
          legend.key.size = unit(1.0, 'lines'),
          legend.position=c(0.01,1),
          legend.justification=c(0,1),
          legend.direction = "horizontal") + 	
    guides(colour="none") +
    labs(title=" ", x=xlabAbs, y= ylabAbs)
  #plot(p1)
  ggsave(paste0("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/absError_SubMillimeterVsSubMinute_",brainVar,".png"), plot= p1,width = 5.0, height =4.0, unit="in", dpi=500)
}


#########################################################
####### ADNI vs 1.2mm vsv 0.8mm  unilateral %error plots
#########################################################

uniPlotVars<-c("eTIV","BrainSegVolNotVentSurf")
breakVals<-c(.25,.5)
totMins<-c(0,0)
totMaxs<-c(1,2)
axisLabels<-c("eTIV","WBV")
units<-c(expr(cm^3),expr(cm^3))
yMargin<-c(5,14.5)     ###This variable has to be manually adjusted so the y-axes line up when the figures are combined in a grid
uniPlotParams<-tibble(region=uniPlotVars,breakval=breakVals,totMin=totMins,totMAX=totMaxs,axisLabel=axisLabels,unit=units,yMargin=yMargin)

uniPlotVarsAbs<-c("eTIV","BrainSegVolNotVentSurf")
breakValsAbs<-c(4,4)
totMinsAbs<-c(0,0)
totMaxsAbs<-c(16,16)
axisLabels<-c("eTIV","WBV")
units<-c(expr(cm^3),expr(cm^3))
yMargin<-c(14,14)
uniPlotParamsAbs<-tibble(region=uniPlotVarsAbs,breakval=breakValsAbs,totMin=totMinsAbs,totMAX=totMaxsAbs,axisLabel=axisLabels,unit=units,yMarginAbs=yMargin)

for(index in 1:nrow(uniPlotParams)){
  
  ####Percent Error
  brainVar <-  uniPlotParams$region[index]
  breakval <- uniPlotParams$breakval[index]
  totMIN <- uniPlotParams$totMin[index]
  totMAX <- uniPlotParams$totMAX[index]
  axisLabel<-uniPlotParams$axisLabel[index]
  unit<-uniPlotParams$unit[[index]]
  buffer<-breakval/4
  yMargin<-uniPlotParams$yMargin[[index]]-5
  
  xlab <- "Scan Type"
  ylab <- paste("Error in",axisLabel,"(%)")
  
  plotDataLong <- allFSdataLong %>% filter(region == brainVar & session < 3)
  plotDataLong <- plotDataLong %>% select(!Date)  %>% distinct()  
  plotDataWide <- plotDataLong %>% pivot_wider(names_from = c(region,session), names_sep = "_Session",values_from = value) %>% drop_na()
  
  session1data<-sym(paste0(brainVar,"_Session1"))
  session2data<-sym(paste0(brainVar,"_Session2"))
  plotDataWide <- plotDataWide %>% mutate(percentError = abs(eval(session1data) - eval(session2data))/(0.5*(eval(session1data) + eval(session2data)))*100)
  plotSummaryData<-plotDataWide %>% group_by(scanID) %>% summarise(mean=mean(percentError),se=sd(percentError)/sqrt(length(percentError)))
  plotSummaryData <- plotSummaryData %>% filter(scanID %in% c("ADNI_1.0_a","CSx6_0.8_a","CSx6_1.2_a"))
  
  p1<-ggplot(data= plotSummaryData, aes(y= mean, x= scanID)) +
    geom_bar(position=position_dodge(.9), stat="identity", color="black", alpha=0.6, size=.75) +
    scale_fill_manual(name=" ", values=Palette) +
    geom_errorbar(aes(ymin=mean, ymax=mean+se),position=position_dodge(.9), colour="black",size=0.75, width = .15) +
    theme_classic() + scale_x_discrete(labels=c("ADNI_1.0_a" = "ADNI","CSx6_0.8_a" = "CSx6 0.8mm","CSx6_1.2_a" = "CSx6 1.2mm")) +
    scale_y_continuous(expand = c(0.0,0),limits=c(totMIN, totMAX*1.0035), breaks=c(seq(totMIN, totMAX, breakval)),labels = c("0",format(seq(totMIN, totMAX, breakval)[-1]))) +
    theme(text = element_text(family = "Geneva"),
          plot.title=element_text(size=16, face="bold"),
          plot.margin = margin(0.02,0.1,0.04,0.1, "in"),
          axis.line = element_line(size = 0.75),
          axis.ticks = element_line(colour = "black", size = 0.75),
          axis.title.x=element_text(size=16, face="bold"), 
          axis.title.y=element_text(size=16, face="bold", margin = margin(t = 0, r = 5, b = 0, l = yMargin)), 	
          axis.text.y=element_text(size=14, colour="black", face="bold"), 
          axis.text.x=element_text( size=12, colour="black", face="bold"), 
          legend.title=element_text(size=14, face="bold"), 
          legend.text=element_text(size=14, face="bold"),
          legend.key.size = unit(1.0, 'lines'),
          legend.position=c(0.01,1),
          legend.justification=c(0,1),
          legend.direction = "horizontal") + 	
    guides(colour="none") +
    labs(title=" ", x=xlab, y= ylab)
  #plot(p1)
  ggsave(paste0("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/perError_SubMillimeterVsSubMinute_",brainVar,".png"), plot= p1,width = 5.0, height =4.0, unit="in", dpi=500)

  #Absolute Error
  brainVarAbs <-  uniPlotParamsAbs$region[index]
  breakvalAbs <- uniPlotParamsAbs$breakval[index]
  totMINAbs <- uniPlotParamsAbs$totMin[index]
  totMAXAbs <- uniPlotParamsAbs$totMAX[index]
  axisLabelAbs<-uniPlotParamsAbs$axisLabel[index]
  unitAbs<-uniPlotParamsAbs$unit[[index]]
  yMarginAbs<-uniPlotParamsAbs$yMarginAbs[[index]]-5
  buffer<-breakval/4
  
  ylabAbs <- as.expression(bquote(.(yStart) ~ .(axisLabelAbs)~(.(unitAbs))))
  
  plotDataLong <- allFSdataLong %>% filter(region == brainVar & session < 3)
  plotDataLong <- plotDataLong %>% select(!Date)  %>% distinct()  
  plotDataWide <- plotDataLong %>% pivot_wider(names_from = c(region,session), names_sep = "_Session",values_from = value) %>% drop_na()
  
  session1data<-sym(paste0(brainVar,"_Session1"))
  session2data<-sym(paste0(brainVar,"_Session2"))
  plotDataWide <- plotDataWide %>% mutate(absError = abs(eval(session1data) - eval(session2data))/1000)
  plotSummaryData<-plotDataWide %>% group_by(scanID) %>% summarise(mean=mean(absError),se=sd(absError)/sqrt(length(absError)))
  plotSummaryData <- plotSummaryData %>% filter(scanID %in% c("ADNI_1.0_a","CSx6_0.8_a","CSx6_1.2_a"))
  
  p1<-ggplot(data= plotSummaryData, aes(y= mean, x= scanID)) +
    geom_bar(position=position_dodge(.9), stat="identity", color="black", alpha=0.6, size=.75) +
    scale_fill_manual(name=" ", values=Palette) +
    geom_errorbar(aes(ymin=mean, ymax=mean+se),position=position_dodge(.9), colour="black",size=0.75, width = .15) +
    theme_classic() + scale_x_discrete(labels=c("ADNI_1.0_a" = "ADNI","CSx6_0.8_a" = "CSx6 0.8mm","CSx6_1.2_a" = "CSx6 1.2mm")) +
    scale_y_continuous(expand = c(0.0,0),limits=c(totMINAbs, totMAXAbs*1.0035), breaks=c(seq(totMINAbs, totMAXAbs, breakvalAbs)),labels = c("0",format(seq(totMINAbs, totMAXAbs, breakvalAbs)[-1]))) +
    theme(text = element_text(family = "Geneva"),
          plot.title=element_text(size=16, face="bold"),
          plot.margin = margin(0.02,0.1,0.04,0.1, "in"),
          axis.line = element_line(size = 0.75),
          axis.ticks = element_line(colour = "black", size = 0.75),
          axis.title.x=element_text(size=16, face="bold"), 
          axis.title.y=element_text(size=16, face="bold",margin = margin(t = 0, r = 5, b = 0, l = yMarginAbs)), 	
          axis.text.y=element_text(size=14, colour="black", face="bold"), 
          axis.text.x=element_text( size=12, colour="black", face="bold"), 
          legend.title=element_text(size=14, face="bold"), 
          legend.text=element_text(size=14, face="bold"),
          legend.key.size = unit(1.0, 'lines'),
          legend.position=c(0.01,1),
          legend.justification=c(0,1),
          legend.direction = "horizontal") + 	
    guides(colour="none") +
    labs(title=" ", x=xlab, y= ylabAbs)
  #plot(p1)
  ggsave(paste0("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/absError_SubMillimeterVsSubMinute_",brainVar,".png"), plot= p1,width = 5.0, height =4.0, unit="in", dpi=500)
  
}

############################
#######Make Percent and Abs Error Grid out of plots to save time In building powerpoint figures
############################

p1<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/perError_SubMillimeterVsSubMinute_eTIV.png") %>% image_ggplot(interpolate = F)
p2<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/perError_SubMillimeterVsSubMinute_BrainSegVolNotVentSurf.png") %>% image_ggplot(interpolate = F)
p3<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/perError_SubMillimeterVsSubMinute_Hippocampus.png") %>% image_ggplot(interpolate = F)
p4<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/perError_SubMillimeterVsSubMinute_Amygdala.png") %>% image_ggplot(interpolate = F)
p5<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/perError_SubMillimeterVsSubMinute_MeanThickness.png") %>% image_ggplot(interpolate = F)
p6<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/perError_SubMillimeterVsSubMinute_superiorfrontal.png") %>% image_ggplot(interpolate = F)
#p6<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/perError_SubMillimeterVsSubMinute_lateraloccipital.png") %>% image_ggplot(interpolate = F)
p7<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/perError_SubMillimeterVsSubMinute_parahippocampal.png") %>% image_ggplot(interpolate = F)
p8<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/perError_SubMillimeterVsSubMinute_rostralanteriorcingulate.png") %>% image_ggplot(interpolate = F)

perErrorGrid<-ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,ncol = 2, nrow=4)
ggsave("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/perErrorGrid_SubMillimeterVsSubMinute.png",plot=perErrorGrid,width = 10, height =16, unit="in", dpi=500)

#TRgrid2<-ggarrange(p2,p5,p7,p8,ncol = 2, nrow=2)
#ggsave("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/perErrorGrid_SubMillimeterVsSubMinute_4LabTalk.png",plot=TRgrid2,width = 10, height =8, unit="in", dpi=500)


p1<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/absError_SubMillimeterVsSubMinute_eTIV.png") %>% image_ggplot(interpolate = F)
p2<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/absError_SubMillimeterVsSubMinute_BrainSegVolNotVentSurf.png") %>% image_ggplot(interpolate = F)
p3<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/absError_SubMillimeterVsSubMinute_Hippocampus.png") %>% image_ggplot(interpolate = F)
p4<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/absError_SubMillimeterVsSubMinute_Amygdala.png") %>% image_ggplot(interpolate = F)
p5<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/absError_SubMillimeterVsSubMinute_MeanThickness.png") %>% image_ggplot(interpolate = F)
p6<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/absError_SubMillimeterVsSubMinute_superiorfrontal.png") %>% image_ggplot(interpolate = F)
#p6<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/absError_SubMillimeterVsSubMinute_lateraloccipital.png") %>% image_ggplot(interpolate = F)
p7<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/absError_SubMillimeterVsSubMinute_parahippocampal.png") %>% image_ggplot(interpolate = F)
p8<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/absError_SubMillimeterVsSubMinute_rostralanteriorcingulate.png") %>% image_ggplot(interpolate = F)

absErrorGrid<-ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,ncol = 2, nrow=4)
ggsave("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/absErrorGrid_SubMillimeterVsSubMinute.png",plot=absErrorGrid,width = 10, height =16, unit="in", dpi=500)

###########################################################
###Scatter plots for test-retest cross-regional comparisons
### Lindsay figure 33 - 36
###########################################################

###Organize all the data
scans<-c("ADNI_1.0_a","CSx6_1.0_a","WAVE_1.0_a")
scanLabels<-c("ADNI","CSx6","WAVEx9")

allThickDataLong<-bind_rows(list(lhThick=lhThickLong,rhThick=rhThickLong), .id = "origData")
allAreaDataLong<-bind_rows(list(lhArea=lhAreaLong,rhArea=rhAreaLong), .id = "origData")

volSummary<-atlasSizeDataVol %>% select(StructName, Volume_mm3_711_2C) %>% rename(region=StructName)%>% filter(str_detect(region,"Amygdala|Accumbens|Pallidum|Caudate|Hippocampus|Putamen|Thalamus|VentralDC|Brain-Stem"))
areaSummary<-atlasSizeDataThick %>% select(ROI_var, SurfArea_711_2C) %>% mutate(ROI_var = gsub("lh_","Left-",ROI_var)) %>% mutate(ROI_var = gsub("rh_","Right-",ROI_var)) %>% rename(region=ROI_var)

regionSelect<-allThickDataLong %>% filter(str_detect(region,"_thickness")) 
thickRegions<-unique(regionSelect$region) %>% str_replace("_thickness","")

Palette2 <- c("steelblue4", "red4")

##Make the test-retest x size plots
#Reliability x size

for(scanInd in 1:3){
  scan<-scans[scanInd]
  scanLabel<-scanLabels[scanInd]
  volTRstats<-TRstats %>% filter(scanID == scan) %>% filter(str_detect(region,"Amygdala|Accumbens|Pallidum|Caudate|Hippocampus|Putamen|Thalamus|VentralDC")) %>% mutate(origData = "Volume")
  thickTRstats <- TRstats %>% filter(region %in% thickRegions) %>% filter(scanID == scan) %>% mutate(origData = "Thickness")
  
  thickTRstats<-thickTRstats %>% mutate(region = gsub("_thickness", "", region)) %>% filter(!str_detect(region,"MeanThickness"))
  areaSummary<-areaSummary %>% mutate(region = gsub("_area", "", region)) %>% filter(!str_detect(region,"WhiteSurfArea")) %>% mutate(region = gsub("_thickness", "", region))
  
  volPlotData<-left_join(volTRstats,volSummary) %>% mutate(mean=Volume_mm3_711_2C/1000)
  thickPlotData<-left_join(thickTRstats,areaSummary) %>% mutate(mean=SurfArea_711_2C/100)

  label<-"Session 1 - Session 2"
  
  ###Plot for Volumes
  xlab <- as.expression(bquote(Subcortical ~ Volume ~(cm^3)))
  ylab <- as.expression(bquote(.(scanLabel) ~ .(label) ~(R^2)))
  
  p1<-ggplot(data= volPlotData) +
    scale_colour_manual(values=Palette2[2], name= " ") +
    geom_point(aes(x= mean, y= trRsquared,size=3,colour=origData),alpha=.6) + scale_size(guide = 'none') +
    theme_classic() +  
    scale_x_continuous(limits=c(0,8), breaks=c(seq(0, 8, 1)),expand=c(0,0))+
    scale_y_continuous(limits=c(0,1), breaks=c(0,0.25,0.50,0.75,1.00),expand=c(0,0),labels=c("0","0.25","0.50","0.75","1.00")) +
    theme(text = element_text(family = "Geneva"),
          axis.line = element_line(size = 0.75),
          axis.ticks = element_line(colour = "black", size = 0.75),
          plot.title=element_text(size=16, face="bold"),		
          plot.margin = margin(0.2,0.1,0.1,0.1, "in"),
          axis.title.x=element_text(size=12, face="bold", margin = margin(t = 5, r = 0, b = 0, l = 0)), 		
          axis.title.y=element_text(size=12, face="bold", margin = margin(t = 0, r = 5, b = 0, l = 0)), 
          axis.text.y=element_text(size=11, colour="black", face="bold"), 
          axis.text.x=element_text( size=11, colour="black", face="bold"), 
          legend.title=element_text(size=12, face="bold"), 
          legend.text=element_text(size=12, face="bold"),
          legend.position=c(.68,.22), 
          legend.spacing.x = unit(.05,"cm"),
          legend.justification=c(0,1),
          legend.text.align = 0) + 
    guides(color = guide_legend(override.aes = list(size = 4))) + 
    labs(x=xlab, y=ylab) + coord_cartesian(clip = "off") 
  #plot(p1)
  ggsave(paste0("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/testRetestBySize_",scanLabel,"_Volume",".png"), plot= p1, width = 4.6, height =4, unit="in", dpi=500)
 
  ###Plot for Thickness
  xlab <- as.expression(bquote(Surface ~ Area ~(cm^2)))
  ylab <- as.expression(bquote(.(scanLabel) ~ .(label) ~(R^2)))
  
  p1<-ggplot(data= thickPlotData) +
    scale_colour_manual(values=Palette2, name= " ") +
    geom_point(aes(x= mean, y= trRsquared,size=3,colour=origData),alpha=.6) + scale_size(guide = 'none') +
    theme_classic() +  
    scale_x_continuous(limits=c(0,50), breaks=c(seq(0, 50, 10)),expand=c(0,0))+
    scale_y_continuous(limits=c(0,1), breaks=c(0,0.25,0.50,0.75,1.00),expand=c(0,0),labels=c("0","0.25","0.50","0.75","1.00")) +
    theme(text = element_text(family = "Geneva"),
          axis.line = element_line(size = 0.75),
          axis.ticks = element_line(colour = "black", size = 0.75),
          plot.title=element_text(size=16, face="bold"),		
          plot.margin = margin(0.2,0.1,0.1,0.1, "in"),
          axis.title.x=element_text(size=12, face="bold", margin = margin(t = 5, r = 0, b = 0, l = 0)), 		
          axis.title.y=element_text(size=12, face="bold", margin = margin(t = 0, r = 5, b = 0, l = 0)), 
          axis.text.y=element_text(size=11, colour="black", face="bold"), 
          axis.text.x=element_text( size=11, colour="black", face="bold"), 
          legend.title=element_text(size=12, face="bold"), 
          legend.text=element_text(size=12, face="bold"),
          legend.position=c(.68,.22), 
          legend.spacing.x = unit(.05,"cm"),
          legend.justification=c(0,1),
          legend.text.align = 0) + 
    guides(color = guide_legend(override.aes = list(size = 4))) + 
    labs(x=xlab, y=ylab) + coord_cartesian(clip = "off") 
  #plot(p1)
  ggsave(paste0("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/testRetestBySize_",scanLabel,"_Thickness",".png"), plot= p1, width = 4.6, height =4, unit="in", dpi=500)
  
}

#######################################
###Reliability + Validity Scatter Plots
#######################################

for(scan in scans[2:3]){
  #for(dayIndex in c(1,2)){
    thickVALstats <- VALstats %>% filter(region %in% thickRegions) %>% filter(scanIDx == scan & scanIDy == "ADNI_1.0_a") #if you want to make separate plots for each day -  & day == dayIndex)
    thickVALstats <- thickVALstats %>% group_by(region) %>% summarise(meanValRsquared = mean(valRsquared),meanMaxValidity = mean(maxValidity)) ##Average validity across days to get more stable estimate for plotting
    volVALstats <- VALstats %>% filter(scanIDx == scan & scanIDy == "ADNI_1.0_a") %>% # add back for days separately -  & day == dayIndex) 
      filter(str_detect(region,"Amygdala|Accumbens|Pallidum|Caudate|Hippocampus|Putamen|Thalamus|VentralDC"))
    volVALstats <- volVALstats %>% group_by(region) %>% summarise(meanValRsquared = mean(valRsquared), meanMaxValidity = mean(maxValidity)) ##Average validity across days to get more stable estimate for plotting
    volTRstats<-TRstats %>% filter(scanID == "ADNI_1.0_a") %>% filter(str_detect(region,"Amygdala|Accumbens|Pallidum|Caudate|Hippocampus|Putamen|Thalamus|VentralDC"))
    thickTRstats <- TRstats %>% filter(region %in% thickRegions) %>% filter(scanID == "ADNI_1.0_a")
    volPlotData<-full_join(volVALstats,volTRstats,by="region")
    thickPlotData<-full_join(thickVALstats,thickTRstats,by="region") #%>% select(region,scanIDx,scanIDy,valRsquared,trRsquared)
    
    scanNameVars<-str_split(scan,pattern = "_")[[1]]
    yAxisLabel<-paste("ADNI - ",scanNameVars[1])
    xAxisLabel<-paste("ADNI Session 1 - Session 2")
    
    xlab <- as.expression(bquote(.(xAxisLabel) ~ (R^2)))
    ylab <- as.expression(bquote(.(yAxisLabel) ~ (R^2)))
    
    p1<-ggplot(data= volPlotData) +
      scale_colour_manual(values="black", name= " ") + 
      geom_point(aes(x= trRsquared, y= meanValRsquared,size=4),alpha=.6) + scale_size(guide = 'none') +
      theme_classic() +  
      scale_x_continuous(limits=c(0,1), breaks=c(0,0.25,0.50,0.75,1.00),expand=c(0,0),labels=c("0","0.25","0.50","0.75","1.00"))+
      scale_y_continuous(limits=c(0,1), breaks=c(0,0.25,0.50,0.75,1.00),expand=c(0,0),labels=c("0","0.25","0.50","0.75","1.00"))+
      coord_equal(expand=F, xlim=c(0,1),ylim=c(0,1)) +  
      geom_segment(x= 0, y= 0, xend= 1, yend= 1, linetype=2, size=0.25, color="black") + 
      theme(text = element_text(family = "Geneva"),
            axis.line = element_line(size = 0.75),
            axis.ticks = element_line(colour = "black", size = 0.75),
            plot.title=element_text(size=16, face="bold"),		
            plot.margin = margin(0.2,0.25,0.1,0.1, "in"),
            axis.title.x=element_text(size=12, face="bold", margin = margin(t = 5, r = 0, b = 0, l = 0)), 		
            axis.title.y=element_text(size=12, face="bold", margin = margin(t = 0, r = 5, b = 0, l = 0)), 
            axis.text.y=element_text(size=11, colour="black", face="bold"), 
            axis.text.x=element_text( size=11, colour="black", face="bold"), 
            legend.title=element_text(size=11, face="bold"), 
            legend.text=element_text(size=11, face="bold",),
            legend.position=c(0.01,1.05), 
            legend.justification=c(0,1),
            legend.text.align = 0) + 
      labs(x=xlab, y=ylab) +
      coord_cartesian(clip = "off") #+ 
    plot(p1)
    ggsave(paste0("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/TRandVal_volume_ADNIto",scan,".png"), plot= p1, width = 4.6, height =4, unit="in", dpi=500)
    
    
    p2<-ggplot(data= thickPlotData) +
      scale_colour_manual(values="black", name= " ") + 
      geom_point(aes(x= trRsquared, y= meanValRsquared,size=4),alpha=.6) + scale_size(guide = 'none') +
      theme_classic() +  
      scale_x_continuous(limits=c(0,1), breaks=c(0,0.25,0.50,0.75,1.00),expand=c(0,0),labels=c("0","0.25","0.50","0.75","1.00"))+
      scale_y_continuous(limits=c(0,1), breaks=c(0,0.25,0.50,0.75,1.00),expand=c(0,0),labels=c("0","0.25","0.50","0.75","1.00"))+
      coord_equal(expand=F, xlim=c(0,1),ylim=c(0,1)) +  
      geom_segment(x= 0, y= 0, xend= 1, yend= 1, linetype=2, size=0.25, color="black") + 
      theme(text = element_text(family = "Geneva"),
            axis.line = element_line(size = 0.75),
            axis.ticks = element_line(colour = "black", size = 0.75),
            plot.title=element_text(size=16, face="bold"),		
            plot.margin = margin(0.2,0.25,0.1,0.1, "in"),
            axis.title.x=element_text(size=12, face="bold", margin = margin(t = 5, r = 0, b = 0, l = 0)), 		
            axis.title.y=element_text(size=12, face="bold", margin = margin(t = 0, r = 5, b = 0, l = 0)), 
            axis.text.y=element_text(size=11, colour="black", face="bold"), 
            axis.text.x=element_text( size=11, colour="black", face="bold"), 
            legend.title=element_text(size=11, face="bold"), 
            legend.text=element_text(size=11, face="bold",),
            legend.position=c(0.01,1.05), 
            legend.justification=c(0,1),
            legend.text.align = 0) + 
      labs(x=xlab, y=ylab)+
      coord_cartesian(clip = "off") #+ 
     plot(p2)
    ggsave(paste0("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/TRandVal_thickness_ADNIto",scan,".png"), plot= p2, width = 4.6, height =4, unit="in", dpi=500)
    
    ###Max Validity to take into account rapid unreliability as well as ADNI
    
    yAxisLabel<-paste("ADNI - ",scanNameVars[1])
    xAxisLabel<-paste("Attenuation Corrected ADNI - ",scanNameVars[1])
    
    xlab <- as.expression(bquote(.(xAxisLabel) ~ (R^2)))
    ylab <- as.expression(bquote(.(yAxisLabel) ~ (R^2)))
    
    p1<-ggplot(data= volPlotData) +
      scale_colour_manual(values="black", name= " ") + 
      geom_point(aes(x= trRsquared, y= meanMaxValidity,size=4),alpha=.6) + scale_size(guide = 'none') +
      theme_classic() +  
      scale_x_continuous(limits=c(0,1), breaks=c(0,0.25,0.50,0.75,1.00),expand=c(0,0),labels=c("0","0.25","0.50","0.75","1.00"))+
      scale_y_continuous(limits=c(0,1), breaks=c(0,0.25,0.50,0.75,1.00),expand=c(0,0),labels=c("0","0.25","0.50","0.75","1.00"))+
      coord_equal(expand=F, xlim=c(0,1),ylim=c(0,1)) +  
      geom_segment(x= 0, y= 0, xend= 1, yend= 1, linetype=2, size=0.25, color="black") + 
      theme(text = element_text(family = "Geneva"),
            axis.line = element_line(size = 0.75),
            axis.ticks = element_line(colour = "black", size = 0.75),
            plot.title=element_text(size=16, face="bold"),		
            plot.margin = margin(0.2,0.25,0.1,0.1, "in"),
            axis.title.x=element_text(size=12, face="bold", margin = margin(t = 5, r = 0, b = 0, l = 0)), 		
            axis.title.y=element_text(size=12, face="bold", margin = margin(t = 0, r = 5, b = 0, l = 0)), 
            axis.text.y=element_text(size=11, colour="black", face="bold"), 
            axis.text.x=element_text( size=11, colour="black", face="bold"), 
            legend.title=element_text(size=11, face="bold"), 
            legend.text=element_text(size=11, face="bold",),
            legend.position=c(0.01,1.05), 
            legend.justification=c(0,1),
            legend.text.align = 0) + 
      labs(x=xlab, y=ylab) +
      coord_cartesian(clip = "off") #+ 
    plot(p1)
    ggsave(paste0("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/maxValandVal_volume_ADNIto",scan,".png"), plot= p1, width = 4.6, height =4, unit="in", dpi=500)
    
    
    p2<-ggplot(data= thickPlotData) +
      scale_colour_manual(values="black", name= " ") + 
      geom_point(aes(x= trRsquared, y= meanMaxValidity,size=4),alpha=.6) + scale_size(guide = 'none') +
      theme_classic() +  
      scale_x_continuous(limits=c(0,1), breaks=c(0,0.25,0.50,0.75,1.00),expand=c(0,0),labels=c("0","0.25","0.50","0.75","1.00"))+
      scale_y_continuous(limits=c(0,1), breaks=c(0,0.25,0.50,0.75,1.00),expand=c(0,0),labels=c("0","0.25","0.50","0.75","1.00"))+
      coord_equal(expand=F, xlim=c(0,1),ylim=c(0,1)) +  
      geom_segment(x= 0, y= 0, xend= 1, yend= 1, linetype=2, size=0.25, color="black") + 
      theme(text = element_text(family = "Geneva"),
            axis.line = element_line(size = 0.75),
            axis.ticks = element_line(colour = "black", size = 0.75),
            plot.title=element_text(size=16, face="bold"),		
            plot.margin = margin(0.2,0.25,0.1,0.1, "in"),
            axis.title.x=element_text(size=12, face="bold", margin = margin(t = 5, r = 0, b = 0, l = 0)), 		
            axis.title.y=element_text(size=12, face="bold", margin = margin(t = 0, r = 5, b = 0, l = 0)), 
            axis.text.y=element_text(size=11, colour="black", face="bold"), 
            axis.text.x=element_text( size=11, colour="black", face="bold"), 
            legend.title=element_text(size=11, face="bold"), 
            legend.text=element_text(size=11, face="bold",),
            legend.position=c(0.01,1.05), 
            legend.justification=c(0,1),
            legend.text.align = 0) + 
      labs(x=xlab, y=ylab)+
      coord_cartesian(clip = "off") #+ 
    plot(p2)
    ggsave(paste0("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/maxValandVal_thickness_ADNIto",scan,".png"), plot= p2, width = 4.6, height =4, unit="in", dpi=500)
    
    
#  }
}




############################
#######Test-retest X Size Grid
############################

p1<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/testRetestBySize_ADNI_Volume.png") %>% image_ggplot(interpolate = F)
p2<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/testRetestBySize_CSx6_Volume.png") %>% image_ggplot(interpolate = F)
p3<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/testRetestBySize_WAVEx9_Volume.png") %>% image_ggplot(interpolate = F)
p4<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/testRetestBySize_ADNI_Thickness.png") %>% image_ggplot(interpolate = F)
p5<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/testRetestBySize_CSx6_Thickness.png") %>% image_ggplot(interpolate = F)
p6<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/testRetestBySize_WAVEx9_Thickness.png") %>% image_ggplot(interpolate = F)

testRetestBySizeGrid<-ggarrange(p1,p2,p3,p4,p5,p6,ncol = 3, nrow=2)
ggsave("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/testRetestBySizeGrid.png",plot=testRetestBySizeGrid,width = 13.8, height =8, unit="in", dpi=500)

############################
#######TR by Val Grid
############################

p1<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/TRandVal_volume_ADNItoCSx6_1.0_a.png") %>% image_ggplot(interpolate = F)
p2<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/maxValandVal_volume_ADNItoCSx6_1.0_a.png") %>% image_ggplot(interpolate = F)
p3<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/TRandVal_volume_ADNItoWAVE_1.0_a.png") %>% image_ggplot(interpolate = F)
p4<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/maxValandVal_volume_ADNItoWAVE_1.0_a.png") %>% image_ggplot(interpolate = F)
p5<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/TRandVal_thickness_ADNItoCSx6_1.0_a.png") %>% image_ggplot(interpolate = F)
p6<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/maxValandVal_thickness_ADNItoCSx6_1.0_a.png") %>% image_ggplot(interpolate = F)
p7<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/TRandVal_thickness_ADNItoWAVE_1.0_a.png") %>% image_ggplot(interpolate = F)
p8<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/maxValandVal_thickness_ADNItoWAVE_1.0_a.png") %>% image_ggplot(interpolate = F)


#p1<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/TRandVal_volume_ADNItoCSx6_1.0_a_Day1.png") %>% image_ggplot(interpolate = F)
#p2<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/TRandVal_volume_ADNItoCSx6_1.0_a_Day2.png") %>% image_ggplot(interpolate = F)
#p3<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/TRandVal_volume_ADNItoWAVE_1.0_a_Day1.png") %>% image_ggplot(interpolate = F)
#p4<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/TRandVal_volume_ADNItoWAVE_1.0_a_Day2.png") %>% image_ggplot(interpolate = F)
#p5<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/TRandVal_thickness_ADNItoCSx6_1.0_a_Day1.png") %>% image_ggplot(interpolate = F)
#p6<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/TRandVal_thickness_ADNItoCSx6_1.0_a_Day2.png") %>% image_ggplot(interpolate = F)
#p7<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/TRandVal_thickness_ADNItoWAVE_1.0_a_Day1.png") %>% image_ggplot(interpolate = F)
#p8<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/TRandVal_thickness_ADNItoWAVE_1.0_a_Day2.png") %>% image_ggplot(interpolate = F)

TRbyValGrid_Day1<-ggarrange(p1,p3,p5,p7,ncol = 2, nrow=2)
ggsave("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/testRetestByValidityGrid.png",plot=TRbyValGrid_Day1,width = 9.2, height =8, unit="in", dpi=500)

TRbyValGrid_Day2<-ggarrange(p2,p4,p6,p8,ncol = 2, nrow=2)
ggsave("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/maxValidityByValidityGrid.png",plot=TRbyValGrid_Day2,width = 9.2, height =8, unit="in", dpi=500)



#######################################
###Reliability X Reliability Scatter Plots
######################################
Palette2 <- c("red4","steelblue4")

for(ind in c(2,3)){
  scan <- scans[ind]
  ADNI<-"ADNI_1.0_a"
  volTRstats<-TRstats %>% filter(scanID %in% c(ADNI,scan)) %>% filter(str_detect(region,"Amygdala|Accumbens|Pallidum|Caudate|Hippocampus|Putamen|Thalamus|VentralDC"))  %>% mutate(source="Volume")
  thickTRstats <- TRstats %>% filter(region %in% thickRegions) %>% filter(scanID %in% c(ADNI,scan)) %>% mutate(source="Thickness")
  plotData<-bind_rows(volTRstats,thickTRstats) %>% mutate(source=fct_relevel(source,"Volume"))

  plotDataWide <- plotData %>% select(scanID,region,trRsquared,source) %>% pivot_wider(names_from = scanID,values_from = trRsquared) %>% drop_na()
  
  scanNameVars<-scanLabels[ind]
  yAxisLabel<-paste(scanNameVars," Session 1 - Session 2")
  xAxisLabel<-paste("ADNI Session 1 - Session 2")
  
  xlab <- as.expression(bquote(.(xAxisLabel) ~ (R^2)))
  ylab <- as.expression(bquote(.(yAxisLabel) ~ (R^2)))
  
  p1<-ggplot(data= plotDataWide) +
  scale_colour_manual(values=Palette2, name= " ") + 
  geom_point(aes(x= eval(sym(ADNI)), y=eval(sym(scan)),size=4,colour=source),alpha=.6) + scale_size(guide = 'none') +
  theme_classic() +  
  scale_x_continuous(limits=c(0,1.0031), breaks=c(0,0.25,0.50,0.75,1.00),expand=c(0,0),labels=c("0","0.25","0.50","0.75","1.00"))+
  scale_y_continuous(limits=c(0,1.0035), breaks=c(0,0.25,0.50,0.75,1.00),expand=c(0,0),labels=c("0","0.25","0.50","0.75","1.00"))+
  coord_equal(expand=F, xlim=c(0,1),ylim=c(0,1)) +  
  geom_segment(x= 0, y= 0, xend= 1, yend= 1, linetype=2, size=0.25, color="black") + 
  theme(text = element_text(family = "Geneva"),
        axis.line = element_line(size = 0.75),
        axis.ticks = element_line(colour = "black", size = 0.75),
        plot.title=element_text(size=16, face="bold"),		
        plot.margin = margin(0.2,0.25,0.1,0.1, "in"),
        axis.title.x=element_text(size=12, face="bold", margin = margin(t = 5, r = 0, b = 0, l = 0)), 		
        axis.title.y=element_text(size=12, face="bold", margin = margin(t = 0, r = 5, b = 0, l = 0)), 
        axis.text.y=element_text(size=12, colour="black", face="bold"), 
        axis.text.x=element_text( size=12, colour="black", face="bold"), 
        legend.title=element_text(size=12, face="bold"), 
        legend.text=element_text(size=12, face="bold"),
        legend.position=c(0.01,1.11),
        legend.spacing.x = unit(.05,"cm"),
        legend.justification=c(0,1),
        legend.text.align = 0) + 
    guides(color = guide_legend(override.aes = list(size = 4))) +#Makes the size of legend dots match the size of graph dots
    labs(x=xlab, y=ylab) +
    coord_cartesian(clip = "off")
  #plot(p1)
  ggsave(paste0("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/TRbyTR_ADNIto",scan,".png"), plot= p1, width = 4.6, height =4, unit="in", dpi=500)
}

p1<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/TRbyTR_ADNItoCSx6_1.0_a.png") %>% image_ggplot(interpolate = F)
p2<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/TRbyTR_ADNItoWAVE_1.0_a.png") %>% image_ggplot(interpolate = F)

TRbyValGrid_Day1<-ggarrange(p1,p2,ncol = 2, nrow=1)
ggsave("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/TRbyTRGrid.png",plot=TRbyValGrid_Day1,width = 9.2, height =4, unit="in", dpi=500)


#######################################
###Validity X Validity Scatter Plots
#######################################
scans<-c("ADNI_1.0_a","CSx6_1.0_a","WAVE_1.0_a")
scanNames<-c("ADNI","CSx6","WAVEx9")
Palette2 <- c( "red4","steelblue4")

for(ind in 2:3){
  #for(dayIndex in c(1,2)){
  scan<-scans[ind]
  thickVALstats <- VALstats %>% filter(region %in% thickRegions) %>% filter(scanIDx == scan & scanIDy == "ADNI_1.0_a")  %>% mutate(origData = "Thickness")
  volVALstats <- VALstats %>% filter(scanIDx == scan & scanIDy == "ADNI_1.0_a") %>% #add back for days separately -  & day == dayIndex) 
    filter(str_detect(region,"Amygdala|Accumbens|Pallidum|Caudate|Hippocampus|Putamen|Thalamus|VentralDC")) %>% mutate(origData = "Volume")
  
  plotData<-bind_rows(volVALstats,thickVALstats) %>% mutate(origData=fct_relevel(origData,"Volume"))
  plotDataWide<-plotData %>% select(region,scanIDx,scanIDy,day,origData,valRsquared) %>% pivot_wider(names_from = day,values_from = valRsquared,names_prefix = "Day")
  
  scanName<-scanNames[ind]
  xAxisLabel<-paste("ADNI -",scanName," Session 1 Validity")
  yAxisLabel<-paste("ADNI -",scanName," Session 2 Validity")
  
  xlab<-as.expression(bquote(.(xAxisLabel) ~ (R^2)))
  ylab<-as.expression(bquote(.(xAxisLabel) ~ (R^2)))
  
  p1<-ggplot(data= plotDataWide) +
    scale_colour_manual(values=Palette2, name= " ") + 
    geom_point(aes(x= Day1, y= Day2,size=4,colour=origData),alpha=.6) + scale_size(guide = 'none') +
    theme_classic() +  
    scale_x_continuous(limits=c(0,1.0031), breaks=c(0,0.25,0.50,0.75,1.00),expand=c(0,0),labels=c("0","0.25","0.50","0.75","1.00"))+
    scale_y_continuous(limits=c(0,1.0035), breaks=c(0,0.25,0.50,0.75,1.00),expand=c(0,0),labels=c("0","0.25","0.50","0.75","1.00"))+
    coord_equal(expand=F, xlim=c(0,1),ylim=c(0,1)) +  
    geom_segment(x= 0, y= 0, xend= 1, yend= 1, linetype=2, size=0.25, color="black") + 
    theme(text = element_text(family = "Geneva"),
          axis.line = element_line(size = 0.75),
          axis.ticks = element_line(colour = "black", size = 0.75),
          plot.title=element_text(size=16, face="bold"),		
          plot.margin = margin(0.2,0.25,0.1,0.1, "in"),
          axis.title.x=element_text(size=12, face="bold", margin = margin(t = 5, r = 0, b = 0, l = 0)), 		
          axis.title.y=element_text(size=12, face="bold", margin = margin(t = 0, r = 5, b = 0, l = 0)), 
          axis.text.y=element_text(size=11, colour="black", face="bold"), 
          axis.text.x=element_text( size=11, colour="black", face="bold"), 
          legend.title=element_text(size=11, face="bold"), 
          legend.text=element_text(size=11, face="bold",),
          legend.position=c(0.01,1.11),
          legend.spacing.x = unit(.05,"cm"),
          legend.justification=c(0,1),
          legend.text.align = 0) + 
    guides(color = guide_legend(override.aes = list(size = 4))) + #Makes the size of legend dots match the size of graph dots
    labs(x=xlab, y=ylab) +
    coord_cartesian(clip = "off") #+ 
  plot(p1)
  ggsave(paste0("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/ValXVal_ADNIto",scan,".png"), plot= p1, width = 4.6, height =4, unit="in", dpi=500)
  
}

p1<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/ValXVal_ADNItoCSx6_1.0_a.png") %>% image_ggplot(interpolate = F)
p2<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/ValXVal_ADNItoWAVE_1.0_a.png") %>% image_ggplot(interpolate = F)

ValbyValGrid<-ggarrange(p1,p2,ncol = 2, nrow=1)
ggsave("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/ValbyValGrid.png",plot=ValbyValGrid,width = 9.2, height =4, unit="in", dpi=500)


#########################################
####Bland-Altman Plots for supplement
#########################################

biPlotVars<-c("Hippocampus","Amygdala","rostralanteriorcingulate","superiorfrontal","lateraloccipital","parahippocampal","entorhinal","MeanThickness")
breakVals<-c(750,900,.6,.4,.4,.7,1.1,.3)
totMins<-c(-1500,0,1.9,1.8,1.5,1.4,0.8,1.8)
totMaxs<-c(1500,1500,0.6,0.4,1,0.6,1,0.3)
axisLabels<-c("Hipp. Vol.","Amyg. Vol.","rACC Thk.","SFG Thk.","LOC Thk.","PHG Thk.","EC Thk.","Mean Thk.")
units<-c(expr(mm^3),expr(mm^3),"mm","mm","mm","mm","mm","mm")
biPlotParams<-tibble(region=biPlotVars,breakval=breakVals,totMin=totMins,totMAX=totMaxs,axisLabel=axisLabels,unit=units)


for(day in c(1,2)){ 
  for(scan in scanIDsShort[2:4]){ #scanIDs[2:21]){  ##Compare everything to ADNI
    for(index in 1:nrow(biPlotParams)){
      print(scan)
      
      brainVar <-  biPlotParams$region[index]
      breakval <- biPlotParams$breakval[index]
      totMIN <- biPlotParams$totMin[index]
      totMAX <- biPlotParams$totMAX[index]
      axisLabel<-biPlotParams$axisLabel[index]
      unit<-biPlotParams$unit[[index]]
      buffer<-breakval/4
      
      
      scanNameVars<-str_split(scan,pattern = "_")[[1]]
      yAxisLabel<-paste(scanNameVars[1]) %>% gsub("WAVE","WAVEx9",.)
      #yAxisLabel<-"CSx6 1.2mm"
      AdniAxisLabel<-paste("ADNI")
      
      xlab <- as.expression(bquote(Average ~ .(axisLabel)~(.(unit))))
      ylab <- as.expression(bquote(.(AdniAxisLabel)~-~~.(yAxisLabel)~ .(axisLabel)~(.(unit))))
      
      print(paste(scan,day,brainVar))
      plotDataLong <- allFSdataLong %>% filter(scanID %in% c(scan, "ADNI_1.0_a") & str_detect(region, brainVar) & session < 3 & session == day)
      plotDataLong <- plotDataLong %>% separate(c("Hemi", "region"),col = "region",sep = "-") %>% select(!Date)  %>% distinct()  
      plotDataWide <- plotDataLong %>% pivot_wider(names_from = c(region,scanID), names_sep = "_",values_from = value) %>% drop_na()
      
      scan1data<-sym(paste(brainVar,"ADNI_1.0_a",sep="_"))
      scan2data<-sym(paste(brainVar,scan,sep="_"))

      
      print(plotDataWide %>% select(ID) %>% unique() %>% nrow())
      
      # Calculate the mean and difference between the two variables
      plotDataWide<-plotDataWide %>% mutate(Avg = (eval(scan1data) + eval(scan2data)) / 2)
      plotDataWide<-plotDataWide %>% mutate(Dif = eval(scan1data) - eval(scan2data))
      
      #scale_x_continuous(limits=c(totMIN-buffer, totMAX+buffer), breaks=c(seq(totMIN, totMAX, breakval)))+
        
      
      # Create the plot
      p1<-ggplot(plotDataWide) +
        geom_point(aes(x= Avg, y= Dif, shape= Hemi, colour=Hemi),  size=4) +
        geom_hline(yintercept = mean(plotDataWide$Dif), colour = "blue", size = 0.5) +
        geom_hline(yintercept = mean(plotDataWide$Dif) - (1.96 * sd(plotDataWide$Dif)), colour = "red", size = 0.5) +
        geom_hline(yintercept = mean(plotDataWide$Dif) + (1.96 * sd(plotDataWide$Dif)), colour = "red", size = 0.5) +
        scale_shape_manual(name=" ", values=c("\u25C4","\u25BA"))+
        scale_colour_manual(values=Palette, name= " ") +
        theme_classic() +
        scale_y_continuous(limits=c(-totMAX-buffer, totMAX+buffer), breaks=c(seq(-totMAX, totMAX, totMAX/2)))+
        theme(text = element_text(family = "Geneva"),
              axis.line = element_line(size = 0.75),
              axis.ticks = element_line(colour = "black", size = 0.75),
              plot.title=element_text(size=16, face="bold"),		
              plot.margin = margin(0.2,0.1,0.1,0.1, "in"),
              axis.title.x=element_text(size=12, face="bold", margin = margin(t = 5, r = 0, b = 0, l = 0)), 		
              axis.title.y=element_text(size=12, face="bold", margin = margin(t = 0, r = 5, b = 0, l = 0)), 
              axis.text.y=element_text(size=11, colour="black", face="bold"), 
              axis.text.x=element_text( size=11, colour="black", face="bold"), 
              legend.title=element_blank(), 
              legend.text=element_text(size=11, face="bold",),
              legend.position=c(0.01,1.05), 
              legend.justification=c(0,1),
              legend.background = element_rect(fill = "transparent"),
              legend.text.align = 0) + 
        labs(x=xlab, y=ylab) 
      
      ggsave(paste0("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/BlandAltman_ADNIto",scan,"_",brainVar,"_Day",day,".png"), plot= p1, width = 6, height =4, unit="in", dpi=500)
    }
  }
}

###unilateral bland altman plots
uniPlotVars<-c("eTIV","BrainSegVolNotVentSurf")
breakVals<-c(.25,.5)
totMins<-c(0,0)
totMaxs<-c(100,100)
axisLabels<-c("eTIV","WBV")
units<-c(expr(cm^3),expr(cm^3))
yMargin<-c(5,14.5)  
uniPlotParams<-tibble(region=uniPlotVars,breakval=breakVals,totMin=totMins,totMAX=totMaxs,axisLabel=axisLabels,unit=units,yMargin=yMargin)


for(day in c(1,2)){ 
  for(scan in scanIDsShort[2:4]){ #scanIDs[2:21]){  ##Compare everything to ADNI
    for(index in 1:nrow(uniPlotParams)){
      print(scan)
      
      brainVar <-  uniPlotParams$region[index]
      breakval <- uniPlotParams$breakval[index]
      totMIN <- uniPlotParams$totMin[index]
      totMAX <- uniPlotParams$totMAX[index]
      axisLabel<-uniPlotParams$axisLabel[index]
      unit<-uniPlotParams$unit[[index]]
      buffer<-breakval/4
      
      scanNameVars<-str_split(scan,pattern = "_")[[1]]
      yAxisLabel<-paste(scanNameVars[1]) %>% gsub("WAVE","WAVEx9",.)
      #yAxisLabel<-"CSx6 1.2mm"
      AdniAxisLabel<-paste("ADNI")
      
      xlab <- as.expression(bquote(Average ~ .(axisLabel)~(.(unit))))
      ylab <- as.expression(bquote(.(AdniAxisLabel)~-~~.(yAxisLabel)~ .(axisLabel)~(.(unit))))
      

      plotDataLong <- allFSdataLong %>% filter(scanID %in% c(scan, "ADNI_1.0_a") & region == brainVar & session < 3 & session == day)
      plotDataLong <- plotDataLong %>% select(!Date) %>% select(!origData) %>% distinct() %>% mutate(value=value/1000)
      plotDataWide <- plotDataLong %>% pivot_wider(names_from = c(region,scanID), names_sep = "_",values_from = value) %>% drop_na()
      
      scan1data<-sym(paste(brainVar,"ADNI_1.0_a",sep="_"))
      scan2data<-sym(paste(brainVar,scan,sep="_"))
      
      print(plotDataWide %>% select(ID) %>% unique() %>% nrow())
      
      # Calculate the mean and difference between the two variables
      plotDataWide<-plotDataWide %>% mutate(Avg = (eval(scan1data) + eval(scan2data)) / 2)
      plotDataWide<-plotDataWide %>% mutate(Dif = eval(scan1data) - eval(scan2data))
      
      # Create the plot
      p1<-ggplot(plotDataWide) +
        geom_point(aes(x= Avg, y= Dif),  size=4) +
        geom_hline(yintercept = mean(plotDataWide$Dif), colour = "blue", size = 0.5) +
        geom_hline(yintercept = mean(plotDataWide$Dif) - (1.96 * sd(plotDataWide$Dif)), colour = "red", size = 0.5) +
        geom_hline(yintercept = mean(plotDataWide$Dif) + (1.96 * sd(plotDataWide$Dif)), colour = "red", size = 0.5) +
        scale_shape_manual(name=" ", values=c("\u25C4","\u25BA"))+
        scale_colour_manual(values=Palette, name= " ") +
        theme_classic() +
        scale_y_continuous(limits=c(-totMAX-buffer, totMAX+buffer), breaks=c(seq(-totMAX, totMAX, totMAX/2)))+
        theme(text = element_text(family = "Geneva"),
              axis.line = element_line(size = 0.75),
              axis.ticks = element_line(colour = "black", size = 0.75),
              plot.title=element_text(size=16, face="bold"),		
              plot.margin = margin(0.2,0.1,0.1,0.1, "in"),
              axis.title.x=element_text(size=12, face="bold", margin = margin(t = 5, r = 0, b = 0, l = 0)), 		
              axis.title.y=element_text(size=12, face="bold", margin = margin(t = 0, r = 5, b = 0, l = 0)), 
              axis.text.y=element_text(size=11, colour="black", face="bold"), 
              axis.text.x=element_text( size=11, colour="black", face="bold"), 
              legend.title=element_blank(), 
              legend.text=element_text(size=11, face="bold",),
              legend.position=c(0.01,1.05), 
              legend.justification=c(0,1),
              legend.background = element_rect(fill = "transparent"),
              legend.text.align = 0) + 
        labs(x=xlab, y=ylab) 
      
      ggsave(paste0("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/BlandAltman_ADNIto",scan,"_",brainVar,"_Day",day,".png"), plot= p1, width = 6, height =4, unit="in", dpi=500)
    }
  }
}


############################
#######Make  Grids out of plots to save time In building powerpoint figures
############################

#CSx6 to ADNI
p1<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/BlandAltman_ADNItoCSx6_1.0_a_eTIV_Day1.png") %>% image_ggplot(interpolate = F)
p2<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/BlandAltman_ADNItoCSx6_1.0_a_eTIV_Day2.png") %>% image_ggplot(interpolate = F)
p3<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/BlandAltman_ADNItoCSx6_1.0_a_BrainSegVolNotVentSurf_Day1.png") %>% image_ggplot(interpolate = F)
p4<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/BlandAltman_ADNItoCSx6_1.0_a_BrainSegVolNotVentSurf_Day2.png") %>% image_ggplot(interpolate = F)
p5<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/BlandAltman_ADNItoCSx6_1.0_a_Hippocampus_Day1.png") %>% image_ggplot(interpolate = F)
p6<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/BlandAltman_ADNItoCSx6_1.0_a_Hippocampus_Day2.png") %>% image_ggplot(interpolate = F)
p7<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/BlandAltman_ADNItoCSx6_1.0_a_Amygdala_Day1.png") %>% image_ggplot(interpolate = F)
p8<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/BlandAltman_ADNItoCSx6_1.0_a_Amygdala_Day2.png") %>% image_ggplot(interpolate = F)

Valgrid1<-ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,ncol = 2, nrow=4)
ggsave("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/BlandAltmanGrid1_ADNItoCSx6.png",plot=Valgrid1,width = 12, height =16, unit="in", dpi=500)

p11<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/BlandAltman_ADNItoCSx6_1.0_a_MeanThickness_Day1.png") %>% image_ggplot(interpolate = F)
p12<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/BlandAltman_ADNItoCSx6_1.0_a_MeanThickness_Day2.png") %>% image_ggplot(interpolate = F)
p13<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/BlandAltman_ADNItoCSx6_1.0_a_superiorfrontal_Day1.png") %>% image_ggplot(interpolate = F)
p14<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/BlandAltman_ADNItoCSx6_1.0_a_superiorfrontal_Day2.png") %>% image_ggplot(interpolate = F)
#p3<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/BlandAltman_ADNItoCSx6_1.0_a_lateraloccipital_Day1.png") %>% image_ggplot(interpolate = F)
#p4<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/BlandAltman_ADNItoCSx6_1.0_a_lateraloccipital_Day2.png") %>% image_ggplot(interpolate = F)
p15<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/BlandAltman_ADNItoCSx6_1.0_a_parahippocampal_Day1.png") %>% image_ggplot(interpolate = F)
p16<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/BlandAltman_ADNItoCSx6_1.0_a_parahippocampal_Day2.png") %>% image_ggplot(interpolate = F)
p17<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/BlandAltman_ADNItoCSx6_1.0_a_rostralanteriorcingulate_Day1.png") %>% image_ggplot(interpolate = F)
p18<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/BlandAltman_ADNItoCSx6_1.0_a_rostralanteriorcingulate_Day2.png") %>% image_ggplot(interpolate = F)

Valgrid2<-ggarrange(p11,p12,p13,p14,p15,p16,p17,p18,ncol = 2, nrow=4)
ggsave("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/BlandAltmanGrid2_ADNItoCSx6.png",plot=Valgrid2,width = 12, height =16, unit="in", dpi=500)

####New Grid for Revise and Resubmit - Session 1
Valgrid3<-ggarrange(p1,p3,p5,p7,p11,p13,p15,p17,ncol = 2, nrow=4)
ggsave("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/BlandAltmanGrid3_ADNItoCSx6.png",plot=Valgrid3,width = 12, height =16, unit="in", dpi=500)

####New Grid for Revise and Resubmit Supplemental - Session 2
Valgrid4<-ggarrange(p2,p4,p6,p8,p12,p14,p16,p18,ncol = 2, nrow=4)
ggsave("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/BlandAltmanGrid4_ADNItoCSx6.png",plot=Valgrid4,width = 12, height =16, unit="in", dpi=500)

#WAVE to ADNI
p1<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/BlandAltman_ADNItoWAVE_1.0_a_eTIV_Day1.png") %>% image_ggplot(interpolate = F)
p2<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/BlandAltman_ADNItoWAVE_1.0_a_eTIV_Day2.png") %>% image_ggplot(interpolate = F)
p3<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/BlandAltman_ADNItoWAVE_1.0_a_BrainSegVolNotVentSurf_Day1.png") %>% image_ggplot(interpolate = F)
p4<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/BlandAltman_ADNItoWAVE_1.0_a_BrainSegVolNotVentSurf_Day2.png") %>% image_ggplot(interpolate = F)
p5<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/BlandAltman_ADNItoWAVE_1.0_a_Hippocampus_Day1.png") %>% image_ggplot(interpolate = F)
p6<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/BlandAltman_ADNItoWAVE_1.0_a_Hippocampus_Day2.png") %>% image_ggplot(interpolate = F)
p7<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/BlandAltman_ADNItoWAVE_1.0_a_Amygdala_Day1.png") %>% image_ggplot(interpolate = F)
p8<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/BlandAltman_ADNItoWAVE_1.0_a_Amygdala_Day2.png") %>% image_ggplot(interpolate = F)

Valgrid1<-ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,ncol = 2, nrow=4)
ggsave("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/BlandAltmanGrid1_ADNItoWAVE.png",plot=Valgrid1,width = 12, height =16, unit="in", dpi=500)

p11<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/BlandAltman_ADNItoWAVE_1.0_a_MeanThickness_Day1.png") %>% image_ggplot(interpolate = F)
p12<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/BlandAltman_ADNItoWAVE_1.0_a_MeanThickness_Day2.png") %>% image_ggplot(interpolate = F)
p13<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/BlandAltman_ADNItoWAVE_1.0_a_superiorfrontal_Day1.png") %>% image_ggplot(interpolate = F)
p14<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/BlandAltman_ADNItoWAVE_1.0_a_superiorfrontal_Day2.png") %>% image_ggplot(interpolate = F)
#p3<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/BlandAltman_ADNItoWAVE_1.0_a_lateraloccipital_Day1.png") %>% image_ggplot(interpolate = F)
#p4<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/BlandAltman_ADNItoWAVE_1.0_a_lateraloccipital_Day2.png") %>% image_ggplot(interpolate = F)
p15<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/BlandAltman_ADNItoWAVE_1.0_a_parahippocampal_Day1.png") %>% image_ggplot(interpolate = F)
p16<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/BlandAltman_ADNItoWAVE_1.0_a_parahippocampal_Day2.png") %>% image_ggplot(interpolate = F)
p17<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/BlandAltman_ADNItoWAVE_1.0_a_rostralanteriorcingulate_Day1.png") %>% image_ggplot(interpolate = F)
p18<-image_read("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/BlandAltman_ADNItoWAVE_1.0_a_rostralanteriorcingulate_Day2.png") %>% image_ggplot(interpolate = F)

Valgrid2<-ggarrange(p11,p12,p13,p14,p15,p16,p17,p18,ncol = 2, nrow=4)
ggsave("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/BlandAltmanGrid2_ADNItoWAVE.png",plot=Valgrid2,width = 12, height =16, unit="in", dpi=500)

####New Grid for Revise and Resubmit - Session 1
Valgrid3<-ggarrange(p1,p3,p5,p7,p11,p13,p15,p17,ncol = 2, nrow=4)
ggsave("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/BlandAltmanGrid3_ADNItoWAVE.png",plot=Valgrid3,width = 12, height =16, unit="in", dpi=500)

####New Grid for Revise and Resubmit Supplemental - Session 2
Valgrid4<-ggarrange(p2,p4,p6,p8,p12,p14,p16,p18,ncol = 2, nrow=4)
ggsave("/Users/maxwellelliott/MyFiles/Manuscripts/precisionRapidMRI_Triarchy/Visualizations/Plots/BlandAltmanGrid4_ADNItoWAVE.png",plot=Valgrid4,width = 12, height =16, unit="in", dpi=500)




