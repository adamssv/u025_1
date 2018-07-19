library("shiny")
#library("DT")

library(RCurl)
library(RJSONIO)
rm(list=ls())

#get data as JSON
getgopr<- postForm(
  .opts=curlOptions(ssl.verifypeer=TRUE),
  uri=Sys.getenv("GO"),
  token=Sys.getenv("GO_TOKEN"),
  content='record',
  format='json',
  type='flat',
  rawOrLabel='raw',
  rawOrLabelHeaders='raw',
  exportCheckboxLabel='false',
  exportSurveyFields='false',
  exportDataAccessGroups='false',
  returnFormat='json' ,
  fields='
  u025_ptid,
  u025,
  u025_observational_arm_consent_complete,
  u025_ucipath,
  u025_telepath,
  u025_er,
  u025_pr,
  u025_her2,
  u025_her2_fish,
  u025_stage,
  u025_exitreason,
  u025_exit ,
  u025_consent_done,
  u025_specconsent_done,
  u025_reconsent1_done,
  u025_reconsent2_done,
  u025_reconsent3_done,
  u025_v1_done,
  u025_ucipath_done,
  u025_v2_done,
  u025_v2_1_done,
  u025_v2_2_done,
  u025_v2_3_done,
  u025_v2_4_done,
  u025_v2_5_done,
  u025_v2_6_done,
  u025_v2_7_done,
  u025_v2_8_done,
  u025_biopsy_done,
  u025_v3_done,
  u025_telepath_done,
  u025_receptor_done
  '
)
gopr<-data.frame(do.call("rbind",fromJSON(getgopr)),stringsAsFactors = FALSE)

row.names(gopr)<-gopr[,"u025_ptid"]

gopr<-gopr[which(gopr$u025==1),]

intlist<-c("u025","u025_observational_arm_consent_complete","u025_ucipath","u025_telepath",
               "u025_er","u025_pr","u025_her2","u025_her2_fish","u025_stage","u025_exitreason",
               "u025_exit___1","u025_consent_done","u025_specconsent_done","u025_reconsent1_done",
               "u025_reconsent2_done","u025_reconsent3_done","u025_v1_done","u025_ucipath_done",
               "u025_v2_done","u025_v2_1_done","u025_v2_2_done","u025_v2_3_done","u025_v2_4_done",
               "u025_v2_5_done","u025_v2_6_done","u025_v2_7_done","u025_v2_8_done","u025_biopsy_done",
               "u025_v3_done","u025_telepath_done","u025_receptor_done")

indicators<-c("u025","u025_observational_arm_consent_complete",
              "u025_exit___1","u025_consent_done","u025_specconsent_done","u025_reconsent1_done",
              "u025_reconsent2_done","u025_reconsent3_done","u025_v1_done","u025_ucipath_done",
              "u025_v2_done","u025_v2_1_done","u025_v2_2_done","u025_v2_3_done","u025_v2_4_done",
              "u025_v2_5_done","u025_v2_6_done","u025_v2_7_done","u025_v2_8_done","u025_biopsy_done",
              "u025_v3_done","u025_telepath_done","u025_receptor_done")

results<-c("u025_stage","u025_exitreason","u025_ucipath","u025_telepath","u025_er","u025_pr","u025_her2","u025_her2_fish")

gopr[,intlist]<-lapply(gopr[,intlist], as.integer)

gopr[,results][is.na(gopr[,results])]<-9999
gopr[,indicators][is.na(gopr[,indicators])]<-0


report1<-matrix('',nrow=2, ncol=2)
i<-0
report1[i<-i+1,]<-c("Participants Registered",sum(gopr$u025))
report1[i<-i+1,]<-c("Confirmed Consents (Enrolled)",sum(gopr$u025_observational_arm_consent_complete==2))
#report1<-data.frame(report1[,3],row.names=report1[,1])

#PATHOLOGY
i<-0
report2<-matrix('',nrow=6, ncol=3)
#report2[i<-i+1,]<-c("UCI Pathology Result Reported",'',sum(gopr$u025_ucipath_done))
report2[i<-i+1,]<-c("Exited without Pathology",'',sum(gopr$u025_ucipath_done==0 & gopr$u025_exit___1==1))
report2[i<-i+1,]<-c("UCI Pathology Results Pending",'',sum(gopr$u025_ucipath_done==0 & gopr$u025_exit___1==0)) 

report2[i<-i+1,]<-c("UCI Pathology Results Received from Lab",'',sum(gopr$u025_ucipath_done))
report2[i<-i+1,]<-c('',"Invasive Breast Cancer Confirmed (UCI Pathology)",sum(gopr$u025_ucipath==4 & gopr$u025_ucipath_done))
report2[i<-i+1,]<-c('',"Not Invasive Breast Cancer (UCI Pathology)",sum(gopr$u025_ucipath!=4 & gopr$u025_ucipath!=5 & gopr$u025_ucipath_done))
report2[i<-i+1,]<-c('',"Awaiting Final Determination (UCI Pathology)",sum(gopr$u025_ucipath==5 & gopr$u025_ucipath_done))
i<-0
report3<-matrix('',nrow=16, ncol=3)
report3[i<-i+1,]<-c("Visit 2.0 complete",'',sum(gopr$u025_v2_done))
report3[i<-i+1,]<-c("Standard of Care Visits",'','')
report3[i<-i+1,]<-c('','Visit 2.1',sum(gopr$u025_v2_1_done))
report3[i<-i+1,]<-c('','Visit 2.2',sum(gopr$u025_v2_2_done))
report3[i<-i+1,]<-c('','Visit 2.3',sum(gopr$u025_v2_3_done))
report3[i<-i+1,]<-c('','Visit 2.4',sum(gopr$u025_v2_4_done))
report3[i<-i+1,]<-c('','Visit 2.5',sum(gopr$u025_v2_5_done))
report3[i<-i+1,]<-c('','Visit 2.6',sum(gopr$u025_v2_6_done))
report3[i<-i+1,]<-c('','Visit 2.7',sum(gopr$u025_v2_7_done))
report3[i<-i+1,]<-c('','Visit 2.8',sum(gopr$u025_v2_8_done))
report3[i<-i+1,]<-c('Stage','','')
report3[i<-i+1,]<-c('','In situ',sum(gopr$u025_v2_1_done==1 & gopr$u025_stage==0))
report3[i<-i+1,]<-c('','Stage 1',sum(gopr$u025_v2_1_done==1 & gopr$u025_stage==1))
report3[i<-i+1,]<-c('','Stage 2',sum(gopr$u025_v2_1_done==1 & gopr$u025_stage==2))
report3[i<-i+1,]<-c('','Stage 3',sum(gopr$u025_v2_1_done==1 & gopr$u025_stage==3))
report3[i<-i+1,]<-c('','Stage 4',sum(gopr$u025_v2_1_done==1 & gopr$u025_stage==4))
i<-0
report4<-matrix('',nrow=11, ncol=3)
report4[i<-i+1,]<-c('Active Participants (Not Exited)','',sum(gopr$u025_exit___1==0))
report4[i<-i+1,]<-c('Exited','',sum(gopr$u025_exit___1==1))
report4[i<-i+1,]<-c('','Reason for Exit','')
report4[i<-i+1,]<-c('','Completed Study (Visit 3)',sum(gopr$u025_v3_done))
report4[i<-i+1,]<-c('','Not diagnosed with Invasive Breast Cancer',sum(gopr$u025_exitreason==0  ))
report4[i<-i+1,]<-c('','Insufficient Tissue at Biopsy',sum(gopr$u025_exitreason==7  ))
report4[i<-i+1,]<-c('','Died',sum(gopr$u025_exitreason==2  ))
report4[i<-i+1,]<-c('','Withdrew Consent',sum(gopr$u025_exitreason==3  ))
report4[i<-i+1,]<-c('','Loss to Follow-up',sum(gopr$u025_exitreason==5 ))
report4[i<-i+1,]<-c('','Investigator Decision',sum(gopr$u025_exitreason==4  ))
report4[i<-i+1,]<-c('','Other',sum(gopr$u025_exitreason==6  ))


options(shiny.host="0.0.0.0", shiny.port=7777)
runApp()

