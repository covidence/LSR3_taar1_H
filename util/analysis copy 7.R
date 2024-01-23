library(tidyverse)
library(readxl)
library(writexl)
library(meta)
library(netmeta)


rm(list=ls())

master<-read_xlsx("data/data_2024-01-22_LSR3_H.xlsx")

rob<-read_xlsx("data/rob_secondary_2024.01.23.xlsx")


#rob <- master %>% select(study_name, Overall) %>% rename(rob=Overall) %>% unique()


continuous_outcomes_smd<-c("overall", "positive", "negative", "functioning", "cognition", "depression")
continuous_outcomes_md<-c("weight","prolactin") #qtc interval not used here as data only from a single study that were caclulated using AUC
dichotomous_outcomes_common<-c("dropout_any", 'dropout_ae', "adverse_event", "response", "relapse",
                               "anticholinergic_symptom", "anxiety","agitation", "headache", "hypotension", "dizziness",
                               "nausea_vomitting", "sedation", "hyperprolactinemia", "qtc_prolongation",
                               "insomnia", "akathisia", "eps_symptoms", "weight_increased")
dichotomous_outcomes_rare<-c("death", "serious")

effic_outcomes<-c("overall", "positive", "negative", "functioning", "cognition", "depression", "response", "relapse")
primary_outcome<-"overall"

meta_outcome<-data.frame(outcome=NA,  comparison=NA, timepoint=NA, k=NA, n=NA,population=NA, duration=NA,
                         sm=NA,  low_bias=NA, moderate_bias=NA, high_bias=NA,
                         TE.placebo.random=NA, seTE.placebo.random=NA, 
                         TE.placebo.fixed=NA, seTE.placebo.fixed=NA, 
                         tau2=NA,  i2=NA,
                         TE.random=NA, seTE.random=NA, TE.fixed=NA, seTE.fixed=NA)

o<-"overall"
time<-"1 day-2 weeks"
comparison="taar1_vs_placebo"

for(o in c(continuous_outcomes_smd, continuous_outcomes_md, dichotomous_outcomes_common, dichotomous_outcomes_rare)){
  for(time in unique(master$timepoint)){
      
      for(comparison in c("taar1_vs_placebo", "taar1_vs_antipsychotic")){
        
        master_i<-master
        
        if(o %in% continuous_outcomes_md){
          master_i$mean<-master[[paste0(o,"_mean")]]
          master_i$sd<-master[[paste0(o,"_sd")]]
          master_i$n<-master[[paste0(o,"_n")]]
          prediction_true=TRUE
          random_true=TRUE
          fixed_true=TRUE #present both fixed and random effects due to small number of studies
          approach="Inverse"
          sm_used="MD"
          outcome_type="continuous"
          minust_trasformation=1
        } else if (o %in%  continuous_outcomes_smd){
          
          master_i$mean<-master[[paste0(o,"_mean")]]*(-1) #minus transformation as all of these are symptoms coded wiht negative for improvement
          master_i$sd<-master[[paste0(o,"_sd")]]
          master_i$n<-master[[paste0(o,"_n")]]
          sm_used="SMD"
          prediction_true=TRUE
          random_true=TRUE
          fixed_true=TRUE #present both fixed and random effects due to small number of studies
          approach="Inverse"
          outcome_type="continuous"
          
        } else if (o %in% dichotomous_outcomes_common){
          master_i$mean<-master[[paste0(o,"_e")]] #used as n with event 
          master_i$sd<-NA
          master_i$n<-master$randomized_n #Use the randomized n in the dichotomous outcomes, asssuming missing patients did not have the outcome (post-hoc from protcol regrding safety outcomes but this we usullay do)
          random_true=TRUE
          fixed_true=TRUE #present both fixed and random effects due to small number of studies
          prediction_true=TRUE
          approach="Inverse"
          sm_used="OR"
          outcome_type="dichotomous"
        } else if(o %in% dichotomous_outcomes_rare){
          master_i$mean<-master[[paste0(o,"_e")]] #used as n with event 
          master_i$sd<-NA
          master_i$n<-master$randomized_n
          random_true=FALSE #Do not present random-effects in outcomes deemed as rare
          prediction_true=FALSE
          fixed_true=TRUE
          approach="MH"
          sm_used="OR"
          outcome_type="dichotomous"
        }
        
        
        if(o %in% effic_outcomes){
          left_label="TAAR1 worse"
          right_label="TAAR1 better"
          prediction_subgroup_true=TRUE
          subgroup_hetstat_true= TRUE
          overall_hetstat_true = FALSE
          test_subgroup_true = FALSE
          overall_true = FALSE
          subgroup_true=TRUE  #present subgroups in all but without testing
        } else {
          left_label="TAAR1 better"
          right_label="TAAR1 worse"
          prediction_subgroup_true=FALSE
          subgroup_hetstat_true= FALSE
          overall_hetstat_true = TRUE
          test_subgroup_true= FALSE 
          overall_true = TRUE
          subgroup_true=TRUE #present subgroups in all but without testing
        }
        
        
        
        if(comparison=="taar1_vs_placebo"){
          master_i<-master_i %>% 
            filter(timepoint==time) %>%  #keep the timepoint
            filter(!is.na(mean)) %>% #keep only arms with data
            select(study_name, study_name_drug, crossover_periods, population, duration_weeks, drug_new, mean, sd, n) %>%  #select only the relevant data for the analysis 
            filter(drug_new=="TAAR1 agonist" | drug_new=="placebo")  #keep the arms relevant for the comparison 
        } else if(comparison=="taar1_vs_antipsychotic"){
          master_i<-master_i %>%  
             filter(timepoint==time) %>%  #keep the timepoint
              filter(!is.na(mean)) %>% #keep only arms with data
             dplyr::select(study_name, study_name_drug, crossover_periods, population, duration_weeks, drug_new, mean, sd, n) %>% 
              filter(drug_new=="TAAR1 agonist" | drug_new=="D2 antipsychotic") 
        }

        if(o %in% c(continuous_outcomes_md, continuous_outcomes_smd)){
          
          master_pooled_i<-master_i %>%  #pooling the different doses or drugs of the same category (not relevant here)
            group_by(study_name, study_name_drug, drug_new, population, duration_weeks)%>%
            summarise(n_an=sum(n),
                      mean_an=weighted.mean(mean,n), 
                      sd_an=sqrt(((sum((n - 1) * sd^2 + n * mean^2)) - sum(n) * mean_an^2)/(sum(n) -  1)))
          
          master_pooled_i_studies<-master_pooled_i %>% 
            select(study_name) %>% group_by(study_name) %>% summarise(count=n()) %>% filter(count!=1) #find studies with only data for one arm 
          
          master_pooled_i<-master_pooled_i %>% filter(study_name %in% master_pooled_i_studies$study_name) 
          
          if(nrow(master_pooled_i)>1){
          pairwise_i<-pairwise(data=master_pooled_i, studlab = study_name_drug, treat=drug_new,
                               mean=mean_an, sd=sd_an, n=n_an, sm=sm_used)
          
          pairwise_i<-as.data.frame(pairwise_i) %>% 
            filter(treat1=="TAAR1 agonist" | treat2=="TAAR1 agonist") %>%
            mutate(treat1_new=ifelse(treat1=="TAAR1 agonist" , treat1, treat2),
                   treat2_new=ifelse(treat1=="TAAR1 agonist" , treat2, treat1),
                   mean1_new=ifelse(treat1=="TAAR1 agonist" , mean1, mean2),
                   mean2_new=ifelse(treat1=="TAAR1 agonist" , mean2, mean1),
                   sd1_new=ifelse(treat1=="TAAR1 agonist" , sd1, sd2),
                   sd2_new=ifelse(treat1=="TAAR1 agonist" , sd2, sd1),
                   n1_new=ifelse(treat1=="TAAR1 agonist" ,n1, n2),
                   n2_new=ifelse(treat1=="TAAR1 agonist" , n2, n1)) %>%
            mutate(comp=paste0(treat1_new," vs. ", treat2_new)) %>%
            select(study_name, study_name_drug, duration_weeks,
                   comp, population, treat1_new, treat2_new, mean1_new, sd1_new, n1_new, mean2_new,  sd2_new, n2_new) %>% 
            mutate(n_total=n1_new + n2_new) %>%
            unique()
          
          
          rob_i<-rob %>% filter(outcome==o & timepoint==time)
          
          pairwise_i<- pairwise_i %>% left_join(rob_i) %>% 
              unique()
          
          meta_comp<-metacont(data=pairwise_i,  #method.random.ci = "HK",  adhoc.hakn.ci="se", #reml default for tau, Do not use HK correction due to <5 studies
                             mean.e = mean1_new, sd.e=sd1_new, n.e=n1_new, mean.c = mean2_new, sd.c=sd2_new, n.c=n2_new,
                             sm=sm_used, random=random_true, fixed=fixed_true,    
                             studlab = study_name_drug, prediction = prediction_true, subgroup = population, 
                             prediction.subgroup = prediction_subgroup_true)
          
          new_master_i_name<- paste0("master_i_", comparison,"_", time,"_",o)  
          new_pairwise_i_name <- paste0("pairwise_i_", comparison,"_", time,"_",o)  
          new_meta_comp_name <- paste0("meta_comp_", comparison,"_",time,"_", o)  
          assign( new_master_i_name, master_i)
          assign( new_pairwise_i_name, pairwise_i)
          assign(new_meta_comp_name, meta_comp)
          
          }

        } else if(o %in% c(dichotomous_outcomes_common, dichotomous_outcomes_rare)){
          
          master_pooled_i<-master_i %>%  #pooling the different doses or drugs of the same category (not relevant here)
            group_by(study_name, study_name_drug, crossover_periods, drug_new, population, duration_weeks)%>%
            summarise(n_an=sum(n),
                      e_an=sum(as.numeric(mean))) %>% 
            unique()
          
          master_pooled_i_studies<-master_pooled_i %>% 
            select(study_name_drug) %>% group_by(study_name_drug) %>% summarise(count=n()) %>% filter(count!=1) #find studies with only data for one arm 
          
          master_pooled_i<-master_pooled_i %>% filter(study_name_drug %in% master_pooled_i_studies$study_name_drug) 
        
            if(nrow(master_pooled_i)>1){
         pairwise_i<-pairwise(data=master_pooled_i, studlab = study_name, treat=drug_new,
                                  event=e_an, n=n_an, sm=sm_used)
         pairwise_i<-as.data.frame(pairwise_i) %>% 
            filter(treat1=="TAAR1 agonist" | treat2=="TAAR1 agonist") %>%
            mutate(treat1_new=ifelse(treat1=="TAAR1 agonist" , treat1, treat2),
                   treat2_new=ifelse(treat1=="TAAR1 agonist" , treat2, treat1),
                   event1_new=ifelse(treat1=="TAAR1 agonist" , event1, event2),
                   event2_new=ifelse(treat1=="TAAR1 agonist" , event2, event1),
                   n1_new=ifelse(treat1=="TAAR1 agonist" ,n1, n2),
                   n2_new=ifelse(treat1=="TAAR1 agonist" , n2, n1)) %>%
            mutate(comp=paste0(treat1_new," vs. ", treat2_new)) %>%
            select(study_name, study_name_drug, comp, duration_weeks,
                   population, crossover_periods, treat1_new, treat2_new, event1_new, n1_new, event2_new, n2_new, TE, seTE) %>%
           mutate(event1_new_corrected=ifelse(event1_new==0, 0.5, event1_new), #To correct for crosssover, i need to add 0.5 when 0.
                  event2_new_corrected=ifelse(event2_new==0, 0.5, event2_new),
                  subtr_event1_new_corrected=ifelse(n1_new- event1_new==0, 0.5, n1_new- event1_new),
                  subtr_event2_new_corrected=ifelse(n2_new- event1_new==0, 0.5, n2_new- event2_new)) %>% 
           mutate(varTE=ifelse(crossover_periods==1, seTE^2,       #correct for crossover studies using correlation of 0.2 and the formula in Elbourne
                               seTE^2-0.4*(n1_new+n2_new)/(sqrt(event1_new_corrected*subtr_event1_new_corrected*event2_new_corrected* subtr_event2_new_corrected))), 
                  n_total=(n1_new+n2_new)/crossover_periods) %>% 
           unique()
          
         rob_i<-rob %>% filter(outcome==o & timepoint==time)
         pairwise_i<- pairwise_i %>% left_join(rob_i) %>% 
           unique()
         
         if(length(unique(pairwise_i$crossover_periods))==1){
         meta_comp<-metabin(data=pairwise_i, 
                            event.e = event1_new, n.e=n1_new, event.c = event2_new, n.c=n2_new,
                            sm=sm_used, random=random_true, fixed=fixed_true,  
                            studlab = study_name_drug, prediction = prediction_true, subgroup=population)
         } else{
           meta_comp<-metagen(data=pairwise_i, #Present forest plots using crossover corrections
                              TE = TE, seTE=sqrt(varTE), 
                              sm=sm_used, random=random_true, fixed=fixed_true,  
                              studlab = study_name_drug, prediction = prediction_true, subgroup=population)
            }
         
         
         meta_comp_plac<-metaprop(data=pairwise_i, event=event2_new, n=n2_new, method="Inverse", #Inverse instead of GLMM was used because in some occassions GLMM did not run, Logit transformation was used
                                  random=random_true, fixed=fixed_true,  
                                  studlab = study_name_drug, prediction = prediction_true, subgroup=population)
         
         new_master_i_name<- paste0("master_i_", comparison,"_", time,"_",o)  
        new_pairwise_i_name <- paste0("pairwise_i_", comparison,"_", time,"_",o)  
         new_meta_comp_name <- paste0("meta_comp_", comparison,"_",time,"_", o)  
         assign( new_master_i_name, master_i)
         assign(new_pairwise_i_name, pairwise_i)
         assign(new_meta_comp_name, meta_comp)
         
           }
        }
        
        if(nrow(master_pooled_i)>1){        
      if((o %in% effic_outcomes) & length(meta_comp$subgroup.levels)>1){
        
        if(o %in% c(continuous_outcomes_md, continuous_outcomes_smd)){
            
            for(p in length(meta_comp$subgroup.levels)){
            
              
              meta_outcome_i<-data.frame(outcome=o, comparison=comparison,timepoint=time,
                                         k=as.integer(meta_comp$k.all.w[p]), 
                                         n=sum(pairwise_i[pairwise_i$population==meta_comp$subgroup.levels[p],]$n_total),
                                         population=meta_comp$subgroup.levels[p],
                                         duration=paste0(min(pairwise_i[pairwise_i$population==meta_comp$subgroup.levels[p],]$duration_weeks),"-", 
                                                         max(pairwise_i[pairwise_i$population==meta_comp$subgroup.levels[p],]$duration_weeks)),
                                         low_bias=sum(pairwise_i[pairwise_i$population==meta_comp$subgroup.levels[p],]$Overall=="Low", na.rm=TRUE), 
                                         moderate_bias=sum(pairwise_i[pairwise_i$population==meta_comp$subgroup.levels[p],]$Overall=="Some concerns", na.rm=TRUE), 
                                         high_bias=sum(pairwise_i[pairwise_i$population==meta_comp$subgroup.levels[p],]$Overall=="High", na.rm=TRUE),
                                         sm=sm_used,
                                         TE.placebo.random=NA, seTE.placebo.random=NA, 
                                         TE.placebo.fixed=NA, seTE.placebo.fixed=NA, 
                                         TE.random=as.double(meta_comp$TE.random.w[p]), seTE.random=as.double(meta_comp$seTE.random.w[p]),
                                         tau2=as.double(meta_comp$tau2.w[p]), 
                                         i2=as.double(meta_comp$I2.w[p]),
                                         TE.fixed=as.double(meta_comp$TE.fixed.w[p]), seTE.fixed=as.double(meta_comp$seTE.fixed.w[p]))
              meta_outcome<-rbind(meta_outcome, meta_outcome_i)
            }
        } else {
          for(p in length(meta_comp$subgroup.levels)){
            
            
            meta_outcome_i<-data.frame(outcome=o, comparison=comparison,timepoint=time,
                                       k=as.integer(meta_comp$k.all.w[p]), 
                                       n=sum(pairwise_i[pairwise_i$population==meta_comp$subgroup.levels[p],]$n_total),
                                       population=meta_comp$subgroup.levels[p],
                                       duration=paste0(min(pairwise_i[pairwise_i$population==meta_comp$subgroup.levels[p],]$duration_weeks),"-", 
                                                       max(pairwise_i[pairwise_i$population==meta_comp$subgroup.levels[p],]$duration_weeks)),
                                       sm=sm_used,
                                       low_bias=sum(pairwise_i[pairwise_i$population==meta_comp$subgroup.levels[p],]$Overall=="Low", na.rm=TRUE), 
                                       moderate_bias=sum(pairwise_i[pairwise_i$population==meta_comp$subgroup.levels[p],]$Overall=="Some concerns", na.rm=TRUE), 
                                       high_bias=sum(pairwise_i[pairwise_i$population==meta_comp$subgroup.levels[p],]$Overall=="High", na.rm=TRUE),
                                       TE.placebo.random=meta_comp_plac$TE.random.w[p], seTE.placebo.random=meta_comp_plac$seTE.random.w[p], 
                                       TE.placebo.fixed=meta_comp_plac$TE.fixed.w[p], seTE.placebo.fixed=meta_comp_plac$seTE.fixed.w[p], 
                                       TE.random=as.double(meta_comp$TE.random.w[p]), seTE.random=as.double(meta_comp$seTE.random.w[p]),
                                       tau2=as.double(meta_comp$tau2.w[p]), 
                                       i2=as.double(meta_comp$I2.w[p]),
                                       TE.fixed=as.double(meta_comp$TE.fixed.w[p]), seTE.fixed=as.double(meta_comp$seTE.fixed.w[p]))
            meta_outcome<-rbind(meta_outcome, meta_outcome_i)
            
            
          }
        }} else {
          
        if(o %in% c(continuous_outcomes_md, continuous_outcomes_smd)){
           meta_outcome_i<-data.frame(outcome=o, comparison=comparison,timepoint=time, 
                                     k=meta_comp$k.all, 
                                     n=sum(pairwise_i$n_total),
                                     population=paste0(meta_comp$subgroup.levels),
                                     sm=sm_used, 
                                     low_bias=sum(pairwise_i$Overall=="Low", na.rm=TRUE), 
                                     moderate_bias=sum(pairwise_i$Overall=="Some concerns", na.rm=TRUE), 
                                     high_bias=sum(pairwise_i$Overall=="High", na.rm=TRUE),
                                     duration=paste0(min(pairwise_i$duration_weeks),"-", max(pairwise_i$duration_weeks)),
                                     TE.placebo.random=NA, seTE.placebo.random=NA, 
                                     TE.placebo.fixed=NA, seTE.placebo.fixed=NA, 
                                     TE.random=meta_comp$TE.random, seTE.random=meta_comp$seTE.random, 
                                     tau2=meta_comp$tau2, i2=meta_comp$I2,
                                     TE.fixed=meta_comp$TE.fixed, seTE.fixed=meta_comp$seTE.fixed)
           meta_outcome<-rbind(meta_outcome, meta_outcome_i)
          }
           else{
          meta_outcome_i<-data.frame(outcome=o, comparison=comparison,timepoint=time, 
                                     k=meta_comp$k.all,
                                     n=sum(pairwise_i$n_total), 
                                     low_bias=sum(pairwise_i$Overall=="Low", na.rm=TRUE), 
                                     moderate_bias=sum(pairwise_i$Overall=="Some concerns", na.rm=TRUE), 
                                     high_bias=sum(pairwise_i$Overall=="High", na.rm=TRUE),
                                     population=paste0(meta_comp$subgroup.levels, collapse=", "),
                                     duration=paste0(min(pairwise_i$duration_weeks),"-", max(pairwise_i$duration_weeks)),
                                     sm=sm_used, 
                                     TE.placebo.random=meta_comp_plac$TE.random, seTE.placebo.random=meta_comp_plac$seTE.random, 
                                     TE.placebo.fixed=meta_comp_plac$TE.fixed, seTE.placebo.fixed=meta_comp_plac$TE.fixed, 
                                     TE.random=meta_comp$TE.random, seTE.random=meta_comp$seTE.random,
                                     tau2=meta_comp$tau2, i2=meta_comp$I2,
                                     TE.fixed=meta_comp$TE.fixed, seTE.fixed=meta_comp$seTE.fixed)
          meta_outcome<-rbind(meta_outcome, meta_outcome_i)
        }
        
        }
        }
        }
        
  }
}
   
      
#Conducting analysis for QTc interval msec using the data from Tsukada et al 2023.
master_qtc<-master[master$study_name=="Tsukada (2023)" & master$drug_name=="ulotaront",]
master_qtc<-master_qtc %>% select(study_name, study_name_drug, qtc_md_point, qtc_md_se, qtc_prolongation_n, timepoint, duration_weeks, population) %>%
  mutate(comparison="taar1_vs_placebo")

rob_qtc<-rob %>% filter(outcome=="qtc_interval" & timepoint=="1 day-2 weeks")

master_qtc<-master_qtc %>% left_join(rob_qtc)

`meta_comp_taar1_vs_antipsychotic_1 day-2 weeks_qtc_interval`<- metagen(data=master_qtc,
               TE=qtc_md_point, seTE=qtc_md_se, sm="MD", studlab = study_name_drug)


meta_outcome_qtc<-data.frame(outcome="qtc_interval", comparison="taar1_vs_placebo",timepoint=unique(master_qtc$timepoint), 
                           k=`meta_comp_taar1_vs_antipsychotic_1 day-2 weeks_qtc_interval`$k.all,
                           n=sum(master_qtc$qtc_prolongation_n),
                           population=master_qtc$population,
                           low_bias=sum(master_qtc$Overall=="Low", na.rm=TRUE), 
                           moderate_bias=sum(master_qtc$Overall=="Some concerns", na.rm=TRUE), 
                           high_bias=sum(master_qtc$Overall=="High", na.rm=TRUE),
                           duration=paste0(min(master_qtc$duration_weeks),"-", max(master_qtc$duration_weeks)),
                           sm="MD", 
                           TE.placebo.random=NA, seTE.placebo.random=NA, 
                           TE.placebo.fixed=NA, seTE.placebo.fixed=NA, 
                           TE.random=`meta_comp_taar1_vs_antipsychotic_1 day-2 weeks_qtc_interval`$TE.random, seTE.random=`meta_comp_taar1_vs_antipsychotic_1 day-2 weeks_qtc_interval`$seTE.random,
                           tau2=`meta_comp_taar1_vs_antipsychotic_1 day-2 weeks_qtc_interval`$tau2, 
                            i2=`meta_comp_taar1_vs_antipsychotic_1 day-2 weeks_qtc_interval`$I2,
                           TE.fixed=`meta_comp_taar1_vs_antipsychotic_1 day-2 weeks_qtc_interval`$TE.fixed, seTE.fixed=`meta_comp_taar1_vs_antipsychotic_1 day-2 weeks_qtc_interval`$seTE.fixed)

meta_outcome<-rbind(meta_outcome, meta_outcome_qtc) 

#Prepartion of the meta outcome object
meta_outcome<-meta_outcome %>% filter(!is.na(outcome))

meta_outcome2<-meta_outcome %>% 
  mutate(TE=ifelse(outcome=="serious" | outcome=="death", TE.fixed, TE.random),
         seTE=ifelse(outcome=="serious" | outcome=="death", seTE.fixed, seTE.random),
         TE.placebo=ifelse(outcome=="serious" | outcome=="death", TE.placebo.fixed, TE.placebo.random),
         duration=ifelse(duration=="0.14-0.14", "1 day",
                         ifelse(duration=="0.14-2", "1 day to 2 weeks",
                                ifelse(duration=="4-4", "4 weeks", 
                                       ifelse(duration=="4-6", "4-6 weeks",
                                              ifelse(duration=="6-6", "6 weeks", NA)))))) %>% 
  mutate(outcome_type=ifelse(outcome %in% effic_outcomes, "efficacy", "other")) %>% 
  mutate(n_possible=ifelse(timepoint=="1 day-2 weeks" & comparison=="taar1_vs_antipsychotic", 100,
                           ifelse(timepoint=="1 day-2 weeks" & comparison=="taar1_vs_placebo", 626,
                                  ifelse(timepoint=="3-13 weeks" & comparison=="taar1_vs_placebo" & outcome_type=="efficacy" & population=="Schizophrenia spectrum", 1383,
                                         ifelse(timepoint=="3-13 weeks" & comparison=="taar1_vs_placebo" & outcome_type=="efficacy" & population=="Parkinson Disease Psychosis", 39,
                                                ifelse(timepoint=="3-13 weeks" & comparison=="taar1_vs_antipsychotic" & outcome_type=="efficacy" & population=="Schizophrenia spectrum", 128,1422)))))) %>%
  mutate(k_possible=ifelse(timepoint=="1 day-2 weeks" & comparison=="taar1_vs_antipsychotic", 2,
                           ifelse(timepoint=="1 day-2 weeks" & comparison=="taar1_vs_placebo", 12,
                                  ifelse(timepoint=="3-13 weeks" & comparison=="taar1_vs_placebo" & outcome_type=="efficacy" & population=="Schizophrenia spectrum", 4,
                                         ifelse(timepoint=="3-13 weeks" & comparison=="taar1_vs_placebo" & outcome_type=="efficacy" & population=="Parkinson Disease Psychosis", 1,
                                                ifelse(timepoint=="3-13 weeks" & comparison=="taar1_vs_antipsychotic" & outcome_type=="efficacy" & population=="Schizophrenia spectrum", 1,5)))))) %>%
  mutate(k_prop=k/k_possible, 
         n_prop=ifelse(k_prop==1, 1, n/n_possible),
         moderate_to_high_bias_prop=(moderate_bias+high_bias)/k) %>%
  mutate(TE_lb=TE-1.96*seTE, 
         TE_ub=TE+1.96*seTE) %>%
  mutate(cer_point=exp(TE.placebo)/(1+exp(TE.placebo))) %>%
  mutate(point=ifelse(sm=="OR", round(exp(TE), 2), round(TE, 2)),
         lb=ifelse(sm=="OR", round(exp(TE_lb), 2), round(TE_lb, 2)),
         ub=ifelse(sm=="OR", round(exp(TE_ub), 2), round(TE_ub, 2))) %>% 
  mutate(ACR=ifelse(sm=="OR", round(100*point*cer_point/(1-cer_point+point*cer_point),1), NA),
         CER=ifelse(!is.na(point), round(100*cer_point, 1), NA),
         t2=round(tau2,4)) %>% 
  mutate(association=ifelse(!is.na(point), 
                            ifelse(k>1 & !is.na(t2), ifelse(sm=="OR", paste0("N=", k, " n=", n, "; ", ACR,"% vs.", CER,"%, ", sm,"=",point,", 95%CI: ", lb, ", ", ub,"; tau2=", t2),
                                   paste0("N=", k, " n=", n, "; ", sm,"=",point,", 95%CI: ", lb, ", ", ub,"; tau2=", t2)), 
                                   ifelse(sm=="OR", paste0("N=", k, " n=", n, "; ", ACR,"% vs.", CER,"%, ", sm,"=",point,", 95%CI: ", lb, ", ", ub),
                                          paste0("N=", k, " n=", n, "; ", sm,"=",point,", 95%CI: ", lb, ", ", ub))),
                                   paste0("N=", k, " n=", n, "; not estimable effect size 0 events in both arms")),
         study_limitations=ifelse(moderate_to_high_bias_prop==0 & k==1, "1 study with an overall low risk of bias",
                                  ifelse(moderate_bias==1 & k==1, "1 study with overall some concerns in risk of bias",
                                         ifelse(high_bias==1 & k==1, "1 study with overall high risk of bias",
                                                paste0(round(100*(1-moderate_to_high_bias_prop),1),"% had overall low risk of bias")))),
         reporting_bias=ifelse(k_prop==1 & k_possible==1, "The single study eligible had usable data",
                               ifelse(k_prop==1 & k_possible>1, paste0("All eligible studies had usuable data"),
                                      ifelse(k_prop<1, paste0(round(100*k_prop,1), "% of eligible studies and ", round(100*n_prop,1),"% of participants had usable data"), NA)))) %>% 
  select(outcome, comparison, timepoint, duration, association, study_limitations, reporting_bias)




#Function to create RoB plots
rob_plot<-function(data){ #RoB function using the extended meta objects
  plot(rob_traffic_light(data=data$data[, 
                                        c("study_name","D1", "D2", "D3", "D4", "D5", "Overall")], tool = "ROB2",  psize = 12))
}

