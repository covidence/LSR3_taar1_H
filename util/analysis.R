library(tidyverse)
library(readxl)
library(writexl)
library(meta)
library(netmeta)


rm(list=ls())

master<-read_xlsx("data/data_2024-01-22_LSR3_H.xlsx")

rob <- master %>% select(study_name, Overall) %>% rename(rob=Overall) %>% unique()


continuous_outcomes_smd<-c("overall", "positive", "negative", "functioning", "cognition", "depression")
continuous_outcomes_md<-c("weight","prolactin") #qtc interval not used here as data only from a single study that were caclulated using AUC
dichotomous_outcomes_common<-c("dropout_any", 'dropout_ae', "adverse_event", "response", "relapse",
                               "anticholinertgic_symptom", "anxiety","agitation", "headache", "hypotension", "dizziness",
                               "nausea_vomitting", "sedation", "hyperprolactinemia", "qtc_prolongation",
                               "insomnia", "akathisia", "eps_symptoms", "weight_increased")
dichotomous_outcomes_rare<-c("death", "serious")

effic_outcomes<-c("overall", "positive", "negative", "functioning", "cognition", "depression", "response", "relapse")
primary_outcome<-"overall"

meta_outcome<-data.frame(outcome=NA,  comparison=NA, timepoint=NA, k=NA, n=NA,
                         sm=NA,  
                         TE.placebo.random=NA, seTE.placebo.random=NA, 
                         TE.placebo.fixed=NA, seTE.placebo.fixed=NA, 
                         TE.random=NA, seTE.random=NA, TE.fixed=NA, seTE.fixed=NA)

o<-"dropout_ae"
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
            select(study_name, study_name_drug, crossover_periods, population, drug_new, mean, sd, n) %>%  #select only the relevant data for the analysis 
            filter(drug_new=="TAAR1 agonist" | drug_new=="placebo")  #keep the arms relevant for the comparison 
        } else if(comparison=="taar1_vs_antipsychotic"){
          master_i<-master_i %>%  
             filter(timepoint==time) %>%  #keep the timepoint
              filter(!is.na(mean)) %>% #keep only arms with data
             dplyr::select(study_name, study_name_drug, crossover_periods, population, drug_new, mean, sd, n) %>% 
              filter(drug_new=="TAAR1 agonist" | drug_new=="D2 antipsychotic") 
        }
       
        if(o %in% c(continuous_outcomes_md, continuous_outcomes_smd)){
          
          master_pooled_i<-master_i %>%  #pooling the different doses or drugs of the same category (not relevant here)
            group_by(study_name, study_name_drug, drug_new, population)%>%
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
            select(study_name, study_name_drug, 
                   comp, population, treat1_new, treat2_new, mean1_new, sd1_new, n1_new, mean2_new,  sd2_new, n2_new) %>% 
            mutate(n_total=n1_new + n2_new) %>%
            unique()
          
          if(o %in% primary_outcome){
            pairwise_i<- pairwise_i %>% left_join(rob) %>% 
              unique()
          }
          
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
            group_by(study_name, study_name_drug, crossover_periods, drug_new, population)%>%
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
            select(study_name, study_name_drug, comp, 
                   population, crossover_periods, treat1_new, treat2_new, event1_new, n1_new, event2_new, n2_new, TE, seTE) %>%
           mutate(event1_new_corrected=ifelse(event1_new==0, 0.5, event1_new), #To correct for crosssover, i need to add 0.5 when 0.
                  event2_new_corrected=ifelse(event2_new==0, 0.5, event2_new),
                  subtr_event1_new_corrected=ifelse(n1_new- event1_new==0, 0.5, n1_new- event1_new),
                  subtr_event2_new_corrected=ifelse(n2_new- event1_new==0, 0.5, n2_new- event2_new)) %>% 
           mutate(varTE=ifelse(crossover_periods==1, seTE^2,       #correct for crossover studies using correlation of 0.2 and the formula in Elbourne
                               seTE^2-0.4*(n1_new+n2_new)/(sqrt(event1_new_corrected*subtr_event1_new_corrected*event2_new_corrected* subtr_event2_new_corrected))), 
                  n_total=(n1_new+n2_new)/crossover_periods) %>% 
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
         
         if(o %in% c(continuous_outcomes_md, continuous_outcomes_smd)){
           meta_outcome_i<-data.frame(outcome=o, comparison=comparison,timepoint=time, 
                                      k=meta_comp$k.all,
                                      n=sum(pairwise_i$n_total),
                                      sm=sm_used, 
                                      TE.placebo.random=NA, seTE.placebo.random=NA, 
                                      TE.placebo.fixed=NA, seTE.placebo.fixed=NA, 
                                      TE.random=meta_comp$TE.random, seTE.random=meta_comp$seTE.random,
                                      TE.fixed=meta_comp$TE.fixed, seTE.fixed=meta_comp$seTE.fixed)
         } else{
           meta_outcome_i<-data.frame(outcome=o, comparison=comparison,timepoint=time, 
                                     k=meta_comp$k.all,
                                     n=sum(pairwise_i$n_total),
                                     sm=sm_used, 
                                     TE.placebo.random=meta_comp_plac$TE.random, seTE.placebo.random=meta_comp_plac$seTE.random, 
                                     TE.placebo.fixed=meta_comp_plac$TE.fixed, seTE.placebo.fixed=meta_comp_plac$TE.fixed, 
                                     TE.random=meta_comp$TE.random, seTE.random=meta_comp$seTE.random,
                                     TE.fixed=meta_comp$TE.fixed, seTE.fixed=meta_comp$seTE.fixed)
         }
         
           meta_outcome<-rbind(meta_outcome, meta_outcome_i)
           }
         }
         
         
         
            }
        }
        
       
        
      }

  




forest_presentation<-function(meta_comp, outcome){
  
  if(outcome=="efficacy"){
    meta::forest(meta_comp, 
                 label.e = unique(meta_comp$data$treat1_new),
                 label.c=unique(meta_comp$data$treat2_new),
                 subgroup.hetstat = TRUE, overall.hetstat =FALSE, test.subgroup.random = FALSE,
                 overall = FALSE, prediction.subgroup = FALSE,
                 subgroup = TRUE,
                 label.left = "TAAR1 worse", label.right="TAAR1 better")
  } else if(outcome=="safety") {
    meta::forest(meta_comp, prediction = FALSE,
                 label.e = unique(meta_comp$data$treat1_new), label.c=unique(meta_comp$data$treat2_new),
                 label.left = "TAAR1 better",
                 label.right="TAAR1 worse", 
                 subgroup.hetstat = TRUE, overall.hetstat =TRUE, test.subgroup.random = FALSE, test.subgroup.fixed = FALSE,
                 overall = TRUE, prediction.subgroup = FALSE,
                 subgroup = FALSE)
    
  }
  
}
