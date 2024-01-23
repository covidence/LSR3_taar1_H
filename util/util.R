library(meta)
library(robvis)

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


rob_plot<-function(data){ #RoB function 
  plot(rob_traffic_light(data=data$data[, 
                                  c("study_name","D1", "D2", "D3", "D4", "D5", "Overall")], tool = "ROB2",  psize = 12))
}
