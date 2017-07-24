library(rpart)
library(readxl)
library(magrittr)
library(tidyverse)
Gini_data <- read_xlsx(path = "Gini_data.xlsx",sheet = 1,col_names = TRUE)
colnames(Gini_data)[-1] = c("A1","A2","A3","A4","A5")
Gini_data$A1 = ifelse(Gini_data$A1 == "青年",1,ifelse(Gini_data$A1 == "中年",2,3))
Gini_data$A2 = ifelse(Gini_data$A2 == "是",1,2)
Gini_data$A3 = ifelse(Gini_data$A3 == "是",1,2)
Gini_data$A4 = ifelse(Gini_data$A4 == "非常好",1,ifelse(Gini_data$A4 == "好",2,3))
Gini_data$A5 = ifelse(Gini_data$A5 == "是",1,2)
Gini_data = Gini_data[,-1]
colnames(Gini_data)[5] = "class_type"


calcShannonEnt <- function(dataSet){
  if (!any(colnames(dataSet) == "class_type"))
    stop("The dataSet must have class_type")
  temp = (table(dataSet$class_type)/dim(dataSet)[1]) %>% 
    as.matrix() 
  return(crossprod(-temp,log2(temp)))
}
calcShannonEnt(Gini_data)
splitDataSet <- function(dataSet,var,value){
      express <- paste(var,"==",value) %>%
        parse(text = .)
      dataSet %>% select(var,"class_type") %>% 
        filter(eval(express)) %>%
        return
  }
chooseBestFeatureToSplit <- function(dataSet){
  ent_before =  calcShannonEnt(dataSet)
  names = colnames(dataSet)
  len = length(names)
  mutal_info = data.frame(var = names[-len],ent = rep(0,times = (length(names) - 1)))
  
  for (var_name in names[-len]) {
    temp_var = dataSet %>%
      group_by(eval(parse(text = var_name))) %>%
      summarise(cnt = n()) %>%
      mutate(prob = cnt / sum(cnt)) %>%
      set_colnames(c(var_name,"cnt","prob"))
     value_unique = select(dataSet,var_name) %>%
       unique()
     temp_class = data.frame(value_unique, ent = rep(0,times = dim(value_unique)[1])) %>%
       set_colnames(c(var_name,"ent"))
     
     for(val in 1:dim(value_unique)[1]) {
       idx = which(temp_class[,var_name] == val)
      temp_class[idx,2] = splitDataSet(dataSet = dataSet, var = var_name,value = val) %>%
         calcShannonEnt() 
     }
     temp = inner_join(x = temp_var,y = temp_class,by = var_name)
     mutal_info[mutal_info$var == var_name,"ent"] = ent_before - sum(temp$prob * temp$ent)
  }
  return(mutal_info)
}
chooseBestFeatureToSplit(dataSet = Gini_data)






