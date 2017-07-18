library(rpart)
#data("kyphosis")
#dataSet = kyphosis
library(readxl)
#colnames(dataSet)[1] = "class_type"
Gini_data <- read_xlsx(path = "Gini_data.xlsx",sheet = 1,col_names = TRUE)
colnames(Gini_data)[-1] = c("A1","A2","A3","A4","A5")
Gini_data$A1 = ifelse(Gini_data$A1 == "青年",1,ifelse(Gini_data$A1=="中年",2,3))
Gini_data$A2 = ifelse(Gini_data$A2 == "是",1,2)
Gini_data$A3 = ifelse(Gini_data$A3 == "是",1,2)
Gini_data$A4 = ifelse(Gini_data$A4 == "非常好",1,ifelse(Gini_data$A4=="好",2,3))
Gini_data$A5 = ifelse(Gini_data$A5 == "是",1,2)

colnames(Gini_data)[6] = "class_type"
calcShannonEnt <- function(dataSet){
  if (!any(colnames(dataSet) == "class_type"))
    stop("The dataSet must have class_type")
  temp = (table(dataSet$class_type)/dim(dataSet)[1]) %>% 
    as.matrix() 
  return(crossprod(-temp,log2(temp)))
}
calcShannonEnt(Gini_data)

splitDataSet <- function(dataSet,var){
  n = length(unique(dataSet$var)) ###计算
  var_unique = unique(dataSet$var)
  list_temp <- vector(mode = "list",length = n)
  names(list_temp) = var_unique
  for(i in 1:n){
    list_temp = select(dataSet,var == var_unique[1])
  }
  
  
  for(var1 in unique(dataSet$var)){
    sum(dataSet$var==var1)/dim(dataSet)[1]
    
  }
}

chooseBestFeatureToSplit <- function(dataSet){
  
}