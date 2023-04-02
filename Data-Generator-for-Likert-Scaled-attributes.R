# @author Natasza Szczypien Jan 2023
# @author Frank Klawonn Jul 2022
######################################


# Dataset IDS (pure random dataset)
# Params:
#                n - sample size (nrow)
# max.likert.value - as the name sais
#       fix.likert - if true all Questionairies have max likert
#                    if max likert will be sampled for each questionaierie
#       nquestions - number of questions (columns) to generate          
Generate_IDS_Datasets <- function(n=1000, 
                                  max.likert.value=4, 
                                  fix.likert=TRUE, 
                                  nquestions=10,
                                  class.prop=c(0.5,0.5)) {
  IDS <- NULL

  for (question in 1:nquestions) {
    if(fix.likert){
      IDS <- cbind(IDS,
                   generate.likert.data( n,
                                         max.likert.value,
                                         most.freq.answer = sample(1:max.likert.value,1),
                                         flatness = sample(0:10,1)/10))
    } else {
      temp.max.likert <- sample(max.likert.value,1,replace = TRUE )
      IDS <- cbind(IDS,
                   generate.likert.data( n,
                                         temp.max.likert,
                                         most.freq.answer = sample(1:temp.max.likert,1),
                                         flatness = sample(0:10,1)/10))
    }
  }
  IDS <- cbind(IDS,Get_Class_Label(n,class.prop))
  return(IDS)
}


# Dataset RDS (dataset with relevant non correlated attributes)
# Params:
#                n - sample size (nrow)
# max.likert.value - as the name sais
#       fix.likert - if true all Questionairies have max likert
#                    if max likert will be sampled for each questionaierie
#       nquestions - number of questions (columns) to generate 
Generate_RDS_Dataset <- function(n=100,
                                 max.likert.value,
                                 nquestions,
                                 flatness,
                                 most.freq.answer,
                                 class.prop){

  
  
  #Generating Relevant part RIDS
  RDS <- NULL
  
    temp_RDS <- NULL
    for(i in 1:nquestions){
      temp1 <- generate.likert.data( (n*class.prop[1]),
                                     max.likert.value, 
                                     most.freq.answer[1],
                                     flatness[1])
      
      temp2 <- generate.likert.data( (n*class.prop[2]),
                                     max.likert.value, 
                                     most.freq.answer[2],
                                     flatness[2])
      temp_RDS <- cbind(temp_RDS,c(temp1, temp2))
    }
    
    temp_RDS <- cbind(temp_RDS, label=Get_Class_Label(n, class.prop))
  return(RDS)
}

Get_Class_Label <- function(n,prop) {
  if(sum(prop)!=1){
    stop("Sum of proportions is not equal to 1")
  }else {
    calculated.absolut.amount <- prop*n
    result <- NULL
    for(i in 1:length(calculated.absolut.amount)){
      result <- c(result, rep(i,calculated.absolut.amount[i]))
    }
    return(result)
  }
}



#@author Prof. Frank Klawonn 
generate.likert.data <- function(n,
                                 max.likert.value,
                                 most.freq.answer,
                                 flatness){
  # invert the flatness to 
  flatness <- abs(flatness-1)
  
  if (flatness==1){
    freqs <- rep(most.freq.answer, n)
    return(freqs)
  }
  
  freqs <- compute.likert.frequencies(max.likert.value=max.likert.value,most.freq.answer=most.freq.answer,flatness=flatness) 
  return(sample(max.likert.value,
                n,
                replace=T,
                prob=freqs))  
}

#@author Prof. Frank Klawonn 
compute.likert.frequencies <- function(max.likert.value,most.freq.answer,flatness){
  
  freqs <- rep(0,max.likert.value)
  alphabeta <- compute.alpha.beta(max.likert.value=max.likert.value,most.freq.answer=most.freq.answer,flatness=flatness)
  freqs[1] <- pbeta(1/max.likert.value,alphabeta[1],alphabeta[2])
  for (i in 2:max.likert.value){
    freqs[i] <- pbeta(i/max.likert.value,alphabeta[1],alphabeta[2]) - pbeta((i-1)/max.likert.value,alphabeta[1],alphabeta[2])
  }
  return(freqs)
}

#@author Prof. Frank Klawonn 
compute.alpha.beta <- function(max.likert.value,most.freq.answer,flatness){
  alpha <- 1
  beta <- 1
  if (flatness>0){
    consta <- flatness/(1-flatness)
    beta <- consta + 1 - consta*(most.freq.answer-0.5)/max.likert.value
    alpha <- consta + 2 - beta
  }
  return(c(alpha,beta))
}

Generate_Likert_datasets_4_Experiments <- function(n,max.likert.value,nquestions){
 

  
  # --- Make relevant irrelevant dataset
  # Fall1: Ballanced
  RIDS_BAL <- Generate_RIDS_Dataset(n,
                                    max.likert.value,
                                    nquestions_rel=10,
                                    nquestions_irel=10,
                                    class.prop = c(0.5, 0.5))
  

  
  # Fall2: Imballanced
  RIDS_IMBAL <- Generate_RIDS_Dataset(n,
                                      max.likert.value,
                                      nquestions_rel=10,
                                      nquestions_irel=10,
                                      class.prop = c(0.2, 0.8))

}

#------------------------------------#
#        Examples:                   #
#------------------------------------#
n <- 10000
likert.value <- 4 
nquestions <- 6

##################################
#  Generate a pure random dataset 
##################################

#########
# Case 1: All attributes have same max.likert, class label is balanced (BAL)
IDS_BAL <-  Generate_IDS_Datasets( n,
                               max.likert.value=likert.value,
                               fix.likert=TRUE,
                               nquestions,
                               class.prop=c(0.5,0.5))
print(IDS_BAL)
print(summary(IDS_BAL))
apply(IDS_BAL, 2, function(x){hist(x)})

#########
# Case 2: All attributes have randomized likert in range [1,max.likert] 
# class labels are imbalanced
IDS_IMBAL <-  Generate_IDS_Datasets( n,
                               max.likert.value=likert.value,
                               fix.likert=TRUE,
                               nquestions,
                               class.prop=c(0.2,0.8))
print(IDS_IMBAL)
print(summary(IDS_IMBAL))
apply(IDS_IMBAL, 2, function(x){hist(x)})

#########
# Case 3: All attributes have randomized likert in range [1,max.likert] 
# multiple classes and labels are imbalanced
IDS_IMBAL_mul <-  Generate_IDS_Datasets( n,
                               max.likert.value=likert.value,
                               fix.likert=TRUE,
                               nquestions,
                               class.prop=c(0.2,0.2,0.6))
print(IDS_IMBAL_mul)
print(summary(IDS_IMBAL_mul))
apply(IDS_IMBAL_mul, 2, function(x){hist(x)})


##################################
#  Generate a dataset with relevant attributes
#  ! Works only for binary class case
##################################

RDS <- Generate_RDS_Dataset (n=100,
                       max.likert.value=4,
                       nquestions=4,
                       flatness=c(0,1),
                       most.freq.answer=c(1,4),
                       class.prop=c(0.5,0.5))
