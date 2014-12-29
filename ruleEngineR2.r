rm(list=ls())
ruleEngine3<-function(clas,inp)
#clas is the label to be classified
#inp is the input dataset
#ruleEngine3 initiatizes the variables and look for new rules until the ruleset is perfect, ie,
#the rule covers all the positive cases if data allows
  {
#Initialization********************************
  X<-inp
  rule<-list()
  nfeatures<-ncol(inp)-1
  L<-as.numeric()
  flist<-list()
  start<-0
  names<-colnames(inp)
  #L each element of L is the number of levels for each feature
  for (i in 1:(ncol(inp)-1)){
    c<-levels(as.factor(inp[,i]))
    ind<-as.numeric(c)+start
    f<-list(ind)
    flist<-c(flist,f)
    l<-length(ind)
    L<-c(L,l)
    start<-start+l
  }
  Y<-inp
  wid<-ncol(inp)
  no.elementsData<-sum(inp[,wid]==clas)
  
  R0<-ruleEngine2(inp,clas,L,flist)
  
  myprint(R0,names,clas)
#filter out the positive cases covered by the first call
  for (i in 1:length(R0)){   
    fvalue<-R0[[i]][[2]]
    ind<-which(X[,R0[[i]][[1]]]==fvalue)
    X<-X[ind,]
    }
  
  
  cind<-ind 
  nout<-length(ind) 
   
  if (nout==no.elementsData){
    print ("The rule set is perfect")
    return()
  }
#Covered positive cases are removed from inp to be fed into ruleEngine again
#Store of copy of the new inp in Y
  X<-Y[!rownames(Y) %in% (rownames(X)),] 
  Y<-X
#continue to combine the positive cases for the second and subsequent calls
#compare the number of cases covered with the total number of cases in the dataset
  while(no.elementsData>nout){
    print('The rule set is not perfect yet')
   
#take away the examples that have already been covered 
    
    R0<-ruleEngine2(X,clas,L,flist)
    myprint(R0,names,clas)
    
#combine positive examples and loop back for another rule   
    for (i in 1:length(R0)){   
      fvalue<-R0[[i]][[2]]
      ind<-which(X[,R0[[i]][[1]]]==fvalue)
      X<-X[ind,]
    }
#ind is the index of the new positive examples       
#remove the new positive examples from inp
    
    X<-Y[!rownames(Y) %in% list(rownames(X)),]
    Y<-X
    
    cind<-c(cind,ind)
    nout<-length(cind)
    }
  print('The Rule Set is now perfect')
  print(' ')
  return()
  }


#ruleEngine2 adds features to the rule. It iterates over all features until it runs out of features 
#or if it runs out of positive cases to cover.
ruleEngine2<-function(X,class,L,flist)
{
#start up call to ruleEngine
  pflag<-0
  rule<-list()
  nfeatures<-ncol(X)-1
  
  engOut<-ruleEngine(X,class,L,flist)
  rule<-c(rule,list(engOut[[1]]))
  
  L[engOut[[1]][[1]]]<-0
  for (lc in 2:nfeatures){
   if (engOut[[length(engOut)]][[1]]==1|sum(L)==0) {return(rule)}
    
    
    fvalue<-engOut[[1]][[2]]
    
    ind<-which(X[,engOut[[1]][[1]]]==fvalue)
    X<-X[ind,]
    
    engOut<-ruleEngine(X,class,L,flist)
    rule<-c(rule,list(engOut[[1]]))  #combine the terms of the rule
    L[engOut[[1]][[1]]]<-0  #update L
    }
  return (rule)
}

#ruleEngine returns one feature term that covers the maximum number of positive cases. 
#the feature term is its position in the all feature vector.
#In the contact lens data set, the vector is made up of 9 terms, or feature value pairs.
#vector element 1 is age=1 or pair (1,1)
#vector element 9 is tear production rate=normal or pair (4,2)
#L tracks the features to be considered; L starts with c(3,2,2,2)
#sum of L should be the number of feature-value pair
#if also sets a flag to 1 if the p/t ratio is 1
#the engine stops if it reaches a p/t of 1
ruleEngine<-function(E,C,L,flist){ 
  LL<-L
  perflag<-0
  outlist<-list()
  v2<-E[,ncol(E)]==C        #vector of rows where outcome is C
  correctInst<-c(rep(-1,sum(LL)))
  n_att<-correctInst
  
  for (n in 1:length(L)) #iterate over all features
  {
    if(LL[n]>0){ 
    for (i in 1:L[n])    #iterate over all values of feature for each feature of E
    { 
      n_att[flist[[n]][[i]]]<-sum(E[,n]==i)
      v1<-E[,n]==i           
      ptratio<-sum(v1&v2)/n_att[flist[[n]][[i]]]
     correctInst[flist[[n]][[i]]]<-ptratio
      if(!is.na(ptratio)){
        if(ptratio==1)
        {perflag<-1}
      }
      
    }
    }
  }
  
  best.term<-(which(correctInst==max(correctInst,na.rm=T)))
  x<-n_att[best.term]
  best.term<-best.term[which(x==max(x,na.rm=T))]
  term<-getind(flist,best.term[1])
  r1<-list(term)
  pflag<-list(perflag)
  
  outlist<-c(r1,pflag)
  return(outlist)
}

#decodes the feature value pair from ruleEngine
getind<-function(list,n){
  i<-1
  while(i<=length(list)){
     pos<-which(list[[i]]==n)
     if (length(pos)!=0)
     {return(term<-c(i,pos))}
     i<-i+1}
    return(c(-1,-1))
  }






myprint<-function(R0,names,clas){
label<-c('hard','soft','none')  
f<-matrix(nrow=4,ncol=3)
f[1,]<-c('young','pre-pres','pres')
f[2,]<-c('myope','hypermetrope','nothing')
f[3,]<-c('no','yes','error')
f[4,]<-c('reduced','normal','error')
  P1<-sprintf("%s is %s if %s = %s",names[5],label[clas],names[R0[[1]][[1]]],f[R0[[1]][[1]],R0[[1]][[2]]])
  if(length(R0)==1){
    print(P1)
    return()}
  for (j in 2:length(R0)){
    P2<-sprintf(" and %s = %s",names[R0[[j]][[1]]],f[R0[[j]][[1]],R0[[j]][[2]]])
    P1<-paste(P1,P2,sep='')
    
  }
  print(P1,na.print='')
}



contactlens<-read.csv('contactLen.csv',header=T)

ruleEngine3(1,contactlens)
ruleEngine3(2,contactlens)
ruleEngine3(3,contactlens)
