# R Fuzzy Toolbox: v0.8: JMG: 02/07/09
# v0.3: JMG: 25/04/05: corrected lack of initialisation of y in smf and zmf
# v0.4: JMG: 24/02/06: added a simple 'showrule' function
# v0.5: JMG: 24/02/06: minor update to correct situations of fis with no rules
# v0.6: JMG: 23/06/09: minor update to remove unneeded escapes from string
# v0.7: JMG: 02/07/09: fixed problem caused by missing rule clauses in first rule-variable
# v0.8: JMG: 02/07/09: fixed minor issue with bisector defuzzification

# membership functions

dsigmf <- function(x, mfParams) {
  a1 <- mfParams[1]
  c1 <- mfParams[2]
  a2 <- mfParams[3]
  c2 <- mfParams[4]
  
  1 / (1 + exp(-a1*(x-c1))) - 1 / (1 + exp(-a2*(x-c2)))
}


gauss2mf <- function(x, mfParams) {
  sig1 <- mfParams[1]
  c1 <- mfParams[2]
  sig2 <- mfParams[3]
  c2 <- mfParams[4]
  
  c1idx= (x<=c1)
  c2idx= (x>=c2)
  
  (exp(-(x-c1)^2/(2*sig1^2))*c1idx + (1-c1idx)) * 
    (exp(-(x-c2)^2/(2*sig2^2))*c2idx + (1-c2idx))
}


gaussmf <- function(x, mfParams) {
  sig <- mfParams[1]
  c <- mfParams[2]
  
  exp(-(x - c)^2/(2 * sig^2))
}


gbellmf <- function(x, mfParams) {
  a <- mfParams[1]
  b <- mfParams[2]
  c <- mfParams[3]
  
  1 / ( 1 + (((x - c)/a)^2)^b)
}


pimf <- function(x, mfParams) {
  smf(x, mfParams[1:2]) * zmf(x, mfParams[3:4])
}


psigmf <- function(x, mfParams) {
  a1 <- mfParams[1]
  c1 <- mfParams[2]
  a2 <- mfParams[3]
  c2 <- mfParams[4]
  
  1 / (1 + exp(-a1*(x-c1))) / (1 + exp(-a2*(x-c2)))
}


sigmf <- function(x, mfParams) {
  a <- mfParams[1]
  c <- mfParams[2]
  
  1 / (1 + exp(-a*(x-c)))
}


smf <- function(x, mfParams) {
  x0 <- mfParams[1]
  x1 <- mfParams[2]
  y= rep(0, length(x))
  
  idx1= which(x <= x0)
  y[idx1]= 0
  
  idx2= which((x0 < x) & (x <= (x0+x1)/2))
  y[idx2] = 2*((x[idx2]-x0)/(x1-x0))^2
  
  idx3= which(((x0+x1)/2 < x) & (x <= x1))
  y[idx3] = 1-2*((x1-x[idx3])/(x1-x0))^2
  
  idx4= which(x1 <= x)
  y[idx4]= 1
  
  y
}


trapmf <- function(x, mfParams) {
  a <- mfParams[1]
  b <- mfParams[2]
  c <- mfParams[3]
  d <- mfParams[4]
  
  y= pmax(pmin( (x-a)/(b-a), 1, (d-x)/(d-c) ), 0)
  y[is.na(y)]= 1; y
}


trimf <- function(x, mfParams) {
  a <- mfParams[1]
  b <- mfParams[2]
  c <- mfParams[3]
  
  y= pmax(pmin( (x-a)/(b-a), (c-x)/(c-b) ), 0)
  y[is.na(y)]= 1; y
}


zmf <- function(x, mfParams) {
  x0 <- mfParams[1]
  x1 <- mfParams[2]
  y= rep(0, length(x))
  
  idx1= which(x <= x0)
  y[idx1]= 1
  
  idx2= which((x0 < x) & (x <= (x0+x1)/2))
  y[idx2] = 1-2*((x[idx2]-x0)/(x1-x0))^2
  
  idx3= which(((x0+x1)/2 < x) & (x <= x1))
  y[idx3] = 2*((x1-x[idx3])/(x1-x0))^2
  
  idx4= which(x1 <= x)
  y[idx4]= 0
  
  y
}


# FIS data structures

addmf <- function(fis, varType, varIndex, mfName, mfType, mfParams) {
  if ( varType == "input" ) {
    if ( varIndex <= length(fis$input) ) {
      fis$input[[varIndex]]$mf <- append(fis$input[[varIndex]]$mf,
                                         list(list(name=mfName, type=mfType, params=mfParams)))
    }
  }
  else {
    if ( varIndex <= length(fis$output) ) {
      fis$output[[varIndex]]$mf <- append(fis$output[[varIndex]]$mf,
                                          list(list(name=mfName, type=mfType, params=mfParams)))
    }
  }
  fis
}


addrule <- function(fis, ruleList) {
  fis$rule <- rbind(fis$rule, rbind(ruleList))
  rownames(fis$rule) <- NULL
  fis
}


addvar <- function(fis, varType, varName, varBounds) {
  if ( varType == "input" ) {
    fis$input <- append(fis$input,
                        list(list(name=varName,range=varBounds,mf=NULL)))
  }
  else {
    fis$output <- append(fis$output,
                         list(list(name=varName,range=varBounds,mf=NULL)))
  }
  fis
}


defuzz <- function(x, mf, type) {
  if ( type == "centroid" )
    sum(mf * x) / sum(mf)
  else if ( type == "bisector" ) {
    cs= cumsum(mf)
    a2= sum(mf)/2
    xs= match(TRUE, cs>a2)
    x[xs-1] + (a2-cs[xs-1])/mf[xs] + (x[xs]-x[xs-1])/2
  } else if ( type == "mom" )
    mean(x[which(mf == max(mf))])
  else if ( type == "som" )
    x[min(which(mf == max(mf)))]
  else if ( type == "lom" )
    x[max(which(mf == max(mf)))]
  else
    NA
}


evaltest <- function(x) {
  if ( !exists("GX") || !identical(GX, x) )
  {
    print("init needed")
    GX <<- x
  }
  
  print("func body")
}


evalfis <- function(input_stack, fis) {
  point_n= 101
  
  if ( !exists("GLOBAL_FIS") || !identical(fis, GLOBAL_FIS) ) {
    #print("initialising ...")
    GLOBAL_FIS    <<- fis
    FIS_TYPE      <<- fis$type
    IN_N          <<- length(fis$input)
    OUT_N         <<- length(fis$output)
    IN_MF_N       <<- NULL
    OUT_MF_N      <<- NULL
    for ( i in 1:IN_N )
      IN_MF_N[i]  <<- length(fis$input[[i]]$mf)
    for ( i in 1:OUT_N )
      OUT_MF_N[i] <<- length(fis$output[[i]]$mf)
    
    RULE_N        <<- nrow(fis$rule)
    RULE_LIST     <<- fis$rule[,1:(IN_N+OUT_N)]
    RULE_ANTE     <<- fis$rule[,1:IN_N]
    RULE_CONS     <<- fis$rule[,(IN_N+1):(IN_N+OUT_N)]
    RULE_WEIGHT   <<- fis$rule[,IN_N+OUT_N+1]
    AND_OR        <<- fis$rule[,IN_N+OUT_N+2]
    AND_METHOD    <<- fis$andMethod
    OR_METHOD     <<- fis$orMethod
    IMP_METHOD    <<- fis$impMethod
    AGG_METHOD    <<- fis$aggMethod
    DEFUZZ_METHOD <<- fis$defuzzMethod
    
    # get input params and types into globals
    IN_TYPE <<- NULL
    IN_PARAMS <<- list()
    
    idx= 1
    for ( i in 1:IN_N ) {
      for ( j in 1:IN_MF_N[i] ) {
        IN_TYPE[idx] <<- fis$input[[i]]$mf[[j]]$type
        IN_PARAMS[idx] <<- list(fis$input[[i]]$mf[[j]]$params)
        idx= idx + 1
      }
    }
    
    # compute OUT_TEMPLATE_MF: matrix (OUT_MF_N_TOTAL+1 X point_n)
    OUT_RANGE <<- matrix(0, OUT_N, 2)
    OUT_TEMPLATE_MF <<- matrix(0, sum(OUT_MF_N)+1, point_n)
    
    # dont care MF
    OUT_TEMPLATE_MF[1,] <<- 1 
    idx= 1
    for ( i in 1:OUT_N ) {
      for ( j in 1:OUT_MF_N[i] ) {
        OUT_RANGE[i,] <<- fis$output[[i]]$range
        OUT_TEMPLATE_MF[idx+1,] <<-  
          evalmf(seq(OUT_RANGE[i,1], OUT_RANGE[i,2], length=point_n),
                 fis$output[[i]]$mf[[j]]$params, fis$output[[i]]$mf[[j]]$type)
        idx= idx + 1
      }
    }
    
    # reorder to fill OUT_MF, an (RULE_N X point_n*OUT_N) matrix
    idx= abs(RULE_CONS)+matrix((0:(OUT_N-1))*RULE_N+1, RULE_N, OUT_N, byrow=TRUE)
    idx[RULE_CONS==0]= 1
    OUT_MF <<- OUT_TEMPLATE_MF[t(idx),]
    OUT_MF[t(RULE_CONS)<0,] <<- 1 - OUT_MF[t(RULE_CONS<0)]
    OUT_MF <<- matrix(t(OUT_MF), RULE_N, point_n*OUT_N, byrow=TRUE)
    
    # allocate other matrices
    QUALIFIED_OUT_MF <<- matrix(0, RULE_N, point_n*OUT_N)
    OVERALL_OUT_MF <<- matrix(0, point_n*OUT_N)
  }
  # end of initialisation
  
  # error checking for input stack
  if ( is.vector(input_stack) ) {
    input_stack= rbind(input_stack)
  }
  data_n= nrow(input_stack)
  
  # allocate output stack
  output_stack= matrix(0, data_n, OUT_N)
  
  # iteration through each row of input stack
  for ( kkk in 1:data_n ) {
    input= input_stack[kkk,]
    
    # get in_template_mf_value, a value for each MF
    in_template_mf_value= rep(0, sum(IN_MF_N))
    idx= 1
    for ( i in 1:IN_N ) {
      for ( j in 1:IN_MF_N[i] ) {
        in_template_mf_value[idx]=  
          evalmf(input[i], IN_PARAMS[[idx]], IN_TYPE[idx])
        idx= idx + 1
      }
    }
    # add a leading zero: (fixes problem with missing rules clauses in first rule)
    in_template_mf_value= c(0, in_template_mf_value)
    
    # reordering to get in_mf_value, a (RULE_N X IN_N) matrix 
    index= matrix(1, RULE_N, 1) %*% cumsum(c(0, IN_MF_N[1:(IN_N-1)])) + abs(RULE_ANTE) + 1
    in_mf_value= matrix(in_template_mf_value[index], RULE_N, IN_N)
    
    # replace dont-care MFs in AND rules with 1, and OR rules with 0
    in_mf_value[which(((AND_OR == 1) * (RULE_ANTE == 0)) == 1)]= 1
    in_mf_value[which(((AND_OR == 2) * (RULE_ANTE == 0)) == 1)]= 0
    
    # take care of NOTs (negative rule indices)
    idx= which(RULE_ANTE < 0)
    in_mf_value[idx]= 1 - in_mf_value[idx]
    
    # cat(in_mf_value, '\n')
    
    # find firing strengths of rules
    firing_strength= matrix(0, RULE_N, 1)
    if ( IN_N == 1 )
      firing_strength= in_mf_value
    else {
      and_index= which(AND_OR == 1)
      or_index= which(AND_OR == 2)
      firing_strength[and_index]=
        apply(rbind(in_mf_value[and_index,]), 1, AND_METHOD)
      firing_strength[or_index]=
        apply(rbind(in_mf_value[or_index,]), 1, OR_METHOD)
    }
    
    firing_strength= firing_strength * RULE_WEIGHT
    # cat(firing_strength, '\n')
    
    if ( FIS_TYPE == 'mamdani' )
    {
      # transform OUT_MF to OUT_QUALIFIED_MF
      tmp= matrix(firing_strength, nrow(firing_strength), point_n*OUT_N)
      if ( IMP_METHOD == 'prod' ) {
        QUALIFIED_OUT_MF <<- tmp * OUT_MF
      } else if ( IMP_METHOD == 'min' ) {
        QUALIFIED_OUT_MF <<- pmin(tmp, OUT_MF)
      } else {
        cat('user-defined implication not implemented yet\n')
      }
      
      # aggregation: 'sum', 'max', 'probor' or user-defined
      OVERALL_OUT_MF <<- apply(QUALIFIED_OUT_MF, 2, AGG_METHOD)
      
      # defuzzify each output
      for ( i in 1:OUT_N ) {
        output_stack[kkk, i]=
          defuzz(seq(OUT_RANGE[i, 1], OUT_RANGE[i, 2], length=point_n),
                 OVERALL_OUT_MF[((i-1)*point_n+1):(i*point_n)], DEFUZZ_METHOD)
      }
    }
    else if ( FIS_TYPE == 'sugeno' )
    {
      cat('sugeno inference not implemented yet\n')
    }
    else {
      cat('unknown inference type\n')
    }
  }
  
  output_stack
}


evalmf <- function(x, mfParams, mfType) {
  switch(mfType,
         dsigmf=dsigmf(x,mfParams),
         gauss2mf=gauss2mf(x,mfParams),
         gaussmf=gaussmf(x,mfParams),
         gbellmf=gbellmf(x,mfParams),
         pimf=pimf(x,mfParams),
         psigmf=psigmf(x,mfParams),
         smf=smf(x,mfParams),
         sigmf=sigmf(x,mfParams),
         trapmf=trapmf(x,mfParams),
         trimf=trimf(x,mfParams),
         zmf=zmf(x,mfParams))
}


gensurf <- function(fis, ix1=1, ix2=2, ox1=1) {
  i1= fis$input[[ix1]]
  i2= fis$input[[ix2]]
  o1= fis$output[[ox1]]
  
  x= seq(i1$range[1], i1$range[2], length=15)
  y= seq(i2$range[1], i2$range[2], length=15)
  m= meshgrid(x, y)
  
  o= evalfis(cbind(c(m$x), c(m$y)), fis)
  z= matrix(o[,ox1], 15, 15, byrow=TRUE)
  
  h= (z[-15,-15] + z[-1,-15] + z[-15,-1] + z[-1,-1]) / 4
  h= floor((h-min(h))/(max(h)-min(h))*14+.5)+1
  
  persp(x, y, z, 
        xlab=i1$name, ylab=i2$name, zlab=o1$name, 
        theta=-30, phi=30, col=rainbow(15)[16-h], ticktype='detailed')
}


newfis <- function(fisName,fisType="mamdani",
                   andMethod="min", orMethod="max", impMethod="min", aggMethod="max",
                   defuzzMethod="centroid") {
  fis <- list(name=fisName, type=fisType,
              andMethod=andMethod, orMethod=orMethod,
              impMethod=impMethod, aggMethod=aggMethod,
              defuzzMethod=defuzzMethod,
              input=NULL, output=NULL, rule=NULL)
}


plotmf <- function(fis, varType, varIndex) {
  if ( varType == 'input' ) {
    var <- fis$input[[varIndex]]
  }
  else {
    var <- fis$output[[varIndex]]
  }
  
  x= seq(var$range[1], var$range[2], length=101)
  y= c(rep(0, 100), 1)
  plot(x, y, type="n")
  
  for ( i in 1:length(var$mf) ) {
    y= evalmf(x, var$mf[[i]]$params, var$mf[[i]]$type)
    lines(x, y)
    text(x[match(TRUE, y==max(y))], 1.02, var$mf[[i]]$name)
  }
}


readfis <- function(fileName) {
  fileText <- readLines(fileName)
  if ( length(fileText) == 0 )
    stop('Zero length file!')
  fis <- list()
  line <- 1
  
  # structure parameters
  line= charmatch('[System]', fileText)
  if ( is.na(line) || line == 0 )
    stop(paste("No '[System]' line in file", fileName))
  line <- line + 1
  
  # defaults in case the user has omitted them
  Name <- 'untitled'
  Type <- 'mamdani'
  AndMethod <- 'min'
  OrMethod <- 'max'
  ImpMethod <- 'min'
  AggMethod <- 'max'
  DefuzzMethod <- 'centroid'
  
  # evaluate the values from the file
  while ( all(is.na(charmatch(c('[In', '[Out', '[Rules'), fileText[line]))) ) {
    eval(parse(text=fileText[line]))
    line <- line + 1
  }
  
  # create a FIS with the given structure
  fis <- list(name=Name, type=Type,
              andMethod=AndMethod, orMethod=OrMethod,
              impMethod=ImpMethod, aggMethod=AggMethod,
              defuzzMethod=DefuzzMethod,
              input=NULL, output=NULL, rule=NULL)
  
  # now begin with the inputs
  for ( varIndex in 1:NumInputs ) {
    while ( is.na(charmatch('[Input', fileText[line])) )
      line <- line + 1
    
    # name and range (needs processing)
    eval(parse(text=fileText[line+1]))
    rangeStr <- fileText[line+2]
    rangeSplit <- unlist(strsplit(rangeStr, "[][ ]"))
    rangeText <- paste(rangeSplit[1], 'c(', paste(rangeSplit[-1], collapse=','), ')')
    eval(parse(text=rangeText))
    
    # now add the variable to the FIS
    fis <- addvar(fis, 'input', Name, Range)
    
    # number of membership functions
    eval(parse(text=fileText[line+3]))
    line <- line + 4
    
    for ( MFIndex in 1:NumMFs) {
      mfStr <- fileText[line]
      #browser()
      mfSplit <- unlist(strsplit(mfStr, "[',]"))
      mfName <- mfSplit[2]
      mfType <- mfSplit[4]
      paramSplit <- unlist(strsplit(mfSplit[6], "[][ ]"))
      paramText <- paste('mfParams=', 'c(', paste(paramSplit[-1], collapse=','), ')')
      eval(parse(text=paramText))
      
      # now add the membership function to the FIS
      fis <- addmf(fis, 'input', varIndex, mfName, mfType, mfParams)
      line <- line + 1
    }    
  }
  
  # now for the outputs
  for ( varIndex in 1:NumOutputs ) {
    while ( is.na(charmatch('[Output', fileText[line])) )
      line <- line + 1
    
    # name and range (needs processing)
    eval(parse(text=fileText[line+1]))
    rangeStr <- fileText[line+2]
    rangeSplit <- unlist(strsplit(rangeStr, "[][ ]"))
    rangeText <- paste(rangeSplit[1], 'c(', paste(rangeSplit[-1], collapse=','), ')')
    eval(parse(text=rangeText))
    
    # now add the variable to the FIS
    fis <- addvar(fis, 'output', Name, Range)
    
    # number of membership functions
    eval(parse(text=fileText[line+3]))
    line <- line + 4
    
    for ( MFIndex in 1:NumMFs) {
      mfStr <- fileText[line]
      #browser()
      mfSplit <- unlist(strsplit(mfStr, "[',]"))
      mfName <- mfSplit[2]
      mfType <- mfSplit[4]
      paramSplit <- unlist(strsplit(mfSplit[6], "[][ ]"))
      paramText <- paste('mfParams= c(', paste(paramSplit[-1], collapse=','), ')')
      eval(parse(text=paramText))
      
      # now add the membership function to the FIS
      fis <- addmf(fis, 'output', varIndex, mfName, mfType, mfParams)
      line <- line + 1
    }    
  }
  
  # now for the rules
  while ( is.na(charmatch('[Rules]', fileText[line])) )
    line <- line + 1
  line <- line + 1
  
  for ( ruleIndex in 1:NumRules ) {
    ruleStr <- fileText[line]
    ruleSplit <- unlist(strsplit(ruleStr, "[ ,():]"))
    ruleSplit <- ruleSplit[nchar(ruleSplit) > 0]
    ruleText <- paste('ruleList= c(', paste(ruleSplit, collapse=','), ')')
    eval(parse(text=ruleText))
    
    # now add the rule to the FIS
    fis= addrule(fis, ruleList)
    line <- line + 1
  }
  
  fis
}


showfis <- function(fis) {
  NumInputs= length(fis$input)
  NumOutputs= length(fis$output)
  
  NumInputMFs= NULL
  for ( i in 1:NumInputs ) {
    NumInputMFs[i]= length(fis$input[[i]]$mf)
  }
  
  NumOutputMFs= NULL
  for ( i in 1:NumOutputs ) {
    NumOutputMFs[i]= length(fis$output[[i]]$mf)
  }
  
  NumRules= nrow(fis$rule)
  
  cat('1.  Name             ', fis$name, '\n')
  cat('2.  Type             ', fis$type, '\n')
  cat('3.  Inputs/Outputs   ', '[', NumInputs, NumOutputs, ']', '\n')
  cat('4.  NumInputMFs      ', '[', NumInputMFs, ']', '\n')
  cat('5.  NumOutputMFs     ', '[', NumOutputMFs, ']', '\n')
  cat('6.  NumRules         ', NumRules, '\n')
  cat('7.  AndMethod        ', fis$andMethod, '\n')
  cat('8.  OrMethod         ', fis$orMethod, '\n')
  cat('9.  ImpMethod        ', fis$impMethod, '\n')
  cat('10. AggMethod        ', fis$aggMethod, '\n')
  cat('11. DefuzzMethod     ', fis$defuzzMethod, '\n')
  frow= 11
  
  if ( NumInputs > 0 ) {
    for ( i in 1:NumInputs ) {
      cat(frow+i, '. InLabels          ', fis$input[[i]]$name, '\n', sep='')
    }
    frow= frow + i
  }
  
  if ( NumOutputs > 0 ) {
    for ( i in 1:NumOutputs ) {
      cat(frow+i, '. OutLabels         ', fis$output[[i]]$name, '\n', sep='')
    }
    frow= frow + i
  }
  
  if ( NumInputs > 0 ) {
    for ( i in 1:NumInputs ) {
      cat(frow+i, '. ', sep='');
      cat('InRange          ', '[', fis$input[[i]]$range, ']', '\n')
    }
    frow= frow + i
  }
  
  if ( NumOutputs > 0 ) {
    for ( i in 1:NumOutputs ) {
      cat(frow+i, '. ', sep='');
      cat('OutRange         ', '[', fis$output[[i]]$range, ']', '\n')
    }
    frow= frow + i
  }
  
  if ( NumInputs > 0 ) {
    for ( i in 1:NumInputs ) {
      for ( j in 1:NumInputMFs[i] ) {
        frow= frow + 1
        cat(frow, '. InMFLabels        ', fis$input[[i]]$mf[[j]]$name, '\n', sep='')
      }
    }
  }
  
  if ( NumOutputs > 0 ) {
    for ( i in 1:NumOutputs ) {
      for ( j in 1:NumOutputMFs[i] ) {
        frow= frow + 1
        cat(frow, '. OutMFLabels	      ', fis$output[[i]]$mf[[j]]$name, '\n', sep='')
      }
    }
  }
  
  if ( NumInputs > 0 ) {
    for ( i in 1:NumInputs ) {
      for ( j in 1:NumInputMFs[i] ) {
        frow= frow + 1
        cat(frow, '. InMFTypes         ', fis$input[[i]]$mf[[j]]$type, '\n', sep='')
      }
    }
  }
  
  if ( NumOutputs > 0 ) {
    for ( i in 1:NumOutputs ) {
      for ( j in 1:NumOutputMFs[i] ) {
        frow= frow + 1
        cat(frow, '. OutMFTypes 	      ', fis$output[[i]]$mf[[j]]$type, '\n', sep='')
      }
    }
  }
  
  if ( NumInputs > 0 ) {
    for ( i in 1:NumInputs ) {
      for ( j in 1:NumInputMFs[i] ) {
        frow= frow + 1
        cat(frow, '. ', sep=''); 
        cat('InMFParams       ', '[', fis$input[[i]]$mf[[j]]$params, ']', '\n')
      }
    }
  }
  
  if ( NumOutputs > 0 ) {
    for ( i in 1:NumOutputs ) {
      for ( j in 1:NumOutputMFs[i] ) {
        frow= frow + 1
        cat(frow, '. ', sep=''); 
        cat('OutMFParams      ', '[', fis$output[[i]]$mf[[j]]$params, ']', '\n')
      }
    }
  }
  
  if ( !is.null(NumRules) ) {
    for ( i in 1:NumRules ) {
      frow= frow + 1
      cat(frow, '. ', sep=''); 
      cat('Rule Antecedent   [', fis$rule[i,1:NumInputs], ']', '\n')
    }
    
    for ( i in 1:NumRules ) {
      frow= frow + 1
      cat(frow, '. ', sep=''); 
      cat('Rule Consequent  ', fis$rule[i,(NumInputs+1):(NumInputs+NumOutputs)], '\n')
    }
    
    for ( i in 1:NumRules ) {
      frow= frow + 1
      cat(frow, '. ', sep=''); 
      cat('Rule Weight      ', fis$rule[i,NumInputs+NumOutputs+1], '\n')
    }
    
    for ( i in 1:NumRules ) {
      frow= frow + 1
      cat(frow, '. ', sep=''); 
      cat('Rule Connection  ', fis$rule[i,NumInputs+NumOutputs+2], '\n')
    }
  }
}


showrule <- function(fis) {
  NumInputs= length(fis$input)
  NumOutputs= length(fis$output)
  NumRules= nrow(fis$rule)
  frow= 0
  
  if ( !is.null(NumRules) ) {
    for ( i in 1:NumRules ) {
      frow= frow + 1
      cat(frow, '. If ', sep='');
      for ( j in 1:NumInputs )
      {
        if ( fis$rule[i,j] != 0 )
        {
          cat('(', fis$input[[j]]$name, ' is ', sep='')
          if ( fis$rule[i,j] < 0 ) cat('not ', sep='')
          cat(fis$input[[j]]$mf[[abs(fis$rule[i,j])]]$name, ') ', sep='')
        }
        if ( j < NumInputs && fis$rule[i,j] != 0 && fis$rule[i,j+1] != 0 )
        {
          if ( fis$rule[i,NumInputs+NumOutputs+2] == 1 )
            cat('and ', sep='')
          else
            cat('or ', sep='')
        }
      }
      cat('then ', sep='')
      for ( j in 1:NumOutputs )
      {
        if ( fis$rule[i,NumInputs+j] != 0 )
        {
          cat('(', fis$output[[j]]$name, ' is ', sep='')
          if ( fis$rule[i,NumInputs+j] < 0 ) cat('not ', sep='')
          cat(fis$output[[j]]$mf[[abs(fis$rule[i,NumInputs+j])]]$name, ') ', sep='')
        }
      }
      cat('(', fis$rule[i,NumInputs+NumOutputs+1], ')\n', sep='')
    }
  }
}


writefis <- function(fis, fileName='fuzzy.fis') {
  fileText= NULL
  
  fileText[1]= "% R-Fuzzy (C) J.M.Garibaldi, 1st Oct 2004 $Revision: 0.1$"
  NumInputs= length(fis$input)
  NumOutputs= length(fis$output)
  
  NumInputMFs= NULL
  for ( i in 1:NumInputs ) {
    NumInputMFs[i]= length(fis$input[[i]]$mf)
  }
  
  NumOutputMFs= NULL
  for ( i in 1:NumOutputs ) {
    NumOutputMFs[i]= length(fis$output[[i]]$mf)
  }
  
  NumRules= nrow(fis$rule)
  
  fileText[2]= "[System]"
  fileText[3]= paste("Name='", fis$name, "'", sep="")
  fileText[4]= paste("Type='", fis$type, "'", sep="")
  fileText[5]= paste("NumInputs=", NumInputs, sep="")
  fileText[6]= paste("NumOutputs=", NumOutputs, sep="")
  fileText[7]= paste("NumRules=", NumRules, sep="")
  fileText[8]= paste("AndMethod='", fis$andMethod, "'", sep="")
  fileText[9]= paste("OrMethod='", fis$orMethod, "'", sep="")
  fileText[10]= paste("ImpMethod='", fis$impMethod, "'", sep="")
  fileText[11]= paste("AggMethod='", fis$aggMethod, "'", sep="")
  fileText[12]= paste("DefuzzMethod='", fis$defuzzMethod, "'", sep="")
  fileText[13]= ""
  line= 14
  
  for ( i in 1:NumInputs ) {
    fileText[line]= paste("[Input", i, "]", sep="")
    line= line + 1
    fileText[line]= paste("Name='", fis$input[[i]]$name, "'", sep="")
    line= line + 1
    fileText[line]= paste("Range=[", fis$input[[i]]$range[1], " ", fis$input[[i]]$range[2], "]", sep="")
    line= line + 1
    fileText[line]= paste("NumMFs=", NumInputMFs[i], sep="")
    line= line + 1
    
    for ( j in 1:NumInputMFs[i] ) {
      part1= paste("MF", j, "='", fis$input[[i]]$mf[[j]]$name, "':", sep="")
      part2= paste("'", fis$input[[i]]$mf[[j]]$type, "',", sep="") 
      part3= paste("[", paste(fis$input[[i]]$mf[[j]]$params, collapse=" "), "]", sep="") 
      fileText[line]= paste(part1, part2, part3, sep="")
      line= line + 1
    }
    
    fileText[line]= ""
    line= line + 1
  }
  
  for ( i in 1:NumOutputs ) {
    fileText[line]= paste("[Output", i, "]", sep="")
    line= line + 1
    fileText[line]= paste("Name='", fis$output[[i]]$name, "'", sep="")
    line= line + 1
    fileText[line]= paste("Range=[", fis$output[[i]]$range[1], " ", fis$output[[i]]$range[2], "]", sep="")
    line= line + 1
    fileText[line]= paste("NumMFs=", NumOutputMFs[i], sep="")
    line= line + 1
    
    for ( j in 1:NumOutputMFs[i] ) {
      part1= paste("MF", j, "='", fis$output[[i]]$mf[[j]]$name, "':", sep="")
      part2= paste("'", fis$output[[i]]$mf[[j]]$type, "',", sep="") 
      part3= paste("[", paste(fis$output[[i]]$mf[[j]]$params, collapse=" "), "]", sep="") 
      fileText[line]= paste(part1, part2, part3, sep="")
      line= line + 1
    }
    
    fileText[line]= ""
    line= line + 1
  }
  
  fileText[line]= "[Rules]"
  line= line + 1
  for ( i in 1:NumRules ) {
    part1= paste(fis$rule[i,1:NumInputs], collapse=" ")
    part2= paste(fis$rule[i,(NumInputs+1):(NumInputs+NumOutputs)], collapse=" ")
    part3= paste(" (", fis$rule[i,NumInputs+NumOutputs+1], ") : ", fis$rule[i,NumInputs+NumOutputs+2], sep="")
    fileText[line]= paste(part1, ", ", part2, part3, sep="")
    line= line + 1
  }
  
  fileText[line]= ""
  writeLines(fileText, fileName)
}


# miscelleny

probor <- function(...) {
  args <- list(...)
  if ( length(args) == 1 )
    xs <- args[[1]]
  else
    xs <- args
  
  result <- 0
  for ( x in xs )
    result <- result + x - result*x
  result
}

meshgrid <- function(a,b) {
  list(x=outer(b*0, a, FUN="+"), y=outer(b, a*0, FUN="+"))
}
