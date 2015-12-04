# PLEASE NOTE: There are some known problems with the visualisation of the inputs/outputs of fuzzy systems

# R Fuzzy Toolbox - Type-1 functions - Interval Type-2 and Non-Stationary specific functions
# are in separate file(s)

# R Fuzzy Toolbox: v1.0: JMG & DTH: 03/02/05
# v0.3: JMG: 25/04/05: corrected lack of initialisation of y in smf and zmf
# v0.4: JMG: 24/02/06: added a simple 'showrule' function
# v0.5: JMG: 24/02/06: minor update to correct situations of fis with no rules
# v1.0: DTH: 16/04/07: added interval type 2 and non-stationary FIS
# v1.5: JMG: 18/11/08: timestamped file to latest version
# v1.6: JMG: 01/09/10: added 'if (draw) ...' to evalfis defuzz routine
#                    : set default point_n to 101 to match old fuzzy.r (0.5)
# v1.7 SMM: 26/05/11: Stripped T1 down to base functionality for package release,
#                   : added comments for readability, added T1 mfs with variable
#                   : height, and equivalent IT2 mfs 

# Notes:
# SMM 28/05/12- Currently draw=TRUE argument in evalfis does not work properly, the resulting PDFs are incorrect

library(grid)

dir <- "./"

# SMM 27/05/11 - IT2(upper_mf,lower_mf)
# Combine two Type-1 sets to make an Interval Type-2 set
IT2 <- function(u,l) {
  
  #Concatenate upper and lower mfs
  rbind(c(u=u,l=l))
  
}

# SMM 26/05/11 - IT2gaussmf(uppermf_sigma,uppermf_centre,uppermf_height,lowermf_sigma,lowermf_centre,lowermf_height)
# Interval Type-2 Gaussian membership function
IT2gaussmf <- function(x, mfParams) {
  
  #Get details of upper mf
  sig_u <- mfParams[1]
  ctr_u <- mfParams[2]
  ht_u <- mfParams[3]
  
  #Get details of lower mf
  sig_l <- mfParams[4]
  ctr_l <- mfParams[5]
  ht_l <- mfParams[6]
  
  #Create y values for upper and lower mfs
  u <- (exp(-(x - ctr_u)^2/(2*sig_u^2)))*ht_u
  l <- (exp(-(x - ctr_l)^2/(2*sig_l^2)))*ht_l
  
  #Concatenate upper and lower mfs
  rbind(c(u=u,l=l))
  
}

# SMM 31/05/11 - IT2gauss2mf(upper_left_curve,upper_left_centre,upper_right_curve,upper_right_centre,upper_height,lower_left_curve,lower_left_centre,lower_right_curve,lower_right_centre,lower_height)
# Interval Type-2 Gaussian curve with differing left and right curves and adjustable height
IT2gauss2mf <- function(x, mfParams) {
  
  #Get upper mf details
  u_sig1 <- mfParam[1]
  u_c1 <- mfParams[2]
  u_sig2 <- mfParams[3]
  u_c2 <- mfParams[4]
  u_ht <- mfParams[5]
  
  #Get lower mf details
  l_sig1 <- mfParams[6]
  l_c1 <- mfParams[7]
  l_sig2 <- mfParams[8]
  l_c2 <- mfParams[9]
  l_ht <- mfParams[10]
  
  #Calculate y values for upper and lower mfs
  
  u_c1idx= (x<=u_c1)
  u_c2idx= (x>=u_c2)
  
  u <- ((exp(-(x-u_c1)^2/(2*u_sig1^2))*u_c1idx + (1-u_c1idx)) *
          (exp(-(x-u_c2)^2/(2*u_sig2^2))*u_c2idx + (1-u_c2idx))) * u_ht
  
  l_c1idx= (x<=l_c1)
  l_c2idx= (x>=l_c2)
  
  l <- ((exp(-(x-l_c1)^2/(2*l_sig1^2))*l_c1idx + (1-l_c1idx)) *
          (exp(-(x-l_c2)^2/(2*l_sig2^2))*l_c2idx + (1-l_c2idx))) * l_ht
  
  #Concatenate upper and lower mfs
  rbind(c(u=u,l=l))
}

# SMM 26/05/11 - IT2trapmf(x,c(upper_left_foot,upper_left_shoulder,upper__right_shoulder,upper_right_foot,upper_height,lower_left_foot,lower_left_shoulder,lower__right_shoulder,lower_right_foot,lower_height))
# Interval Type-2 Trapezoidal membership function
IT2trapmf <- function(x, mfParams) {
  
  #Get upper membership function details
  u_a <- mfParams[1]
  u_b <- mfParams[2]
  u_c <- mfParams[3]
  u_d <- mfParams[4]
  u_ht <- mfParams[5]
  
  #Get lower membership function details
  l_a <- mfParams[6]
  l_b <- mfParams[7]
  l_c <- mfParams[8]
  l_d <- mfParams[9]
  l_ht <- mfParams[10]
  
  #Create y values for upper and lower membership functions
  u = pmax(pmin( (x-u_a)/(u_b-u_a), u_ht, (u_d-x)/(u_d-u_c) ), 0)
  l = pmax(pmin( (x-l_a)/(l_b-l_a), l_ht, (l_d-x)/(l_d-l_c) ), 0)
  
  #If its NaN, make it ht
  u[is.na(u)]= u_ht
  l[is.na(l)]= l_ht
  
  #Concatenate upper and lower membership functions
  rbind(c(u=u,l=l))
  
}

# SMM 26/05/11 - IT2trimf_h(upper_left,upper_centre,upper_right,upper_height,lower_left,lower_centre,lower_right,lower_height)
# Interval Type-2 triangular membership function
IT2trimf <- function(x, mfParams) {
  
  #Get details of upper mf
  u_a <- mfParams[1]
  u_b <- mfParams[2]
  u_c <- mfParams[3]
  u_ht <- mfParams[4]
  
  #Get details of lower mf
  l_a <- mfParams[5]
  l_b <- mfParams[6]
  l_c <- mfParams[7]
  l_ht <- mfParams[8]
  
  #Create y values for mfs
  u = (pmax(pmin( (x-u_a)/(u_b-u_a), (u_c-x)/(u_c-u_b) ), 0))*u_ht
  l = (pmax(pmin( (x-l_a)/(l_b-l_a), (l_c-x)/(l_c-l_b) ), 0))*l_ht
  
  #Concatenate upper and lower membership functions
  rbind(c(u=u,l=l))
}

# SMM 27/05/11 - IT2dsigmf(upper_left_curve,upper_left_centre,upper_right_curve,upper_right_centre,upper_height,lower_left_curve,lower_left_centre,lower_right_curve,lower_right_centre,lower_height)
# Interval Type-2 difference between two sigmoids membership function
IT2dsigmf <- function(x, mfParams) {
  
  #Get details of upper mf
  u_a1 <- mfParams[1]
  u_c1 <- mfParams[2]
  u_a2 <- mfParams[3]
  u_c2 <- mfParams[4]
  u_ht <- mfParams[5]
  
  #Get details of lower mf
  l_a1 <- mfParams[6]
  l_c1 <- mfParams[7]
  l_a2 <- mfParams[8]
  l_c2 <- mfParams[9]
  l_ht <- mfParams[10]
  
  #Create y values for upper and lower mfs
  u <- u_ht / ( 1 + exp(-u_a1*(x-u_c1))) - u_ht / ( 1 + exp(-u_a2*(x-u_c2)))
  l <- l_ht / ( 1 + exp(-l_a1*(x-l_c1))) - l_ht / ( 1 + exp(-l_a2*(x-l_c2)))
  
  #Concatenate upper and lower mfs
  rbind(c(u=u,l=l))
}

# SMM 27/05/11 - IT2gbellmf(upper_width,upper_curve,upper_centre,upper_height,lower_width,lower_curve,lower_centre,lower_height)
# Interval Type-2 Generalised bell curve membership function with adjustable height
IT2gbellmf <- function(x, mfParams) {
  
  #Get details of upper membership function
  u_a <- mfParams[1]
  u_b <- mfParams[2]
  u_c <- mfParams[3]
  u_ht <- mfParams[4]
  
  #Get details of lower membership function
  l_a <- mfParams[5]
  l_b <- mfParams[6]
  l_c <- mfParams[7]
  l_ht <- mfParams[8]
  
  #Create y values for upper and lower mfs
  u <- u_ht / ( 1 + (((x - u_c)/u_a)^2)^u_b)
  l <- l_ht / ( 1 + (((x - l_c)/l_a)^2)^l_b)
  
  #Concatenate upper and lower mfs
  rbind(c(u=u,l=l))
}

# SMM 27/05/11 - IT2pimf(upper_s_start,upper_s_end,upper_z_start,upper_z_end,upper_height,lower_s_start,lower_s_end,lower_z_start,lower_z_end,lower_height)
# Interval Type-2 Pi shaped membership function with adjustable height
IT2pimf <- function(x, mfParams) {
  
  #Create upper and lower membership functions
  u <- smf_h(x, c(mfParams[1:2],mfParams[5])) * zmf_h(x, mfParams[3:5])
  l <- smf_h(x, c(mfParams[6:7],mfParams[10])) * zmf_h(x, mfParams[8:10])
  
  #Concatenate upper and lower mfs
  rbind(c(u=u,l=l))
  
}

# SMM 27/05/11 - IT2smf(upper_start,upper_end,upper_height,lower_start,lower_end,lower_height)
# Interval Type-2 S shaped membership function with adjustable height
IT2smf <- function(x, mfParams) {
  
  #Get details of upper membership function
  u_x0 <- mfParams[1]
  u_x1 <- mfParams[2]
  u_ht <- mfParams[3]
  u= rep(0, length(x))
  
  #Get details of lower membership function
  l_x0 <- mfParams[4]
  l_x1 <- mfParams[5]
  l_ht <- mfParams[6]
  l= rep(0, length(x))
  
  #Calculate upper y values
  u_idx1= which(x <= u_x0)
  u[u_idx1]= 0
  
  u_idx2= which((u_x0 < x) & (x <= (u_x0+u_x1)/2))
  u[u_idx2] = (2*((x[u_idx2]-u_x0)/(u_x1-u_x0))^2)*u_ht
  
  u_idx3= which(((u_x0+u_x1)/2 < x) & (x <= u_x1))
  u[u_idx3] = (1-2*((u_x1-x[u_idx3])/(u_x1-u_x0))^2)*u_ht
  
  u_idx4= which(u_x1 <= x)
  u[u_idx4]= u_ht
  
  #Calculate lower y values
  
  l_idx1= which(x <= l_x0)
  l[l_idx1]= 0
  
  l_idx2= which((l_x0 < x) & (x <= (l_x0+l_x1)/2))
  l[l_idx2] = (2*((x[l_idx2]-l_x0)/(l_x1-l_x0))^2)*l_ht
  
  l_idx3= which(((l_x0+l_x1)/2 < x) & (x <= l_x1))
  l[l_idx3] = (1-2*((l_x1-x[l_idx3])/(l_x1-l_x0))^2)*l_ht
  
  l_idx4= which(l_x1 <= x)
  l[l_idx4]= l_ht
  
  #Combine upper and lower membership functions
  rbind(c(u=u,l=l))
}

# SMM 31/05/11 IT2zmf(upper_start,upper_end,upper_height,lower_start,lower_end,lower_height)
#Interval Type-2 Z shaped membership function with adjustable height
IT2zmf <- function(x, mfParams) {
  
  #Get details of upper membership function
  u_x0 <- mfParams[1]
  u_x1 <- mfParams[2]
  u_ht <- mfParams[3]
  
  u= rep(0, length(x))
  
  #Get details of lower membership function
  l_x0 <- mfParams[4]
  l_x1 <- mfParams[5]
  l_ht <- mfParams[6]
  
  l= rep(0, length(x))
  
  #Calculate y values for upper membership function                  
  u_idx1= which(x <= u_x0)
  u[u_idx1]= u_ht
  
  u_idx2= which((u_x0 < x) & (x <= (u_x0+u_x1)/2))
  u[u_idx2] = (1-2*((x[u_idx2]-u_x0)/(u_x1-u_x0))^2)*u_ht
  
  u_idx3= which(((u_x0+u_x1)/2 < x) & (x <= u_x1))
  u[u_idx3] = (2*((u_x1-x[u_idx3])/(u_x1-u_x0))^2)*u_ht
  
  u_idx4= which(u_x1 <= x)
  u[u_idx4]= 0
  
  #Calculate y values for lower membership function                  
  l_idx1= which(x <= l_x0)
  l[l_idx1]= l_ht
  
  l_idx2= which((l_x0 < x) & (x <= (l_x0+l_x1)/2))
  l[l_idx2] = (1-2*((x[l_idx2]-l_x0)/(l_x1-l_x0))^2)*l_ht
  
  l_idx3= which(((l_x0+l_x1)/2 < x) & (x <= l_x1))
  l[l_idx3] = (2*((l_x1-x[l_idx3])/(l_x1-l_x0))^2)*l_ht
  
  l_idx4= which(l_x1 <= x)
  l[l_idx4]= 0
  
  #Combine upper and lower membership functions
  rbind(c(u=u,l=l))
}

# SMM 31/05/11 - IT2psigmf(upper_left_curve,upper_left_centre,upper_right_curve,upper_right_centre,upper_height,lower_left_curve,lower_left_centre,lower_right_curve,lower_right_centre,lower_height) 
# Interval Type-2 Product of 2 sigmoidal membership functions with adjustable height
IT2psigmf <- function(x, mfParams) {
  
  #Get details of upper mf
  u_a1 <- mfParams[1]
  u_c1 <- mfParams[2]
  u_a2 <- mfParams[3]
  u_c2 <- mfParams[4]
  u_ht <- mfParams[5]
  
  #Get details of lower mf
  l_a1 <- mfParams[6]
  l_c1 <- mfParams[7]
  l_a2 <- mfParams[8]
  l_c2 <- mfParams[9]
  l_ht <- mfParams[10]
  
  #Calculate y values for upper mf
  u <- u_ht / (1 + exp(-u_a1*(x-u_c1))) / (1 + exp(-u_a2*(x-u_c2)))
  
  #Calculate y values for lower mf
  l <- l_ht / (1 + exp(-l_a1*(x-l_c1))) / (1 + exp(-l_a2*(x-l_c2)))
  
  #Combine upper and lower membership functions
  rbind(c(u=u,l=l))
}

# SMM 31/05/11 - IT2sigmf(upper_curve,upper_centre,lower_curve,lower_centre)
# Interval Type-2 Sigmoidal membership function with adjustable height
IT2sigmf <- function(x, mfParams) {
  
  #Get details of upper mf
  u_a <- mfParams[1]
  u_c <- mfParams[2]
  u_ht <- mfParams[3]
  
  #Get details of lower mf
  l_a <- mfParams[4]
  l_c <- mfParams[5]
  l_ht <- mfParams[6]
  
  #Calculate y values for upper and lower mfs
  u <- u_ht / (1 + exp(-u_a*(x-u_c)))
  l <- l_ht / (1 + exp(-l_a*(x-l_c)))	
  
  #Combine upper and lower membership functions
  rbind(c(u=u,l=l))
  
}

# SMM 16/06/06 IT2combineT1s(x,upper_type,upper_params,lower_type,lower_params)
IT2combineT1s <- function(x, u_type,u_params,l_type,l_params) {
  
  #initialise upper and lower
  u= rep(0, length(x))
  l= rep(0, length(x))
  
  #create upper ys
  if (u_type == "trimf_h") {
    
    #create upper_ys
    u <- trimf(x,u_params)
  }
  
  #create lower ys
  if (l_type == "trimf_h") {
    l <- trimf_h(x,l_params)}
  
  #Combine upper and lower membership functions
  rbind(c(u=u,l=l))
  
}


# SMM 16/06/11 - addITmf - Add 2 T1 mfs to make an IT2 mf
# It's just the T1 version at the moment as I'm not sure about how it will work
# with the lists
addIT2mf <- function(fis, u_varType, l_varType, varIndex, mfName, mfUpper, mfLower) {
  if ( varType == "input" ) {
    if ( varIndex <= length(fis$input) ) {
      
      fis$input[[varIndex]]$mf <- append(fis$input[[varIndex]]$mf,
                                         list(list(name=mfName, type=mfType, params=mfParams, perturbation=NULL)))
      
    }
  }
  else {
    if ( varIndex <= length(fis$output) ) {
      
      fis$output[[varIndex]]$mf <- append(fis$output[[varIndex]]$mf,
                                          list(list(name=mfName, type=mfType, params=mfParams, perturbation=NULL)))
      
    }
  }
  fis
}

#############################################
# IT2 defuzzification - need to go through these

#CENTROID
#B: 2*M X point_n * OUT_N matrix
#y: 2*point_n X  OUT_N matrix
#outs : 2*M x OUT_N
trCentroid <- function(y,B){
  
  M <- nrow(B) %/% 2
  point_n <- nrow(y) %/% 2
  OUT_N <- ncol(y)
  out <- matrix(0, nrow(B),OUT_N)
  
  ff <- rotate2(B,OUT_N)
  out <- iterative_method(y,ff)
  
  out
}

#COS : matrix RULE_N*2 x OUT_N
#f : RULE_N*2 x 1(OUT_N)
#out: OUT_N*2 x 1
trCOS <- function(COS,f){
  tmp <- center_spread(COS)
  cc <- tmp$c
  s <- tmp$s
  
  tmp <- center_spread(f)
  h <- tmp$c
  OUT_N <- ncol(COS)
  delta <- tmp$s
  
  outs <-matrix(0,2,OUT_N)
  for (i in 1:OUT_N ){
    tmp<- interval_wtdavg(cc[,i],s[,i],h,delta)
    outs[1,i]<- tmp$r_out
    outs[2,i]<- tmp$l_out
  }
  
  outs
}

#y : M*2  x OUT_N
#f : M*2*N  x OUT_N
#out : 2*N  x OUT_N
#Used in centroid
iterative_method <- function(y,f){
  OUT_N <- ncol(y)
  N <- nrow(f) %/% nrow(y)
  M <- nrow(y) %/% 2
  
  tmp <- center_spread(f)
  h <- tmp$c #N*M x OUT_N
  delta <- tmp$s  #N*M x OUT_N
  
  tmp <- center_spread(y)
  c <- tmp$c
  s <- tmp$s #c,s : M x OUT_N
  
  out <- matrix(0,2*N,OUT_N)
  for (i in 1:OUT_N){
    for (j in 0:(N-1)){
      tmp <- interval_wtdavg(c[,i],s[,i],h[(1:M)+j*M,i],delta[(1:M)+j*M,i])
      out[j+1,i] <- tmp$r_out
      out[j+1+N,i] <- tmp$l_out
    }
  }
  out
}

#x : 2*N x M matrix
#$c,$s : N*M matrix
center_spread <- function(x){
  N = nrow(x) %/% 2
  M = ncol(x)
  cc <- matrix(0,N,M)
  s <- matrix(0,N,M)
  for (i in 1:N){
    cc[i,] = (x[i,]+x[i+N,])/2
    s[i,] = (x[i,]-cc[i,])
  }
  
  list(c=cc,s=s)
}

#c,s,h,delta are all vetors length M
interval_wtdavg <- function(c,s,h,delta){
  lower = h - delta ;
  upper = h + delta ;
  
  l_out = adapt(c-s,lower,upper,-1)$outextreme ;
  r_out = adapt(c+s,lower,upper,1)$outextreme ;
  list(l_out=l_out,r_out=r_out)
}

#OUT_MF : RULE_N*2 X point_n*OUT_N
#out : RULE_N*2*point_n X OUT_N
#used in Centroid
rotate2 <- function(OUT_MF,OUT_N){
  point_n <- ncol(OUT_MF) %/% OUT_N
  out<-matrix(0,nrow(OUT_MF)*point_n,OUT_N)
  for (i in 1:OUT_N){
    tmp <- OUT_MF[,(1:point_n)+(i-1)*point_n]
    #tmp : RULE_N*2 X point_n
    out[,i] <- t(tmp)
  }
  out
}

# Inputs : "ypoint", "lower" and "upper" are all M-dimensional vectors.
# "ypoint" contains the "y_l"s. "lower" and "upper"
# contain, respectively, the "w_lower" and "w_upper" values for each
# weight "w_l". If "maxflag > 0" (scalar), "S" is maximized, else
# it is minimized.
# Used in interval_wtdavg
adapt <- function(ypoint,lower,upper,maxflag){
  tmp <- sort(ypoint,index=TRUE)
  z <- tmp$x
  ix <- tmp$ix
  lower_sort <- lower[ix]
  upper_sort <- upper[ix]
  lz <- length(z)
  
  hl = (lower_sort+upper_sort)/2 ;
  S = sum(z*hl)/sum(hl) ;   # starting point
  
  eps = 1e-5 ;   # small quantity to avoid floating point equality problems
  
  count = 0 ;
  theta = hl ;
  S_new = S + 10*eps ;
  
  if ((abs(S-z[1]) < eps) | (abs(S-z[lz]) < eps)){
    outextreme = S ;
  } else {
    while (abs(S-S_new) > eps) {
      count = count + 1;
      
      if (count > 1)
        S = S_new ;
      
      in1 = which(z > (S-eps)) ;
      min1 = min(in1) ;
      
      if (min1 > 2){
        in2 = 1 : (min1-1) ;
      } else {
        in2 = 1;
      }
      
      if (maxflag > 0){
        theta[in1] = upper_sort[in1] ;
        theta[in2] = lower_sort[in2] ;
      } else {
        theta[in1] = lower_sort[in1] ;
        theta[in2] = upper_sort[in2] ;
        
        
        # To avoid division by zero if all lower_sort=0
        if (abs(S - z[min1]) < eps)
          theta[min1] = upper_sort[min1] ;
      } #end if maxflag
      
      
      S_new = sum(z*theta)/sum(theta) ;
      
    } #end while
    outextreme = S_new ;
  } # end if
  
  list(outextreme=outextreme,count=count,theta=theta)
}


#############################################
# testing
IT2tipper <- function() {
  
  fis= newfis('tipper',fType='it2')
  
  fis= addvar(fis, 'input', 'service', c(0, 10))
  fis= addvar(fis, 'input', 'food', c(0, 10))
  fis= addvar(fis, 'output', 'tip', c(0, 30))
  
  fis= addmf(fis, 'input', 1, 'poor', 'IT2gaussmf', c(2, 2, 1, 1, 2, 0.8))
  fis= addmf(fis, 'input', 1, 'good', 'IT2gaussmf', c(2, 5, 1, 1, 5, 0.8))
  fis= addmf(fis, 'input', 1, 'excellent', 'IT2gaussmf', c(2, 8, 1, 1, 8, 0.8))
  
  fis= addmf(fis, 'input', 2, 'rancid', 'IT2trapmf', c(0,0,3,5,1,0,0,2,4,0.8))
  fis= addmf(fis, 'input', 2, 'delicious', 'IT2trapmf', c(5,7,10,10,1,6,8,10,10,0.8))
  
  fis= addmf(fis, 'output', 1, 'cheap', 'IT2trimf', c(0, 5, 10, 1, 2, 5, 8, 0.8))
  fis= addmf(fis, 'output', 1, 'average', 'IT2trimf', c(10, 15, 20, 1, 12, 15, 18, 0.8))
  fis= addmf(fis, 'output', 1, 'generous', 'IT2trimf', c(20, 25, 30, 1, 22, 25, 28, 0.8))
  
  rl = rbind(c(1,1,1,1,2), c(2,0,2,1,1), c(3,2,3,1,2))
  fis= addrule(fis, rl)
  
  fis
}

########################################################################
# TYPE-1 membership functions

# SMM 26/05/11 - gaussmf(x,c(curve, centre, height)) - Gaussian curve membership function with adjustable height
gaussmf <- function(x, mfParams) {
  
  #Get mf details
  sig <- mfParams[1]
  ctr <- mfParams[2]
  ht <- mfParams[3]
  
  #Create mf y values
  (exp(-(x - ctr)^2/(2*sig^2)))*ht
  
}

# SMM 31/05/11 - gauss2mf(left_curve,left_centre,right_curve,right_curve,height) - Gaussian curve with differing left and right curves and adjustable height
gauss2mf <- function(x, mfParams) {
  
  #Get mf details
  sig1 <- mfParams[1]
  c1 <- mfParams[2]
  sig2 <- mfParams[3]
  c2 <- mfParams[4]
  ht <- mfParams[5]
  
  #Calculate y values for mf
  c1idx= (x<=c1)
  c2idx= (x>=c2)
  
  ((exp(-(x-c1)^2/(2*sig1^2))*c1idx + (1-c1idx)) *
    (exp(-(x-c2)^2/(2*sig2^2))*c2idx + (1-c2idx))) * ht
}

# SMM 26/05/11 - trapmf(x,c(left_foot,left_shoulder,_right_shoulder,right_foot,height)) - Trapezoidal membership function with adjustable height
trapmf <- function(x, mfParams) {
  
  #Get details of mf
  a <- mfParams[1]
  b <- mfParams[2]
  c <- mfParams[3]
  d <- mfParams[4]
  ht <- mfParams[5]
  
  #Create y values of membership function
  y= (pmax(pmin( (x-a)/(b-a), 1, (d-x)/(d-c) ), 0))*ht
  y[is.na(y)]= 1; y
}

# SMM 26/05/11 - trimf(left,centre,right,height) - Triangular membership function with adjustable height
trimf <- function(x, mfParams) {
  
  #Get details of mf
  a <- mfParams[1]
  b <- mfParams[2]
  c <- mfParams[3]
  ht <- mfParams[4]
  
  ##Create y values for mf
  y = (pmax(pmin( (x-a)/(b-a), (c-x)/(c-b) ), 0))*ht
  y[is.na(y)]= 1;	y
}

# SMM 27/05/11 - dsigmf(left_curve,left_centre,right_curve,right_centre,height) - difference between two sigmoidal membership functions with adjustable height
dsigmf <- function(x, mfParams) {
  
  #Get details of mf
  a1 <- mfParams[1]
  c1 <- mfParams[2]
  a2 <- mfParams[3]
  c2 <- mfParams[4]
  ht <- mfParams[5]
  
  #Create y values for mf
  ht / ( 1 + exp(-a1*(x-c1))) - ht / ( 1 + exp(-a2*(x-c2)))
}

# SMM 27/05/11 - gbellmf(width,curve,centre,height) - Generalised bell curve membership function with adjustable height
gbellmf <- function(x, mfParams) {
  
  #Get details of mf
  a <- mfParams[1]
  b <- mfParams[2]
  c <- mfParams[3]
  ht <- mfParams[4]
  
  #Create y values for mf
  ht / ( 1 + (((x - c)/a)^2)^b)
}

# SMM 27/05/11 - pimf(s_start,s_end,z_start,z_end,height) - Pi shaped membership function with adjustable height
pimf <- function(x, mfParams) {
  
  #Create a S and a Z to form a Pi
  smf(x, c(mfParams[1:2],mfParams[5])) * zmf(x, mfParams[3:5])
}

# SMM 27/05/11 - smf(start,end,height) S shaped membership function with adjustable height
smf <- function(x, mfParams) {
  x0 <- mfParams[1]
  x1 <- mfParams[2]
  ht <- mfParams[3]
  y= rep(0, length(x))
  
  idx1= which(x <= x0)
  y[idx1]= 0
  
  idx2= which((x0 < x) & (x <= (x0+x1)/2))
  y[idx2] = (2*((x[idx2]-x0)/(x1-x0))^2)*ht
  
  idx3= which(((x0+x1)/2 < x) & (x <= x1))
  y[idx3] = (1-2*((x1-x[idx3])/(x1-x0))^2)*ht
  
  idx4= which(x1 <= x)
  y[idx4]= ht
  
  y
}

# SMM 31/05/11 zmf(start,end,height) - Z shaped membership function with adjustable height
zmf <- function(x, mfParams) {
  
  #Get details of mf
  x0 <- mfParams[1]
  x1 <- mfParams[2]
  ht <- mfParams[3]
  y= rep(0, length(x))
  
  #Calculate y values for mf
  idx1= which(x <= x0)
  y[idx1]= ht
  
  idx2= which((x0 < x) & (x <= (x0+x1)/2))
  y[idx2] = (1-2*((x[idx2]-x0)/(x1-x0))^2)*ht
  
  idx3= which(((x0+x1)/2 < x) & (x <= x1))
  y[idx3] = (2*((x1-x[idx3])/(x1-x0))^2)*ht
  
  idx4= which(x1 <= x)
  y[idx4]= 0
  
  y
}

# SMM 31/05/11 - psigmf(left_curve,left_centre,right_curve,right_centre,height) - Product of 2 sigmoidal membership functions with adjustable height
psigmf <- function(x, mfParams) {
  
  #Get details of upper mf
  a1 <- mfParams[1]
  c1 <- mfParams[2]
  a2 <- mfParams[3]
  c2 <- mfParams[4]
  ht <- mfParams[5] 
  
  #Calculate y values for mf
  ht / (1 + exp(-a1*(x-c1))) / (1 + exp(-a2*(x-c2)))
}

# SMM 31/05/11 - sigmf(curve,centre) - Sigmoidal membership function with adjustable height
sigmf <- function(x, mfParams) {
  
  #Get details of mf
  a <- mfParams[1]
  c <- mfParams[2]
  ht <- mfParams[3]
  
  #Calculate y values for mf
  ht / (1 + exp(-a*(x-c)))
}

##############################################################################
# FILE I/O

# writefis - Save fis to a file
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
  fileText[4]= paste("FType='", fis$fType, "'", sep="")
  fileText[5]= paste("Type='", fis$type, "'", sep="")
  fileText[6]= paste("NumInputs=", NumInputs, sep="")
  fileText[7]= paste("NumOutputs=", NumOutputs, sep="")
  fileText[8]= paste("NumRules=", NumRules, sep="")
  fileText[9]= paste("AndMethod='", fis$andMethod, "'", sep="")
  fileText[10]= paste("OrMethod='", fis$orMethod, "'", sep="")
  fileText[11]= paste("ImpMethod='", fis$impMethod, "'", sep="")
  fileText[12]= paste("AggMethod='", fis$aggMethod, "'", sep="")
  fileText[13]= paste("DefuzzMethod='", fis$defuzzMethod, "'", sep="")
  fileText[14]= ""
  line= 15
  
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
      perturbation = fis$input[[i]]$mf[[j]]$perturbation
      part1= paste("MF", j, "='", fis$input[[i]]$mf[[j]]$name, "':", sep="")
      part2= paste("'", fis$input[[i]]$mf[[j]]$type, "',", sep="")
      part3= paste("[", paste(fis$input[[i]]$mf[[j]]$params, collapse=" "), "]", sep="")
      part4= paste(";  Perturbations : ")
      if (length(perturbation)>0){
        for (ii in 1:length(perturbation)){
          ind <- perturbation[[ii]]$ind
          type <- perturbation[[ii]]$type
          prms <- perturbation[[ii]]$params
          part4= paste(part4,"{ParamIndex :",ind,";PF:",type,";[",paste(prms, collapse=" "),"]}",sep="")
        }
      } else {
        part4 = paste(part4,"NULL")
      }
      fileText[line]= paste(part1, part2, part3,part4, sep="")
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
      perturbation = fis$output[[i]]$mf[[j]]$perturbation
      part1= paste("MF", j, "='", fis$output[[i]]$mf[[j]]$name, "':", sep="")
      part2= paste("'", fis$output[[i]]$mf[[j]]$type, "',", sep="")
      part3= paste("[", paste(fis$output[[i]]$mf[[j]]$params, collapse=" "), "]", sep="")
      part4= paste(";  Perturbations : ")
      if (length(perturbation)>0){
        for (ii in 1:length(perturbation)){
          ind <- perturbation[[ii]]$ind
          type <- perturbation[[ii]]$type
          prms <- perturbation[[ii]]$params
          part4= paste(part4,"{ParamIndex :",ind,";PF:",type,";[",paste(prms, collapse=" "),"]}",sep="")
        }
      } else {
        part4 = paste(part4,"NULL")
      }
      
      fileText[line]= paste(part1, part2, part3,part4, sep="")
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

# readfis - Read fis from a file
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
  FType <- 't1'
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
  fis <- list(name=Name, fType=FType,type=Type,
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
      mfSplit <- unlist(strsplit(mfStr, "[',]"))
      mfName <- mfSplit[2]
      mfType <- mfSplit[4]
      paramSplit <- unlist(strsplit(mfSplit[6], "; +Perturbations +: *"))
      params <- unlist(strsplit(paramSplit[1],"[][ ]"))
      mfParams <- as.numeric(params[-1])
      
      # now add the membership function to the FIS
      fis <- addmf(fis, 'input', varIndex, mfName, mfType, mfParams)
      
      pfParams <- unlist(strsplit(paramSplit[2],"[{]"))
      for (pfStr in pfParams[-1]){
        pfSplit <- unlist(strsplit(pfStr,"[:;]"))
        pfIndex <- as.numeric(pfSplit[2])
        pfType <- pfSplit[4]
        params <- unlist(strsplit(pfSplit[5],"[][ ]"))
        params <- params[params!="}"]
        params <- as.numeric(params[-1])
        fis <- addpf(fis,'input',varIndex,MFIndex,pfIndex,pfType,params)
      }
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
      mfSplit <- unlist(strsplit(mfStr, "[',]"))
      mfName <- mfSplit[2]
      mfType <- mfSplit[4]
      paramSplit <- unlist(strsplit(mfSplit[6], "; +Perturbations +: *"))
      params <- unlist(strsplit(paramSplit[1],"[][ ]"))
      mfParams <- as.numeric(params[-1])
      
      # now add the membership function to the FIS
      fis <- addmf(fis, 'output', varIndex, mfName, mfType, mfParams)
      pfParams <- unlist(strsplit(paramSplit[2],"[{]"))
      for (pfStr in pfParams[-1]){
        pfSplit <- unlist(strsplit(pfStr,"[:;]"))
        pfIndex <- as.numeric(pfSplit[2])
        pfType <- pfSplit[4]
        params <- unlist(strsplit(pfSplit[5],"[][ ]"))
        params <- params[params!="}"]
        params <- as.numeric(params[-1])
        fis <- addpf(fis,'output',varIndex,MFIndex,pfIndex,pfType,params)
      }
      
      line <- line + 1
    }
  }
  
  # now for the rules
  while ( is.na(charmatch('[Rules]', fileText[line])) )
    line <- line + 1
  line <- line + 1
  
  for ( ruleIndex in 1:NumRules ) {
    ruleStr <- fileText[line]
    ruleSplit <- unlist(strsplit(ruleStr, "[ ,\\(\\):]"))
    ruleSplit <- ruleSplit[nchar(ruleSplit) > 0]
    ruleText <- paste('ruleList= c(', paste(ruleSplit, collapse=','), ')')
    eval(parse(text=ruleText))
    
    # now add the rule to the FIS
    fis= addrule(fis, ruleList)
    line <- line + 1
  }
  
  fis
}

#############################################
# Plotting

# plotmf - Plot the memberships for a particular variable
plotmf <- function(fis, varType, varIndex,xx=NULL,timelimit=0,xlab=NULL,ylab=NULL,main=NULL) {
  point_n=501
  if ( varType == 'input' ) {
    var <- fis$input[[varIndex]]
  } else var <- fis$output[[varIndex]]
  
  x= seq(var$range[1], var$range[2], length=point_n)
  y= c(rep(0, point_n-1), 1)
  plot_graph(x,y,title=main,xlab=var$name,ylab="u")
  
  
  if (timelimit==0){
    for ( i in 1:length(var$mf) ) {
      y= evalmf(x, var$mf[[i]]$params, var$mf[[i]]$type)
      if (length(y)>= (2*length(x))){
        polygon(c(x,rev(x)), c(y[1:point_n],y[(2*point_n):(point_n+1)]),border=F,col=gray((i+2)/10))
      } else {
        lines(x,y,col=i,lwd=5)
      }
      
      if (length(xx)>0){
        for (j in 1:length(xx)){
          tmp <- evalmf(xx[j], var$mf[[i]]$params, var$mf[[i]]$type)
          lines(c(xx[j],xx[j],0),c(0,tmp[1],tmp[1]),col="red",lty="dashed");
          lines(c(xx[j],xx[j],0),c(0,tmp[2],tmp[2]),col="red",lty="dashed");
        }
      }
    }
    for ( i in 1:length(var$mf) ) {
      y= evalmf(x, var$mf[[i]]$params, var$mf[[i]]$type)
      tx= which.max(y)
      
      if ( tx >= point_n*0.95 ) {
        tx= x[point_n*0.95]
      }
      else
      {
        tx= x[tx + 20]
      }
      text(tx, 1.03-i/20, var$mf[[i]]$name, font=2, cex=.8)
    }
  } else if (timelimit>0){
    posx <-c()
    first <- TRUE
    
    for (t in 1:timelimit){
      for ( i in 1:length(var$mf) ) {
        repeat{
          params <- var$mf[[i]]$params
          e<-0
          perturbation <- var$mf[[i]]$perturbation
          if (length(perturbation)>0) {
            tmp <- perturb(t,params,perturbation)
            params <- tmp$params
            e <- tmp$e
          }
          y= evalmf(x, params, var$mf[[i]]$type)+e
          if (all(!is.nan(y))) break
        }
        y[y>1]=1
        y[y<0]=0
        if (length(y)>= (2*length(x))){
          polygon(c(x,rev(x)), c(y[1:point_n],y[(2*point_n):(point_n+1)]),border=F,col=gray((i+2)/10))
        } else {
          lines(x,y,col=gray((i*2)/10))
        }
        
        if (length(xx)>0){
          for (j in 1:length(xx)){
            tmp <- evalmf(xx[j], var$mf[[i]]$params, var$mf[[i]]$type)
            lines(c(xx[j],xx[j],0),c(0,tmp[1],tmp[1]),col="red",lty="dashed");
            lines(c(xx[j],xx[j],0),c(0,tmp[2],tmp[2]),col="red",lty="dashed");
          }#end of for
        }#end of if
        if (first) posx[i] <- x[match(TRUE, y==max(y))+10]
      }#end of for
      first = FALSE;
    }#end of for
    for ( i in 1:length(var$mf) ) {
      text(posx[i], 0.95, var$mf[[i]]$name,col="red",cex=.8)
    }
  }
  
  if (length(xx)>0)
    
    for (i in 1:length(xx)){
      mtext(paste(xx[i]),side=1,col="red",at=c(xx[i]),cex=.8);
      
    }
}

# plot_graph - used in plotmf
plot_graph <- function (x,y,xlab=NULL,ylab=NULL,title=NULL){
  plot(x, y, type="n",col=2,bty="o",xaxs = "i",yaxs = "i",cex.axis=.8,cex.lab=.8,main=title,xaxt="n",yaxt="n",ylab="",xlab="",las=1)
  
  point_n <- length(x)
  axis(1,labels=FALSE,tick=TRUE,pos=0)
  mtext(x[1],side=1,line=1,outer=FALSE,adj=0,cex=.8)
  if (length(xlab)==0){
    mtext("x",side=1,line=1,outer=FALSE,padj=0,cex=.8)
  } else mtext(xlab,side=1,line=1,outer=FALSE,padj=0,cex=.8)
  mtext(x[point_n],side=1,line=1,outer=FALSE,adj=1,cex=.8)
  
  axis(2,labels=FALSE,tick=TRUE,pos=0)
  mtext(y[1],side=2,line=1,outer=FALSE,at=c(0),cex=.8,las=1)
  if (length(ylab)==0){
    mtext("m",side=2,line=1,outer=FALSE,padj=0,cex=.8,las=1,font=5)
  } else mtext(ylab,side=2,line=1,outer=FALSE,padj=0,cex=.8,las=1,font=5)
  mtext(y[point_n],side=2,line=1,outer=FALSE,at=c(1),cex=.8,las=1)
  
}

# gensurf - Generate a 3D surface representing fis output
gensurf <- function(fis, ix1=1, ix2=2, ox1=1,timelimit=0,point_n=50,phi=30,theta=-30) {
  i1= fis$input[[ix1]]
  i2= fis$input[[ix2]]
  o1= fis$output[[ox1]]
  
  str <- paste(fis$name,fis$fType,timelimit,"z.txt",sep="")
  
  x= seq(i1$range[1], i1$range[2], length=point_n)
  y= seq(i2$range[1], i2$range[2], length=point_n)
  if (file.exists(str)){
    z=as.matrix(read.table(file=str ))
  } else {
    m= meshgrid(x, y)
    
    o= evalf(cbind(c(m$x), c(m$y)), fis,timelimit)
    z= matrix(o[,ox1], point_n, point_n, byrow=TRUE)
    write.table(z,file=str)
  }#end of if
  
  h= (z[-point_n,-point_n] + z[-1,-point_n] + z[-point_n,-1] + z[-1,-1]) / 4
  h= floor((h-min(h))/(max(h)-min(h))*(point_n-1)+.5)+1
  
  persp(x, y, z,
        xlab=i1$name, ylab=i2$name, zlab=o1$name,
        theta=theta, phi=phi, col=rainbow(point_n)[point_n+1-h] ,ticktype='detailed',main=fis$name)
}

#Used when draw==TRUE in evalfis
drawAnteEval<- function(fis,i,input,in_mf_value,point_n=101,title=NULL,fontsize=12,label=TRUE){
  NUM_INS          <- length(fis$input)
  NUM_OUTS         <- length(fis$output)
  NUM_RULES        <- nrow(fis$rule)
  LIST_RULES     <- fis$rule[i,1:(NUM_INS+NUM_OUTS)]
  ANT_RULES     <- fis$rule[i,1:NUM_INS]
  CON_RULES     <- fis$rule[i,(NUM_INS+1):(NUM_INS+NUM_OUTS)]
  WT_RULES   <- fis$rule[i,NUM_INS+NUM_OUTS+1]
  AND_OR        <- fis$rule[i,NUM_INS+NUM_OUTS+2]
  WT_RULES   <<- fis$rule[i,NUM_INS+NUM_OUTS+1]
  support <- 0
  
  num = length(ANT_RULES[ANT_RULES!=0])+1
  pushViewport(viewport(layout=grid.layout(2,1,heights=c(unit(0.1,"npc"),unit(0.85,"npc")) ,widths=c(1,1),default.units="npc" ) ))
  
  pushViewport(viewport(layout.pos.col = 1, layout.pos.row = 1))					
  if (length(title)==0) 
    title = paste("Evaluate antecedent of rule",i,":",showante(fis,i))
  grid.text(title)
  fs <- convertHeight(unit(1,"npc"),"points",valueOnly=TRUE)
  fontsize = min(fontsize,fs)
  popViewport()
  
  pushViewport(viewport(layout.pos.col = 1, layout.pos.row = 2))						
  w <- c(rep(1,num-1),4,2.5)	
  u <- c(rep("null",num-1),"lines","lines")
  pushViewport(viewport(layout=grid.layout(1,num+NUM_OUTS,widths=unit(w, u )  ) ))	
  j=0
  
  for (i in 1:NUM_INS){
    if (ANT_RULES[i]!=0){
      j=j+1
      if (num!=2){
        pushViewport(viewport(layout.pos.col = num, layout.pos.row = 1))
        pushViewport(viewport(width=0.5,x=1,just="right"))
        pushViewport(viewport(layout=grid.layout(2,1,heights=c(0.1,0.9),widths=c(1,1),default.units="npc" ) ))
        pushViewport(viewport(layout.pos.col = 1, layout.pos.row = 2))
        grid.move.to(y=in_mf_value[i],default.units = "native")
        popViewport()
        popViewport()
        popViewport()
        popViewport()
      }
      
      var <- fis$input[[i]]
      ind <- abs(ANT_RULES[i])
      s<-paste(var$name, 'is') 
      if ( ANT_RULES[i] < 0 ) s<- paste(s,'not')
      s<-paste(s,var$mf[[ind]]$name)
      x= seq(var$range[1], var$range[2], length=point_n)									
      y= evalmf(x, var$mf[[ind]]$params, var$mf[[i]]$type)
      y[y>1]=1
      y[y<0]=0
      
      if (ANT_RULES[i]<0) y=1-y
      pushViewport(viewport(layout.pos.col = j, layout.pos.row = 1))
      xTicks = FALSE
      yTicks = FALSE
      labelxx=TRUE
      labelyy=FALSE
      xlb = FALSE
      ylb = FALSE
      lb = label
      if (j==1) {
        lb = TRUE
        labelyy=TRUE
        ylb=TRUE
      }
      
      if (num==2){
        xTicks = TRUE
        yTicks = TRUE
        xlb = TRUE
        ylb = TRUE
      }
      
      plotData(x,y,xx=c(input[i]),yy=c(in_mf_value[i]),range=var$range,avoidx=var$range,point_n=point_n,main=s,
               fontsize=fontsize,label=lb,xTicks=xTicks,yTicks=yTicks,labelxx=labelxx,labelyy=labelyy,xlb=xlb,ylb=ylb)                
      #grid.move.to(x=input[i],y=in_mf_value[i],default.units = "native")
      #grid.line.to(x=var$range[2],y=in_mf_value[i],default.units="native",gp=gpar(col="red",lwd=2),arrow=arrow(ends="first",type="open",length=unit(0.25,"lines")))
      #pushViewport(viewport(layout=grid.layout(2,1,heights=c(0.1,0.9),widths=c(1,1),default.units="npc" ) ))			
      #pushViewport(viewport(layout.pos.col = 1, layout.pos.row = 2))		
      
      #popViewport()
      #popViewport()
      popViewport()					
    }
  }
  
  j=j+1
  fontsize=fontsize*0.75
  small <-convertHeight(unit(fontsize,"points"),"native",valueOnly=TRUE)
  smallW <- convertWidth(unit(fontsize,"points"),"native",valueOnly=TRUE)
  
  if (num!=2){
    pushViewport(viewport(layout.pos.col = j, layout.pos.row = 1))
    
    pushViewport(viewport(width=0.5,x=1,just="right"))
    
    pushViewport(viewport(layout=grid.layout(2,1,heights=c(0.1,0.9),widths=c(1,1),default.units="npc" ) ))
    pushViewport(viewport(layout.pos.col = 1, layout.pos.row = 2))
    gpar(fontsize=fontsize)
    grid.rect(gp = gpar(fill = gray(0.8)))
    
    if (AND_OR==1) {
      grid.text("AND",gp=gpar(fontsize=fontsize))
      grid.text(fis$andMethod,gp=gpar(fontsize=fontsize),y=unit(-0.75,"lines"))	
      support <- as.numeric(apply(rbind(in_mf_value[in_mf_value!=0]), 1,fis$andMethod))		
      
      
    } else {
      grid.text("OR",gp=gpar(fontsize=fontsize))	
      grid.text(fis$orMethod,gp=gpar(fontsize=fontsize),y=unit(-0.75,"lines"))
      support <- as.numeric(apply(rbind(in_mf_value[in_mf_value!=0]), 1,fis$orMethod))
    }
    support=support*WT_RULES
    
    
    
    tmp <- sort(round(in_mf_value*100)/100,decreasing=FALSE)
    for (i in 1:NUM_INS){
      if (tmp[i]!=0){		
        if (i==NUM_INS) 
          up <- max(1.6*small,1-tmp[i])
        else up <- tmp[i+1]-tmp[i]
        if (i==1)  
          down <- max(1.6*small,tmp[i])
        else down <-  tmp[i]-tmp[i-1]
        if (down==0) next
        val <- tmp[i]
        
        if (up>down && up>1.5*small){
          grid.text(val,y=val+0.5*small,gp=gpar(col="red",fontsize=fontsize),x=unit(-1,"lines"),default.unit="native",just=c("right","bottom"),check=TRUE)
        } else if (down>up && down>1.5*small){
          grid.text(val,y=val-1.2*small,gp=gpar(col="red",fontsize=fontsize),x=unit(-1,"lines"),default.unit="native",just=c("right","bottom"),check=TRUE)
        }			
      }
    }
    
    grid.move.to(y=support,x=1,default.units = "native")
    
    popViewport()
    popViewport()
    popViewport()
    popViewport()
  }
  
  j=j+1
  pushViewport(viewport(layout.pos.col = j, layout.pos.row = 1))
  
  pushViewport(viewport(width=0.45,x=1,just="right"))
  
  pushViewport(viewport(layout=grid.layout(2,1,heights=c(0.1,0.9),widths=c(1,1),default.units="npc" ) ))
  pushViewport(viewport(layout.pos.col = 1, layout.pos.row = 2))		
  grid.line.to(x=1,y=support,default.units="native",gp=gpar(col="red",lwd=2),arrow=arrow(ends="last",type="open",length=unit(0.25,"lines")))
  grid.text(x=unit(1,"npc")-unit(1,"lines"),y=support+0.5*small,label=paste(round(support*100)/100,sep=''),gp=gpar(fontsize=fontsize,col="red"),just=c("right","bottom"))
  popViewport()
  
  popViewport()
  popViewport()
  popViewport()
  
  popViewport()
  popViewport()
  popViewport()
  
}

#Used when draw==TRUE in evalfis
drawConsEval<- function(fis,i,support,point_n=101,OUT_RULE_CONS,title=NULL,label=TRUE,fontsize=12,yLabel=TRUE,xTicks=TRUE){
  NUM_INS          <- length(fis$input)
  NUM_OUTS         <- length(fis$output)
  NUM_RULES        <- nrow(fis$rule)
  LIST_RULES     <- fis$rule[i,1:(NUM_INS+NUM_OUTS)]
  ANT_RULES     <- fis$rule[i,1:NUM_INS]
  CON_RULES     <- fis$rule[i,(NUM_INS+1):(NUM_INS+NUM_OUTS)]
  WT_RULES   <- fis$rule[i,NUM_INS+NUM_OUTS+1]
  AND_OR        <- fis$rule[i,NUM_INS+NUM_OUTS+2]
  WT_RULES   <<- fis$rule[i,NUM_INS+NUM_OUTS+1]
  
  num = length(CON_RULES[CON_RULES!=0])
  #pushViewport(viewport(layout=grid.layout(2,1,heights=c(0.1,0.85),widths=c(1,1),default.units="npc" ) ))
  pushViewport(viewport(layout=grid.layout(2,1,heights=c(unit(0.1,"npc"),unit(0.85,"npc")),widths=c(1,1),default.units="npc" ) ))
  
  pushViewport(viewport(layout.pos.col = 1, layout.pos.row = 1))					
  if (length(title)==0) 
    title = paste("Evaluate consequent of rule",i,":",showcons(fis,i))
  grid.text(title,gp=gpar(fontsize=fontsize+1))
  fs <- convertHeight(unit(1,"npc"),"points",valueOnly=TRUE)
  fontsize = min(fontsize,fs)
  popViewport()
  
  pushViewport(viewport(layout.pos.col = 1, layout.pos.row = 2))						
  w <- rep(1,num)	
  u <- rep("null",num)
  pushViewport(viewport(layout=grid.layout(1,num,widths=unit(w, u )  ) ))	
  j=0
  
  for (i in 1:NUM_OUTS){
    if (CON_RULES[i]!=0){
      j=j+1
      
      pushViewport(viewport(layout.pos.col = j, layout.pos.row = 1))
      pushViewport(viewport(layout=grid.layout(2,1,heights=c(0.1,0.9),widths=c(1,1),default.units="npc" ) ))
      
      #pushViewport(viewport(layout.pos.col = 1, layout.pos.row = 1))
      #grid.rect(gp = gpar(fill = "grey"))
      var <- fis$output[[i]]
      ind <- abs(CON_RULES[i])
      s<-paste(var$name, 'is') 
      if ( CON_RULES[i] < 0 ) s<- paste(s,'not')
      #fs <- convertHeight(unit(0.75,"npc"),"points",valueOnly=TRUE)
      s<-paste(s,var$mf[[ind]]$name)
      #grid.text(paste(s,var$mf[[ind]]$name),gp=gpar(fontsize=min(fontsize,fs)),just="centre")
      popViewport()	
      
      pushViewport(viewport(layout.pos.col = 1, layout.pos.row = 1))
      #grid.rect()			
      x= seq(var$range[1], var$range[2], length=point_n)
      y= c(rep(0, point_n-1), 1)
      
      #if (label){
      #	grid.text(tolower(var$name),y = unit(-0.75, "lines"), gp = gpar(fontsize = fontsize))
      #	grid.yaxis(label=FALSE,main=TRUE)
      #	if (xTicks) grid.xaxis(label=FALSE,main=TRUE)								
      #}
      
      y= evalmf(x, var$mf[[ind]]$params, var$mf[[i]]$type)
      y[y>1]=1
      y[y<0]=0
      
      if (CON_RULES[i]<0) y=1-y
      #plot consequents of rules
      plotData(x,y,range=var$range,avoidx=var$range,point_n=point_n,main=s,
               fontsize=fontsize,label=label,xTicks=TRUE,yTicks=TRUE,labelxx=TRUE,labelyy=TRUE,xlb=TRUE,ylb=TRUE)
      
      
      
      #grid.lines(x,y,default.units = "native",gp=gpar(lty="dashed"))
      
      
      # if (j==1 && label && yLabel) {				
      # grid.text("m",x=unit(-0.5, "lines"),gp=gpar(fontsize=fontsize,font=5))
      # grid.text(label=0,gp=gpar(fontsize=fontsize),y=unit(0,"native"),x = unit(-1, "lines"))
      # grid.text(label=1,gp=gpar(fontsize=fontsize),y=unit(1,"native"),x = unit(-1, "lines"))				
      # }
      # if (num==1 && label){
      # tmm = -0.5
      # if (xTicks) 
      # tmm = -1
      
      # grid.text(label=x[1],gp=gpar(fontsize=fontsize),x=unit(x[1],"native"),y = unit(tmm, "lines"))
      # grid.text(label=x[point_n],gp=gpar(fontsize=fontsize),x=unit(x[point_n],"native"),y = unit(tmm, "lines"))
      
      # }
      y= OUT_RULE_CONS[(1:point_n)+(i-1)*point_n]	
      # Plot aggregated output
      plotData(x,y,range=var$range,avoidx=var$range,point_n=point_n,main=s,fill=TRUE,fillCol=gray((i + 2)/10),
               fontsize=fontsize,label=label,xTicks=FALSE,yTicks=FALSE,labelxx=TRUE,labelyy=TRUE,xlb=FALSE,ylb=TRUE)
      
      #grid.polygon(c(x,x[length(x)],x[1]),c(y,0,0),gp=gpar(fill=gray((i+2)/10)),default.units = "native")
      
      #if (fis$impMethod=="min"){
      #	grid.lines(x=c(0,1),y=c(support,support),default.units = "native",gp=gpar(lty="dashed",col="red"))
      #}
      
      #popViewport()
      popViewport()
      popViewport()
    }
  }###### end of for #########
  
  popViewport()
  popViewport()
  popViewport()
  
}

#Used when draw==TRUE in evalfis
drawRuleEval <- function(fis,i,input,support,in_mf_value,OUT_RULE_CONS,point_n=101,title=NULL,num_ante=-1,num_cons=-1,label=FALSE,fontsize=12){
  NUM_INS          <- length(fis$input)
  NUM_OUTS         <- length(fis$output)
  NUM_RULES        <- nrow(fis$rule)
  LIST_RULES     <- fis$rule[i,1:(NUM_INS+NUM_OUTS)]
  ANT_RULES     <- fis$rule[i,1:NUM_INS]
  CON_RULES     <- fis$rule[i,(NUM_INS+1):(NUM_INS+NUM_OUTS)]
  WT_RULES   <- fis$rule[i,NUM_INS+NUM_OUTS+1]
  AND_OR        <- fis$rule[i,NUM_INS+NUM_OUTS+2]
  WT_RULES   <<- fis$rule[i,NUM_INS+NUM_OUTS+1]
  
  if (num_ante==-1) num_ante = length(ANT_RULES[ANT_RULES!=0])
  if (num_cons==-1) num_cons = length(CON_RULES[CON_RULES!=0])
  
  if (length(title)==0) {
    grid.text(y=1,paste("Evaluate rule",i,":",showrule(fis,i)))
  } else grid.text(title)
  
  
  w <- c(rep(1,num_ante),6.5,rep(1,num_cons))	
  u <- c(rep("null",num_ante),"lines",rep("null",num_cons))	
  pushViewport(viewport(layout=grid.layout(1,num_ante+1+num_cons,widths=unit(w, u )  ) ))	
  pushViewport(viewport(layout.pos.col = 1:(num_ante+1), layout.pos.row = 1))						
  drawAnteEval(fis,i,input,in_mf_value,point_n=point_n,title="",fontsize=fontsize,label=label)
  popViewport()
  pushViewport(viewport(layout.pos.col = (1:num_cons)+(num_ante+1), layout.pos.row = 1))						
  drawConsEval(fis,i,OUT_RULE_CONS=OUT_RULE_CONS,support=support,point_n=point_n,title="",label=label,fontsize=fontsize,yLabel=FALSE,xTicks=FALSE)
  
  popViewport()
  popViewport()	
}

#Used when draw==TRUE in evalfis
drawRuleAggr <- function(fis,input,f_str,in_mf_value,OUT_RULE_CONS,point_n=101,title=NULL,fontsize=12){
  NUM_INS          <- length(fis$input)
  NUM_OUTS         <- length(fis$output)
  NUM_RULES        <- nrow(fis$rule)
  LIST_RULES     <- fis$rule[,1:(NUM_INS+NUM_OUTS)]
  ANT_RULES     <- as.matrix(fis$rule[,1:NUM_INS])
  CON_RULES     <- as.matrix(fis$rule[,(NUM_INS+1):(NUM_INS+NUM_OUTS)])
  WT_RULES   <- fis$rule[,NUM_INS+NUM_OUTS+1]
  
  num_ante=-1
  
  for (i in 1:NUM_RULES){
    rule = ANT_RULES[i,]
    num = length(rule[rule!=0])
    if (num_ante<num) num_ante = num
  }
  
  num_cons=-1
  
  for (i in 1:NUM_RULES){
    rule = CON_RULES[i,]
    num = length(rule[rule!=0])
    if (num_cons<num) num_cons = num
  }
  
  if (length(title)==0) {
    grid.text(y=1,"Rule aggregation")
  } else grid.text(title)
  
  pushViewport(viewport(layout=grid.layout(NUM_RULES,1,default.units="npc" )) )
  for (i in 1:NUM_RULES){						
    pushViewport(viewport(layout.pos.col = 1, layout.pos.row = i))
    drawRuleEval(fis,i,input,f_str[i,],in_mf_value[i,],OUT_RULE_CONS[i,],point_n=point_n,title="",num_ante=num_ante,num_cons=num_cons,label=FALSE,fontsize=fontsize)
    popViewport()
  }
  popViewport()	
}

#Used when draw==TRUE in evalfis
drawAllSteps <- function(fis,input,f_str,in_mf_value,OUT_RULE_CONS,OUT_RULE_AGG,output,point_n=101,title=NULL,label=TRUE,fontsize=12){
  NUM_INS          <- length(fis$input)
  NUM_OUTS         <- length(fis$output)
  NUM_RULES        <- nrow(fis$rule)
  NUM_RULES        <- nrow(fis$rule)
  ANT_RULES     <- as.matrix(fis$rule[,1:NUM_INS])
  CON_RULES     <- as.matrix(fis$rule[,(NUM_INS+1):(NUM_INS+NUM_OUTS)])
  WT_RULES   <- fis$rule[,NUM_INS+NUM_OUTS+1]
  AND_OR        <- fis$rule[,NUM_INS+NUM_OUTS+2]
  WT_RULES   <- fis$rule[,NUM_INS+NUM_OUTS+1]
  
  num_ante=-1
  
  for (i in 1:NUM_RULES){
    rule = ANT_RULES[i,]
    num = length(rule[rule!=0])
    if (num_ante<num) num_ante = num
  }
  
  num_cons=-1
  
  for (i in 1:NUM_RULES){
    rule = CON_RULES[i,]
    num = length(rule[rule!=0])
    if (num_cons<num) num_cons = num
  }
  
  h <- c(rep(1,NUM_RULES),2,1)	
  u <- c(rep("null",NUM_RULES),"lines","null") 	
  
  pushViewport(viewport(layout=grid.layout(NUM_RULES+2,1,heights=unit(h,u) )) )
  pushViewport(viewport(layout.pos.col = 1, layout.pos.row = 1:NUM_RULES))
  drawRuleAggr(fis,input,f_str,in_mf_value,OUT_RULE_CONS,point_n=point_n,title=title)	
  popViewport()	
  
  pushViewport(viewport(layout.pos.col = 1, layout.pos.row = NUM_RULES+1))
  
  grid.lines(c(0,1),c(0.5,0.5),gp=gpar(lwd=2,col="grey"))
  txt=""
  if (fis$defuzzMethod=="centroid")
    txt="Centroid of area"
  else if (fis$defuzzMethod=="bisector")
    txt="Bisector of area"
  else if (fis$defuzzMethod=="mom")
    txt="Mean value of maximum"
  else if (fis$defuzzMethod=="som")
    txt="Smallest (absolute) value of maximum"
  else txt="Largest (absolute) value of maximum"
  
  grid.text(x=0.5,y=0.25,label=paste("\"",txt,"\" defuzzification",sep=""),gp=gpar(fontsize=fontsize*0.75,fontface="bold",col=gray(0.7)))
  popViewport()			
  
  pushViewport(viewport(layout.pos.col = 1, layout.pos.row = NUM_RULES+2))
  
  w <- c(6.5,rep(1,num_ante+num_cons))	
  u <- c("lines",rep("null",num_ante+num_cons))	
  pushViewport(viewport(layout=grid.layout(1,(num_ante+1+num_cons),widths=unit(w, u )  ) ))		
  
  for (i in 1:NUM_OUTS){	
    
    pushViewport(viewport(layout.pos.col = num_ante+1+num_cons-NUM_OUTS+i, layout.pos.row = 1))			
    pushViewport(viewport(layout=grid.layout(2,1,heights=c(0.1,0.85),widths=c(1,1),default.units="npc" ) ))
    
    var <- fis$output[[i]]		
    s<-paste(var$name, 'Aggregated') 		
    x= seq(var$range[1], var$range[2], length=point_n)
    y= OUT_RULE_AGG[(1:point_n)+(i-1)*point_n]	
    
    pushViewport(viewport(layout.pos.col = 1, layout.pos.row = 1))
    fs <- convertHeight(unit(1,"npc"),"points",valueOnly=TRUE)
    fontsize = min(fontsize,fs)
    popViewport()	
    
    pushViewport(viewport(layout.pos.col = 1, layout.pos.row = 2))	
    
    ylb=FALSE
    if (i==1 && label){						
      ylb=TRUE				
    }		
    
    outputy <- defuzzY(x,y,fis$defuzzMethod)
    
    plotData(x,y,xx=c(output[i]),yy=c(outputy),range=var$range,avoidx=var$range,point_n=point_n,main=s,fill=TRUE,fillCol=gray((i + 2)/10),
             fontsize=fontsize,label=label,xTicks=FALSE,yTicks=FALSE,labelxx=TRUE,labelyy=TRUE,xlb=FALSE,ylb=ylb)
    
    grid.circle(output[i],outputy,r=0.05,gp=gpar(fill=gray(0.9),col=gray(0.9)))		
    
    popViewport()
    popViewport()
  }
  
  
  popViewport()
  popViewport()	
  popViewport()			
  popViewport()
}


# meshgrid - used in gensurf
meshgrid <- function(a,b) {
  list(x=outer(b*0, a, FUN="+"), y=outer(b, a*0, FUN="+"))
}

# evalf - used in gensurf
evalf <- function(inputs,fis,timelimit,draw=FALSE,point_n=101,xx=NULL,dir="NSFS-Figures/"){
  rangex <-c()
  op<-NULL
  if (draw){
    rangex <- seq(0,1,length=point_n)
    s<-inputs[1]
    for (i in 2:length(inputs))
      s<-paste(s,inputs[i],sep="")
    
    str<-paste(dir,"o",cnv(s),substring(fis$name,4),timelimit,".png",sep="")
    png(str,width=96*6,height=4.644*96,res=96)
    op <- par(pty="m",mar=c(2,4,0.4,2) +0.1 )
    plot_graph(rangex,rangex)
    
  }
  if (fis$fType == 'it2'){
    rs <- evalfis(inputs,fis,point_n=point_n,draw=draw)
    n <- nrow(rs) %/% 2;
    ind <- (1:n)*2-1;
    return(as.matrix(rs[ind,]))
  } else if (timelimit != 0 || fis$fType=='ns') {
    n <- length(inputs) %/% 2
    rs<- matrix(0,timelimit,n)
    len <- length(xx)
    values <- matrix(0,timelimit,len)
    for (i in 1:timelimit){
      rs[i,] <- evalfis(inputs,fis,i,point_n=point_n,draw=draw)
      if (len>0)
        for (j in 1:len){
          start <- fis$output[[1]]$range[1]
          step <- (fis$output[[1]]$range[2] -start)/(point_n-1)
          indx <- (xx[j]-start) %/% step
          values[i,j]<-OUT_RULE_AGG[1,indx]
        }
    }
    
    if (len>0){
      s<-inputs[1]
      for (i in 2:length(inputs))
        s<-paste(s,inputs[i],sep="")
      
      for (i in 1:len){
        str<-paste(dir,"o",cnv(s),"x",gsub("\\.","",paste(xx[i])),substring(fis$name,4),timelimit,".pdf",sep="")
        pdf(str,width=6,height=4.644)
        opa <- par(pty="m",mar=c(2.1,4.1,0,2.1) )
        histogram(values[,i])
        par(opa)
        dev.off()
      }
    }
    
    tmp <- matrix(0,n,1)
    
    for (i in 1:n)
      tmp[i,] <- mean(rs[,i],na.rm=TRUE)
    if (draw){
      par(op)
      dev.off()
    }
    return(tmp)
  } else if (fis$fType=='t1'){
    if (draw){
      par(op)
      dev.off()
    }
    
    return(evalfis(inputs,fis,point_n=point_n,draw=draw))
  }
  if (draw){
    par(op)
    dev.off()
  }
}

# showfis - Print out details of fis
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
  cat('2.  FType            ', fis$fType, '\n')
  cat('3.  Type             ', fis$type, '\n')
  cat('4.  Inputs/Outputs   ', '[', NumInputs, NumOutputs, ']', '\n')
  cat('5.  NumInputMFs      ', '[', NumInputMFs, ']', '\n')
  cat('6.  NumOutputMFs     ', '[', NumOutputMFs, ']', '\n')
  cat('7.  NumRules         ', NumRules, '\n')
  cat('8.  AndMethod        ', fis$andMethod, '\n')
  cat('9.  OrMethod         ', fis$orMethod, '\n')
  cat('10.  ImpMethod        ', fis$impMethod, '\n')
  cat('11. AggMethod        ', fis$aggMethod, '\n')
  cat('12. DefuzzMethod     ', fis$defuzzMethod, '\n')
  frow= 12
  
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
        
        perturbation <- fis$input[[i]]$mf[[j]]$perturbation
        part4= paste(";  Perturbations : ")
        if (length(perturbation)>0){
          for (ii in 1:length(perturbation)){
            ind <- perturbation[[ii]]$ind
            type <- perturbation[[ii]]$type
            prms <- perturbation[[ii]]$params
            part4= paste(part4,"{ParamIndex :",ind,";PF:",type,";[",paste(prms, collapse=" "),"]}",sep="")
          }
        } else {
          part4 = paste(part4,"NULL")
        }
        
        cat('InMFParams       ', '[', fis$input[[i]]$mf[[j]]$params, ']', part4,'\n')
      }
    }
  }
  
  if ( NumOutputs > 0 ) {
    for ( i in 1:NumOutputs ) {
      for ( j in 1:NumOutputMFs[i] ) {
        frow= frow + 1
        cat(frow, '. ', sep='');
        
        perturbation <- fis$output[[i]]$mf[[j]]$perturbation
        part4= paste(";  Perturbations : ")
        if (length(perturbation)>0){
          for (ii in 1:length(perturbation)){
            ind <- perturbation[[ii]]$ind
            type <- perturbation[[ii]]$type
            prms <- perturbation[[ii]]$params
            part4= paste(part4,"{ParamIndex :",ind,";PF:",type,";[",paste(prms, collapse=" "),"]}",sep="")
          }
        } else {
          part4 = paste(part4,"NULL")
        }
        
        cat('OutMFParams      ', '[', fis$output[[i]]$mf[[j]]$params, ']', part4,'\n')
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

# showrules - Print out rules for a fis
# SMM - 26/05/11 - I've changed this from 'showrule' to differentiate
# between showing one rule and showing them all
# function overrides are not allowed in R, though it could
# be rewritten to work properly for both one and all rules

showrules <- function(fis) {
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

# showrule - Print out a particular rule of a fis
showrule <- function(fis,i) {
  NumInputs= length(fis$input)
  NumOutputs= length(fis$output)
  NumRules= nrow(fis$rule)
  
  s <- ""
  if ( !is.null(NumRules) ) {
    s <- paste('IF ', sep='');
    for ( j in 1:NumInputs )
    {
      var <- fis$input[[j]]
      if ( fis$rule[i,j] != 0 )
      {
        s<-paste(s, var$name, 'is')
        if ( fis$rule[i,j] < 0 ) s<- paste(s,'not',)
        s<-paste(s,var$mf[[abs(fis$rule[i,j])]]$name)
      }
      if ( j < NumInputs && fis$rule[i,j] != 0 && fis$rule[i,j+1] != 0 )
      {
        if ( fis$rule[i,NumInputs+NumOutputs+2] == 1 )
          s<-paste(s,'and')
        else
          s<-paste(s,'or')
      }
    }
    s<-paste(s,'then')
    for ( j in 1:NumOutputs )
    {
      var <- fis$output[[j]]
      if ( fis$rule[i,NumInputs+j] != 0 )
      {
        s<-paste(s, var$name, 'is')
        if ( fis$rule[i,NumInputs+j] < 0 ) s<-paste(s,'not')
        s<-paste(s,var$mf[[abs(fis$rule[i,NumInputs+j])]]$name)
      }
    }
    s<-paste(s,' ( weight = ', fis$rule[i,NumInputs+NumOutputs+1], ' )', sep='')
  }
  
  s
}

####################################################
# FIS creation

# newfis - Create a new fis
newfis <- function(fisName,fType="t1",fisType="mamdani",
                   andMethod="min", orMethod="max", impMethod="min", aggMethod="max",
                   defuzzMethod="centroid") {
  fis <- list(name=fisName, fType=fType,type=fisType,
              andMethod=andMethod, orMethod=orMethod,
              impMethod=impMethod, aggMethod=aggMethod,
              defuzzMethod=defuzzMethod,
              input=NULL, output=NULL, rule=NULL)
}

# addmf - Add a membership function to a fis
addmf <- function(fis, varType, varIndex, mfName, mfType, mfParams) {
  if ( varType == "input" ) {
    if ( varIndex <= length(fis$input) ) {
      fis$input[[varIndex]]$mf <- append(fis$input[[varIndex]]$mf,
                                         list(list(name=mfName, type=mfType, params=mfParams, perturbation=NULL)))
    }
  }
  else {
    if ( varIndex <= length(fis$output) ) {
      fis$output[[varIndex]]$mf <- append(fis$output[[varIndex]]$mf,
                                          list(list(name=mfName, type=mfType, params=mfParams, perturbation=NULL)))
    }
  }
  fis
}

# addrule - add a rule/list of rules to a fis
addrule <- function(fis, ruleList) {
  fis$rule <- rbind(fis$rule, rbind(ruleList))
  
  rownames(fis$rule) <- NULL
  fis
}

# addvar - Add a variable to a fis
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

# evalmf - Evaluate a membership function
evalmf <- function(x, mfParams, mfType,mfName) {
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
         zmf=zmf(x,mfParams),
         gausstype2mf=gausstype2mf(x,mfParams),
         gsf2=gsf2(x,mfParams),
         usermf=usermf(x,mfParams),
         gausst2mf=gausst2mf(x,mfParams),
         trapmf2=trapmf2(x,mfParams),
         trimf2=trimf2(x,mfParams),
         idmf = idmf(x,mfParams),
         sinpf=sinpf(x,mfParams,mfName),
         prandpf=prandpf(x,mfParams,mfName),
         randpf=randpf(x,mfParams,mfName),
         tri2mf=tri2mf(x,mfParams),
         gaussmf2=gaussmf2(x,mfParams),
         trapmf2_SM=trapmf2_SM(x,mfParams),
         trimf_h=trimf_h(x,mfParams),
         trapmf_h=trapmf_h(x,mfParams),
         gaussmf_h=gaussmf_h(x,mfParams),
         gauss2mf_h=gauss2mf_h(x,mfParams),
         dsigmf_h=dsigmf_h(x,mfParams),
         gbellmf_h=gbellmf_h(x,mfParams),
         smf_h=smf_h(x,mfParams),
         zmf_h=zmf_h(x,mfParams),
         pimf_h=pimf_h(x,mfParams),
         psigmf_h=psigmf_h(x,mfParams),
         sigmf_h=sigmf_h(x,mfParams),
         IT2gaussmf=IT2gaussmf(x,mfParams),
         IT2trapmf=IT2trapmf(x,mfParams),
         IT2trimf=IT2trimf(x,mfParams),
         IT2dsigmf=IT2dsigmf(x,mfParams),
         IT2gbellmf=IT2gbellmf(x,mfParams),
         IT2pimf=IT2pimf(x,mfParams),
         IT2smf=IT2smf(x,mfParams),
         IT2zmf=IT2zmf(x,mfParams),
         IT2psigmf=IT2psigmf(x,mfParams),
         IT2sigmf=IT2sigmf(x,mfParams),
         IT2gauss2mf=IT2gauss2mf(x,mfParams))
  #IT2combineT1s=IT2combineT1s(x, u_type,u_params,l_type,l_params))
}

# evalfis - evaluate input using fis
evalfis <- function(in_stack, fis,time=1,point_n=101,draw=FALSE) {
  first <- FALSE
  
  #Check the make sure there isn't already a global fis, or that if there is it's
  #not identical to the one being passed before initialising
  if ( !exists("GLOBAL_FIS") || !identical(fis, GLOBAL_FIS)) {
    first <- TRUE
    
    #Get details of fis and create matrices
    GLOBAL_FIS    <<- fis
    FIS_TYPE      <<- fis$type
    NUM_INS          <<- length(fis$input)
    NUM_OUTS         <<- length(fis$output)
    NUM_IN_MFS       <<- NULL
    NUM_OUT_MFS      <<- NULL
    for ( i in 1:NUM_INS )
      NUM_IN_MFS[i]  <<- length(fis$input[[i]]$mf)
    for ( i in 1:NUM_OUTS )
      NUM_OUT_MFS[i] <<- length(fis$output[[i]]$mf)
    
    NUM_RULES        <<- nrow(fis$rule)
    LIST_RULES     <<- fis$rule[,1:(NUM_INS+NUM_OUTS)]
    ANT_RULES     <<- fis$rule[,1:NUM_INS]
    CON_RULES     <<- fis$rule[,(NUM_INS+1):(NUM_INS+NUM_OUTS)]
    WT_RULES   <<- fis$rule[,NUM_INS+NUM_OUTS+1]
    AND_OR        <<- fis$rule[,NUM_INS+NUM_OUTS+2]
    NUM_TOTAL_OUT_MFS <<- sum(NUM_OUT_MFS)
    NUM_TOTAL_IN_MFS <<- sum(NUM_IN_MFS)
    
    # Get input params and types into globals
    IN_TYPE <<- NULL
    IN_PARAMS <<- list()
    IN_PERTURBATION <<-list()
    OUT_PERTURBATION <<-list()
    
    # Check to see with type of FL is being used
    FTYPE <<- 1
    if (fis$fType=='it2') {
      FTYPE <<- 2
      OUT_COUPLE <<- c(0,NUM_TOTAL_OUT_MFS)
      COUPLE <<- c(0,NUM_RULES)
      IN_COUPLE <<- c(0,NUM_TOTAL_IN_MFS)
    } else {
      OUT_COUPLE <<- 0
      COUPLE <<- 0
      IN_COUPLE <<- 0
    }
    
    #Get the type, parameters and perturbation functions for each INPUT mf
    idx= 1
    for ( i in 1:NUM_INS ) {
      for ( j in 1:NUM_IN_MFS[i] ) {
        IN_TYPE[idx] <<- fis$input[[i]]$mf[[j]]$type
        IN_PARAMS[idx] <<- list(fis$input[[i]]$mf[[j]]$params)
        IN_PERTURBATION[idx] <<-list(fis$input[[i]]$mf[[j]]$perturbation)
        idx= idx + 1
      }
    }
    
    #Get perturbation function for OUTPUT mfs
    no_output_perturbation<<-TRUE
    idx =1
    for ( i in 1:NUM_OUTS ) {
      for ( j in 1:NUM_OUT_MFS[i] ) {
        perturbation <- fis$output[[i]]$mf[[j]]$perturbation
        
        #if this mf has a perturbation function set flag to FALSE
        if (length(perturbation)>0) {
          no_output_perturbation <<- FALSE
        }
        OUT_PERTURBATION[idx] <<- list(perturbation)
        idx =idx+1
      }
    }
    
    #Get range of each OUTPUT mf, and create a sequence over that range
    OUT_RANGE <<- matrix(0, NUM_OUTS, 2)
    rangex <<- matrix(0,point_n,NUM_OUTS)
    for ( i in 1:NUM_OUTS ) {        
      OUT_RANGE[i,] <<- fis$output[[i]]$range
      rangex[,i] <<- seq(OUT_RANGE[i,1], OUT_RANGE[i,2], length=point_n)
    }
    
    # allocate matrices for rule consequents and aggregated consequents
    OUT_RULE_CONS <<- matrix(0, NUM_RULES*FTYPE, point_n*NUM_OUTS)
    OUT_RULE_AGG <<- matrix(0, FTYPE,point_n*NUM_OUTS)
    
  }
  
  first<-TRUE
  if (first || !exists("no_output_perturbation") || !no_output_perturbation) {
    
    OUT_TEMP_MF <<- matrix(0, (NUM_TOTAL_OUT_MFS)*FTYPE+FTYPE, point_n)
    
    OUT_TEMP_MF[OUT_COUPLE+1,] <<- 1
    idx= 1
    
    for ( i in 1:NUM_OUTS ) {
      
      #if draw is set plot each MF in this output variable and produce a PDF
      str<-paste(dir,substring(fis$name,length(fis$name)),"output",i,".pdf",sep="")
      if (draw && time==1 && !file.exists(str)){
        pdf(str)
        pushViewport(plotViewport())
        plotMF(fis,"output",i,xlab=tolower(fis$output[[i]]$name),main=fis$output[[i]]$name)
        popViewport();
        dev.off()
      }
      
      for ( j in 1:NUM_OUT_MFS[i] ) {
        
        #Get the parameters, type and perturbation function for each mf in this output var
        params <- fis$output[[i]]$mf[[j]]$params
        type <- fis$output[[i]]$mf[[j]]$type
        perturbation <- OUT_PERTURBATION[[idx]]
        
        #If there are any perturbation functions, perturb the membership function
        if (length(perturbation)>0) {
          count<-0
          repeat{
            tmp <- perturb(time,params,perturbation)
            params <- tmp$params
            e <- tmp$e
            tmp <- matrix( evalmf(rangex[,i],
                                  params, type)+e,FTYPE,point_n,byrow=TRUE)
            if (count>1000 || all(!is.nan(tmp))) break
            count <- count+1
          }
        } else {
          tmp <- matrix( evalmf(rangex[,i], params, type),FTYPE,point_n,byrow=TRUE)
        }
        #clipping
        tmp[tmp<0]=0
        tmp[tmp>1]=1
        
        #Copy perturbed mf
        OUT_TEMP_MF[OUT_COUPLE+idx+1,] <<-tmp
        idx= idx + 1
      }
    }
    
    # restructure to fit OUT_MF
    idx=matrix(1, NUM_RULES, 1) %*% cumsum(c(1, NUM_OUT_MFS[0:(NUM_OUTS-1)])) +  abs(CON_RULES)
    idx[CON_RULES==0]= 1
    
    OUT_MF <<- OUT_TEMP_MF[t(idx),]
    
    #Now OUT_MF is NUM_RULES*NUM_OUTS X point_n matrix
    OUT_MF[t(CON_RULES)<0,] <<- 1 - OUT_MF[t(CON_RULES<0),]
    
    #if it's a type-2 fis,
    if (FTYPE==2){
      
      idx <- idx+NUM_TOTAL_OUT_MFS
      tmp <- OUT_TEMP_MF[t(idx),]
      
      #Now OUT_MF is NUM_RULES*NUM_OUTS X point_n matrix
      if (any(CON_RULES<0)){
        tt <- t(CON_RULES)<0
        tmp[tt,] <- 1 - tmp[tt,]
        temp <- tmp[tt,]
        tmp[tt,] <- OUT_MF[tt,]
        OUT_MF[tt,] <<- temp
      }
      OUT_MF <<- rbind(OUT_MF,tmp)
    }
    OUT_MF <<- matrix(t(OUT_MF), NUM_RULES*FTYPE, point_n*NUM_OUTS, byrow=TRUE)
  }
  # end of initialisation
  
  # check for errors in the input stack
  if ( is.vector(in_stack) ) {
    #Put the inputs into seperate columns
    in_stack= matrix(in_stack,ncol=NUM_INS,byrow=TRUE)#Error here
  }
  data_n= nrow(in_stack)
  
  # create output stack
  out_stack= matrix(0, data_n*FTYPE, NUM_OUTS)
  
  # loop through each input
  for ( k in 1:data_n ) {
    input= in_stack[k,]
    # create in_temp_mf_value
    in_temp_mf_value= matrix(0, NUM_TOTAL_IN_MFS*FTYPE ,1 )
    
    idx= 1
    pr<-c()
    for ( i in 1:NUM_INS ) {
      
      #create a filename
      str<-paste(dir,substring(fis$name,length(fis$name)),"input",i,".pdf",sep="")
      
      #if draw, create a pdf and plot this input mf
      if (draw && time==1 && !file.exists(str)){
        pdf(str)
        pushViewport(plotViewport())
        plotMF(fis,"input",i,xlab=tolower(fis$input[[i]]$name),main=fis$input[[i]]$name)
        popViewport()
        dev.off()
      }
      
      #Create filename
      str<-paste(dir,substring(fis$name,length(fis$name)),"-fuzzify-input-",i,"-x=",input[i],".pdf",sep="")
      
      #if draw, create pdf and plot input mf, this time alter xx 
      if (draw && time==1 && !file.exists(str)){
        pdf(str)
        pushViewport(plotViewport())
        plotMF(fis,"input",i,xlab=tolower(fis$input[[i]]$name),main=fis$input[[i]]$name,xx=input[i])
        popViewport()
        dev.off()
      }
      
      for ( j in 1:NUM_IN_MFS[i] ) {
        
        #Get details of this mf
        perturbation <- IN_PERTURBATION[[idx]]
        params <- IN_PARAMS[[idx]]
        type <- IN_TYPE[idx]
        
        #If there are any perturbation functions
        if (length(perturbation)>0) {
          count<-0
          #Apply perturbation function
          repeat{
            tmp <- perturb(time,params,perturbation)
            params <- tmp$params
            e <- tmp$e
            tmp <- matrix(evalmf(input[i],params , type)+e , FTYPE,1,byrow=TRUE)
            if (count>1000 || all(!is.nan(tmp)))  break
            count<-count+1
          }
        } else {
          tmp <- matrix(evalmf(input[i], params, type) , FTYPE,1,byrow=TRUE)
        }
        
        #Tidy up
        tmp[tmp>1]=1
        tmp[tmp<0]=0
        
        in_temp_mf_value[IN_COUPLE+idx,]<-tmp
        idx= idx + 1
      }
    }
    
    # restructure row of in_temp_mf_value to fit in_mf_value
    ind= matrix(1, NUM_RULES, 1) %*% cumsum(c(0, NUM_IN_MFS[0:(NUM_INS-1)])) + abs(ANT_RULES)
    in_mf_value= matrix(in_temp_mf_value[ind,], NUM_RULES, NUM_INS)
    
    # replace dont-care MFs in AND rules with 1, and OR rules with 0
    in_mf_value[which(((AND_OR == 1) * (ANT_RULES == 0)) == 1)]= 1
    in_mf_value[which(((AND_OR == 2) * (ANT_RULES == 0)) == 1)]= 0
    
    # sort out NOTs (negative rule indices)
    idx= which(ANT_RULES < 0)
    in_mf_value[idx]= 1 - in_mf_value[idx]
    
    #If it's a type-2 system 
    if (FTYPE==2){
      #set the ind to the total number of input mfs
      ind <- ind+NUM_TOTAL_IN_MFS
      tmp= matrix(in_temp_mf_value[ind,], NUM_RULES, NUM_INS)
      
      # replace dont-care MFs in AND rules with 1, and OR rules with 0
      tmp[which(((AND_OR == 1) * (ANT_RULES == 0)) == 1)]= 1
      tmp[which(((AND_OR == 2) * (ANT_RULES == 0)) == 1)]= 0
      
      # take care of NOTs (negative rule indices)
      if (length(idx)>0){
        temp <- in_mf_value[idx]
        in_mf_value[idx]= 1 - tmp[idx]
        tmp[idx] = temp
      }
      in_mf_value = rbind(in_mf_value,tmp)
    }
    
    # Get firing strengths for the rules
    f_str <- matrix(0, NUM_RULES*FTYPE, 1)
    if ( NUM_INS == 1 )
      f_str= in_mf_value
    else {
      and_ind= which(AND_OR == 1)
      or_ind= which(AND_OR == 2)
      f_str[and_ind,]=
        apply(rbind(in_mf_value[and_ind,]), 1, fis$andMethod)
      
      #Check type
      if (FTYPE==2){
        f_str[and_ind+NUM_RULES,]=
          apply(rbind(in_mf_value[and_ind+NUM_RULES,]), 1, fis$andMethod)
      }
      
      f_str[or_ind,]=
        apply(rbind(in_mf_value[or_ind,]), 1, fis$orMethod)
      
      #Check type
      if (FTYPE==2){
        f_str[or_ind+NUM_RULES,]=
          apply(rbind(in_mf_value[or_ind+NUM_RULES,]), 1, fis$orMethod)
      }
    }
    
    #Calculate weighted firing strength
    f_str= f_str * WT_RULES 
    
    if (all(f_str==0)){# if no rules fired,
      for (i in 1:NUM_OUTS){
        for (j in 1:FTYPE){
          out_stack[(k-1)*FTYPE+j,i]=NaN
        }
      }
    } else {
      #Check system type and implication methods
      if ( FIS_TYPE == 'mamdani' )	{
        
        tmp= matrix(f_str, nrow(f_str), point_n*NUM_OUTS)
        if ( fis$impMethod == 'prod' ) {
          OUT_RULE_CONS <<- tmp * OUT_MF
        } else if ( fis$impMethod == 'min' ) {
          OUT_RULE_CONS <<- pmin(tmp, OUT_MF)
        } else {
          cat('user-defined implication not implemented yet\n')
        }
        
        #IF draw and it's type-1, plot antecedents, consequents and aggregation and put them in PDFs
        if (draw && FTYPE==1 && time==1){
          input_set <- paste(tolower(fis$input[[1]]$name),"=",input[1])
          for (i in 2:NUM_INS)
            input_set <- paste(input_set,"and",tolower(fis$input[[i]]$name),"=",input[i])
          
          for (i in 1:NUM_RULES){
            str<-paste(dir,substring(fis$name,length(fis$name)),"-ante-eval-",i,"-(",sep="")
            s<-paste(str,input_set,").pdf",sep="")
            
            if (time==1 && !file.exists(s)){
              pdf(s)
              pushViewport(plotViewport())
              drawAnteEval(fis,i,input,in_mf_value[i,],point_n=point_n,title=NULL,label=TRUE)
              popViewport()
              dev.off()
            }
            
            str<-paste(dir,substring(fis$name,length(fis$name)),"-cons-eval-",i,"-(",sep="")
            s<-paste(str,input_set,").pdf",sep="")
            if (time==1 && !file.exists(s)){
              pdf(s)
              pushViewport(plotViewport())
              drawConsEval(fis,i,support=f_str[i,],OUT_RULE_CONS=OUT_RULE_CONS[i,],point_n=point_n,title=NULL,label=TRUE)
              popViewport()
              dev.off()
            }
            
            str<-paste(dir,substring(fis$name,length(fis$name)),"-rule-eval-",i,"-(",sep="")
            s<-paste(str,input_set,").pdf",sep="")
            if (time==1 && !file.exists(s)){
              pdf(s)
              pushViewport(plotViewport())
              drawRuleEval(fis,i,input=input,in_mf_value=in_mf_value[i,],support=f_str[i,],OUT_RULE_CONS=OUT_RULE_CONS[i,],point_n=point_n,title=NULL,label=TRUE)
              popViewport()
              dev.off()
            }
            
          }
          
          str<-paste(dir,substring(fis$name,length(fis$name)),"-rule-evaluation-(",sep="")
          s<-paste(str,input_set,").pdf",sep="")
          if (time==1 && !file.exists(s)){
            pdf(s)  
            
            pushViewport(viewport(width=0.9,height=0.9))
            
            drawRuleAggr(fis,input=input,in_mf_value=in_mf_value,f_str=f_str,OUT_RULE_CONS=OUT_RULE_CONS,point_n=point_n,title=NULL)
            popViewport()
            dev.off()
          }
          
        }
        
        
        # Check type and aggregate rule consequents
        for (ii in 1:FTYPE)
          OUT_RULE_AGG[ii,] <<- apply(OUT_RULE_CONS[(ii-1)*NUM_RULES+(1:NUM_RULES),], 2, fis$aggMethod)
        #perform type reduction if it's type-2
        if (FTYPE==2){
          if (fis$defuzzMethod=="centroid"){
            rs <- trCentroid(rbind(rangex,rangex),OUT_RULE_AGG)
            
            #plot output mf
            if (draw) plot_it2(rangex,OUT_RULE_AGG,"Y")
            
          } else if (fis$defuzzMethod=="cos"){ #Centre of Spread, for NSFS
            if (first || !exists("no_output_perturbation") || !no_output_perturbation){
              COS <<- matrix(0,NUM_RULES*2,NUM_OUTS)
              COS <<- trCentroid(rbind(rangex,rangex),OUT_MF)
            }
            rs <- trCOS(COS,f_str )
          } else if (fis$defuzzMethod=="coh"){ #Centre of Height?
            height <- matrix(0,NUM_RULES,NUM_OUTS)
            B <- matrix(0,NUM_RULES*2,NUM_OUTS)
            sp <- matrix(0,NUM_RULES,NUM_OUTS)
            
            for(i in 1:NUM_RULES){
              for (j in 1:NUM_OUTS){
                maxp <- max(OUT_RULE_CONS[i,(1:point_n)+(j-1)*point_n])
                height[i,j] <- mean(rangex[which(OUT_RULE_CONS[i,(1:point_n)+(j-1)*point_n]==maxp),j] )
                tmp <- abs(rangex[,j]-height[i,j])
                pos = which(tmp==min(tmp))[1]
                B[i,j] <- OUT_RULE_CONS[i,pos]
                B[i+NUM_RULES,j] <- OUT_RULE_CONS[i+NUM_RULES,pos]
                sp[i,j] <- (OUT_RULE_CONS[i,pos]+OUT_RULE_CONS[i+NUM_RULES,pos])/2
              }
            }
            
            rs <- iterative_method(rbind(height,height),B)
          } else if (fis$defuzzMethod=="csum"){ #cumulative sum?
            SUMS <<- matrix(0,point_n*2,NUM_OUTS)
            for (i in 1:NUM_OUTS){
              for (j in 1:point_n){
                SUMS[j,i] <- sum(OUT_RULE_CONS[1:NUM_RULES,(i-1)*point_n+j])
                SUMS[j+point_n,i] <- sum(OUT_RULE_CONS[(1+NUM_RULES):(NUM_RULES+NUM_RULES),(i-1)*point_n+j])
              }
            }
            rs <- iterative_method(rbind(rangex,rangex),SUMS)
          } else if (fis$defuzzMethod=="user"){   #User defined
            rs<-iterative_method(matrix(OUT_MF[,1],50,1 ),f_str)
          }
          
          # defuzzify each output
          for ( i in 1:NUM_OUTS ) {
            out_stack[2*k-1, i]=mean(rs[,i])
            out_stack[2*k, i]=rs[1,i]-mean(rs[,i])
          }
        } else {
          
          # defuzzify each output
          for ( i in 1:NUM_OUTS ) {
            
            out_stack[k, i]=
              defuzz(rangex[,i],
                     OUT_RULE_AGG[1,((i-1)*point_n+1):(i*point_n)], fis$defuzzMethod)
          }
          
          if ( draw ) {
            str<-paste(dir,substring(fis$name,length(fis$name)),"-all-evaluations-(",sep="")
            s<-paste(str,input_set,").pdf",sep="")
          }
          if (draw && time==1 && !file.exists(str)){
            #pdf(s)
            pushViewport(viewport(width=0.9,height=0.9))
            drawAllSteps(fis,input=input,in_mf_value=in_mf_value,f_str=f_str,OUT_RULE_CONS=OUT_RULE_CONS,
                         OUT_RULE_AGG=OUT_RULE_AGG,output=out_stack[k,],point_n=point_n,title=NULL)
            popViewport()
            #dev.off()
          }
        }
      }
      else if ( FIS_TYPE == 'sugeno' )
      {
        cat('sugeno inference not implemented yet\n')
      }
      else {
        cat('unknown inference type\n')
      }
    }#end of if
  }
  
  out_stack
}

# defuzz - return a defuzzified crisp value for a membership function
defuzz <- function(x, mf, type) {
  if ( type == "centroid" )
    sum(mf * x) / sum(mf)
  else if ( type == "bisector" )
    x[match(TRUE, cumsum(mf) >= sum(mf) / 2)]
  else if ( type == "mom" )
    mean(x[which(mf == max(mf))])
  else if ( type == "som" )
    x[min(which(mf == max(mf)))]
  else if ( type == "lom" )
    x[max(which(mf == max(mf)))]
  else
    NA
}

#############################################
# testing
tipper <- function() {
  fis= newfis('tipper')
  
  fis= addvar(fis, 'input', 'service', c(0, 10))
  fis= addvar(fis, 'input', 'food', c(0, 10))
  fis= addvar(fis, 'output', 'tip', c(0, 30))
  
  fis= addmf(fis, 'input', 1, 'poor', 'gaussmf', c(1.5, 0, 1))
  fis= addmf(fis, 'input', 1, 'good', 'gaussmf', c(1.5, 5, 1))
  fis= addmf(fis, 'input', 1, 'excellent', 'gaussmf', c(1.5, 10, 1))
  
  fis= addmf(fis, 'input', 2, 'rancid', 'trapmf', c(0, 0, 1, 3, 1))
  fis= addmf(fis, 'input', 2, 'delicious', 'trapmf', c(7, 9, 10, 10, 1))
  
  fis= addmf(fis, 'output', 1, 'cheap', 'trimf', c(0, 5, 10, 1))
  fis= addmf(fis, 'output', 1, 'average', 'trimf', c(10, 15, 20, 1))
  fis= addmf(fis, 'output', 1, 'generous', 'trimf', c(20, 25, 30, 1))
  
  rl = rbind(c(1,1,1,1,2), c(2,0,2,1,1), c(3,2,3,1,2))
  fis= addrule(fis, rl)
  
  fis
}