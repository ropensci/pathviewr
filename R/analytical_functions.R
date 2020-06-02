## Last updated: 2020-02-19 VBB

################################ get_rb_velocity ###############################
## Get instantaneous velocity for rigid body objects

get_rb_velocity <- function(obj_name,
                            time_col,
                            x_col,
                            y_col,
                            z_col,
                            ...) {
  ## Check that it's a motiv object
  if (!any(class(obj_name) == "motiv")) {
    stop("Input data should be of class `motiv`")
  }
  
  df <- as.data.frame(obj_name) # convert back to standard df?
  
  ## Create vector of time differences
  time_diffs <- diff(df[,time_col])
  
  ## Calculate instantaneous velocities
  ## First entry is set to zero because calculating velocity depends on previous 
  ## step. We may opt change this to `NA` later on so that these can be filtered 
  ## easily.
  x_inst <- c(0, diff(df[,x_col]) / time_diffs)
  y_inst <- c(0, diff(df[,y_col]) / time_diffs)
  z_inst <- c(0, diff(df[,z_col]) / time_diffs)
  
  ## Calculate object velocity
  vel <- sqrt((x_inst ^ 2) + (y_inst ^ 2) + (z_inst ^ 2))
  
  ## Combine
  res <- tibble::tibble(velocity = vel, x_inst, y_inst, z_inst)
  
  ## Output
  return(res)
}


#################################### rad2deg ###################################
## convert radians to degrees 
rad2deg <- function(rad) {
  
  ## Check that it's a numeric
  if (!any(class(rad) == "numeric")) {
    stop("Input angle must be a numeric")
  }
  
  return((rad * 180) / (pi))

}


#################################### deg2rad ###################################
## convert degrees to radians 
deg2rad <- function(deg) {
  
  ## Check that it's a numeric
  if (!any(class(deg) == "numeric")) {
    stop("Input angle must be a numeric")
  }
  
  return((deg * pi) / (180))
  
}


## Remaining three functions originally written by Christina Harvey
## May adapt for our purposes in the future
#angles in 3D space
xyzangles <- function(x1,y1,z1,x2,y2,z2,x3,y3,z3){
  i1=x2-x1
  i2=x2-x3
  j1=y2-y1
  j2=y2-y3
  k1=z2-z1
  k2=z2-z3
  dotprod=(i1*i2)+(j1*j2)+(k1*k2)
  len1=sqrt(i1^2+j1^2+k1^2)
  len2=sqrt(i2^2+j2^2+k2^2)
  theta=acos(dotprod/(len1*len2))*(180/pi)
  theta
}


#angles in 2D space
xyangles<-function(x1,y1,x2,y2,x3,y3){ 
  i1=x1;i2=x2;i3=x3
  j1=y1;j2=y2;j3=y3
  a=c(i1,j1)-c(i2,j2)
  b=c(i3,j3)-c(i2,j2)
  theta=acos(sum(a*b)/(sqrt(sum(a*a))*sqrt(sum(b*b))))*(180/pi)
  theta
} 


#angles between two planes in 3D space
xyzplaneangles <- function(x1,y1,z1,x2,y2,z2,x3,y3,z3, #plane 1
                           x4,y4,z4,x5,y5,z5,x6,y6,z6){ #plane 2
  #x1_std,y1_std,z1_std,x2_std,y2_std,z2_std,x3_std,y3_std,z3_std){
  #This computes the angle betweeon two planes composed of three points each
  #--- Vectors of Plane 1 - create vector from two 3D pts
  i1 = x2-x1
  j1 = y2-y1
  k1 = z2-z1
  i2 = x2-x3
  j2 = y2-y3
  k2 = z2-z3
  #--- Vectors of Plane 2 - create vector from two 3D pts
  i3 = x5-x4
  j3 = y5-y4
  k3 = z5-z4
  i4 = x5-x6
  j4 = y5-y6
  k4 = z5-z6
  #--- Orthogonal Vector of Plane 1 - Cross Product of the vectors on the plane
  i5 = j1*k2-k1*j2
  j5 = k1*i2-i1*k2
  k5 = i1*j2-j1*i2
  #--- Orthogonal Vector of Plane 2 - Cross Product of the vectors on the plane
  i6 = j3*k4-k3*j4
  j6 = k3*i4-i3*k4
  k6 = i3*j4-j3*i4
  #--- Dot Product of Orthogonal Vectors
  dotprod = ((i5*i6)+(j5*j6)+(k5*k6))
  len1    = sqrt(i5^2+j5^2+k5^2)
  len2    = sqrt(i6^2+j6^2+k6^2)
  interior = dotprod/(len1*len2)
  theta   = acos(interior)*(180/pi)
  
  return(theta)
}

