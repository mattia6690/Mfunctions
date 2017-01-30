# R Spatial Functions MR

#' @title Extract 2
#' @description With this functions raster values are extracted by various
#' overlaying spatial points created within a polygon shapefile.
#' @param rr Raster object; raster image
#' @param pp Spatial object; polygon file
#' @param points integer; Number of SpatialPoints created in the Polygon
#' @param samp.type character; Sampling technique for the Spatial Points
#' generated as described in the spsample() function of the "sp" package
#' @param weight boolean; Weighting image considering the underlaying rasters a
#' differentweight in the extraction computation. This makes pixel partially
#' located within a polygon less weighted. Input is T/F
#' @param sd boolean; computes the standard deviation of each extracted raster
#' @param narm boolean; removes na values
#' @param seed integer; applies the set.seed() function enabling the control over
#' @export

extract2<- function (rr,pp,points=1000,samp.type="regular",weight=F,sd=F,narm=F,seed=1){

  #Transform the Raster to Array (much faster and cellnumbers)
  t5<-as.array(values(rr))
  #Initialize Array for the Values and the Cells used
  val<-array(dim=nrow(pp))
  ncells<-array(dim=nrow(pp))
  if (sd==T){stdev<- array(dim=nrow(pp))}
  for (fi in 1:nrow(pp)){
    set.seed(seed)
    # Create a regular sampled Points in every Polygon
    u<-spsample(pp[fi,],n=points,type=samp.type)
    # Obtain the unique Cellnumbers intersecting with the Points
    if (weight==T){t1<- cellFromXY(rr,u)}
    if (weight==F){t1<- unique(cellFromXY(rr,u))}
    # Compute the Mean of the Cellvalues
    t2<- mean(t5[t1],na.rm=narm)
    if (sd==T){t3<- sd(t5[t1],na.rm=narm)}
    # Write the Values to the Arrays
    val[fi]<-t2
    ncells[fi]<- length(unique(t1))
    if (sd==T){stdev[fi]<- t3}
    if (fi%%5000==0){print(str_c(fi, " of ", nrow(pp)))}
  }
  # Combine both Arrays in one Dataframe and return the result
  if(sd==F){stat<-data.frame(val,ncells)}
  if(sd==T){stat<-data.frame(val,ncells,stdev)}
  return(stat)
}
