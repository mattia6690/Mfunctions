# R Spatial Functions MR

#' @title Vectorized Extraction of rasterValues
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
#' @param narm boolean; removes na values
#' @param seed integer; applies the set.seed() function enabling the control over
#' @import raster
#' @import sp
#' @import magrittr
#' @export

extract2<- function (rr,pp,points=1000,samp.type="regular",weight=F,narm=F,seed=1){

  #Transform the Raster to Array (much faster and cellnumbers)
  if(class(rr)=="RasterLayer"){t5<-values(rr) %>% as.array}
  if(class(rr)=="array"){t5<-rr}
  #Initialize Array for the Values and the Cells used
  val<-array(dim=nrow(pp))
  ncells<-array(dim=nrow(pp))
  nas<-array(dim=nrow(pp))
  stdev<- array(dim=nrow(pp))
  for (fi in 1:nrow(pp)){
    set.seed(seed)
    # Create a regular sampled Points in every Polygon
    u<-spsample(pp[fi,],n=points,type=samp.type)
    # Obtain the unique Cellnumbers intersecting with the Points
    if (weight==T){t1<- cellFromXY(rr,u)}
    if (weight==F){t1<- cellFromXY(rr,u) %>% unique}
    # Compute the Mean of the Cellvalues
    t2<- t5[t1] %>% mean(.,na.rm=narm)
    t3<- t5[t1] %>% sd(.,na.rm=narm)
    # Write the Values to the Arrays
    val[fi]<-t2
    ncells[fi]<- t1 %>% unique %>% length
    stdev[fi]<- t3
    nas[fi] <-t1 %>% unique %>% t5[.] %>% is.na %>% which %>% length
  }
  # Combine both Arrays in one Dataframe and return the result
  stat<-data.frame(val,ncells,stdev,nas)
  return(stat)
}

#' @title Mean of Points within Shapefile
#' @description The centroid Mean function returns a SpatialPointDataframe
#' containing the Centroid mean of Points within ESRI Shapefile(s).
#' @param spat SpatialPolygonDataFrame; A file containoing one or more Spatial
#' Polygons
#' @param pnt SpatialPointDataFrame; A file containing the Point cloud.
#' @param count boolean; If T, count the number of points within one Polygon. OPTIONAL
#' @import raster
#' @import rgeos
#' @export

centroidMean<-function(spat,pnt,count=T){

  proj1<-projection(spat)
  spat[["data"]]<-seq(1,length(spat),1)

  spat_sp<-pj <- spTransform(spat[2], CRS=CRS(proj1))
  pnt_sp<-pj <- spTransform(pnt, CRS=CRS(proj1))

  ov<-over(pnt_sp,spat_sp)

  vals<-unique(ov$data)
  if(count==F){mycenters<- matrix(ncol=2,nrow=length(spat))}else{
    mycenters<- matrix(ncol=3,nrow=length(spat))
  }

  for(i in 1:length(vals)){

    myog_temp<-pnt[which(ov==i),]
    if(length(myog_temp)==1){
      mycenters[i,c(1,2)]<-coordinates(myog_temp)}else{
      mycenters[i,c(1,2)]<-colMeans(coordinates(myog_temp))}
    if(count==T){
      mycenters[i,3]<-length(myog_temp)
    }
  }

  centers_dat<-as.data.frame(seq(1,nrow(mycenters),1))
  centers<-SpatialPointsDataFrame(mycenters,data=centers_dat,proj4string = CRS(projection(pnt)))
  names(centers@data)<-c("Data")
  if(count==T){centers@data[["Count"]]<-mycenters[,3]}

  return(centers)
}




