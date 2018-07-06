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
#' @param returnVals boolean; return all extracted values
#' @import raster
#' @import sp
#' @import magrittr
#' @export

extract2<- function (rr,pp,points=NA,samp.type="regular",weight=F,narm=F,seed=1,returnVals=F){

  #Transform the Raster to Array (much faster and cellnumbers)

  if(is.na(points)) points<-ncell(rr)
  if(class(rr)=="RasterLayer"){t5<-values(rr) %>% as.array}
  if(class(rr)=="array"){t5<-rr}
  #Initialize Array for the Values and the Cells used
  Mean<-array(dim=nrow(pp))
  Ncells<-array(dim=nrow(pp))
  Nas<-array(dim=nrow(pp))
  Stdev<- array(dim=nrow(pp))
  if(returnVals==T) vallist<-list()
  for (fi in 1:nrow(pp)){
    set.seed(seed)
    # Create a regular sampled Points in every Polygon
    u<-spsample(pp[fi,],n=points,type=samp.type)
    # Obtain the unique Cellnumbers intersecting with the Points
    if (weight==T){t1<- cellFromXY(rr,u)}
    if (weight==F){t1<- cellFromXY(rr,u) %>% unique}

    values<- t5[t1]
    if(returnVals==T) {vallist[[fi]]<-values; next}

    # Compute the Mean of the Cellvalues
    t2<- values %>% mean(.,na.rm=narm)
    t3<- values %>% sd(.,na.rm=narm)

    # Write the Values to the Arrays
    Mean[fi]<-t2
    Ncells[fi]<- t1 %>% unique %>% length
    Stdev[fi]<- t3
    Nas[fi] <-t1 %>% unique %>% t5[.] %>% is.na %>% which %>% length
  }
  # Combine both Arrays in one Dataframe and return the result
  if(returnVals==T) return(vallist) else return(data.frame(Mean,Stdev,Ncells,Nas))
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




