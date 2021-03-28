### Classification and AOA Testing

# setup set working directory
getwd()
path <-"C:/Envimaster/AOA-Impact" # set drive letter for stick
setwd(path)


# load libs
require(raster)
require(caret)
require(CAST)
require(doParallel)
require(rgeos)

# required for visualization
require(viridis)
require(png)
require(latticeExtra)
require(gridExtra)
require(IKARUS)
# load functions
source(file.path(path,"R/002_FUN_IKARUS.R"))
source(file.path(path,"R/003_Dawn2.R"))

# load data

# RGB data
lau_rgb <- raster::stack(file.path(path,"Data/lau_RGB.grd") )# the example rgb imaga
FFS_Stk <- raster::stack(file.path(path,"Data/FFS_Stack.grd") )# the example rgb imaga

# plot
plotRGB(lau_rgb,3,2,1)

# load Training Points
lau_tP1 <-rgdal::readOGR(file.path(path,"DATA/lau_Tpoints.shp"))
lau_tP2 <-rgdal::readOGR(file.path(path,"DATA/lau_Tpoints2.shp"))
lau_tP3 <-rgdal::readOGR(file.path(path,"DATA/lau_Tpoints3.shp"))
# handle CRS string
crs(lau_tP) <- crs(lau_rgb)
crs(lau_tP2) <- crs(lau_rgb)
crs(lau_tP3) <- crs(lau_rgb)

# load response layer
lau_rsp <-rgdal::readOGR(system.file("extdata","lau_TreeSeg.shp",package = "IKARUS"))
# handle CRS string
crs(lau_rsp) <- crs(lau_rgb)

# path to save resulting pngs
path_png <- file.path(path,"Results//")

# ffs c15
ffs115 <-Dawn2(FFS=T, Tpoints = lau_tP1,buf_size = 0.15,design = "ROUND",Stk = FFS_Stk,Stk_name = "ffs_tp1",plot_res = T,save_png = F,save_res = F,path_png = path_png,validate = T,rsp = lau_rsp,rsp_class = "t")
ffs215 <-Dawn2(FFS=T, Tpoints = lau_tP2,buf_size = 0.15,design = "ROUND",Stk = FFS_Stk,Stk_name = "ffs_tp2",plot_res = T,save_png = F,save_res = F,path_png = path_png,validate = T,rsp = lau_rsp,rsp_class = "t")
ffs315 <-Dawn2(FFS=T, Tpoints = lau_tP3,buf_size = 0.15,design = "ROUND",Stk = FFS_Stk,Stk_name = "ffs_tp3",plot_res = T,save_png = F,save_res = F,path_png = path_png,validate = T,rsp = lau_rsp,rsp_class = "t")

# ffs c30
ffs130 <-Dawn2(FFS=T, Tpoints = lau_tP1,buf_size = 0.30,design = "ROUND",Stk = FFS_Stk,Stk_name = "ffs_tp1",plot_res = T,save_png = F,save_res = F,path_png = path_png,validate = T,rsp = lau_rsp,rsp_class = "t")
ffs230 <-Dawn2(FFS=T, Tpoints = lau_tP2,buf_size = 0.30,design = "ROUND",Stk = FFS_Stk,Stk_name = "ffs_tp2",plot_res = T,save_png = F,save_res = F,path_png = path_png,validate = T,rsp = lau_rsp,rsp_class = "t")
ffs330 <-Dawn2(FFS=T, Tpoints = lau_tP3,buf_size = 0.30,design = "ROUND",Stk = FFS_Stk,Stk_name = "ffs_tp3",plot_res = T,save_png = F,save_res = F,path_png = path_png,validate = T,rsp = lau_rsp,rsp_class = "t")

# ffs c45
ffs145 <-Dawn2(FFS=T, Tpoints = lau_tP1,buf_size = 0.45,design = "ROUND",Stk = FFS_Stk,Stk_name = "ffs_tp1",plot_res = T,save_png = F,save_res = F,path_png = path_png,validate = T,rsp = lau_rsp,rsp_class = "t")
ffs245 <-Dawn2(FFS=T, Tpoints = lau_tP2,buf_size = 0.45,design = "ROUND",Stk = FFS_Stk,Stk_name = "ffs_tp2",plot_res = T,save_png = F,save_res = F,path_png = path_png,validate = T,rsp = lau_rsp,rsp_class = "t")
ffs345 <-Dawn2(FFS=T, Tpoints = lau_tP3,buf_size = 0.45,design = "ROUND",Stk = FFS_Stk,Stk_name = "ffs_tp3",plot_res = T,save_png = F,save_res = F,path_png = path_png,validate = T,rsp = lau_rsp,rsp_class = "t")

# ffs c60
ffs160 <-Dawn2(FFS=T, Tpoints = lau_tP1,buf_size = 0.60,design = "ROUND",Stk = FFS_Stk,Stk_name = "ffs_tp1",plot_res = T,save_png = F,save_res = F,path_png = path_png,validate = T,rsp = lau_rsp,rsp_class = "t")
ffs260 <-Dawn2(FFS=T, Tpoints = lau_tP2,buf_size = 0.60,design = "ROUND",Stk = FFS_Stk,Stk_name = "ffs_tp2",plot_res = T,save_png = F,save_res = F,path_png = path_png,validate = T,rsp = lau_rsp,rsp_class = "t")
ffs360 <-Dawn2(FFS=T, Tpoints = lau_tP3,buf_size = 0.60,design = "ROUND",Stk = FFS_Stk,Stk_name = "ffs_tp3",plot_res = T,save_png = F,save_res = F,path_png = path_png,validate = T,rsp = lau_rsp,rsp_class = "t")

# ffs c75
ffs175 <-Dawn2(FFS=T, Tpoints = lau_tP1,buf_size = 0.75,design = "ROUND",Stk = FFS_Stk,Stk_name = "ffs_tp1",plot_res = T,save_png = F,save_res = F,path_png = path_png,validate = T,rsp = lau_rsp,rsp_class = "t")
ffs275 <-Dawn2(FFS=T, Tpoints = lau_tP2,buf_size = 0.75,design = "ROUND",Stk = FFS_Stk,Stk_name = "ffs_tp2",plot_res = T,save_png = F,save_res = F,path_png = path_png,validate = T,rsp = lau_rsp,rsp_class = "t")
ffs375 <-Dawn2(FFS=T, Tpoints = lau_tP3,buf_size = 0.75,design = "ROUND",Stk = FFS_Stk,Stk_name = "ffs_tp3",plot_res = T,save_png = F,save_res = F,path_png = path_png,validate = T,rsp = lau_rsp,rsp_class = "t")

# ffs c90
ffs190 <-Dawn2(FFS=T, Tpoints = lau_tP1,buf_size = 0.90,design = "ROUND",Stk = FFS_Stk,Stk_name = "ffs_tp1",plot_res = T,save_png = F,save_res = F,path_png = path_png,validate = T,rsp = lau_rsp,rsp_class = "t")
ffs290 <-Dawn2(FFS=T, Tpoints = lau_tP2,buf_size = 0.90,design = "ROUND",Stk = FFS_Stk,Stk_name = "ffs_tp2",plot_res = T,save_png = F,save_res = F,path_png = path_png,validate = T,rsp = lau_rsp,rsp_class = "t")
ffs390 <-Dawn2(FFS=T, Tpoints = lau_tP3,buf_size = 0.90,design = "ROUND",Stk = FFS_Stk,Stk_name = "ffs_tp3",plot_res = T,save_png = F,save_res = F,path_png = path_png,validate = T,rsp = lau_rsp,rsp_class = "t")

# ffs c105
ffs1105 <-Dawn2(FFS=T, Tpoints = lau_tP1,buf_size = 0.105,design = "ROUND",Stk = FFS_Stk,Stk_name = "ffs_tp1",plot_res = T,save_png = F,save_res = F,path_png = path_png,validate = T,rsp = lau_rsp,rsp_class = "t")
ffs2105 <-Dawn2(FFS=T, Tpoints = lau_tP2,buf_size = 0.105,design = "ROUND",Stk = FFS_Stk,Stk_name = "ffs_tp2",plot_res = T,save_png = F,save_res = F,path_png = path_png,validate = T,rsp = lau_rsp,rsp_class = "t")
ffs3105 <-Dawn2(FFS=T, Tpoints = lau_tP3,buf_size = 0.105,design = "ROUND",Stk = FFS_Stk,Stk_name = "ffs_tp3",plot_res = T,save_png = F,save_res = F,path_png = path_png,validate = T,rsp = lau_rsp,rsp_class = "t")

# ffs c120
ffs1120 <-Dawn2(FFS=T, Tpoints = lau_tP1,buf_size = 0.120,design = "ROUND",Stk = FFS_Stk,Stk_name = "ffs_tp1",plot_res = T,save_png = F,save_res = F,path_png = path_png,validate = T,rsp = lau_rsp,rsp_class = "t")
ffs2120 <-Dawn2(FFS=T, Tpoints = lau_tP2,buf_size = 0.120,design = "ROUND",Stk = FFS_Stk,Stk_name = "ffs_tp2",plot_res = T,save_png = F,save_res = F,path_png = path_png,validate = T,rsp = lau_rsp,rsp_class = "t")
ffs3120 <-Dawn2(FFS=T, Tpoints = lau_tP3,buf_size = 0.120,design = "ROUND",Stk = FFS_Stk,Stk_name = "ffs_tp3",plot_res = T,save_png = F,save_res = F,path_png = path_png,validate = T,rsp = lau_rsp,rsp_class = "t")




# get list for all values
ls <- list(ffs115$VALUES,ffs215$VALUES,ffs315$VALUES,
           ffs130$VALUES,ffs230$VALUES,ffs330$VALUES,
           ffs145$VALUES,ffs245$VALUES,ffs345$VALUES,
           ffs160$VALUES,ffs260$VALUES,ffs360$VALUES,
           ffs175$VALUES,ffs275$VALUES,ffs375$VALUES,
           ffs190$VALUES,ffs290$VALUES,ffs390$VALUES,
           ffs1105$VALUES,ffs2105$VALUES,ffs3105$VALUES,
           ffs1120$VALUES,ffs2120$VALUES,ffs3120$VALUES)


# function to merge df
mergeDFs <- function(ls){
  # loop
  for (i in 1:(length(ls)-1)){
    
    # first run set df 
    if(i==1){
      df <- merge(ls[i],ls[i+1],all=T)
      # all other runs add next ls to df
    } else {
      df <- merge(df,ls[i+1],all=T)
    }
  } 
  if(nrow(df)!=length(ls)){
    stop("something wrong")
  }
  return(df)
}# end function

# merge df

results <- mergeDFs(ls)
results