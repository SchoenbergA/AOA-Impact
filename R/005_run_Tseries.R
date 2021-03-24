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
source(file.path(path,"R/100_Dawn2.R"))

# load data

# RGB data
lau_rgb <- raster::stack(file.path(path,"Data/lau_RGB.grd") )# the example rgb imaga

# plot
plotRGB(lau_rgb,3,2,1)

# load Training Points
lau_tP <-rgdal::readOGR(file.path(path,"DATA/lau_Tpoints.shp"))
# handle CRS string
crs(lau_tP) <- crs(lau_rgb)

# load response layer
lau_rsp <-rgdal::readOGR(system.file("extdata","lau_TreeSeg.shp",package = "IKARUS"))
# handle CRS string
crs(lau_rsp) <- crs(lau_rgb)


### MSc Test Series I Impact of training design on RGB only
# Note: width is in radius meter

# path to save resulting pngs
path_png <- file.path(path,"Data/results/RGB_sizes//")

# with 0.15 cell size: 0.15 steps makes sense, bigger than 1.2 look too big
t1 <-Dawn2(validate = T,rsp = lau_rsp,rsp_class = "t", Tpoints = lau_tP,buf_size = 0.15,design = "ROUND",Stk = lau_rgb,Stk_name = "rgb",plot_res = F,save_png = F,save_res = F,path_png = path_png)
t2 <-Dawn2(validate = T,rsp = lau_rsp,rsp_class = "t", Tpoints = lau_tP,buf_size = 0.3 ,design = "ROUND",Stk = lau_rgb,Stk_name = "rgb",plot_res = T,save_png = F,save_res = F,path_png = path_png)
t3 <-Dawn2(validate = T,rsp = lau_rsp,rsp_class = "t", Tpoints = lau_tP,buf_size = 0.60,design = "ROUND",Stk = lau_rgb,Stk_name = "rgb",plot_res = F,save_png = F,save_res = F,path_png = path_png)


# function to merge all df in a list
mergeDFs <- function(ls){
  # loop
  for (i in 1:length(ls)-1){

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
}



IKARUS_dawn(Tpoints = lau_tP,buf_size = 1.2 ,design = "ROUND",Stk = lau_rgb,Stk_name = "rgb",plot_res = T,save_png = T,save_res = F,path_png = path_png)

IKARUS_dawn(Tpoints = lau_tP,buf_size = 0.15,design = "SQUARE",Stk = lau_rgb,Stk_name = "rgb",plot_res = T,save_png = T,save_res = F,path_png = path_png)
IKARUS_dawn(Tpoints = lau_tP,buf_size = 0.3 ,design = "SQUARE",Stk = lau_rgb,Stk_name = "rgb",plot_res = T,save_png = T,save_res = F,path_png = path_png)
IKARUS_dawn(Tpoints = lau_tP,buf_size = 0.60,design = "SQUARE",Stk = lau_rgb,Stk_name = "rgb",plot_res = T,save_png = T,save_res = F,path_png = path_png)
IKARUS_dawn(Tpoints = lau_tP,buf_size = 1.2 ,design = "SQUARE",Stk = lau_rgb,Stk_name = "rgb",plot_res = T,save_png = T,save_res = F,path_png = path_png)
############################

### MSc Test Series II LEGION Stk


# load Stacks
hmgy_selected <- raster::stack(file.path("C:/Envimaster/MSc_Thesis/Data/LEGION_stk/hmgy_selected.grd"))
hmgy_small <- raster::stack(file.path("C:/Envimaster/MSc_Thesis/Data/LEGION_stk/hmgy_small.grd"))
hmgy_full <- raster::stack(file.path("C:/Envimaster/MSc_Thesis/Data/LEGION_stk/hmgy_full.grd"))
corT_full <- raster::stack(file.path("C:/Envimaster/MSc_Thesis/Data/LEGION_stk/corT2_full.grd"))

# path to save png
path_png <- file.path("G:/Data/results/LEGIONstk//")

IKARUS_dawn(Tpoints = lau_tP,buf_size = 0.3,design = "ROUND",Stk = hmgy_selected,Stk_name = "hmgy_selected",plot_res = T,save_png = T,save_res = F,path_png = path_png)
IKARUS_dawn(Tpoints = lau_tP,buf_size = 0.3,design = "ROUND",Stk = hmgy_small,Stk_name = "hmgy_small",plot_res = T,save_png = T,save_res = F,path_png = path_png)
IKARUS_dawn(Tpoints = lau_tP,buf_size = 0.3,design = "ROUND",Stk = hmgy_full,Stk_name = "hmgy_full",plot_res = T,save_png = T,save_res = F,path_png = path_png)
IKARUS_dawn(Tpoints = lau_tP,buf_size = 0.3,design = "ROUND",Stk = corT_full,Stk_name = "corT_full",plot_res = T,save_png = T,save_res = F,path_png = path_png)
############################


### MSc Test Series III PCA

# load PCA Stacks
pca_rgb       <- raster::stack(file.path(path,"Data/PCA_stk/pca_rgb.grd")) # pca_rgb
#stk_pca_rgb2      <- raster::stack(file.path(path,"Data/PCA_stk/pca_rgb2.grd")) # pca rgb + rgb
pca_ind       <- raster::stack(file.path(path,"Data/PCA_stk/pca_indrgb.grd"))# pca for rgb+indices
pca_ind_only  <- raster::stack(file.path(path,"Data/PCA_stk/pca_ind.grd")) # pca for only indices
#stk_pca_ind_rgb   <- raster::stack(file.path(path,"Data/PCA_stk/pca_ind_RGB.grd")) # pca for ind only + rgb bands
pca_rgb_filt  <- raster::stack(file.path(path,"Data/PCA_stk/pca_rgb_filt.grd")) # pca for rgb filter

pca_hmgy_full      <- raster::stack(file.path(path,"Data/PCA_stk/pca_hmgy_full.grd"))
pca_hmgy_selected  <- raster::stack(file.path(path,"Data/PCA_stk/pca_hmgy_selected.grd"))
pca_hmgy_small     <- raster::stack(file.path(path,"Data/PCA_stk/pca_hmgy_small.grd"))
pca_corT_full     <- raster::stack(file.path(path,"Data/PCA_stk/pca_corT2_full.grd"))

# path to save png
path_png <- file.path(path,"Data/results/PCA_03//")

IKARUS_dawn(Tpoints = lau_tP,buf_size = 0.3,design = "ROUND",Stk = pca_rgb,Stk_name      = "pca_rgb",plot_res = T,save_png = T,save_res = F,path_png = path_png)
#IKARUS_dawn(Tpoints = lau_tP,buf_size = 0.3,design = "ROUND",Stk = pca_rgb2,Stk_name     = "pca_rgb2",plot_res = T,save_png = T,save_res = F,path_png = path_png)
IKARUS_dawn(Tpoints = lau_tP,buf_size = 0.3,design = "ROUND",Stk = pca_ind,Stk_name      = "pca_ind",plot_res = T,save_png = T,save_res = F,path_png = path_png)
IKARUS_dawn(Tpoints = lau_tP,buf_size = 0.3,design = "ROUND",Stk = pca_ind_only,Stk_name = "pca_ind_only",plot_res = T,save_png = T,save_res = F,path_png = path_png)
#IKARUS_dawn(Tpoints = lau_tP,buf_size = 0.3,design = "ROUND",Stk = pca_ind_rgb,Stk_name  = "pca_ind_rgb",plot_res = T,save_png = T,save_res = F,path_png = path_png)
IKARUS_dawn(Tpoints = lau_tP,buf_size = 0.3,design = "ROUND",Stk = pca_rgb_filt,Stk_name = "pca_rgb_filt",plot_res = T,save_png = T,save_res = F,path_png = path_png)

IKARUS_dawn(Tpoints = lau_tP,buf_size = 0.3,design = "ROUND",Stk = pca_hmgy_full,Stk_name      = "pca_hmgy_full",plot_res = T,save_png = T,save_res = F,path_png = path_png)
IKARUS_dawn(Tpoints = lau_tP,buf_size = 0.3,design = "ROUND",Stk = pca_hmgy_selected,Stk_name = "pca_hmgy_selected",plot_res = T,save_png = T,save_res = F,path_png = path_png)
IKARUS_dawn(Tpoints = lau_tP,buf_size = 0.3,design = "ROUND",Stk = pca_hmgy_small,Stk_name  = "pca_hmgy_small",plot_res = T,save_png = T,save_res = F,path_png = path_png)
IKARUS_dawn(Tpoints = lau_tP,buf_size = 0.3,design = "ROUND",Stk = pca_corT_full,Stk_name = "pca_corT_full",plot_res = T,save_png = T,save_res = F,path_png = path_png)
##########################


### MSc Test Series IV Impact of FFS tests


# load stk
ffs_ind_only  <- raster::stack(file.path(path,"Data/FFS_stk/ind_only.grd")) # indices (without HI, RI)
ffs_ind   <- raster::stack(file.path(path,"Data/FFS_stk/indRGB.grd")) # indices (without HI, RI) + RGB
ffs_ind_all   <- raster::stack(file.path(path,"Data/FFS_stk/ind_all.grd")) # all indices

ffs_filtcor     <- raster::stack(file.path(path,"Data/FFS_stk/rgb_filt.grd")) # filter RGB and cor test +RGB
#filtercor_only  <- raster::stack(file.path(path,"Data/FFS_stk/filtercor_only.grd")) # filter RGB and cor test
ffs_pca         <- raster::stack(file.path(path,"Data/FFS_stk/stk_pca.grd")) # PCA for rgb_filtcor + rgb_filtcor

ffs_pca         <- raster::stack(file.path("G:/Data/FFS_stk/stk_pca.grd")) # PCA for rgb_filtcor + rgb_filtcor

# path to save png
path_png <- file.path(path,"Data/results/FFS_03//")

# save to variable to access 'selected_vars'
f1 <-IKARUS_dawn(Tpoints = lau_tP,buf_size = 0.3,design = "ROUND",Stk = ffs_ind_only,Stk_name = "ffs_ind_only",plot_res = T,save_png = T,save_res = F,path_png = path_png)
f2 <-IKARUS_dawn(Tpoints = lau_tP,buf_size = 0.3,design = "ROUND",Stk = ffs_ind ,Stk_name = "ffs_ind ",plot_res = T,save_png = T,save_res = F,path_png = path_png)
f3 <-IKARUS_dawn(Tpoints = lau_tP,buf_size = 0.3,design = "ROUND",Stk = ffs_ind_all,Stk_name = "ffs_ind_all",plot_res = T,save_png = T,save_res = F,path_png = path_png)
f4 <-IKARUS_dawn(Tpoints = lau_tP,buf_size = 0.3,design = "ROUND",Stk = ffs_filtcor,Stk_name = "ffs_filtcor",plot_res = T,save_png = T,save_res = F,path_png = path_png)
f5 <-IKARUS_dawn(Tpoints = lau_tP,buf_size = 0.3,design = "ROUND",Stk = ffs_pca,Stk_name = "ffs_pca",plot_res = T,save_png = T,save_res = F,path_png = path_png)

# selected vars
f1$cl$model_LLOCV$selectedvars
f2$cl$model_LLOCV$selectedvars
f3$cl$model_LLOCV$selectedvars
f4$cl$model_LLOCV$selectedvars
f5$cl$model_LLOCV$selectedvars
#######################################################


### MSc Test Series V Finetune PCA
# path to save png
path_png <- file.path(path,"Data/results/FT//")

IKARUS_dawn(Tpoints = lau_tP,buf_size = 0.15,design = "ROUND",Stk = pca_rgb_filt,Stk_name = "pca_rgb_filt",plot_res = T,save_png = T,save_res = F,path_png = path_png)
IKARUS_dawn(Tpoints = lau_tP,buf_size = 0.3,design = "ROUND",Stk = pca_rgb_filt,Stk_name = "pca_rgb_filt",plot_res = T,save_png = T,save_res = F,path_png = path_png)
IKARUS_dawn(Tpoints = lau_tP,buf_size = 0.6,design = "ROUND",Stk = pca_rgb_filt,Stk_name = "pca_rgb_filt",plot_res = T,save_png = T,save_res = F,path_png = path_png)
IKARUS_dawn(Tpoints = lau_tP,buf_size = 1.2,design = "ROUND",Stk = pca_rgb_filt,Stk_name = "pca_rgb_filt",plot_res = T,save_png = T,save_res = F,path_png = path_png)



ft1 <-IKARUS_dawn(Tpoints = lau_tP,buf_size = 0.15,design = "ROUND",Stk = ffs_filtcor,Stk_name = "ffs_filtcor",plot_res = T,save_png = T,save_res = F,path_png = path_png)
ft2 <-IKARUS_dawn(Tpoints = lau_tP,buf_size = 0.15,design = "ROUND",Stk = ffs_pca,Stk_name = "ffs_pca",plot_res = T,save_png = T,save_res = F,path_png = path_png)

ft3 <-IKARUS_dawn(Tpoints = lau_tP,buf_size = 0.3,design = "ROUND",Stk = ffs_filtcor,Stk_name = "ffs_filtcor",plot_res = T,save_png = T,save_res = F,path_png = path_png)
ft4 <-IKARUS_dawn(Tpoints = lau_tP,buf_size = 0.3,design = "ROUND",Stk = ffs_pca,Stk_name = "ffs_pca",plot_res = T,save_png = T,save_res = F,path_png = path_png)

ft5 <-IKARUS_dawn(Tpoints = lau_tP,buf_size = 0.6,design = "ROUND",Stk = ffs_filtcor,Stk_name = "ffs_filtcor",plot_res = T,save_png = T,save_res = F,path_png = path_png)
ft6 <-IKARUS_dawn(Tpoints = lau_tP,buf_size = 0.6,design = "ROUND",Stk = ffs_pca,Stk_name = "ffs_pca",plot_res = T,save_png = T,save_res = F,path_png = path_png)

ft7 <-IKARUS_dawn(Tpoints = lau_tP,buf_size = 1.2,design = "ROUND",Stk = ffs_filtcor,Stk_name = "ffs_filtcor",plot_res = T,save_png = T,save_res = F,path_png = path_png)
ft8 <-IKARUS_dawn(Tpoints = lau_tP,buf_size = 1.2,design = "ROUND",Stk = ffs_pca,Stk_name = "ffs_pca",plot_res = T,save_png = T,save_res = F,path_png = path_png)
###########################################################
