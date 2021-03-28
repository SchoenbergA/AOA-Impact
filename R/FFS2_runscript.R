### Classification and AOA Testing

# setup set working directory
getwd()
path <-"D:/MSC" # set drive letter for stick
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

# load functions
source(file.path(path,"R/002_FUN_IKARUS.R"))
source(file.path(path,"R/003_Dawn2.R"))

# load data

# RGB data
lau_FFS2 <- raster::stack(file.path(path,"Data/Area2/FFS2_Stack.grd") )

# plot
plot(lau_FFS2[[1:3]])


# response layer
lau2_rsp <-rgdal::readOGR(file.path(path,"DATA/Area2/area2_response.shp"))

# handle CRS string
crs(lau2_rsp) <- crs(lau_FFS2)


# load Training Points
lau2_tP1 <-rgdal::readOGR(file.path(path,"DATA/Area2/lau2_Tpoints.shp"))
lau2_tP2 <-rgdal::readOGR(file.path(path,"DATA/Area2/lau2_Tpoints2.shp"))

# handle CRS string
crs(lau2_tP1) <- crs(lau_FFS2)
crs(lau2_tP2) <- crs(lau_FFS2)

# path to save resulting pngs
path_png <- file.path(path,"result_IMG2//")


# RGB2 c15
rgb2115 <-Dawn2(FFS=T, Tpoints = lau2_tP1,buf_size = 0.15,design = "ROUND",Stk = lau_FFS2,Stk_name = "FFS2_tp1",plot_res = T,save_png = T,save_res = F,path_png = path_png,validate = T,rsp = lau2_rsp,rsp_class = "t")
rgb2215 <-Dawn2(FFS=T, Tpoints = lau2_tP2,buf_size = 0.15,design = "ROUND",Stk = lau_FFS2,Stk_name = "FFS2_tp2",plot_res = T,save_png = T,save_res = F,path_png = path_png,validate = T,rsp = lau2_rsp,rsp_class = "t")

# RGB2 c30
rgb2130 <-Dawn2(FFS=T, Tpoints = lau2_tP1,buf_size = 0.30,design = "ROUND",Stk = lau_FFS2,Stk_name = "FFS2_tp1",plot_res = T,save_png = T,save_res = F,path_png = path_png,validate = T,rsp = lau2_rsp,rsp_class = "t")
rgb2230 <-Dawn2(FFS=T, Tpoints = lau2_tP2,buf_size = 0.30,design = "ROUND",Stk = lau_FFS2,Stk_name = "FFS2_tp2",plot_res = T,save_png = T,save_res = F,path_png = path_png,validate = T,rsp = lau2_rsp,rsp_class = "t")

# RGB2 c45
rgb2145 <-Dawn2(FFS=T, Tpoints = lau2_tP1,buf_size = 0.45,design = "ROUND",Stk = lau_FFS2,Stk_name = "FFS2_tp1",plot_res = T,save_png = T,save_res = F,path_png = path_png,validate = T,rsp = lau2_rsp,rsp_class = "t")
rgb2245 <-Dawn2(FFS=T, Tpoints = lau2_tP2,buf_size = 0.45,design = "ROUND",Stk = lau_FFS2,Stk_name = "FFS2_tp2",plot_res = T,save_png = T,save_res = F,path_png = path_png,validate = T,rsp = lau2_rsp,rsp_class = "t")

# RGB2 c60
rgb2160 <-Dawn2(FFS=T, Tpoints = lau2_tP1,buf_size = 0.60,design = "ROUND",Stk = lau_FFS2,Stk_name = "FFS2_tp1",plot_res = T,save_png = T,save_res = F,path_png = path_png,validate = T,rsp = lau2_rsp,rsp_class = "t")
rgb2260 <-Dawn2(FFS=T, Tpoints = lau2_tP2,buf_size = 0.60,design = "ROUND",Stk = lau_FFS2,Stk_name = "FFS2_tp2",plot_res = T,save_png = T,save_res = F,path_png = path_png,validate = T,rsp = lau2_rsp,rsp_class = "t")

# RGB2 c75
rgb2175 <-Dawn2(FFS=T, Tpoints = lau2_tP1,buf_size = 0.75,design = "ROUND",Stk = lau_FFS2,Stk_name = "FFS2_tp1",plot_res = T,save_png = T,save_res = F,path_png = path_png,validate = T,rsp = lau2_rsp,rsp_class = "t")
rgb2275 <-Dawn2(FFS=T, Tpoints = lau2_tP2,buf_size = 0.75,design = "ROUND",Stk = lau_FFS2,Stk_name = "FFS2_tp2",plot_res = T,save_png = T,save_res = F,path_png = path_png,validate = T,rsp = lau2_rsp,rsp_class = "t")

# RGB2 c90
rgb2190 <-Dawn2(FFS=T, Tpoints = lau2_tP1,buf_size = 0.90,design = "ROUND",Stk = lau_FFS2,Stk_name = "FFS2_tp1",plot_res = T,save_png = T,save_res = F,path_png = path_png,validate = T,rsp = lau2_rsp,rsp_class = "t")
rgb2290 <-Dawn2(FFS=T, Tpoints = lau2_tP2,buf_size = 0.90,design = "ROUND",Stk = lau_FFS2,Stk_name = "FFS2_tp2",plot_res = T,save_png = T,save_res = F,path_png = path_png,validate = T,rsp = lau2_rsp,rsp_class = "t")


# get list for all values
ls <- list(rgb2115$VALUES,rgb2215$VALUES,
           rgb2130$VALUES,rgb2230$VALUES,
           rgb2145$VALUES,rgb2245$VALUES,
           rgb2160$VALUES,rgb2260$VALUES,
           rgb2175$VALUES,rgb2275$VALUES,
           rgb2190$VALUES,rgb2290$VALUES
)


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

write.csv(results,file.path(path,"results_FFS2.csv"))

# selected variables
rgb2115$model_LLOCV$selectedvars
rgb2215$model_LLOCV$selectedvars
rgb2130$model_LLOCV$selectedvars
rgb2230$model_LLOCV$selectedvars
rgb2145$model_LLOCV$selectedvars
rgb2245$model_LLOCV$selectedvars
rgb2160$model_LLOCV$selectedvars
rgb2260$model_LLOCV$selectedvars
rgb2175$model_LLOCV$selectedvars
rgb2275$model_LLOCV$selectedvars
rgb2190$model_LLOCV$selectedvars
rgb2290$model_LLOCV$selectedvars