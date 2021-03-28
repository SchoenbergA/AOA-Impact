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
lau_FFS3 <- raster::stack(file.path(path,"Data/Area3/FFS3_Stack.grd") )
# plot
plot(lau_FFS3[[1:3]])

# response layer
lau3_rsp <-rgdal::readOGR(file.path(path,"DATA/Area3/area3_response.shp"))
# handle CRS string
crs(lau3_rsp) <- crs(lau_FFS3)

# load Training Points
lau3_tP1 <-rgdal::readOGR(file.path(path,"DATA/Area3/lau3_Tpoints.shp"))
lau3_tP2 <-rgdal::readOGR(file.path(path,"DATA/Area3/lau3_Tpoints2.shp"))
# handle CRS string
crs(lau3_tP1) <- crs(lau_FFS3)
crs(lau3_tP2) <- crs(lau_FFS3)

# path to save resulting pngs
path_png <- file.path(path,"result_IMG3//")


# RGB3 c15
rgb3115 <-Dawn2(FFS=T, Tpoints = lau3_tP1,buf_size = 0.15,design = "ROUND",Stk = lau_FFS3,Stk_name = "FFS3_tp1",plot_res = T,save_png = T,save_res = F,path_png = path_png,validate = T,rsp = lau3_rsp,rsp_class = "t")
rgb3215 <-Dawn2(FFS=T, Tpoints = lau3_tP2,buf_size = 0.15,design = "ROUND",Stk = lau_FFS3,Stk_name = "FFS3_tp2",plot_res = T,save_png = T,save_res = F,path_png = path_png,validate = T,rsp = lau3_rsp,rsp_class = "t")

# RGB3 c30
rgb3130 <-Dawn2(FFS=T, Tpoints = lau3_tP1,buf_size = 0.30,design = "ROUND",Stk = lau_FFS3,Stk_name = "FFS3_tp1",plot_res = T,save_png = T,save_res = F,path_png = path_png,validate = T,rsp = lau3_rsp,rsp_class = "t")
rgb3230 <-Dawn2(FFS=T, Tpoints = lau3_tP2,buf_size = 0.30,design = "ROUND",Stk = lau_FFS3,Stk_name = "FFS3_tp2",plot_res = T,save_png = T,save_res = F,path_png = path_png,validate = T,rsp = lau3_rsp,rsp_class = "t")

# RGB3 c45
rgb3145 <-Dawn2(FFS=T, Tpoints = lau3_tP1,buf_size = 0.45,design = "ROUND",Stk = lau_FFS3,Stk_name = "FFS3_tp1",plot_res = T,save_png = T,save_res = F,path_png = path_png,validate = T,rsp = lau3_rsp,rsp_class = "t")
rgb3245 <-Dawn2(FFS=F, Tpoints = lau3_tP2,buf_size = 0.45,design = "ROUND",Stk = lau_FFS3,Stk_name = "FFS3_tp2",plot_res = T,save_png = T,save_res = F,path_png = path_png,validate = T,rsp = lau3_rsp,rsp_class = "t")

# RGB3 c60
rgb3160 <-Dawn2(FFS=T, Tpoints = lau3_tP1,buf_size = 0.60,design = "ROUND",Stk = lau_FFS3,Stk_name = "FFS3_tp1",plot_res = T,save_png = T,save_res = F,path_png = path_png,validate = T,rsp = lau3_rsp,rsp_class = "t")
rgb3260 <-Dawn2(FFS=T, Tpoints = lau3_tP2,buf_size = 0.60,design = "ROUND",Stk = lau_FFS3,Stk_name = "FFS3_tp2",plot_res = T,save_png = T,save_res = F,path_png = path_png,validate = T,rsp = lau3_rsp,rsp_class = "t")

# RGB3 c75
rgb3175 <-Dawn2(FFS=T, Tpoints = lau3_tP1,buf_size = 0.75,design = "ROUND",Stk = lau_FFS3,Stk_name = "FFS3_tp1",plot_res = T,save_png = T,save_res = F,path_png = path_png,validate = T,rsp = lau3_rsp,rsp_class = "t")
rgb3275 <-Dawn2(FFS=T, Tpoints = lau3_tP2,buf_size = 0.75,design = "ROUND",Stk = lau_FFS3,Stk_name = "FFS3_tp2",plot_res = T,save_png = T,save_res = F,path_png = path_png,validate = T,rsp = lau3_rsp,rsp_class = "t")

# RGB3 c90
rgb3190 <-Dawn2(FFS=T, Tpoints = lau3_tP1,buf_size = 0.90,design = "ROUND",Stk = lau_FFS3,Stk_name = "FFS3_tp1",plot_res = T,save_png = T,save_res = F,path_png = path_png,validate = T,rsp = lau3_rsp,rsp_class = "t")
rgb3290 <-Dawn2(FFS=F, Tpoints = lau3_tP2,buf_size = 0.90,design = "ROUND",Stk = lau_FFS3,Stk_name = "FFS3_tp2",plot_res = T,save_png = T,save_res = F,path_png = path_png,validate = T,rsp = lau3_rsp,rsp_class = "t")


# get list for all values
ls <- list(rgb3115$VALUES,rgb3215$VALUES,
           rgb3130$VALUES,rgb3230$VALUES,
           rgb3145$VALUES,rgb3245$VALUES,
           rgb3160$VALUES,rgb3260$VALUES,
           rgb3175$VALUES,rgb3275$VALUES,
           rgb3190$VALUES,rgb3290$VALUES
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

write.csv(results,file.path(path,"results_FFS3.csv"))

# selected variables
rgb3115$model_LLOCV$selectedvars
rgb3215$model_LLOCV$selectedvars
rgb3130$model_LLOCV$selectedvars
rgb3230$model_LLOCV$selectedvars
rgb3145$model_LLOCV$selectedvars
rgb3245$model_LLOCV$selectedvars
rgb3160$model_LLOCV$selectedvars
rgb3260$model_LLOCV$selectedvars
rgb3175$model_LLOCV$selectedvars
rgb3275$model_LLOCV$selectedvars
rgb3190$model_LLOCV$selectedvars
rgb3290$model_LLOCV$selectedvars