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
lau_FFS1 <- raster::stack(file.path(path,"Data/Area1/FFS1_Stack.grd") )

# plot
plot(lau_FFS1[[1:3]])


# response layer
lau_rsp <-rgdal::readOGR(file.path(path,"DATA/Area1/area1_response.shp"))
# handle CRS string
crs(lau_rsp) <- crs(lau_FFS1)


# load Training Points
lau_tP1 <-rgdal::readOGR(file.path(path,"DATA/Area1/lau_Tpoints.shp"))
lau_tP2 <-rgdal::readOGR(file.path(path,"DATA/Area1/lau_Tpoints2.shp"))

# handle CRS string
crs(lau_tP1) <- crs(lau_FFS1)
crs(lau_tP2) <- crs(lau_FFS1)

# path to save resulting pngs
path_png <- file.path(path,"result_IMG1//")

# RGB c15
rgb115 <-Dawn2(FFS=T, Tpoints = lau_tP1,buf_size = 0.15,design = "ROUND",Stk = lau_FFS1,Stk_name = "FFS1_tp1",plot_res = T,save_png = F,save_res = F,path_png = path_png,validate = T,rsp = lau_rsp,rsp_class = "t")
rgb215 <-Dawn2(FFS=T, Tpoints = lau_tP2,buf_size = 0.15,design = "ROUND",Stk = lau_FFS1,Stk_name = "FFS1_tp2",plot_res = T,save_png = T,save_res = F,path_png = path_png,validate = T,rsp = lau_rsp,rsp_class = "t")

# RGB c30
rgb130 <-Dawn2(FFS=T, Tpoints = lau_tP1,buf_size = 0.30,design = "ROUND",Stk = lau_FFS1,Stk_name = "FFS1_tp1",plot_res = T,save_png = T,save_res = F,path_png = path_png,validate = T,rsp = lau_rsp,rsp_class = "t")
rgb230 <-Dawn2(FFS=T, Tpoints = lau_tP2,buf_size = 0.30,design = "ROUND",Stk = lau_FFS1,Stk_name = "FFS1_tp2",plot_res = T,save_png = T,save_res = F,path_png = path_png,validate = T,rsp = lau_rsp,rsp_class = "t")

# RGB c45
rgb145 <-Dawn2(FFS=T, Tpoints = lau_tP1,buf_size = 0.45,design = "ROUND",Stk = lau_FFS1,Stk_name = "FFS1_tp1",plot_res = T,save_png = T,save_res = F,path_png = path_png,validate = T,rsp = lau_rsp,rsp_class = "t")
rgb245 <-Dawn2(FFS=T, Tpoints = lau_tP2,buf_size = 0.45,design = "ROUND",Stk = lau_FFS1,Stk_name = "FFS1_tp2",plot_res = T,save_png = T,save_res = F,path_png = path_png,validate = T,rsp = lau_rsp,rsp_class = "t")

# RGB c60
rgb160 <-Dawn2(FFS=T, Tpoints = lau_tP1,buf_size = 0.60,design = "ROUND",Stk = lau_FFS1,Stk_name = "FFS1_tp1",plot_res = T,save_png = T,save_res = F,path_png = path_png,validate = T,rsp = lau_rsp,rsp_class = "t")
rgb260 <-Dawn2(FFS=T, Tpoints = lau_tP2,buf_size = 0.60,design = "ROUND",Stk = lau_FFS1,Stk_name = "FFS1_tp2",plot_res = T,save_png = T,save_res = F,path_png = path_png,validate = T,rsp = lau_rsp,rsp_class = "t")

# RGB c75
rgb175 <-Dawn2(FFS=T, Tpoints = lau_tP1,buf_size = 0.75,design = "ROUND",Stk = lau_FFS1,Stk_name = "FFS1_tp1",plot_res = T,save_png = T,save_res = F,path_png = path_png,validate = T,rsp = lau_rsp,rsp_class = "t")
rgb275 <-Dawn2(FFS=T, Tpoints = lau_tP2,buf_size = 0.75,design = "ROUND",Stk = lau_FFS1,Stk_name = "FFS1_tp2",plot_res = T,save_png = T,save_res = F,path_png = path_png,validate = T,rsp = lau_rsp,rsp_class = "t")

# RGB c90
rgb190 <-Dawn2(FFS=T, Tpoints = lau_tP1,buf_size = 0.90,design = "ROUND",Stk = lau_FFS1,Stk_name = "FFS1_tp1",plot_res = T,save_png = T,save_res = F,path_png = path_png,validate = T,rsp = lau_rsp,rsp_class = "t")
rgb290 <-Dawn2(FFS=T, Tpoints = lau_tP2,buf_size = 0.90,design = "ROUND",Stk = lau_FFS1,Stk_name = "FFS1_tp2",plot_res = T,save_png = T,save_res = F,path_png = path_png,validate = T,rsp = lau_rsp,rsp_class = "t")



# get list for all values
ls <- list(rgb115$VALUES,rgb215$VALUES,
           rgb130$VALUES,rgb230$VALUES,
           rgb145$VALUES,rgb245$VALUES,
           rgb160$VALUES,rgb260$VALUES,
           rgb175$VALUES,rgb275$VALUES,
           rgb190$VALUES,rgb290$VALUES
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

write.csv(results,file.path(path,"results_FFS1.csv"))

# selected variables
rgb115$model_LLOCV$selectedvars
rgb215$model_LLOCV$selectedvars
rgb130$model_LLOCV$selectedvars
rgb230$model_LLOCV$selectedvars
rgb145$model_LLOCV$selectedvars
rgb245$model_LLOCV$selectedvars
rgb160$model_LLOCV$selectedvars
rgb260$model_LLOCV$selectedvars
rgb175$model_LLOCV$selectedvars
rgb275$model_LLOCV$selectedvars
rgb190$model_LLOCV$selectedvars
rgb290$model_LLOCV$selectedvars
