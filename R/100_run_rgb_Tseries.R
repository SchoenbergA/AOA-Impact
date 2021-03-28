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
lau_rgb <- raster::stack(file.path(path,"Data/Area1/lau_RGB1.grd") )
lau_rgb2 <- raster::stack(file.path(path,"Data/Area2/lau_RGB2.grd") )
lau_rgb3 <- raster::stack(file.path(path,"Data/Area3/lau_RGB3.grd") )

# plot
plotRGB(lau_rgb,1,2,3)
plotRGB(lau_rgb2,1,2,3)
plotRGB(lau_rgb3,1,2,3)

# load Training Points
lau_tP1 <-rgdal::readOGR(file.path(path,"Data/Area1/lau_Tpoints.shp"))
lau_tP2 <-rgdal::readOGR(file.path(path,"Data/Area1/lau_Tpoints2.shp"))

lau2_tP1 <-rgdal::readOGR(file.path(path,"Data/Area2/lau2_Tpoints.shp"))
lau2_tP2 <-rgdal::readOGR(file.path(path,"Data/Area2/lau2_Tpoints2.shp"))

lau3_tP1 <-rgdal::readOGR(file.path(path,"Data/Area3/lau3_Tpoints.shp"))
lau3_tP2 <-rgdal::readOGR(file.path(path,"Data/Area3/lau3_Tpoints2.shp"))

# handle CRS string
crs(lau_tP1) <- crs(lau_rgb)
crs(lau2_tP1) <- crs(lau_rgb)
crs(lau3_tP1) <- crs(lau_rgb)
crs(lau_tP2) <- crs(lau_rgb)
crs(lau2_tP2) <- crs(lau_rgb)
crs(lau3_tP2) <- crs(lau_rgb)

# load response layer
lau_rsp <-rgdal::readOGR(file.path(path,"Data/Area1/area1_response.shp"))
lau2_rsp <-rgdal::readOGR(file.path(path,"Data/Area2/area2_response.shp"))
lau3_rsp <-rgdal::readOGR(file.path(path,"Data/Area3/area3_response.shp"))
# handle CRS string
crs(lau_rsp) <- crs(lau_rgb)
crs(lau2_rsp) <- crs(lau_rgb)
crs(lau3_rsp) <- crs(lau_rgb)

# path to save resulting pngs
path_png <- file.path(path,"result_IMG//")

### AREA1

# RGB c15
rgb115 <-Dawn2(FFS=F, Tpoints = lau_tP1,buf_size = 0.15,design = "ROUND",Stk = lau_rgb,Stk_name = "rgb_tp1",plot_res = T,save_png=T,save_res = F,path_png = path_png,validate = T,rsp = lau_rsp,rsp_class = "t")
rgb215 <-Dawn2(FFS=F, Tpoints = lau_tP2,buf_size = 0.15,design = "ROUND",Stk = lau_rgb,Stk_name = "rgb_tp2",plot_res = T,save_png=T,save_res = F,path_png = path_png,validate = T,rsp = lau_rsp,rsp_class = "t")

# RGB c30
rgb130 <-Dawn2(FFS=F, Tpoints = lau_tP1,buf_size = 0.30,design = "ROUND",Stk = lau_rgb,Stk_name = "rgb_tp1",plot_res = T,save_png=T,save_res = F,path_png = path_png,validate = T,rsp = lau_rsp,rsp_class = "t")
rgb230 <-Dawn2(FFS=F, Tpoints = lau_tP2,buf_size = 0.30,design = "ROUND",Stk = lau_rgb,Stk_name = "rgb_tp2",plot_res = T,save_png=T,save_res = F,path_png = path_png,validate = T,rsp = lau_rsp,rsp_class = "t")

# RGB c45
rgb145 <-Dawn2(FFS=F, Tpoints = lau_tP1,buf_size = 0.45,design = "ROUND",Stk = lau_rgb,Stk_name = "rgb_tp1",plot_res = T,save_png=T,save_res = F,path_png = path_png,validate = T,rsp = lau_rsp,rsp_class = "t")
rgb245 <-Dawn2(FFS=F, Tpoints = lau_tP2,buf_size = 0.45,design = "ROUND",Stk = lau_rgb,Stk_name = "rgb_tp2",plot_res = T,save_png=T,save_res = F,path_png = path_png,validate = T,rsp = lau_rsp,rsp_class = "t")

# RGB c60
rgb160 <-Dawn2(FFS=F, Tpoints = lau_tP1,buf_size = 0.60,design = "ROUND",Stk = lau_rgb,Stk_name = "rgb_tp1",plot_res = T,save_png=T,save_res = F,path_png = path_png,validate = T,rsp = lau_rsp,rsp_class = "t")
rgb260 <-Dawn2(FFS=F, Tpoints = lau_tP2,buf_size = 0.60,design = "ROUND",Stk = lau_rgb,Stk_name = "rgb_tp2",plot_res = T,save_png=T,save_res = F,path_png = path_png,validate = T,rsp = lau_rsp,rsp_class = "t")

# RGB c75
rgb175 <-Dawn2(FFS=F, Tpoints = lau_tP1,buf_size = 0.75,design = "ROUND",Stk = lau_rgb,Stk_name = "rgb_tp1",plot_res = T,save_png=T,save_res = F,path_png = path_png,validate = T,rsp = lau_rsp,rsp_class = "t")
rgb275 <-Dawn2(FFS=F, Tpoints = lau_tP2,buf_size = 0.75,design = "ROUND",Stk = lau_rgb,Stk_name = "rgb_tp2",plot_res = T,save_png=T,save_res = F,path_png = path_png,validate = T,rsp = lau_rsp,rsp_class = "t")

# RGB c90
rgb190 <-Dawn2(FFS=F, Tpoints = lau_tP1,buf_size = 0.90,design = "ROUND",Stk = lau_rgb,Stk_name = "rgb_tp1",plot_res = T,save_png=T,save_res = F,path_png = path_png,validate = T,rsp = lau_rsp,rsp_class = "t")
rgb290 <-Dawn2(FFS=F, Tpoints = lau_tP2,buf_size = 0.90,design = "ROUND",Stk = lau_rgb,Stk_name = "rgb_tp2",plot_res = T,save_png=T,save_res = F,path_png = path_png,validate = T,rsp = lau_rsp,rsp_class = "t")

### AREA2

# RGB c15
rgb2115 <-Dawn2(FFS=F, Tpoints = lau2_tP1,buf_size = 0.15,design = "ROUND",Stk = lau_rgb2,Stk_name = "rgb2_tp1",plot_res = T,save_png=T,save_res = F,path_png = path_png,validate = T,rsp = lau2_rsp,rsp_class = "t")
rgb2215 <-Dawn2(FFS=F, Tpoints = lau2_tP2,buf_size = 0.15,design = "ROUND",Stk = lau_rgb2,Stk_name = "rgb2_tp2",plot_res = T,save_png=T,save_res = F,path_png = path_png,validate = T,rsp = lau2_rsp,rsp_class = "t")

# rgb2 c30
rgb2130 <-Dawn2(FFS=F, Tpoints = lau2_tP1,buf_size = 0.30,design = "ROUND",Stk = lau_rgb2,Stk_name = "rgb2_tp1",plot_res = T,save_png=T,save_res = F,path_png = path_png,validate = T,rsp = lau2_rsp,rsp_class = "t")
rgb2230 <-Dawn2(FFS=F, Tpoints = lau2_tP2,buf_size = 0.30,design = "ROUND",Stk = lau_rgb2,Stk_name = "rgb2_tp2",plot_res = T,save_png=T,save_res = F,path_png = path_png,validate = T,rsp = lau2_rsp,rsp_class = "t")

# rgb2 c45
rgb2145 <-Dawn2(FFS=F, Tpoints = lau2_tP1,buf_size = 0.45,design = "ROUND",Stk = lau_rgb2,Stk_name = "rgb2_tp1",plot_res = T,save_png=T,save_res = F,path_png = path_png,validate = T,rsp = lau2_rsp,rsp_class = "t")
rgb2245 <-Dawn2(FFS=F, Tpoints = lau2_tP2,buf_size = 0.45,design = "ROUND",Stk = lau_rgb2,Stk_name = "rgb2_tp2",plot_res = T,save_png=T,save_res = F,path_png = path_png,validate = T,rsp = lau2_rsp,rsp_class = "t")

# rgb2 c60
rgb2160 <-Dawn2(FFS=F, Tpoints = lau2_tP1,buf_size = 0.60,design = "ROUND",Stk = lau_rgb2,Stk_name = "rgb2_tp1",plot_res = T,save_png=T,save_res = F,path_png = path_png,validate = T,rsp = lau2_rsp,rsp_class = "t")
rgb2260 <-Dawn2(FFS=F, Tpoints = lau2_tP2,buf_size = 0.60,design = "ROUND",Stk = lau_rgb2,Stk_name = "rgb2_tp2",plot_res = T,save_png=T,save_res = F,path_png = path_png,validate = T,rsp = lau2_rsp,rsp_class = "t")

# rgb2 c75
rgb2175 <-Dawn2(FFS=F, Tpoints = lau2_tP1,buf_size = 0.75,design = "ROUND",Stk = lau_rgb2,Stk_name = "rgb2_tp1",plot_res = T,save_png=T,save_res = F,path_png = path_png,validate = T,rsp = lau2_rsp,rsp_class = "t")
rgb2275 <-Dawn2(FFS=F, Tpoints = lau2_tP2,buf_size = 0.75,design = "ROUND",Stk = lau_rgb2,Stk_name = "rgb2_tp2",plot_res = T,save_png=T,save_res = F,path_png = path_png,validate = T,rsp = lau2_rsp,rsp_class = "t")

# rgb2 c90
rgb2190 <-Dawn2(FFS=F, Tpoints = lau2_tP1,buf_size = 0.90,design = "ROUND",Stk = lau_rgb2,Stk_name = "rgb2_tp1",plot_res = T,save_png=T,save_res = F,path_png = path_png,validate = T,rsp = lau2_rsp,rsp_class = "t")
rgb2290 <-Dawn2(FFS=F, Tpoints = lau2_tP2,buf_size = 0.90,design = "ROUND",Stk = lau_rgb2,Stk_name = "rgb2_tp2",plot_res = T,save_png=T,save_res = F,path_png = path_png,validate = T,rsp = lau2_rsp,rsp_class = "t")


### AREA3

# rgb3 c15
rgb3115 <-Dawn2(FFS=F, Tpoints = lau3_tP1,buf_size = 0.15,design = "ROUND",Stk = lau_rgb3,Stk_name = "rgb3_tp1",plot_res = T,save_png=T,save_res = F,path_png = path_png,validate = T,rsp = lau3_rsp,rsp_class = "t")
rgb3215 <-Dawn2(FFS=F, Tpoints = lau3_tP2,buf_size = 0.15,design = "ROUND",Stk = lau_rgb3,Stk_name = "rgb3_tp2",plot_res = T,save_png=T,save_res = F,path_png = path_png,validate = T,rsp = lau3_rsp,rsp_class = "t")

# rgb3 c30
rgb3130 <-Dawn2(FFS=F, Tpoints = lau3_tP1,buf_size = 0.30,design = "ROUND",Stk = lau_rgb3,Stk_name = "rgb3_tp1",plot_res = T,save_png=T,save_res = F,path_png = path_png,validate = T,rsp = lau3_rsp,rsp_class = "t")
rgb3230 <-Dawn2(FFS=F, Tpoints = lau3_tP2,buf_size = 0.30,design = "ROUND",Stk = lau_rgb3,Stk_name = "rgb3_tp2",plot_res = T,save_png=T,save_res = F,path_png = path_png,validate = T,rsp = lau3_rsp,rsp_class = "t")

# rgb3 c45
rgb3145 <-Dawn2(FFS=F, Tpoints = lau3_tP1,buf_size = 0.45,design = "ROUND",Stk = lau_rgb3,Stk_name = "rgb3_tp1",plot_res = T,save_png=T,save_res = F,path_png = path_png,validate = T,rsp = lau3_rsp,rsp_class = "t")
rgb3245 <-Dawn2(FFS=F, Tpoints = lau3_tP2,buf_size = 0.45,design = "ROUND",Stk = lau_rgb3,Stk_name = "rgb3_tp2",plot_res = T,save_png=T,save_res = F,path_png = path_png,validate = T,rsp = lau3_rsp,rsp_class = "t")

# rgb3 c60
rgb3160 <-Dawn2(FFS=F, Tpoints = lau3_tP1,buf_size = 0.60,design = "ROUND",Stk = lau_rgb3,Stk_name = "rgb3_tp1",plot_res = T,save_png=T,save_res = F,path_png = path_png,validate = T,rsp = lau3_rsp,rsp_class = "t")
rgb3260 <-Dawn2(FFS=F, Tpoints = lau3_tP2,buf_size = 0.60,design = "ROUND",Stk = lau_rgb3,Stk_name = "rgb3_tp2",plot_res = T,save_png=T,save_res = F,path_png = path_png,validate = T,rsp = lau3_rsp,rsp_class = "t")

# rgb3 c75
rgb3175 <-Dawn2(FFS=F, Tpoints = lau3_tP1,buf_size = 0.75,design = "ROUND",Stk = lau_rgb3,Stk_name = "rgb3_tp1",plot_res = T,save_png=T,save_res = F,path_png = path_png,validate = T,rsp = lau3_rsp,rsp_class = "t")
rgb3275 <-Dawn2(FFS=F, Tpoints = lau3_tP2,buf_size = 0.75,design = "ROUND",Stk = lau_rgb3,Stk_name = "rgb3_tp2",plot_res = T,save_png=T,save_res = F,path_png = path_png,validate = T,rsp = lau3_rsp,rsp_class = "t")

# rgb3 c90
rgb3190 <-Dawn2(FFS=F, Tpoints = lau3_tP1,buf_size = 0.90,design = "ROUND",Stk = lau_rgb3,Stk_name = "rgb3_tp1",plot_res = T,save_png=T,save_res = F,path_png = path_png,validate = T,rsp = lau3_rsp,rsp_class = "t")
rgb3290 <-Dawn2(FFS=F, Tpoints = lau3_tP2,buf_size = 0.90,design = "ROUND",Stk = lau_rgb3,Stk_name = "rgb3_tp2",plot_res = T,save_png=T,save_res = F,path_png = path_png,validate = T,rsp = lau3_rsp,rsp_class = "t")




# get list for all values
ls <- list(rgb115$VALUES,rgb215$VALUES,
           rgb130$VALUES,rgb230$VALUES,
           rgb145$VALUES,rgb245$VALUES,
           rgb160$VALUES,rgb260$VALUES,
           rgb175$VALUES,rgb275$VALUES,
           rgb190$VALUES,rgb290$VALUES,
           
           rgb2115$VALUES,rgb2215$VALUES,
           rgb2130$VALUES,rgb2230$VALUES,
           rgb2145$VALUES,rgb2245$VALUES,
           rgb2160$VALUES,rgb2260$VALUES,
           rgb2175$VALUES,rgb2275$VALUES,
           rgb2190$VALUES,rgb2290$VALUES,
           
           rgb3115$VALUES,rgb3215$VALUES,
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

write.csv(results,file.path(path,"results_rgb.csv"))
