### Data processing

# setup set working directory
getwd()
path <-"C:/Envimaster/AOA-Impact" # set drive letter for stick
setwd(path)

# load libs
require(raster)
require(RStoolbox)
# load LEGION functions from separate script
source(file.path(path,"R/001_FUN_LEGION.R"))


# load org data
lau_rgb1  <- raster::stack(file.path(path,"Data/Area1/lau_RGB1.grd") )
lau_rgb2 <- raster::stack(file.path(path,"Data/Area2/lau_RGB2.grd") )
lau_rgb3 <- raster::stack(file.path(path,"Data/Area3/lau_RGB3.grd") )

# plot
plotRGB(lau_rgb1,1,2,3)
plotRGB(lau_rgb2,1,2,3)
plotRGB(lau_rgb3,1,2,3)

# workflow for AREA 1

# set variable
lau_rgb <- lau_rgb1

# calculate 1st PCA for  RGB
pca_rgb = RStoolbox::rasterPCA(lau_rgb,maskCheck = T, spca = T, nComp = 1)

# filter PCA, all filter, sizes 3 and 5
filt <- filter_Stk(pca_rgb$map,sizes=c(3,5))

# compute all vegetation indices for RGB
index_rgb <-vegInd_RGB(lau_rgb,1,2,3)

# check for INF
is.infinite(index_rgb)

# Stack all layer
full_stk <- raster::stack(index_rgb,lau_rgb,filt,pca_rgb)

nlayers(full_stk)


# cor test
corT_full <- detct_RstCor(full_stk,0.9)
# homogeneity test
hmgy_full <-detct_RstHmgy (corT_full,THvalue=0.9,valueRange=0.1)
# save Raster
writeRaster(hmgy_full,"Data/Area1/FFS1_Stack.grd", format="raster",overwrite=T)


         # workflow for AREA 2
         
         # set variable
         lau_rgb <- lau_rgb2
         
         # calculate 1st PCA for  RGB
         pca_rgb = RStoolbox::rasterPCA(lau_rgb,maskCheck = T, spca = T, nComp = 1)
         
         # filter PCA, all filter, sizes 3 and 5
         filt <- filter_Stk(pca_rgb$map,sizes=c(3,5))
         
         # compute all vegetation indices for RGB
         index_rgb <-vegInd_RGB(lau_rgb,1,2,3)
         
         # check for INF
         is.infinite(index_rgb)
         
         # Stack all layer
         full_stk <- raster::stack(index_rgb,lau_rgb,filt,pca_rgb)
         
         nlayers(full_stk)
         
         
         # cor test
         corT_full <- detct_RstCor(full_stk,0.9)
         # homogeneity test
         hmgy_full <-detct_RstHmgy (corT_full,THvalue=0.9,valueRange=0.1)
         # save Raster
         writeRaster(hmgy_full,"Data/Area2/FFS2_Stack.grd", format="raster",overwrite=T)
         
                  # workflow for AREA 3
                  
                  # set variable
                  lau_rgb <- lau_rgb3
                  
                  # calculate 1st PCA for  RGB
                  pca_rgb = RStoolbox::rasterPCA(lau_rgb,maskCheck = T, spca = T, nComp = 1)
                  
                  # filter PCA, all filter, sizes 3 and 5
                  filt <- filter_Stk(pca_rgb$map,sizes=c(3,5))
                  
                  # compute all vegetation indices for RGB
                  index_rgb <-vegInd_RGB(lau_rgb,1,2,3)
                  
                  # check for INF
                  is.infinite(index_rgb)
                  
                  # Stack all layer
                  full_stk <- raster::stack(index_rgb,lau_rgb,filt,pca_rgb)
                  
                  nlayers(full_stk)
                  
                  
                  # cor test
                  corT_full <- detct_RstCor(full_stk,0.9)
                  # homogeneity test
                  hmgy_full <-detct_RstHmgy (corT_full,THvalue=0.9,valueRange=0.1)
                  # save Raster
                  writeRaster(hmgy_full,"Data/Area3/FFS3_Stack.grd", format="raster",overwrite=T)
               