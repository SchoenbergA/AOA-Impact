### Data processing

# setup set working directory
getwd()
path <-"C:/Envimaster/MSc_Thesis" # set drive letter for stick
setwd(path)

### 1. Compute Predictor Stacks ##############################################################################

# load libs
require(raster)
# load LEGION functions from separate script
source("R/FUN_LEGION.R")


# org data
lau_rgb <- raster::stack(file.path(path,"Data/lau_RGB.grd") )

plotRGB(lau_rgb,3,2,1)

### compute 'full_stk' with all filter in 3, 5, 7 and 9 for RGB and all indices + RGB and indices (unfiltered)


# compute all vegetation indices for RGB
index_rgb <-vegInd_RGB(lau_rgb,3,2,1)

# combine single rgb bands and indices
rgbandindex <- raster::stack(index_rgb,lau_rgb[[1:3]])

# compute all provided filter on all indices and RGB
filt <- filter_Stk(rgbandindex,sizes=c(3,5,7,9))

# combine all filter ALS with the unfiltered indices and RGB
full_stk <- raster::stack(rgbandindex,filt)
nlayers(full_stk)

# save full Stk
writeRaster(full_stk,"full_stk.grd", format="raster")




      ### compute 'small_stk' equal to 'full_stk' but only filter sizes of 3 and 5
      index_rgb2 <-vegInd_RGB(lau_rgb,3,2,1)

      # combine single rgb bands and indices
      rgbandindex2 <- raster::stack(index_rgb2,lau_rgb[[1:3]])

      # compute all provided filter on all ALs
      filt2 <- filter_Stk(rgbandindex2,sizes=c(3,5))

      # combine all filter ALS with the unfiltered indices and RGB
      small_stk <- raster::stack(rgbandindex2,filt2)

      # save small_stk
      writeRaster(small_stk,"small_stk.grd", format="raster")



            ### compute 'selected_stk' with selected filter and indices

            # compute selected indices
            index_rgb3 <-vegInd_RGB(lau_rgb,3,2,1,indlist = c("VVI","NDTI","GLI","NGRDI"))

            # combine single rgb bands and indices
            rgbandindex3 <- raster::stack(index_rgb3,lau_rgb[[1:3]])

            # compute selected filter with sizes of 3, 5, 7 and 9
            filt3 <- filter_Stk(rgbandindex3,fLS = c("sum","mean","modal","sobel","sobel_hrzt","sobel_vert"),sizes=c(3,5,7,9))

            ### combine all filter ALS with the unfiltered indices and RGB
            selected_stk <- raster::stack(rgbandindex3,filt3)

            # save selected_stk
            writeRaster(selected_stk,"selected_stk.grd", format="raster")

### 2. Dimensional Reduction (corT and hmgy) ####################################################################

###'full_stk' cor -> hmgy
# cor test
corT_full <- detct_RstCor(full_stk,0.7)
# homogeneity test
hmgy_full <-detct_RstHmgy (corT_full,THvalue=0.9,valueRange=0.1)
# save Raster
writeRaster(hmgy_full,"hmgy_full.grd", format="raster")

         ### 'small_stk' cor -> hmgy
         # cor test
         corT_small <- detct_RstCor(small_stk,0.7)
         # homogeneity test
         hmgy_small <-detct_RstHmgy (corT_small,THvalue=0.9,valueRange=0.1)
         # write raster
         writeRaster(hmgy_small,"hmgy_small.grd", format="raster")

                  ### 'selected_stk' cor -> hmgy
                  # cor test
                  corT_selected <- detct_RstCor(selected_stk,0.7)
                  # homogeneity test
                  hmgy_selected <-detct_RstHmgy (corT_selected,THvalue=0.9,valueRange=0.1)
                  # save raster
                  writeRaster(hmgy_selected,"hmgy_selected.grd", format="raster")

                            ### 'full_stk' hmgy -> cor
                            # homogeneity test
                            hmgy2_full <-detct_RstHmgy (full_stk,THvalue=0.9,valueRange=0.1)
                            # cor test
                            corT2_full <- detct_RstCor(hmgy2_full,0.7)
                            writeRaster(corT2_full,"corT2_full.grd", format="raster")

### 3. compute PCA Stacks ########################################################################################

### simple RGB PCA
pca_rgb = RStoolbox::rasterPCA(lau_rgb,maskCheck = T, spca = T, nComp = 3)
# pca to stk
stk_pca_rgb <-pca_rgb$map # stk = pca (rgb only)
writeRaster(stk_pca_rgb,"DATA/PCA_stk/pca_rgb.grd", format="raster")

# add single bands to increase weight for RGB
stk_pca_rgb2<- raster::stack(stk_pca_rgb,lau_rgb) # stk = pca (rgb only) + single band rgb
writeRaster(stk_pca_rgb2,"DATA/PCA_stk/pca_rgb2.grd", format="raster")


         ### PCA with INDEX ###

         ### Index + RGB PCA
         # all indices + single band RGB for PCA
         # build new Stack
         ind <-vegInd_RGB(lau_rgb,3,2,1,indlist = c("VVI","VARI","NDTI","CI","BI","SI","TGI","GLI","NGRDI"))
         # HI and RI lead to INF values which raster PCA cannot handle. incidence to drop both from LEGION because often problems occure.
         ind_stk <- raster::stack(lau_rgb,ind)
         # pca
         pca_ind = RStoolbox::rasterPCA(ind_stk,maskCheck = T, spca = T, nComp = 3)
         # pca to Stack
         stk_pca_ind <- pca_ind$map # stk = pca (indices and rgb)
         writeRaster(stk_pca_ind,"DATA/PCA_stk/pca_indrgb.grd", format="raster")

         ### Index only PCA ####
         pca_ind_only = RStoolbox::rasterPCA(ind,maskCheck = T, spca = T, nComp = 3)
         # pca to stk
         stk_pca_ind_only <- pca_ind_only$map # stk = pca (indices only)
         writeRaster(stk_pca_ind_only,"DATA/PCA_stk/pca_ind.grd", format="raster")

         ### Index only PCA + RGB in Stack ####
         stk_pca_ind_rgb <- raster::stack(stk_pca_ind_only,lau_rgb) # stk = pca (indices only) +RGB
         writeRaster(stk_pca_ind_rgb,"DATA/PCA_stk/pca_ind_RGB.grd", format="raster")


                  # filter on RGB
                  rgb_filt <- filter_Stk(lau_rgb,sizes = c(3,5,7,9))
                  # combine rgb filter with org rgb bands
                  stk_rgb <- raster::stack(rgb_filt,lau_rgb)
                  # pca
                  pca_rgb_filt = RStoolbox::rasterPCA(stk_rgb,maskCheck = T, spca = T, nComp = 3)
                  # pca to stack
                  stk_pca_rgb_filt <- pca_rgb_filt$map
                  writeRaster(stk_pca_rgb_filt,"DATA/PCA_stk/pca_rgb_filt.grd", format="raster")

                  #### Filter Index and RGB PCA ####
                  ## indices, filter indices + rgb +org indices###
                  #
                  ## filter indices
                  #ind_filt <- filter_Stk(ind_stk,sizes = c(3,5,7,9))
                  ## combine filter and index filter and indices+rgb org
                  #ind_filt_stk <- raster::stack(ind_filt,ind_stk,stk_rgb)
                  ## pca
                  #pca_ind_filt = RStoolbox::rasterPCA(ind_filt_stk,maskCheck = T, spca = T, nComp = 3)
                  ## pca to stk
                  #stk_pca_ind_only <- pca_ind_filt$map #stk = pca (filter both ind and rgb, indices org, rgb)
                  #writeRaster(pca_ind_filt,"PCA_stk/pca_rgbind_filt.grd", format="raster")


### PCAs for the LEGION_Stks
hmgy_selected <- raster::stack(file.path("C:/Envimaster/MSc_Thesis/Data/LEGION_stk/hmgy_selected.grd"))
hmgy_small <- raster::stack(file.path("C:/Envimaster/MSc_Thesis/Data/LEGION_stk/hmgy_small.grd"))
hmgy_full <- raster::stack(file.path("C:/Envimaster/MSc_Thesis/Data/LEGION_stk/hmgy_full.grd"))
corT2_full <- raster::stack(file.path("C:/Envimaster/MSc_Thesis/Data/LEGION_stk/corT2_full.grd"))


pca_hmgy_full <- RStoolbox::rasterPCA(hmgy_full,maskCheck = T, spca = T, nComp = 3)
# PCA to Stk
stk_pca_hmgy_full <- pca_hmgy_full$map
# write
writeRaster(stk_pca_hmgy_full,"DATA/PCA_stk/pca_hmgy_full.grd", format="raster")

         pca_hmgy_selected <- RStoolbox::rasterPCA(hmgy_selected,maskCheck = T, spca = T, nComp = 3)
         # PCA to Stk
         stk_pca_hmgy_selected <- pca_hmgy_selected$map
         # write
         writeRaster(stk_pca_hmgy_selected,"DATA/PCA_stk/pca_hmgy_selected.grd", format="raster")

                  pca_hmgy_small <- RStoolbox::rasterPCA(hmgy_small,maskCheck = T, spca = T, nComp = 3)
                  # PCA to Stk
                  stk_pca_hmgy_small <- pca_hmgy_small$map
                  # write
                  writeRaster(stk_pca_hmgy_small,"DATA/PCA_stk/pca_hmgy_small.grd", format="raster")

                           pca_corT2_full <- RStoolbox::rasterPCA(corT2_full,maskCheck = T, spca = T, nComp = 3)
                           # PCA to Stk
                           stk_pca_corT2_full <- pca_corT2_full$map
                           # write
                           writeRaster(stk_pca_corT2_full,"DATA/PCA_stk/pca_corT2_full.grd", format="raster")

### 4. FFS Stacks

# compute RGB indices using LEGION
ind <-vegInd_RGB(lau_rgb,3,2,1,indlist = c("VVI","VARI","NDTI","CI","BI","SI","TGI","GLI","NGRDI"))
# NOTE: HI and RI lead to INF values which raster PCA cannot handle. incidence to drop both from LEGION because often problems occure.

# combine RGB and Indices
ind_stk <- raster::stack(lau_rgb,ind)

writeRaster(ind_stk,"DATA/FFS_stk/indRGB.grd", format="raster") # equal to pca_indrgb
writeRaster(ind,"DATA/FFS_stk/ind_only.grd", format="raster") # equal to pca_ind

         # filter
         rgb_filt <- filter_Stk(lau_rgb,sizes = c(3,5,7,9))
         nlayers(rgb_filt) # 108 seems to be too much for my laptop for FFS
         rgb_filtcor <- detct_RstCor(Stk = rgb_filt,THvalue = 0.9)
         nlayers(rgb_filtcor)# 18 looks better

         # combine rgb filter with org rgb bands
         stk_rgb <- raster::stack(rgb_filtcor,lau_rgb)
         writeRaster(stk_rgb,"DATA/FFS_stk/rgb_filt.grd", format="raster") # not equal to pca_rgb_filt but same idea

                  # rgb filter only to compare impact of single band rgb addition
                  writeRaster(rgb_filtcor,"DATA/FFS_stk/filtercor_only.grd", format="raster")
                  # all indices to see if HI or RI is selected by FFS
                  ind_all <-vegInd_RGB(lau_rgb,3,2,1)
                  writeRaster(ind_all,"DATA/FFS_stk/ind_all.grd", format="raster")

                           # at least to test how PCAs are selected combine a PCA with the original Layers it was computed from

                           pca_filt = RStoolbox::rasterPCA(stk_rgb,maskCheck = T, spca = T, nComp = 3)
                           map_pca <- pca_filt$map
                           stk_pca <- stack(map_pca,stk_rgb)
                           writeRaster(stk_pca,"DATA/FFS_stk/stk_pca.grd", format="raster")
# end of script

