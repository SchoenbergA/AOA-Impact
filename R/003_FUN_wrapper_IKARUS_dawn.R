### Wrapper for IKARUS functions
# full IKARUS workflow in one cline of code
# addtionlly modifies input Training Points in size and form.

#' Classification and AOA with buffered TrainPoints
#' @description wrapper for IKARUS and AOA with buffered TrainPoints.
#' @param FFS boolean - if True uses a FFS. Default= FALSE
#' @param Tpoints SpatialPoints - PointLayer with the TrainingPositions
#' @param buf_size numeric - with for the buffer in meter
#' @param design character - from for the buffer. Choose from "ROUND","FLAT","SQUARE"
#' @param Stk RasterStack - with predictors for classfication
#' @param Stk_name character - name of input Stack for the name to write in images and output name.

#' @param plot_res boolean - set 'TRUE' to plot the prediction and AOA. Default = FALSE.

#' @param save_png boolean - set 'TRUE' to write the images in.png format for prediction and AOA. Default = FALSE.
#' @param fsize numeric - the desired front size for the main and sub text. default = 24.
#' @param save_res boolean - set 'TRUE' to write the prediction and AOA in .grd format. Default = FALSE.
#' @param path_res character - the path to save the resulting raster and models. Required if  "save_res" option is used.
#' @param path_png_cl character - the path to save the resulting plots for classification as png. Required if  "save_png" option is used.
#' @param path_png_aoa character - the path to save the resulting plots for AOA png. Required if  "save_png" option is used.

#' @return Returns the classification and the AOA.
#' @details
#' If 'plot_res' = TRUE will further plot the results.
#' If 'save_res' = TRUE will further return the Raster Layer to desired direction in .grd format.
#' If 'save_png' = TURE will write two .png images with prediction and respective AOA. 'fsize' can be used to modify the front sizes of the text (optimal for presentation purposes)
#' @author Andreas Schönberg


IKARUS_dawn <- function(FFS=FALSE,Tpoints,buf_size,design,Stk,Stk_name,plot_res=FALSE,save_png=FALSE,save_res=FALSE,path_res,path_png,fsize=24){

  ### prepare training data

  # compute TP buffer
  buf <-gBuffer(Tpoints,byid = T,width = buf_size,capStyle = design)

  # extract Training Data
  tDat <- exrct_Traindat_LLOCV(buf,Stk,"class","location")

  ###
  # run classification and return prediction
  if(FFS==T){
    FFSmodel <- BestPredFFS_LLOCV(tDat=tDat,classCol = "class",classLocCol="class_location",nk=5)
    # compute prediction and arrange pred and model equal to outpout from RFclassLLOCV to fit in following dawn code
    pred <- raster::predict(Stk,FFSmodel)

    # rename variables
    cl <- list("prediction"=pred,"model_LLOCV"=FFSmodel)
    cat(" ",sep = "\n")
    cat("IKARUS finished",sep = "\n")
  } else {
    cl <- RFclass_LLOCV(tDat = tDat,nk=5,predStk = Stk,classCol = "class")
  }


  # AOA calculation
  cat(" ",sep = "\n")
  cat("calculation AOA",sep = "\n")
  # run AOA
  aoa <- CAST::aoa(newdata = Stk,model = cl$model_LLOCV)

  ### Output modes


  # set name of design
  if (design=="FLAT"){
    form = "f"
  }
  if (design=="SQUARE"){
    form = "r"
  }
  if (design=="ROUND"){
    form = "c"
  }

  # merge name (bufsize * 100 to prevent "." in output filename)
  name_cl  <- paste0(Stk_name,"_",form,buf_size*100,"_CL")

  name_aoa <- paste0(Stk_name,"_",form,buf_size*100,"_AOA")

  # prepare sub text for CL with accuracy and kappa for best tune
  res_values <-(subset(cl$model_LLOCV$results, mtry == cl$model_LLOCV$bestTune[1,1]))
  sub_cl <- paste0("accuracy: ",round(res_values[2],4)," kappa: ",round(res_values[3],4))

  # prepare sub text for AOA with percent of AOA area
  per_aoa <- sum(aoa$AOA[aoa$AOA ==1]) /  ncell(aoa$AOA)
  sub_aoa <- paste0("AOA_area: ", round(per_aoa,4))

  # get raster without attributes to run with splot
  pred <- setValues(raster(cl$prediction), cl$prediction[])

  ### save result plots as png
  if(plot_res==TRUE){

    # plot prediction and AOA in grid
    grid.arrange(
    spplot(pred,col.regions=viridis(100),sub=sub_cl,main=name_cl,par.settings = list(fontsize = list(text = 12))),
    spplot(aoa$AOA ,col.regions=c("red","grey") ,main=name_aoa,sub=sub_aoa,par.settings = list(fontsize = list(text = 12))),
    ncol=2)


    cat(" ",sep = "\n")
    cat("plotting results",sep = "\n")

  }

  if(save_png==TRUE){
    # save plot code to variable due to png requires print() to run with a function
    pCL <- spplot(pred,col.regions=viridis(100),colorkey=NULL,sub=sub_cl,main=name_cl,par.settings = list(fontsize = list(text = fsize)))
    pAO <-spplot(aoa$AOA ,col.regions=c("red","grey") ,colorkey=NULL,main=name_aoa,sub=sub_aoa,par.settings = list(fontsize = list(text = fsize)))

    # open device for .png
    png(filename=paste0(path_png,name_cl,".png"))
    # print the plot
    print(pCL)
    # close device
    dev.off()

    # open device for .png
    png(filename=paste0(path_png,name_aoa,".png"))
    # print the plot
    print(pAO)
    # close device
    dev.off()

    # cat path
    cat(" ",sep = "\n")
    cat(paste0("images saved to:",path_png,sep = "\n"))
  }

  # save results
  if(save_res==T){
    writeRaster(cl$prediction,paste0(path_res,name_cl,".grd"), format="raster")
    writeRaster(aoa$AOA,paste0(path_res,name_aoa,".grd"), format="raster")
    cat(" ",sep = "\n")
    cat(paste0("result raster saved to:",path_res,sep = "\n"))
  }
  # output
  cat(" ",sep = "\n")
  cat("finished",sep = "\n")

  re <-list("prediction"=cl$prediction,"model_LLOCV"=cl$model_LLOCV,"AOA"=aoa$AOA)
  return(re)

} # end function

