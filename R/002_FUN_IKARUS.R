### separated functions from Package IKARUS for CAST based classifications
# see https://github.com/SchoenbergA/IKARUS for the Package and Tutorial

### Extract Training data for LLOCV ####

#' Extract training data from RasterStacks for LLOCV
#' @description Extracts values from a RasterStack of predictor variables by training polygons
#' @param trainPoly SpatialPolygonsDataframe -  with training polygons to assign values
#' @param predStk RasterStack - with layers to extract values from
#' @param classCol character - name of the column containing the class information, default=NULL
#' @param locname character - name of the column containing the location information, default=NULL

#' @return Returns a data.frame with values of each Rasterlayer per pixel for the training polygons along with the location.
#' Additionally adds a column of the respective class information.
#' @details This function is used to extract training data from a Raster Stack. This training dataset is used for IKARUS::BestPredFFS and IKARUS::RFclass.

#' * classCol - the column with information about the class of a polygon. Supports either character or numeric values for classes (eg 1,2,3 or "tree","stone","grass")
#' * predictor selection - specific predictors can be selected by using [[]] in parameter 'predStk'. E.g. predStk = x[[1:4]].
#' * locname - the column with information about the location of the polygon for LLOCV. Supports either character or numeric values for classes (eg 1,2,3 or "north","east","west")
#' @note the function will check for INF and or NA values. INF values are first set to NA and further all NA will be deleted to prevent errors in further processing with IKARUS::BestPredFFS and IKARUS::RFclass.
#' @author Andreas Schönberg
#' @examples
#' # load data
#' require(raster)
#' require(IKARUS)
#' lau_Stk <- raster::stack(system.file("extdata","lau_Stk.tif",package = "IKARUS"))
#' #set layer names
#' names(lau_Stk)<- c("blue","green","red","nir","NDVI","NDVI_sum3","NDVI_sobel3")
#' lau_tP <-rgdal::readOGR(system.file("extdata","lau_TrainPoly_LLOCV2.shp",package = "IKARUS"))
#' # handle CRS string
#' crs(lau_tP) <- crs(lau_Stk)
#' ### check column names
#' names(lau_tP)
#' # -> lau_tP has both character and numeric class information
#' ### extract values using character class information
#' tDat <- exrct_Traindat_LLOCV(lau_tP,lau_Stk,"class","location")
#' head(tDat)
#' ### extract values using numeric class information
#' tDat2 <- exrct_Traindat_LLOCV(lau_tP,lau_Stk,"class_num","loc_num")
#' head(tDat2)
#' @export exrct_Traindat_LLOCV
#' @aliases exrct_Traindat_LLOCV



exrct_Traindat_LLOCV <- function(trainPoly,predStk,classCol=NULL,locname=NULL,lyrname=names(predStk)){


  # check input
  if(length(lyrname)!=nlayers(predStk)){
    stop("Incorrect number of layer names: Input layernames are more or less than Rasterlayers ")
  }
  names(predStk)<-lyrname

  # start extraction with location

  cat("IKARUS starting Extraction",sep = "\n")

  # get levels for factors and save orgnames in lvls
  classpos <- which(names(trainPoly)==classCol)
  nfactor <-length(unique(trainPoly[[classpos,]]))
  trainPoly[[classpos]] <- as.factor(trainPoly[[classpos]])

  lvlClass <-levels(as.factor(trainPoly[[classpos]]))

  locpos <- which(names(trainPoly)==locname)
  nflocpos <-length(unique(trainPoly[[locpos,]]))
  trainPoly[[locpos]] <- as.factor(trainPoly[[locpos]])

  lvlLoc <-levels(as.factor(trainPoly[[locpos]]))

  trainPoly[[locpos]]
  as.factor(trainPoly[[locpos]])

  # rasterize
  shp2rst <- raster::rasterize(trainPoly,predStk,field=classCol)
  #plot(shp2rst)
  locID <- raster::rasterize(trainPoly,predStk,field=locname)
  #plot(locID)
  maskedStk <- mask(shp2rst,predStk)
  # reduce mask to one layer
  masked <- maskedStk[[1]]
  names(masked) <-classCol

  # add mask and get values
  trainStk <- addLayer(predStk,masked,locID)
  names(trainStk) <- c(names(predStk),classCol,"locationID")
  dat <- getValues(trainStk)

  # check for INF and NA
  if(any(is.infinite(dat))==TRUE){
    nINF <- sum(is.infinite(dat))
    # set inf to NA
    cat(" ",sep = "\n")
    cat(paste("INF values detected: setting ",nINF,"INF to NA"),sep = "\n")
    dat[mapply(is.infinite, dat)] <- NA
  }

  if(any(is.na(dat))==TRUE){
    nNA <- sum(is.na(dat))
    # delete NAs
    cat(" ",sep = "\n")
    cat(paste("NAs detected: deleting",nNA," NAs"),sep = "\n")
    dat_clean <-na.omit(dat)
  }

  # transform to dataframe
  TrainDat <- as.data.frame(dat_clean)

  #rename factors to input names for location
  for (i in (1:max(TrainDat$locationID))){
    TrainDat$locationID[TrainDat$locationID==i] <- lvlLoc[i]
  }
  # rename classcol to 'class'
  names(TrainDat)[names(TrainDat) == classCol] <-"class"
  #rename factors to input names
  for (i in (1:max(TrainDat$class))){
    TrainDat$class[TrainDat$class==i] <- lvlClass[i]
  }
  # change name to org input name for class
  names(TrainDat)[names(TrainDat) == "class"] <-classCol

  #add location by class for LLOCV
  TrainDat$class_location <- paste(TrainDat$class,sep="_",TrainDat$locationID)
  cat(" ",sep = "\n")
  cat("IKARUS finished Extraction",sep = "\n")
  return(TrainDat)


} # end of function
### Random Forest classification with LLOCV ####
#' Random Forest Classification with Leave Location Out Cross-Validation
#' @description RF Classification with LLOCV
#' @param tDat data.frame - with values of the predictors (see details)
#' @param predStk - RasterStack - with the predictors.
#' @param predCol numeric - seq of columns with predictor values. By default uses 1:(length(tDat)-1) for tDat format computed by IKARUS::exrct_Traindat
#' @param classCol character - name of the column containing the class information
#' @param classLocCol character - name of the column containing the class and location information
#' @param nk - numeric - number for k in spacefolds
#' @param Cores numeric - amount of Cores to exclude from calculation, default = 1
#' @return returns a list with the model and the prediction
#' @details
#' * predCol -  specific predictors can be selected by setting predCol = x:y
#' * tDat - the use of IKARUS::exrct_Traindat is recommended.
#' * parallel processing - the function uses parallel processing for multicore processors. by default all cores -1 are used.
#' @author Andreas Schönberg
#' @examples
#'# load data
#' require(caret)
#' require(CAST)
#' require(doParallel)
#' require(raster)
#' require(IKARUS)
#' lau_Stk <- raster::stack(system.file("extdata","lau_Stk.tif",package = "IKARUS"))
#' lau_tP <-rgdal::readOGR(system.file("extdata","lau_TrainPoly_LLOCV2.shp",package = "IKARUS"))
#' # handle CRS string
#' crs(lau_tP) <- crs(lau_Stk)
#' #set layer names
#' names(lau_Stk)<- c("blue","green","red","nir","NDVI","NDVI_sum3","NDVI_sobel3")
#' ### extract values using 'exrct_Tdat' to generate training dataset
#' tDat <- exrct_Traindat_LLOCV(lau_tP,lau_Stk,classCol="class",locname="location")
#' # check for class column and predictor columns in input training dataset
#' head(tDat)
#' # classification
#' model1 <- RFclass_LLOCV(tDat = tDat,predCol = "default",predStk = lau_Stk,classCol = "class")
#' #check model
#' model1$model_LLOCV
#' # plot prediction
#' plot(model1$prediction)
#' # classification with only RGB + NIR
#' model2 <- RFclass_LLOCV(tDat = tDat,predCol = 1:4,predStk = lau_Stk[[1:4]],classCol = "class")
#' #check model
#' model2$model_LLOCV
#' # plot prediction
#' plot(model2$prediction)

#' @export RFclass_LLOCV
#' @aliases RFclass_LLOCV


RFclass_LLOCV <- function(tDat,predCol="default",predStk=NULL,classCol="class",classLocCol="class_location",nk=NULL,Cores=1){

  # check input
  cat("checking inputs ",sep="\n")
        ## missing arguments
        if(is.null(predStk)){
          stop("missing argument predStk")
        }
              if(is.null(predCol)){
                stop("missing argument predCol")
              }
                    if(is.null(classCol)){
                      stop("missing argument classCol")
                    }
  if(is.null(classLocCol)){
    stop("missing argument classLocCol")
        }
        if(is.null(nk)){
          stop("missing argument nk")
        }
              if(any(names(tDat)==classCol)==FALSE){
                stop("selected column name for 'classCol' could not be found in tDat")
              }

  ### prepare input parameters and default

  # prepare class column
  classColumn <- which(names(tDat)==classCol)

        # prepare predictor columns if default
        if(any(predCol=="default")==TRUE){
          predCol <- seq(1:(length(tDat)-3))
        }

              # selected predictor check
              npred <-length(predCol)
              nlayStk <- nlayers(predStk)
              if(identical(npred,nlayStk)==FALSE){
                stop("number of selected predictors in Stack is not equal to number of selected predictors in Tdat ")
              }
              cat("using predictors:  ")
              cat(paste(names(tDat[,predCol]),collapse = ", "),sep="\n")

  # prepare Cores
  cl =  makeCluster(detectCores()-Cores)
  cat(paste("using",length(cl),"of",length(cl)+Cores,"availible Cores"),sep="\n")

  ### prepare LLOCV

  #set seed
  set.seed(112019)

        #create Spacefolds, k= amount of unique spatial units
        indices = CAST::CreateSpacetimeFolds(tDat, spacevar = classLocCol, k = nk)

              #set seed
              set.seed(112019)

  #create trainControl for LLOCV
  tC <-trainControl(method = "cv", classProbs = TRUE, index = indices$index, indexOut = indices$indexOut)
  cat(" ",sep = "\n")
  cat("IKARUS starting model with LLOCV",sep = "\n")
        # start cores
        set.seed(112019)
        registerDoParallel(cl)

  #start RF LLOCV
  starttime <- Sys.time()
  LLOCVmodel = caret::train(tDat[,predCol],
                            tDat[,classColumn],
                            method = "rf", withinSE = FALSE, metric= "Kappa",
                            importance = TRUE, trControl = tC)
  #stop RF
  stopCluster(cl)
  stoptime <- Sys.time()

  # calculate time
  diftim <-round(difftime(stoptime,starttime,units = "hours"),4)
  cat(" ",sep = "\n")
  cat("finished model",sep = "\n")
  cat(paste0("needed ",diftim," hours",sep = "\n"))
  cat("best result:",sep = "\n")
  cat(" ",sep = "\n")

  # results
  print(subset(LLOCVmodel$results, mtry == LLOCVmodel$bestTune[1,1]))
  cat("order of classes:",sep = "\n")
  print(LLOCVmodel$level[1:length(LLOCVmodel$level)])
  cat(" ",sep = "\n")

  # predict
  cat("IKARUS starting prediciton",sep = "\n")

  pred <- raster::predict(predStk,LLOCVmodel)

  # list output
  LS_output <- list("prediction"=pred,"model_LLOCV"=LLOCVmodel)
  cat(" ",sep = "\n")
  cat("IKARUS finished",sep = "\n")
  return(LS_output)
} # end of main function


### FFS for LLOCV ####

#' Select best performance layers for classification with a LLOCV
#' @description uses a forward feature selection (FFS) to select the best predictors for the classification
#' @param tDat data.frame - with values of the predictors (see details)
#' @param predCol numeric - seq of columns with the predictor values. By default uses 1:(length(tDat)-1) for tDat format computed by 'IKARUS::exrct_Traindat'
#' @param classCol character - name of the column containing the class information
#' @param classLocCol character - name of the column containing the class and location information
#' @param nk - numeric - number for k in spacefolds
#' @param Cores numeric - amount of Cores to exclude from calculation, default = 1

#' @return returns a list of best performing predictors
#' @details The function is used to select best performing predictor variables for a classification. The
#'  * predCol -  specific predictors can be selected by setting predCol = x:y
#'  * tDat - the use of IKARUS::exrct_Traindat is recommended.
#'  * parallel processing - the function uses parallel processing for multicore processors. by default all cores -1 are used.
#' @note The function will compute a huge number of models. Depending on the sizes of the training data the
#'  process can take long time even with multicore processing.
#' @author Andreas Schönberg
#' @examples
#'# load data
#' require(caret)
#' require(CAST)
#' require(doParallel)
#' require(raster)
#' require(IKARUS)
#' lau_Stk <- raster::stack(system.file("extdata","lau_Stk.tif",package = "IKARUS"))
#' lau_tP <-rgdal::readOGR(system.file("extdata","lau_TrainPoly_LLOCV2.shp",package = "IKARUS"))
#' # handle CRS string
#' crs(lau_tP) <- crs(lau_Stk)
#' #set layer names
#' names(lau_Stk)<- c("blue","green","red","nir","NDVI","NDVI_sum3","NDVI_sobel3")
#' ### extract values using 'exrct_Tdat' to generate training dataset
#' tDat <- exrct_Traindat_LLOCV(lau_tP,lau_Stk,classCol="class",locname="location")
#' # check for class column and predictor columns in input training dataset
#' head(tDat)
#' # FFS with all layers in the RasterStack (this example could take some minutes)
#' #ffs <- BestPredFFS_LLOCV(tDat=tDat,classCol = "class",classLocCol="class_location",nk=5)

#' # FFS with selected layers "blue","green","red","nir"
#' ffs2 <- BestPredFFS_LLOCV(tDat=tDat,predCol = 1:4,classCol = "class",classLocCol="class_location",nk=5)

#' # some code to look at the results
#' ffs1$selectedvars # show seleted variables
#' ffs1$perf_all # show performance of all combinations
#' ffs1$finalModel # show confusion matrix
#' @export BestPredFFS_LLOCV
#' @aliases BestPredFFS_LLOCV

BestPredFFS_LLOCV <- function(tDat,predCol="default",classCol=NULL,classLocCol="class_location",nk=NULL,Cores=1){

  #check input
  cat("checking inputs ",sep="\n")
  ## missing arguments
  if(is.null(predCol)){
    stop("missing argument predCol")
  }
  if(is.null(classCol)){
    stop("missing argument classCol")
  }
  if(is.null(classLocCol)){
    stop("missing argument classLocCol")
  }
  if(is.null(nk)){
    stop("missing argument nk")
  }
  if(any(names(tDat)==classCol)==FALSE){
    stop("selected column name for 'classCol' could not be found in tDat")
  }
  if(any(names(tDat)==classLocCol)==FALSE){
    stop("selected column name for 'classLocCol' could not be found in tDat")
  }
  # prepare columns
  classColumn <- which(names(tDat)==classCol)
  # prepare predictor columns
  if(any(predCol=="default")==TRUE){
    predCol <- seq(1:(length(tDat)-3))
  }
  cat("using predictors:  ")
  cat(paste(names(tDat[,predCol]),collapse = ", "),sep="\n")

  # prepare Cores
  cl =  makeCluster(detectCores()-Cores)
  cat(paste("using",length(cl),"of",length(cl)+Cores,"availible Cores"),sep="\n")

  #set seed
  set.seed(112019)
  #create Spacefolds, k= amount of unique spatial units
  indices = CAST::CreateSpacetimeFolds(tDat, spacevar = classLocCol, k = nk)
  #set seed


  set.seed(112019)
  #create trainControl for LLOCV
  tC <-trainControl(method = "cv", classProbs = TRUE, index = indices$index, indexOut = indices$indexOut)
  cat(" ",sep = "\n")
  cat("IKARUS starting model with LLOCV",sep = "\n")
  # start cores
  set.seed(112019)
  registerDoParallel(cl)

  # start FFS
  starttime <- Sys.time()
  FFSmodel <- CAST::ffs(tDat[,predCol],
                        tDat[,classColumn],
                        method = "rf", withinSE = FALSE, metric= "Kappa",
                        importance = TRUE, trControl = tC)



  #stop FFS
  stopCluster(cl)
  stoptime <- Sys.time()
  diftim <-round(difftime(stoptime,starttime,units = "hours"),4)
  cat(" ",sep = "\n")
  cat(paste0("needed ",diftim," hours"))
  cat(" ",sep = "\n")
  cat(" ",sep = "\n")
  cat("selected variables",sep = "\n")
  cat(paste(FFSmodel$selectedvars,collapse = ", "))
  cat(" ",sep = "\n")
  cat(" ",sep = "\n")
  cat("IKARUS finished",sep = "\n")
  return(FFSmodel)
} # end of main function
