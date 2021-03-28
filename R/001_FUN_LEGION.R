### separated functions from package "LEGION" for computing artificial layers with RGB
# see https://github.com/SchoenbergA/LEGION for the Package

### Index computation for rgb ####

#' Calculate RGB indices
#' @description computes several indices based on RGB bands
#' @param rgb a RasterStack with RGB bands
#' @param red the band/layer number of band 'red'
#' @param green the band/layer number of band 'green'
#' @param blue the band/layer number of band 'blue'
#' @param indlist comma-separated character combinations of the desired indices. Select from
#' "VVI","VARI","NDTI","RI","CI","BI","SI","HI","TGI","GLI","NGRDI", default=all. See details
#' @return returns a RasterStack with the selected indices
#' @details
#' ## available indices
#' * "VVI" Visible Vegetation Index (1 - abs((red - 30) / (red + 30))) * (1 - abs((green - 50) / (green + 50))) *(1 - abs((blue - 1) / (blue + 1)))
#' * "NDTI" Normalized Difference Turbidity Index (red-green)/(red+green)
#' * "CI" Soil Colour Index (red-green)/(red+green)
#' * "BI" Brightness Index sqrt((red**2+green**2+blue*2)/3)
#' * "SI" Spectra Slope Saturation Index (red-blue)/(red+blue)
#' * "TGI" Triangular Greenness Index (-0.5*(190*(red - green)- 120*(red - blue))
#' * "GLI" Green Leaf Index (2*green-red-blue)/(2*green+red+blue)
#' * "NGRDI" Normalized Green Red Difference Index (green-red)/(green+red)
#' @author Andreas Schönberg
#' @references
#' The IDB Project (2020): Index Database (https://www.indexdatabase.de/)
#' @examples
#' ### load data
#' require(raster)
#' require(LEGION)
#' mspec <- raster::stack(system.file("extdata","lau_mspec.tif",package = "LEGION"))
#' names(mspec)<- c("blue","green","red","nir")
#' ### compute all vegetation indizes
#' x <-LEGION::vegInd_RGB(mspec,3,2,1)
#' plot(x)
#' ### select specific vegetation indices
#' vi <-c("VVI","SI","GLI")
#' y <-LEGION::vegInd_RGB(mspec,3,2,1,indlist=vi)
#' plot(y)
#' @export vegInd_RGB
#' @aliases vegInd_RGB

vegInd_RGB<- function(rgb,red=NULL,green=NULL,blue=NULL,indlist="all"){

  ### check input
  if(any(indlist=="all")){
    indlist <-c("VVI","NDTI","CI","BI","SI","TGI","GLI","NGRDI")
  }else{indlist=indlist}

        #create notin and check for wrong input
        `%notin%` <- Negate(`%in%`)
        if(any(indlist %notin% c("VVI","NDTI","CI","BI","SI","TGI","GLI","NGRDI"))) {
          stop("wrong Vegetation Index selected or not supported")
        }


              #check if raster is an 3 band layer
              if (any(is.null(red),is.null(green),is.null(blue))==TRUE){
                stop("no bands or less bands defined")
              }
  # get rgb bands with given band position (to prevent usage of wrong bands eg if used in order bgr from sentinal)
  red <- rgb[[red]]
  green <- rgb[[green]]
  blue <- rgb[[blue]]

  #calculate selected indizes
  indices <- lapply(indlist, function(item){
    if (item=="VVI"){
      cat(" ",sep = "\n")
      cat("### LEGION calculating (Visible Vegetation Index (VVI)) ###",sep = "\n")
      VVI <- (1 - abs((red - 30) / (red + 30))) *
        (1 - abs((green - 50) / (green + 50))) *
        (1 - abs((blue - 1) / (blue + 1)))
      names(VVI) <- "VVI"
      return(VVI)


    } else if (item=="NDTI"){
      cat(" ",sep = "\n")
      cat("### LEGION calculating (Normalized difference turbidity index (NDTI)) ###",sep = "\n")
      NDTI<-(red-green)/(red+green)
      names(NDTI) <- "NDTI"
      return(NDTI)


    } else if (item=="CI"){
      cat(" ",sep = "\n")
      cat("### LEGION calculating (Soil Colour Index (CI)) ###",sep = "\n")
      CI<-(red-green)/(red+green)
      names(CI) <- "CI"
      return(CI)

    } else if (item=="BI"){
      cat(" ",sep = "\n")
      cat("### LEGION calculating (Brightness Index (BI)) ###",sep = "\n")
      BI<-sqrt((red**2+green**2+blue*2)/3)
      names(BI) <- "BI"
      return(BI)

    } else if (item=="SI"){
      cat(" ",sep = "\n")
      cat("### LEGION calculating (Spectra Slope Saturation Index (SI)) ###",sep = "\n")
      SI<-(red-blue)/(red+blue)
      names(SI) <- "SI"
      return(SI)


    } else if (item=="TGI"){
      cat(" ",sep = "\n")
      cat("### LEGION calculating (Triangular greenness index (TGI)) ###",sep = "\n")
      TGI <- -0.5*(190*(red - green)- 120*(red - blue))
      names(TGI) <- "TGI"
      return(TGI)

    } else if (item=="GLI"){
      cat(" ",sep = "\n")
      cat("### LEGION calculating (Green leaf index (GLI)) ###",sep = "\n")
      GLI<-(2*green-red-blue)/(2*green+red+blue)
      names(GLI) <- "GLI"
      return(GLI)

    } else if (item=="NGRDI"){
      cat(" ",sep = "\n")
      cat("### LEGION calculating (Normalized green red difference index  (NGRDI)) ###",sep = "\n")
      NGRDI<-(green-red)/(green+red)
      names(NGRDI) <- "NGRDI"
      return(NGRDI)

    }
  })
  cat(" ",sep = "\n")
  cat("###########################",sep = "\n")
  cat("### The LEGION is ready ###",sep = "\n")

  return(raster::stack(indices))
}


### Filter computation with loop for using a Raster Stack ####

#' Filter Single-band RasterLayer
#' @description applies several filter functions on single RasterLayer.
#' @param rst a single-band RasterLayer.
#' @param fLS comma-separated character combinations of the desired filter functions. Select from
#' "sum","min","max","sd","mean","modal","sobel","sobel_hrzt","sobel_vert".
#' default = all; (see details).
#' @param sizes numeric - a single value or combinations  for the MovingWindow, number must be odd.
#' @param NArm boolean - removes NA values, default= TRUE.
#' @return returns a RasterStack with the desired filtered artificial layers.
#' @details
#' ## available filter functions
#' * "sum" - sum of all cells in a MovingWindow
#' * "min" - minimum value of all cells in a MovingWindow
#' * "max" - maximum value of all cells in a MovingWindow
#' * "mean"- mean value of all cells in a MovingWindow
#' * "sd"  - standard deviation of all cells in a MovingWindow
#' * "modal" - most frequent value of all cells in a MovingWindow
#' * "sobel" - sobel edge detection filter in horizontal and vertical directions
#' * "sobel_hrzt" - sobel edge detection filter in horizontal direction only
#' * "sobel_vert" - sobel edge detection filter in vertical direction only
#' @author Andreas Schönberg
#' @seealso \code{\link{focal}}
#' @examples
#' ### load data
#' require(raster)
#' require(LEGION)
#' mspec <- raster::stack(system.file("extdata","lau_mspec.tif",package = "LEGION"))
#' names(mspec)<- c("blue","green","red","nir")
#' ### seperate single raster layer
#' rst <- mspec$nir
#' ### compute all filter
#' x <- filter_Rst(rst,sizes=c(3,5,7))
#' plot(x[[3]])
#' ### compute specific filters
#' flist <- c("modal","sobel_vert","mean")
#' y <- filter_Rst(rst,fLS=flist,sizes=c(3,5,7))
#' @export filter_Rst
#' @aliases filter_Rst

filter_Rst <- function(rst,fLS="all",sizes,NArm=TRUE){

  ### check inputs
  # set default if fLS is set to all or missing. uses all availible filter
  if(any(fLS=="all")){
    fLS <-c("sum","min","max","sd","mean","modal","sobel","sobel_hrzt","sobel_vert")
  }else{fLS==fLS}

        #create notin and check for wrong input
        `%notin%` <- Negate(`%in%`)
        if(any(fLS %notin% c("sum","min","max","sd","mean","modal","sobel","sobel_hrzt","sobel_vert"))) {
          stop("wrong Filter selected or not supported")
        }

              #check for wrong sizes input
              if(any(sizes %% 2 == 0)){
                stop("sizes contain even values (use odd values only)")
              }

  # compute selected filter
  filterstk <-lapply(fLS, function(item){

    # sum filter
    if (item=="sum"){
      cat(" ",sep = "\n")
      cat("### LEGION calculating sum filter ###",sep = "\n")
      lapply(sizes,function(f){
        cat(paste0("### starting sum  ",as.factor(f),"*",as.factor(f),sep = "\n"))
        sumLS <- raster::focal(rst,w=matrix(1/(f*f),nrow=f,ncol=f),fun=sum,na.rm=NArm)
        names(sumLS) <- paste0(names(rst),"_sum" ,as.factor(f))
        stack(sumLS)
        return(sumLS)
      })
    }#end

    # min filter
    else if (item=="min"){
      cat(" ",sep = "\n")
      cat("### LEGION calculating minimum filter ###",sep = "\n")
      lapply(sizes,function(f){
        cat(paste0("### starting min  ",as.factor(f),"*",as.factor(f),sep = "\n"))
        minLS <- raster::focal(rst,w=matrix(1/(f*f),nrow=f,ncol=f),fun=min,na.rm=NArm)
        names(minLS) <- paste0(names(rst),"_min" ,as.factor(f))
        return(minLS)
      })
    }#end

    # max filter
    else if (item=="max"){
      cat(" ",sep = "\n")
      cat("### LEGION calculating maximum filter ###",sep = "\n")
      lapply(sizes,function(f){
        cat(paste0("### starting max  ",as.factor(f),"*",as.factor(f),sep = "\n"))
        maxLS <- raster::focal(rst,w=matrix(1/(f*f),nrow=f,ncol=f),fun=max,na.rm=NArm)
        names(maxLS) <- paste0(names(rst),"_max" ,as.factor(f))
        return(maxLS)
      })
    }#end

    # sd filter
    else if (item=="sd"){
      cat(" ",sep = "\n")
      cat("### LEGION calculating standard deviation filter ###",sep = "\n")
      lapply(sizes,function(f){
        cat(paste0("### starting sd   ",as.factor(f),"*",as.factor(f),sep = "\n"))
        sdLS <- raster::focal(rst,w=matrix(1/(f*f),nrow=f,ncol=f),fun=sd,na.rm=NArm)
        names(sdLS) <- paste0(names(rst),"_sd" ,as.factor(f))
        return(sdLS)
      })
    }#end

    # mean filter
    else if (item=="mean"){
      cat(" ",sep = "\n")
      cat("### LEGION calculating mean filter ###",sep = "\n")
      lapply(sizes,function(f){
        cat(paste0("### starting mean  ",as.factor(f),"*",as.factor(f),sep = "\n"))
        meanLS <- raster::focal(rst,w=matrix(1/(f*f),nrow=f,ncol=f),fun=mean,na.rm=NArm)
        names(meanLS) <- paste0(names(rst),"_mean" ,as.factor(f))
        return(meanLS)
      })
    }#end

    # modal filter
    else if (item=="modal"){
      cat(" ",sep = "\n")
      cat("### LEGION calculating modal filter ###",sep = "\n")
      lapply(sizes,function(f){
        cat(paste0("### starting modal  ",as.factor(f),"*",as.factor(f),sep = "\n"))
        modalLS <- raster::focal(rst,w=matrix(1/(f*f),nrow=f,ncol=f),fun=modal,na.rm=NArm)
        names(modalLS) <- paste0(names(rst),"_modal" ,as.factor(f))
        return(modalLS)
      })
    }#end

    # compute sobel filter
    else if (item=="sobel"){
      cat(" ",sep = "\n")
      cat("### LEGION calculating sobel filter ###",sep = "\n")
      lapply(sizes,function(f){
        cat(paste0("### starting sobel  ",as.factor(f),"*",as.factor(f),sep = "\n"))
        range = f/2
        mx = matrix(nrow = f, ncol = f)
        my = mx

        for(i in seq(-floor(range), floor(range))){
          for(j in seq(-floor(range), floor(range))){
            mx[i+ceiling(range),j+ceiling(range)] = i / (i*i + j*j)
            my[i+ceiling(range),j+ceiling(range)] = j / (i*i + j*j)
          }
        }

        mx[is.na(mx)] = 0
        my[is.na(my)] = 0

        sobelLS <- sqrt(raster::focal(rst,mx,fun=sum,na.rm=NArm)**2+
                          raster::focal(rst,my,fun=sum,na.rm=NArm)**2 )
        names(sobelLS) <- paste0(names(rst),"_sobel" ,as.factor(f))
        return(sobelLS)
      })
    }#end

    # only horizontal sobel
    else if (item=="sobel_hrzt"){
      cat(" ",sep = "\n")
      cat("### LEGION calculating sobel horizontal filter ###",sep = "\n")
      lapply(sizes,function(f){
        cat(paste0("### starting sobel horizontal  ",as.factor(f),"*",as.factor(f),sep = "\n"))
        range = f/2
        mx = matrix(nrow = f, ncol = f)
        my = mx

        for(i in seq(-floor(range), floor(range))){
          for(j in seq(-floor(range), floor(range))){
            mx[i+ceiling(range),j+ceiling(range)] = i / (i*i + j*j)
            my[i+ceiling(range),j+ceiling(range)] = j / (i*i + j*j)
          }
        }

        mx[is.na(mx)] = 0
        my[is.na(my)] = 0

        sobel_hLS <- raster::focal(rst, mx, fun = sum,na.rm=NArm)
        names(sobel_hLS) <- paste0(names(rst),"_sobel_h" ,as.factor(f))
        return(sobel_hLS)
      })
    }#end

    # only vertical sobel
    else if (item=="sobel_vert"){
      cat(" ",sep = "\n")
      cat("### LEGION calculating sobel vertical filter ###",sep = "\n")
      lapply(sizes,function(f){
        cat(paste0("### starting sobel vertical  ",as.factor(f),"*",as.factor(f),sep = "\n"))
        range = f/2
        mx = matrix(nrow = f, ncol = f)
        my = mx

        for(i in seq(-floor(range), floor(range))){
          for(j in seq(-floor(range), floor(range))){
            mx[i+ceiling(range),j+ceiling(range)] = i / (i*i + j*j)
            my[i+ceiling(range),j+ceiling(range)] = j / (i*i + j*j)
          }
        }

        mx[is.na(mx)] = 0
        my[is.na(my)] = 0

        sobel_vLS <- raster::focal(rst, mx, fun = sum,na.rm=NArm)
        names(sobel_vLS) <- paste0(names(rst),"_sobel_v" ,as.factor(f))
        return(sobel_vLS)
      })
    }#end

  })#end main lapply

  #########################################

  #handle output format (due to output of lapply is a list)
  unLS <- unlist(filterstk)# unlist to get a Stack
  cat(" ",sep = "\n")
  cat("###########################",sep = "\n")
  cat("### The LEGION is ready ###",sep = "\n")
  return(raster::stack(unLS))

} # end fun

### wrapper for using 'filter_Rst' for a Raster Stack

#' Wrapper for 'filter_Rst' to filter RasterStacks
#' @description applies several filter functions to each RasterLayer in a RasterStack.
#' @param Stk a RasterStack.
#' @param fLS comma-separated character combinations of the desired filter functions. Select from
#' "sum","min","max","sd","mean","modal","sobel","sobel_hrzt","sobel_vert".
#' default = all; (see details).
#' @param sizes numeric - a single value or combinations for the MovingWindow, number must be odd.
#' @param layernames optional - comma-seperated character combinations of the desired layernames.
#' @return returns a RasterStack with the desired filtered artificial layers.
#' @details
#' ## available filter functions
#' * "sum" - sum of all cells in a MovingWindow
#' * "min" - minimum value of all cells in a MovingWindow
#' * "max" - maximum value of all cells in a MovingWindow
#' * "mean"- mean value of all cells in a MovingWindow
#' * "sd"  - standard deviation of all cells in a MovingWindow
#' * "modal" - most frequent value of all cells in a MovingWindow
#' * "sobel" - sobel edge detection filter in horizontal and vertical directions
#' * "sobel_hrzt" - sobel edge detection filter in horizontal direction only
#' * "sobel_vert" - sobel edge detection filter in vertical direction only
#' @author Andreas Schönberg
#' @seealso \code{\link{focal}},\code{\link{filter_Rst}}
#' @examples
#' ### load data
#' require(raster)
#' require(LEGION)
#' mspec <- raster::stack(system.file("extdata","lau_mspec.tif",package = "LEGION"))
#' ### compute all filters or every layer in stack
#' x <- filter_Stk(mspec,sizes=3)
#' x # note that the names are basic set x.1
#' ### define layernames
#' ln <- c("blue","green","red","nir")
#' z <- filter_Stk(mspec,fLS="sum",sizes=3,layernames=ln)
#' names(z)
#' ### compute specific filters
#' flist <- c("modal","sobel_vert","mean")
#' y <- filter_Stk(mspec,fLS=flist,sizes=c(3,5,7))
#' y
#' @export filter_Stk
#' @aliases filter_Stk

filter_Stk <- function(stk,fLS="all",sizes,layernames=names(stk)){

  #check name input
  if(length(layernames)!=nlayers(stk)){
    stop("incorrect number of names in 'layername'")
  }
  names(stk) <- layernames


  all <-lapply(1:nlayers(stk), function(i){
    cat(" ",sep = "\n")
    cat(paste0("starting filter for layer ",names(stk[[i]])))
    cat(" ",sep = "\n")
    rt <-filter_Rst(stk[[i]],fLS,sizes)
    cat(paste0("### layer ",as.factor(i)," / ",nlayers(stk)))
    cat(" ",sep = "\n")
    return(rt)
  })
  #handle output format
  unLS <- unlist(all)
  cat(" ",sep = "\n")
  cat("###########################",sep = "\n")
  cat("### The LEGION is ready ###",sep = "\n")
  return(raster::stack(unLS))


}# end function


### Testing for correlations using cor and caret ####

#' Detect Raster Correlation
#' @description detects correlation of RasterLayers in a RasterStack
#' @param Stk a RasterStack
#' @param THvalue numeric as 0.X - threshold value for correlation value of correlating layers to drop correlating layers (see details).
#' @param returnCorTab boolean - to return either the cleaned RasterStack (FALSE) or to return the correlation table (TRUE); default= FALSE


#' @return returns a RasterStack with the layers correlating less than the treshold value.
#' @details
#' This function is used to test a RasterStack on the correlation of the RasterLayers. All RasterLayers which have a higher correlation value than 'THvalue'
#' will be dropped from the RasterStack. E.g. if THvalue=0.9 all RasterLayers with correlation values >0.9 and  < -0.9 will be dropped.
#' @note
#' * To perform a correlation test all values are cleaned of INF and/or NA values.
#' * The output RasterStack is NOT cleaned from INF and/or NA values.
#' @author Andreas Schönberg
#' @examples
#' ### load data to compute RGB Indices
#' require(raster)
#' require(LEGION)
#' mspec <- raster::stack(system.file("extdata","lau_mspec.tif",package = "LEGION"))
#' names(mspec)<- c("blue","green","red","nir")
#' ### compute all vegetation indizes
#' x <-LEGION::vegInd_RGB(exp_rgb,3,2,1)
#' # perform Cor Test
#' y <- detct_RstCor(x,0.7)
#' names(y)
#' # to return the Correlation Matrix
#' z <- detct_RstCor(x,0.7,returnCorTab=TRUE)
#' @export detct_RstCor
#' @aliases detct_RstCor

detct_RstCor <- function(Stk,THvalue,returnCorTab=FALSE){
  cat("### LEGION testing Raster Correlation",sep="\n")

  #get values from RasterStack
  val <- getValues(Stk)
          # check for INF and set INF to NA values
          if(any(is.infinite(val))==TRUE){
            nINF <- sum(is.infinite(val))
            cat(" ",sep = "\n")
            cat(paste("INF values detected: setting ",nINF,"INF to NA"))
            val[mapply(is.infinite, val)] <- NA
          }
                # Delete NA values (and INF values)
                if(any(is.na(val))==TRUE){
                  nNA <- sum(is.na(val))
                  cat(" ",sep = "\n")
                  cat("NAs detected: deleting",nNA," NAs")
                  val <-na.omit(val)
                }

  #cortest
  cortab <- cor(val)
  # plot correlation table (not usable for bigger datasets)
  corrplot::corrplot(cortab)
  # remove pairwise correlation by caret::findCorrelation
  lyrnames <-caret::findCorrelation(cortab,abs(THvalue),names=T)
  cat(" ",sep = "\n")
  cat(" ",sep = "\n")
  cat(paste("correlating layers detected"))
  # remove selected all layers with correlation detected by caret
  cat(" ",sep = "\n")
  cat(paste("dropping from Stack:"))
  cat(paste(lyrnames,collapse=", "),sep="\n")
  cat(" ",sep = "\n")
  cat("### LEGION finished testing Raster Correlation",sep="\n")
  # output for either the Layers without correlation or the CorPlot
  if(returnCorTab==FALSE){
    # return only layers which are not correlating
    Stk_clean <- dropLayer(Stk,lyrnames)
    return(Stk_clean)}
  if(returnCorTab==TRUE){
    # return only the cortab
    return(cortab)
  }
}# end of function

### Testing for homogenity in layers ####

#' Detect Raster Homogeneity
#' @description detects homogeneity of Rasterlayers in a RasterStack and drops RasterLayers with homogeneity higher than a set Threshold Value.
#' @param Stk a RasterStack
#' @param THvalue numeric  - in percent (0.x) Threshold Value for homogeneity Value to drop Layers.
#' @param valueRange numeric - in percent (0.x) Range of Values with most data (see details).
#' @return Returns the RasterStack without homogeneous RasterLayers.
#' @details
#' This function is used to test a RasterStack for homogeneous RasterLayers.A RasterLayer is makred as homogenious if >= x% of the data is distributed in y % of the value range.
#' E.G. If 90% (THvalue=0.9) of the Raster cells have values within 10% of the value range (valueRange=0.1) the RasterLayer is dropped if due to homogeneity.
#' @note
#' * To perform the test for homogeneity the data is cleand from INF and NA values. Further the data will be normalized to set 100 breaks representing 1% of the data range.
#'
#' * The RasterLayers in the output Stack will be selected by their homogeneity and are NOT manipulated (not clean from INF or NA or normalized)
#' @author Andreas Schönberg
#' @examples
#' ### load data
#' require(LEGION)
#' require(raster)
#' mspec <- raster::stack(system.file("extdata","lau_mspec.tif",package = "LEGION"))
#' names(mspec)<- c("blue","green","red","nir")
#' ### compute all vegetation indices
#' x <-LEGION::vegInd_RGB(mspec,3,2,1)
#' plot(x)
#' ### homogenity if 90% of data represent 10% of the data range
#' hmgy90 <-detct_RstHmgy (x,THvalue=0.9,valueRange=0.1)
#' ### homogenity if 70% of data represent 10% of the data range
#' hmgy70 <-detct_RstHmgy (x,THvalue=0.7,valueRange=0.1)
#' ### homogenity if 70% of data represent 5% of the data range
#' hmgy90 <-detct_RstHmgy (x,THvalue=0.7,valueRange=0.05)


#' @export detct_RstHmgy
#' @aliases detct_RstHmgy


detct_RstHmgy <- function(Stk,valueRange,THvalue){
  cat("### LEGION testing Raster homogeneity",sep="\n")

  # check all Rst for homogeneity lapply
  hmgy <-lapply(1:nlayers(Stk), function(i){

    # separate single raster and get values
    rst <- Stk[[i]]
    nc <- ncell(rst)
    val <- getValues(rst)
    # check for INF values and set to NA
    val[!is.finite(val)] <- NA
    # delete NA values (and INF values)
    val <- val[!is.na(val)]
    # normalize the data
    nmlz <- function(x){(x-min(x))/(max(x)-min(x))}
    val_n <- nmlz(val)

    # compute histogramm, 100 breaks to represent 1% of data each break
        h <- hist(val_n,plot=F, breaks= seq(0,1,0.01))

    # sort counts decreasing and get count in % of total cells
    sh <-sort(h$counts,decreasing = T)/nc

    # sum the values of all breaks within the given range
    vr <-round(sum(sh[1:(valueRange*100)]),digits = 4)
    # check if the result is >= the given treshold value
    check <-vr>=THvalue

    # compare amount of data in given range with THvalue

    # set layer to drop due to detected homogenity (save layer name in the variable 'drop')
    if(check==TRUE){
      cat(" ",sep = "\n")
      cat(names(Stk[[i]]),sep = "\n")
      cat(paste("Layer has: ",round(vr*100,digits = 4),"% of values in",valueRange*100,"% of the value range ---> drop"))
      cat(" ",sep = "\n")
      drop <- names(Stk[[i]])

    # set layer to keep due to missing homogenity (save as NA in the variable 'drop')
    } else {
      cat(" ",sep = "\n")
      cat(names(Stk[[i]]),sep = "\n")
      cat(paste("Layer has: ",round(vr*100,digits = 4),"% of values in",valueRange*100,"% of the value range ---> keep"))
      cat(" ",sep = "\n")
      drop <- "NA"
    }

  }# end lapply function

  )# end lapply

  ### output

  # unlist
  hmgy <-unlist(hmgy)
  # get vector with all layers to drop by dropping NA (which represent layers without homogeneity)
  hmgy = hmgy[hmgy!= "NA"]
  cat(" ",sep = "\n")
  cat(" ",sep = "\n")

  # handle output an messages
  # fork for any homogeneity, no homogeneity or full homogeneity
  if(length(hmgy)<nlayers(Stk) & length(hmgy)!=0){
    cat(paste("layers with homogeneity detected"))
    # remove layers with homogeneity
    Stk_clean <- dropLayer(Stk,hmgy)
    cat(" ",sep = "\n")
    cat(paste("dropping from Stack: "))
    cat(paste(hmgy,collapse=", "),sep="\n")
    cat(" ",sep = "\n")
    cat("### LEGION finished Raster homogeneity detection",sep="\n")
    return(Stk_clean)
    # if no layers with homogeneity are detected
  } else if (length(hmgy)==nlayers(Stk)){
    cat("homogeneity for all RasterLayers in the RasterStack detected, returning empty Stack")
    Stk_clean <- dropLayer(Stk,hmgy)
    return(Stk_clean)
    # no homogeneity
  } else {
    cat("no layers with homogeneity detected, returning the full Stk")
    return(Stk)
  }# end fork for homogeneity

}# end of function
