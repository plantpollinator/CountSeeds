# CountSeeds_..R
# Original author: (c) Philipp Schlueter 06 Mar 2015
# Developers: Philipp Schlueter
# Licence: Artistic License 2.0
# PURPOSE: To allow mouse-based scoring of images associated with samples by a user
# -------
# Idea: make this runnable by RScript and allos USER as command-line argument OR by reading a user.txt file
# automatically continue from previous session (or not) and allow multi-user data input (to avoid re-scoring, but 
# allow a predefined number of randomly selected pictures to be scored twice...)
# NB: - need to check if Sys.Date() gives same format across machines...
#     - some functions make assumptions/refs to globals and would need refactoring...
#     - functionality should be validated/tested before used with real data
# -------
# Change Log:
# v00 2015-03-06 PMS -- original proof-of-concept version, not feature complete, hardly documented
# v02 2015-03-08 PMS -- fixed some obvious bugs, enabled loading / saving in interactive mode, not feature complete, 
#                       but basic functionality is there...
# v02.01 2015-03-11 KB -- modifying this for Linux using X11() instead of windows() (with automatic system detection), setting current user, using jpeg instead of biOps, fixing mouse issues in Linux, bugfixes for loading/saving
# V03 2015-03-12 PMS -- adding a few conditionals, bug-fixing, validation code, ....
# v03.01 2015-03-27 KB -- coordinates now in pixels with area bounding, loading data now draws points and advances to the next incomplete image (but only loads points for that image, so the software assumes you save and quit with only one incomplete image rather than multiple, which honestly makes sense anyhow), bug-fixing for completeness data, load/save verification bug-fixed and working
# v03.02 2015-04-17 KB -- added key binding for turning debug-mode on and off, debug print for key presses


### Parameters
# max # points to score per sample before proceeding to next sample
MAX_N_PER_SAMPLE <- 150
# min # points to score per sample before it is considered done
MIN_N_PER_SAMPLE <- 100
# max # pix to score per sample
MAX_IMG_PER_SAMPLE <- 20
# min # pix to score per sample
MIN_IMG_PER_SAMPLE <- 3
# min overlap in scored data points between independent users
MIN_USER_OVERLAP_PROPORTION <- 0.1
#If TRUE -> proceed to a random next item ELSE -> proceed according to list
RANDOM_NEXT <- FALSE
#Code/Initials of current user
CURRENT_USER <- "kbyers"
#Turn this on to see debug messages:
DEBUG_MESSAGES <- TRUE

# Settings and Constants
currpath <- getwd() # Current path
PixPath <- currpath # Path to folder containing images to be analysed
DataFilePath <- PixPath
PixSuffix <- "jpg"      # Suffix of image files
EscapeKey <- "ctrl-["   # R key code for "Esc" key
ReturnKey <- "ctrl-J"   # R key code for "Enter" key
FileNamePartContainingSampleInfo <- 1 # part of the file name that is the sample name
FileNameSampleDelimiter <- "-" # delimiter char for parts of input file part
# Data files
data_file_headers <- c("Sample","File","State","CoordX","CoordY","User","Date","Complete")
data_file_default_stem <- "seedcounts"
data_file_delim <- "_"
data_file_default_suffix <- "tsv"
#User file
username_def_file <- "user.txt" # This file (if present) should store the USER name in its first line, with a line break at the end




### Small helper functions

concat <- function (...) {paste(...,sep="")}


### GLOBALS!!! 
sample_names <- NULL     # Sorted array of all sample names, same order as the next variable:
sample_img_files <- NULL # <- this list will contain all the relevant info on samples, images and scoring
#
Smp <- 1  # Number of sample in sample_names
SImg <- 1 # Number of current image for sample Smp
curr_sample <- sample_names[Smp] # Current sample
curr_file <- sample_img_files[[Smp]]$Img[SImg] # File associated with current sample and image
curr_jpg <- NULL
curr_jpg_dim <- NULL



### record handling functions

rec_init <- function (SampleName, ImageFiles) {
  Smp <- SampleName
  if (!is.null(ImageFiles)) {
    sample_img_files[[Smp]]$Img<<- ImageFiles # list of images associated with samples
  }
  sample_img_files[[Smp]]$CoordX <<- NULL # marked X coordinate in an image
  sample_img_files[[Smp]]$CoordY <<- NULL # marked Y coordinate in an image
  sample_img_files[[Smp]]$IsGood <<- NULL # TRUE/FALSE according to user
  sample_img_files[[Smp]]$ImgID  <<- NULL # ID (1-based) of to which CoordX,Y, and IsGood reger 
  sample_img_files[[Smp]]$User   <<- NULL # Current user
  sample_img_files[[Smp]]$Date   <<- NULL # Current date  
  sample_img_files[[Smp]]$NCounts <<- 0   # total number of counts made for a particular sample across all images
  sample_img_files[[Smp]]$ImgComplete <<- rep (FALSE,length(sample_img_files[[Smp]]$Img)) # Indicates if the sample is completely scored
}

delete_sample_img_rec <- function (N) { # N: items to be deleted
  sample_img_files[[Smp]]$CoordX  <<- sample_img_files[[Smp]]$CoordX[-N]
  sample_img_files[[Smp]]$CoordY  <<- sample_img_files[[Smp]]$CoordY[-N]
  sample_img_files[[Smp]]$IsGood  <<- sample_img_files[[Smp]]$IsGood[-N]
  sample_img_files[[Smp]]$ImgID   <<- sample_img_files[[Smp]]$ImgID[-N]
  sample_img_files[[Smp]]$User    <<- sample_img_files[[Smp]]$User[-N] 
  sample_img_files[[Smp]]$Date    <<- sample_img_files[[Smp]]$Date[-N]
  sample_img_files[[Smp]]$NCounts <<- sample_img_files[[Smp]]$NCounts -length(N)
  if (sample_img_files[[Smp]]$NCounts == 0) {
    rec_init (Smp, ImageFiles= NULL) 
  }
}

clear_img_recs <- function () {
  id <- which (sample_img_files[[curr_sample]]$Img == curr_file)
  sel_ids<-which (sample_img_files[[curr_sample]]$ImgID  == id)
  sample_img_files[[curr_sample]]$ImgID[sel_ids]
  delete_sample_img_rec(sel_ids)
}

##### Summary and I/O functions

make_report <- function (SaveToFile=NULL) {
  print("Preparing report...")
  # implicitly takes sample_img_files as input
  Report <- matrix (0, ncol=3, nrow=length(sample_names), dimnames=list(sample_names,c("Proportion.Good.Seeds","N.Seeds","N.Pix.Scored")))
  for (i in sample_names) {
    if (sample_img_files[[i]]$NCounts>0) {
      Report[i,1] <- sum(sample_img_files[[i]]$IsGood)/sample_img_files[[i]]$NCounts
    } else {
      Report[i,1] <- NA
    }
    Report[i,"N.Seeds"] <- sample_img_files[[i]]$NCounts
    Report[i,"N.Pix.Scored"] <- length(unique(sample_img_files[[i]]$ImgID)) # actually exclude pix that weren't scored
  }
  print(Report)
  if (!is.null(SaveToFile)) {write.table (Report,file=SaveToFile,quote=FALSE,sep="\t")} else {
    basefn <- concat (data_file_default_stem, data_file_delim, CURRENT_USER,data_file_delim,as.character(Sys.Date()),"_report")
    file_N <- length(list.files(path=DataFilePath,pattern=basefn)) + 1
    FileName <- concat(basefn,data_file_delim,file_N,".", data_file_default_suffix)
    write.table (Report,file=FileName,quote=FALSE,sep="\t")
  }
  return(Report)
}

is_sample_complete <- function(x) {  # x is one list sample item in sample_img_list
  N_img_complete <- sum(x$ImgComplete)
  r<- N_img_complete == length(x$Img) # if all images are complete, the sample is complete
  r <- r || (N_img_complete >= MAX_IMG_PER_SAMPLE) # if more than the max # is scored, it is complete
  N <- length(x$IsGood)
  r <- r || (N >= MAX_N_PER_SAMPLE) # if more than the max # data points is scored, the sample is complete
  if (!r) {
    r <- ((N > MIN_N_PER_SAMPLE) && (N_img_complete > MIN_IMG_PER_SAMPLE)) # sample is complete if we have BOTH scored more than the req'd number of pix AND the req'd number of data points
  }
  return(r)
}  

list_img_complete <- function () {
  print(lapply (sample_img_files, function(X){X$ImgComplete}))
}

save_counts <- function (FileName=NULL) {
  headers <- data_file_headers
  output_data <- NULL
  for (i in sample_names) {
    if (sample_img_files[[i]]$NCounts == 0) {
      tmp <- NULL
      tmp <- matrix (NA, nrow=length(sample_img_files[[i]]$Img),ncol=length(headers),dimnames=list(NULL,headers))
      tmp[,"File"] <- sample_img_files[[i]]$Img
      tmp[,"Sample"] <- i
      tmp[,"Complete"] <- FALSE
    } else {
      tmp <- NULL
      tmp <- matrix (NA, nrow=length(sample_img_files[[i]]$ImgID),ncol=length(headers),dimnames=list(NULL,headers))
      tmp[,"Sample"] <- i      
      tmp[,"File"] <- sample_img_files[[i]]$Img[sample_img_files[[i]]$ImgID]
      tmp[,"State"] <- sample_img_files[[i]]$IsGood
      tmp[,"CoordX"] <- sample_img_files[[i]]$CoordX      
      tmp[,"CoordY"] <- sample_img_files[[i]]$CoordY            
      tmp[,"User"] <- sample_img_files[[i]]$User            
      tmp[,"Date"] <- sample_img_files[[i]]$Date
      tmp[,"Complete"] <- sample_img_files[[i]]$ImgComplete[sample_img_files[[i]]$ImgID]
      # and those that haven't been scored (yet)
      morefiles <- sample_img_files[[i]]$Img[!((1:length(sample_img_files[[i]]$Img)) %in% sample_img_files[[i]]$ImgID)]
      if (length(morefiles > 0)){
        tmp2 <- NULL
        tmp2 <- matrix (NA, nrow=length(morefiles),ncol=length(headers),dimnames=list(NULL,headers))
        tmp2[,"File"] <- morefiles
        tmp2[,"Sample"] <- i
        tmp2[,"Complete"] <- FALSE
        tmp <- rbind (tmp, tmp2)
      }
    }
    output_data <- rbind (output_data, tmp)
  }
  stopifnot (is.null(output_data) == FALSE)
  if (is.null(FileName)){
    basefn <- concat (data_file_default_stem, data_file_delim, CURRENT_USER,data_file_delim,as.character(Sys.Date()))
    file_N <- length(list.files(path=DataFilePath,pattern=basefn)) + 1
    FileName <- concat(basefn,data_file_delim,file_N,".", data_file_default_suffix)
  }
  write.table (output_data,file=FileName,quote=FALSE,sep="\t",row.names=FALSE)
  return (FileName)
}

load_counts <- function (FileName=NULL,User=CURRENT_USER,ToGlobal=TRUE) {
  if (is.null(FileName)) {
    # if no filename is provided, we look for the most recent version for the selected user
    basefn <- concat (data_file_default_stem, data_file_delim, User, data_file_delim)
    files <- list.files(path=DataFilePath,pattern=basefn)
    files_split <- strsplit (files,data_file_delim)
    dates <- unlist(lapply (files_split,function (x) {return(x[3])}))
    dates <- sort(unique(dates))
    basefn <- concat(basefn,dates[length(dates)])
    files <- list.files(path=DataFilePath,pattern=basefn)
    files_split <- strsplit (files,data_file_delim)
    files_split <- unlist(lapply (files_split,function (x) {return(x[4])}))
    file_version <- as.integer(unlist(lapply(strsplit (files_split,".",fixed=TRUE),function (x) {return(x[1])})))
    FileName <- concat(basefn,data_file_delim,max(file_version),".",data_file_default_suffix)
  }
  stopifnot(file.exists(FileName)==TRUE)
  print (concat("Loading file: ",FileName))
  # Reading data file
  data <- read.delim (FileName,header=TRUE,colClasses = c("character","character","logical","numeric","numeric","character","character","logical"),na.strings="NA")
  #str(data)
  
  #Parsing data file
  #v3.1: sample_img_files now repopulated in the same order as the original!!!
  x_sample_names <- NULL
  x_sample_names <- unique(sort(data$Sample))
  X <- NULL
  for (i in x_sample_names) {
    X[[i]]$Img <- unique(sort(data$File[data$Sample == i]))
    X[[i]]$NCounts <- 0
    X[[i]]$ImgComplete <- rep (FALSE, length(X[[i]]$Img))
    for (k in (1:length(X[[i]]$Img))) {
      k_file <- X[[i]]$Img[k]
      X[[i]]$ImgComplete[k] <- (sum(data$Complete[data$File == k_file])>0)
    }
    for (j in 1:length(X[[i]]$Img)) {
      sel_lines <- (data$File == X[[i]]$Img[j])
      NAs <- sum(is.na(data$State[sel_lines]))
      if (NAs==0) {
        X[[i]]$CoordX <- c(X[[i]]$CoordX, data$CoordX[sel_lines])
        X[[i]]$CoordY <- c(X[[i]]$CoordY, data$CoordY[sel_lines])
        X[[i]]$IsGood <- c(X[[i]]$IsGood,data$State[sel_lines]) # V03 -- fixed serious bug. Data were added in the wrong order!
	X[[i]]$ImgID  <- c(X[[i]]$ImgID, rep(j,sum(sel_lines)))
        X[[i]]$User   <- c(X[[i]]$User,data$User[sel_lines])
        X[[i]]$Date   <- c(X[[i]]$Date,data$Date[sel_lines])
        X[[i]]$NCounts <- X[[i]]$NCounts + sum(sel_lines) - NAs
      }
    }
  }
  stopifnot(data_file_headers == colnames(data))
  if (ToGlobal) {
    sample_img_files <<-X
    sample_names <<- x_sample_names
    return(TRUE)
  } else {
    return (X)
  }
}

compare_lists <- function (CMP, REF) {
  stopifnot(length(CMP)>0)
  nErr <- 0
  for (i in 1:length(CMP)) {
    things_to_evaluate <- names(CMP[[i]])
    E <- sum(names(CMP[[i]]) != things_to_evaluate)
# this error check is mostly useless, since the full list of attributes isn't populated until all images are analyzed, and before then they won't match for all samples... hence why the assignment of things_to_evaluate was moved inside the loop
    if (E >0) {print("ERROR: Inconsistent items on CMP")}
    nErr <- nErr + E
    E <- sum(names(REF[[i]]) != things_to_evaluate)
    if (E >0) {print("ERROR: Inconsistent items CMP <> REF")}
    nErr <- nErr + E    
    for (k in things_to_evaluate){
      E <- sum(names(REF[[i]]) != things_to_evaluate)
      if (E > 0){print(concat("ERROR: Mismatch CMP <> REF at sample= ",i," attribute= ",k))}
      nErr <- nErr + E
    }
  }
  return(nErr)
}

validate_load_save <- function () {
  print("Starting load/save validation check.")
  ref_sample_names <- sample_names
  ref_sample_img_files <- sample_img_files
  ref_Smp <- Smp
  ref_SImg <- SImg
  ref_curr_sample <- curr_sample
  ref_curr_file <- curr_file
  ref_curr_jpg <- curr_jpg
  ref_curr_jpg_dim <- curr_jpg_dim
  fn <- "___tmp.tsv"
  save_counts(fn)  
    OK <- (compare_lists (ref_sample_img_files, sample_img_files) == 0)
    if (OK) {print ("Saving files worked fine!")}
    if (!OK) {print ("Saving files scRewed something up")}
  sample_img_files <<- NULL
  sample_names <<- NULL
  Smp <<- 0
  SImg <<- 0
  curr_sample <<- NULL
  curr_file <<- NULL
  curr_jpg <<- NULL
  curr_jpg_dim <<- NULL
  load_counts(fn)
  OK2 <- OK && (compare_lists (ref_sample_img_files, sample_img_files) == 0)
  gr_next_img (DoNotPlot=TRUE)
  if (!OK2) {print ("Loading files scRewed something up MAJORLY")}
  if (OK2) {print ("Everything loaded just fine.")}
  print(curr_sample)
  print(curr_file)
  print(curr_jpg_dim)
  if (sum(ref_sample_names != sample_names)>0) {print("Loading ERROR - sample_names")} else {print ("sample_names OK")}
  if (ref_Smp != Smp) {print("Loading ERROR - Smp")} else {print("Smp OK")}
  if (ref_SImg != SImg) {print("Loading ERROR - SImg")} else {print("SImg OK")}
  if (ref_curr_sample != curr_sample) {print("Loading ERROR - curr_sample")} else {print("curr_sample OK")}
  if (ref_curr_file != curr_file) {print("Loading ERROR - curr_file")} else {print("curr_file OK")}
  if (ref_curr_jpg != curr_jpg) {print("Loading ERROR - curr_jpg")} else {print("curr_jpg OK")}
  if (sum(ref_curr_jpg_dim != curr_jpg_dim)>0) {print("Loading ERROR - curr_jpg_dim")} else {print("curr_jpg_dim OK")}
  print("Load/save validation check complete.")
  file.remove(fn)
  return(OK2)
}

switch_debugging <- function () {
  if (DEBUG_MESSAGES) {
     DEBUG_MESSAGES <<- FALSE
     print("Debugging functions disabled")
     print(DEBUG_MESSAGES)
  } else {
     DEBUG_MESSAGES <<- TRUE
     print("Debugging functions enabled")
     print(DEBUG_MESSAGES)
     }
}


###### Functions for interactive scoring
  getX <- function (x) {grconvertX(x, "ndc", "user")}
  getY <- function (y) {grconvertY(y, "ndc", "user")}

  plot_jpg <- function (jpg_image){
     jpg_dim <- dim(jpg_image)
     curr_jpg_dim <<- jpg_dim
     plot(0:max(jpg_dim),type="n",xlab="",ylab="")
     rasterImage(jpg_image,0,0,jpg_dim[2],jpg_dim[1]) # readJPEG doesn't work with just bare plot
  }

    
  plot_img <- function (image_file, PlotRecords=FALSE) {
    if (DEBUG_MESSAGES) {print (concat("Plot img: ",image_file))}
    if (image_file != curr_file) {
      if (DEBUG_MESSAGES) {print ("Current file and image file not the same!")}
      curr_file <<- image_file
      curr_jpg <<- readJPEG (curr_file,native=TRUE)
      for (i in 1:length(sample_img_files)) {
        if (length(which (sample_img_files[[i]]$Img == curr_file))!=0) {
          curr_sample <<- names(sample_img_files[i])
          break()
        }
      }
    }
    curr_jpg <<- readJPEG (curr_file,native=TRUE)
#    plot (curr_jpg)
     plot_jpg (curr_jpg)	
    if (PlotRecords) {
      print ("Plotting records...")
      id <- which(sample_img_files[[curr_sample]]$Img == curr_file)
      sel_ids <- which(sample_img_files[[curr_sample]]$ImgID == id)
      xx <- sample_img_files[[curr_sample]]$CoordX[sel_ids]
      yy <- sample_img_files[[curr_sample]]$CoordY[sel_ids]
      pt <- as.integer(sample_img_files[[curr_sample]]$IsGood[sel_ids])+1
      cl <- c("red","blue")[pt]
      pc <- c(4,3)[pt]
      points (xx,yy,pch=pc,col=cl)
    }
  }
  
  gr_info <- function (){
    print (concat("Sample: ",curr_sample," File: ",curr_file))
  }

  gr_print_progress <- function (){
    rpt <-make_report()
    print (concat ("N Pix scored in current sample: ",rpt[curr_sample,"N.Pix.Scored"], ". Total N Pix scored: ",sum(rpt[,"N.Pix.Scored"]),". Total samples with pix scored: ", sum(rpt[,"N.Pix.Scored"]>0)))
  }

  gr_toggle_random_mode <- function (){
    RANDOM_NEXT <<- !RANDOM_NEXT
    print (concat("Random mode: ",RANDOM_NEXT))
  }

  gr_plot_title<- function  () {
    title (main=curr_sample,sub=curr_file)
  }

  gr_print_help <- function (){
    print("HELP ^(;;)^ !!!")
    print("Enter - image complete/done, go to next image")
    #print("Right - proceed to next image")
    #print("PgDn - proceed to next sample")
    #print("Left - show last image")
    print("r - Toggle Random/List mode")
    print("i - show image info")
    print("p - show progress")
    print("t - show image info in title")
    print("s - save to next file")
    print("l - load from/revert to last file")
    print("v - verify load/save functions are working properly")
    print("d - turn debugging functions on/off")
    print("w - write a summary report")
    print("q/Esc - quit recording")
    print("c - clear all data points for current image")
    print("h - help")
    print("x/Del - delete last point recorded in image")
  }

  gr_delete_last_rec <- function (){
    if (sample_img_files[[Smp]]$NCounts == 0) {return()}
    N <- sample_img_files[[Smp]]$NCounts
    print (concat("Deleting sample data point ",N))
    # visually delete (sort-of) last record by plotting over it
    points (sample_img_files[[Smp]]$CoordX[N],sample_img_files[[Smp]]$CoordY[N],pch=8,col="black")
    # delete data point from record
    delete_sample_img_rec(N)
  }

  gr_next_img <- function (follow_list = !RANDOM_NEXT, DoNotPlot=FALSE) {
    # NB this does not yet take into account different users
    samples_complete <- unlist(lapply (sample_img_files,is_sample_complete))
    if (sum(!samples_complete)==0){
      # ALL IS DONE
      print ("Scoring finished -- press q")
      text (getX(0.5),getY(0.4),col="red","Scoring finshed -- press q")
      return()
    }
    samples_to_do <- sample_names[!samples_complete]
    if (follow_list) {
      # follow list
      curr_sample <<- samples_to_do[1]
      curr_file <<- sample_img_files[[curr_sample]]$Img[min(which (!sample_img_files[[curr_sample]]$ImgComplete))]
    } else {
      # random next file
      rn <- round(runif (min =1, max = length( samples_to_do), n=1),0)
      curr_sample <<- samples_to_do[rn]
      cand_img <- which (!sample_img_files[[curr_sample]]$ImgComplete)
      rn <- round(runif (min =1, max = length( cand_img), n=1),0)
      curr_file <<- sample_img_files[[curr_sample]]$Img[rn]
    }
    Smp <<- which(sample_names == curr_sample)
    SImg  <<- which(sample_img_files[[curr_sample]]$Img == curr_file)
    curr_jpg <<- readJPEG (curr_file,native=TRUE)
    curr_jpg_dim <<- dim(curr_jpg)
    if (!DoNotPlot) {
       plot_jpg (curr_jpg)
    }
  }

  gr_save_to_file <- function (FileName = NULL) {
     fn <- save_counts (FileName)    
    if (DEBUG_MESSAGES) {list_img_complete()}
    print (concat("Saved records to file ",fn))
  }
  
  gr_revert_to_saved <- function (FileName = NULL) {
    load_counts (FileName=NULL,User=CURRENT_USER,ToGlobal=TRUE) 
    gr_next_img (DoNotPlot = TRUE) # will set curr_file etc.
    if (DEBUG_MESSAGES) (print(concat("current file: ",curr_file)))
    image_file <- curr_file
    plot_img (image_file, PlotRecords=TRUE) 
  }

  gr_img_complete_load_next <- function (follow_list = !RANDOM_NEXT) {
    if (DEBUG_MESSAGES) {print(concat("Hooray, image #",SImg,", ",curr_file,", of ",curr_sample," is complete"))}
    sample_img_files[[Smp]]$ImgComplete[SImg] <<- TRUE
    if (DEBUG_MESSAGES) {list_img_complete()}
    gr_next_img (follow_list)
  }

  gr_reset_current_img <- function (){
    print ("Resetting current image -- records cleared")
    clear_img_recs()
    plot_img (curr_file, PlotRecords=FALSE)
  }

  keydown_handler <- function(key) {
    if(DEBUG_MESSAGES) {print (concat("key: ",key))}
    if (key == EscapeKey) {key <- "q"}
    if (key == "\033") {key <- "q"}
    if (key == ReturnKey) {key <- "__ENTER__"}
    if (key == "q") {
      print("Exiting")
      return(invisible(1))
    }
    switch (key,
            "i" = gr_info(),
            "p" = gr_print_progress(),
            "x" = gr_delete_last_rec(),
	    "\177" = gr_delete_last_rec(),
            "Del" = gr_delete_last_rec(),
            "h" = gr_print_help(),
            "c" = gr_reset_current_img(),
            "r" = gr_toggle_random_mode(),
            "t" = gr_plot_title(),
            "__ENTER__" = gr_img_complete_load_next(),
	    "\r" = gr_img_complete_load_next(),
            #"Right"= print ("To-do: proceed to next image"),
            #"PgDn" = print ("To-do: proceed to next sample"),
            #"Left" = print ("To-do: show last image"),
            "s" = gr_save_to_file (),
            "l" = gr_revert_to_saved (),
	    "v" = validate_load_save (),
	    "d" = switch_debugging (),
	    "w" = make_report ()
    )
    NULL
  }
  
  mousedown_handler <- function(buttons, x, y) {

   if(DEBUG_MESSAGES) {print (buttons)} # for debugging Linux button issues

   # this if-clause fixes the fact that R in Linux reads right-mouse as c(0,1) rather than just 2
   if (length(buttons) != 1) {
      buttons <- 2
   }

    Colour <- switch (buttons+1, "blue", NULL, "red")
    if (is.null(Colour)) {return()}
    Sym <- switch (buttons+1, 3, NULL, 4)
    IsOK <- switch (buttons+1, TRUE, NULL, FALSE)
    if (DEBUG_MESSAGES) {print (concat(curr_sample,",",curr_file,",",floor(getX(x)),",",floor(getY(y)),",",buttons,",",Colour,",",IsOK))}
#    write (concat(curr_sample,",",curr_file,",",x,",",y,",",buttons,",",Colour),file="seedcount-data",append=TRUE) # for testing purposes only
    # Plotting point, test to make sure it's within the image itself before plotting and recording!
    if(0 <= getX(x) & getX(x) <= curr_jpg_dim[2] & 0 <= getY(y) & getY(y) <= curr_jpg_dim[1]) {
        points (getX(x),getY(y),pch=Sym,col=Colour,cex=1.5)
    # Recording point
        sample_img_files[[Smp]]$NCounts <<- sample_img_files[[Smp]]$NCounts+1
        sample_img_files[[Smp]]$CoordX  <<- c(sample_img_files[[Smp]]$CoordX,getX(x))
	sample_img_files[[Smp]]$CoordY  <<- c(sample_img_files[[Smp]]$CoordY,getY(y))
	sample_img_files[[Smp]]$IsGood  <<- c(sample_img_files[[Smp]]$IsGood,IsOK)
    	sample_img_files[[Smp]]$ImgID   <<- c(sample_img_files[[Smp]]$ImgID, SImg)
    	sample_img_files[[Smp]]$User    <<- CURRENT_USER
    	sample_img_files[[Smp]]$Date    <<- c(sample_img_files[[Smp]]$Date,as.character(Sys.time()))   
    	if (is_sample_complete(sample_img_files[[Smp]])) {text (getX(0.5),getY(0.5),col="blue","Sample is complete - press Enter to finalise.")}
    } else {
        if (DEBUG_MESSAGES) {print ("Clicked outside the area!")}
    }
    NULL
  }


########## Main ####################

### Setting up


currpath <- getwd()
systype <- Sys.info()["sysname"]  # needed for OS-specific options to call the right plot window
#library(biOps) # biOps is deprecated, use jpeg instead
library(jpeg)
if (file.exists(username_def_file)) {CURRENT_USER <- readLines(username_def_file)[1]} # Added inV03
stopifnot (CURRENT_USER != "X") # WE WANT USER INFO!

# Selecting image files
img_list <- list.files (PixPath, pattern=concat(".",PixSuffix)) # files will contain PixSuffix but perhaps not at end of filename
img_list <- img_list[substr (img_list, nchar(img_list) - nchar(PixSuffix), nchar(img_list)) == concat(".",PixSuffix)] # only use those with suffix at end


# Parse out image file / sample info and store it in a list / intialise list
sample_names <- sort(unique(unlist(lapply(strsplit (img_list,FileNameSampleDelimiter,fixed=TRUE),function(x){return(x[FileNamePartContainingSampleInfo])}))))
sample_img_files <- NULL # <- this list will contain all the relevant info
for (Smp in sample_names) {
  current_sample_part <- concat(Smp,FileNameSampleDelimiter)
  rec_init(Smp, as.character(img_list[grep (current_sample_part,img_list)]) )
}  

Smp <- 1 
SImg <- 1
curr_file <- sample_img_files[[Smp]]$Img[SImg]
curr_sample <- sample_names[Smp]


### Starting Interactive plotting


if (systype %in% c("Linux","Darwin")) {
  # V03 -- so far not tested on Mac!  
   X11(type = "Xlib")
   winOpt <- X11.options()
} else {
   windows()
   winOpt <- windows.options()
}


  plot_img(curr_file)
  getGraphicsEvent (prompt="Recording data... press q to quit.",onMouseDown = mousedown_handler,onKeybd = keydown_handler)
  
  setGraphicsEventHandlers(onMouseDown=NULL,onMouseUp=NULL,onMouseMove=NULL,prompt=NULL)

dev.off()








