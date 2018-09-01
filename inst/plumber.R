#* @apiTitle BSSEmsembleR
#* @apiDescription a plumber back-end for real-time emsemble modelling

# ------ Imports -------- #



# ------ Utilities -------- #

queryByID <- function(obid,field='_id'){
  return(paste('{"',field,'" : {"$oid":"',obid,'"}}',sep=""))
}

queryByUserID <- function(obid){
  return(queryByID(obid,field='user'))
}

getSafe<- function(x, i, default, is.valid=function(x){T}) {
  i <- match(i, names(x))
  if (is.na(i)) {
    default
  } else if(!is.valid(x[[i]])) {
    default
  }else{
    x[[i]]
  }
}

saveUserFileID <- function(col,userid,fileid){
  obid <- OBID()
  toinsert<-paste('{"_id":{"$oid":"',obid,'"},"user":{"$oid":"',userid,'"},"file":{"$oid":"',fileid,'"}}',sep="")
  col$insert(toinsert)
  return(obid)
}

MultipartDataset2GridFS <- function(req,grid){
  form <- Rook::Multipart$parse(req)
  if(!grepl(".RData",form$file$filename))
    stop("Input file is not a valid .RData file.")
  else{
    upload <-grid$write(form$file$tempfile,form$file$filename)
    return(upload$id)
  }

}

MultipartModel2GridFS <- function(req,grid){
  form <- Rook::Multipart$parse(req)
  if(!grepl(".RDS",form$file$filename))
    stop("Input file is not a valid .RDS file.")
  else{
    upload <-grid$write(form$file$tempfile,form$file$filename)
    return(upload$id)
  }

}

getConfig <- function(req,userid){
  form <- jsonlite::fromJSON(req$postBody)
  obid<-OBID()
  fields<-list("_id" = list("$oid" = jsonlite::unbox(obid)),"user" = list("$oid" = jsonlite::unbox(userid)))
  fields$methods<-getSafe(form,'methods','base',function(x){is.character(x)})
  fields$metric<-getSafe(form,'metric','rmse',function(x){is.character(x) && length(x)==1})
  fields$maximise<-getSafe(form,'maximise',FALSE,function(x){is.logical(x) && length(x)==1})
  fields$tune_length<-getSafe(form,'tune_length',5,function(x){is.integer(x) && length(x)==1 && x>0})
  fields$preprocess<-getSafe(form,'preprocess',c('nzv'),function(x){all(is.character(x))})
  fields$thresh<-getSafe(form,'thresh',0.95,function(x){is.numeric(x) && length(x)==1 && x>0 && x<1})
  fields$pca_comp<-getSafe(form,'pca_comp',NA,function(x){is.integer(x) && length(x)==1 && x>0})
  fields$ica_comp<-getSafe(form,'ica_comp',5,function(x){is.integer(x) && length(x)==1 && x>0})
  fields$k<-getSafe(form,'k',5,function(x){is.integer(x) && length(x)==1 && x>0})
  fields$fudge<-getSafe(form,'fudge',0.2,function(x){is.numeric(x) && length(x)==1 && x>0 && x<1})
  fields$num_unique<-getSafe(form,'num_unique',3,function(x){is.integer(x) && length(x)==1 && x>0})
  fields$freq_cut<-getSafe(form,'freq_cut',19,function(x){is.integer(x) && length(x)==1 && x>0})
  fields$unique_cut<-getSafe(form,'unique_cut',10,function(x){is.integer(x) && length(x)==1 && x>0})
  fields$cut_off<-getSafe(form,'cut_off',0.9,function(x){is.numeric(x) && length(x)==1 && x>0 && x<1})
  fields$cv_method<-getSafe(form,'cv_method','repeatedcv',function(x){is.character(x) && length(x)==1})
  fields$number<-getSafe(form,'number',5,function(x){is.integer(x) && length(x)==1 && x>0})
  fields$repeats<-getSafe(form,'repeats',5,function(x){is.integer(x) && length(x)==1 && x>0})
  fields$p<-getSafe(form,'p',0.75,function(x){is.numeric(x) && length(x)==1 && x>0 && x<1})
  fields$search<-getSafe(form,'search','grid',function(x){is.character(x) && length(x)==1 && x %in% c('grid','random')})
  fields$initial_window<-getSafe(form,'initial_window',NA,function(x){is.integer(x) && length(x)==1 && x>0})
  fields$horizon<-getSafe(form,'horizon',10,function(x){is.integer(x) && length(x)==1 && x>0})
  fields$fixed_window<-getSafe(form,'fixed_window',TRUE,function(x){is.logical(x) && length(x)==1})
  fields$skip<-getSafe(form,'skip',0,function(x){is.integer(x) && length(x)==1})
  fields$adaptive_min<-getSafe(form,'adaptive_min',5,function(x){is.integer(x) && length(x)==1 && x>0})
  fields$adaptive_alpha<-getSafe(form,'adaptive_alpha',0.05,function(x){is.numeric(x) && length(x)==1 && x>0 && x<1})
  fields$adaptive_method<-getSafe(form,'adaptive_method','gls',function(x){is.character(x) && length(x)==1 && x %in% c('gls','BT')})
  fields$adaptive_complete<-getSafe(form,'adaptive_complete',TRUE,function(x){is.logical(x) && length(x)==1})
  return(fields)
}

getFileGridFS <- function(grid,fileID){
  t <- tempfile()
  out <- grid$read(paste0("id:", fileID),t, progress = FALSE)
  return(t)
}

OBID <- function(){
  ei <- as.hexmode(as.integer(Sys.time())) # 4-byte
  mi <- as.hexmode(6666666) #3-byte (I don't really care about the machine suplying this)
  pi <- as.hexmode(Sys.getpid()) # 2-byte
  ci <- as.hexmode(sample(1048576:16777215,1)) # 3-byte
  return(paste0(ei,mi,pi,ci))
}

getFileIDByObjectID<- function(col,obid){
  return(col$find(queryByID(obid),'{"file":1,"_id":0}')$file)
}

getDatasetValidation <- function(file){

  tryCatch({

    out<-list()
    load(file)
    X<-as.data.frame(X)
    Y<-as.data.frame(Y)

    #X Validation
    stopifnot(ncol(X)>2)
    stopifnot(nrow(X)>0)
    stopifnot(class(X[,1])=="integer")
    stopifnot(class(X[,2])=="factor")
    stopifnot(all(sapply(X[,3:ncol(X)], is.numeric)))

    #Y validation
    stopifnot(ncol(Y)>0)
    stopifnot(nrow(Y)>0)
    stopifnot(class(Y[,1]) %in% c("numeric","integer"))

    #mutual validation
    stopifnot(nrow(X)==nrow(Y))
    complete_observations<-sum((complete.cases(X) & complete.cases(Y)))
    stopifnot(complete_observations>0)
    out$valid<-T
    out$msg<-''
    return(out)
  },
  error=function(e){
    out$valid <- F
    out$msg <- as.character(e)
    return(out)
  })

}

getHtmlSummary <- function(df){
  st<- summarytools::dfSummary(df, round.digits = 3)
  stv<- summarytools::view(st,method='render',transpose =T,style="rmarkdown")
  html<- htmltools::renderTags(stv)$html
  return(html)
}

getHtmlDescriptive <-function(df){
  st<- summarytools::descr(df)
  stv<- summarytools::view(st,method='render',transpose =T,style="rmarkdown")
  return( htmltools::renderTags(stv)$html)
}

getHtmlBatchSummary <-function(df,cla){
  lapply(split(df,cla),getHtmlDescriptive)
}

tryDo<-function(exec=function(){},end=function(){}){
  tryCatch(exec,warning=function(w){},error=function(e){},finnaly=end)
}

getDatasetSummary <- function(file){

  XSummary <-''
  XBatchSummary<-''
  YSummary <-''
  YBatchSummary<-''

  tryDo({load(file)})
  tryDo(X<-as.data.frame(X))
  tryDo(Y<-as.data.frame(Y))
  tryDo({XSummary<-getHtmlSummary(X)})
  tryDo({XBatchSummary<-getHtmlBatchSummary(X,X[,2])})
  tryDo({YSummary<-getHtmlSummary(Y)})
  tryDo({YSummary<-getHtmlBatchSummary(Y,X[,2])})
  lst<-list(XSummary,XBatchSummary,YSummary,YBatchSummary)
  names(lst)<-c('XSummary','XBatchSummary','YSummary','YBatchSummary')
  return(lst)
}
getModelSummary <-function(file){
  props<-''
  specs<-''
  tryDo({model<-readRDS(file)})
  tryDo({props<-getModelProperties(model)})
  tryDo({specs<-getModelSpecs(model)})
  return(list("Properties"=props,"Specifications"=specs))
}
getModelProperties <-function(model){
  lst<-list()
  if(isCaretLegacy(model)){
    tryDo({lst$meta <- 'Caret Legacy Model'})
    tryDo({lst$method <- model$method})
    tryDo({lst$type <- model$modelType})
    tryDo({lst$results <- dplyr::inner_join(model$results,model$bestTune)})
    tryDo({lst$metric <- model$metric})
    tryDo({lst$maximize <- model$maximize})
    tryDo({lst$control <- model$control})
    tryDo({lst$control$index <- NULL})
    tryDo({lst$control$indexOut <- NULL})
    tryDo({lst$control$seeds <- NULL})
    tryDo({lst$preprocess <- names(model$preProcess$method)})
    tryDo({lst$performance <- model$perfNames})
    tryDo({lst$ylimits <- model$yLimits})
  }else if(isCaretStack(model)){
    lst$meta <- 'Caret Stack Model'
    getStackProperties(list,model)
  }else if(isCaretEnsemble(model)){
    lst$meta <- 'Caret Ensemble Model'
    getEnsembleProperties(list,model)
  }else{
    lst$meta <- "Not a valid caret legacy model, caret Ensemble or caret Stack model."
  }
  return(lst)
}
isCaretLegacy <- function(model){
  inherits(model,"train")
}
isCaretStack<- function(model){
  inherits(model,"caretStack") && !inherits(model,'caretEnsemble')
}
isCaretEnsemble <- function(model){
  inherits(model,"caretEnsemble")
}


#-------- Routes -------- #

# -- Validation -- #

#TODO:// allow users : should be a filter

# -- AVAILABLE -- #

#* Get list of available datasets for a user
#* @param userid userid for which the available datasets ids will be retrieved
#* @get /datasets/available
function(userid){
  query<-queryByUserID(userid)
  fields<-'{"_id":1}'
  .GlobalEnv$datasets$find(query,fields)
}

#* Get list of available configs for a user
#* @param userid userid for which the available configs ids will be retrieved
#* @get /configs/available
function(userid){
  query<-queryByUserID(userid)
  fields<-'{"_id":1}'
  .GlobalEnv$configs$find(query,fields)
}

#* Get list of available models for a user
#* @param userid userid for which the available models ids will be retrieved
#* @get /models/available
function(userid){
  query<-queryByUserID(userid)
  fields<-'{"_id":1}'
  .GlobalEnv$models$find(query,fields)
}

# -- Load -- #

#* Loads dataset file in BSSEmsembler
#* @param userid userid corresponding to the owner of the dataset
#* @post /datasets/load
function(req,userid){
  fileid <- MultipartDataset2GridFS(req,.GlobalEnv$gridFS)
  saveUserFileID(.GlobalEnv$datasets,userid,fileid)
}

#* Loads configuration into Configs collection of BSSEmsembleR (as JSON object?)
#* @param userid userid corresponding to the owner of the config
#* @post /configs/load
function(req,userid){
  lista<-getConfig(req,userid)
  .GlobalEnv$configs$insert(jsonlite::toJSON(lista))
  return(lista$'_id'$'$oid')
}

#* Loads model file in BSSEmsembler
#* @param userid userid corresponding to the owner of the model
#* @post /models/load
function(req,userid){
  fileid <- MultipartModel2GridFS(req,.GlobalEnv$gridFS)
  saveUserFileID(.GlobalEnv$models,userid,fileid)
}

# -- MODEL -- # GOAL IS TO CREATE EMSEMBLE MODELS

#TODO://takes 1 dataset and one config and produces one model

# -- PREDICT -- # CREATE SESSION AND PREDICT ON THAT SECTION

#TODO://takes 1 dataset and activatesit and allow user to send vector of doubles and get predictions

# -- INFO -- # RETRIEVE INFORMATION OVER A DATASET, CONFIGURATION AND MODEL

#* Gets dataset information in BSSEmsembler
#* @param datasetid corresponding to the dataset which the information will be retrieved
#* @get /datasets/info
function(datasetid){
  fileid <- getFileIDByObjectID(.GlobalEnv$datasets,datasetid)
  file <- getFileGridFS(.GlobalEnv$gridFS, fileid)
  summary <- getDatasetSummary(file)
  validation <- getDatasetValidation(file)
  unlink(file)
  toreturn<-list(summary,validation)
  names(toreturn)<-c('Summary','Validation')
  return(toreturn)
}

#* Gets config information in BSSEmsembler
#* @param configsid corresponding to the config document which the information will be retrieved
#* @get /configs/info
function(configid){
  return(.GlobalEnv$configs$find(queryByID(configid)))
}

#* Gets model information in BSSEmsembler
#* @param modelid corresponding to the model which the information will be retrieved
#* @get /models/info
function(modelsid){
  fileid <- getFileIDByObjectID(.GlobalEnv$models,modelid)
  file <- getFileGridFS(.GlobalEnv$gridFS, modesid)
  summary <- getModelSummary(file)
  validation <- getModelValidation(file)
  unlink(file)
  toreturn <- list(summary,validation)
  names(toreturn)<-c('Summary','Validation')
  return(toreturn)
}
