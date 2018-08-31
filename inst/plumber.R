# ------ Imports -------- #



# ------ Utilities -------- #

queryByID <- function(obid,field='_id'){
  return(paste('{"',field,'" : {"$oid":"',obid,'"}}',sep=""))
}

queryByUserID <- function(obid){
  return(queryByID(obid,field='user'))
}

saveUserFileID <- function(col,userid,fileid){
  obid <- OBID()
  toinsert<-paste('{"_id":{"$oid":"',obid,'"},"user":{"$oid":"',userid,'"},"file":{"$oid":"',fileid,'"}}',sep="")
  col$insert(toinsert)
  return(obid)
}

Multipart2GridFS <- function(req,grid){
  formContents <- Rook::Multipart$parse(req)
  if(!grepl(".RData",formContents$file$filename))
    stop("Input file is not a valid .RData file.")
  else{
    upload <-grid$write(formContents$file$tempfile,formContents$file$filename)
    return(upload$id)
  }

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

worst<-function(warn,err){
  if(err != "" ){
    return(err)
  }else if(warn !=""){
    return(warn)
  }else{
    return("")
  }
}

getDatasetValidation <- function(file){

  tryCatch({

    out<-list()

    load(file)
    X<-as.data.frame(X)
    Y<-as.data.frame(Y)

    #X Validation
    stopifnot(ncol[X]<3)
    if(ncol[X]<10){warning('X contains insufficient number of predictors (<10) to develop a valid model.')}
    stopifnot(nrow[X]<1)
    stopifnot(class(X[,1])=="integer")
    stopifnot(class(X[,2])=="factor")
    stopifnot(all(sapply(X[,3:ncol(X)], is.numeric)))

    #Y validation
    stopifnot(ncol[Y]<1)
    stopifnot(nrow[Y]<1)
    stopifnot(class(Y[,1])=="numeric")

    #mutual validation
    stopifnot(ncol(X)==ncol(y))
    complete_observations<-sum((complete.cases(X) & complete.cases(Y)))
    if(complete_observations<50){warning('X and Y contain insufficient number of complete observations (<50) to develop a valid model.')}
    stopifnot(complete_observations<1)
    out$valid<-T
    return(out)
  },
  warning=function(w){
    out$msg <- as.character(w)
  },
  error=function(e){
    out$valid <- F
    out$msg <- as.character(e)
    return(out)
  })

}

getHtmlSummary <- function(df){
  st<-summarytools::dfSummary(df, round.digits = 3)
  stv<-summarytools::view(st,method='render',transpose =T,style="rmarkdown")
  html<-htmltools::renderTags(stv)$html
  return(html)
}

getHtmlDescriptive <-function(df){
  st<-summarytools::descr(df)
  stv<-summarytools::view(st,method='render',transpose =T,style="rmarkdown")
  return(htmltools::renderTags(stv)$html)
}

getHtmlBatchSummary <-function(df,cla){
  lst <- lapply(split(df,cla),getHtmlDescriptive)
  names(lst) <- as.character(cla)
  return(lst)
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
  fileid <- Multipart2GridFS(req,.GlobalEnv$gridFS)
  saveUserFileID(.GlobalEnv$datasets,userid,fileid)
}

#* Loads configuration into Configs collection of BSSEmsembleR (as JSON object?)
#* @param userid userid corresponding to the owner of the config
#* @post /configs/load
function(req,userid){ #TODO:How are we defining the models configuration.


}

#* Loads model file in BSSEmsembler
#* @param userid userid corresponding to the owner of the model
#* @post /models/load
function(req,userid){
  fileid <- Multipart2GridFS(req,.GlobalEnv$gridFS)
  saveUserFileID(.GlobalEnv$models,userid,fileid)
}

# -- MODEL -- # GOAL IS TO CREATE EMSEMBLE MODELS

#TODO://takes 1 dataset and one config and produces one model

# -- PREDICT -- # CREATE SESSION AND PREDICT ON THAT SECTION

#TODO://takes 1 dataset and activatesit and allow user to send vector of doubles and get predictions

# -- INFO -- # RETRIEVE INFORMATION OVER A DATASET, CONFIGURATION AND MODEL

#* Gets dataset information in BSSEmsembler
#* @param datasetid corresponding to the dataset which the information will be retrieved
#* @post /datasets/info
function(datasetid){
  fileid <- getFileIDByObjectID(.GlobalEnv$datasets,datasetid)
  file <- getFileGridFS(.GlobalEnv$gridFS, fileid)
  summary <- getDatasetSummary(file)
  validation <- getDatasetValidation(file)
  unlink(file)
  return(list(summary,validation))
}
