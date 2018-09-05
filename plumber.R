#* @apiTitle BSSEnsembleR
#* @apiDescription a plumber back-end for real-time ensemble modelling

# ------ Imports -------- #



# ------ Utilities -------- #

queryByID <- function(obid,field='_id'){
  q<-list(list("$oid" = unbox(obid)))
  names(q)<-field
  return(jsonlite::toJSON(q))
}
queryByUserID <- function(obid){
  queryByID(obid,field='user')
}


getUserByID<-function(userid){
  .GlobalEnv$users$find(queryByID(userid),'{}')
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
tryDo<-function(exec=function(){},end=function(){}){
  tryCatch(exec,warning=function(w){},error=function(e){},finnaly=end)
}
saveUserFileID <- function(col,userid,fileid){
  obid <- OBID()
  toinsert<-paste0('{"_id":{"$oid":"',obid,'"},"user":{"$oid":"',userid,'"},"file":{"$oid":"',fileid,'"}}')
  col$insert(toinsert)
  return(obid)
}
getFileGridFS <- function(grid,fileID){
  t <- tempfile()
  out <- grid$read(paste0("id:", fileID),t, progress = FALSE)
  return(t)
}
getFileIDByObjectID<- function(col,obid){
  return(col$find(queryByID(obid),'{"file":1,"_id":0}')$file)
}



MultipartDataset2GridFS <- function(req,grid){
  form <- Rook::Multipart$parse(req)
  testit::assert("Input file is not a valid .RData file.",{!grepl(".RData",form$file$filename)})
  getDatasetValidation(form$file$tempfile)
  upload <-grid$write(form$file$tempfile,form$file$filename)
  return(list(fileid = upload$id, userid = form$userid))
}
MultipartModel2GridFS <- function(req,grid){
  form <- Rook::Multipart$parse(req)
  if(!grepl(".RDS",form$file$filename))
    stop("Input file is not a valid .RDS file.")
  else{
    getTokenValidation(form)
    analysis <-getModelValidation(form$file$filename)
    if(!analysis$Valid){
      error(analysis$Message)
    }else{
      upload <-grid$write(form$file$tempfile,form$file$filename)
      return(upload$id)
    }
  }

}
Json2Config <- function(req,userid){
  form <- jsonlite::fromJSON(req$postBody)
  obid<-OBID()
  analysis<-getConfigValidation(form)
  if(!analysis$Valid){
    error(analysis$Message)
  }else{
    ids<-list("_id" = list("$oid" = jsonlite::unbox(obid)),"user" = list("$oid" = jsonlite::unbox(userid)))
    append(ids,form)
  }
}
getConfig <- function(configid){
  .GlobalEnv$configs$find(queryByID(configid))
}

getTokenValidation<-function(body){
  testit::assert('Invalid security token.',{isValidString(body$userid)})
  testit::assert('Invalid security token.',{isValidString(body$token)})
  user <- getUserByID(body$userid)
  testit::assert('Invalid security token',{!is.null(user)})
  testit::assert('Invalid security token.', {bcrypt::checkpw(user$hash[[1]],body$token)})
}

classNumber <- function(x){
  return(inherits(x,'numeric') || inherits(x,'integer'))
}
getDatasetValidation <- function(file){


    load(file)
    X<-as.data.frame(X)
    Y<-as.data.frame(Y)

    #X Validation
    testit::assert(paste0('X has insufficient number of predictors inputs:',as.character(ncol(X))),{ncol(X)>2})
    testit::assert(paste0('X has insufficient number of observations:',as.character(nrow(X))),{nrow(X)>0})
    testit::assert(paste0('Firts column of X is class ',class(X[,1]),', and cannot be coerced to integer class.'),!testit::has_error({as.integer(X[,1])}))
    testit::assert(paste0('Second column of X is class ',class(X[,2]),', and cannot be coerced to factor class.'),!testit::has_error({as.factor(X[,2])}))
    testit::assert('All supplied predictors inputs, except for column one and two, should be of integer or numeric class.',{all(sapply(X[,3:ncol(X)], classNumber))})

    #Y validation
    testit::assert(paste0('Y has insufficient number of predictors outputs:',as.character(ncol(Y))),{ncol(Y)>0})
    testit::assert(paste0('Y has insufficient number of observations:',as.character(nrow(Y))),{nrow(Y)>0})
    testit::assert('The Supplied predictor output should be of integer or numeric class.',{classNumber(Y[,1])})

    #mutual validation
    testit::assert(paste0('X number of observations (',as.character(nrow(X)),') differs from Y (',as.character(nrow(Y)),').'),{nrow(X)!=nrow(Y)})
    testit::assert('X and Y have independent number of NA or null observations.',{sum((complete.cases(X) & complete.cases(Y)))>0})


}
getDatasetSummary <- function(file){

  XSummary <- NULL
  XBatchSummary <- NULL
  YSummary <- NULL
  YBatchSummary <- NULL

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

getModelValidation <-function(file){
  tryCatch({
    out<-list()
    testit::assert('Could not load the model from .RDS file',!testit::has_error({model<-readRDS(file)}))

    #is one of the valid models
    testit::assert('model is not a valid train, caretEnsemble or caretStack object.',{isCaretLegacy(model) || isCaretStack(model) || isCaretEnsemble(model)})

    #is a regression model
    testit::assert('model is not a valid Regression model.',validRegressionModel(model))

    out$Valid<-T
    out$Message<-NULL
    return(out)
  },
  error=function(e){
    out$Valid <- F
    out$Message <- as.character(e)
    return(out)
  })
}
getModelSummary <-function(file){
  props<-NULL
  specs<-NULL
  tryDo({model<-readRDS(file)})
  tryDo({props<-getModelProperties(model)})
  tryDo({specs<-getModelSpecs(model)})
  return(list("Properties"=props,"Specifications"=specs))
}
validRegressionModel<-function(model){
  if(isCaretLegacy(model)){
    isRegression(model)
  }else if(isCaretStack(model) || isCaretEnsemble(model)){
    validRegressionSE(model)
  }
}
validRegressionSE<-function(model){
  res<-unique(lapply(model$models,isRegression))
  if(length(res)!=1){
    F
  }else{
    res[[1]]
  }
}
isRegression <- function(model){
  model$modelType == 'Regression'
}
getModelProperties <-function(model){
  lst <- list()
  if(isCaretLegacy(model)){
    tryDo({lst<-extractProperties(model)})
    tryDo({lst$meta <- 'Caret Legacy Model'})
  }else if(isCaretStack(model)){
    tryDo({lst<-getSEProperties(model)})
    tryDo({lst$stack <- extractProperties(model$ens_model)})
    tryDo({lst$error <- model$error})
    tryDo({lst$meta <- 'Caret Stack Model'})
  }else if(isCaretEnsemble(model)){
    tryDo({lst<-getSEProperties(model)})
    tryDo({lst$ensemble <- extractProperties(model$ens_model)})
    tryDo({lst$error <- model$error})
    lst$meta <- 'Caret Ensemble Model'
  }
  return(lst)
}
extractProperties <- function(model){
  lst<-list()
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
  return(lst)
}
getSEProperties <-function(model){
  lapply(model$models,extractProperties)
}
getModelSpecs <- function(model){
  if(isCaretLegacy(model)){
    model$finalModel$xNames
  }else if(isCaretStack(model) || isCaretEnsemble(model)){
    getSESpecs(model)
  }
}
getSESpecs<-function(model){
  listaf<-lapply(model$models, getSafe, i='finalModel')
  listax<-lapply(listaf,getSafe,i='xNames')
  res <- unique(listax)
  if(length(res)==1){
    return(unlist(res))
  }else{
    return(NULL)
  }
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

is01 <- function(x){
  x>0 && x<0
}
ispos <- function(x){
  x>0
}
isvar <- function(x){
  length(x)==1
}
getConfigValidation <-function(c){
  tryCatch({
    out<-list()
    testit::assert('"legacy" parameter must be a logical.', {is.logical(c$legacy)})
    testit::assert('"stack" parameter must be a logical.', {is.logical(c$stack)})
    testit::assert('"ensemble" parameter must be a logical.', {is.logical(c$ensemble)})
    testit::assert('Multiple meta types of models selected.', {sum(c$legacy,c$stack,c$ensemble) ==1})
    testit::assert('Invalid submodels selection.',{
      (length(c$methods)>0 && all(c$methods %in% names(.GlobalEnv$BSSEModels)))
    })

    if(c$legacy){
      # essemble asssertions
      testit::assert('Multiple submodels selected for legacy model.', {isvar(c$methods)})
    }else if(c$ensemble){
      testit::assert('Select at least 2 submodels for building a greedy ensemble model.', {length(c$methods)>1},{all(c$method%in%names(.GlobalEnv$BSSEModels))})

      testit::assert('"ensemble_metric" parameter must be a single valid metric character.',{is.var(c$ensemble_metric)},{c$ensemble_metric%in%.GlobalEnv$BSSEMetrics})
      testit::assert('"ensemble_maximize" parameter must be a valid logical.',{isvar(c$ensemble_maximize)},{is.logical(c$ensemble_maximize)})

      #ensemble train control
      testit::assert('"ensemble_cv_method" parameter must be a valid cross validation character.',{isvar(c$ensemble_cv_method)},{all(c$ensemble_cv_method%in%.GlobalEnv$BSSECrossvalidation)})
      testit::assert('"ensemble_cv_method" parameter = "none" requires the ensemble_tune_length parameter to be = 1.',{!(c$ensemble_cv_method=="none" && c$ensemble_tune_length !=1)})
      testit::assert('"ensemble_number" parameter must be a valid positive integer.',{isvar(c$ensemble_number)},{is.integer(c$ensemble_number)},{ispos(c$ensemble_number)})
      testit::assert('"ensemble_repeats" parameter must be a valid positive integer.',{isvar(c$ensemble_repeats)},{is.integer(c$ensemble_repeats)},{ispos(c$ensemble_repeats)})
      testit::assert('"ensemble_p_locv" parameter must be a between 0-1.',{isvar(c$ensemble_p_locv)},{is01(c$ensemble_p_locv)})
      testit::assert('"ensemble_search" parameter must be a valid (grid,random) character.',{isvar(c$ensemble_search)},{all(c$ensemble_search%in%c('grid','random'))})

      #ensemble slices
      testit::assert('"ensemble_initial_window" parameter must be NA or positive integer.',{is.var(c$ensemble_initial_window)},{is.null(c$ensemble_initial_window) ||  is.integer(c$ensemble_initial_window) && ispos(c$ensemble_initial_window)})
      testit::assert('"ensemble_horizon" parameter must be a valid positive integer.',{isvar(horizon)},{is.integer(c$ensemble_horizon)},{ispos(c$ensemble_horizon)})
      testit::assert('"ensemble_fixed_window" parameter must be a valid logical.',{isvar(c$ensemble_fixed_window)},{is.logical(c$ensemble_fixed_window)})
      testit::assert('"ensemble_skip" parameter must be a 0 or valid integer.',{isvar(c$ensemble_skip)},{c$ensemble_skip==0 || (is.integer(c$ensemble_skip)&&ispos(c$ensemble_skip))})

      #ensemble adaptive
      testit::assert('"ensemble_adaptive_method" parameter must be a valid (gls,BT) character.',{isvar(c$ensemble_adaptive_method)},{all(c$ensemble_adaptive_method%in% c('gls','BT'))})
      testit::assert('"ensemble_adaptive_alpha" parameter must be a between 0-1.',{isvar(c$ensemble_adaptive_alpha)},{is01(c$ensemble_adaptive_alpha)})
      testit::assert('"ensemble_adaptive_complete" parameter must be a valid logical.',{isvar(c$ensemble_adaptive_complete)},{is.logical(c$ensemble_adaptive_complete)})
      testit::assert('"ensemble_adaptive_min" parameter must be a valid positive integer.',{isvar(c$ensemble_adaptive_min)},{is.integer(c$ensemble_adaptive_min)},{ispos(c$ensemble_adaptive_min)})

    }else if(c$stack){
      testit::assert('Select at least 2 submodels for building a stack model.', {length(c$methods)>1},{all(c$method%in%names(.GlobalEnv$BSSEModels))})
      testit::assert('Stack model require one valid ensemble method parameter.',{isvar(c$ensemble_method)},{all(c$ensemble_method%in%names(.GlobalEnv$BSSEModels))})

      testit::assert('"ensemble_metric" parameter must be a single valid metric character.',{is.var(c$ensemble_metric)},{c$ensemble_metric%in%.GlobalEnv$BSSEMetrics})
      testit::assert('"ensemble_maximize" parameter must be a valid logical.',{isvar(c$ensemble_maximize)},{is.logical(c$ensemble_maximize)})

      #ensemble train control
      testit::assert('"ensemble_cv_method" parameter must be a valid cross validation character.',{isvar(c$ensemble_cv_method)},{all(c$ensemble_cv_method%in%.GlobalEnv$BSSECrossvalidation)})
      testit::assert('"ensemble_cv_method" parameter = "none" requires the ensemble_tune_length parameter to be = 1.',{!(c$ensemble_cv_method=="none" && c$ensemble_tune_length !=1)})
      testit::assert('"ensemble_number" parameter must be a valid positive integer.',{isvar(c$ensemble_number)},{is.integer(c$ensemble_number)},{ispos(c$ensemble_number)})
      testit::assert('"ensemble_repeats" parameter must be a valid positive integer.',{isvar(c$ensemble_repeats)},{is.integer(c$ensemble_repeats)},{ispos(c$ensemble_repeats)})
      testit::assert('"ensemble_p_locv" parameter must be a between 0-1.',{isvar(c$ensemble_p_locv)},{is01(c$ensemble_p_locv)})
      testit::assert('"ensemble_search" parameter must be a valid (grid,random) character.',{isvar(c$ensemble_search)},{all(c$ensemble_search%in%c('grid','random'))})

      #ensemble slices
      testit::assert('"ensemble_initial_window" parameter must be NA or positive integer.',{is.var(c$ensemble_initial_window)},{is.null(c$ensemble_initial_window) ||  is.integer(c$ensemble_initial_window) && ispos(c$ensemble_initial_window)})
      testit::assert('"ensemble_horizon" parameter must be a valid positive integer.',{isvar(horizon)},{is.integer(c$ensemble_horizon)},{ispos(c$ensemble_horizon)})
      testit::assert('"ensemble_fixed_window" parameter must be a valid logical.',{isvar(c$ensemble_fixed_window)},{is.logical(c$ensemble_fixed_window)})
      testit::assert('"ensemble_skip" parameter must be a 0 or valid integer.',{isvar(c$ensemble_skip)},{c$ensemble_skip==0 || (is.integer(c$ensemble_skip)&&ispos(c$ensemble_skip))})

      #ensemble adaptive
      testit::assert('"ensemble_adaptive_method" parameter must be a valid (gls,BT) character.',{isvar(c$ensemble_adaptive_method)},{all(c$ensemble_adaptive_method%in% c('gls','BT'))})
      testit::assert('"ensemble_adaptive_alpha" parameter must be a between 0-1.',{isvar(c$ensemble_adaptive_alpha)},{is01(c$ensemble_adaptive_alpha)})
      testit::assert('"ensemble_adaptive_complete" parameter must be a valid logical.',{isvar(c$ensemble_adaptive_complete)},{is.logical(c$ensemble_adaptive_complete)})
      testit::assert('"ensemble_adaptive_min" parameter must be a valid positive integer.',{isvar(c$ensemble_adaptive_min)},{is.integer(c$ensemble_adaptive_min)},{ispos(c$ensemble_adaptive_min)})

    }

    #general assertions
    #Train
    testit::assert('"metric" parameter must be a single valid metric character.',{is.var(c$metric)},{c$metric%in%.GlobalEnv$BSSEMetrics})
    testit::assert('"maximize" parameter must be a valid logical.',{isvar(c$maximize)},{is.logical(c$maximize)})
    testit::assert('"preprocess_methods" parameter must be a NULL or valid preprocessing characters.',{(is.var(c$preprocessing_methods) && is.null(c$preprocessing_methods)) || all(c$preprocessing_methods%in%.GlobalEnv$BSSEPreprocessing)})
    testit::assert('"tune_length" parameter must be a valid positive integer.',{isvar(c$tune_length)},{is.integer(c$tune_length)},{ispos(c$tune_length)})

    #preprocessing
    testit::assert('"thresh" parameter must be a between 0-1.',{isvar(c$thresh)},{is01(c$thresh)})
    testit::assert('"pca_comp" parameter must be NA or positive integer.',{is.var(c$pca_comp)},{is.null(c$pca_com) ||  is.integer(c$pca_comp) && ispos(c$pca_comp)})
    testit::assert('"k" parameter must be a valid positive integer.',{isvar(c$k)},{is.integer(c$k)},{ispos(c$k)})
    testit::assert('"fudge" parameter must be a between 0-1.',{isvar(c$fudge)},{is01(c$fudge)})
    testit::assert('"num_unique" parameter must be a valid positive integer.',{isvar(c$num_unique)},{is.integer(c$num_unique)},{ispos(c$num_unique)})
    testit::assert('"freq_cut" parameter must be a valid positive integer.',{isvar(c$freq_cut)},{is.integer(c$freq_cut)},{ispos(c$freq_cut)})
    testit::assert('"unique_cut" parameter must be a valid positive integer.',{isvar(c$unique_cut)},{is.integer(c$unique_cut)},{ispos(c$unique_cut)})
    testit::assert('"cut_off" parameter must be a between 0-1.',{isvar(c$cut_off)},{is01(c$cut_off)})
    testit::assert('"range_bounds" must be a numerical tuple.',{length(c$range_bounds)==2},{classNumber(c$range_bounds[1])&&classNumber(c$range_bounds[2])})

    #ica
    testit::assert('"ica_comp" parameter must be a valid positive integer.',{isvar(c$ica_comp)},{is.integer(c$ica_comp)},{ispos(c$ica_comp)})
    testit::assert('"ica_alg_type" parameter must be a valid ica algorithm type character (parallel,deflation).',{isvar(c$ica_alg_type)},{c$ica_alg_type%in%c('parallel','deflation')})
    testit::assert('"ica_fun" parameter must be a valid ica approximation function character (logcosh,exp).',{isvar(c$ica_fun)},{c$ica_fun%in%c('logcosh','exp')})
    testit::assert('"ica_alpha" parameter must be between [1,2]',{isvar(c$ica_alpha)},{classNumber(c$ica_alpha)},{c$ica_alpha>=1 && c$ica_alpha<=2})
    testit::assert('"ica_row_norm" parameter must be a logical',{isvar(c$ica_row_norm)},{is.logical(c$ica_row_norm)})
    testit::assert('"ica_maxit" parameter must be a valid positive integer.',{isvar(c$ica_maxit)},{is.integer(c$ica_maxit)},{ispos(c$ica_maxit)})
    testit::assert('"ica_tol" parameter must be a between 0-1.',{isvar(c$ica_tol)},{is01(c$ica_tol)})

    #train control
    testit::assert('"cv_method" parameter must be a valid cross validation character.',{isvar(c$cv_method)},{all(c$cv_method%in%.GlobalEnv$BSSECrossvalidation)})
    testit::assert('"cv_method" parameter = "none" requires the tune_length parameter to be = 1.',{!(c$cv_method=="none" && c$tune_length !=1)})
    testit::assert('"number" parameter must be a valid positive integer.',{isvar(c$number)},{is.integer(c$number)},{ispos(c$number)})
    testit::assert('"repeats" parameter must be a valid positive integer.',{isvar(c$repeats)},{is.integer(c$repeats)},{ispos(c$repeats)})
    testit::assert('"p_locv" parameter must be a between 0-1.',{isvar(c$p_locv)},{is01(c$p_locv)})
    testit::assert('"search" parameter must be a valid (grid,random) character.',{isvar(c$search)},{all(c$search%in%c('grid','random'))})

    #slices
    testit::assert('"initial_window" parameter must be NA or positive integer.',{is.var(c$initial_window)},{is.null(c$initial_window) ||  is.integer(c$initial_window) && ispos(c$initial_window)})
    testit::assert('"horizon" parameter must be a valid positive integer.',{isvar(horizon)},{is.integer(c$horizon)},{ispos(c$horizon)})
    testit::assert('"fixed_window" parameter must be a valid logical.',{isvar(c$fixed_window)},{is.logical(c$fixed_window)})
    testit::assert('"skip" parameter must be a 0 or valid integer.',{isvar(c$skip)},{c$skip==0 || (is.integer(c$skip)&&ispos(c$skip))})

    #adaptive
    testit::assert('"adaptive_method" parameter must be a valid (gls,BT) character.',{isvar(c$adaptive_method)},{all(c$adaptive_method%in% c('gls','BT'))})
    testit::assert('"adaptive_alpha" parameter must be a between 0-1.',{isvar(c$adaptive_alpha)},{is01(c$adaptive_alpha)})
    testit::assert('"adaptive_complete" parameter must be a valid logical.',{isvar(c$adaptive_complete)},{is.logical(c$adaptive_complete)})
    testit::assert('"adaptive_min" parameter must be a valid positive integer.',{isvar(c$adaptive_min)},{is.integer(c$adaptive_min)},{ispos(c$adaptive_min)})

    #others
    testit::assert('"p" parameter must be a between 0-1.',{isvar(c$p)},{is01(c$p)})
    testit::assert('"seed" parameter must be a valid positive integer.',{isvar(c$seed)},{is.integer(c$seed)},{ispos(c$seed)})
    testit::assert('"parallel" parameter must be a valid logical.',{isvar(c$parallel)},{is.logical(c$parallel)})
    testit::assert('"cores" parameter must be a valid positive integer.',{isvar(c$cores)},{is.integer(c$cores)},{ispos(c$cores)})

    out$Valid<-T
    out$Message<-NULL
    return(out)
  },
  error=function(e){
    out$Valid <- F
    out$Message <- as.character(e)
    return(out)
  })
}

buildModel<-function(X,Y,config){
  if(config$legacy){
    buildLegacy(X,Y,config)
  }else if(config$ensemble){
    buildEnsemble(X,Y,config)
  }else if(config$stack){
    buildStack(X,Y,config)
  }else{
    error('Failed to detect which meta type of model to build.')
  }
}
buildLegacy<-function(X,Y,config){
  model<-NULL
  tryCatch({

    if(config$parallel){
      cl <- parallel::makePSOCKcluster(config$cores)
      doParallel::registerDoParallel(cl)
    }

    set.seed(config$seed)

    idx_train <- caret::createDataPartition(X[2],times=1,p=config$p,list=FALSE)
    idx_test <- (1:nrow(X))[-idx_train]

    control <- caret::trainControl(
      method = config$cv_method,
      number = config$number,
      repeats = config$repeats,
      p = config$p_locv,
      search = config$search,
      initialWindow = config$initial_window,
      horizon = config$horizon,
      fixedWindow = config$fixed_window,
      skip= config$skip,
      verboseIter = F,
      returnData = T,
      returnResamp = "all",
      savePredictions = T,
      classProbs = F,
      summaryFunction = RegressionAbsoluteSummary,
      selectionFunction = config$selection,
      preProcOptions = getPreProcessOptions(config),
      sampling=config$sampling,
      timingSamples =0,
      indexFinal = idx_test,
      adaptive = list('min'=config$adaptive_min,'alpha'=config$adaptive_alpha,'method'=config$adaptive_method,'complete'=config$adaptive_complete),
      trim=F,
      allowParallel = config$parallel)


    model<-train(x = getPredictors(X,config),
          y = Y,
          method = config$methods,
          preProcess = getPreprocessMethods(config),
          metrics = config$metric,
          maximise = config$maximize,
          trControl = control,
          tuneLength =  config$tune_length,
          na.action = na.omit)


  },finally = {
    if(config$parallel){
      parallel::stopCluster(cl)
    }
    })
  return(model)
}
buildStack<-function(X,Y,config){
  model<-NULL
  tryCatch({

    if(config$parallel){
      cl <- parallel::makePSOCKcluster(config$cores)
      doParallel::registerDoParallel(cl)
    }

    set.seed(config$seed)

    idx_train <- caret::createDataPartition(X[2],times=1,p=config$p,list=FALSE)
    idx_test <- (1:nrow(X))[-idx_train]

    submodel_control <- caret::trainControl(
      method = config$cv_method,
      number = config$number,
      repeats = config$repeats,
      p = config$p_locv,
      search = config$search,
      initialWindow = config$initial_window,
      horizon = config$horizon,
      fixedWindow = config$fixed_window,
      skip= config$skip,
      verboseIter = F,
      returnData = T,
      returnResamp = "all",
      savePredictions = T,
      classProbs = F,
      summaryFunction = RegressionAbsoluteSummary,
      selectionFunction = config$selection_function,
      preProcOptions = getPreProcessOptions(config),
      sampling=config$sampling,
      timingSamples =0,
      indexFinal = idx_test,
      adaptive = list('min'=config$adaptive_min,'alpha'=config$adaptive_alpha,'method'=config$adaptive_method,'complete'=config$adaptive_complete),
      trim=F,
      allowParallel = config$parallel
    )

    ensemble_list <- caretEnsemble::caretList(
      x=getPredictors(X,config),y=Y,
      trControl=sub_control,
      tuneList = getEnsembleModelSpecsList(config),
      continue_on_fail = F
    )

    ensemble_control<-caret::trainControl(
      method = config$ensemble_cv_method,
      number = config$ensemble_number,
      repeats = config$ensemble_repeats,
      p = config$ensemble_p_locv,
      search = config$ensemble_search,
      initialWindow = config$ensemble_initial_window,
      horizon = config$ensemble_horizon,
      fixedWindow = config$ensemble_fixed_window,
      skip= config$ensemble_skip,
      verboseIter = F,
      returnData = T,
      returnResamp = "all",
      savePredictions = T,
      classProbs = F,
      summaryFunction = RegressionAbsoluteSummary,
      selectionFunction = config$ensemble_selection_function,
      sampling=config$sampling,
      timingSamples =0,
      adaptive = list('min'=config$ensemble_adaptive_min,'alpha'=config$ensemble_adaptive_alpha,'method'=config$ensemble_adaptive_method,'complete'=config$ensemble_adaptive_complete),
      trim=F,
      allowParallel = config$parallel
    )

    model <- caretEnsemble::caretStack(
      ensemble_list,
      method=config$ensemble_method,
      metric=config$ensemble_metric,
      maximize=config$ensemble_maximize,
      trControl=ensemble_control
    )


  },finally = {
    if(config$parallel){
      parallel::stopCluster(cl)
    }
  })
}
buildEnsemble<-function(X,Y,config){
  model<-NULL
  tryCatch({

    if(config$parallel){
      cl <- parallel::makePSOCKcluster(config$cores)
      doParallel::registerDoParallel(cl)
    }

    set.seed(config$seed)

    idx_train <- caret::createDataPartition(X[2],times=1,p=config$p,list=FALSE)
    idx_test <- (1:nrow(X))[-idx_train]

    submodel_control <- caret::trainControl(
      method = config$cv_method,
      number = config$number,
      repeats = config$repeats,
      p = config$p_locv,
      search = config$search,
      initialWindow = config$initial_window,
      horizon = config$horizon,
      fixedWindow = config$fixed_window,
      skip= config$skip,
      verboseIter = F,
      returnData = T,
      returnResamp = "all",
      savePredictions = T,
      classProbs = F,
      summaryFunction = RegressionAbsoluteSummary,
      selectionFunction = config$selection_function,
      preProcOptions = getPreProcessOptions(config),
      sampling=config$sampling,
      timingSamples =0,
      indexFinal = idx_test,
      adaptive = list('min'=config$adaptive_min,'alpha'=config$adaptive_alpha,'method'=config$adaptive_method,'complete'=config$adaptive_complete),
      trim=F,
      allowParallel = config$parallel
    )

    ensemble_list <- caretEnsemble::caretList(
      x=getPredictors(X,config),y=Y,
      trControl=sub_control,
      tuneList = getEnsembleModelSpecsList(config),
      continue_on_fail = F
    )

    ensemble_control<-caret::trainControl(
      method = config$ensemble_cv_method,
      number = config$ensemble_number,
      repeats = config$ensemble_repeats,
      p = config$ensemble_p_locv,
      search = config$ensemble_search,
      initialWindow = config$ensemble_initial_window,
      horizon = config$ensemble_horizon,
      fixedWindow = config$ensemble_fixed_window,
      skip= config$ensemble_skip,
      verboseIter = F,
      returnData = T,
      returnResamp = "all",
      savePredictions = T,
      classProbs = F,
      summaryFunction = RegressionAbsoluteSummary,
      selectionFunction = config$ensemble_selection_function,
      sampling=config$sampling,
      timingSamples =0,
      adaptive = list('min'=config$ensemble_adaptive_min,'alpha'=config$ensemble_adaptive_alpha,'method'=config$ensemble_adaptive_method,'complete'=config$ensemble_adaptive_complete),
      trim=F,
      allowParallel = config$parallel
    )

    model <- caretEnsemble::caretEnsemble(
      ensemble_list,
      metric=config$ensemble_metric,
      maximize=config$ensemble_maximize,
      trControl=ensemble_control
    )


  },finally = {
    if(config$parallel){
      parallel::stopCluster(cl)
    }
  })
}

getPreprocessMethods<-function(config){
  pre<-config$preprocess_methods
  if(length(pre)==0 || is.na(pre) || pre=="" || is.null(pre)){
    return(NULL)
  }else{
    return(pre)
  }
}
getPreProcessOptions<-function(config){

  pre<-list()
  pre$thresh <- config$thresh
  pre$pcaComp <- config$pca_comp
  pre$k <- config$k
  pre$knnSummary <- mean
  pre$fudge <- config$fudge
  pre$numUnique <- config$num_unique
  pre$freqCut <- config$freq_cut
  pre$uniqueCut <- config$unique_cut
  pre$cutoff <- config$cut_off
  pre$rangeBounds <- config$range_bounds
  pre$n.comp <- config$ica_comp
  pre$alg.typ <- config$ica_alg_type
  pre$fun <- config$ica_fun
  pre$alpha <- config$ica_alpha
  pre$method <- config$ica_method
  pre$row.norm <- config$ica_row_norm
  pre$maxit <- config$ica_maxit
  pre$tol <- config$ica_tol

  return(pre)

  # pre$thresh = 0.95
  # pre$pcaComp = NULL
  # pre$na.remove = TRUE
  # pre$k = 5
  # pre$knnSummary = mean
  # pre$outcome = NULL
  # pre$fudge = 0.2
  # pre$numUnique = 3
  # pre$verbose = FALSE
  # pre$freqCut = 95/5
  # pre$uniqueCut = 10
  # pre$cutoff = 0.9
  # pre$rangeBounds = c(0, 1)
  # pre$n.comp
  # pre$alg.typ = c("parallel","deflation")
  # pre$fun = c("logcosh","exp")
  # pre$alpha = 1.0
  # pre$method = c("R","C")
  # pre$row.norm = FALSE
  # pre$maxit =
  # pre$tol = 1e-04
  # pre$verbose = FALSE
  # pre$w.init = NULL

}
getPredictors <- function(X,config){
  if(config$use_time){
    return(X[-2,drop=F])
  }else{
    return(X[-(1:2),drop=F])
  }
}
tryMetric <- function(obs,pred,fun){
  metric<-NA
  tryDo({metric<-fun(obs,pred)})
  return(metric)
}
RegressionAbsoluteSummary <- function(data, lev = NULL, model = NULL){
  pred <- data[,"pred",drop = T]
  obs  <- data[,"obs", drop = T]
  prob_stats <- c(
  tryMetric(obs,pred,Metrics::ae),
  tryMetric(obs,pred,Metrics::ape),
  tryMetric(obs,pred,Metrics::bias),
  tryMetric(obs,pred,Metrics::mae),
  tryMetric(obs,pred,Metrics::mape),
  tryMetric(obs,pred,Metrics::mase),
  tryMetric(obs,pred,Metrics::mdae),
  tryMetric(obs,pred,Metrics::mse),
  tryMetric(obs,pred,Metrics::msle),
  tryMetric(obs,pred,Metrics::percent_bias),
  tryMetric(obs,pred,Metrics::rae),
  tryMetric(obs,pred,Metrics::rmse),
  tryMetric(obs,pred,Metrics::rmsle),
  tryMetric(obs,pred,Metrics::rrse),
  tryMetric(obs,pred,Metrics::rse),
  tryMetric(obs,pred,Metrics::se),
  tryMetric(obs,pred,Metrics::sle),
  tryMetric(obs,pred,Metrics::smape),
  tryMetric(obs,pred,Metrics::sse))
  names(prob_stats) <- c(
    "ae",
    "ape",
    "bias",
    "mae",
    "mape",
    "mase",
    "mdae",
    "mse",
    "msle",
    "percent_bias",
    "rae",
    "rmse",
    "rmsle",
    "rrse",
    "rse",
    "se",
    "sle",
    "smape",
    "sse"
  );
  }
getEnsembleModelSpec<-function(x,config){
  caretEnsembleModelSpec(method = x, preProcess = getPreprocessMethods(config),
                         metrics = config$metric,
                         maximise = config$maximize,
                         tuneLength = config$tune_length,
                         na.action = na.omit)
}
getEnsembleModelSpecsList <- function(config){

  modelnames <- config$methods
  toreturn<-lapply(modelnames,getEnsembleModelSpec,config)
  names(toreturn)<-modelnames
  return(toreturn)

}

#-------- Routes -------- #

# -----FILTERS ------ #



# -- Validation -- #

#* Allow user to validate in server creating a user document (passwords should not be stored in the database)
#* @preempt tokenizer
#* @post /register
function(req,res){
  body<-jsonlite::fromJSON(req$postBody)
  getRegistrationValidation(body)
  newuser<-createUser(body$username,body$password)
  return(list(userid=newuser$id,token=bcrypt::hashpw(newuser$hash)))
}

#* Initial login validation
#* @preempt tokenizer
#* @post /login
function(req,res){
  body<-jsonlite::fromJSON(req$postBody)
  getLoginValidation(body)
  user<-getUserByUsername(body$username)
  return(list('userid'=user["_id"],'token'=bcrypt::hashpw(user$hash[[1]])))
}






# -- AVAILABLE -- #

#* Get list of available datasets for a user
#* @post /datasets/available
function(req,res){
  body<-jsonlite::fromJSON(req$postBody)
  query<-queryByUserID(body$userid)
  fields<-'{"_id":1}'
  return(list(ids=.GlobalEnv$datasets$find(query,fields)))
}

#* Get list of available configs for a user
#* @post /configs/available
function(req,res){
  body<-jsonlite::fromJSON(req$postBody)
  query<-queryByUserID(body$userid)
  fields<-'{"_id":1}'
  return(list(ids=.GlobalEnv$configs$find(query,fields)))
}

#* Get list of available models for a user
#* @post /models/available
function(userid){
  body<-jsonlite::fromJSON(req$postBody)
  query<-queryByUserID(body$userid)
  fields<-'{"_id":1}'
  return(list(ids=.GlobalEnv$models$find(query,fields)))
}

# -- Load -- #

#* Loads dataset file in BSSEmsembler
#* @preempt tokenizer
#* @param userid
#* @param token
#* @post /datasets/load
function(req,userid,token){
  getTokenValidation(list('userid'=userid,'token'=token))
  ids <- MultipartDataset2GridFS(req,.GlobalEnv$gridFS)
  saveUserFileID(.GlobalEnv$datasets,ids$userid,ids$fileid)
}

#* Loads configuration into Configs collection of BSSEmsembleR (as JSON object?)
#* @param userid userid corresponding to the owner of the config
#* @post /configs/load
function(req,userid){
  lista<-Json2Config(req,userid)
  .GlobalEnv$configs$insert(jsonlite::toJSON(lista))
  return(lista$'_id'$'$oid')
}

#* Loads model file in BSSEmsembler
#* @param userid userid corresponding to the owner of the model
#* @post /models/load
function(req){
  fileid <- MultipartModel2GridFS(req,.GlobalEnv$gridFS)
  saveUserFileID(.GlobalEnv$models,userid,fileid)
}

# -- MODEL -- #

#* Create a new model based on a configuration and dataset ids
#* @param userid corresponding to the user owning the new model.
#* @param datasetid corresponding to the dataset which provide data for the model.
#* @param configid corresponding to the dataset which configures the model.
#* @get /models/new
function(userid,datasetid,configid){

  fileid <- getFileIDByObjectID(datasetid)
  file<- getFileGridFS(.GlobalEnv$gridFS,fileID)

  #Loading (X,Y,config)
  load(file)
  unlink(file)
  config<-getConfig(configid)

  #from config decide which type of model to build
  model <- buildModel(X,Y,config)

  #Saving the model
  temp<-tempfile()
  saveRDS(model,temp)
  upload <-grid$write(temp,paste0('Build_',datasetid,'_',configid))
  saveUserFileID(.GlobalEnv$models,userid,upload$id)
  return(upload$id)

}

# -- PREDICT -- #

#TODO://takes 1 dataset and activatesit and allow user to send vector of doubles and get predictions

# -- INFO -- #

#* Gets dataset information in BSSEmsembler
#* @param datasetid corresponding to the dataset which the information will be retrieved
#* @get /datasets/info
function(datasetid){
  sum <- NULL
  val <- NULL
  pls <-NULL #TODO: CREATE PLOTS FOR THE DATASET LOADED

  fileid <- getFileIDByObjectID(.GlobalEnv$datasets,datasetid)
  file <- getFileGridFS(.GlobalEnv$gridFS, fileid)
  tryDo({sum<-getDatasetSummary(file)})
  tryDo(val<-getDatasetValidation(file))
  #TODO:   tryDo(pls<-getDatasetPlots(file))
  unlink(file)
  return(list('Summary'=sum,'Validation'=val,'Plots'=pls))
}

#* Gets config information in BSSEmsembler
#* @param configsid corresponding to the config document which the information will be retrieved
#* @get /configs/info
function(configid){
  return(getConfig(configid))
}

#* Gets model information in BSSEmsembler
#* @param modelid corresponding to the model which the information will be retrieved
#* @get /models/info
function(modelid){
  sum <- NULL
  val <- NULL
  pls <-NULL #TODO: CREATE PLOTS FOR THE DATASET LOADED

  fileid <- getFileIDByObjectID(.GlobalEnv$models,modelid)
  file <- getFileGridFS(.GlobalEnv$gridFS, fileid)
  tryDo({sum<-getModelSummary(file)})
  tryDo(val<-getModelValidation(file))
  #TODO:   tryDo(pls<-getDatasetPlots(file))
  unlink(file)
  return(list('Summary'=sum,'Validation'=val,'Plots'=pls))
}
