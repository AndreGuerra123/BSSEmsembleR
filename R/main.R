#' BSSEn sembleR Server
#'
#' Starts the BSSEnsembleR Server.
#'
#' @param port int port to bind plumber. Default: 8080
#' @param https boolean https secure encryption. Default: False
#' @param url str mongolite url parameter. Default: 'mongodb://localhost'
#' @param options object mongolite options parameter. Default: None
#' @return None
#'
#' @examples
#' startServer()
#'
#' @importFrom  plumber plumb
#' @importFrom  mongolite mongo
#' @importFrom  mongolite gridfs
#' @import jsonlite
#' @import summarytools
#' @import htmltools
#' @import Rook
#' @import caret
#' @import caretEnsemble
#' @import Metrics
#' @import testit
#' @import bcrypt
#'
#' @export
startServer <- function(port=8080,invitation=NULL,url='mongodb://localhost',options=NULL) {

  users <- mongolite::mongo(collection="users", db="bssemsembler", url=url, options = options)
  assign('users',users,envir = .GlobalEnv)

  models <- mongolite::mongo(collection="models", db="bssemsembler", url=url, options = options)
  assign('models',models,envir = .GlobalEnv)

  datasets <- mongolite::mongo(collection="datasets", db="bssemsembler", url=url, options = options)
  assign('datasets',datasets,envir = .GlobalEnv)

  configs <- mongolite::mongo(collection="configs", db="bssemsembler", url=url, options = options)
  assign('configs',configs,envir = .GlobalEnv)

  gridFS <- mongolite::gridfs(prefix="fs",db="bssemsembler",url=url,options=options)
  assign('gridFS',gridFS,envir=.GlobalEnv)

  models<-readRDS(system.file('models.RDS',package = "BSSEnsembleR"))
  assign('BSSEModels',models,envir=.GlobalEnv)

  metrics<-c("ae","ape","bias","mae","mape","mase","mdae","mse","msle","percent_bias","rae","rmse","rmsle","rrse","rse","se","sle","smape","sse")
  assign('BSSEMetrics',metrics,envir=.GlobalEnv)

  preprocessing <- c("BoxCox","YeoJohnson","expoTrans","center","scale","range","knnImpute","bagImpute","medianImpute","pca","ica","spatialSign")
  assign('BSSEPreprocessing',preprocessing,envir=.GlobalEnv)

  cross<-c("boot", "boot632", "optimism_boot","boot_all","cv","repeatedcv","LOOCV","LGOCV","none","oob","timeslice","adaptive_cv","adaptive_boot","adaptive_LGOCV")
  assign('BSSECrossvalidation',cross,envir=.GlobalEnv)

  if(is.null(invitation) ||is.na(invitation) ||  invitation=="" || !is.character(invitation)){
    invitation <- rawToChar(as.raw(sample(c(65:90,97:122), 8, replace=T)))
  }
  print(paste0('Invitation key: ',invitation))
  assign('BSSEInvitation',invitation,envir=.GlobalEnv)
  assign('BSSEPID',Sys.getpid(),envir=.GlobalEnv)

  r <- plumber::plumb(system.file('plumber.R',package = "BSSEnsembleR"))
  r$run(port=port)
}
#
#killServer <- function(){
  #bssl <- system("tskill",arg=.GlobalEnv$BSSEPID, stout=TRUE, sterr = FALSE)
#}
