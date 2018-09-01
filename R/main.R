#' BSSEmsembleR Server
#'
#' Starts the BSSEmsembleR Server.
#'
#' @param port int port to bind plumber. Default: 8080
#' @param https boolean https secure encryption. Default: False
#' @param url str mongolite url parameter. Default: 'mongodb://localhost'
#' @param options object mongolite options parameter. Default: None
#'
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
#'
#' @export
startServer <- function(port=8080,https=F,url='mongodb://localhost',options=NULL) {

  users <- mongolite::mongo(collection="users", db="bssemsembler", url=url, options = options)
  assign('users',users,envir = .GlobalEnv)

  models <- mongolite::mongo(collection="models", db="bssemsembler", url=url, options = options)
  assign('models',models,envir = .GlobalEnv)

  datasets <- mongolite::mongo(collection="datasets", db="bssemsembler", url=url, options = options)
  assign('datasets',datasets,envir = .GlobalEnv)

  configs <- mongolite::mongo(collection="configs", db="bssemsembler", url=url, options = options)
  assign('configs',configs,envir = .GlobalEnv)

  gridFS <- mongolite::gridfs(prefix="fs",db="bssemsembler",url=url,options=options)
  assign('gridFS',gridFS,envir = .GlobalEnv)


  assign('BSSEPID',Sys.getpid(),envir=.GlobalEnv)
  r <- plumber::plumb(system.file('plumber.R',package = "BSSEnsembleR"))
  r$run(port=port)
}
#
#killServer <- function(){
  #bssl <- system("tskill",arg=.GlobalEnv$BSSEPID, stout=TRUE, sterr = FALSE)
#}
