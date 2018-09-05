#* @apiTitle BSSEnsembleR
#* @apiDescription a plumber back-end for real-time ensemble modelling

# ---- GENERICS ------ #
isValidString<-function(x){
  !all(is.null(x) || is.na(x) || !is.atomic(x) || identical(x,"") || !is.character(x))
} #Done

OBID <- function(){ #Done
  ei <- as.hexmode(as.integer(Sys.time())) # 4-byte
  mi <- as.hexmode(6666666) #3-byte (I don't really care about the machine suplying this)
  pi <- as.hexmode(Sys.getpid()) # 2-byte
  ci <- as.hexmode(sample(1048576:16777215,1)) # 3-byte
  return(paste0(ei,mi,pi,ci))
}


assim <- function(exp,msg){
  a<-tryCatch(exp,error=function(e){a<-as.character(e)})
  if(!identical(a,T)){
    if(identical(a,F)){
      stop(paste0("Asserted that ",msg))
    }else{
      stop(paste0("Fail to asssert: ",msg,", cause: ",as.character(a)))
    }
  }
}


# ----- FILTERS ------ #

#* @filter cors
cors <- function(res) { #Done
  res$setHeader("Access-Control-Allow-Origin", "*")
  plumber::forward()
} #Done

#* @filter tokenizer
tokenizer <-function(req){ #Done
  body<-jsonlite::fromJSON(req$postBody)
  assertion<-getTokenValidation(body)
  if(assertion$Valid){
    print(assertion)
    plumber::forward()
  }else{
    out <- list(error = assertion$Message)
    res$status <- 404
    print(out)
    return(out)
  }
}

# ----- QUERIES ---- #
queryByField<-function(obj,field){ #Done
  q<-list(unbox(obj))
  names(q)<-field
  return(jsonlite::toJSON(q))
}

queryByUsername<-function(username){ #Done
  out<-queryByField(username,"username")
  print(out)
  return(out)
}

# ----- GETTERS ---- #
getUserByUsername<-function(username){
  out<-.GlobalEnv$users$find(queryByUsername(username),'{}')#Done
  print(out)
  return(out)
}

# -- HELPERS -- #

createNewUser<-function(username,password){
  id<-OBID()
  hash<-bcrypt::hashpw(password)
  .GlobalEnv$users$insert(jsonlite::toJSON(list("_id"=list("$oid" = jsonlite::unbox(id)),"username"=username,"hash"=hash)))
  out<-list("_id"=id,"username"=username,"hash"=hash)
  return(out)
} #Done #Verified
authorizeUser<-function(user,password){
  nrow(user) == 1 && isValidString(user$hash[[1]]) && bcrypt::checkpw(password, user$hash[[1]])
} #Done #Verified

# -- vALIDATION -- #

getRegistrationValidation <- function(body) {
  tryCatch({
    assim({isValidString(body$username) == T},"username is not valid.")
    assim({isValidString(body$password) == T},"passord is not valid")
    assim({isValidString(body$validation) == T},"password confirmation is not valid.")
    assim({body$password == body$validation},"passwords don't match.")
    assim({body$invitation == .GlobalEnv$BSSEInvitation},"invitation key don't match.")
    assim({length(getUserByUsername(body$username)) == 0},"username already exists.")
    out <- list(Valid = T, Message = '')
    return(out)
  }, error = function(e) {
    out <- list(Valid = F, Message = e)
    return(out)
  })
} #Done Verified

getLoginValidation <- function(body) {
  tryCatch({
    assim({isValidString(body$username)},'username is invalid.')
    assim({isValidString(body$password)},'password is invalid.')
    user<-getUserByUsername(body$username);
    assim({authorizeUser(user,body$password)},'username does not exist or password is wrong.')

    out <- list(Valid = T, Message = '')
    return(out)
  }, error = function(e) {
    out <- list(Valid = F, Message = as.character(e))
    return(out)
  })
} #Done Verified

# -- AUTHENTICATION -- #

#* Allow user to validate in server creating a user document (passwords should not be stored in the database)
#* @preempt tokenizer
#* @post /register
function(req, res) {
  body <- jsonlite::fromJSON(req$postBody)
  assertion <- getRegistrationValidation(body)
  if (assertion$Valid) {
    newuser <- createNewUser(body$username, body$password)
    out <- list(userid = newuser$'_id' ,token = bcrypt::hashpw(newuser$'hash'))
    res$status <- 202
    print(out)
    return(out)
  } else{
    out <- list(error = assertion$Message)
    res$status <- 404
    print(out)
    return(out)
  }
} #Done Verified

#* Initial login validation
#* @preempt tokenizer
#* @post /login
function(req, res) {
  body <- jsonlite::fromJSON(req$postBody)
  assertion <- getLoginValidation(body)
  if (assertion$Valid) {
    user <- getUserByUsername(body$username)
    out <-
      list(userid = user$"_id",
           token = bcrypt::hashpw(user$hash[[1]]))
    res$status <- 202
    print(out)
    return(out)
  } else{
    out <- list(error = assertion$Message)
    res$status <- 404
    print(out)
    return(out)
  }
} #Done
