#* @apiTitle BSSEnsembleR
#* @apiDescription a plumber back-end for real-time ensemble modelling

# ---- GENERICS ------ #
isValidString<-function(x){
  !all(is.null(x) || is.na(x) || !is.atomic(x) || identical(x,"") || !is.character(x))
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
  newid<-OBID()
  newhash<-bcrypt::hashpw(password)
  .GlobalEnv$users$insert(jsonlite::toJSON(list("_id"=list("$oid" = jsonlite::unbox(newid)),"username"=username,"hash"=newhash)))
  out<-list("_id"=newid,"hash"=newhash)
  print(out)
  return(out)
} #Done

# -- vALIDATION -- #
# out<-list(Valid=F,Message=NULL)
# tryCatch({
#   out$Valid<-T
#   out$Message<-''
# },error=function(e){
#   out$Valid<-F
#   out$Message<-as.character(e)
# })
# print(out)
# return(out)

getRegistrationValidation <- function(body) {
  out <- list(Valid = F, Message = NULL)
  tryCatch({
    testit::assert('Invalid username.', {
      isValidString(body$username)
    })
    testit::assert('Invalid password.', {
      isValidString(body$password)
    })
    testit::assert('Invalid password confirmation.', {
      isValidString(body$validation)
    })
    testit::assert('Invalid password confirmation.', {
      body$password == body$validation
    })
    testit::assert('Invalid invitation key.', {
      body$invitation == .GlobalEnv$BSSEInvitation
    })
    user <- getUserByUsername(body$username)
    testit::assert('Invalid username.', {
      length(user) == 0
    })
    out$Valid <- T
    out$Message <- ''
  }, error = function(e) {
    out$Valid <- F
    out$Message <- as.character(e)
  })
  print(out)
  return(out)
} #Done

getLoginValidation <- function(body) {
  out <- list(Valid = F, Message = NULL)
  tryCatch({
    testit::assert('Invalid username.', {
      isValidString(body$username)
    })
    testit::assert('Invalid password.', {
      isValidString(body$password)
    })
    user <- getUserByUsername(body$username)
    testit::assert('Invalid username.', {
      length(user) != 0
    })
    testit::assert('Failed to retrieve hash.', {
      isValidString()
    })
    testit::assert('Invalid password.', {
      bcrypt::checkpw(body$password, user$hash[[1]])
    })
    out$Valid <- T
    out$Message <- ''
  }, error = function(e) {
    out$Valid <- F
    out$Message <- as.character(e)
  })
  print(out)
  return(out)
} #Done

# -- AUTHENTICATION -- #

#* Allow user to validate in server creating a user document (passwords should not be stored in the database)
#* @preempt tokenizer
#* @post /register
function(req, res) {
  body <- jsonlite::fromJSON(req$postBody)
  assertion <- getRegistrationValidation(body)
  if (assertion$Valid) {
    newuser <- createNewUser(body$username, body$password)
    out <- list(userid = newuser$id,
                token = bcrypt::hashpw(newuser$hash))
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
