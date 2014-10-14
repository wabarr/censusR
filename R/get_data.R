#' Function to get data
#'
#' get data
#' @param version Version of the API to use.  Default is "v1"
#' @param base_url Url to use to access API. Useful to override to local server during development. This should probably be removed once API is production ready
#' @param resource API resource to access. Default is occurrence.
#' @param ... An arbitrary number of filter critera in the following form: genusName__contains="Aepycer"
#' @keywords bovid census data
#' @export
#' @examples
#' get_data(source)



get_data <- function(resource = "occurrence", version="v1", base_url="wabarr.com/census", ...) {
  require(httr)
  require(jsonlite)
  require(plyr)
  
  #format the filters as GET parameters
  filter <- paste(
    paste(
      names(list(...)),
      list(...),
      sep="="
    ),
    collapse="&"
  )
  
  URL_parameters = paste0("?format=json", "&", "limit=0", "&", filter)
  
  formattedURL <- paste(
    paste(base_url, "api", version, resource, sep="/"), 
    URL_parameters, 
    sep="/"
  )
  
  attempt <-  httr::GET(url = formattedURL)
  if (attempt$status_code == 401) {
    #if unauthorized, try it with username and api_key
    if(any(!exists('username'), !exists('api_key'))) stop("You need to set your username and api_key using set_credentials().")
    attempt <-  httr::GET(url = sprintf("%s&username=%s&api_key=%s", formattedURL, username, api_key))
  }
  
  if (attempt$status_code == 401) stop (sprintf("User %s with api_key=%s is not authorized to get this data.", username, api_key))
  if (attempt$status_code != 200) stop ("There was an error.  Maybe you mispelled something or tried to filter on a field that doesn't exist?")
  
  theDATA <- httr::content(attempt, as="parsed")
  
  flattened <- lapply(theDATA$objects, FUN = function(object) {
    flat <- as.data.frame(rbind(unlist(object)))
    return(flat)
  })
  result <- plyr::rbind.fill(flattened)
  result <- lapply(result, FUN=function(col){
    if (suppressWarnings(all(!is.na(as.numeric(as.character(col)))))) {
      as.numeric(as.character(col))
    } else {
      col
    }
  })
  return(as.data.frame(result))
}