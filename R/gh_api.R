# Github API

get_gh_query_params <- function(params) {
  query_params <- list(
    client_id=params$gh_id,
    client_secret=params$gh_secret,
    per_page=100)
  return(query_params)
}


get_gh_api_resp <- function (req_url, query_params, access_token, accept_text) {
  if(str_length(access_token) > 0) {
    req_url <- paste0(req_url, "?access_token=", access_token)
  }
  
  req <- GET(req_url, query, accept(accept_text))
  json <- content(req, as = "text")
  df <- fromJSON(json, flatten=TRUE)
}
