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

# make a list of domains
gitlog_networks_domains <- bind_rows(gitlog_networks %>% 
                                       select(committer_host, committer_domain) %>% 
                                       rename(host=committer_host, domain=committer_domain), 
                                     gitlog_networks %>% 
                                       select(author_host, author_domain) %>%
                                       rename(host=author_host, domain=author_domain)
) %>% unique()


gitlog_networks_domains <- gitlog_networks_domains %>% 
  group_by(domain) %>% 
  summarise(host=first(host))

# Clean Clearbit Domain Records

clearbit <- tibble()
for (n in 1:nrow(gitlog_networks_domains)) {
  paste(gitlog_networks_domains$host[n])
  clearbit_url <- paste0("https://company.clearbit.com/v2/companies/find?domain=", gitlog_networks_domains$host[n])
  #clearbit_get <- GET(clearbit_url, add_headers(Authorization = clearbit_auth))
  clearbit_get <- GET(clearbit_url, add_headers(Authorization = paste("Bearer", params$clearbit_api_key)))
  if (clearbit_get$status_code == 422) {
    next()
  }
  clearbit_json <- fromJSON(content(clearbit_get, as = "text"), flatten=TRUE)
  clearbit_json$domainAliases = paste(clearbit_json$domainAliases, collapse=",")
  clearbit_json$tags = paste(clearbit_json$tags, collapse=",")
  clearbit_df <-  clearbit_json %>% unlist() %>% as.data.frame.list()
  
  clearbit_df <- clearbit_df %>% 
    mutate(row=n, 
           host_looked_up=gitlog_networks_domains$host[n])
  
  write_rds(clearbit_df, paste0("domain_info/clearbit_df_", gitlog_networks_domains$domain[n],".Rds"))
  clearbit <- bind_rows(clearbit, clearbit_df)
}

# Clearbit data is dirty. Some of the companies listed as private are actually public and have no ticker. Also email hosts are linked to public companies. Type should be "personal" for these.
# 
# * Use the stock ticker to identify public companies that were misidentified by Clearbit as private.
# * Non-US Educational Institutions often use the "ac" suffix
# * Use the .org suffix to identify nonprofits typed as private companies, unless it's already identified as personal
