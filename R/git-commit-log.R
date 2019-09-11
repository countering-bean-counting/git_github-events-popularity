library(stringr)
library(lubridate)
library(urltools)

## Validation

#' Check Repo
#'
#' @param repo 
#'
#' @return boolean
#' @export
#'
#' @examples
check_repo <- function(repo) {
  return(str_detect(repo, "/"))
}

#' Invalid Repo
#'
#' @param repo 
#'
#' @return boolean
#' @export
#'
#' @examples
invalid_repo <- function(repo) {
  stop(paste("Invalid repo provided:", repo))
}

## Parsing Parameters

#' Git URL
#'
#' @param repo 
#'
#' @return char
#' @export
#'
#' @examples
get_git_url <- function(repo) {
  check_repo(repo)
  return(paste0("git@github.com:", repo, ".git"))
}

#' Git Repo Short Name
#'
#' @param repo 
#'
#' @return char
#' @export
#'
#' @examples
get_git_repo_short <- function(repo) {
  check_repo(repo)
  return(str_split(repo, "/")[[1]][2])
}

get_git_clone_path_full <- function(repo, clone_path, repo_type) {
  return(paste(clone_path, repo, "repo", sep="/"))
}

get_git_log_filename <- function(repo) {
  return(paste0("gitlog_", get_git_repo_short(repo), ".txt"))
}

get_git_log_path <- function(repo, clone_path) {
  return(paste(clone_path, repo, get_git_log_filename(repo), sep="/"))
}

## Clone

#' Git Clone Repo Command
#' The consumer should run this command using system() or some variation
#'
#' @param repo
#' @param clone_path
#'
#' @return
#' @export
#'
#' @examples
get_git_clone_repo_cmd <- function(repo, clone_path) {
  return(paste0("git clone ", get_git_url(repo), " ", get_git_clone_path_full(repo, clone_path)))
}

get_git_pull_repo_cmd <- function(repo, clone_path) {
  return(paste0("cd ", get_git_clone_path_full(repo, clone_path), "; git pull"))
}

get_git_log_cmd <- function(repo, clone_path) {
  paste0('cd ', get_git_clone_path_full(repo, clone_path),
         '; git log ',
         ' --no-merges ',
         ' --date=short --pretty=tformat:"%ad|%an|%ae|%cd|%cn|%ce|%H" > ',
         "../", get_git_log_filename(repo))
}

## Reading In (side effects)

gitlog_fix_dates <- function(df) {
  df %>% mutate(
    author_date=as.Date(author_date, tz="UTC"),
    committer_date=as.Date(committer_date, tz="UTC"),
    commit_date=ymd(committer_date),
    commit_year=floor_date(commit_date, "year"),
    commit_halfyear=floor_date(commit_date, "halfyear"),
    commit_quarter=floor_date(commit_date, "quarter"),
    commit_month=floor_date(commit_date, "month"),
    commit_bimonth=floor_date(commit_date, "bimonth"),
    commit_week=floor_date(commit_date, "week")
  )
}

gitlog_extract_emails <- function(df) {
  df %>% 
    mutate(
      author_name=str_to_lower(author_name),
      author_email=str_to_lower(author_email),
      committer_name=str_to_lower(committer_name),
      committer_email=str_to_lower(committer_email)
    ) %>% 
    separate(author_email, c("author_username", "author_host"), sep="@", remove=FALSE) %>%
    separate(committer_email, c("committer_username", "committer_host"), sep="@", remove=FALSE) %>%
    mutate(
      author_domain = suffix_extract(author_host)$domain,
      author_suffix = suffix_extract(author_host)$suffix,
      committer_domain = suffix_extract(committer_host)$domain,
      committer_suffix = suffix_extract(committer_host)$suffix
    )
}

read_git_log <- function(repo, clone_path) {
  gitlog_repo <- read_delim(get_git_log_path(repo, clone_path),
                            delim = "|", quote="",
                            col_names=c("author_date", "author_name", "author_email", 
                                        "committer_date", "committer_name", "committer_email", 
                                        "sha"))
  gitlog_repo <- gitlog_repo %>% 
    mutate(
      repo=repo, 
      repo_short=get_git_repo_short(repo)
    ) %>%
    gitlog_fix_dates %>%
    gitlog_extract_emails
  

  return(gitlog_repo)
}