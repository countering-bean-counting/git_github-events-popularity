library(dplyr)
library(igraph)

#' Title
#'
#' @param gitlog_commits 
#'
#' @return gitlog_commits
#' @export TRUE?
#'
#' @examples get
get_gh_committers_by_email <- function(gitlog_commits) {
  gitlog_commits %>%
    rename(name=committer_name, email=committer_email) %>%
    arrange(desc(commit_date)) %>%
    group_by(email, name) %>%
    summarise(last_commit=max(commit_date)) %>%
    arrange(desc(last_commit)) 
}

committers_lookup <- function(gh_committers_by_email) {
  gh_committers_join1 <- gh_committers_by_email %>%
    inner_join(gh_committers_by_email %>% select(name, email) %>% rename(name2=name), "email") %>%
    unique()
  
  gh_committers_join <- gh_committers_join1 %>%
    inner_join(gh_committers_join1 %>% select(name, email) %>% rename(email2=email), "name") %>%
    unique()
  
  return(gh_committers_join)
}

# group commits by email address and name
get_gh_authors_by_email <- function(gitlog_commits) {
  gh_authors_by_email <- gitlog_commits %>%
    rename(name=author_name, email=author_email) %>%
    arrange(desc(commit_date)) %>%
    group_by(email, name) %>%
    summarise(num_commits = n(), 
              last_commit=max(commit_date)) %>%
    arrange(desc(last_commit))
}

authors_lookup <- function(gh_authors_by_email) {
  # join on name to show emails tied to the same names
  gh_authors_join1 <- gh_authors_by_email %>%
    inner_join(gh_authors_by_email %>% select(name, email) %>% rename(name2=name), "email") %>%
    unique()
  
  # join on email to show names tied to the same emails
  gh_authors_join <- gh_authors_join1 %>%
    inner_join(gh_authors_join1 %>% select(name, email) %>% rename(email2=email), "name") %>%
    unique()
  
  return(gh_authors_join)
}

join_authors_and_committers <- function(gh_authors_join, gh_committers_join) {
  gh_emails <- bind_rows(gh_authors_join %>% select(email, email2), gh_committers_join %>% select(email, email2))
  gh_emails <- gh_emails %>% ungroup() %>% unique()
  return(gh_emails)
}

build_emails_graph <- function(gh_emails, gitlog_commits) {

  # this might need to be directed in the future based on commit dates
  gh_emails_graph_big <- graph_from_data_frame(gh_emails,
                                               directed=FALSE)
  
  E(gh_emails_graph_big)$weight <- 1
  gh_emails_graph <- simplify(gh_emails_graph_big, 
                              edge.attr.comb=list(
                                weight = "sum", 
                                transaction_amount = "sum", 
                                function(x)length(x))
  )
  
  # identify clusters
  gh_emails_networks <- clusters(as.undirected(gh_emails_graph))
  V(gh_emails_graph)$network <- gh_emails_networks$membership
  
  # extract vertices
  gh_emails_nodes_vert <- get.data.frame(gh_emails_graph, what="vertices")
  
  # create nodes with fields used by Visnetwork for plotting
  gh_emails_nodes <- data.frame(id = gh_emails_nodes_vert$name,
                                title = gh_emails_nodes_vert$name, 
                                group = gh_emails_nodes_vert$network)
  gh_emails_nodes <- gh_emails_nodes[order(gh_emails_nodes$id, decreasing = F),]
  
  # extract edges
  gh_emails_edges <- get.data.frame(gh_emails_graph, what="edges")[1:2]
  
  # remove data structures we no longer need
  rm(gh_committers_emails_graph, gh_emails_graph, gh_emails_networks, gh_emails_nodes_pre)
  
  # join by committer email address with git log data to get the clusters
  gitlog_networks <- gitlog_commits %>% 
    ungroup() %>%
    inner_join(gh_emails_nodes %>% 
                 select(id, group) %>% 
                 rename(committer_group=group), 
               by=c("committer_email"="id"))
  
  # join by author
  gitlog_networks <- gitlog_networks %>% 
    ungroup() %>%
    inner_join(gh_emails_nodes %>% 
                 select(id, group) %>% 
                 rename(author_group=group), 
               by=c("author_email"="id"))
  return(gitlog_networks)
}

get_gitlog_network <- function(gitlog_commits) {
  # build lookup of committers
  gh_committers_by_email <- get_gh_committers_by_email(gitlog_commits)
  gh_committers <- committers_lookup(gh_committers_by_email)
  
  # build lookup of authors
  gh_authors_by_email <- get_gh_authors_by_email(gitlog_commits)
  gh_authors <- authors_lookup(gh_authors_by_email)
  
  # join both
  gh_emails <- join_authors_and_committers(gh_authors, gh_committers)
  
  # create network
  emails_graph <- build_emails_graph(gh_emails, gitlog_commits)
  
  return(emails_graph)
}