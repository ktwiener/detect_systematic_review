library(easyPubMed)
library(dplyr)
library(readr)

create_tbl <- function(journal, volumes, issues){
  
  not_allowed <- c("Comment", "Randomized Controlled Trial", "Review",
                   "Systematic Review", "Letter", "Meta-Analysis", 
                   "Validation Study", "Video-Audio Media", "Personal Narrative",
                   "Case Reports", "Patient Education Handout", "Published Erratum", "Webcast",
                   "Editorial")
  
  issues_str <- `if`(!is.null(issues),
                     sprintf("( %s )", paste0(issues, " [IP]", collapse = " OR ")),
                     NULL)
  volumes_str <- `if`(!is.null(volumes),
                      sprintf("( %s )", paste0(volumes, " [VI]", collapse = " OR ")),
                      NULL)
  journal_str <- paste0(journal, " [TA]")
  
  my_query <- paste0(c(journal_str, volumes_str, issues_str), collapse = " AND ")
  
  my_entrez_id <- get_pubmed_ids(my_query)
  x <- fetch_pubmed_data(my_entrez_id)
  
  dplyr::tibble(
    journal = custom_grep(x, "ISOAbbreviation", "char"),
    year  = purrr::map_chr(custom_grep(x, "PubDate"), custom_grep, "Year", "char"),
    volume = custom_grep(x, "Volume", "char"),
    issue = custom_grep(x, "Issue", "char"),
    title = custom_grep(x, "ArticleTitle", "char"),
    type  = purrr::map(custom_grep(x, "PublicationTypeList"), custom_grep, "PublicationType")
  ) %>%
    dplyr::filter(
      purrr::map_lgl(type, ~!any(.x %in% not_allowed))
    )
}

test1 <- create_tbl(journal = "Pharmacoepidemiology and Drug Safety", volumes = 29, issues = 3:5)
test2 <- create_tbl(journal = "Annals of Internal Medicine", volumes = 174, issues = 9:11)
test3 <- create_tbl(journal = "Journal of Clinical Epidemiology", volumes = 141:143, issues = NULL)

bind_rows(test1, test2, test3) %>% 
  readr::write_csv("results/test_run.csv")
  
