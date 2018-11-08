library(tidyverse)
library(lubridate)
library(janitor)
library(Rcpp)


run_chi_square <- function(.data, test_agency) {
  
  # cat(test_agency, "\n")
  m1 <- .data %>% 
    filter(agency == test_agency) %>% 
    pull(n) %>%
    unique() %>% 
    sort()
  
  m1 <- min(length(m1),(which(diff(m1) != 1)[1]), na.rm = TRUE, 10)
  
  m2 <- .data %>% 
    filter(agency != test_agency) %>% 
    pull(n) %>%
    unique() %>% 
    sort()
  
  m2 <- min(length(m2),(which(diff(m2) != 1)[1]), na.rm = TRUE, 10)
  
  m <- min(m1, m2)
  # cat(m, "\n")
  
  # if (m <= 1){ return(NA)}
  
  agency_counts <- .data %>%
    filter(agency == test_agency, n <= m) %>%
    select(n) %>%
    table()
  
  overall_props <- .data %>%
    filter(agency != test_agency, n <= m) %>%
    group_by(n) %>%
    summarize(num = n()) %>%
    mutate(prop = num/sum(num)) %>% 
    pull(prop)
  
  # sim <- any(sum(agency_counts)*overall_props <= 5)
  # cat(sim, "\n")
  
  chisq.test(agency_counts, p = overall_props) %>%
    broom::tidy() %>% 
    mutate(agency = test_agency)
}


make_plots <- function(.data, test_agency) {
  
  # cat(test_agency, "\n")
  m1 <- .data %>% 
    filter(agency == test_agency) %>% 
    pull(n) %>%
    unique() %>% 
    sort()
  
  m1 <- min(length(m1),(which(diff(m1) != 1)[1]), na.rm = TRUE, 10)
  
  m2 <- .data %>% 
    filter(agency != test_agency) %>% 
    pull(n) %>%
    unique() %>% 
    sort()
  
  m2 <- min(length(m2),(which(diff(m2) != 1)[1]), na.rm = TRUE, 10)
  
  m <- min(m1, m2)
  # cat(m, "\n")
  
  # if (m <= 1){ return(NA)}
  
  agency_counts <- .data %>%
    filter(agency == test_agency, n <= m) 
  
  overall_props <- .data %>%
    filter(agency != test_agency, n <= m) 
  
  # sim <- any(sum(agency_counts)*overall_props <= 5)
  # cat(sim, "\n")
  
  # chisq.test(agency_counts, p = overall_props) %>%
  #   broom::tidy() %>% 
  #   mutate(agency = test_agency)
  
  # print(
    ggplot(agency_counts, aes(n)) + 
      geom_histogram(data = overall_props, aes(n, ..density..), binwidth = 1, fill = "gray") +
      geom_histogram(aes(y = ..density..),binwidth = 1, alpha = .6, fill = "red") +
      labs(title = test_agency)
      # )
}
