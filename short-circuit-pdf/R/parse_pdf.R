# ------------------------------------------------------------------------- 
# INITIALISATION 
# ------------------------------------------------------------------------- 

library(tidyverse)
library(magrittr)
library(pdftools)
library(tidytext)

# ------------------------------------------------------------------------- 
# FUNCTIONS
# ------------------------------------------------------------------------- 

# Read document data ------------------------------------------------------
read_pdf_data <- function(filename) {
    
    # Data frame of metadata for each word in document.
    df <- pdf_data(filename) %>% 
        bind_rows(.id = 'page') %>%
        mutate(page = as.integer(page))
    
    # Document Line number are unique combos of page and y location.
    line_no <- df %>% 
        distinct(page, y) %>% 
        arrange(page, y) %>%
        mutate(doc_line = row_number())
    
    df %>% left_join(line_no, by = c('page', 'y'))
}

# Identify section names -------------------------------------------------
get_section_lookup <- function(df, font_height) { 
    df %>%
        # Filter words that make up headings.
        dplyr::filter(height == font_height) %>%
        # Group words that form each heading and assign head_ID.
        dplyr::mutate(doc_line_diff = doc_line - lag(doc_line)) %>%
        tidyr::replace_na(list(doc_line_diff = -1)) %>%
        dplyr::mutate(head_id = cumsum(doc_line_diff > 1)) %>%
        # Concatenate words with the same head_ID.
        dplyr::group_by(head_id) %>%
        dplyr::summarise(doc_line = first(doc_line),
                         page = first(page),
                         heading = str_c(text, collapse = " "),
                         .groups = 'drop')
}

# Create corpus -----------------------------------------------------------
make_corpus <- function(df) {
    df %>% 
        group_by(head_id) %>%
        group_split() %>%
        map_df(~tibble(heading = unique(.x$heading),
                       corpus = str_c(.x$text, collapse = " ")))
}