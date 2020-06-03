# ------------------------------------------------------------------------- 
# INITIALISATION 
# -------------------------------------------------------------------------
library(tidyverse)
library(magrittr)
library(viridis)
library(ggwordcloud)

# ------------------------------------------------------------------------- 
# FUNCTIONS
# ------------------------------------------------------------------------- 

# Bar plot of word frequency.

plot_word_freq <- function(df) {
    
    # Calculate number of heading.
    no_sections <- df %>% 
        distinct(heading) %>% 
        pull(heading) %>% 
        length()
    
    # Only use top 10 most frequent words in each section.
    df %<>% 
        group_by(heading) %>% 
        arrange(desc(tf_idf)) %>%
        slice(1:10) %>% 
        ungroup() 

    # Base plot.
    g <- ggplot(df, aes(x = fct_reorder2(word, heading, desc(tf_idf)),
                        y = tf_idf, fill = heading)) +
        geom_col(show.legend = FALSE) +
        coord_flip() +
        scale_fill_viridis_d() +
        labs(x = 'TF-IDF', y = 'Word') +
        theme_linedraw()
    
    # If there is more than one heading, facet by heading.
    if (no_sections > 1) {
        plot_ncol <- ((no_sections - 1) %/% 3) + 1
        g <- g + facet_wrap(~heading, ncol = plot_ncol, scales = 'free_y')
    }  
    g
}

plot_word_cloud <- function(df) {
    
    # Calculate number of heading.
    no_sections <- df %>% 
        distinct(heading) %>% 
        pull(heading) %>% 
        length()
    
    # Only use top 50 most frequent words in each section.
    df %<>%
        group_by(heading) %>% 
        top_n(50, wt = tf_idf) %>%
        slice(1:50)
    
    # Base plot.
    g <- ggplot(df, aes(label = word, size = tf_idf)) +
        geom_text_wordcloud(seed = 20190804) +
        scale_size_area(max_size = 20) +
        theme_linedraw()
    
    # If there is more than one heading, facet by heading.
    if (no_sections > 1) {
        plot_ncol <- ((no_sections - 1) %/% 3) + 1
        g <- g + facet_wrap(~heading, ncol = plot_ncol, scales = 'free_y')
    }
    g
}
