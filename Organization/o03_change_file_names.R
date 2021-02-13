#--- rename raw-yield files when they are named raw_yield ---#
raw_yield_files <- list.files(recursive = TRUE) %>%
  .[str_detect(., "raw_yield")] %>%
  file.rename(., gsub("raw_yield", "raw-yield", .))

#--- rename as-planted files when they are named as-planted ---#
# list.files(
#   here("Data", "Growers"), 
#   recursive = TRUE, 
#   full.names = TRUE
# ) %>%
# .[str_detect(., "as-applied-s")] %>%
# file.rename(., gsub("as-planted", "as-applied-s", .))

#--- rename as-applied N files when they are just named as-applied ---#
# list.files(
#   here("Data", "Growers"), 
#   recursive = TRUE, 
#   full.names = TRUE
# ) %>%
# .[str_detect(., "as-applied")] %>%
# file.rename(., gsub("as-applied", "as-applied-n", .))

#--- rename ab-line files when they are named raw_yield ---#
ab_line_files <- list.files(
    here("Data", "Growers"), 
    recursive = TRUE, 
    full.names = TRUE
  ) %>%
  .[str_detect(., c("ab_line|ab-lines"))] 

ab_line_files_new <- ab_line_files %>% 
  gsub("ab_line", "ab-line", .) %>% 
  gsub("ab-lines", "ab-line", .)  

file.rename(ab_line_files, ab_line_files_new) 

