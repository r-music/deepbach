#- Reading kern data ---------------------------------------------------
#-----------------------------------------------------------------------
library(magrittr)
library(data.table)
library(stringr)
# 116, 130:133, 325, 326

l <- list.files(pattern = ".krn", recursive = TRUE)

for(j in 327:length(l)){
  d <- data.table::fread(paste0(l[j])) %>% as.list()

  da_res <- list()

  for(i in 1:4){
    da <- d[i] %>% unlist() %>% as.data.frame() %>% setNames("column")
    ind <- da %>%
      dplyr::filter(!("!" %in% column)) %>% # ritornello
      dplyr::mutate(compass = as.numeric(
        stringr::str_extract(column, pattern = "(?<=\\=)[0-9]{1,2}"))) %>%
      tidyr::fill(compass) %>%
      na.omit() %>%
      dplyr::mutate(
        fermata = ifelse(stringr::str_detect(column, pattern = ";"), 1, 0),
        ligature = ifelse(stringr::str_detect(column, pattern = "\\["), 1, 0),
        note = stringr::str_extract(column, pattern = "[a-zA-z]{1,3}[\\#]|[a-zA-z]{1,3}"),
        time_div = ifelse(str_detect(column, "r"), "pause",
                          ifelse(str_detect(column, "4"), "quarter",
                                 ifelse(str_detect(column, "8"), "eighter",
                                        ifelse(str_detect(column, "2"), "minim",
                                               ifelse(str_detect(column, "16"), "sixteenth",
                                                      NA)))))) %>%
      dplyr::group_by(compass) %>%
      dplyr::slice(-1) %>%
      dplyr::mutate(
        position = row_number())  %>%
      tidyr::fill(note, time_div)

    da_res[[i]] <- ind
  }

  names(da_res) <- c("Bass", "Tenor", "Alto", "Soprano")
  name_data <- str_replace(paste0(l[j]), pattern = "krn", replacement = "rda")
  save(da_res, file = paste0("res/", name_data))
  print(j)
}



