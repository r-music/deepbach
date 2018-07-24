#- Reading kern data ---------------------------------------------------
#-----------------------------------------------------------------------
library(magrittr)
library(data.table)
library(stringr)
# 116, 130:133, 325, 326

l <- list.files(pattern = ".krn", recursive = TRUE)
indices <- seq_along(l)
indices <- indices[-c(116, 130:133, 325, 326)]

for(j in indices){
  d <- data.table::fread(paste0(l[j])) %>% as.list()
  da_res <- list()

  if (any(stringr::str_detect(d[[1]], "4/4$"))) {
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
        dplyr::mutate(position = dplyr::row_number()) %>%
        # tomar cuidado aqui pois já estamos fazendo algumas coisas que estão
        # sendo feitas lá embaixo
        dplyr::filter(column != ".") %>%
        dplyr::ungroup()
      da_res[[i]] <- ind
    }

    names(da_res) <- c("Bass", "Tenor", "Alto", "Soprano")
    name_data <- str_replace(paste0(l[j]), pattern = "krn", replacement = "rda")

    name_data <- paste0("data-raw/kern-data/rds/",
                        basename(tools::file_path_sans_ext(l[j])),
                        ".rds")
    saveRDS(da_res, file = name_data)
  }
  print(j)
}

# esse codigo monta a base com 16 linhas por compasso
# ainda não arrumamos
# [] anacruse
# [] ritornello
# [] final da musica (3 tempos)
# [] integrar tudo na mesma fç
arrumar_voz <- function(da_voz) {
  da_voz %>%
    dplyr::mutate(column = as.character(column)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(column != ".") %>%
    dplyr::mutate(mult = as.numeric(stringr::str_extract(column, "[0-9]{1,2}")),
                  ponto = stringr::str_detect(column, "(?<=[0-9]{1,2})\\."),
                  mult = dplyr::if_else(ponto, mult * 2/3, mult),
                  rept = 16 / mult) %>%
    dplyr::filter(!is.na(rept)) %>%
    tidyr::uncount(rept, .id = "tempo") %>%
    dplyr::mutate(note = dplyr::if_else(tempo > 1, "_", note))
}
da_res <- readr::read_rds("data-raw/kern-data/rds/chor008.rds")
da_res %>% purrr::map_dfr(arrumar_voz, .id = "voz")

