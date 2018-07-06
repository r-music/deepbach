#- Reading kern data ---------------------------------------------------
#-----------------------------------------------------------------------
library(magrittr)
d <- data.table::fread("/home/navarro/Desktop/bachs/bach-chorales/kern/chor371.krn") %>%
  setNames(c("Bass", "Tenor", "Alto", "Soprano"))
sop <- d$Soprano %>% as.data.frame() %>% setNames("sop")


library(stringr)

ind <- sop %>%
  dplyr::filter(!("!" %in% sop)) %>% # ritornello
  dplyr::mutate(compass = as.numeric(
    stringr::str_extract(sop, pattern = "(?<=\\=)[0-9]{1,2}"))) %>%
  tidyr::fill(compass) %>%
  dplyr::mutate(
    fermata = ifelse(stringr::str_detect(sop, pattern = ";"), 1, 0),
    ligature = ifelse(stringr::str_detect(sop, pattern = "\\["), 1, 0),
    note = stringr::str_extract(sop, pattern = "[a-zA-z]{1,3}[\\#]|[a-zA-z]{1,3}"),
    time_div = ifelse(str_detect(sop, "r"), "pause",
                      ifelse(str_detect(sop, "4"), "quarter",
                      ifelse(str_detect(sop, "8"), "eighter",
                             ifelse(str_detect(sop, "2"), "minim",
                                    ifelse(str_detect(sop, "16"), "sixteenth",
                                    NA)))))) %>%
  dplyr::group_by(compass) %>%
  dplyr::slice(-1) %>%
  dplyr::mutate(
    position = row_number())  %>%
  tidyr::fill(note, time_div)





