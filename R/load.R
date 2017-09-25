read_midi <- function() {
  reticulate::use_condaenv('deepbach')
  m <- reticulate::import_from_path('DeepBach', '.')
  d <- reticulate::py_run_file('script.py')

  X <- d$X
  X_metadatas <- d$X_metadatas
  voice_ids <- d$voice_ids
  index2notes <- d$index2notes
  note2indexes <- d$note2indexes
  metadatas <- d$metadatas

  i <- 4
  as.vector(unlist(index2notes[[i]][X[[1]][i,] + 1]))

  timesteps <- 16

  left <- keras::layer_input(timesteps, )



}
