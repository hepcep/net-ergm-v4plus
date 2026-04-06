# Helper function to only run fits that were not previously saved ----------
## Useful for checkpointing

load_or_run <- function(name, expr) {
  path <- file.path(out_dir, paste0(name, ".rds"))
  if (file.exists(path)) {
    message("Loading saved ", name)
    readRDS(path)
  } else {
    message("Running and saving ", name)
    obj <- eval(expr)
    saveRDS(obj, path)
    obj
  }
}