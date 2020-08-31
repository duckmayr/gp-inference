#' Ensure one or more directories exist
#' 
#' @param directories A character vector giving relative or absolute paths for
#'      the desired directories
ensure_directories_exist <- function(directories) {
    for ( directory in directories ) {
        if ( dir.exists(directory) ) {
            next()
        } else {
            dir.create(directory)
        }
    }
}
