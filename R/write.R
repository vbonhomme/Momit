#' Write lines to files
#'
#' Given the lines returned by [to]`_*`, write file(s).
#'
#' @param x `character` lines, typically from one of [to]`_*` functions
#' @param path `character`, where to write the file(s)
#' @param folder `character`, where to write the file(s) (default ``, ie root of the working directory)
#' @param extension `character`, the extension to use, with or
#' without a `.` before extension (default `mom`)
#'
#' @name write
#' @rdname write
#' @export
write_single <- function(x, path){
  x %>%
    do.call("c", .) %>%
    writeLines(path)
}

#' @rdname write
#' @export
write_separately <- function(x, folder="", extension=""){
  if (!dir.exists(folder))
    dir.create(folder)
  paths <- paste0(folder, "/", names(x), ".", extension) %>% gsub("\\.+", ".", .)
  silent <- lapply(seq_along(x), function(i) x[[i]] %>% writeLines(paths[i]))
}
