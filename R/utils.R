#' @export
print.mailgun_result <- function(x, ...) {
  cat(x$message)
}
