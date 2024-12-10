#' Indent multiple strings
#'
#' @description
#' `str_indent()` adds a customizable amount of spaces (or other characters) 
#' before every word, as if it was tabbed, and puts every word on a new line.
#' @param indent_level controls the number of characters to add, 
#' @param indent_character controls what character is placed.
#' @returns a newline separated list of words with a 'tab' character in between
#' @seealso [str_spaceout()] to add spaces between words.
#' @export
#' @examples
#' x <- c("why", "video", "cross", "extra", "deal", "authority")
#' # Default options
#' str_indent(x)
#' # Extra indentation
#' str_indent(x, indent_level = 4)
#' # Using periods for visibility
#' str_indent(x, indent_character = '.')
#' # Both parameters changed
#' str_indent(x, indent_level = 6, indent_character = '-')

str_indent <- function(x, indent_level = 3, indent_character = ' ') {
  # If vector not atomic, turns it into a single string separated by newlines
  if (length(x) >= 2) {
    x <- paste(x, collapse = "\n")
  }
  if (indent_level < 0) {
    stop("Invalid indent level, indent_level must be zero or positive.")
  }
  # Create a "tab" string out of 3 spaces or other character
  indent <- paste(rep(indent_character, indent_level), collapse = '')
  return(gsub("(?m)^", indent, x, perl = TRUE))
  # perl must be TRUE to use regular expressions in baser
}

