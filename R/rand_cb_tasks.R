#' Title Randomise Clipboard Tasks
#'
#' @description Randomises tasks copied to the clipboard, with earlier tasks more likely to be ranked higher
#'
#' @return Vector of randomised tasks in Markdown format
#' @export
rand_cb_tasks <- function() {
  tasks <- clipr::read_clip(allow_non_interactive = TRUE)
  tasks <- gsub("^\\s*\u2610\\s*", "", tasks)
  tasks <- gsub("^", "- [ ] ", tasks)
  probs <- 1 / seq_along(tasks)
  probs <- probs / sum(probs)
  chosen <- sample(tasks, prob = probs)
  paste(chosen, collapse = "\n")
}

