#' Title Randomise Clipboard Tasks
#'
#' @description Randomises tasks copied to the clipboard, with earlier tasks more likely to be ranked higher
#'
#' @param additional_tasks Optional character string of additional tasks (one per line) to combine with clipboard tasks before randomisation
#'
#' @return Vector of randomised tasks in Markdown format
#' @export
# NEW: Added additional_tasks parameter with default NULL to maintain backward compatibility
rand_cb_tasks <- function(additional_tasks = NULL) {
  tasks <- clipr::read_clip(allow_non_interactive = TRUE)
  
  # NEW: If additional_tasks is provided and non-empty, split by newlines and combine with clipboard tasks

  # This allows email tasks from the textarea to be merged with clipboard tasks
  if (!is.null(additional_tasks) && nzchar(additional_tasks)) {
    # Split the textarea content by newlines to get individual task lines
    extra_tasks <- unlist(strsplit(additional_tasks, "\n"))
    # Remove empty lines from the additional tasks
    extra_tasks <- extra_tasks[nzchar(trimws(extra_tasks))]
    # Combine clipboard tasks with additional tasks
    n <- min(length(tasks), length(extra_tasks))
    tasks <- c(
      as.vector(rbind(tasks[seq_len(n)], extra_tasks[seq_len(n)])),
      tasks[-seq_len(n)],
      extra_tasks[-seq_len(n)]
    )
  }
  
  tasks <- gsub("^\\s*\u2610\\s*", "", tasks)
  tasks <- gsub("^", "- [ ] ", tasks)
  probs <- 1 / seq_along(tasks)
  probs <- probs / sum(probs)
  chosen <- sample(tasks, prob = probs)
  paste(chosen, collapse = "\n")
}