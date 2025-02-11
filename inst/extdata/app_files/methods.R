methods_page <- function(id) {
  ns <- NS(id)
  
  page_fillable(
    withMathJax(),
    div(
      class = "methods-panel",
      includeMarkdown("www/methods.md")
    )
  )
}