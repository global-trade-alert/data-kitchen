# Saved queries UI function

savedQueriesUI <- function(id) {
  
  # Create a namespace function using the provided id
  ns <- NS(id)
  
  tags$div(class="saved-queries",
           tags$div(class="query",
                    HTML("<a href='http://tinyurl.com/yxse8r5v' style='color:blue;'>"),
                    HTML("Query 1</a>"),
                    tags$p("This is the description"))
           )
}