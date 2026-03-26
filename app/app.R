# Whiny Shiny app
#
# Assumptions:
# - "Whiny" means elongating vowels and adding sighs; kept simple and client-side.
# - Uses bslib for theme and layout, DT for example table.
# - No extra packages are installed automatically.

library(shiny)
library(bslib)
library(DT)

# Helper: produce a "whiny" version of text
make_whiny <- function(text, intensity = 1, style = c("mild", "classic", "dramatic")) {
  style <- match.arg(style)
  if (nzchar(trimws(text)) == FALSE) return("")
  chars <- strsplit(text, "")[[1]]
  vowels <- "[aeiouAEIOU]"
  # elongate vowels by repeating them 'intensity' times
  out_chars <- vapply(chars, function(ch) {
    if (grepl(vowels, ch)) {
      paste0(ch, paste0(rep(ch, intensity), collapse = ""))
    } else ch
  }, FUN.VALUE = character(1))
  base <- paste0(out_chars, collapse = "")
  suffix <- switch(style,
    mild     = paste0(rep(" *sigh*", max(1, ceiling(intensity/2))), collapse = ""),
    classic  = paste0(rep(" ...ugh", max(1, intensity)), collapse = ""),
    dramatic = paste0(rep(" !!!", max(1, intensity)), collapse = "")
  )
  paste0(base, suffix)
}

# Example data
examples <- data.frame(
  original = c(
    "I have so much to do",
    "Why is this taking forever?",
    "The coffee is cold again",
    "Nobody listens to me",
    "It worked yesterday"
  ),
  stringsAsFactors = FALSE
)

ui <- page_sidebar(
  title = "Whiny App",
  theme = bs_theme(bootswatch = "flatly"),
  sidebar = sidebarPanel(
    textInput("text", "Enter text to whine:", value = "I have so much to do"),
    sliderInput("intensity", "Intensity", min = 1, max = 6, value = 2),
    selectInput("style", "Style", choices = c("mild", "classic", "dramatic"), selected = "classic"),
    actionButton("go", "Whine!", class = "btn-primary"),
    width = 3
  ),
  main = mainPanel(
    # Card showing generated whiny text
    div(class = "p-2",
      bslib::card(
        title = "Whined Text",
        div(style = "font-size:1.25rem; white-space:pre-wrap;", textOutput("whined_text")),
        footer = tagList(
          downloadButton("download_txt", "Download"),
          span(" ", style = "display:inline-block; width:8px;"),
          actionButton("copy_btn", "Copy to clipboard")
        ),
        width = 12
      )
    ),
    # Card with examples table
    div(class = "p-2",
      bslib::card(
        title = "Examples",
        DT::dataTableOutput("examples_tbl"),
        width = 12
      )
    )
  )
)

server <- function(input, output, session) {
  # reactive for generated whine
  whined <- eventReactive(input$go, {
    make_whiny(input$text, intensity = input$intensity, style = input$style)
  }, ignoreNULL = FALSE)
  
  output$whined_text <- renderText({
    whined()
  })
  
  # DT table showing examples and their whined forms with current settings
  output$examples_tbl <- DT::renderDT({
    df <- examples
    df$whiny <- vapply(df$original, make_whiny, FUN.VALUE = character(1),
                       intensity = input$intensity, style = input$style)
    DT::datatable(df, options = list(pageLength = 5), rownames = FALSE)
  })
  
  # Download handler for the whined text
  output$download_txt <- downloadHandler(
    filename = function() {
      paste0("whiny-", Sys.Date(), ".txt")
    },
    content = function(file) {
      writeLines(whined(), file, useBytes = TRUE)
    }
  )
  
  # Copy to clipboard (uses JS)
  observeEvent(input$copy_btn, {
    js <- sprintf("navigator.clipboard.writeText(%s);", jsonlite::toJSON(whined(), auto_unbox = TRUE))
    session$sendCustomMessage(type = "evaljs", message = js)
  })
  
  # Register handler for evaljs
  session$onSessionEnded(function() {
    # no-op
  })
}

# small JS bridge
jsCode <- "
Shiny.addCustomMessageHandler('evaljs', function(message) { eval(message); });
"

shinyApp(ui = tagList(tags$head(tags$script(HTML(jsCode))), ui), server = server)
