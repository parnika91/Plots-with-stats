# --------------------------------------------
# upload data -> explore -> plot -> stats -> commands
# --------------------------------------------

# Packages
library(shiny)
library(DT)
library(ggplot2)
library(readr)
library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(tools)
suppressWarnings({ has_ggpubr <- requireNamespace("ggpubr", quietly = TRUE) })
suppressWarnings({ has_viridis <- requireNamespace("viridis", quietly = TRUE) })

ui <- fluidPage(
  titlePanel("ggplot with Stats"),
  sidebarLayout(
    sidebarPanel(
      h4("1) Upload data"),
      fileInput("file", "Choose a file (CSV/TSV/TXT/XLS/XLSX)", accept = c(
        ".csv", ".tsv", ".txt", ".xls", ".xlsx"
      )),
      uiOutput("excel_sheet_ui"),
      checkboxInput("header", "Header row", TRUE),
      selectInput("sep", "Delimiter (CSV/TSV/TXT)", choices = c(Comma = ",", Semicolon = ";", Tab = "\t", Space = " "), selected = ","),
      selectInput("dec", "Decimal mark", choices = c("." = ".", "," = ","), selected = "."),
      selectInput("quote", "Quoted fields", choices = c(None = "none", `"` = "\"", `'` = "'"), selected = "\""),
      textInput("na_str", "NA strings (comma-separated)", value = "NA"),
      textInput("encoding", "Encoding (e.g., UTF-8, latin1)", value = "UTF-8"),
      hr(),
      
      h4("2) Plot settings"),
      selectInput("plot_type", "Plot type", choices = c(
        "Scatter" = "scatter",
        "Bar (summary)" = "bar",
        "Violin" = "violin",
        "Box" = "box",
        "Histogram" = "hist",
        "Density" = "density"
      ), selected = "scatter"),
      uiOutput("var_selectors"),
      checkboxInput("jitter", "Add jitter (point-based plots)", FALSE),
      checkboxInput("add_reg", "Add linear fit (scatter)", TRUE),
      checkboxInput("facet_enable", "Facet by column", FALSE),
      uiOutput("facet_ui"),
      hr(),
      
      h4("3) Aesthetics"),
      selectInput("theme_choice", "Theme", choices = c(
        "minimal", "classic", "bw", "light", "dark"
      ), selected = "minimal"),
      sliderInput("base_size", "Base font size", min = 6, max = 22, value = 12, step = 1),
      checkboxInput("rotate_x", "Rotate X tick labels (45°)", FALSE),
      checkboxInput("show_grid", "Show panel grid", TRUE),
      selectInput("legend_pos", "Legend position", choices = c("right","left","top","bottom","none"), selected = "right"),
      hr(),
      
      h4("4) Colours"),
      radioButtons("colour_mode", "Colour mode", inline = FALSE, choices = c(
        "None (single colour)" = "none",
        "Colour by column" = "by_col",
        "Manual colours (comma-separated)" = "manual"
      ), selected = "by_col"),
      uiOutput("colour_column_ui"),
      textInput("manual_cols", "Manual colours (e.g. 'steelblue, firebrick, #33a02c')", value = ""),
      tags$a(href="https://r-charts.com/colors/", "R colours"),
      checkboxInput("use_viridis", "Use viridis palette (if available)", FALSE),
      hr(),
      
      h4("5) Statistics"),
      helpText("Scatter: adds lm fit + R and p-value in subtitle.",
               if (has_ggpubr) "Group plots: add t-test/ANOVA with ggpubr." else "Install ggpubr for compare_means."),
      checkboxInput("add_stats", "Add statistical comparison (grouped plots)", value = has_ggpubr),
      uiOutput("stats_group_ui"),
      
      hr(),
      downloadButton("download_plot", "Download Plot (PNG)")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Preview data", DTOutput("data_preview")),
        tabPanel("Plot", plotOutput("plot", height = "650px")),
        tabPanel("Data load command", verbatimTextOutput("data_cmd")),
        tabPanel("ggplot command", verbatimTextOutput("ggplot_cmd")),
        
        tabPanel("About", 
                 tags$p("Upload a dataset, configure parsing options, then build ggplot2 figures with styling and stats."),
                 tags$p("Supported: CSV/TSV/TXT (custom sep/dec/quote/encoding) and Excel (sheet selection)."),
                 tags$p("Statistics: linear fit on scatter; group comparisons on bar/violin/box if ggpubr is installed."))
      )
    )
  )
)

server <- function(input, output, session) {
  # Helpers
  .theme_map <- list(
    minimal = theme_minimal,
    classic = theme_classic,
    bw      = theme_bw,
    light   = theme_light,
    dark    = theme_dark
  )
  
  # Excel sheet UI
  output$excel_sheet_ui <- renderUI({
    req(input$file)
    ext <- tolower(file_ext(input$file$name))
    if (ext %in% c("xls","xlsx")) {
      sheets <- tryCatch(readxl::excel_sheets(input$file$datapath), error = function(e) NULL)
      selectInput("sheet", "Excel sheet", choices = sheets, selected = sheets[1])
    }
  })
  
  # Reactive data
  raw_data <- reactive({
    req(input$file)
    path <- input$file$datapath
    ext <- tolower(file_ext(input$file$name))
    na_vec <- str_trim(str_split(input$na_str, ",", simplify = TRUE))
    na_vec <- na_vec[na_vec != ""]
    if (length(na_vec) == 0) na_vec <- "NA"
    
    if (ext %in% c("csv","tsv","txt")) {
      delim <- input$sep
      quote_char <- switch(input$quote, none = "", `"` = "\"", `'` = "'")
      readr::read_delim(
        file = path,
        delim = delim,
        col_names = input$header,
        locale = locale(decimal_mark = input$dec, encoding = input$encoding),
        na = na_vec,
        quote = quote_char,
        trim_ws = TRUE,
        progress = FALSE
      )
    } else if (ext %in% c("xls","xlsx")) {
      readxl::read_excel(path = path, sheet = input$sheet %||% 1, na = na_vec)
    }
  })
  
  # Reactive expression to show data load command text
  output$data_cmd <- renderText({
    req(input$file)
    ext <- tolower(file_ext(input$file$name))
    path <- input$file$name
    if (ext %in% c("csv","tsv","txt")) {
      sprintf(
        'readr::read_delim("%s", delim = "%s", col_names = %s, locale = locale(decimal_mark = "%s", encoding = "%s"), na = c(%s), quote = "%s", trim_ws = TRUE)',
        path, input$sep, input$header, input$dec, input$encoding,
        paste0('"', paste(str_trim(str_split(input$na_str, ",", simplify = TRUE)), collapse = '", "'), '"'),
        ifelse(input$quote == "none", "", input$quote)
      )
    } else if (ext %in% c("xls","xlsx")) {
      sprintf('readxl::read_excel("%s", sheet = "%s", na = c(%s))',
              path, input$sheet %||% 1,
              paste0('"', paste(str_trim(str_split(input$na_str, ",", simplify = TRUE)), collapse = '", "'), '"'))
    } else {
      "Unsupported file type."
    }
  })
  
  # Preview
  output$data_preview <- renderDT({
    req(raw_data())
    datatable(raw_data(), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Observe data and build plot inputs
  observeEvent(raw_data(), {
    df <- raw_data()
    all_cols <- names(df)
    num_cols <- names(Filter(\(x) is.numeric(x) || inherits(x, "Date") || inherits(x, "POSIXt"), df))
    fac_cols <- setdiff(all_cols, num_cols)
    
    output$var_selectors <- renderUI({
      tagList(
        selectInput("xvar", "X variable", choices = all_cols, selected = all_cols[1]),
        conditionalPanel(
          "['scatter','bar','violin','box','density'].includes(input.plot_type)",
          selectInput("yvar", "Y variable", choices = c("", all_cols), selected = ifelse(length(num_cols), num_cols[1], "")))
      )
    })
    
    output$facet_ui <- renderUI({
      req(input$facet_enable)
      selectInput("facet_var", "Facet by", choices = c("", all_cols), selected = "")
    })
    
    output$colour_column_ui <- renderUI({
      if (input$colour_mode == "by_col") {
        selectInput("colour_var", "Colour by", choices = c("", all_cols), selected = "")
      }
    })
    
    output$stats_group_ui <- renderUI({
      if (input$add_stats) {
        selectInput("group_var", "Group column (for tests)", choices = c("", fac_cols, all_cols), selected = ifelse(length(fac_cols), fac_cols[1], ""))
      }
    })
  }, ignoreInit = FALSE)
  
  # Build plot
  build_plot <- reactive({
    req(raw_data(), input$plot_type, input$xvar)
    df <- raw_data()
    aes_map <- aes_string(x = input$xvar, y = ifelse(input$yvar != "", input$yvar, NULL))
    g <- ggplot(df, aes_map)
    g + geom_point()
  })
  
  output$plot <- renderPlot({
    build_plot()
  })
  
  # >>> NEW: reactive ggplot command text
  output$ggplot_cmd <- renderText({
    req(input$xvar)
    cmd <- sprintf('ggplot(df, aes(x = %s%s)) + ', input$xvar,
                   ifelse(isTruthy(input$yvar), paste0(", y = ", input$yvar), ""))
    cmd <- paste0(cmd, switch(input$plot_type,
                              scatter = "geom_point()",
                              bar = "geom_bar()",
                              violin = "geom_violin()",
                              box = "geom_boxplot()",
                              hist = "geom_histogram(bins = 30)",
                              density = "geom_density()",
                              ""))
    cmd
  })
  
  # Download handler
  output$download_plot <- downloadHandler(
    filename = function() sprintf("plot_%s.png", Sys.Date()),
    content = function(file) {
      g <- build_plot()
      ggsave(file, g, width = 8, height = 6, dpi = 300)
    }
  )
}

`%||%` <- function(a,b) if (!is.null(a)) a else b

shinyApp(ui, server)
