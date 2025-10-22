# app.R
# --------------------------------------------
# Shiny Plotter: upload data -> explore -> plot -> stats
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
library(tools)    # file_ext
suppressWarnings({ has_ggpubr <- requireNamespace("ggpubr", quietly = TRUE) })
suppressWarnings({ has_viridis <- requireNamespace("viridis", quietly = TRUE) })

ui <- fluidPage(
  titlePanel("Shiny GGPlot with Stats"),
  sidebarLayout(
    sidebarPanel(
      h4("1) Upload data"),
      fileInput("file", "Choose a file (CSV/TSV/TXT/XLS/XLSX)", accept = c(
        ".csv", ".tsv", ".txt", ".xls", ".xlsx"
      )),
      uiOutput("excel_sheet_ui"),
      checkboxInput("header", "Header row", TRUE),
      selectInput("sep", "Delimiter (CSV/TXT)", choices = c(Comma = ",", Semicolon = ";", Tab = "\t"), selected = ","),
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
        tabPanel("About", 
                 tags$p("Upload a dataset, configure parsing options, then build ggplot2 figures with styling and stats."),
                 tags$p("Supported: CSV/TSV/TXT (custom sep/dec/quote/encoding) and Excel (sheet selection)."),
                 tags$p("Statistics: linear fit on scatter; group comparisons on bar/violin/box if ggpubr is installed."))
      )
    )
  )
)

server <- function(input, output, session) {
  # ---- Helpers ----
  .theme_map <- list(
    minimal = theme_minimal,
    classic = theme_classic,
    bw      = theme_bw,
    light   = theme_light,
    dark    = theme_dark
  )
  
  # Read Excel sheets list if needed
  output$excel_sheet_ui <- renderUI({
    req(input$file)
    ext <- tolower(file_ext(input$file$name))
    if (ext %in% c("xls","xlsx")) {
      sheets <- tryCatch(readxl::excel_sheets(path = input$file$datapath), error = function(e) NULL)
      selectInput("sheet", "Excel sheet", choices = sheets, selected = sheets[1])
    }
  })
  
  # Reactive: read data
  raw_data <- reactive({
    req(input$file)
    path <- input$file$datapath
    ext <- tolower(file_ext(input$file$name))
    na_vec <- str_trim(str_split(input$na_str, ",", simplify = TRUE))
    na_vec <- na_vec[na_vec != ""]
    if (length(na_vec) == 0) na_vec <- "NA"
    
    if (ext %in% c("csv","tsv","txt")) {
      delim <- switch(input$sep, "," = ",", ";" = ";", "\t" = "\t")
      quote_char <- switch(input$quote, none = "", `"` = "\"", `'` = "'")
      readr::read_delim(
        file      = path,
        delim     = delim,
        col_names = input$header,
        locale    = locale(decimal_mark = input$dec, encoding = input$encoding),
        na        = na_vec,
        quote     = quote_char,
        trim_ws   = TRUE,
        progress  = FALSE
      )
    } else if (ext %in% c("xls","xlsx")) {
      readxl::read_excel(
        path = path,
        sheet = input$sheet %||% 1,
        na = na_vec
      )
    } else {
      validate(need(FALSE, "Unsupported file type."))
    }
  })
  
  # Preview
  output$data_preview <- renderDT({
    req(raw_data())
    datatable(raw_data(), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Update variable selectors based on data
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
    
    # Theme
    theme_fun <- .theme_map[[input$theme_choice]] %||% theme_minimal
    thm <- theme_fun(base_size = input$base_size) +
      theme(legend.position = input$legend_pos) +
      if (!input$show_grid) theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) else theme() +
      if (isTRUE(input$rotate_x)) theme(axis.text.x = element_text(angle = 45, hjust = 1)) else theme()
    
    # Aesthetics
    aes_map <- aes(x = .data[[input$xvar]])
    needs_y <- input$plot_type %in% c("scatter","bar","violin","box","density")
    if (needs_y && !isTruthy(input$yvar)) {
      validate(need(FALSE, "Please select a Y variable for this plot type."))
    }
    if (needs_y && isTruthy(input$yvar)) aes_map$y <- as.name(input$yvar)
    
    colour_scale <- NULL
    fill_scale <- NULL
    
    # Color handling
    if (input$colour_mode == "by_col" && isTruthy(input$colour_var)) {
      # use colour aesthetic for scatter/line; fill for violin/box/bar
      if (input$plot_type %in% c("violin","box","bar")) {
        aes_map$fill <- as.name(input$colour_var)
      } else {
        aes_map$colour <- as.name(input$colour_var)
      }
      if (isTRUE(input$use_viridis) && has_viridis) {
        if (input$plot_type %in% c("violin","box","bar")) fill_scale <- viridis::scale_fill_viridis_d()
        else colour_scale <- viridis::scale_color_viridis_d()
      }
      if (nzchar(input$manual_cols)) {
        cols <- str_trim(unlist(str_split(input$manual_cols, ",")))
        if (input$plot_type %in% c("violin","box","bar")) fill_scale <- scale_fill_manual(values = cols)
        else colour_scale <- scale_color_manual(values = cols)
      }
    } else if (input$colour_mode == "manual" && nzchar(input$manual_cols)) {
      # Single colour fallback if no column selected
      cols <- str_trim(unlist(str_split(input$manual_cols, ",")))
      if (length(cols) >= 1) {
        if (input$plot_type %in% c("violin","box","bar")) {
          fill_scale <- scale_fill_manual(values = cols, guide = "none")
        } else {
          colour_scale <- scale_color_manual(values = cols, guide = "none")
        }
      }
    }
    
    g <- ggplot(df, mapping = aes_map)
    
    # Geoms
    if (input$plot_type == "scatter") {
      g <- g + geom_point(alpha = 0.8, size = 2, stroke = 0.2)
      if (isTRUE(input$jitter)) g <- g + geom_jitter(width = 0.1, height = 0.1, alpha = 0.6)
      if (isTRUE(input$add_reg) && isTruthy(input$yvar) && is.numeric(df[[input$xvar]]) && is.numeric(df[[input$yvar]])) {
        # correlation stats
        this_df <- df %>% filter(!is.na(.data[[input$xvar]]), !is.na(.data[[input$yvar]]))
        if (nrow(this_df) >= 3) {
          cor_val <- suppressWarnings(cor(this_df[[input$xvar]], this_df[[input$yvar]], use = "pairwise.complete.obs"))
          pval <- tryCatch(cor.test(this_df[[input$xvar]], this_df[[input$yvar]])$p.value, error = function(e) NA)
          subtitle <- sprintf("Linear fit  |  r = %.3f,  p = %s", cor_val, ifelse(is.na(pval), "NA", format.pval(pval, digits = 3)))
          g <- g + labs(subtitle = subtitle) + geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 0.6, formula = y ~ x)
        }
      }
      
    } else if (input$plot_type == "bar") {
      # summary bar: mean +/- SE if y is numeric; else count
      if (isTruthy(input$yvar) && is.numeric(df[[input$yvar]])) {
        g <- g + stat_summary(fun = mean, geom = "bar", position = position_dodge(width = 0.9))
        g <- g + stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2, position = position_dodge(width = 0.9))
      } else {
        g <- g + geom_bar()
      }
      
    } else if (input$plot_type == "violin") {
      g <- g + geom_violin(trim = FALSE)
      if (isTRUE(input$jitter)) g <- g + geom_jitter(width = 0.1, alpha = 0.5, size = 1)
      
    } else if (input$plot_type == "box") {
      g <- g + geom_boxplot(outlier.shape = 19, outlier.size = 1.2)
      if (isTRUE(input$jitter)) g <- g + geom_jitter(width = 0.1, alpha = 0.5, size = 1)
      
    } else if (input$plot_type == "hist") {
      g <- g + geom_histogram(bins = 30, fill = "grey70", color = "white")
      
    } else if (input$plot_type == "density") {
      if (input$colour_mode == "by_col" && isTruthy(input$colour_var)) {
        g <- g + geom_density(alpha = 0.4)
      } else {
        g <- g + geom_density(fill = "grey70", alpha = 0.6)
      }
    }
    
    # Facet
    if (isTRUE(input$facet_enable) && isTruthy(input$facet_var)) {
      g <- g + facet_wrap(vars(.data[[input$facet_var]]), scales = "free_y")
    }
    
    # Stats on grouped plots
    if (isTRUE(input$add_stats) && has_ggpubr &&
        input$plot_type %in% c("bar","violin","box") &&
        isTruthy(input$yvar) && isTruthy(input$group_var)) {
      # Add compare_means (auto t-test/ANOVA choice handled by ggpubr)
      g <- g + ggpubr::stat_compare_means(mapping = aes(group = .data[[input$group_var]]), label = "p.format")
    }
    
    # Apply scales
    if (!is.null(colour_scale)) g <- g + colour_scale
    if (!is.null(fill_scale))   g <- g + fill_scale
    
    # Labels & theme
    g <- g + labs(x = input$xvar, y = if (isTruthy(input$yvar)) input$yvar else NULL) + thm
    g
  })
  
  output$plot <- renderPlot({
    build_plot()
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

# Shiny helper for %||%
`%||%` <- function(a,b) if (!is.null(a)) a else b

shinyApp(ui, server)
