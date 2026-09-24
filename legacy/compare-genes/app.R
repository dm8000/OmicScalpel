# Paths come from config/config.txt -- see R/config.R
source("../../R/config.R")
Sys.setenv(R_LIBS = os_path("lib"))
Sys.setenv(R_LIBS_USER = os_path("lib"))
.libPaths(os_path("lib"))
library(shiny)
library(shinydashboard)
library(sortable)
library(readxl)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(rlang)
library(RColorBrewer)
library(purrr)
library(colourpicker)
library(svglite)
library(jsonlite)
library(ggbreak)
library(patchwork)

metadata_path <- os_path("hubdata", "Metadata.xlsx")
metadata <- read_excel(metadata_path)

ui <- dashboardPage(
  dashboardHeader(title = "Dataset Plotter"),
  dashboardSidebar(width = 325,
                   sidebarMenu(
                     menuItem("Boxplot Analysis", tabName = "boxplot_analysis", icon = icon("chart-bar")),
                     selectInput("dataset", "Select Dataset", choices = unique(metadata$dataset)),
                     uiOutput("condition_select"),
                     uiOutput("gene_select"),
                     actionButton("plot", "Generate Plot"),
                     box(title = "Reorder groups", status = "primary", solidHeader = TRUE,
                         collapsible = TRUE, collapsed = TRUE, width = 12,
                         uiOutput("sortable_conditions")
                     ),
                     box(title = "Color selection", status = "primary", solidHeader = TRUE,
                         collapsible = TRUE, collapsed = TRUE, width = 12,
                         uiOutput("colorpicker_ui")
                     ),
                     box(title = "Labels", status = "primary", solidHeader = TRUE,
                         collapsible = TRUE, collapsed = TRUE, width = 12,
                         textInput("plot_title", "Plot Title", value = "Facet Plot"),
                         textInput("x_label", "X-axis Label", value = "Conditions"),
                         textInput("y_label", "Y-axis Label", value = "Expression"),
                         div(style = "max-height: 150px; overflow-y: auto;",
                             sliderInput("title_size", "Title Font Size:", min = 8, max = 36, value = 15, step = 1),
                             sliderInput("axis_font_size", "Axis Labels Font Size:", min = 8, max = 24, value = 15, step = 1),
                             sliderInput("group_font_size", "Group Labels Font Size:", min = 8, max = 24, value = 15, step = 1),
                             sliderInput("facet_font_size", "Facet Labels Font Size:", min = 8, max = 24, value = 15, step = 1)
                         )
                     ),
                     box(title = "Size adjustments", status = "primary", solidHeader = TRUE,
                         collapsible = TRUE, collapsed = TRUE, width = 12,
                         div(style = "max-height: 200px; overflow-y: auto;",
                             sliderInput("plot_width", "Plot Width (pixels)", min = 400, max = 2000, value = 800, step = 50),
                             sliderInput("plot_height", "Plot Height (pixels)", min = 400, max = 2000, value = 800, step = 50),
                             sliderInput("jitter_size", "Jitter Dot Size:", min = 0, max = 10, value = 2.5, step = 0.1),
                             sliderInput("box_outline_size", "Box Outline Thickness:", min = 0, max = 3, value = 0.5, step = 0.1),
                             sliderInput("median_line_size", "Median Line Thickness:", min = 0, max = 3, value = 0.5, step = 0.1),
                             sliderInput("stat_line_size", "Statistics Line Thickness:", min = 0, max = 3, value = 0.5, step = 0.1)
                         )
                     ),
                     box(title = "Grid Controls", status = "primary", solidHeader = TRUE,
                         collapsible = TRUE, collapsed = TRUE, width = 12,
                         checkboxInput("show_major_x", "Show Major Vertical Grid Lines", value = TRUE),
                         checkboxInput("show_major_y", "Show Major Horizontal Grid Lines", value = TRUE),
                         checkboxInput("show_minor_x", "Show Minor Vertical Grid Lines", value = FALSE),
                         checkboxInput("show_minor_y", "Show Minor Horizontal Grid Lines", value = FALSE),
                         sliderInput("grid_line_size", "Grid Line Thickness:", min = 0.1, max = 2, value = 0.2, step = 0.1)
                     ),
                     box(title = "Statistics", status = "primary", solidHeader = TRUE,
                         collapsible = TRUE, collapsed = TRUE, width = 12,
                         checkboxInput("show_wilcox", "Show Wilcoxon Test Lines", value = FALSE),
                         checkboxInput("log2_transform", "Log2 Transform Data", value = FALSE),
                         actionButton("wilcox_test", "Perform Pairwise Wilcox Test")
                     ),
                     box(title = "Y axis scale", status = "primary", solidHeader = TRUE,
                         collapsible = TRUE, collapsed = TRUE, width = 12,
                         uiOutput("yaxis_sliders")
                     ),
                     box(title = "Aesthetic Settings", status = "primary", solidHeader = TRUE,
                         collapsible = TRUE, collapsed = TRUE, width = 12,
                         downloadButton("download_settings", "Export Settings"),
                         br(), br(),
                         fileInput("upload_settings", "Import Settings", accept = ".json"),
                         br(),
                         helpText("Export/Import color scheme and plot settings")
                     )
                   )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper .box.box-primary { background: white !important; border: 1px solid #FFFFFF !important; }
        .content-wrapper .box.box-primary > .box-body { background: white !important; }
        .main-sidebar .box.box-primary { background: transparent !important; border: 1px solid #FFFFFF !important; }
        .main-sidebar .box.box-primary > .box-body { background: transparent !important; }
        .main-sidebar .box .box-header .box-title { font-size: 16px; font-family: Calibri, sans-serif; font-weight: bold; color: #FFFFFF; }
        .bucket-list-container, .rank-list-container, .rank-list { background-color: rgb(34, 45, 50) !important; }
        .rank-list-item { background-color: #7AA4B8 !important; border: 1px solid #FFFFFF !important; border-radius: 2px !important; text-align: center !important; color: #FFFFFF !important; padding: 10px !important; margin-bottom: 5px !important; font-size: 14px !important; font-family: Calibri, sans-serif !important; }
        .rank-list-item:hover { background-color: #5A6C7A !important; }
        .rank-list-title { color: #FFFFFF !important; font-weight: bold !important; font-family: Calibri, sans-serif !important; font-size: 14px !important; margin-bottom: 10px !important; }
        .hidden-groups { border: 2px dashed #FFFFFF !important; background-color: rgb(65, 65, 65) !important; }
        #colorpicker_ui .form-group > label { font-size: 14px; font-family: Calibri, sans-serif; color: #FFFFFF; }
      "))
    ),
    tabItems(
      tabItem(tabName = "boxplot_analysis",
              box(title = "Facet Plot", width = 12, status = "primary",
                  div(style = "position: relative;",
                      uiOutput("plot_ui"),
                      absolutePanel(top = 10, right = 10, draggable = FALSE,
                                    downloadButton("download_plot", "Download PNG"),
                                    br(),
                                    downloadButton("download_plot_svg", "Download SVG"),
                                    br(),
                                    downloadButton("download_plot_pdf", "Download PDF")
                      )
                  )
              ),
              box(title = "Wilcoxon Test Results", width = 12, status = "primary",
                  tableOutput("wilcox_results")
              )
      )
    )
  )
)


server <- function(input, output, session) {
  unit_reactive  <- reactiveVal("LogTMM")
  plot_obj       <- reactiveVal(NULL)
  all_conditions <- reactiveVal(NULL)
  
  ## helper to find first‐existing expression file and set unit
  find_expr_path <- function(dataset) {
    files_to_check <- c("_TMM.txt","_CPM.txt","_TPM.txt",
                        "_FPKM.txt","_count.txt","_unknown_unit.txt")
    expr_path <- NULL
    for (suf in files_to_check) {
      p <- file.path(os_path("hubdata"),
                     dataset, paste0(dataset, suf))
      if (file.exists(p)) {
        # set unit
        u <- if (suf == "_unknown_unit.txt") "unknown" else
          toupper(sub("^_|\\.txt$", "", suf))
        unit_reactive(u)
        expr_path <- p
        break
      }
    }
    if (is.null(expr_path)) {
      stop("Nenhum arquivo válido foi encontrado para o dataset especificado.")
    }
    expr_path
  }
  
  output$plot_ui <- renderUI({
    req(input$plot_width, input$plot_height)
    plotOutput("facet_plot",
               width  = paste0(input$plot_width,  "px"),
               height = paste0(input$plot_height, "px"))
  })
  
  observeEvent(input$dataset, {
    req(input$dataset)
    
    ## metadata & condition selector as before
    selected_metadata <- metadata %>% filter(dataset == input$dataset)
    condition_columns <- names(selected_metadata)[4:ncol(selected_metadata)]
    valid_conditions  <- condition_columns[sapply(condition_columns, function(col) {
      vals <- unique(na.omit(selected_metadata[[col]]))
      length(vals) > 1 && !all(vals == "NA")
    })]
    
    output$condition_select <- renderUI({
      selectInput("conditions", "Select Conditions",
                  choices  = valid_conditions,
                  multiple = TRUE, selectize = TRUE)
    })
    
    ## find and read expression file
    expr_path <- find_expr_path(input$dataset)
    expr_tbl  <- read.delim(expr_path, stringsAsFactors = FALSE)
    
    ## gene selector
    output$gene_select <- renderUI({
      selectizeInput("genes", "Select Genes",
                     choices = NULL,
                     multiple = TRUE,
                     options = list(server = TRUE, maxOptions = 1000))
    })
    updateSelectizeInput(session, "genes",
                         choices = expr_tbl$Symbol,
                         server  = TRUE)
  })
  
  observeEvent(input$conditions, {
    req(input$conditions)
    sel_meta <- metadata %>% filter(dataset == input$dataset)
    vals_list <- lapply(input$conditions, function(cn)
      unique(sel_meta[[cn]]))
    all_conds <- apply(expand.grid(vals_list), 1, paste, collapse = ".")
    all_conditions(all_conds)
    
    output$sortable_conditions <- renderUI({
      bucket_list(
        header = NULL,
        add_rank_list("Visible Groups", labels   = all_conds,
                      input_id = "visible_conditions"),
        add_rank_list("Hidden Groups",  labels   = NULL,
                      input_id = "hidden_conditions",
                      class    = "hidden-groups"),
        orientation = "horizontal"
      )
    })
    outputOptions(output, "sortable_conditions",
                  suspendWhenHidden = FALSE)
    updateTextInput(session, "x_label",
                    value = paste(input$conditions, collapse = "&"))
  })
  
  output$colorpicker_ui <- renderUI({
    req(input$genes)
    n <- length(input$genes)
    default_cols <- brewer.pal(min(max(n,3),12),"Set3")
    if (n > length(default_cols))
      default_cols <- colorRampPalette(default_cols)(n)
    
    tagList(lapply(seq_along(input$genes), function(i) {
      colourInput(inputId = paste0("color_", i),
                  label   = input$genes[i],
                  value   = default_cols[i])
    }))
  })
  outputOptions(output, "colorpicker_ui", suspendWhenHidden = FALSE)
  
  reactive_palette <- reactive({
    req(input$genes)
    cols <- sapply(seq_along(input$genes), function(i) {
      inpt <- input[[paste0("color_", i)]]
      if (is.null(inpt)) {
        d <- brewer.pal(min(max(length(input$genes),3),12),"Set3")
        if (i <= length(d)) d[i] else "#CCCCCC"
      } else inpt
    })
    names(cols) <- input$genes
    cols
  })
  
  plot_data_reactive <- eventReactive(input$plot, {
    req(input$dataset, input$genes, input$visible_conditions)
    sel_meta <- metadata %>% filter(dataset == input$dataset)
    combined  <- apply(sel_meta[input$conditions], 1, paste, collapse=".")
    keep      <- combined %in% input$visible_conditions
    sel_meta  <- sel_meta[keep, ]
    sel_meta$CombinedCondition <- factor(combined[keep],
                                         levels = input$visible_conditions)
    samp_ids  <- sel_meta$SampleID
    
    ## re-find the expression file
    expr_path <- find_expr_path(input$dataset)
    expr_tbl  <- read.delim(expr_path, stringsAsFactors = FALSE)
    
    df_list <- lapply(input$genes, function(g) {
      gd <- expr_tbl %>% filter(Symbol == g) %>% select(-Symbol)
      gd <- gd %>% select(all_of(samp_ids))
      data.frame(
        Expression        = as.numeric(gd),
        SampleID          = samp_ids,
        Gene              = g,
        CombinedCondition = sel_meta$CombinedCondition,
        stringsAsFactors  = FALSE
      )
    })
    df <- bind_rows(df_list)
    if (input$log2_transform) df$Expression <- log2(df$Expression + 1)
    df
  })
  
  output$yaxis_sliders <- renderUI({
    df <- plot_data_reactive()
    req(df)
    max_vals <- df %>% group_by(Gene) %>%
      summarise(maxE = max(Expression, na.rm = TRUE))
    
    tagList(lapply(seq_len(nrow(max_vals)), function(i) {
      gene <- max_vals$Gene[i]
      mx   <- max_vals$maxE[i]
      sliderInput(paste0("ymax_", gene),
                  label = paste("Max Y for", gene),
                  min   = 0,
                  max   = ceiling(mx * 2),
                  value = ceiling(mx),
                  step  = ceiling(mx / 100))
    }))
  })
  
  generate_plot <- reactive({
    df <- plot_data_reactive()
    req(df)
    df$Gene <- factor(df$Gene, levels = input$genes)
    pal     <- reactive_palette()
    
    y_lab_full <- if (input$log2_transform) {
      paste0("Log2(", input$y_label, ") (", unit_reactive(), ")")
    } else {
      paste0(input$y_label, " (", unit_reactive(), ")")
    }
    
    p <- ggplot(df, aes(x = Gene, y = Expression, fill = Gene)) +
      geom_boxplot(outlier.shape = NA,
                   color  = if (input$box_outline_size > 0) "black" else NA,
                   alpha  = 0.7,
                   size   = input$box_outline_size,
                   fatten = if (input$median_line_size > 0)
                     input$median_line_size * 2 else 0) +
      { if (input$jitter_size > 0)
        geom_jitter(position = position_jitter(width = 0),
                    color = "black",
                    size  = input$jitter_size,
                    alpha = 0.6)
      } +
      scale_fill_manual(values = pal) +
      labs(x = input$x_label, y = y_lab_full) +
      theme_minimal(base_size = input$axis_font_size) +
      theme(
        plot.title        = element_text(size = input$title_size,
                                         face = "bold", hjust = 0.5),
        axis.text.x       = element_text(angle = 45, hjust = 1,
                                         size = input$group_font_size),
        axis.text.y       = element_text(size = input$group_font_size),
        strip.text        = element_text(size = input$facet_font_size,
                                         face = "bold"),
        legend.position   = "none",
        panel.background  = element_rect(fill = "white", colour = NA),
        plot.background   = element_rect(fill = "white", colour = NA),
        panel.grid.major.x= element_line(colour = ifelse(input$show_major_x,
                                                         "grey90", NA),
                                         size = input$grid_line_size),
        panel.grid.major.y= element_line(colour = ifelse(input$show_major_y,
                                                         "grey90", NA),
                                         size = input$grid_line_size),
        panel.grid.minor.x= element_line(colour = ifelse(input$show_minor_x,
                                                         "grey95", NA),
                                         size = input$grid_line_size * 0.5),
        panel.grid.minor.y= element_line(colour = ifelse(input$show_minor_y,
                                                         "grey95", NA),
                                         size = input$grid_line_size * 0.5)
      ) +
      facet_wrap(~ CombinedCondition, scales = "free_y") +
      plot_annotation(title = input$plot_title)
    
    if (input$show_wilcox && input$stat_line_size > 0) {
      gene_pairs <- combn(levels(df$Gene), 2, simplify = FALSE)
      p <- p + geom_pwc(
        comparisons = gene_pairs,
        method      = "wilcox.test",
        label       = "p.signif",
        symnum.args = list(
          cutpoints = c(0, 0.0001, 0.001, 0.01, 0.05, Inf),
          symbols   = c("****","***","**","*","ns")
        ),
        hide.ns     = TRUE,
        size        = input$stat_line_size
      )
    }
    
    plot_obj(p)
    p
  })
  
  output$facet_plot <- renderPlot({ generate_plot() })
  
  ## download handlers as before...
  output$download_plot <- downloadHandler(
    filename = function() paste0("facet_plot_", Sys.Date(), ".png"),
    content  = function(file) {
      req(plot_obj())
      ggsave(file, plot = plot_obj(), device = "png", dpi = 300,
             width  = input$plot_width/72,
             height = input$plot_height/72)
    }
  )
  output$download_plot_svg <- downloadHandler(
    filename = function() paste0("facet_plot_", Sys.Date(), ".svg"),
    content  = function(file) {
      req(plot_obj())
      ggsave(file, plot = plot_obj(),
             device = svglite::svglite, dpi = 300,
             width  = input$plot_width/72,
             height = input$plot_height/72)
    }
  )
  output$download_plot_pdf <- downloadHandler(
    filename = function() paste0("facet_plot_", Sys.Date(), ".pdf"),
    content  = function(file) {
      req(plot_obj())
      ggsave(file, plot = plot_obj(), device = "pdf", dpi = 300,
             width  = input$plot_width/72,
             height = input$plot_height/72)
    }
  )
  
  ## settings export/import & table‐based wilcox as before...
  get_aesthetic_settings <- reactive({
    req(input$genes)
    cols <- sapply(seq_along(input$genes), function(i)
      input[[paste0("color_", i)]])
    names(cols) <- input$genes
    list(
      plot_width      = input$plot_width,
      plot_height     = input$plot_height,
      plot_title      = input$plot_title,
      x_label         = input$x_label,
      y_label         = input$y_label,
      title_size      = input$title_size,
      axis_font_size  = input$axis_font_size,
      group_font_size = input$group_font_size,
      facet_font_size = input$facet_font_size,
      jitter_size     = input$jitter_size,
      box_outline_size= input$box_outline_size,
      median_line_size= input$median_line_size,
      stat_line_size  = input$stat_line_size,
      grid_line_size  = input$grid_line_size,
      show_major_x    = input$show_major_x,
      show_major_y    = input$show_major_y,
      show_minor_x    = input$show_minor_x,
      show_minor_y    = input$show_minor_y,
      show_wilcox     = input$show_wilcox,
      log2_transform  = input$log2_transform,
      colors          = as.list(cols)
    )
  })
  output$download_settings <- downloadHandler(
    filename = function() paste0("plot_settings_", Sys.Date(), ".json"),
    content  = function(file) {
      write_json(get_aesthetic_settings(), file, pretty = TRUE)
    }
  )
  observeEvent(input$upload_settings, {
    req(input$upload_settings)
    settings <- fromJSON(input$upload_settings$datapath)
    updateSliderInput(session, "plot_width", value = settings$plot_width)
    updateSliderInput(session, "plot_height", value = settings$plot_height)
    updateTextInput(session, "plot_title", value = settings$plot_title)
    updateTextInput(session, "x_label",   value = settings$x_label)
    updateTextInput(session, "y_label",   value = settings$y_label)
    updateSliderInput(session, "title_size",      value = settings$title_size)
    updateSliderInput(session, "axis_font_size",  value = settings$axis_font_size)
    updateSliderInput(session, "group_font_size", value = settings$group_font_size)
    updateSliderInput(session, "facet_font_size", value = settings$facet_font_size)
    updateSliderInput(session, "jitter_size",     value = settings$jitter_size)
    updateSliderInput(session, "box_outline_size",value = settings$box_outline_size)
    updateSliderInput(session, "median_line_size",value = settings$median_line_size)
    updateSliderInput(session, "stat_line_size",  value = settings$stat_line_size)
    updateSliderInput(session, "grid_line_size",  value = settings$grid_line_size)
    updateCheckboxInput(session, "show_major_x",   value = settings$show_major_x)
    updateCheckboxInput(session, "show_major_y",   value = settings$show_major_y)
    updateCheckboxInput(session, "show_minor_x",   value = settings$show_minor_x)
    updateCheckboxInput(session, "show_minor_y",   value = settings$show_minor_y)
    updateCheckboxInput(session, "show_wilcox",    value = settings$show_wilcox)
    if (!is.null(settings$log2_transform)) {
      updateCheckboxInput(session, "log2_transform", value = settings$log2_transform)
    }
    imported_colors <- reactiveVal(settings$colors)
    observe({
      req(input$visible_conditions, imported_colors())
      cols <- imported_colors()
      for (i in seq_along(input$visible_conditions)) {
        nm <- input$visible_conditions[i]
        if (nm %in% names(cols)) {
          updateColourInput(session, paste0("color_", i), value = cols[[nm]])
        }
      }
    })
  })
  observeEvent(input$wilcox_test, {
    # table of gene–gene p-values per condition
    results <- do.call(rbind, lapply(input$visible_conditions, function(cond) {
      subdf <- plot_data_reactive() %>% filter(CombinedCondition == cond)
      pairs <- combn(levels(subdf$Gene), 2, simplify = FALSE)
      do.call(rbind, lapply(pairs, function(pair) {
        v1 <- subdf$Expression[subdf$Gene == pair[1]]
        v2 <- subdf$Expression[subdf$Gene == pair[2]]
        p  <- wilcox.test(v1, v2)$p.value
        data.frame(
          Condition = cond,
          Gene1     = pair[1],
          Gene2     = pair[2],
          P.Value   = p,
          stringsAsFactors = FALSE
        )
      }))
    }))
    output$wilcox_results <- renderTable({
      results %>%
        mutate(Significance = case_when(
          P.Value <= 0.0001 ~ "****",
          P.Value <= 0.001  ~ "***",
          P.Value <= 0.01   ~ "**",
          P.Value <= 0.05   ~ "*",
          TRUE              ~ "ns"
        )) %>%
        arrange(Condition, P.Value)
    })
  })
}


shinyApp(ui = ui, server = server)

