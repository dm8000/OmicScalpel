Sys.setenv(R_LIBS = "/n/shiny/OmicScalpel/lib")
Sys.setenv(R_LIBS_USER = "/n/shiny/OmicScalpel/lib")
.libPaths("/n/shiny/OmicScalpel/lib")
library(shiny)
library(shinydashboard)
library(sortable)
library(readxl)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(rlang)
library(RColorBrewer)
library(viridis)
library(colourpicker)
library(jsonlite)
library(svglite)    # for SVG export

metadata_path <- "/n/shiny/OmicScalpel/hubdata/Metadata.xlsx"
metadata <- read_excel(metadata_path)

ui <- dashboardPage(
  dashboardHeader(title = "Dataset Plotter"),
  dashboardSidebar(
    width = 325,
    tags$head(
      tags$style(HTML("
        /* existing sidebar box styling */
        .main-sidebar .box {
          background-color: transparent !important;
          border: none !important;
          box-shadow: none !important;
        }
        .main-sidebar .box .box-body {
          background-color: transparent !important;
          border-top: none !important;
        }
        .main-sidebar .box .box-footer {
          background-color: transparent !important;
          border-top: none !important;
        }

        /* sortable bucket styling */
        .bucket-list-container,
        .rank-list-container,
        .rank-list {
          background-color: rgb(34, 45, 50) !important;
        }
        .rank-list-item {
          background-color: #7AA4B8 !important;
          border: 1px solid #FFFFFF !important;
          border-radius: 2px !important;
          text-align: center !important;
          color: #FFFFFF !important;
          padding: 10px !important;
          margin-bottom: 5px !important;
          font-size: 14px !important;
          font-family: Calibri, sans-serif !important;
        }
        .rank-list-item:hover {
          background-color: #5A6C7A !important;
        }
        .rank-list-title {
          color: #FFFFFF !important;
          font-weight: bold !important;
          font-family: Calibri, sans-serif !important;
          font-size: 14px !important;
          margin-bottom: 10px !important;
        }
        .hidden-groups {
          border: 2px dashed #FFFFFF !important;
          background-color: rgb(65, 65, 65) !important;
        }
      "))
    ),
    sidebarMenu(
      menuItem("Continuous Data", tabName = "continuous_data", icon = icon("chart-line")),
                           tags$div(style = "display: flex; align-items: center;",
                              # your existing selector
                              selectInput("dataset", "Select Dataset", choices = unique(metadata$dataset)),
                              # the new reload button
                              actionButton("reload_metadata", label = NULL,
                                           icon = icon("refresh"),   # font-awesome reload symbol
                                           title = "Re-read metadata",  # hover tooltip
                                           style = "margin-left: 8px;")
                              ),
      uiOutput("condition_select"),
      uiOutput("gene_select"),
      uiOutput("numeric_column_select"),
      actionButton("plot", "Plot"),
      
      box(
        title = "Reorder & hide groups", status = "primary", solidHeader = TRUE,
        collapsible = TRUE, collapsed = TRUE, width = 12,
        uiOutput("sortable_conditions")
      ),
      
      box(
        title = "Labels", status = "primary", solidHeader = TRUE,
        collapsible = TRUE, collapsed = TRUE, width = 12,
        textInput("plot_title", "Plot Title", value = "My Plot Title"),
        textInput("x_axis_label", "X-axis Label", value = "Numeric Value"),
        textInput("y_axis_label", "Y-axis Label", value = "Expression"),
        div(style = "max-height: 150px; overflow-y: auto;",
            sliderInput("title_font_size", "Title Font Size:", min = 10, max = 30, value = 18),
            sliderInput("axis_label_font_size", "Axis Labels Font Size:", min = 8, max = 20, value = 15),
            sliderInput("axis_text_font_size", "Axis Values Font Size:", min = 6, max = 16, value = 10),
            sliderInput("stat_text_font_size", "Stat Text Font Size:", min = 2, max = 8, value = 5)
        )
      ),
      
      box(
        title = "Size adjustments", status = "primary", solidHeader = TRUE,
        collapsible = TRUE, collapsed = TRUE, width = 12,
        div(style = "max-height: 200px; overflow-y: auto;",
            sliderInput("plot_width", "Plot Width (px)", min = 400, max = 2000, value = 800, step = 50),
            sliderInput("plot_height", "Plot Height (px)", min = 400, max = 2000, value = 800, step = 50),
            sliderInput("plot_cols", "Columns", min = 1, max = 10, value = 3),
            sliderInput("plot_rows", "Rows", min = 1, max = 10, value = 3),
            sliderInput("dot_size", "Dot Size", min = 1, max = 5, value = 2, step = 0.1),
            sliderInput("line_thickness", "Line Thickness", min = 0.1, max = 3, value = 0.5, step = 0.1)
        )
      ),
      
      box(
        title = "Statistics", status = "primary", solidHeader = TRUE,
        collapsible = TRUE, collapsed = TRUE, width = 12,
        actionButton("correlation_test", "Perform Spearman Correlation Test"),
        checkboxInput("log2_y", "Log2 Transform Y-axis", FALSE),
        checkboxInput("log2_x", "Log2 Transform X-axis", FALSE)
      ),
      
      box(
        title = "Color selection", status = "primary", solidHeader = TRUE,
        collapsible = TRUE, collapsed = TRUE, width = 12,
        selectInput("color_palette", "Color Palette",
                    choices = c("Dark2","Set1","Accent","Paired","Set2","Set3",
                                "Pastel1","Pastel2","Custom","viridis","magma",
                                "plasma","inferno")),
        conditionalPanel(
          condition = "input.color_palette == 'Custom'",
          uiOutput("custom_palette_ui")
        )
      ),
      
      box(
        title = "Settings", status = "primary", solidHeader = TRUE,
        collapsible = TRUE, collapsed = TRUE, width = 12,
        downloadButton("download_settings", "Export Settings"),
        br(), br(),
        fileInput("upload_settings", "Import Settings", accept = ".json"),
        helpText("Export/Import color & plot settings")
      )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "continuous_data",
              box(
                title = "Facet Plot", width = 12,
                div(style = "position: relative;",
                    plotOutput("facet_plot",
                               width  = "auto",
                               height = "auto"),
                    absolutePanel(top = 10, right = 10, draggable = FALSE,
                                  downloadButton("download_plot_png", "PNG"),
                                  br(),
                                  downloadButton("download_plot_svg", "SVG"),
                                  br(),
                                  downloadButton("download_plot_pdf", "PDF")
                    )
                )
              )
      )
    )
  )
)

server <- function(input, output, session) {
  
  observeEvent(input$reload_metadata, {
    metadata <<- read_excel(metadata_path)
    updateSelectInput(session, "dataset",
                      choices  = unique(metadata$dataset),
                      selected = isolate(input$dataset))
  })
  
  # Helpers ---------------------------------------------------------------
  get_conditions_with_multiple_values <- function(ds) {
    sel <- metadata %>% filter(dataset == ds)
    cols <- names(sel)[4:ncol(sel)]
    keep <- sapply(cols, function(cn) {
      vals <- unique(sel[[cn]])
      vals <- vals[!is.na(vals) & vals != "NA"]
      length(vals) > 1
    })
    names(which(keep))
  }
  get_numeric_columns_with_multiple_values <- function(ds) {
    sel <- metadata %>% filter(dataset == ds)
    
    numcols <- sapply(names(sel), function(cn) {
      col_data <- sel[[cn]]
      if (is.numeric(col_data)) {
        clean_vals <- col_data[!is.na(col_data)]
        return(length(unique(clean_vals)) >= 2)
      }
      char_data <- as.character(col_data)
      is_valid <- grepl("^-?\\d*\\.?\\d+$", char_data, perl = TRUE)
      numeric_vals <- suppressWarnings(as.numeric(char_data))
      numeric_vals[!is_valid] <- NA
      clean_vals <- numeric_vals[!is.na(numeric_vals)]
      length(unique(clean_vals)) >= 2
    })
    
    names(sel)[numcols]
  }
  
  # Custom palette UI -----------------------------------------------------
  output$custom_palette_ui <- renderUI({
    req(input$visible_conditions)
    n <- length(input$visible_conditions)
    base <- brewer.pal(min(n,12), "Set3")
    if (n > length(base)) base <- colorRampPalette(base)(n)
    lapply(seq_along(input$visible_conditions), function(i) {
      colourInput(paste0("custom_color_", i),
                  input$visible_conditions[i],
                  value = base[i])
    })
  })
  
  # Reactive palette ------------------------------------------------------
  reactive_palette <- reactive({
    if (is.null(input$conditions) || length(input$conditions) == 0) {
      return("black")
    }
    sel_md <- metadata %>% filter(dataset == input$dataset)
    combos <- expand.grid(
      lapply(input$conditions, function(c) unique(sel_md[[c]]))
    )
    default_lbls <- apply(combos, 1, paste, collapse = ".")
    order_conds <- if (!is.null(input$visible_conditions)) {
      input$visible_conditions
    } else {
      default_lbls
    }
    n <- length(order_conds)
    if (input$color_palette == "Custom") {
      sapply(seq_len(n), function(i) input[[paste0("custom_color_", i)]])
    } else if (input$color_palette %in% c("viridis","magma","plasma","inferno")) {
      viridis::viridis(n, option = input$color_palette)
    } else {
      colorRampPalette(brewer.pal(min(9,n), input$color_palette))(n)
    }
  })
  
  # Build condition & gene selectors --------------------------------------
  observeEvent(input$dataset, {
    output$condition_select <- renderUI({
      selectInput("conditions", "Select Conditions",
                  choices = get_conditions_with_multiple_values(input$dataset),
                  multiple = TRUE)
    })
    files <- c("_TMM.txt","_CPM.txt","_TPM.txt","_FPKM.txt","_count.txt","_unknown_unit.txt")
    tmm_path <- NULL; unit <- ""
    for (suf in files) {
      p <- file.path("/n/shiny/OmicScalpel/hubdata", input$dataset, paste0(input$dataset, suf))
      if (file.exists(p)) {
        tmm_path <- p
        unit <- if (suf=="_unknown_unit.txt") "unknown" else toupper(sub("^_|\\.txt$","", suf))
        break
      }
    }
    req(tmm_path)
    updateTextInput(session, "y_axis_label", value = paste0("Expression (", unit, ")"))
    tmm_data <- read.delim(tmm_path)
    output$gene_select <- renderUI({
      selectizeInput("genes", "Select Genes",
                     choices = unique(tmm_data$Symbol),
                     multiple = TRUE, options = list(server=TRUE, maxOptions=1000))
    })
    vals <- get_numeric_columns_with_multiple_values(input$dataset)
    output$numeric_column_select <- renderUI({
      if (length(vals) == 0) h4("No numeric columns…")
      else selectInput("numeric_columns", "Select Numeric Columns", choices = vals)
    })
  })
  
  # Build reorder & hide widget ------------------------------------------
  observeEvent(input$conditions, {
    req(input$conditions)
    sel_md <- metadata %>% filter(dataset == input$dataset)
    vals_list <- lapply(input$conditions, function(cn) unique(sel_md[[cn]]))
    all_conds <- apply(expand.grid(vals_list), 1, paste, collapse = ".")
    
    output$sortable_conditions <- renderUI({
      bucket_list(
        header = NULL,
        add_rank_list(
          text     = "Visible Groups",
          labels   = all_conds,
          input_id = "visible_conditions"
        ),
        add_rank_list(
          text     = "Hidden Groups",
          labels   = NULL,
          input_id = "hidden_conditions",
          class    = "hidden-groups"
        ),
        orientation = "horizontal"
      )
    })
    outputOptions(output, "sortable_conditions", suspendWhenHidden = FALSE)
  })
  
  # Reactive plot object --------------------------------------------------
  plot_reactive <- reactive({
    req(input$plot, input$dataset, input$genes)
    
    # 1) Determine panels
    panels_needed    <- {
      if (is.null(input$conditions) || length(input$conditions)==0) {
        length(input$genes)
      } else {
        if (!is.null(input$visible_conditions)) length(input$visible_conditions)
        else {
          sel_md2 <- metadata %>% filter(dataset==input$dataset)
          nrow(expand.grid(lapply(input$conditions, function(c) unique(sel_md2[[c]]))))
        }
      }
    }
    panels_available <- input$plot_rows * input$plot_cols
    if (panels_needed > panels_available) {
      return(
        ggplot() +
          annotate("text", x=0.5,y=0.5,
                   label="Error: Not enough panels.\nIncrease rows or columns.",
                   size=6) +
          theme_void()
      )
    }
    
    # 2) Metadata + CombinedCondition
    sel_md <- metadata %>% filter(dataset==input$dataset)
    if (is.null(input$conditions) || length(input$conditions)==0) {
      sel_md$CombinedCondition <- factor("All Data")
    } else {
      default_lbls <- apply(
        expand.grid(lapply(input$conditions, function(c) unique(sel_md[[c]]))),
        1, paste, collapse="."
      )
      order_conds <- if (!is.null(input$visible_conditions)) input$visible_conditions else default_lbls
      sel_md$CombinedCondition <- factor(
        apply(sel_md[input$conditions],1,paste,collapse="."),
        levels=order_conds
      )
      sel_md <- sel_md %>% filter(CombinedCondition %in% order_conds)
    }
    
    # 3) Expression
    files <- c("_TMM.txt","_CPM.txt","_TPM.txt","_FPKM.txt","_count.txt","_unknown_unit.txt")
    tmm_path <- NULL
    for (s in files) {
      p <- file.path("/n/shiny/OmicScalpel/hubdata", input$dataset, paste0(input$dataset,s))
      if (file.exists(p)) { tmm_path <- p; break }
    }
    expr <- read.delim(tmm_path)
    
    # 4) Build df
    ids <- sel_md$SampleID
    plot_df <- bind_rows(lapply(input$genes, function(g) {
      row_vals <- expr %>% filter(Symbol==g) %>% select(all_of(ids))
      vals     <- as.numeric(unlist(row_vals[1,]))
      data.frame(
        Expression        = vals,
        SampleID          = ids,
        Gene              = g,
        NumericValue      = as.numeric(sel_md[[input$numeric_columns]]),
        CombinedCondition = sel_md$CombinedCondition,
        stringsAsFactors  = FALSE
      )
    }))
    
    # 5) Log transforms
    ylab <- input$y_axis_label
    if (input$log2_y) {
      plot_df$Expression <- log2(plot_df$Expression + 1)
      ylab <- paste0("log2(", ylab, " + 1)")
    }
    xlab <- input$x_axis_label
    if (input$log2_x) {
      plot_df$NumericValue <- log2(plot_df$NumericValue + 1)
      xlab <- paste0("log2(", xlab, " + 1)")
    }
    
    # 6) Base plot
    p <- ggplot(plot_df, aes(x=NumericValue, y=Expression, color=CombinedCondition)) +
      geom_point(size=input$dot_size, alpha=0.5) +
      geom_smooth(method="lm", formula=y~x, se=FALSE,
                  linetype="dashed", size=input$line_thickness) +
      scale_color_manual(values=reactive_palette()) +
      facet_wrap(vars(Gene, CombinedCondition),
                 ncol=input$plot_cols,
                 nrow=input$plot_rows,
                 scales="free_y") +
      labs(x=xlab, y=ylab, title=input$plot_title) +
      theme_minimal() +
      theme(
        plot.title       = element_text(hjust=0.5, face="bold", size=input$title_font_size),
        axis.text.x      = element_text(angle=45, hjust=1, size=input$axis_text_font_size),
        axis.text.y      = element_text(size=input$axis_text_font_size),
        panel.grid       = element_blank(),
        axis.line        = element_line(color="black"),
        axis.title       = element_text(face="bold", size=input$axis_label_font_size),
        axis.ticks       = element_line(color="black"),
        strip.text       = element_text(face="bold", size=input$axis_label_font_size),
        strip.background = element_blank(),
        legend.position  = "none"
      )
    
    # 7) Correlation labels
    if (input$correlation_test > 0) {
      cors <- plot_df %>%
        filter(!is.na(CombinedCondition), !is.na(Expression), !is.na(NumericValue)) %>%
        group_by(Gene, CombinedCondition) %>%
        summarize(
          SpearmanCorr = cor(Expression, NumericValue, method="spearman", use="complete.obs"),
          PValue       = cor.test(Expression, NumericValue, method="spearman", exact=FALSE)$p.value,
          .groups      = "drop"
        ) %>%
        mutate(Label = paste0("rho=", round(SpearmanCorr,2), "\np=", signif(PValue,2)))
      p <- p + geom_text(
        data       = cors,
        aes(x=Inf, y=Inf, label=Label),
        inherit.aes = FALSE, hjust=1, vjust=1,
        size        = input$stat_text_font_size, fontface="bold"
      )
    }
    
    p
  })
  
  # Render Plot ------------------------------------------------------------
  output$facet_plot <- renderPlot({
    plot_reactive()
  },
  width  = reactive(input$plot_width),
  height = reactive(input$plot_height),
  res    = 96)
  
  # Download Handlers -----------------------------------------------------
  output$download_plot_png <- downloadHandler(
    filename = function() paste0("facet_plot_", Sys.Date(), ".png"),
    content = function(file) {
      ggsave(file, plot = plot_reactive(),
             device = "png", dpi = 300,
             width = input$plot_width/72,
             height= input$plot_height/72)
    }
  )
  output$download_plot_svg <- downloadHandler(
    filename = function() paste0("facet_plot_", Sys.Date(), ".svg"),
    content = function(file) {
      svglite::svglite(file,
                       width = input$plot_width/72,
                       height= input$plot_height/72)
      print(plot_reactive())
      dev.off()
    }
  )
  output$download_plot_pdf <- downloadHandler(
    filename = function() paste0("facet_plot_", Sys.Date(), ".pdf"),
    content = function(file) {
      ggsave(file, plot = plot_reactive(),
             device = "pdf", dpi = 300,
             width = input$plot_width/72,
             height= input$plot_height/72)
    }
  )
  
  # Export settings --------------------------------------------------------
  output$download_settings <- downloadHandler(
    filename = function() {
      paste0("plot_settings_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".json")
    },
    content = function(file) {
      s <- list(
        plot_title           = input$plot_title,
        title_font_size      = input$title_font_size,
        x_axis_label         = input$x_axis_label,
        y_axis_label         = input$y_axis_label,
        axis_label_font_size = input$axis_label_font_size,
        axis_text_font_size  = input$axis_text_font_size,
        stat_text_font_size  = input$stat_text_font_size,
        plot_width           = input$plot_width,
        plot_height          = input$plot_height,
        plot_cols            = input$plot_cols,
        plot_rows            = input$plot_rows,
        dot_size             = input$dot_size,
        line_thickness       = input$line_thickness,
        color_palette        = input$color_palette
      )
      if (input$color_palette == "Custom" && !is.null(input$visible_conditions)) {
        s$custom_colors <- lapply(seq_along(input$visible_conditions),
                                  function(i) input[[paste0("custom_color_", i)]])
      }
      writeLines(toJSON(s, pretty = TRUE), file)
    }
  )
  
  # Import settings --------------------------------------------------------
  observeEvent(input$upload_settings, {
    req(input$upload_settings)
    try({
      s <- fromJSON(readLines(input$upload_settings$datapath))
      updateTextInput(session,   "plot_title",           value = s$plot_title)
      updateSliderInput(session, "title_font_size",      value = s$title_font_size)
      updateTextInput(session,   "x_axis_label",         value = s$x_axis_label)
      updateTextInput(session,   "y_axis_label",         value = s$y_axis_label)
      updateSliderInput(session, "axis_label_font_size", value = s$axis_label_font_size)
      updateSliderInput(session, "axis_text_font_size",  value = s$axis_text_font_size)
      updateSliderInput(session, "stat_text_font_size",  value = s$stat_text_font_size)
      updateSliderInput(session, "plot_width",           value = s$plot_width)
      updateSliderInput(session, "plot_height",          value = s$plot_height)
      updateSliderInput(session, "plot_cols",            value = s$plot_cols)
      updateSliderInput(session, "plot_rows",            value = s$plot_rows)
      updateSliderInput(session, "dot_size",             value = s$dot_size)
      updateSliderInput(session, "line_thickness",       value = s$line_thickness)
      updateSelectInput(session, "color_palette",        selected = s$color_palette)
      if (!is.null(s$custom_colors) && !is.null(input$visible_conditions)) {
        for (i in seq_along(s$custom_colors)) {
          updateColourInput(session, paste0("custom_color_", i),
                            value = s$custom_colors[[i]])
        }
      }
    }, silent = TRUE)
  })
}

shinyApp(ui, server)
