# Load necessary libraries
library(shiny)
library(shinyjs)

# --- Helper Functions ---

# Helper to generate a reproducible R code string for the list
generate_r_code <- function(header) {
  deparse_structure <- function(h) {
    if (is.null(h) || length(h) == 0) return("list()")
    
    items <- lapply(h, function(spec) {
      elements <- c(
        paste0("label = \"", spec$label, "\""),
        paste0("just = \"", spec$just, "\"")
      )
      if (!is.null(spec$align_on)) {
        elements <- c(elements, paste0("align_on = \"", spec$align_on, "\""))
      }
       if (!is.null(spec$pct)) {
        elements <- c(elements, paste0("pct = ", spec$pct))
      }
      if (!is.null(spec$sub) && length(spec$sub) > 0) {
        elements <- c(elements, paste0("sub = ", deparse_structure(spec$sub)))
      }
      paste0("list(", paste(elements, collapse = ", "), ")")
    })
    # Use indent to make the output more readable
    paste0("list(\n  ", paste(items, collapse = ",\n  "), "\n)")
  }
  
  deparse_structure(header)
}


# --- Shiny UI ---
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      .header-item { 
        border: 1px solid #ddd; 
        border-radius: 5px; 
        padding: 10px; 
        margin: 5px; 
        background-color: #f9f9f9;
        cursor: pointer;
        text-align: center;
        transition: background-color 0.2s, border-color 0.2s;
      }
      .header-item:hover {
        background-color: #f0f0f0;
      }
      .header-item.selected {
        border-color: #337ab7;
        background-color: #dbe9f5;
        box-shadow: 0 0 5px #337ab7;
      }
      .spanner {
        border: 2px solid #5cb85c;
        background-color: #e8f5e8;
      }
      .spanner > .header-item-content > .header-item-children {
        display: flex;
        justify-content: center;
        align-items: flex-start;
        flex-wrap: wrap;
      }
      .header-item-label {
        font-weight: bold;
        margin-bottom: 5px;
      }
      #header-display {
        padding: 15px;
        border: 1px solid #ccc;
        border-radius: 5px;
        min-height: 150px;
        background-color: #fff;
        display: flex;
        justify-content: center;
        align-items: flex-start;
      }
      .control-panel {
        background-color: #f8f9fa;
        padding: 20px;
        border-radius: 5px;
        border: 1px solid #dee2e6;
      }
      h3 {
        border-bottom: 2px solid #dee2e6;
        padding-bottom: 10px;
        margin-top: 0;
      }
      .btn-full-width {
        width: 100%;
      }
    "))
  ),
  titlePanel("Interactive R Table Header Builder"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      class = "control-panel",
      h3("Controls"),
      
      textInput("new_col_label", "New Column Label", placeholder = "e.g., Column A"),
      actionButton("add_col_btn", "Add Column", class = "btn-primary btn-full-width"),
      hr(),
      
      textInput("spanner_label", "Spanner Label", placeholder = "e.g., Group 1"),
      actionButton("span_btn", "Create Spanner from Selection", class = "btn-success btn-full-width"),
      hr(),
      
      h4("Edit Selected Item"),
      uiOutput("edit_panel_ui"),
      
      hr(),
      actionButton("reset_btn", "Reset All", class = "btn-danger btn-full-width")
    ),
    mainPanel(
      width = 9,
      h3("Preview Placeholder"),
      p("The output from your package's function would be displayed here. Use the code below to generate the preview:"),
      verbatimTextOutput("preview_code_placeholder"),
      
      hr(),
      
      h3("Interactive Structure Builder"),
      p("Click items to select them. Select adjacent items to group them into a spanner."),
      uiOutput("header_display"),
      
      br(),
      
      h3("Generated R Code for 'specs' object"),
      verbatimTextOutput("header_code_output")
    )
  )
)

# --- Shiny Server ---
server <- function(input, output, session) {
  
  header_structure <- reactiveVal(list())
  selected_path <- reactiveVal(NULL)
  
  # --- Recursive UI Rendering ---
  render_header_ui <- function(header_list, current_path = "") {
    children <- lapply(seq_along(header_list), function(i) {
      item <- header_list[[i]]
      path_id <- if (current_path == "") as.character(i) else paste(current_path, i, sep = "-")
      item_class <- "header-item"
      item_content <- div(class = "header-item-label", item$label)
      if (!is.null(item$sub) && length(item$sub) > 0) {
        item_class <- paste(item_class, "spanner")
        item_content <- div(class="header-item-content",
          item_content,
          div(class="header-item-children", render_header_ui(item$sub, path_id))
        )
      }
      if (!is.null(selected_path()) && path_id %in% selected_path()) {
        item_class <- paste(item_class, "selected")
      }
      div(id = paste0("item-", path_id), class = item_class,
          onclick = paste0("Shiny.setInputValue('selected_item_path', '", path_id, "', {priority: 'event'});"),
          item_content)
    })
    return(children)
  }
  
  output$header_display <- renderUI({
    if(length(header_structure()) == 0) {
      return(p("Add a column to begin.", style="color: #888; text-align: center; padding: 20px;"))
    }
    render_header_ui(header_structure())
  })
  
  # --- Observers for Actions ---
  observeEvent(input$add_col_btn, {
    req(input$new_col_label)
    new_col <- list(label = input$new_col_label, just = 'c')
    header_structure(c(header_structure(), list(new_col)))
    updateTextInput(session, "new_col_label", value = "")
    selected_path(NULL)
  })
  
  observeEvent(input$selected_item_path, {
    path <- input$selected_item_path
    current_selection <- selected_path()
    if (is.null(current_selection) || !are_siblings(current_selection[1], path)) {
       selected_path(path)
    } else {
       new_selection <- toggle_selection(current_selection, path)
       if (is_contiguous(new_selection)) {
         selected_path(new_selection)
       } else {
         selected_path(path)
       }
    }
  })
  
  get_parent_path <- function(path) {
    parts <- strsplit(path, "-")[[1]]
    if (length(parts) == 1) return("")
    paste(parts[-length(parts)], collapse = "-")
  }
  are_siblings <- function(path1, path2) get_parent_path(path1) == get_parent_path(path2)
  toggle_selection <- function(current, new) if (new %in% current) setdiff(current, new) else c(current, new)
  is_contiguous <- function(paths) {
    if (length(paths) <= 1) return(TRUE)
    indices <- as.numeric(sapply(strsplit(paths, "-"), tail, 1))
    all(diff(sort(indices)) == 1)
  }
  
  observeEvent(input$span_btn, {
    req(input$spanner_label, selected_path())
    paths <- selected_path()
    parent_path <- get_parent_path(paths[1])
    indices <- as.numeric(sapply(strsplit(paths, "-"), tail, 1))
    current_header <- header_structure()
    accessor <- "current_header"
    if (parent_path != "") {
      path_parts <- strsplit(parent_path, "-")[[1]]
      for(part in path_parts) accessor <- paste0(accessor, "[[", part, "]]$sub")
    }
    target_list <- eval(parse(text = accessor))
    sub_list <- target_list[indices]
    spanner <- list(label = input$spanner_label, just = 'c', sub = sub_list)
    new_list <- list()
    min_idx <- min(indices)
    max_idx <- max(indices)
    if (min_idx > 1) new_list <- c(new_list, target_list[1:(min_idx-1)])
    new_list <- c(new_list, list(spanner))
    if (max_idx < length(target_list)) new_list <- c(new_list, target_list[(max_idx + 1):length(target_list)])
    eval(parse(text = paste(accessor, "<- new_list")))
    header_structure(current_header)
    selected_path(NULL)
    updateTextInput(session, "spanner_label", value = "")
  })
  
  output$edit_panel_ui <- renderUI({
    req(selected_path())
    if(length(selected_path()) != 1) {
      return(p("Select a single item to edit.", style="color: #888;"))
    }
    path <- selected_path()[1]
    
    # BUG FIX: Correctly build the accessor string for nested lists
    path_parts <- strsplit(path, "-")[[1]]
    accessor <- "header_structure()"
    accessor <- paste0(accessor, "[[", path_parts[1], "]]")
    if (length(path_parts) > 1) {
        for (part in path_parts[-1]) {
            accessor <- paste0(accessor, "$sub[[", part, "]]")
        }
    }
    
    selected_item <- eval(parse(text = accessor))
    
    tagList(
      textInput("edit_label", "Label", value = selected_item$label),
      radioButtons("edit_just", "Justification", choices = c("Left" = "l", "Center" = "c", "Right" = "r"), selected = selected_item$just, inline = TRUE),
      actionButton("update_item_btn", "Update Item", class = "btn-info btn-full-width"),
      br(),br(),
      actionButton("delete_item_btn", "Delete Item", class = "btn-warning btn-full-width")
    )
  })
  
  observeEvent(input$update_item_btn, {
    req(selected_path(), length(selected_path()) == 1)
    path <- selected_path()[1]
    current_header <- header_structure()
    
    # BUG FIX: Correctly build the accessor string for nested lists
    path_parts <- strsplit(path, "-")[[1]]
    accessor <- "current_header"
    accessor <- paste0(accessor, "[[", path_parts[1], "]]")
    if (length(path_parts) > 1) {
        for (part in path_parts[-1]) {
            accessor <- paste0(accessor, "$sub[[", part, "]]")
        }
    }
    
    eval(parse(text = paste0(accessor, "$label <- '", input$edit_label, "'")))
    eval(parse(text = paste0(accessor, "$just <- '", input$edit_just, "'")))
    header_structure(current_header)
  })

  observeEvent(input$delete_item_btn, {
    req(selected_path(), length(selected_path()) == 1)
    
    path <- selected_path()[1]
    current_header <- header_structure()
    
    parent_path <- get_parent_path(path)
    index_to_remove <- as.numeric(tail(strsplit(path, "-")[[1]], 1))
    
    if (parent_path == "") {
      # It's a top-level item
      current_header <- current_header[-index_to_remove]
    } else {
      # BUG FIX: Correctly build accessor to the PARENT's sublist to modify it
      path_parts_parent <- strsplit(parent_path, "-")[[1]]
      
      list_accessor <- "current_header"
      list_accessor <- paste0(list_accessor, "[[", path_parts_parent[1], "]]")
      if (length(path_parts_parent) > 1) {
          for (part in path_parts_parent[-1]) {
              list_accessor <- paste0(list_accessor, "$sub[[", part, "]]")
          }
      }
      list_accessor <- paste0(list_accessor, "$sub")
      
      current_list <- eval(parse(text = list_accessor))
      updated_list <- current_list[-index_to_remove]
      
      eval(parse(text = paste0(list_accessor, " <- updated_list")))
    }
    
    header_structure(current_header)
    selected_path(NULL)
  })

  observeEvent(input$reset_btn, {
    header_structure(list())
    selected_path(NULL)
  })
  
  # --- Code Output Generation ---
  
  # For the full 'specs' object
  output$header_code_output <- renderText({
    if (length(header_structure()) == 0) return("# Add columns to generate the specs object.")
    generate_r_code(header_structure())
  })
  
  # For the preview placeholder
  output$preview_code_placeholder <- renderText({
    if (length(header_structure()) == 0) return("# Your preview command will appear here.")
    
    # Construct the command string for the user's function
    specs_code <- generate_r_code(header_structure())
    paste0("create_header_from_specs(specs = ", specs_code, ")")
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
