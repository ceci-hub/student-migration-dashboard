# ==========================
# app.R — Student Migration Dashboard (2024)
# Three tabs: Map & Totals | Bar Charts | CSVs
# ==========================

# ---- Packages ----
pkgs <- c(
  "shiny","bslib","dplyr","tidyr","stringr","readxl","ggplot2",
  "plotly","scales","DT","tibble","tidyselect","readr","leaflet","maps","RColorBrewer"
)
inst <- setdiff(pkgs, rownames(installed.packages()))
if (length(inst)) install.packages(inst, dependencies = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))

# ============================================================
# 0) DATA (safe loading; never stop at global)
# ============================================================

# 0.1 Portable path: prefer ./data, fallback to J: drive
input_path_candidates <- c(
  "data/flows_long_justst.xlsx",
  "J:/research/Enrollment Research Team/Transition Plan - ceci/power BI/flows_long_justst.xlsx"
)
input_path <- {
  hits <- input_path_candidates[file.exists(input_path_candidates)]
  if (length(hits) >= 1) hits[1] else NA_character_
}
sheet_name <- "flows"

valid_states <- c(state.name, "District of Columbia")
territories  <- c("American Samoa","Federated States of Micronesia","Guam",
                  "Marshall Islands","Northern Marianas","Palau",
                  "Puerto Rico","Virgin Islands")

# 0.2 State -> Region mapping (used for colors and CSV mapping)
region_map <- tibble::tribble(
  ~state,                  ~Region,
  "Alaska","Far West","California","Far West","Hawaii","Far West",
  "Nevada","Far West","Oregon","Far West","Washington","Far West",
  "Illinois","Great Lakes","Indiana","Great Lakes","Michigan","Great Lakes",
  "Ohio","Great Lakes","Wisconsin","Great Lakes",
  "New Jersey","Middle Atlantic","New York","Middle Atlantic","Pennsylvania","Middle Atlantic",
  "Connecticut","New England","Maine","New England","Massachusetts","New England",
  "New Hampshire","New England","Rhode Island","New England","Vermont","New England",
  "Iowa","Plains","Kansas","Plains","Minnesota","Plains",
  "Missouri","Plains","Nebraska","Plains","North Dakota","Plains","South Dakota","Plains",
  "Colorado","Rocky Mountain","Idaho","Rocky Mountain","Montana","Rocky Mountain",
  "Utah","Rocky Mountain","Wyoming","Rocky Mountain",
  "Arkansas","South Central","Louisiana","South Central","Oklahoma","South Central","Texas","South Central",
  "Alabama","Southeast","District of Columbia","Southeast","Delaware","Southeast",
  "Florida","Southeast","Georgia","Southeast","Kentucky","Southeast","Maryland","Southeast",
  "Mississippi","Southeast","North Carolina","Southeast","South Carolina","Southeast",
  "Tennessee","Southeast","Virginia","Southeast","West Virginia","Southeast",
  "Arizona","Southwest","New Mexico","Southwest"
)

# 0.3 State centroids (for the bubble map)
state_centers <- tibble::tibble(
  To_State = c(state.name, "District of Columbia"),
  lat = c(as.numeric(state.center$y), 38.9072),
  lon = c(as.numeric(state.center$x), -77.0369)
)

# 0.4 Safe Excel load (no stop in global)
load_flows_long <- function(path, sheet) {
  tryCatch({
    if (is.na(path)) stop("File not found")
    raw <- readxl::read_excel(path, sheet = sheet, na = c("N/A","NA",""))
    names(raw) <- names(raw) |>
      stringr::str_trim() |>
      stringr::str_replace_all("\\s+"," ")
    if (!"From_State" %in% names(raw)) names(raw)[1] <- "From_State"
    
    is_long <- all(c("From_State","To_State","Count") %in% names(raw))
    
    if (is_long) {
      df <- raw |>
        dplyr::transmute(
          From_State = stringr::str_trim(From_State),
          To_State   = stringr::str_trim(To_State),
          Count      = as.numeric(stringr::str_replace_all(as.character(Count), ",", ""))
        )
    } else {
      dest_cols <- setdiff(names(raw), c("From_State","Total","Instate","Outstate","per_Instate","per_Outstate"))
      dest_cols <- intersect(dest_cols, valid_states)
      
      df <- raw |>
        tidyr::pivot_longer(cols = tidyselect::all_of(dest_cols),
                            names_to = "To_State", values_to = "Count") |>
        dplyr::mutate(
          From_State = stringr::str_trim(From_State),
          To_State   = stringr::str_trim(To_State),
          Count      = as.numeric(stringr::str_replace_all(as.character(Count), ",", ""))
        )
    }
    
    df |>
      dplyr::filter(!From_State %in% c("Total","State unknown",
                                       "Residence not reported (balance line)","Foreign countries",
                                       territories))
  }, error = function(e) {
    message("Could not read Excel: ", conditionMessage(e))
    NULL
  })
}

flows_long <- load_flows_long(input_path, sheet_name)

# 0.5 Enrich (if data exists), else keep empty tibble but let app start
if (is.null(flows_long)) {
  flows_long <- tibble::tibble(From_State = character(), To_State = character(), Count = double())
}
if (nrow(flows_long) > 0) {
  flows_long <- flows_long |>
    dplyr::left_join(region_map, by = c("To_State"="state")) |> dplyr::rename(DestRegion = Region) |>
    dplyr::left_join(region_map, by = c("From_State"="state")) |> dplyr::rename(OriginRegion = Region) |>
    dplyr::mutate(
      InState     = From_State == To_State,
      InRegion    = !InState & !is.na(DestRegion) & DestRegion == OriginRegion,
      OutOfRegion = !InState & !is.na(DestRegion) & DestRegion != OriginRegion
    )
} else {
  flows_long <- flows_long |>
    dplyr::mutate(InState = logical(), InRegion = logical(), OutOfRegion = logical(),
                  DestRegion = character(), OriginRegion = character())
}

# 0.6 Per-origin summary (may be empty)
in_out <- flows_long |>
  dplyr::group_by(From_State) |>
  dplyr::summarise(
    total         = sum(Count, na.rm = TRUE),
    in_state      = sum(Count[InState], na.rm = TRUE),
    out_of_state  = total - in_state,
    pct_out_state = dplyr::if_else(total > 0, out_of_state / total, NA_real_),
    .groups = "drop"
  )

# ============================================================
# UI — 3 tabs
#   1) Selected state — Map & totals
#   2) Bar charts
#   3) CSVs (downloads + mapping explanation)
# ============================================================
ui <- bslib::page_navbar(
  theme = bslib::bs_theme(
    bootswatch = "flatly",
    base_font = bslib::font_google("Inter"),
    heading_font = bslib::font_google("Inter")
  ),
  title = "Student Migration (2024)",
  
  header = tags$head(
    tags$style(HTML("
      .container-fluid { max-width: 1400px; }
      .form-select, .form-control { height: 36px; }
      .bslib-value-box .value { font-size: 1.05rem; white-space: nowrap; }
      .dataTables_wrapper .dataTables_filter input { max-width: 220px; }
    "))
  ),
  
  # ---------------- Tab 1: Selected state — Map & totals ----------------
  bslib::nav_panel(
    "Selected state — Map & totals",
    
    # Filters
    bslib::layout_column_wrap(
      width = "100%", heights_equal = "row",
      bslib::card(
        class = "mb-3",
        bslib::layout_column_wrap(
          width = 320, heights_equal = "row",
          selectInput(
            "origin", "Origin state:",
            choices = if (nrow(flows_long) > 0) sort(unique(flows_long$From_State)) else character(),
            selected = if (nrow(flows_long) > 0) {
              if ("Colorado" %in% flows_long$From_State) "Colorado" else sort(unique(flows_long$From_State))[1]
            } else NULL
          ),
          sliderInput("topn", "Top N destinations (out-of-state)", min = 3, max = 20, value = 10, step = 1),
          checkboxInput("labels", "Show labels on bars", TRUE)
        )
      )
    ),
    
    # KPIs (placed together with the map per your request)
    bslib::layout_column_wrap(
      width = 380, heights_equal = "row",
      bslib::value_box("Total (origin)", textOutput("vb_total_origin")),
      bslib::value_box("Out-of-state (origin)", textOutput("vb_ooo_origin"))
    ),
    
    # Map
    bslib::layout_column_wrap(
      width = "100%",
      bslib::card(
        full_screen = TRUE, height = 720,
        bslib::card_header("Geographic map — destinations (out-of-state only)"),
        bslib::card_body_fill(
          leafletOutput("map_states", height = "100%")
        )
      )
    )
  ),
  
  # ---------------- Tab 2: Bar charts only ----------------
  bslib::nav_panel(
    "Bar charts",
    
    bslib::layout_column_wrap(
      width = 680, heights_equal = "row",
      bslib::card(
        full_screen = TRUE, height = 700,
        bslib::card_header("Top destinations (out-of-state only)"),
        bslib::card_body_fill(
          plotlyOutput("dest_rank_plot", height = "100%")
        )
      ),
      bslib::card(
        full_screen = TRUE, height = 700,
        bslib::card_header("Destinations by region (out-of-state only)"),
        bslib::card_body_fill(
          plotlyOutput("region_summary_plot", height = "100%")
        )
      )
    )
  ),
  
  # ---------------- Tab 3: CSVs & mapping explanation ----------------
  bslib::nav_panel(
    "CSVs",
    
    bslib::layout_column_wrap(
      width = 560,
      
      bslib::card(
        header = "Download — Selected origin’s destinations (out-of-state)",
        bslib::card_body(
          div(style="color:#5f6b7a;margin-bottom:8px;",
              HTML("CSV includes, for the selected origin state: <b>Destination state</b>, <b>Region</b>, <b>High school students who enrolled in a college in 2024</b>, and <b>% share of origin’s out-of-state total</b>.")),
          downloadButton("dl_origin_destinations", "Download CSV — Selected origin", class = "btn-primary")
        )
      ),
      
      bslib::card(
        header = "Download — KPIs for all states",
        bslib::card_body(
          div(style="color:#5f6b7a;margin-bottom:8px;",
              HTML("CSV includes, for each origin state: <b>Total</b>, <b>In-state</b>, <b>Out-of-state</b>, and <b>% Out-of-state</b> (share of total).")),
          downloadButton("dl_kpis_allstates", "Download CSV — All states KPIs", class = "btn-secondary")
        )
      ),
      
      bslib::card(
        header = "State ↔ Region mapping (view, filter by region, and download)",
        bslib::card_body(
          div(style="color:#5f6b7a;margin-bottom:8px;",
              HTML("This mapping assigns each U.S. state (plus District of Columbia) to a macro-region we use for charts and the map legend. 
                    Use the region filter to preview, and click below to download the full mapping as CSV.")),
          selectInput("region_filter", "Filter by region:", choices = c("All", sort(unique(region_map$Region))), selected = "All"),
          DTOutput("tbl_region_map"),
          tags$div(style="height:8px;"),
          downloadButton("dl_region_map", "Download CSV — State/Region mapping")
        )
      ),
      
      bslib::card(
        header = "What’s in each CSV and where to use it",
        bslib::card_body(
          tags$ul(
            tags$li(HTML("<b>Selected origin’s destinations (out-of-state)</b>: Use it to analyze which out-of-state destinations are most relevant for your chosen origin. The <i>% share</i> is computed over the origin’s out-of-state total.")),
            tags$li(HTML("<b>All states KPIs</b>: Use it to benchmark origins: total high school students who enrolled in a college in 2024, how many stayed in-state, how many went out-of-state, and the out-of-state share.")),
            tags$li(HTML("<b>State/Region mapping</b>: Use it when grouping destinations by macro-region, or to align colors in visuals and the map legend."))
          )
        )
      )
    )
  )
)

# ============================================================
# SERVER
# ============================================================
server <- function(input, output, session) {
  
  # If no data, inform and avoid hard failures
  observe({
    if (nrow(flows_long) == 0) {
      showModal(modalDialog(
        title = "Data file not found",
        "Please ensure 'data/flows_long_justst.xlsx' exists (exact name) and includes a sheet named 'flows'.",
        easyClose = TRUE, footer = NULL
      ))
    }
  })
  
  # Subset by origin
  origin_df <- reactive({
    validate(need(nrow(flows_long) > 0, "No data to display"))
    validate(need(!is.null(input$origin) && nzchar(input$origin), "Pick an origin state"))
    flows_long |> dplyr::filter(From_State == input$origin)
  })
  
  # KPIs
  origin_totals <- reactive({
    df <- origin_df()
    total <- sum(df$Count, na.rm = TRUE)
    ooo   <- sum(df$Count[!df$InState], na.rm = TRUE)
    list(total = total, out_of_state = ooo)
  })
  
  output$vb_total_origin <- renderText({
    validate(need(nrow(flows_long) > 0, ""))
    scales::comma(origin_totals()$total)
  })
  output$vb_ooo_origin <- renderText({
    validate(need(nrow(flows_long) > 0, ""))
    paste0(
      scales::comma(origin_totals()$out_of_state),
      " (",
      ifelse(origin_totals()$total > 0,
             scales::percent(origin_totals()$out_of_state / origin_totals()$total, accuracy = 0.1),
             "—"),
      ")"
    )
  })
  
  # Out-of-state destinations for selected origin (DESC)
  origin_out_df <- reactive({
    df <- origin_df() |>
      dplyr::filter(!InState) |>
      dplyr::group_by(To_State, DestRegion) |>
      dplyr::summarise(Count = sum(Count, na.rm = TRUE), .groups = "drop") |>
      dplyr::arrange(dplyr::desc(Count))
    
    tot_ooo <- sum(df$Count, na.rm = TRUE)
    if (tot_ooo == 0) {
      df$ShareOOO <- NA_real_
    } else {
      df <- df |> dplyr::mutate(ShareOOO = Count / tot_ooo)
    }
    df
  })
  
  # ---------------- Bar chart: Top N out-of-state destinations ----------------
  output$dest_rank_plot <- renderPlotly({
    df <- origin_out_df()
    validate(need(nrow(df) > 0, "No out-of-state destinations for this origin"))
    
    df <- df |>
      dplyr::slice_head(n = input$topn) |>
      dplyr::mutate(
        To_State = factor(To_State, levels = rev(To_State)),
        label_txt = paste0(
          scales::comma(Count), " (",
          ifelse(is.na(ShareOOO), "—", scales::percent(ShareOOO, accuracy = 0.1)),
          ")"
        )
      )
    
    p <- ggplot(
      df,
      aes(x = Count, y = To_State, fill = DestRegion,
          text = paste0(
            "<b>", To_State, "</b>",
            "<br>High school students who enrolled in a college in 2024: ", scales::comma(Count),
            "<br>Share of origin’s out-of-state: ",
            ifelse(is.na(ShareOOO), "—", scales::percent(ShareOOO, accuracy = 0.1)),
            "<br>Region: ", dplyr::coalesce(DestRegion, "Unknown")
          ))
    ) +
      geom_col(width = 0.7, show.legend = TRUE) +
      labs(
        x = "High school students who enrolled in a college in 2024",
        y = NULL, fill = "Region",
        title = paste0("Out-of-state destinations — ", input$origin, " (Top ", input$topn, ")")
      ) +
      scale_x_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.08))) +
      theme_minimal(base_size = 12) +
      theme(plot.margin = margin(10, 25, 10, 10))
    
    if (isTRUE(input$labels)) {
      p <- p + geom_text(aes(label = label_txt), hjust = 1.02, color = "white", size = 3.2)
    }
    
    plotly::ggplotly(p, tooltip = "text") |>
      layout(margin = list(l = 140, r = 30, t = 60, b = 40))
  })
  
  # ---------------- Bar chart: Out-of-state destinations by region ----------------
  output$region_summary_plot <- renderPlotly({
    df <- origin_df() |>
      dplyr::filter(!InState) |>
      dplyr::mutate(DestRegion = dplyr::coalesce(DestRegion, "Unknown")) |>
      dplyr::group_by(DestRegion) |>
      dplyr::summarise(Count = sum(Count, na.rm = TRUE), .groups = "drop") |>
      dplyr::arrange(dplyr::desc(Count))
    
    validate(need(nrow(df) > 0, "No out-of-state destinations by region for this origin"))
    
    tot <- sum(df$Count, na.rm = TRUE)
    df <- df |>
      dplyr::mutate(
        Share = ifelse(tot > 0, Count / tot, NA_real_),
        DestRegion = factor(DestRegion, levels = rev(DestRegion)),
        label_txt = paste0(
          scales::comma(Count), " (",
          ifelse(is.na(Share), "—", scales::percent(Share, accuracy = 0.1)),
          ")"
        )
      )
    
    p <- ggplot(
      df,
      aes(x = Count, y = DestRegion, fill = DestRegion,
          text = paste0(
            "<b>", DestRegion, "</b>",
            "<br>High school students who enrolled in a college in 2024: ", scales::comma(Count),
            "<br>Share of origin’s out-of-state: ",
            ifelse(is.na(Share), "—", scales::percent(Share, accuracy = 0.1))
          ))
    ) +
      geom_col(width = 0.7, show.legend = FALSE) +
      labs(
        x = "High school students who enrolled in a college in 2024",
        y = NULL
      ) +
      scale_x_continuous(labels = scales::comma, expand = expansion(mult = c(0, 0.08))) +
      theme_minimal(base_size = 12) +
      theme(plot.margin = margin(10, 25, 10, 10))
    
    if (isTRUE(input$labels)) {
      p <- p + geom_text(aes(label = label_txt), hjust = 1.02, color = "white", size = 3.0)
    }
    
    plotly::ggplotly(p, tooltip = "text") |>
      layout(margin = list(l = 140, r = 30, t = 30, b = 30))
  })
  
  # ---------------- Map: initialize once ----------------
  output$map_states <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = TRUE)) |>
      addProviderTiles("CartoDB.Positron") |>
      setView(lng = -96, lat = 39, zoom = 4)
  })
  
  # ---------------- Map: update bubbles via leafletProxy ----------------
  observeEvent(list(input$origin, input$topn), {
    req(nrow(flows_long) > 0)
    
    df <- origin_out_df() |>
      dplyr::slice_head(n = input$topn) |>
      dplyr::mutate(DestRegion = dplyr::coalesce(DestRegion, "Unknown")) |>
      dplyr::inner_join(state_centers, by = "To_State")
    
    proxy <- leafletProxy("map_states")
    proxy |> clearMarkers() |> clearControls()
    
    if (nrow(df) == 0) {
      showNotification("No out-of-state destinations (or missing coordinates for destinations).", type = "message", duration = 4)
      return(invisible(NULL))
    }
    
    pal <- colorFactor(RColorBrewer::brewer.pal(8, "Set1"), domain = unique(df$DestRegion))
    
    proxy |>
      addCircleMarkers(
        lng = df$lon, lat = df$lat,
        radius = pmax(6, sqrt(df$Count)),
        color = pal(df$DestRegion),
        fillColor = pal(df$DestRegion),
        fillOpacity = 0.85, stroke = TRUE, weight = 1,
        label = sprintf("%s — %s", df$To_State, scales::comma(df$Count)),
        popup = paste0(
          "<b>", df$To_State, "</b>",
          "<br/>High school students who enrolled in a college in 2024: ", scales::comma(df$Count),
          "<br/>Region: ", df$DestRegion
        )
      ) |>
      addLegend("bottomright", pal = pal, values = df$DestRegion, title = "Region")
  }, ignoreInit = TRUE)
  
  # ---------------- All-states KPIs table (2nd tab) ----------------
  kpi_df <- reactive({
    in_out |>
      dplyr::transmute(
        State = From_State,
        Total = total,
        `In-state` = in_state,
        `Out-of-state` = out_of_state,
        `% Out-of-state` = pct_out_state
      )
  })
  
  output$kpi_allstates_table <- renderDT({
    df <- kpi_df()
    # default order: Out-of-state (desc)
    datatable(
      df,
      rownames = FALSE,
      options = list(
        pageLength = 15,
        order = list(list(3, "desc")),
        columnDefs = list(
          list(targets = 1, render = DT::JS("function(d){return d.toLocaleString()}")), # Total
          list(targets = 2, render = DT::JS("function(d){return d.toLocaleString()}")), # In-state
          list(targets = 3, render = DT::JS("function(d){return d.toLocaleString()}")), # Out-of-state
          list(targets = 4, render = DT::JS("function(d){return (d==null)?'':(100*d).toFixed(1)+'%'}")) # %
        )
      ),
      filter = "top"
    )
  })
  
  # ---------------- CSVs (3rd tab) ----------------
  # 1) Selected origin — destinations (out-of-state)
  output$dl_origin_destinations <- downloadHandler(
    filename = function() {
      origin <- if (!is.null(input$origin) && nzchar(input$origin)) input$origin else "no_origin"
      paste0("out_of_state_destinations_", gsub("\\s+","_", tolower(origin)), ".csv")
    },
    content = function(file) {
      if (nrow(flows_long) == 0 || is.null(input$origin) || !nzchar(input$origin)) {
        readr::write_csv(
          tibble::tibble(
            Origin = character(), Destination = character(), Region = character(),
            HighSchoolStudents2024 = double(), ShareOfOriginOutOfState = double()
          ),
          file
        )
      } else {
        df <- origin_out_df() |>
          dplyr::mutate(
            Origin = input$origin,
            ShareOfOriginOutOfState = ShareOOO
          ) |>
          dplyr::transmute(
            Origin,
            Destination = To_State,
            Region = dplyr::coalesce(DestRegion, "Unknown"),
            HighSchoolStudents2024 = Count,
            ShareOfOriginOutOfState
          )
        readr::write_csv(df, file)
      }
    }
  )
  
  # 2) All states KPIs
  output$dl_kpis_allstates <- downloadHandler(
    filename = function() "kpis_all_states.csv",
    content = function(file) {
      if (nrow(in_out) == 0) {
        readr::write_csv(
          tibble::tibble(
            State=character(), Total=double(), `In-state`=double(),
            `Out-of-state`=double(), PctOutOfState=double()
          ),
          file
        )
      } else {
        df <- in_out |>
          dplyr::transmute(
            State = From_State,
            Total = total,
            `In-state` = in_state,
            `Out-of-state` = out_of_state,
            PctOutOfState = pct_out_state
          )
        readr::write_csv(df, file)
      }
    }
  )
  
  # 3) State/Region mapping — view & download
  output$tbl_region_map <- renderDT({
    df <- region_map
    if (!is.null(input$region_filter) && input$region_filter != "All") {
      df <- df |> dplyr::filter(Region == input$region_filter)
    }
    df |>
      dplyr::arrange(Region, state) |>
      dplyr::rename(State = state) |>
      datatable(rownames = FALSE, filter = "top", options = list(pageLength = 12))
  })
  
  output$dl_region_map <- downloadHandler(
    filename = function() "state_region_mapping.csv",
    content = function(file) {
      readr::write_csv(region_map |> dplyr::arrange(Region, state), file)
    }
  )
}

shinyApp(ui, server)