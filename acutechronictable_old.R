
# acute_chronic_table <- reactive({
#   
#   
#   chronic <- stats %>%
#     filter(athlete_name %in% athletes_catapult$athlete_name & position_name != "Goal Keeper") %>% 
#     select(athlete_name | date | total_distance | high_speed_distance | sprint_distance | accel_decel_efforts) %>% 
#     filter(date >= (floor_date(input$date_range1[2], unit = "week", week_start = 1)-weeks(3)) & date < floor_date(input$date_range1[2], unit = "week", week_start = 1))
#   group_by(athlete_name) %>% 
#     summarize(across(where(is.numeric), sum))
#   
#   
#   thresholds <-  chronic %>% 
#     mutate(across(!athlete_name,~ (0.7*.x)/3.3, .names ="{.col}_lower"),
#            across(!athlete_name & !contains("_lower"),~ (1.3*.x)/2.7, .names="{.col}_upper"))
#   
#   daily <- stats %>%
#     filter(position_name != "Goal Keeper") %>% 
#     select(athlete_name | date | total_distance | high_speed_distance | sprint_distance | accel_decel_efforts) %>% 
#     filter(date >= floor_date(input$date_range1[2], unit = "week", week_start = 1) & date <= input$date_range1[2])
#   
#   acute <- daily %>% 
#     group_by(athlete_name) %>% 
#     summarize(across(where(is.numeric), sum))
#   
#   date_seq <- seq(from = floor_date(input$date_range1[2], unit = "week", week_start = 1), by = "day", length.out=7)
#   
#   metrics <- data.frame(athlete_name = athletes_catapult$athlete_name, 
#                         total_distance =0,
#                         high_speed_distance = 0, 
#                         sprint_distance = 0,
#                         accel_decel_efforts = 0)
#   
#   
#   date_grid <- data.frame(athlete_name = athletes_catapult$athlete_name) %>%
#     group_by(athlete_name) %>%
#     reframe(date = date_seq) %>%
#     left_join(metrics, by=join_by(athlete_name)) %>%
#     mutate(name_date = paste0(athlete_name, date)) %>%
#     filter(!(name_date %in% paste0(daily$athlete_name,daily$date))) %>%
#     select(!name_date)
#   
#   weekly_details <- daily %>%
#     full_join(date_grid) %>%
#     arrange(athlete_name, date) %>%
#     mutate(date=format(date, format = "%a, %b %d")) %>%
#     rename(Player=athlete_name) %>% 
#     rename_with(~str_to_title(str_replace_all(.x,"_", " "))) %>% 
#     rename_with(~paste(.x,  "(m)"), .cols=contains("Distance")) %>% 
#     mutate(across(where(is.numeric), round))
#   
#   load_plan_table <- acute %>%
#     full_join(thresholds %>% select(athlete_name | contains("_lower") | contains("_upper")), by=join_by(athlete_name)) %>% 
#     mutate(total_distance_remaining = total_distance_upper - total_distance,
#            high_speed_distance_remaining = high_speed_distance_upper - high_speed_distance,
#            sprint_distance_remaining = sprint_distance_upper - sprint_distance,
#            accel_decel_efforts_remaining = accel_decel_efforts_upper - accel_decel_efforts) %>%
#     relocate(contains("total_distance"), contains("high_speed_distance"), contains("sprint_distance"),contains("accel_decel_efforts"), .after=athlete_name) %>% 
#     rename(Player=athlete_name) %>% 
#     arrange(Player) %>% 
#     rename_with(~str_to_title(str_replace_all(.x,"_", " "))) %>% 
#     rename_with(~paste(.x,  "(m)"), .cols=contains("Distance"))  %>% 
#     mutate(across(where(is.numeric), round))
#   
#   
#   reactable(
#     load_plan_table,
#     striped = F,
#     outline=F,
#     bordered = T,
#     compact = T,
#     highlight = F,
#     defaultPageSize =nrow(load_plan_table),
#     details = function(index) {
#       weekly_info <- weekly_details[weekly_details$Player == load_plan_table$Player[index], ]
#       htmltools::div(style = "padding: 1rem",
#                      reactable(weekly_info %>% select(!Player), 
#                                striped = F,
#                                outline=F,
#                                bordered = T,
#                                compact = T,
#                                highlight = F, 
#                                fullWidth = F)
#       )
#     })
#   
# })


# 
# # 1. Initialize a reactive tracking value for user modifications
# editable_daily <- reactiveVal(NULL)
# 
# # 2. Re-populate the 7-day baseline grid when the date picker changes
# observe({
#   req(input$date_range1)
#   
#   # Pull base stats records for the selected week
#   daily_base <- stats %>%
#     filter(athlete_name %in% athletes_catapult$athlete_name & position_name != "Goal Keeper") %>%
#     select(athlete_name | date | total_distance | high_speed_distance | sprint_distance | accel_decel_efforts) %>%
#     filter(date >= floor_date(input$date_range1[2], unit = "week", week_start = 1) & date <= input$date_range1[2])
#   
#   # Generate explicit filler slots for dates missing logs this week
#   dates_seq <- seq(from = floor_date(input$date_range1[2], unit = "week", week_start = 1), by = "day", length.out = 7)
#   
#   metrics_placeholder <- data.frame(
#     athlete_name = athletes_catapult %>% filter(position_name != "Goal Keeper") %>% pull(athlete_name),
#     total_distance = 0, high_speed_distance = 0, sprint_distance = 0, accel_decel_efforts = 0
#   )
#   
#   dates_grid <- data.frame(athlete_name = athletes_catapult %>% filter(position_name != "Goal Keeper") %>% pull(athlete_name)) %>%
#     group_by(athlete_name) %>%
#     reframe(date = dates_seq) %>%
#     left_join(metrics_placeholder, by = join_by(athlete_name)) %>%
#     mutate(name_date = paste0(athlete_name, date)) %>%
#     filter(!(name_date %in% paste0(daily_base$athlete_name, daily_base$date))) %>%
#     select(!name_date)
#   
#   # Merge active logs with the empty tracking row matrix slots
#   combined_daily <- daily_base %>%
#     full_join(dates_grid, by = c("athlete_name", "date", "total_distance", "high_speed_distance", "sprint_distance", "accel_decel_efforts")) %>%
#     arrange(athlete_name, date) 
#   
#   editable_daily(combined_daily)
# }) %>% bindEvent(input$date_range1)
# 
# # 3. MANUAL TRIGGER: Scrape cell changes and compute data changes ONLY when the button is clicked
# observe({
#   df <- editable_daily()
#   req(df, input$date_range1)
#   changed <- FALSE
#   metrics_cols <- c("total_distance", "high_speed_distance", "sprint_distance", "accel_decel_efforts")
#   
#   for (i in 1:nrow(df)) {
#     p_id <- stringr::str_replace_all(df$athlete_name[i], " ", "_")
#     d_id <- df$date[i]
#     
#     for (col in metrics_cols) {
#       input_id <- paste("inp", col, p_id, d_id, sep = "__")
#       val <- input[[input_id]]
#       
#       # Scrape the user entries into our memory matrix
#       if (!is.null(val) && !is.na(val) && val != df[i, col]) {
#         df[i, col] <- val
#         changed <- TRUE
#       }
#     }
#   }
#   
#   if (changed) {
#     editable_daily(df)
#     
#     # Inline generation of updated plan data to avoid helper calls
#     chronic <- stats %>%
#       filter(athlete_name %in% athletes_catapult$athlete_name & position_name != "Goal Keeper") %>%
#       select(athlete_name | date | total_distance | high_speed_distance | sprint_distance | accel_decel_efforts) %>%
#       filter(date >= (floor_date(input$date_range1[2], unit = "week", week_start = 1) - weeks(3)) & date < floor_date(input$date_range1[2], unit = "week", week_start = 1)) %>%
#       group_by(athlete_name) %>%
#       summarize(across(where(is.numeric), sum))
#     
#     thresholds <- chronic %>%
#       mutate(across(!athlete_name, ~ (0.7 * .x) / 3.3, .names = "{.col}_lower"),
#              across(!athlete_name & !contains("_lower"), ~ (1.3 * .x) / 2.7, .names = "{.col}_upper"))
#     
#     acute <- df %>%
#       group_by(athlete_name) %>%
#       summarize(across(where(is.numeric), sum))
#     
#     updated_plan_table <- acute %>%
#       full_join(thresholds %>% select(athlete_name | contains("_lower") | contains("_upper")), by = join_by(athlete_name)) %>%
#       mutate(total_distance_remaining = total_distance_upper - total_distance,
#              high_speed_distance_remaining = high_speed_distance_upper - high_speed_distance,
#              sprint_distance_remaining = sprint_distance_upper - sprint_distance,
#              accel_decel_efforts_remaining = accel_decel_efforts_upper - accel_decel_efforts) %>%
#       relocate(contains("total_distance"), contains("high_speed_distance"), contains("sprint_distance"), contains("accel_decel_efforts"), .after = athlete_name) %>%
#       rename(Player = athlete_name) %>%
#       arrange(Player) %>%
#       rename_with(~ str_to_title(str_replace_all(.x, "_", " "))) %>%
#       rename_with(~ paste(.x, "(m)"), .cols = contains("Distance"))  %>%
#       rename_with(~ str_replace(.x, "High Speed", "HSR"))  %>%
#       rename_with(~ str_replace(.x, "Accel Decel", "Accel + Decel"))  %>%
#       mutate(across(where(is.numeric), round))
#     
#     # Push data changes cleanly without resetting row visibility
#     updateReactable("AcuteChronicTable", data = updated_plan_table)
#   }
# }) %>% bindEvent(input$update_table_btn) # Assumes an actionButton named 'update_table_btn' in your UI
# 
# 
# observe({
#   req(input$date_range1)
#   
#   # Re-extract the unedited baseline logs exactly like step 2
#   daily_base <- stats %>%
#     filter(athlete_name %in% athletes_catapult$athlete_name & position_name != "Goal Keeper") %>%
#     select(athlete_name | date | total_distance | high_speed_distance | sprint_distance | accel_decel_efforts) %>%
#     filter(date >= floor_date(input$date_range1[2], unit = "week", week_start = 1) & date <= input$date_range1[2])
#   
#   dates_seq <- seq(from = floor_date(input$date_range1[2], unit = "week", week_start = 1), by = "day", length.out = 7)
#   
#   metrics_placeholder <- data.frame(
#     athlete_name = athletes_catapult %>% filter(position_name != "Goal Keeper") %>% pull(athlete_name),
#     total_distance = 0, high_speed_distance = 0, sprint_distance = 0, accel_decel_efforts = 0
#   )
#   
#   dates_grid <- data.frame(athlete_name = athletes_catapult %>% filter(position_name != "Goal Keeper") %>% pull(athlete_name)) %>%
#     group_by(athlete_name) %>%
#     reframe(date = dates_seq) %>%
#     left_join(metrics_placeholder, by = join_by(athlete_name)) %>%
#     mutate(name_date = paste0(athlete_name, date)) %>%
#     filter(!(name_date %in% paste0(daily_base$athlete_name, daily_base$date))) %>%
#     select(!name_date)
#   
#   combined_daily <- daily_base %>%
#     full_join(dates_grid, by = c("athlete_name", "date", "total_distance", "high_speed_distance", "sprint_distance", "accel_decel_efforts")) %>%
#     arrange(athlete_name, date)
#   
#   # Reset the reactive memory to original values
#   editable_daily(combined_daily)
#   
#   # Crucial: Since sub-table input text fields hold onto browser states, 
#   # we must trigger a structural UI reload by calling shinyjs::refresh() or updating the output slot
#   shinyjs::runjs("Shiny.setInputValue('acute_chronic_table_state', Math.random());") 
#   
# }) %>% bindEvent(input$clear_table_btn)
# 
# # 4. Table Structure Definition Output (Matches your layout preferences)
# output$AcuteChronicTable <- renderReactable({
#   daily_current <- editable_daily()
#   req(daily_current, input$date_range1)
#   
#   # Listens to our random trigger above to force sub-table input redraws when cleared
#   input$acute_chronic_table_state 
#   
#   # Inline generation of layout datasets to avoid helper calls
#   chronic <- stats %>%
#     filter(athlete_name %in% athletes_catapult$athlete_name & position_name != "Goal Keeper") %>%
#     select(athlete_name | date | total_distance | high_speed_distance | sprint_distance | accel_decel_efforts) %>%
#     filter(date >= (floor_date(input$date_range1[2], unit = "week", week_start = 1) - weeks(3)) & date < floor_date(input$date_range1[2], unit = "week", week_start = 1)) %>%
#     group_by(athlete_name) %>%
#     summarize(across(where(is.numeric), sum))
#   
#   thresholds <- chronic %>%
#     mutate(across(!athlete_name, ~ (0.7 * .x) / 3.3, .names = "{.col}_lower"),
#            across(!athlete_name & !contains("_lower"), ~ (1.3 * .x) / 2.7, .names = "{.col}_upper"))
#   
#   acute <- daily_current %>%
#     group_by(athlete_name) %>%
#     summarize(across(where(is.numeric), sum))
#   
#   load_plan_table <- acute %>%
#     full_join(thresholds %>% select(athlete_name | contains("_lower") | contains("_upper")), by = join_by(athlete_name)) %>%
#     mutate(total_distance_remaining = total_distance_upper - total_distance,
#            high_speed_distance_remaining = high_speed_distance_upper - high_speed_distance,
#            sprint_distance_remaining = sprint_distance_upper - sprint_distance,
#            accel_decel_efforts_remaining = accel_decel_efforts_upper - accel_decel_efforts) %>%
#     relocate(contains("total_distance"), contains("high_speed_distance"), contains("sprint_distance"), contains("accel_decel_efforts"), .after = athlete_name) %>%
#     rename(Player = athlete_name) %>%
#     arrange(Player) %>%
#     rename_with(~ str_to_title(str_replace_all(.x, "_", " "))) %>%
#     rename_with(~ paste(.x, "(m)"), .cols = contains("Distance"))  %>%
#     rename_with(~ str_replace(.x, "High Speed", "HSR"))  %>%
#     rename_with(~ str_replace(.x, "Accel Decel", "Accel + Decel"))  %>%
#     mutate(across(where(is.numeric), round))
#   
#   weekly_details <- daily_current %>%
#     arrange(athlete_name, date) %>%
#     mutate(formatted_date = format(date, format = "%a, %b %d")) %>%
#     rename(Player = athlete_name) %>% 
#     relocate(formatted_date,.after=Player)
#   
#   reactable(
#     load_plan_table,
#     striped = F, outline = F, bordered = T, compact = T, highlight = F,
#     defaultPageSize = nrow(load_plan_table),
#     onClick = "expand",    
#     defaultColDef = colDef(align = "center"), 
#     columns = list(
#       Player = colDef(align = "left",
#                       sticky = "left", # Locks column during horizontal scrolling
#                       style = list(backgroundColor = "#fff", zIndex = 1)
#       )
#     ),
#     details = function(index) {
#       player_name <- load_plan_table$Player[index]
#       weekly_info <- weekly_details %>% filter(Player == player_name)
#       
#       htmltools::div(
#         style = "padding: 1rem; background-color: #fcfcfc;",
#         reactable(
#           weekly_info %>% select(!c(Player, date)),
#           striped = F, outline = F, bordered = T, compact = T, highlight = F, fullWidth = F,
#           defaultColDef = colDef(align = "center"), 
#           columns = list(
#             formatted_date = colDef(name = "Date", align="left",sortable = FALSE),
#             total_distance = colDef(name = "Total Distance (m)", cell = function(val, r_idx) {
#               p_clean <- stringr::str_replace_all(player_name, " ", "_")
#               as.character(numericInput(paste("inp", "total_distance", p_clean, weekly_info$date[r_idx], sep = "__"), NULL, value = round(val),min = 0,  width = "100px"))
#             }, html = TRUE),
#             high_speed_distance = colDef(name = "HSR Distance (m)", cell = function(val, r_idx) {
#               p_clean <- stringr::str_replace_all(player_name, " ", "_")
#               as.character(numericInput(paste("inp", "high_speed_distance", p_clean, weekly_info$date[r_idx], sep = "__"), NULL, value = round(val), min = 0, width = "100px"))
#             }, html = TRUE),
#             sprint_distance = colDef(name = "Sprint Distance (m)", cell = function(val, r_idx) {
#               p_clean <- stringr::str_replace_all(player_name, " ", "_")
#               as.character(numericInput(paste("inp", "sprint_distance", p_clean, weekly_info$date[r_idx], sep = "__"), NULL, value = round(val), min = 0, width = "100px"))
#             }, html = TRUE),
#             accel_decel_efforts = colDef(name = "Accel + Decel Efforts", cell = function(val, r_idx) {
#               p_clean <- stringr::str_replace_all(player_name, " ", "_")
#               as.character(numericInput(paste("inp", "accel_decel_efforts", p_clean, weekly_info$date[r_idx], sep = "__"), NULL, value = round(val), min = 0, width = "100px"))
#             }, html = TRUE)
#           )
#         )
#       )
#     }
#   )
# })