library(shiny)
library(shinyWidgets)
library(bslib)
library(DT)
library(reactable)
library(hms)
# library(rmarkdown)
library(tidyverse)
library(scales)
# library(zoo)
library(httr2)
# library(jsonlite)
library(googledrive)
# library(googlesheets4)
library(gargle)
library(fresh)
library(DescTools)
library(rootSolve)
# library(htmltools)
library(plotly)
library(here)

options(digits = 12, 
        reactable.theme = reactableTheme(
          color = "#221C35",
          stripedColor =rgb(229, 225, 230, round(0.4 * 255),maxColorValue = 255),
          highlightColor = rgb(0, 176, 185,alpha=(0.6*255), maxColorValue = 255),
          style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"),
          headerStyle = list(borderColor = "#221C35", backgroundColor = "#221C35", color = "white", fontWeight = "bold"),
          borderColor = "#221C35",
          borderWidth = "1.5px"
        )
)
# 
# parameters <- list(stats_period = stats_period, md_input="24th May 2026 - MD 5 vs Calgary (A)")
# 
# RendermyREPORT <- function(parameters = list()){
#   rmarkdown::render(input = here("TidesMatchReport.Rmd"),
#                     output_file = here(paste0("TidesMatchReport_",max(stats_period$date), ".pdf")),
#                     output_format = "all",
#                     params = parameters,
#                     encoding = "UTF-8", clean = T)}
# 
# 
# 
# RendermyREPORT(parameters = parameters)
# 



ui <- uiOutput("page_content") # Placeholder for either login or dashboard UI



server <- function(input, output, session) {
  
  
  
  
  googledrive::drive_auth(path=gargle::secret_decrypt_json(here::here(".secrets", "halifaxtidesdashboard-serviceaccount-encrypted.json"), "googledrive_token"))
  
  googledrive::drive_download(googledrive::as_id(Sys.getenv("rpe_file_id")), path="XPS RPE.csv", overwrite=T)
  googledrive::drive_download(googledrive::as_id(Sys.getenv("wellness_file_id")), path="XPS Wellness.csv", overwrite=T)
  
  xps_rpe_db <- read_csv("XPS RPE.csv", show_col_types =F) 
  
  xps_wellness_db <- read_csv("XPS Wellness.csv", show_col_types =F) 
  
  
  # Function to check for empty character strings and replace them with an empty data frame structure
  replace_empty_char_wellness <- function(x) {
    if (length(x) == 0) {
      tibble(id =NA_character_, localTime = "1970-01-01T00:00", value =NA_real_)
    } else {
      x
    }
  }
  
  replace_empty_char_rpe <- function(x) {
    if (length(x) == 0) {
      tibble(id =NA_character_, localTime = "1970-01-01T00:00", name = NA_character_,rpe =NA_real_,minutes =NA_real_)
    } else {
      x
    }
  }
  
  
  call_xps <- "https://www4.sidelinesports.com"
  
  
  athletes_xps <- request(call_xps) %>%
    req_url_path(path = "xpsweb/xpsapi/listathletes") %>%
    req_headers(access_key = Sys.getenv("xps_token")) %>%
    req_body_json(list(withGroupAccess = F)) %>%
    req_perform() %>%
    resp_body_json(flatten = T, simplifyDataFrame=T) %>%
    pluck("data")
  
  
  tests_xps <- request(call_xps) %>%
    req_url_path(path = "xpsweb/xpsapi/listtests") %>%
    req_headers(access_key = Sys.getenv("xps_token")) %>%
    req_perform()%>%
    resp_body_json(flatten = T, simplifyDataFrame=T)%>%
    pluck("root") %>%
    pluck("_children")
  
  
  athlete_id <- athletes_xps %>%
    dplyr::filter(name != "Test Test") %>%
    pull(id)
  
  test_id <- tests_xps %>%
    dplyr::filter(`_name`=="Tides Wellness") %>%
    select(`_children`) %>%
    unnest(`_children`) %>%
    dplyr::filter(unitType=="Custom") %>%
    # dplyr::filter(!`_isFolder`) %>%
    pull(`_guid`)
  
  wellness_last_entry <- max(xps_wellness_db$localTime)
  
  xps_wellness_new <- request(call_xps) %>%
    req_url_path(path = "xpsweb/xpsapi/gettestresults") %>%
    req_headers(access_key = Sys.getenv("xps_token")) %>%
    req_body_json(list(athleteIds = athlete_id,
                       testTemplateIds = test_id,
                       fromUtcSec = as.integer(wellness_last_entry+minutes(1)),
                       toUtcSec = as.integer(Sys.time()))) %>%
    req_perform() %>%
    resp_body_json(flatten = T, simplifyDataFrame=T) %>%
    pluck("data")  %>%
    rename(athlete_name = name) %>%
    unnest(tests) %>% 
    mutate(results = map(.x = results, .f=replace_empty_char_wellness)) %>% 
    unnest(results) %>%
    mutate(date = as.Date(str_extract(localTime, "\\d{4}-\\d{2}-\\d{2}")),
           localTime = ymd_hm(localTime),
           athlete_name = iconv(athlete_name, from="UTF-8",to="ASCII//TRANSLIT")) %>% 
    dplyr::filter(!is.na(value))
  
  
  if  (nrow(xps_wellness_new) >= 1) {
    xps_wellness_db <- rbind(xps_wellness_db, xps_wellness_new)
    write_csv(xps_wellness_db,file="XPS Wellness.csv")
    googledrive::drive_put("XPS Wellness.csv", path=googledrive::as_id(Sys.getenv("drive_folder_id")), name = "XPS Wellness.csv")
  }
  
  rpe_last_entry <- max(xps_rpe_db$localTime)
  
  xps_rpe_new <- request(call_xps) %>%
    req_url_path(path = "xpsweb/xpsapi/gettrainingloadresults") %>%
    req_headers(access_key = Sys.getenv("xps_token")) %>%
    req_body_json(list(athleteIds = athlete_id,
                       fromUtcSec = as.integer(rpe_last_entry+minutes(1)),
                       toUtcSec = as.integer(Sys.time()))) %>%
    req_perform() %>%
    resp_body_json(flatten = T, simplifyDataFrame=T)%>%
    pluck("data") %>%
    rename(athlete_name = name) %>%
    mutate(results = map(.x = results, .f=replace_empty_char_rpe)) %>% 
    unnest(results) %>%
    mutate(date = as.Date(str_extract(localTime, "\\d{4}-\\d{2}-\\d{2}")),
           localTime = ymd_hm(localTime),
           athlete_name = iconv(athlete_name, from="UTF-8",to="ASCII//TRANSLIT"))  %>% 
    dplyr::filter(!is.na(name))
  
  
  if  (nrow(xps_rpe_new) >= 1) {
    xps_rpe_db <- rbind(xps_rpe_db, xps_rpe_new)
    write_csv(xps_rpe_db,file="XPS RPE.csv")
    googledrive::drive_put("XPS RPE.csv", path=googledrive::as_id(Sys.getenv("drive_folder_id")), name = "XPS RPE.csv")
    
  }
  
  xps_wellness <- xps_wellness_db %>% 
    #Take most recent if duplicate entries on same day
    group_by(athlete_name,date,name) %>% 
    dplyr::filter(localTime == max(localTime)) %>% 
    ungroup %>% 
    select(athlete_name, date, name, value) %>% 
    mutate(category = case_when(str_detect(name, "Sleep") ~ "Sleep", 
                                str_detect(name, "Urine") | str_detect(name, "Meals") | str_detect(name, "Nutrition") ~ "Nutrition", 
                                str_detect(name, "Level") ~ "Mental",
                                str_detect(name, "Soreness") | str_detect(name, "Fatigue") ~ "Physical", 
                                str_detect(name, "Health") ~ "Health"))  %>% 
    mutate(item_ratio = if_else(category != "Nutrition", (value-1)/(7-1), (value-1)/(5-1)),
           item_percent = item_ratio*100,
           item_label_y = item_ratio*0.5) %>%
    arrange(athlete_name, date, category, desc(name)) %>% 
    group_by(athlete_name, date, category) %>% 
    mutate(category_item_ratio = if_else(category != "Nutrition", (value-1)/(n()*7-n()), (value-1)/(n()*5-n())),
           category_item_percent = category_item_ratio*100,
           category_item_label_y = cumsum(category_item_ratio) - (category_item_ratio*0.5),
           category_total = sum(value),
           category_ratio = if_else(category != "Nutrition", (category_total-n())/(n()*7-n()),(category_total-n())/(n()*5-n())),
           category_percent = category_ratio*100) %>% 
    ungroup
  
  wellness_total <- xps_wellness %>% 
    select(athlete_name,date,category,category_percent) %>% 
    unique() %>% 
    group_by(athlete_name, date) %>% 
    mutate(total_percent = mean(category_percent)) %>% 
    ungroup %>%
    select(athlete_name,date,total_percent) %>% 
    unique()
  
  
  wellness_scores <- xps_wellness %>% 
    left_join(wellness_total, by=join_by(athlete_name,date)) %>% 
    mutate(total_ratio=total_percent/100) %>% 
    relocate(total_ratio, .before=total_percent)
  
  
  RPE_soccer <- xps_rpe_db %>%
    dplyr::filter(name == "Team Training" | name == "Game / Competition") %>%
    mutate(session_rpe=rpe*minutes) %>% 
    group_by(athlete_name, date) %>%
    mutate(daily_rpe = sum(session_rpe, na.rm=T)) %>% 
    ungroup %>% 
    select(athlete_name, date, daily_rpe) %>% 
    unique
  
  RPE_all <- xps_rpe_db %>%
    mutate(session_rpe=rpe*minutes) %>%
    group_by(athlete_name, date) %>%
    mutate(daily_rpe = sum(session_rpe, na.rm=T)) %>%
    ungroup
  
  catapult_url <- "https://connect-us.catapultsports.com"
  
  
  googledrive::drive_auth(path=gargle::secret_decrypt_json(here::here(".secrets", "halifaxtidesdashboard-serviceaccount-encrypted.json"), "googledrive_token"))
  
  googledrive::drive_download(googledrive::as_id(Sys.getenv("stats_period_file_id")), path="Catapult Stats By Period.csv", overwrite=T)
  googledrive::drive_download(googledrive::as_id(Sys.getenv("stats_activity_file_id")), path="Catapult Stats By Activity.csv", overwrite=T)
  googledrive::drive_download(googledrive::as_id(Sys.getenv("activities_file_id")), path="Catapult Activities.csv", overwrite=T)
  
  stats_activity_db <- read_csv("Catapult Stats By Activity.csv", show_col_types =F) %>% 
    mutate(across(c(date_modified, start_time, end_time), ~ with_tz(.x,tzone="")))
  
  stats_period_db <- read_csv("Catapult Stats By Period.csv", show_col_types =F) %>% 
    mutate(across(c(date_modified, start_time, end_time), ~ with_tz(.x,tzone="")))
  
  activities_db <- read_csv("Catapult Activities.csv", show_col_types =F) %>% 
    mutate(across(c(date_modified, start_time, end_time), ~ with_tz(.x,tzone="")))
  
  
  #Check for new data
  
  # Get Halifax Tides FC team id
  team_id <- request(catapult_url) %>% 
    req_url_path(path = "api/v6/teams") %>% 
    req_auth_bearer_token(Sys.getenv("catapult_token")) %>%
    req_headers(accept= "application/json") %>% 
    req_perform() %>% 
    resp_body_json(flatten = T, simplifyDataFrame=T) %>% 
    dplyr::filter(name == "Halifax Tides FC") %>% 
    pull(id)
  
  # Get athletes currently on Halifax Tides FC
  athletes_catapult <- request(catapult_url) %>% 
    req_url_path(path = "api/v6/athletes") %>% 
    req_auth_bearer_token(Sys.getenv("catapult_token")) %>%
    req_headers(accept= "application/json") %>% 
    req_perform() %>% 
    resp_body_json(flatten = T, simplifyDataFrame=T) %>% 
    dplyr::filter(current_team_id == team_id) %>% 
    unite("athlete_name", ends_with("st_name"),sep =" ")
  
  
  activities <- request(catapult_url) %>% 
    req_url_path(path = "api/v6/activities") %>% 
    req_url_query(start_time=1767225600) %>%  
    req_auth_bearer_token(Sys.getenv("catapult_token")) %>%
    req_headers(accept= "application/json") %>% 
    req_perform() %>% 
    resp_body_json(flatten = T, simplifyDataFrame=T) %>% 
    mutate(start_time = as.POSIXct(start_time),
           end_time = as.POSIXct(end_time),
           date_modified = as.POSIXct(modified_at))%>% 
    select(id, name, date_modified, start_time, end_time, tag_list) %>%
    rename(activity_id=id, activity_name=name) %>% 
    unnest(tag_list) %>% 
    rename(tag_id=id) %>% 
    dplyr::filter(tag_type_name == "DayCode") 
  # %>% 
  #   mutate(start_time=if_else(start_time < as.Date("2025-01-01"),as.POSIXct("2025-02-11 09:00:00"), start_time),
  #          end_time=if_else(end_time < as.Date("2025-01-01"),as.POSIXct("2025-02-11 11:35:09"), end_time))
  
  
  date_from <- max(activities_db$date_modified)
  
  
  if (max(activities$date_modified) > date_from) {
    
    athletes_filter <- athletes_catapult$id
    activities_filter <- activities %>% dplyr::filter(date_modified > date_from) %>% pull(activity_id)
    
    filter_df <- data.frame(name = c(rep("athlete_id",length(athletes_filter)), rep("activity_id", length(activities_filter))),
                            comparison = c(rep("=",length(athletes_filter)),rep("=",length(activities_filter))),
                            values = c(athletes_filter,activities_filter)) %>%
      group_by(name, comparison) %>%
      summarise(values = list(values))
    
    
    groupby_activity <- c("athlete", "activity")
    
    groupby_period <- c("athlete", "period")
    
    params_activity <- c("athlete_name", "day_name", "team_name", "date", "start_time", "end_time","activity_id", "activity_name","position_name",
                         "bench_time", "field_time", "total_distance", "meterage_per_minute","velocity_band5_total_distance", "velocity_band6_total_distance", "velocity2_band6_total_distance",
                         "gen2_acceleration_band7plus_total_effort_count", "gen2_acceleration_band2plus_total_effort_count", "max_vel", "percentage_max_velocity",
                         "heart_rate_band5_average_duration_session", "heart_rate_band6_average_duration_session","heart_rate_band7_average_duration_session",
                         "heart_rate_band8_average_duration_session", "mean_heart_rate","percentage_avg_heart_rate", "percentage_max_heart_rate","max_heart_rate", "athlete_max_hr",
                         "total_goalkeeping_dives","total_dives_left", "total_dives_right","total_dives_centre","low_dive_load_(avg)","med_dive_load_(avg)","high_dive_load_(avg)",
                         "diveloadleft_band1_average_count_session","diveloadright_band1_average_count_session","diveloadcentre_band1_average_count_session",
                         "diveloadleft_band2_average_count_session","diveloadright_band2_average_count_session","diveloadcentre_band2_average_count_session",
                         "diveloadleft_band3_average_count_session","diveloadright_band3_average_count_session","diveloadcentre_band3_average_count_session",
                         "total_dive_load","total_dive_load_left","total_dive_load_right","total_dive_load_centre", "total_dive_load_low_intensity", "total_dive_load_med_intensity","total_dive_load_high_intensity",
                         "total_dive_load_left_low_intensity","total_dive_load_right_low_intensity", "total_dive_load_centre_low_intensity","total_dive_load_left_med_intensity","total_dive_load_right_med_intensity", "total_dive_load_centre_med_intensity","total_dive_load_left_high_intensity","total_dive_load_right_high_intensity", "total_dive_load_centre_high_intensity",
                         "median_time_to_feet", "average_time_to_feet", "average_time_to_feeet_left", "average_time_to_feeet_right", "average_time_to_feet_centre",
                         "explosive_efforts", "total_jumps", "ima_accels" , "ima_decels")
    
    
    params_period <- c("athlete_name", "day_name", "team_name", "date", "start_time", "end_time","activity_id", "activity_name", "period_id", "period_name","position_name",
                       "bench_time", "field_time", "total_distance", "meterage_per_minute","velocity_band5_total_distance", "velocity_band6_total_distance", "velocity2_band6_total_distance",
                       "gen2_acceleration_band7plus_total_effort_count", "gen2_acceleration_band2plus_total_effort_count", "max_vel", "percentage_max_velocity",
                       "heart_rate_band5_average_duration_session","heart_rate_band6_average_duration_session", "heart_rate_band7_average_duration_session",
                       "heart_rate_band8_average_duration_session", "mean_heart_rate","percentage_avg_heart_rate", "percentage_max_heart_rate","max_heart_rate", "athlete_max_hr",
                       "total_goalkeeping_dives","total_dives_left", "total_dives_right","total_dives_centre","low_dive_load_(avg)","med_dive_load_(avg)","high_dive_load_(avg)",
                       "diveloadleft_band1_average_count_session","diveloadright_band1_average_count_session","diveloadcentre_band1_average_count_session",
                       "diveloadleft_band2_average_count_session","diveloadright_band2_average_count_session","diveloadcentre_band2_average_count_session",
                       "diveloadleft_band3_average_count_session","diveloadright_band3_average_count_session","diveloadcentre_band3_average_count_session",
                       "total_dive_load","total_dive_load_left","total_dive_load_right","total_dive_load_centre", "total_dive_load_low_intensity", "total_dive_load_med_intensity","total_dive_load_high_intensity",
                       "total_dive_load_left_low_intensity","total_dive_load_right_low_intensity", "total_dive_load_centre_low_intensity","total_dive_load_left_med_intensity","total_dive_load_right_med_intensity", "total_dive_load_centre_med_intensity","total_dive_load_left_high_intensity","total_dive_load_right_high_intensity", "total_dive_load_centre_high_intensity",
                       "median_time_to_feet", "average_time_to_feet", "average_time_to_feeet_left", "average_time_to_feeet_right", "average_time_to_feet_centre",
                       "explosive_efforts", "total_jumps", "ima_accels" , "ima_decels")
    
    
    stats_activity_new <- request(catapult_url) %>% 
      req_url_path(path = "api/v6/stats") %>% 
      req_url_query(requested_only=TRUE) %>% 
      req_auth_bearer_token(Sys.getenv("catapult_token")) %>%
      req_headers(accept= "application/json") %>% 
      req_body_json(list(filters = filter_df,
                         parameters = params_activity,
                         group_by = groupby_activity)) %>% 
      req_perform() %>% 
      resp_body_json(flatten = T, simplifyDataFrame=T) %>% 
      mutate(date = as.Date(date, "%d/%m/%Y"),
             start_time = as.POSIXct(start_time),
             end_time = as.POSIXct(end_time)) %>%
      left_join(activities %>% select(c(activity_id, date_modified, tag_name)), by=join_by(activity_id), relationship="many-to-many")
    
    
    stats_period_new <- request(catapult_url) %>% 
      req_url_path(path = "api/v6/stats") %>% 
      req_url_query(requested_only=TRUE) %>% 
      req_auth_bearer_token(Sys.getenv("catapult_token")) %>%
      req_headers(accept= "application/json") %>% 
      req_body_json(list(filters = filter_df,
                         parameters = params_period,
                         group_by = groupby_period)) %>% 
      req_perform() %>% 
      resp_body_json(flatten = T, simplifyDataFrame=T) %>% 
      mutate(date = as.Date(date, "%d/%m/%Y"),
             start_time = as.POSIXct(start_time),
             end_time = as.POSIXct(end_time)) %>%
      left_join(activities %>% select(c(activity_id, date_modified, tag_name)), by=join_by(activity_id), relationship="many-to-many")
    
    
    stats_activity_temp <- stats_activity_db %>% 
      filter(!(activity_id %in% activities_filter))
    
    
    stats_period_temp <- stats_period_db %>% 
      filter(!(activity_id %in% activities_filter))
    
    stats_activity_db <- rbind(stats_activity_temp, stats_activity_new)
    
    stats_period_db <- rbind(stats_period_temp, stats_period_new)
    
    activities_db <- activities
    
    write_csv(stats_activity_db, file="Catapult Stats By Activity.csv")
    write_csv(stats_period_db, file="Catapult Stats By Period.csv")
    write_csv(activities_db, file="Catapult Activities.csv")
    
    googledrive::drive_put("Catapult Stats By Activity.csv", path=googledrive::as_id(Sys.getenv("drive_folder_id")), name = "Catapult Stats By Activity.csv")
    googledrive::drive_put("Catapult Stats By Period.csv", path=googledrive::as_id(Sys.getenv("drive_folder_id")), name = "Catapult Stats By Period.csv")
    googledrive::drive_put("Catapult Activities.csv", path=googledrive::as_id(Sys.getenv("drive_folder_id")), name = "Catapult Activities.csv")
    
  }
  

  metrics <- stats_activity_db %>% 
    mutate(across(where(is.numeric),~replace(.x,1:nrow(stats_activity_db),0))) %>% 
    select(athlete_name | where(is.numeric)) %>% 
    distinct
  

  dates <- data.frame(athlete_name = athletes_catapult$athlete_name) %>% 
    group_by(athlete_name) %>% 
    reframe(date =seq.Date(min(stats_activity_db$date), if_else(max(stats_activity_db$date)>=Sys.Date(), max(stats_activity_db$date),Sys.Date()), by='days')) %>% 
    left_join(metrics, by=join_by(athlete_name)) %>% 
    mutate(name_date = paste0(athlete_name, date)) %>% 
    filter(!(name_date %in% paste0(stats_activity_db$athlete_name,stats_activity_db$date))) %>% 
    select(!name_date)
  

  
  columns <- c("field_time", "total_distance", "high_speed_distance", "sprint_distance", "meterage_per_minute", 
               "accel_efforts", "decel_efforts","accel_decel_efforts", "max_vel_kph","max_heart_rate", "mean_heart_rate", 
               "total_dive_load", "dive_count", "explosive_efforts", "wellness", "rpe")
  
  BETA <- function(n) {
    2/(n+1)}
  
  
  EWMA <- function(x,n,gap){
    x_interp <- zoo::na.approx(x,maxgap=gap, na.rm = FALSE)
    ewma <- c()
    index <- min(which(!is.na(x_interp)))
    if (!all(is.na(x_interp)) && (index+n-1) <= length(x_interp)) {
      ewma[1:(index+n-2)] <- NA_real_
      ewma[index+n-1] <- mean(x_interp[index:(index+n-1)],na.rm=T)
      start <- index+n
      beta <- BETA(n)
      if (start <= length(x_interp)) {
        for (j in start:length(x_interp)) {
          
          ewma[j] <- beta*x_interp[j]+(1-beta)*ewma[j-1]
        }
      }
    } else {ewma <- rep(NA_real_, length(x))}
    return(ewma)
  }
  
  stats <- stats_activity_db %>% 
    full_join(dates) %>%
    left_join(RPE_soccer %>% rename(rpe=daily_rpe), by = join_by(athlete_name,date)) %>%
    left_join(wellness_total %>% rename(wellness=total_percent), by = join_by(athlete_name,date)) %>% 
    rename(sprint_distance = velocity_band6_total_distance,
           high_speed_distance = velocity_band5_total_distance,
           accel_efforts=gen2_acceleration_band7plus_total_effort_count,
           decel_efforts=gen2_acceleration_band2plus_total_effort_count,
           dive_count = total_goalkeeping_dives) %>% 
    # rowwise() %>% 
    # mutate(total_dive_impact = sum(c_across(contains("total_impact_dive_load")))) %>% 
    # ungroup %>% 
    mutate(rpe=replace_na(rpe,0),
           accel_decel_efforts = accel_efforts+decel_efforts,
           max_vel_kph = max_vel*3.6
    ) %>%
    arrange(athlete_name,date) %>% 
    group_by(athlete_name) %>%
    mutate(
      across(all_of(columns), ~zoo::rollapplyr(.x, 3, function(x) mean(x,na.rm=T), fill=NA),.names="al_{.col}"),
      across(all_of(columns), ~EWMA(.x, 7, 5),.names="al_ewma_{.col}"),
      across(all_of(columns), ~zoo::rollapplyr(.x, 28, function(x) mean(x,na.rm=T), fill=NA),.names="cl_{.col}"),
      across(all_of(columns), ~EWMA(.x, 28, 5),.names="cl_ewma_{.col}"),
      across(all_of(columns), ~zoo::rollapplyr(.x, 28, function(x) sd(x,na.rm=T), fill=NA),.names="cl_sd_{.col}"),
      acwr_ewma_total_distance = al_ewma_total_distance/cl_ewma_total_distance,
      acwr_ewma_high_speed_distance = al_ewma_high_speed_distance/cl_ewma_high_speed_distance,
      acwr_ewma_sprint_distance = al_ewma_sprint_distance/cl_ewma_sprint_distance,
      acwr_ewma_field_time = al_ewma_field_time/cl_ewma_field_time,
      acwr_ewma_meterage_per_minute = al_ewma_meterage_per_minute/cl_ewma_meterage_per_minute,
      acwr_ewma_max_vel_kph = al_ewma_max_vel_kph/cl_ewma_max_vel_kph,
      acwr_ewma_accel_efforts = al_ewma_accel_efforts/cl_ewma_accel_efforts,
      acwr_ewma_decel_efforts = al_ewma_decel_efforts/cl_ewma_decel_efforts,
      acwr_ewma_accel_decel_efforts = al_ewma_accel_decel_efforts/cl_ewma_accel_decel_efforts,
      acwr_ewma_dive_count = al_ewma_dive_count/cl_ewma_dive_count,
      acwr_ewma_total_dive_load = al_ewma_total_dive_load/cl_ewma_total_dive_load,
      # acwr_ewma_total_dive_impact = al_ewma_total_dive_impact/cl_ewma_total_dive_impact,
      acwr_ewma_explosive_efforts = al_ewma_explosive_efforts/cl_ewma_explosive_efforts,
      acwr_ewma_max_heart_rate = al_ewma_max_heart_rate/cl_ewma_max_heart_rate,
      acwr_ewma_mean_heart_rate = al_ewma_mean_heart_rate/cl_ewma_mean_heart_rate,
      acwr_ewma_rpe = al_ewma_rpe/cl_ewma_rpe,
      acwr_ewma_wellness = al_ewma_wellness/cl_ewma_wellness,
      zscore_7_28_total_distance = (al_total_distance - cl_total_distance)/cl_sd_total_distance,
      zscore_7_28_high_speed_distance = (al_high_speed_distance - cl_high_speed_distance)/cl_sd_high_speed_distance,
      zscore_7_28_sprint_distance = (al_sprint_distance - cl_sprint_distance)/cl_sd_sprint_distance,
      zscore_7_28_field_time = (al_field_time - cl_field_time)/cl_sd_field_time,
      zscore_7_28_meterage_per_minute = (al_meterage_per_minute - cl_meterage_per_minute)/cl_sd_meterage_per_minute,
      zscore_7_28_max_vel_kph = (al_max_vel_kph - cl_max_vel_kph)/cl_sd_max_vel_kph,
      zscore_7_28_accel_efforts = (al_accel_efforts - cl_accel_efforts)/cl_sd_accel_efforts,
      zscore_7_28_decel_efforts = (al_decel_efforts - cl_decel_efforts)/cl_sd_decel_efforts,
      zscore_7_28_accel_decel_efforts = (al_accel_decel_efforts - cl_accel_decel_efforts)/cl_sd_accel_decel_efforts,
      zscore_7_28_dive_count = (al_dive_count - cl_dive_count)/cl_sd_dive_count,
      zscore_7_28_total_dive_load = (al_total_dive_load - cl_total_dive_load)/cl_sd_total_dive_load,
      # zscore_7_28_total_dive_impact = (al_total_dive_impact - cl_total_dive_impact)/cl_sd_total_dive_impact,
      zscore_7_28_explosive_efforts = (al_explosive_efforts - cl_explosive_efforts)/cl_sd_explosive_efforts,
      zscore_7_28_max_heart_rate = (al_max_heart_rate - cl_max_heart_rate)/cl_sd_max_heart_rate,
      zscore_7_28_mean_heart_rate = (al_mean_heart_rate - cl_mean_heart_rate)/cl_sd_mean_heart_rate,
      zscore_7_28_rpe = (al_rpe - cl_rpe)/cl_sd_rpe,
      zscore_7_28_wellness = (al_wellness - cl_wellness)/cl_sd_wellness
    ) %>%
    ungroup %>% 
    mutate(tag_name=if_else(is.na(tag_name), "OFF",tag_name))
  
  stats_period <- stats_period_db %>% 
    full_join(dates) %>%
    # left_join(RPE_soccer %>% rename(rpe=daily_rpe), by = join_by(athlete_name,date)) %>%
    # left_join(wellness_total %>% rename(wellness=total_percent), by = join_by(athlete_name,date)) %>% 
    rename(sprint_distance = velocity_band6_total_distance,
           high_speed_distance = velocity_band5_total_distance,
           accel_efforts=gen2_acceleration_band7plus_total_effort_count,
           decel_efforts=gen2_acceleration_band2plus_total_effort_count,
           dive_count = total_goalkeeping_dives) %>% 
    # rowwise() %>% 
    # mutate(total_dive_impact = sum(c_across(contains("total_impact_dive_load")))) %>% 
    # ungroup %>% 
    mutate(
      # rpe=replace_na(rpe,0),
          accel_decel_efforts = accel_efforts+decel_efforts,
           max_vel_kph = max_vel*3.6,
           tag_name=if_else(is.na(tag_name), "OFF",tag_name)) %>%
    arrange(athlete_name,date) 
  
  
  # Reactive value to track login status
  logged_in_coach <- reactiveVal(FALSE)
  logged_in_IST <- reactiveVal(FALSE)
  
  # Login page UI
  login_ui <- function() {
    page_fluid(
      title="",
      theme = bs_theme(version = 5, bootswatch = "lumen",
                       bg = "#FFFFFF",
                       fg = "#221C35",
                       navbar_bg = "#221C35",
                       primary = "#00B0B9",
                       secondary = "#00B0B9",
                       success = "#00B0B9",
                       info = "#572C5F",
                       warning = "#572C5F",
                       danger = "#572C5F",
                       "card-cap-bg" = "#221C35",
                       "card-cap-color" = "#FFFFFF"),  
      tags$div(
        class = "d-flex justify-content-center align-items-center",
        style = "min-height: 100vh;",
        card(
          card_header("Halifax Tides FC Load Monitoring"),
          textInput("username", "Username"),
          passwordInput("password", "Password"),
          actionButton("login_button", "Login"),
          uiOutput("login_message")
        )))
  }
  
  athlete_positions <- athletes_catapult %>% 
    select(athlete_name, position_name)
  
  athlete1 <- virtualSelectInput(
    inputId = "athlete1",
    label = "Player",
    choices = prepare_choices(athlete_positions, athlete_name, athlete_name, position_name),
    selected = athlete_positions$athlete_name[1],
    showValueAsTags = F,
    search = F,
    multiple = T)
  
  athlete2 <- virtualSelectInput(
    inputId = "athlete2",
    label = "Player",
    choices = prepare_choices(athlete_positions, athlete_name, athlete_name, position_name),
    selected = athlete_positions$athlete_name[1],
    showValueAsTags = F,
    search = F,
    multiple = T)
  
  athlete3 <- virtualSelectInput(
    inputId = "athlete3",
    label = "Player",
    choices = prepare_choices(athlete_positions, athlete_name, athlete_name, position_name),
    selected = athlete_positions$athlete_name[1],
    showValueAsTags = F,
    search = F,
    multiple = T)
  
  
  athlete4 <- virtualSelectInput(
    inputId = "athlete4",
    label = "Player",
    choices = prepare_choices(athlete_positions, athlete_name, athlete_name, position_name),
    selected = athlete_positions$athlete_name[1],
    showValueAsTags = F,
    search = F,
    multiple = T)
  
  
  athlete5 <- virtualSelectInput(
    inputId = "athlete5",
    label = "Player",
    choices = prepare_choices(athlete_positions, athlete_name, athlete_name, position_name),
    selected = athlete_positions$athlete_name[1],
    showValueAsTags = F,
    search = F,
    multiple = F)
  
  
  athlete6 <- virtualSelectInput(
    inputId = "athlete6",
    label = "Player",
    choices = prepare_choices(athlete_positions, athlete_name, athlete_name, position_name),
    selected = athlete_positions$athlete_name,
    showValueAsTags = F,
    search = F,
    multiple = T)
  
  
  athlete7 <- virtualSelectInput(
    inputId = "athlete7",
    label = "Player",
    choices = prepare_choices(athlete_positions, athlete_name, athlete_name, position_name),
    selected = athlete_positions$athlete_name,
    showValueAsTags = F,
    search = F,
    multiple = T)
  
  athlete8 <- virtualSelectInput(
    inputId = "athlete8",
    label = "Player",
    choices = prepare_choices(athlete_positions, athlete_name, athlete_name, position_name),
    selected = athlete_positions$athlete_name,
    showValueAsTags = F,
    search = F,
    multiple = T)
  
  athlete9 <- virtualSelectInput(
    inputId = "athlete9",
    label = "Player",
    choices = prepare_choices(athlete_positions, athlete_name, athlete_name, position_name),
    selected = athlete_positions$athlete_name,
    showValueAsTags = F,
    search = F,
    multiple = T)
  
  date_input1 <- dateInput(
    "date_input1", "Date",
    value=Sys.Date(),
    min = as.Date("2026-01-01"), 
    max = Sys.Date(),
    format = "yyyy-M-dd")
  
  date_input2 <- dateInput(
    "date_input2", "Date",
    value=Sys.Date(),
    min = as.Date("2026-01-01"), 
    max = Sys.Date(),
    format = "yyyy-M-dd")
  
  date_input3 <- dateInput(
    "date_input3", "Date",
    value=Sys.Date(),
    min = as.Date("2026-01-01"), 
    max = Sys.Date(),
    format = "yyyy-M-dd")
  
  date_input4 <- dateInput(
    "date_input4", "Date",
    value=Sys.Date(),
    min = as.Date("2026-01-01"), 
    max = Sys.Date(),
    format = "yyyy-M-dd")
  
  date_input5 <- dateInput(
    "date_input5", "Date",
    value=Sys.Date(),
    min = as.Date("2026-01-01"), 
    max = Sys.Date(),
    format = "yyyy-M-dd")
  
  
  date_input6 <- dateInput(
    "date_input6", "Date",
    value = Sys.Date(),
    min = as.Date("2026-01-01"), 
    max = Sys.Date(),
    format = "yyyy-M-dd")
  
  date_input7 <- dateInput(
    "date_input7", "Date",
    value = stats %>% filter(tag_name == "MD") %>% select(date) %>% summarize(date = max(date)) %>% pull(date),
    min = stats %>% filter(tag_name == "MD") %>% select(date) %>% summarize(date = min(date)) %>% pull(date), 
    max = stats %>% filter(tag_name == "MD") %>% select(date) %>% summarize(date = max(date)) %>% pull(date),
    format = "yyyy-M-dd")
  
  md_input <- selectInput(
    "md_input", "Match",
    choices = stats %>% filter(tag_name == "MD") %>% arrange(desc(date)) %>% pull(activity_name) %>% unique,
    selected = stats %>% filter(tag_name == "MD") %>% filter(date == max(date)) %>% pull(activity_name) %>% unique)
    
  date_range1 <- dateRangeInput(
    "date_range1", "Date Range",
    start = Sys.Date()-weeks(4),
    end = Sys.Date(),
    min = as.Date("2026-01-01"), 
    max = Sys.Date(),
    format = "yyyy-M-dd")
  
  date_range2 <- dateRangeInput(
    "date_range2", "Date Range",
    start = Sys.Date()-weeks(4),
    end = Sys.Date(),
    min = as.Date("2026-01-01"), 
    max = Sys.Date(),
    format = "yyyy-M-dd")
  
  date_range3 <- dateRangeInput(
    "date_range3", "Date Range",
    start = Sys.Date()-weeks(4),
    end = Sys.Date(),
    min = as.Date("2026-01-01"), 
    max = Sys.Date(),
    format = "yyyy-M-dd")
  
  date_range4 <- dateRangeInput(
    "date_range4", "Date Range",
    start = Sys.Date()-weeks(4),
    end = Sys.Date(),
    min = as.Date("2026-01-01"), 
    max = Sys.Date(),
    format = "yyyy-M-dd")
  
  
  ext_load_param <- selectInput(
    "ext_load_param", "External Workload Parameter",
    c("Total Distance" = "external_load_total_distance", "High Speed Distance" = "external_load_high_speed_distance", "Sprint Distance" = "external_load_sprint_distance", 
      "Field Time" = "external_load_field_time", "Meterage per Minute" = "external_load_meterage_per_minute", "Max Velocity" = "external_load_max_vel_kph", 
      "Accel Efforts" = "external_load_accel_efforts", "Decel Efforts" = "external_load_decel_efforts","Accel + Decel Efforts" = "external_load_accel_decel_efforts",
      "Dive Count" = "external_load_dive_count", "Total Dive Load" = "external_load_total_dive_load", "Explosive Efforts" = "external_load_explosive_efforts"),
    selected = "Total Distance")
  
  workload_param <- selectInput(
    "workload_param", "Workload Parameter",
    c("Total Distance" = "workload_total_distance", "High Speed Distance" = "workload_high_speed_distance", "Sprint Distance" = "workload_sprint_distance", 
      "Field Time" = "workload_field_time", "Meterage per Minute" = "workload_meterage_per_minute", "Max Velocity" = "workload_max_vel_kph", 
      "Accel Efforts" = "workload_accel_efforts", "Decel Efforts" = "workload_decel_efforts","Accel + Decel Efforts" = "workload_accel_decel_efforts",
      "Dive Count" = "workload_dive_count", "Total Dive Load" = "workload_total_dive_load", "Explosive Efforts" = "workload_explosive_efforts",
      "Avg HR"="workload_mean_heart_rate", "Max HR"="workload_max_heart_rate","RPE" = "workload_rpe"),
    selected = "Total Distance")
  
  acwr_param <- selectInput(
    "acwr_param", "Workload Parameter",
    c("Total Distance" = "total_distance", "High Speed Distance" = "high_speed_distance", "Sprint Distance" = "sprint_distance", 
      "Field Time" = "field_time", "Meterage per Minute" = "meterage_per_minute", "Max Velocity" = "max_vel_kph",
      "Accel Efforts" = "accel_efforts", "Decel Efforts" = "decel_efforts", "Accel + Decel Efforts" = "accel_decel_efforts", 
      "Avg HR"="mean_heart_rate", "Max HR"="max_heart_rate", 
      "Dive Count" = "dive_count", "Total Dive Load" = "total_dive_load", "Explosive Efforts" = "explosive_efforts"),
    selected = "Total Distance")
  
  acwr_param2 <- selectInput(
    "acwr_param2", "Workload Parameter",
    c("Total Distance" = "total_distance", "High Speed Distance" = "high_speed_distance", "Sprint Distance" = "sprint_distance", 
      "Field Time" = "field_time", "Meterage per Minute" = "meterage_per_minute", "Max Velocity" = "max_vel_kph",
      "Accel Efforts" = "accel_efforts", "Decel Efforts" = "decel_efforts", "Accel + Decel Efforts" = "accel_decel_efforts", 
      "Avg HR"="mean_heart_rate", "Max HR"="max_heart_rate", 
      "Dive Count" = "dive_count", "Total Dive Load" = "total_dive_load", "Explosive Efforts" = "explosive_efforts"),
    selected = "Total Distance")
  
  # aggregation <- selectInput(
  #   "aggregation", "Aggregation",
  #   c("Mean", "Sum", "Max", "Min"),
  #   selected = "Mean")
  
  wellness_values <- rbind(wellness_scores %>% select(c(category, name)) %>% distinct(), data.frame(category = c("Wellness", "Physical", "Mental", "Sleep", "Nutrition", "Health"), name = c("Total Wellness","Total Physical", "Total Mental",  "Total Sleep", "Total Nutrition", "Total Health")))
  
  wellness_param <- virtualSelectInput(
    inputId = "wellness_param",
    label = "Wellness Parameter",
    choices = prepare_choices(wellness_values, name, name, category),
    selected = "Total Wellness",
    showValueAsTags = F,
    search = F,
    multiple = F)
  
  acwr_input <- numericInput(
    "acwr_input", "Planned ACWR",
    value = 1,
    min = 0.5,
    max = 1.5,
    step = 0.1)
  
  period_input <- virtualSelectInput(
    inputId = "period_input", 
    label = "Drill/Period",
    choices = NULL,
    selected = NULL,
    showValueAsTags = F,
    search = F,
    multiple = T)
  
  observe({
    
    req(input$athlete7, input$date_input6)
    
    stats_period_filtered <- stats_period %>% 
      dplyr::filter(athlete_name %in% input$athlete7 & date == input$date_input6) %>% 
      drop_na(period_name)
    
    updateVirtualSelect(
      inputId = "period_input", 
      label = "Drill/Period",
      choices = unique(stats_period_filtered$period_name),
      selected = unique(stats_period_filtered$period_name))
    
  }) %>% bindEvent(input$athlete7, input$date_input6)
  
  # Main dashboard UI
  dashboard_ui <- function() {
    page_navbar(
      theme = bs_theme(version = 5, 
                       bootswatch = "lumen",
                       bg = "#FFFFFF",
                       fg = "#221C35",
                       navbar_bg = "#221C35",
                       primary = "#00B0B9",
                       secondary = "#00B0B9",
                       success = "#00B0B9",
                       info = "#572C5F",
                       warning = "#572C5F",
                       danger = "#572C5F",
                       "card-cap-bg" = "#221C35",
                       "card-cap-color" = "#FFFFFF"),  
      title = div(img(src = "HfxTidesFC.png", height = "40px", style = "margin-right: 10px;"), "Halifax Tides FC Load Monitoring"),
      sidebar=NULL,
      fillable = T,
      nav_spacer(),
      nav_panel(title="Daily Load Report", 
                layout_sidebar(      
                  sidebar = sidebar(athlete6, date_input5, bg = "#E5E1E6"),
                  layout_column_wrap(
                      width=1/6,
                      heights_equal = "row",
                      uiOutput("total_distance_valuebox"),
                      uiOutput("high_speed_distance_valuebox"),
                      uiOutput("sprint_distance_valuebox"),
                      uiOutput("accel_efforts_valuebox"),
                      uiOutput("decel_efforts_valuebox"),
                      uiOutput("max_vel_valuebox")
                    ),
                  layout_column_wrap(
                    width=1/2,
                    heights_equal = "row",
                    card(
                      full_screen = TRUE,
                      card_header("Total Distance (Group Avg)"),
                      card_body(min_height = 200, plotlyOutput("TotalDistanceGroupAvg"))
                    ),
                    card(
                      full_screen = TRUE,
                      card_header("High Speed Running and Sprint Distance (Group Avg)"),
                      card_body(min_height = 200, plotlyOutput("HSDistanceGroupAvg"))
                    )
                  ),
                  card(
                      full_screen = TRUE,
                      card_header("Distance by Player"),
                      card_body(min_height = 200, plotlyOutput("DistanceByPlayer"))
                    ),
                  card(
                    full_screen = TRUE,
                    card_header("Player Summary"),
                    card_body(min_height = 200, reactableOutput("PlayerDailySummaryTable"))
                  )
                )
      ),
      nav_panel(title="Daily Drill Report",
                layout_sidebar(
                  sidebar = sidebar(athlete7, date_input6, period_input, bg = "#E5E1E6"),
                  layout_column_wrap(
                    width=1/6,
                    heights_equal = "row",
                    uiOutput("total_distance_drill_valuebox"),
                    uiOutput("high_speed_distance_drill_valuebox"),
                    uiOutput("sprint_distance_drill_valuebox"),
                    uiOutput("accel_efforts_drill_valuebox"),
                    uiOutput("decel_efforts_drill_valuebox"),
                    uiOutput("max_vel_drill_valuebox")
                  ),
                  layout_column_wrap(
                    width=1/2,
                    heights_equal = "row",
                    card(
                      full_screen = TRUE,
                      card_header("Total Distance By Drill (Group Avg)"),
                      card_body(min_height = 200, plotlyOutput("TotalDistanceDrillGroupAvg"))
                    ),
                    card(
                      full_screen = TRUE,
                      card_header("High Speed Running and Sprint Distance by Drill (Group Avg)"),
                      card_body(min_height = 200, plotlyOutput("HSDistanceDrillGroupAvg"))
                    )
                  ),
                  card(
                    full_screen = TRUE,
                    card_header("Drill Distance by Player"),
                    card_body(min_height = 200, plotlyOutput("DistanceDrillByPlayer"))
                  ),
                  card(
                    full_screen = TRUE,
                    card_header("Drill Summary"),
                    card_body(min_height = 200, reactableOutput("DrillSummaryTable"))
                  )
                )
      ),
      nav_panel(title="Match Day Report",
                layout_sidebar(
                  sidebar = sidebar(athlete8, md_input, 
                                    fileInput("images","Select Image Files", multiple = T,accept = "image/*", width="100%"),
                                    downloadButton("download_pdf", "Download Match Report"), 
                                    bg = "#E5E1E6"),
                  card(
                    full_screen = TRUE,
                    card_header("Match Day Summary"),
                    card_body(min_height = 200, reactableOutput("MatchDayTable"))
                  ),
                  card(
                    full_screen = TRUE,
                    card_header("Distance Per Half (Group Avg)"),
                    card_body(min_height = 200, plotlyOutput("MDDistancePerHalf"))
                  ),
                  layout_column_wrap(
                    width=1/2,
                    heights_equal = "row",
                    card(
                      full_screen = TRUE,
                      card_header("Total Distance By Player"),
                      card_body(min_height = 200, plotlyOutput("MDTotalDistanceByPlayer"))
                    ),
                    card(
                      full_screen = TRUE,
                      card_header("Total Distance Per Min By Player"),
                      card_body(min_height = 200, plotlyOutput("MDTotalDistancePerMinByPlayer"))
                    ),
                    card(
                      full_screen = TRUE,
                      card_header("HSR Distance By Player"),
                      card_body(min_height = 200, plotlyOutput("MDHSRDistanceByPlayer"))
                    ),
                    card(
                      full_screen = TRUE,
                      card_header("HSR Distance Per Min By Player"),
                      card_body(min_height = 200, plotlyOutput("MDHSRDistancePerMinByPlayer"))
                    ),
                    card(
                      full_screen = TRUE,
                      card_header("Sprint Distance By Player"),
                      card_body(min_height = 200, plotlyOutput("MDSprintDistanceByPlayer"))
                    ),
                    card(
                      full_screen = TRUE,
                      card_header("Sprint Distance Per Min By Player"),
                      card_body(min_height = 200, plotlyOutput("MDSprintDistancePerMinByPlayer"))
                    )
                  ),
                  card(
                    full_screen = TRUE,
                    card_header("Total Distance Across Match"),
                    card_body(min_height = 200, plotlyOutput("MDTotalDistance15min"))
                  ),
                  # card(
                  #   full_screen = TRUE,
                  #   card_header("Total Distance Per Min Across Match"),
                  #   card_body(min_height = 200, plotlyOutput("MDTotalDistancePerMin15min"))
                  # ),
                  card(
                    full_screen = TRUE,
                    card_header("HSR Distance Across Match"),
                    card_body(min_height = 200, plotlyOutput("MDHSRDistance15min"))
                  ),
                  # card(
                  #   full_screen = TRUE,
                  #   card_header("HSR Distance Per Min Across Match"),
                  #   card_body(min_height = 200, plotlyOutput("MDHSRDistancePerMin15min"))
                  # ),
                  card(
                    full_screen = TRUE,
                    card_header("Sprint Distance Across Match"),
                    card_body(min_height = 200, plotlyOutput("MDSprintDistance15min"))
                  ),
                  # card(
                  #   full_screen = TRUE,
                  #   card_header("Sprint Distance Per Min Across Match"),
                  #   card_body(min_height = 200, plotlyOutput("MDSprintDistancePerMin15min"))
                  # ),
                  layout_column_wrap(
                    width=1/2,
                    heights_equal = "row",
                    card(
                      full_screen = TRUE,
                      card_header("MD Comparison Total Distance"),
                      card_body(min_height = 200, plotlyOutput("MDComparisonTotalDistance"))
                    ),
                    card(
                      full_screen = TRUE,
                      card_header("MD Comparison Total Distance Per Min"),
                      card_body(min_height = 200, plotlyOutput("MDComparisonTotalDistancePerMin"))
                    ),
                    card(
                      full_screen = TRUE,
                      card_header("MD Comparison HSR Distance"),
                      card_body(min_height = 200, plotlyOutput("MDComparisonHSRDistance"))
                    ),
                    card(
                      full_screen = TRUE,
                      card_header("MD Comparison HSR Distance Per Min"),
                      card_body(min_height = 200, plotlyOutput("MDComparisonHSRDistancePerMin"))
                    ),
                    card(
                      full_screen = TRUE,
                      card_header("MD Comparison Sprint Distance"),
                      card_body(min_height = 200, plotlyOutput("MDComparisonSprintDistance"))
                    ),
                    card(
                      full_screen = TRUE,
                      card_header("MD Comparison Sprint Distance Per Min"),
                      card_body(min_height = 200, plotlyOutput("MDComparisonSprintDistancePerMin"))
                    )
                  )
                )
      ),
      nav_panel(title="Acute:Chronic Workload", 
                layout_sidebar(      
                  sidebar = sidebar(athlete1, date_range1, acwr_param, bg = "#E5E1E6"),
                  card(
                    height = 400,
                    full_screen = TRUE,
                    card_header("Acute:Chronic Workload"),
                    card_body(min_height = 200, plotlyOutput("AcuteChronicLoad"))
                  )
                )
      ),
      nav_panel(title="Quadrant Graphs",
                layout_sidebar(
                  sidebar = sidebar(athlete2, date_input1, ext_load_param, workload_param, bg = "#E5E1E6"),
                  layout_column_wrap(
                    width=1/2,
                    card(
                      height = 400,
                      full_screen = TRUE,
                      card_header("Internal vs. External Workload"),
                      card_body(min_height = 200, plotlyOutput("IntExtLoad"))
                    ),
                    card(
                      height = 400,
                      full_screen = TRUE,
                      card_header("Subjective vs. External Workload"),
                      card_body(min_height = 200, plotlyOutput("SubExtLoad"))
                    ),
                    card(
                      height = 400,
                      full_screen = TRUE,
                      card_header("Wellness vs. Workload"),
                      card_body(min_height = 200, plotlyOutput("WellnessWorkload"))
                    ),
                    card(
                      height = 400,
                      full_screen = TRUE,
                      card_header("Readiness vs. Wellness"),
                      card_body(min_height = 200, plotlyOutput("ReadinessWellness"))
                    )
                  )
                )
      ),
      nav_panel(title="Subjective Load",
                layout_sidebar(
                  sidebar = sidebar(athlete5, date_range2, bg = "#E5E1E6"),
                  card(
                    height = 400,
                    full_screen = TRUE,
                    card_header("Subjective Load"),
                    card_body(min_height = 200,plotlyOutput("RPE"))
                  )
                )        
      ),
      nav_panel(title="Wellness",
                layout_sidebar(
                  sidebar = sidebar(athlete3, bg = "#E5E1E6"),
                  card(
                    height = 400,
                    full_screen = TRUE,
                    card_header("Daily Wellness"),
                    layout_sidebar(
                      sidebar = sidebar(date_input2, bg = "#E5E1E6"),
                      card_body(min_height = 200,plotlyOutput("Wellness"))
                    )
                  ),
                  card(
                    height = 400,
                    full_screen = TRUE,
                    card_header("Historical Wellness"),
                    layout_sidebar(
                      sidebar = sidebar(date_range3, wellness_param, bg = "#E5E1E6"),
                      card_body(min_height = 200,plotlyOutput("HistoricalWellness"))
                    )
                  )
                )        
      ),
      # nav_panel(title="Hydration Status",
      #           layout_sidebar(
      #             sidebar = sidebar(date_input4, bg = "#E5E1E6"),
      #             card(
      #               height = 400,
      #               full_screen = TRUE,
      #               card_header("Hydration Status"),
      #               card_body(min_height = 200,uiOutput("HydrationValueBoxes"))
      #             )
      #           )        
      # ),
      nav_panel(title="Load Planning",
                layout_sidebar(sidebar = sidebar(athlete4, acwr_param2, acwr_input, bg = "#E5E1E6"),
                               card(
                                 height = 400,
                                 full_screen = TRUE,
                                 card_header("Load Planning"),
                                 card_body(min_height = 200, uiOutput("PlannedLoad"))
                               )
                )        
      ),
      # nav_panel(title="Load Report",
      #           layout_sidebar(
      #             sidebar = sidebar(date_input3,
      #                               downloadButton("DownloadReport", "Download Report", class="button1"),
      #                               downloadButton("DownloadReportForAthletes", "Download Player Report", class="button1"),
      #                               bg = "#E5E1E6"),
      #             navset_card_underline(
      #               title = "",
      #               nav_panel("Player Summary", DTOutput("PlayerSummaryTable")),
      #               nav_panel("Keeper Summary", DTOutput("KeeperSummaryTable")),
      #               nav_panel("Player Workload", DTOutput("PlayerLoadTable")),
      #               nav_panel("Keeper Workload", DTOutput("KeeperLoadTable"))
      #             )
      #           )        
      # ),
      nav_item(actionButton("logout_button", "Logout"))
    )
  }
  
  
  # Render either login or dashboard based on logged_in state
  output$page_content <- renderUI({
    if (logged_in_IST() | logged_in_coach()) {
      dashboard_ui()
    } else {
      login_ui()
    }
  })
  
  
  # Handle login attempt
  observe({
    if (input$username == Sys.getenv("username_IST") && input$password == Sys.getenv("password_IST")){
      logged_in_IST(TRUE)
    } else if (input$username == Sys.getenv("username_coach") && input$password == Sys.getenv("password_coach")){
      logged_in_coach(TRUE)
    } else{
      output$login_message <- renderUI({span("Invalid credentials", style = "color: red;")})}
  }) %>% 
    bindEvent(input$login_button)
  
  # Handle logout
  observe({logged_in_IST(FALSE) & logged_in_coach(FALSE)}) %>% 
    bindEvent(input$logout_button)
  
  
  distance_group_avg_plot <- reactive({
    
    shiny::validate(need(!is.null(input$athlete6), "Select one or more players"))
    
    
    shiny::validate(need(nrow(stats %>%
                                mutate(across(where(is.numeric), ~if_else(field_time == 0 & total_distance == 0, NA_real_, .x))) %>% 
                                dplyr::filter(athlete_name %in% input$athlete6 & date <= input$date_input5 & date >= (input$date_input5-days(6))) %>%
                                filter(!if_all(where(is.numeric), is.na))) > 0 ,"No Data"))
    
    
    distance_group_avg_stats <- stats %>%
      mutate(across(where(is.numeric), ~if_else(field_time == 0 & total_distance == 0, NA_real_, .x))) %>% 
      select(athlete_name | date | tag_name | total_distance) %>% 
      dplyr::filter(athlete_name %in% input$athlete6 &  date <= input$date_input5 & date >= (input$date_input5-days(6))) %>%
      drop_na(tag_name) %>% 
      group_by(date,tag_name) %>% 
      summarize(across(where(is.numeric), ~mean(.x,na.rm=T))) %>% 
      ungroup
    

    distance_group_avg_stats %>% 
      plot_ly() %>%
      add_trace(x = ~date, y = ~total_distance, type = "bar", customdata=~tag_name, color=I("#00B0B9"),
                hovertemplate = paste0(
                  "<b>Date:</b> %{x| %b %d, %Y}<br>",
                  "<b>MD Code:</b> %{customdata}<br>",
                  "<b>%{yaxis.title.text}:</b> %{y:.1f}",
                  "<extra></extra>"))%>% 
      config(displaylogo = FALSE, scrollZoom = FALSE, displayModeBar = FALSE) %>%
      layout(
        xaxis = list(range=c(input$date_input5-days(7), input$date_input5+days(1)),tickvals = seq(input$date_input5-days(6), input$date_input5, by="day"), showline=TRUE,showgrid = FALSE,type = 'date', tickformat = "%a", dtick=86400000, title=""),
        yaxis = list(showline=TRUE,showgrid = FALSE,title="Total Distance (m)"),
        plot_bgcolor  = rgb(0,0,0,0),
        paper_bgcolor = rgb(0,0,0,0))
    
  })
  
  hs_distance_group_avg_plot <- reactive({
    
    shiny::validate(need(!is.null(input$athlete6), "Select one or more players"))
    
    
    shiny::validate(need(nrow(stats %>%
                                mutate(across(where(is.numeric), ~if_else(field_time == 0 & total_distance == 0, NA_real_, .x))) %>% 
                                dplyr::filter(athlete_name %in% input$athlete6 & date <= input$date_input5 & date >= (input$date_input5-days(6))) %>%
                                filter(!if_all(where(is.numeric), is.na))) > 0 ,"No Data"))
    
    
    hs_distance_group_avg_stats <- stats %>%
      mutate(across(where(is.numeric), ~if_else(field_time == 0 & total_distance == 0, NA_real_, .x))) %>% 
      select(athlete_name | date | tag_name | high_speed_distance | sprint_distance) %>% 
      dplyr::filter(athlete_name %in% input$athlete6 & date <= input$date_input5 & date >= (input$date_input5-days(6))) %>%
      drop_na(tag_name) %>% 
      group_by(date,tag_name) %>% 
      summarize(across(where(is.numeric), ~mean(.x,na.rm=T))) %>% 
      ungroup
    
    hs_distance_group_avg_stats %>% 
      plot_ly() %>%
      add_trace(x = ~date, y = ~high_speed_distance, name="HSR Distance (m)",  customdata=~tag_name, type = "bar", color=I("#00B0B9"),
                hovertemplate = paste0(
                  "<b>Date:</b> %{x| %b %d, %Y}<br>",
                  "<b>MD Code:</b> %{customdata}<br>",
                  "<b>HSR Distance (m):</b> %{y:.1f}",
                  "<extra></extra>"))%>% 
      add_trace(x = ~date, y = ~sprint_distance, name="Sprint Distance (m)", customdata=~tag_name, type = "bar", color=I("#B2C9D4"),
                hovertemplate = paste0(
                  "<b>Date:</b> %{x| %b %d, %Y}<br>",
                  "<b>MD Code:</b> %{customdata}<br>",
                  "<b>Sprint Distance (m):</b> %{y:.1f}",
                  "<extra></extra>"))%>% 
      config(displaylogo = FALSE, scrollZoom = FALSE, displayModeBar = FALSE) %>%
      layout(
        xaxis = list(range=c(input$date_input5-days(7), input$date_input5+days(1)),tickvals = seq(input$date_input5-days(6), input$date_input5, by="day"),showline=TRUE,showgrid = FALSE,type = 'date', tickformat = "%a", dtick=86400000, title=""),
        yaxis = list(showline=TRUE,showgrid = FALSE,title="Distance (m)"),
        legend = list(orientation = 'h',xanchor = "center", x = 0.5,y = -0.1),
        plot_bgcolor  = rgb(0,0,0,0),
        paper_bgcolor = rgb(0,0,0,0))
    
  })
  
  
  
  distance_by_player_plot <- reactive({
    
    shiny::validate(need(!is.null(input$athlete6), "Select one or more players"))
    
    
    shiny::validate(need(nrow(stats %>%
                                mutate(across(where(is.numeric), ~if_else(field_time == 0 & total_distance == 0, NA_real_, .x))) %>% 
                                dplyr::filter(athlete_name %in% input$athlete6 & date == input$date_input5) %>%
                                filter(!if_all(where(is.numeric), is.na))) > 0 ,"No Data"))
    
    
    distance_by_player_stats <- stats %>%
      mutate(across(where(is.numeric), ~if_else(field_time == 0 & total_distance == 0, NA_real_, .x))) %>% 
      select(athlete_name | date | tag_name | total_distance | high_speed_distance | sprint_distance) %>% 
      dplyr::filter(
        athlete_name %in% input$athlete6 &
                      date == input$date_input5)
    
    
    distance_by_player_stats %>% 
      plot_ly() %>%
      add_trace(x = ~str_extract(athlete_name,"(?<=\\s).+$"), y = ~total_distance, name = "Total Distance (m)", customdata=~paste0(athlete_name, "\n<b>Date:</b> ",format(date, "%b %d, %Y"), "\n<b>MD Code:</b> ", tag_name),
                type = "bar", color=I("#00B0B9"),
                hovertemplate = paste0(
                  "<b>Player:</b> %{customdata}<br>",
                  "<b>Total Distance (m):</b> %{y:.1f}",
                  "<extra></extra>"))%>% 
      add_trace(x = ~str_extract(athlete_name,"(?<=\\s).+$"), y = ~high_speed_distance, name="HSR Distance (m)", customdata=~paste0(athlete_name, "\n<b>Date:</b> ",format(date, "%b %d, %Y"), "\n<b>MD Code:</b> ", tag_name),
                type = "bar", color=I("#572C5F"),yaxis = "y2", 
                hovertemplate = paste0(
                  "<b>Player:</b> %{customdata}<br>",
                  "<b>HSR Distance (m):</b> %{y:.1f}",
                  "<extra></extra>"))%>% 
      add_trace(x = ~str_extract(athlete_name,"(?<=\\s).+$"), y = ~sprint_distance, name="Sprint Distance (m)", customdata=~paste0(athlete_name, "\n<b>Date:</b> ",format(date, "%b %d, %Y"), "\n<b>MD Code:</b> ", tag_name),
                type = "bar",color=I("#B2C9D4"), yaxis = "y2", 
                hovertemplate = paste0(
                  "<b>Player:</b> %{customdata}<br>",
                  "<b>Sprint Distance (m):</b> %{y:.1f}",
                  "<extra></extra>"))%>% 
      config(displaylogo = FALSE, scrollZoom = FALSE, displayModeBar = FALSE) %>%
      layout(
        xaxis = list(showline=TRUE,showgrid = FALSE, title=""),
        yaxis = list(showline=TRUE,showgrid = FALSE, title="Total Distance (m)"),
        yaxis2 = list(showline=TRUE,showgrid = FALSE,title="HSR/Sprint Distance (m)", overlaying = "y", automargin=T,side = "right"),
        legend = list(orientation = 'h',xanchor = "center", x = 0.5,y = -0.25),
        plot_bgcolor  = rgb(0,0,0,0),
        paper_bgcolor = rgb(0,0,0,0))
    
  })
  

  
  distance_drill_group_avg_plot <- reactive({
    
    shiny::validate(need(!is.null(input$athlete7), "Select one or more players"))
    
    
    shiny::validate(need(nrow(stats_period %>%
                                mutate(across(where(is.numeric), ~if_else(field_time == 0 & total_distance == 0, NA_real_, .x))) %>% 
                                dplyr::filter(athlete_name %in% input$athlete7 & period_name %in% input$period_input & date == input$date_input6) %>%
                                filter(!if_all(where(is.numeric), is.na))) > 0 ,"No Data"))
    
    distance_group_avg_stats <- stats_period %>%
      mutate(across(where(is.numeric), ~if_else(field_time == 0 & total_distance == 0, NA_real_, .x))) %>% 
      select(athlete_name | date | tag_name | period_name | total_distance) %>% 
      dplyr::filter(athlete_name %in% input$athlete7 & period_name %in% input$period_input & date == input$date_input6) %>%
      drop_na(c(tag_name, period_name)) %>% 
      group_by(date,tag_name, period_name) %>% 
      summarize(across(where(is.numeric), ~mean(.x,na.rm=T))) %>% 
      ungroup
    
    
    distance_group_avg_stats %>% 
      plot_ly() %>%
      add_trace(x = ~period_name, y = ~total_distance, type = "bar", customdata = ~paste0(format(date, "%b %d, %Y"), "\n<b>MD Code:</b> ", tag_name), 
                name = "Total Distance (m)", color=I("#00B0B9"),
                hovertemplate = paste0(
                  "<b>Date:</b> %{customdata}<br>",
                  "<b>Period:</b> %{x}<br>",
                  "<b>%{yaxis.title.text}:</b> %{y:.1f}",
                  "<extra></extra>"))%>% 
      config(displaylogo = FALSE, scrollZoom = FALSE, displayModeBar = FALSE) %>%
      layout(
        xaxis = list(showline=TRUE,showgrid = FALSE, title=""),
        yaxis = list(showline=TRUE,showgrid = FALSE,title="Total Distance (m)"),
        plot_bgcolor  = rgb(0,0,0,0),
        paper_bgcolor = rgb(0,0,0,0))
    
  })
  
  hs_distance_drill_group_avg_plot <- reactive({
    
    shiny::validate(need(!is.null(input$athlete7), "Select one or more players"))

    shiny::validate(need(nrow(stats_period %>%
      mutate(across(where(is.numeric), ~if_else(field_time == 0 & total_distance == 0, NA_real_, .x))) %>% 
      dplyr::filter(athlete_name %in% input$athlete7 & period_name %in% input$period_input & date == input$date_input6) %>%
      filter(!if_all(where(is.numeric), is.na))) > 0 ,"No Data"))
    
    hs_distance_group_avg_stats <- stats_period %>%
      mutate(across(where(is.numeric), ~if_else(field_time == 0 & total_distance == 0, NA_real_, .x))) %>% 
      select(athlete_name | date | tag_name | period_name | high_speed_distance | sprint_distance) %>% 
      dplyr::filter(athlete_name %in% input$athlete7 & period_name %in% input$period_input & date == input$date_input6) %>%
      drop_na(c(tag_name, period_name)) %>% 
      group_by(date,tag_name, period_name) %>% 
      summarize(across(where(is.numeric), ~mean(.x,na.rm=T))) %>% 
      ungroup
    
    hs_distance_group_avg_stats %>% 
      plot_ly() %>%
      add_trace(x = ~period_name, y = ~high_speed_distance, name="HSR Distance (m)", customdata = ~paste0(format(date, "%b %d, %Y"), "\n<b>MD Code:</b> ", tag_name), 
                type = "bar", color=I("#00B0B9"),
                hovertemplate = paste0(
                  "<b>Date:</b> %{customdata}<br>",
                  "<b>Period:</b> %{x}<br>",
                  "<b>HSR Distance (m):</b> %{y:.1f}",
                  "<extra></extra>"))%>% 
      add_trace(x = ~ period_name, y = ~sprint_distance, name="Sprint Distance (m)", customdata = ~paste0(format(date, "%b %d, %Y"), "\n<b>MD Code:</b> ", tag_name), 
                type = "bar", color=I("#B2C9D4"),
                hovertemplate = paste0(
                  "<b>Date:</b> %{customdata}<br>",
                  "<b>Period:</b> %{x}<br>",
                  "<b>Sprint Distance (m):</b> %{y:.1f}",
                  "<extra></extra>"))%>% 
      config(displaylogo = FALSE, scrollZoom = FALSE, displayModeBar = FALSE) %>%
      layout(
        xaxis = list(showline=TRUE,showgrid = FALSE, title=""),
        yaxis = list(showline=TRUE,showgrid = FALSE,title="Distance (m)"),
        legend = list(orientation = 'h',xanchor = "center", x = 0.5,y = -0.4),
        plot_bgcolor  = rgb(0,0,0,0),
        paper_bgcolor = rgb(0,0,0,0))
    
  })
  
  
  
  distance_drill_by_player_plot <- reactive({
    
    shiny::validate(need(!is.null(input$athlete7), "Select one or more players"))
    
    
    shiny::validate(need(nrow(stats_period %>%
                                mutate(across(where(is.numeric), ~if_else(field_time == 0 & total_distance == 0, NA_real_, .x))) %>% 
                                dplyr::filter(date == input$date_input6) %>%
                                filter(!if_all(where(is.numeric), is.na))) > 0 ,"No Data"))
    
    
    distance_by_player_stats <- stats_period %>%
      mutate(across(where(is.numeric), ~if_else(field_time == 0 & total_distance == 0, NA_real_, .x))) %>% 
      select(athlete_name | date | tag_name | period_name | total_distance | high_speed_distance | sprint_distance) %>% 
      dplyr::filter(athlete_name %in% input$athlete7 & date == input$date_input6 & period_name %in% input$period_input) %>% 
      group_by(athlete_name, date, tag_name) %>% 
      summarize(across(where(is.numeric), ~sum(.x,na.rm=T))) %>% 
      ungroup
    
    
    distance_by_player_stats %>% 
      plot_ly() %>%
      add_trace(x = ~str_extract(athlete_name,"(?<=\\s).+$"), y = ~total_distance, name = "Total Distance (m)", customdata=~paste0(athlete_name, "\n<b>Date:</b> ",format(date, "%b %d, %Y"), "\n<b>MD Code:</b> ", tag_name),
                type = "bar", color=I("#00B0B9"),
                hovertemplate = paste0(
                  "<b>Player:</b> %{customdata}<br>",
                  "<b>Total Distance (m):</b> %{y:.1f}",
                  "<extra></extra>"))%>% 
      add_trace(x = ~str_extract(athlete_name,"(?<=\\s).+$"), y = ~high_speed_distance, name="HSR Distance (m)", customdata=~paste0(athlete_name, "\n<b>Date:</b> ",format(date, "%b %d, %Y"), "\n<b>MD Code:</b> ", tag_name),
                type = "bar", color=I("#572C5F"),yaxis = "y2", 
                hovertemplate = paste0(
                  "<b>Player:</b> %{customdata}<br>",
                  "<b>HSR Distance (m):</b> %{y:.1f}",
                  "<extra></extra>"))%>% 
      add_trace(x = ~str_extract(athlete_name,"(?<=\\s).+$"), y = ~sprint_distance, name="Sprint Distance (m)", customdata=~paste0(athlete_name, "\n<b>Date:</b> ",format(date, "%b %d, %Y"), "\n<b>MD Code:</b> ", tag_name),
                type = "bar",color=I("#B2C9D4"), yaxis = "y2", 
                hovertemplate = paste0(
                  "<b>Player:</b> %{customdata}<br>",
                  "<b>Sprint Distance (m):</b> %{y:.1f}",
                  "<extra></extra>"))%>% 
      config(displaylogo = FALSE, scrollZoom = FALSE, displayModeBar = FALSE) %>%
      layout(
        xaxis = list(showline=TRUE,showgrid = FALSE, title=""),
        yaxis = list(showline=TRUE,showgrid = FALSE, title="Total Distance (m)"),
        yaxis2 = list(showline=TRUE,showgrid = FALSE,title="HSR/Sprint Distance (m)", overlaying = "y", automargin=T,side = "right"),
        legend = list(orientation = 'h',xanchor = "center", x = 0.5,y = -0.25),
        plot_bgcolor  = rgb(0,0,0,0),
        paper_bgcolor = rgb(0,0,0,0))
    
  })
  
  md_distance_per_half_plot <- reactive({
    
    shiny::validate(need(!is.null(input$athlete8), "Select one or more players"))
    
    md_distance_per_half <- stats_period %>% 
      dplyr::filter(athlete_name %in% input$athlete8 & activity_name == input$md_input & str_detect(period_name, "^\\d{1,2}\\. [[:alpha:]]{5,6} Half$")) %>%
      # dplyr::filter(activity_name == "18th May 2026 - MD 4  vs Vancouver (H)" & str_detect(period_name, "^\\d{1,2}\\. [[:alpha:]]{5,6} Half$")) %>%
      select(activity_name, date, athlete_name, period_name, field_time, total_distance, high_speed_distance, sprint_distance) %>% 
      mutate(across(where(is.numeric), ~if_else(field_time == 0 & total_distance == 0, NA_real_, .x)),
             period_name=str_remove(period_name, "\\d{1,2}\\.\\s")) %>% 
      group_by(activity_name, date, athlete_name, period_name) %>% 
      summarize(across(where(is.numeric), ~sum(.x,na.rm=T))) %>% 
      ungroup %>% 
      group_by(activity_name, date, period_name) %>% 
      summarize(across(where(is.numeric), ~sum(.x,na.rm=T))) %>% 
      ungroup 
    
    md_distance_per_half %>% 
      plot_ly() %>%
      add_trace(x = ~period_name, y = ~total_distance, name = "Total Distance (m)", customdata=~activity_name,
                type = "bar", color=I("#00B0B9"),
                width = 0.2, 
                alignmentgroup = 'true', 
                offsetgroup = '1',
                hovertemplate = paste0(
                  "<b>Match:</b> %{customdata}<br>",
                  "<b>Period:</b> %{x}<br>",
                  "<b>Total Distance (m):</b> %{y:.1f}",
                "<extra></extra>"))%>% 
    add_trace(x = ~period_name, y = ~high_speed_distance, name="HSR Distance (m)", customdata=~activity_name,
              type = "bar", color=I("#572C5F"),yaxis = "y2", 
              width = 0.2, 
              alignmentgroup = 'true', 
              offsetgroup = '2',
              hovertemplate = paste0(
                "<b>Match:</b> %{customdata}<br>",
                "<b>Period:</b> %{x}<br>",
                "<b>HSR Distance (m):</b> %{y:.1f}",
                "<extra></extra>"))%>% 
    add_trace(x = ~period_name, y = ~sprint_distance, name="Sprint Distance (m)", customdata=~activity_name,
              type = "bar",color=I("#B2C9D4"), yaxis = "y2", 
              width = 0.2, 
              alignmentgroup = 'true', 
              offsetgroup = '3',
              hovertemplate = paste0(
                "<b>Match:</b> %{customdata}<br>",
                "<b>Period:</b> %{x}<br>",
                "<b>Sprint Distance (m):</b> %{y:.1f}",
                "<extra></extra>"))%>% 
    config(displaylogo = FALSE, scrollZoom = FALSE, displayModeBar = FALSE) %>%
    layout(
      barmode = 'group',
      bargap = 0.3,
      bargroupgap = 0.2,
      xaxis = list(showline=TRUE,showgrid = FALSE, title=""),
      yaxis = list(showline=TRUE,showgrid = FALSE, title="Total Distance (m)"),
      yaxis2 = list(showline=TRUE,showgrid = FALSE,title="HSR/Sprint Distance (m)", overlaying = "y", automargin=T,side = "right"),
      legend = list(orientation = 'h',xanchor = "center", x = 0.5,y = -0.1),
      plot_bgcolor  = rgb(0,0,0,0),
      paper_bgcolor = rgb(0,0,0,0))
  
  })
  
  md_distance_by_player <- reactive({

    stats_period %>%
    dplyr::filter(athlete_name %in% input$athlete8 & activity_name == input$md_input & str_detect(period_name, "^\\d{1,2}\\. [[:alpha:]]{5,6} Half$")) %>%
    # dplyr::filter(activity_name == "18th May 2026 - MD 4  vs Vancouver (H)" & str_detect(period_name, "^\\d{1,2}\\. [[:alpha:]]{5,6} Half$")) %>%
    select(activity_name, date, athlete_name, period_name, field_time, total_distance, high_speed_distance, sprint_distance) %>%
    mutate(across(where(is.numeric), ~if_else(field_time == 0 & total_distance == 0, NA_real_, .x)),
           period_name=str_remove(period_name, "\\d{1,2}\\.\\s")) %>%
    group_by(activity_name, date, athlete_name, period_name) %>%
    summarize(across(where(is.numeric), ~sum(.x,na.rm=T))) %>%
    ungroup %>% 
    mutate(across(where(is.numeric) & !field_time, ~.x/(field_time/60), .names="{.col}_per_min"))

  })
  
  
  md_total_distance_by_player_plot <- reactive({
    
    shiny::validate(need(!is.null(input$athlete8), "Select one or more players"))
    
    # md_distance_by_player <- stats_period %>% 
    #   dplyr::filter(athlete_name %in% input$athlete8 & activity_name == input$md_input & str_detect(period_name, "^\\d{1,2}\\. [[:alpha:]]{5,6} Half$")) %>%
    #   # dplyr::filter(activity_name == "18th May 2026 - MD 4  vs Vancouver (H)" & str_detect(period_name, "^\\d{1,2}\\. [[:alpha:]]{5,6} Half$")) %>%
    #   select(activity_name, date, athlete_name, period_name, field_time, total_distance, high_speed_distance, sprint_distance) %>% 
    #   mutate(across(where(is.numeric), ~if_else(field_time == 0 & total_distance == 0, NA_real_, .x)),
    #          period_name=str_remove(period_name, "\\d{1,2}\\.\\s")) %>% 
    #   group_by(activity_name, date, athlete_name, period_name) %>% 
    #   summarize(across(where(is.numeric), ~sum(.x,na.rm=T))) %>% 
    #   ungroup 
    
    md_distance_by_player <- md_distance_by_player()
    
    
    md_distance_by_player %>% 
      plot_ly() %>%
      add_trace(x = ~str_extract(athlete_name,"(?<=\\s).+$"), y = ~total_distance, customdata=~paste0(activity_name,"\n<b>Period:</b> ", period_name),
                type = "bar", color=~period_name, colors=c("#00B0B9","#572C5F"),
                hovertemplate = paste0(
                  "<b>Match:</b> %{customdata}<br>",
                  "<b>Total Distance (m):</b> %{y:.1f}",
                  "<extra></extra>"))%>% 
      config(displaylogo = FALSE, scrollZoom = FALSE, displayModeBar = FALSE) %>%
      layout(
        barmode = "stack",
        xaxis = list(showline=TRUE,showgrid = FALSE, title=""),
        yaxis = list(showline=TRUE,showgrid = FALSE, title="Total Distance (m)"),        
        legend = list(orientation = 'h',xanchor = "center", x = 0.5,y = -0.25),
        plot_bgcolor  = rgb(0,0,0,0),
        paper_bgcolor = rgb(0,0,0,0))
    
  })
  
  
  md_hsr_distance_by_player_plot <- reactive({
    
    shiny::validate(need(!is.null(input$athlete8), "Select one or more players"))
    
    md_distance_by_player <- md_distance_by_player()
    
    
    md_distance_by_player %>% 
      plot_ly() %>%
      add_trace(x = ~str_extract(athlete_name,"(?<=\\s).+$"), y = ~high_speed_distance, customdata=~paste0(activity_name,"\n<b>Period:</b> ", period_name),
                type = "bar", color=~period_name, colors=c("#00B0B9","#572C5F"),
                hovertemplate = paste0(
                  "<b>Match:</b> %{customdata}<br>",
                  "<b>HSR Distance (m):</b> %{y:.1f}",
                  "<extra></extra>"))%>% 
      config(displaylogo = FALSE, scrollZoom = FALSE, displayModeBar = FALSE) %>%
      layout(
        barmode = "stack",
        xaxis = list(showline=TRUE,showgrid = FALSE, title=""),
        yaxis = list(showline=TRUE,showgrid = FALSE, title="HSR Distance (m)"),        
        legend = list(orientation = 'h',xanchor = "center", x = 0.5,y = -0.25),
        plot_bgcolor  = rgb(0,0,0,0),
        paper_bgcolor = rgb(0,0,0,0))
    
  })
  
  md_sprint_distance_by_player_plot <- reactive({
    
    shiny::validate(need(!is.null(input$athlete8), "Select one or more players"))
    
    
    md_distance_by_player <- md_distance_by_player()
    
    
    md_distance_by_player %>% 
      plot_ly() %>%
      add_trace(x = ~str_extract(athlete_name,"(?<=\\s).+$"), y = ~sprint_distance, customdata=~paste0(activity_name,"\n<b>Period:</b> ", period_name),
                type = "bar", color=~period_name, colors=c("#00B0B9","#572C5F"),
                hovertemplate = paste0(
                  "<b>Match:</b> %{customdata}<br>",
                  "<b>Sprint Distance (m):</b> %{y:.1f}",
                  "<extra></extra>"))%>% 
      config(displaylogo = FALSE, scrollZoom = FALSE, displayModeBar = FALSE) %>%
      layout(
        barmode = "stack",
        xaxis = list(showline=TRUE,showgrid = FALSE, title=""),
        yaxis = list(showline=TRUE,showgrid = FALSE, title="Sprint Distance (m)"),        
        legend = list(orientation = 'h',xanchor = "center", x = 0.5,y = -0.25),
        plot_bgcolor  = rgb(0,0,0,0),
        paper_bgcolor = rgb(0,0,0,0))
    
  })
  
  
  md_total_distance_per_min_by_player_plot <- reactive({
    
    shiny::validate(need(!is.null(input$athlete8), "Select one or more players"))
  
    
    md_distance_by_player <- md_distance_by_player()
    
    
    md_distance_by_player %>% 
      plot_ly() %>%
      add_trace(x = ~str_extract(athlete_name,"(?<=\\s).+$"), y = ~total_distance_per_min, customdata=~paste0(activity_name,"\n<b>Period:</b> ", period_name),
                type = "bar", color=~period_name, colors=c("#00B0B9","#572C5F"),
                hovertemplate = paste0(
                  "<b>Match:</b> %{customdata}<br>",
                  "<b>Total Distance Per Min (m/min):</b> %{y:.1f}",
                  "<extra></extra>"))%>% 
      config(displaylogo = FALSE, scrollZoom = FALSE, displayModeBar = FALSE) %>%
      layout(
        barmode = "group",
        xaxis = list(showline=TRUE,showgrid = FALSE, title=""),
        yaxis = list(showline=TRUE,showgrid = FALSE, title="Total Distance Per Min (m/min)"),        
        legend = list(orientation = 'h',xanchor = "center", x = 0.5,y = -0.25),
        plot_bgcolor  = rgb(0,0,0,0),
        paper_bgcolor = rgb(0,0,0,0))
    
  })
  
  
  md_hsr_distance_per_min_by_player_plot <- reactive({
    
    shiny::validate(need(!is.null(input$athlete8), "Select one or more players"))
    
    md_distance_by_player <- md_distance_by_player()
    
    
    md_distance_by_player %>% 
      plot_ly() %>%
      add_trace(x = ~str_extract(athlete_name,"(?<=\\s).+$"), y = ~high_speed_distance_per_min, customdata=~paste0(activity_name,"\n<b>Period:</b> ", period_name),
                type = "bar", color=~period_name, colors=c("#00B0B9","#572C5F"),
                hovertemplate = paste0(
                  "<b>Match:</b> %{customdata}<br>",
                  "<b>HSR Distance Per Min (m/min):</b> %{y:.1f}",
                  "<extra></extra>"))%>% 
      config(displaylogo = FALSE, scrollZoom = FALSE, displayModeBar = FALSE) %>%
      layout(
        barmode = "group",
        xaxis = list(showline=TRUE,showgrid = FALSE, title=""),
        yaxis = list(showline=TRUE,showgrid = FALSE, title="HSR Distance Per Min (m/min)"),        
        legend = list(orientation = 'h',xanchor = "center", x = 0.5,y = -0.25),
        plot_bgcolor  = rgb(0,0,0,0),
        paper_bgcolor = rgb(0,0,0,0))
    
  })
  
  md_sprint_distance_per_min_by_player_plot <- reactive({
    
    shiny::validate(need(!is.null(input$athlete8), "Select one or more players"))
    
    
    md_distance_by_player <- md_distance_by_player()
    
    
    md_distance_by_player %>% 
      plot_ly() %>%
      add_trace(x = ~str_extract(athlete_name,"(?<=\\s).+$"), y = ~sprint_distance_per_min, customdata=~paste0(activity_name,"\n<b>Period:</b> ", period_name),
                type = "bar", color=~period_name, colors=c("#00B0B9","#572C5F"),
                hovertemplate = paste0(
                  "<b>Match:</b> %{customdata}<br>",
                  "<b>Sprint Distance Per Min (m/min):</b> %{y:.1f}",
                  "<extra></extra>"))%>% 
      config(displaylogo = FALSE, scrollZoom = FALSE, displayModeBar = FALSE) %>%
      layout(
        barmode = "group",
        xaxis = list(showline=TRUE,showgrid = FALSE, title=""),
        yaxis = list(showline=TRUE,showgrid = FALSE, title="Sprint Distance Per Min (m/min)"),        
        legend = list(orientation = 'h',xanchor = "center", x = 0.5,y = -0.25),
        plot_bgcolor  = rgb(0,0,0,0),
        paper_bgcolor = rgb(0,0,0,0))
    
  })
  
  md_distance_team_total <- reactive({
    
    stats_period %>%
      dplyr::filter(athlete_name %in% input$athlete8 & tag_name == "MD" & date >= as.Date("2026-04-01") & date <= (stats_period %>% filter(activity_name == input$md_input) %>% pull(date) %>% unique) & str_detect(period_name, "^\\d{1,2}\\. [[:alpha:]]{5,6} Half$")) %>%
      # dplyr::filter(tag_name == "MD" & date >= as.Date("2026-04-01") & date <= (stats_period %>% filter(activity_name ==  "24th May 2026 - MD 5 vs Calgary (A)") %>% pull(date) %>% unique) & str_detect(period_name, "^\\d{1,2}\\. [[:alpha:]]{5,6} Half$")) %>%
      select(activity_name, date, athlete_name, period_name, field_time, total_distance, high_speed_distance, sprint_distance) %>%
      mutate(across(where(is.numeric), ~if_else(field_time == 0 & total_distance == 0, NA_real_, .x))) %>%
      group_by(activity_name, date) %>%
      summarize(across(where(is.numeric), ~sum(.x,na.rm=T))) %>%
      ungroup %>% 
      mutate(across(where(is.numeric) & !field_time, ~.x/(field_time/60), .names="{.col}_per_min"),
             logo = case_when(str_detect(activity_name, "Halifax") ~ "https://upload.wikimedia.org/wikipedia/en/thumb/3/37/Halifax_Tides_FC.svg/1280px-Halifax_Tides_FC.svg.png",
                               str_detect(activity_name, "Montreal") ~ "https://upload.wikimedia.org/wikipedia/en/thumb/3/38/Montreal_Roses_FC.svg/1280px-Montreal_Roses_FC.svg.png",
                                 str_detect(activity_name, "Ottawa") ~ "https://upload.wikimedia.org/wikipedia/en/thumb/f/f2/Ottawa_Rapid_FC.svg/1280px-Ottawa_Rapid_FC.svg.png",
                                 str_detect(activity_name, "Toronto") ~ "https://upload.wikimedia.org/wikipedia/commons/thumb/1/15/AFC_Toronto_logo.svg/1280px-AFC_Toronto_logo.svg.png",
                                 str_detect(activity_name, "Calgary") ~ "https://upload.wikimedia.org/wikipedia/commons/thumb/a/a5/Calgary_Wild_FC_logo.svg/1280px-Calgary_Wild_FC_logo.svg.png",
                                 str_detect(activity_name, "Vancouver") ~ "https://upload.wikimedia.org/wikipedia/en/thumb/d/d2/Vancouver_Rise_FC.svg/1280px-Vancouver_Rise_FC.svg.png",
                                 str_detect(activity_name, "Portsmouth") ~ "https://upload.wikimedia.org/wikipedia/en/thumb/3/38/Portsmouth_FC_logo.svg/1280px-Portsmouth_FC_logo.svg.png",
                                 str_detect(activity_name, "Everton") ~ "https://upload.wikimedia.org/wikipedia/en/thumb/7/7c/Everton_FC_logo.svg/1280px-Everton_FC_logo.svg.png",
                                str_detect(activity_name, "West Ham") ~ "https://upload.wikimedia.org/wikipedia/en/thumb/c/c2/West_Ham_United_FC_logo.svg/1280px-West_Ham_United_FC_logo.svg.png",
                              str_detect(activity_name, "AUS") ~ "https://upload.wikimedia.org/wikipedia/en/thumb/0/0b/Atlantic_University_Sport_Logo.svg/1280px-Atlantic_University_Sport_Logo.svg.png")) %>% 
      arrange(date) %>% 
      mutate(date=as.character(date))
  
    })
  
  md_distance_desc <- reactive({
    
    md_distance_team_total() %>% 
      mutate(across(where(is.numeric), ~mean(.x), .names="{.col}_mean"), 
             across(where(is.numeric), ~sd(.x), .names="{.col}_sd")) %>% 
      select(contains("mean") | contains("sd")) %>% 
      unique
  })
  
  md_comparison_total_distance <- reactive({
    
    shiny::validate(need(!is.null(input$athlete8), "Select one or more players"))
  
    md_distance_team_total <- md_distance_team_total()
    
    md_distance_desc <- md_distance_desc()
    
    range_x <- length(unique(md_distance_team_total$activity_name))+1
    
    max_y <- if_else(max(md_distance_team_total$total_distance) > (md_distance_desc$total_distance_mean+(2*md_distance_desc$total_distance_sd)),
                     max(md_distance_team_total$total_distance),
                     md_distance_desc$total_distance_mean+(2*md_distance_desc$total_distance_sd)) 
    min_y <- if_else(min(md_distance_team_total$total_distance) < (md_distance_desc$total_distance_mean-(2*md_distance_desc$total_distance_sd)),
                     min(md_distance_team_total$total_distance),
                     md_distance_desc$total_distance_mean-(2*md_distance_desc$total_distance_sd))
    
    range_y <- max_y-min_y

    
    image_list <- pmap(md_distance_team_total %>% select(date,total_distance,logo), function(date, total_distance, logo) {
      list(
        source = logo,
        xref = "x",        # Aligns the image horizontally with the x-axis scale
        yref = "y",        # Aligns the image vertically with the y-axis scale
        x = date,             # Horizontal center position (matches the data point)
        y = total_distance,             # Vertical center position (matches the data point)
        sizex = range_x*0.1,       # Width of the image in data units
        sizey = range_y*0.1,       # Height of the image in data units
        xanchor = "center",# Centers the image horizontally on the coordinate
        yanchor = "middle",# Centers the image vertically on the coordinate
        opacity = 1,     # Image opacity
        layer = "above"    # Forces the logo to render on top of the grid lines
      )
    })
    
md_distance_team_total %>% 
      plot_ly() %>%
      add_trace(x = ~as.character(date), y = ~total_distance, customdata=~activity_name,
                type = "scatter", mode = 'lines', line=list(color="#572C5F"),
                hovertemplate = paste0(
                  "<b>Match:</b> %{customdata}<br>",
                  "<b>Total Distance (m):</b> %{y:.1f}",
                  "<extra></extra>"))%>% 
      config(displaylogo = FALSE, scrollZoom = FALSE, displayModeBar = FALSE) %>%
      layout(
        xaxis = list(range = c(-0.5,range_x-1.5), showline=TRUE,showgrid = FALSE,showticklabels = FALSE,title=""),
        yaxis = list(showline=TRUE,showgrid = FALSE, title="Total Distance (m)"),        
        plot_bgcolor  = rgb(0,0,0,0),
        paper_bgcolor = rgb(0,0,0,0),
        shapes = list(
          list(type = "line", layer = "below", x0 = 0, x1 = 1, xref = "paper",  y0 = md_distance_desc$total_distance_mean, y1 = md_distance_desc$total_distance_mean, yref = "y", line = list(color = "red", dash = "dash")),
          list(type = "line", layer = "below", x0 = 0, x1 = 1, xref = "paper",  y0 = md_distance_desc$total_distance_mean+(2*md_distance_desc$total_distance_sd), y1 =  md_distance_desc$total_distance_mean+(2*md_distance_desc$total_distance_sd), yref = "y", line = list(color = "blue", dash = "dash")),
          list(type = "line", layer = "below", x0 = 0, x1 = 1, xref = "paper",  y0 =  md_distance_desc$total_distance_mean-(2*md_distance_desc$total_distance_sd), y1 =  md_distance_desc$total_distance_mean-(2*md_distance_desc$total_distance_sd), yref = "y", line = list(color = "blue", dash = "dash"))
          ),
        images=image_list
        )
    
  })

  md_comparison_hsr_distance <- reactive({
    
    shiny::validate(need(!is.null(input$athlete8), "Select one or more players"))
    
    md_distance_team_total <- md_distance_team_total()
    
    md_distance_desc <- md_distance_desc()
    
    range_x <- length(unique(md_distance_team_total$activity_name))+1
    
    max_y <- if_else(max(md_distance_team_total$high_speed_distance) > (md_distance_desc$high_speed_distance_mean+(2*md_distance_desc$high_speed_distance_sd)),
                     max(md_distance_team_total$high_speed_distance),
                     md_distance_desc$high_speed_distance_mean+(2*md_distance_desc$high_speed_distance_sd)) 
    min_y <- if_else(min(md_distance_team_total$high_speed_distance) < (md_distance_desc$high_speed_distance_mean-(2*md_distance_desc$high_speed_distance_sd)),
                     min(md_distance_team_total$high_speed_distance),
                     md_distance_desc$high_speed_distance_mean-(2*md_distance_desc$high_speed_distance_sd))
    
    range_y <- max_y-min_y
    
    image_list <- pmap(md_distance_team_total %>% select(date,high_speed_distance,logo), function(date, high_speed_distance, logo) {
      list(
        source = logo,
        xref = "x",        # Aligns the image horizontally with the x-axis scale
        yref = "y",        # Aligns the image vertically with the y-axis scale
        x = date,             # Horizontal center position (matches the data point)
        y = high_speed_distance,             # Vertical center position (matches the data point)
        sizex = range_x*0.1,       # Width of the image in data units
        sizey = range_y*0.1,       # Height of the image in data units
        xanchor = "center",# Centers the image horizontally on the coordinate
        yanchor = "middle",# Centers the image vertically on the coordinate
        opacity = 1,     # Image opacity
        layer = "above"    # Forces the logo to render on top of the grid lines
      )
    })
    
    md_distance_team_total %>% 
      plot_ly() %>%
      add_trace(x = ~as.character(date), y = ~high_speed_distance, customdata=~activity_name,
                type = "scatter", mode = 'lines', line=list(color="#572C5F"),
                hovertemplate = paste0(
                  "<b>Match:</b> %{customdata}<br>",
                  "<b>HSR Distance (m):</b> %{y:.1f}",
                  "<extra></extra>"))%>% 
      config(displaylogo = FALSE, scrollZoom = FALSE, displayModeBar = FALSE) %>%
      layout(
        xaxis = list(range = c(-0.5,range_x-1.5), showline=TRUE,showgrid = FALSE,showticklabels = FALSE,title=""),
        yaxis = list(showline=TRUE,showgrid = FALSE, title="HSR Distance (m)"),        
        plot_bgcolor  = rgb(0,0,0,0),
        paper_bgcolor = rgb(0,0,0,0),
        shapes = list(
          list(type = "line", layer = "below", x0 = 0, x1 = 1, xref = "paper",  y0 = md_distance_desc$high_speed_distance_mean, y1 = md_distance_desc$high_speed_distance_mean, yref = "y", line = list(color = "red", dash = "dash")),
          list(type = "line", layer = "below", x0 = 0, x1 = 1, xref = "paper",  y0 = md_distance_desc$high_speed_distance_mean+(2*md_distance_desc$high_speed_distance_sd), y1 =  md_distance_desc$high_speed_distance_mean+(2*md_distance_desc$high_speed_distance_sd), yref = "y", line = list(color = "blue", dash = "dash")),
          list(type = "line", layer = "below", x0 = 0, x1 = 1, xref = "paper",  y0 =  md_distance_desc$high_speed_distance_mean-(2*md_distance_desc$high_speed_distance_sd), y1 =  md_distance_desc$high_speed_distance_mean-(2*md_distance_desc$high_speed_distance_sd), yref = "y", line = list(color = "blue", dash = "dash"))
        ),
        images=image_list
      )
    
  })
  
  md_comparison_sprint_distance <- reactive({
    
    shiny::validate(need(!is.null(input$athlete8), "Select one or more players"))
    
    md_distance_team_total <- md_distance_team_total()
    
    md_distance_desc <- md_distance_desc()
    
    range_x <- length(unique(md_distance_team_total$activity_name))+1
    
    max_y <- if_else(max(md_distance_team_total$sprint_distance) > (md_distance_desc$sprint_distance_mean+(2*md_distance_desc$sprint_distance_sd)),
                     max(md_distance_team_total$sprint_distance),
                     md_distance_desc$sprint_distance_mean+(2*md_distance_desc$sprint_distance_sd)) 
    min_y <- if_else(min(md_distance_team_total$sprint_distance) < (md_distance_desc$sprint_distance_mean-(2*md_distance_desc$sprint_distance_sd)),
                     min(md_distance_team_total$sprint_distance),
                     md_distance_desc$sprint_distance_mean-(2*md_distance_desc$sprint_distance_sd))
    
    range_y <- max_y-min_y
    
    image_list <- pmap(md_distance_team_total %>% select(date,sprint_distance,logo), function(date, sprint_distance, logo) {
      list(
        source = logo,
        xref = "x",        # Aligns the image horizontally with the x-axis scale
        yref = "y",        # Aligns the image vertically with the y-axis scale
        x = date,             # Horizontal center position (matches the data point)
        y = sprint_distance,             # Vertical center position (matches the data point)
        sizex = range_x*0.1,       # Width of the image in data units
        sizey = range_y*0.1,       # Height of the image in data units
        xanchor = "center",# Centers the image horizontally on the coordinate
        yanchor = "middle",# Centers the image vertically on the coordinate
        opacity = 1,     # Image opacity
        layer = "above"    # Forces the logo to render on top of the grid lines
      )
    })
    
    md_distance_team_total %>% 
      plot_ly() %>%
      add_trace(x = ~as.character(date), y = ~sprint_distance, customdata=~activity_name,
                type = "scatter", mode = 'lines', line=list(color="#572C5F"),
                hovertemplate = paste0(
                  "<b>Match:</b> %{customdata}<br>",
                  "<b>Sprint Distance (m):</b> %{y:.1f}",
                  "<extra></extra>"))%>% 
      config(displaylogo = FALSE, scrollZoom = FALSE, displayModeBar = FALSE) %>%
      layout(
        xaxis = list(range = c(-0.5,range_x-1.5), showline=TRUE,showgrid = FALSE,showticklabels = FALSE,title=""),
        yaxis = list(showline=TRUE,showgrid = FALSE, title="Sprint Distance (m)"),        
        plot_bgcolor  = rgb(0,0,0,0),
        paper_bgcolor = rgb(0,0,0,0),
        shapes = list(
          list(type = "line", layer = "below", x0 = 0, x1 = 1, xref = "paper",  y0 = md_distance_desc$sprint_distance_mean, y1 = md_distance_desc$sprint_distance_mean, yref = "y", line = list(color = "red", dash = "dash")),
          list(type = "line", layer = "below", x0 = 0, x1 = 1, xref = "paper",  y0 = md_distance_desc$sprint_distance_mean+(2*md_distance_desc$sprint_distance_sd), y1 =  md_distance_desc$sprint_distance_mean+(2*md_distance_desc$sprint_distance_sd), yref = "y", line = list(color = "blue", dash = "dash")),
          list(type = "line", layer = "below", x0 = 0, x1 = 1, xref = "paper",  y0 =  md_distance_desc$sprint_distance_mean-(2*md_distance_desc$sprint_distance_sd), y1 =  md_distance_desc$sprint_distance_mean-(2*md_distance_desc$sprint_distance_sd), yref = "y", line = list(color = "blue", dash = "dash"))
        ),
        images=image_list
      )
    
  })
  
  
  
  md_comparison_total_distance_per_min <- reactive({
    
    shiny::validate(need(!is.null(input$athlete8), "Select one or more players"))
    
    md_distance_team_total <- md_distance_team_total()
    
    md_distance_desc <- md_distance_desc()
    
    range_x <- length(unique(md_distance_team_total$activity_name))+1
    
    max_y <- if_else(max(md_distance_team_total$total_distance_per_min) > (md_distance_desc$total_distance_per_min_mean+(2*md_distance_desc$total_distance_per_min_sd)),
                     max(md_distance_team_total$total_distance_per_min),
                     md_distance_desc$total_distance_per_min_mean+(2*md_distance_desc$total_distance_per_min_sd)) 
    min_y <- if_else(min(md_distance_team_total$total_distance_per_min) < (md_distance_desc$total_distance_per_min_mean-(2*md_distance_desc$total_distance_per_min_sd)),
                     min(md_distance_team_total$total_distance_per_min),
                     md_distance_desc$total_distance_per_min_mean-(2*md_distance_desc$total_distance_per_min_sd))
    
    range_y <- max_y-min_y
    
    image_list <- pmap(md_distance_team_total %>% select(date,total_distance_per_min,logo), function(date, total_distance_per_min, logo) {
      list(
        source = logo,
        xref = "x",        # Aligns the image horizontally with the x-axis scale
        yref = "y",        # Aligns the image vertically with the y-axis scale
        x = date,             # Horizontal center position (matches the data point)
        y = total_distance_per_min,             # Vertical center position (matches the data point)
        sizex = range_x*0.1,       # Width of the image in data units
        sizey = range_y*0.1,       # Height of the image in data units
        xanchor = "center",# Centers the image horizontally on the coordinate
        yanchor = "middle",# Centers the image vertically on the coordinate
        opacity = 1,     # Image opacity
        layer = "above"    # Forces the logo to render on top of the grid lines
      )
    })
    
    md_distance_team_total %>% 
      plot_ly() %>%
      add_trace(x = ~as.character(date), y = ~total_distance_per_min, customdata=~activity_name,
                type = "scatter", mode = 'lines', line=list(color="#572C5F"),
                hovertemplate = paste0(
                  "<b>Match:</b> %{customdata}<br>",
                  "<b>Total Distance Per Min (m/min):</b> %{y:.1f}",
                  "<extra></extra>"))%>% 
      config(displaylogo = FALSE, scrollZoom = FALSE, displayModeBar = FALSE) %>%
      layout(
        xaxis = list(range = c(-0.5,range_x-1.5), showline=TRUE,showgrid = FALSE,showticklabels = FALSE,title=""),
        yaxis = list(showline=TRUE,showgrid = FALSE, title="Total Distance Per Min (m/min)"),        
        plot_bgcolor  = rgb(0,0,0,0),
        paper_bgcolor = rgb(0,0,0,0),
        shapes = list(
          list(type = "line", layer = "below", x0 = 0, x1 = 1, xref = "paper",  y0 = md_distance_desc$total_distance_per_min_mean, y1 = md_distance_desc$total_distance_per_min_mean, yref = "y", line = list(color = "red", dash = "dash")),
          list(type = "line", layer = "below", x0 = 0, x1 = 1, xref = "paper",  y0 = md_distance_desc$total_distance_per_min_mean+(2*md_distance_desc$total_distance_per_min_sd), y1 =  md_distance_desc$total_distance_per_min_mean+(2*md_distance_desc$total_distance_per_min_sd), yref = "y", line = list(color = "blue", dash = "dash")),
          list(type = "line", layer = "below", x0 = 0, x1 = 1, xref = "paper",  y0 =  md_distance_desc$total_distance_per_min_mean-(2*md_distance_desc$total_distance_per_min_sd), y1 =  md_distance_desc$total_distance_per_min_mean-(2*md_distance_desc$total_distance_per_min_sd), yref = "y", line = list(color = "blue", dash = "dash"))
        ),
        images=image_list
      )
    
  })
  
  md_comparison_hsr_distance_per_min <- reactive({
    
    shiny::validate(need(!is.null(input$athlete8), "Select one or more players"))
    
    md_distance_team_total <- md_distance_team_total()
    
    md_distance_desc <- md_distance_desc()
    
    range_x <- length(unique(md_distance_team_total$activity_name))+1
    
    max_y <- if_else(max(md_distance_team_total$high_speed_distance_per_min) > (md_distance_desc$high_speed_distance_per_min_mean+(2*md_distance_desc$high_speed_distance_per_min_sd)),
                     max(md_distance_team_total$high_speed_distance_per_min),
                     md_distance_desc$high_speed_distance_per_min_mean+(2*md_distance_desc$high_speed_distance_per_min_sd)) 
    min_y <- if_else(min(md_distance_team_total$high_speed_distance_per_min) < (md_distance_desc$high_speed_distance_per_min_mean-(2*md_distance_desc$high_speed_distance_per_min_sd)),
                     min(md_distance_team_total$high_speed_distance_per_min),
                     md_distance_desc$high_speed_distance_per_min_mean-(2*md_distance_desc$high_speed_distance_per_min_sd))
    
    range_y <- max_y-min_y
    
    image_list <- pmap(md_distance_team_total %>% select(date,high_speed_distance_per_min,logo), function(date, high_speed_distance_per_min, logo) {
      list(
        source = logo,
        xref = "x",        # Aligns the image horizontally with the x-axis scale
        yref = "y",        # Aligns the image vertically with the y-axis scale
        x = date,             # Horizontal center position (matches the data point)
        y = high_speed_distance_per_min,             # Vertical center position (matches the data point)
        sizex = range_x*0.1,       # Width of the image in data units
        sizey = range_y*0.1,       # Height of the image in data units
        xanchor = "center",# Centers the image horizontally on the coordinate
        yanchor = "middle",# Centers the image vertically on the coordinate
        opacity = 1,     # Image opacity
        layer = "above"    # Forces the logo to render on top of the grid lines
      )
    })
    
    md_distance_team_total %>% 
      plot_ly() %>%
      add_trace(x = ~as.character(date), y = ~high_speed_distance_per_min, customdata=~activity_name,
                type = "scatter", mode = 'lines', line=list(color="#572C5F"),
                hovertemplate = paste0(
                  "<b>Match:</b> %{customdata}<br>",
                  "<b>HSR Distance Per min (m/min):</b> %{y:.1f}",
                  "<extra></extra>"))%>% 
      config(displaylogo = FALSE, scrollZoom = FALSE, displayModeBar = FALSE) %>%
      layout(
        xaxis = list(range = c(-0.5,range_x-1.5), showline=TRUE,showgrid = FALSE,showticklabels = FALSE,title=""),
        yaxis = list(showline=TRUE,showgrid = FALSE, title="HSR Distance Per Min (m/min)"),        
        plot_bgcolor  = rgb(0,0,0,0),
        paper_bgcolor = rgb(0,0,0,0),
        shapes = list(
          list(type = "line", layer = "below", x0 = 0, x1 = 1, xref = "paper",  y0 = md_distance_desc$high_speed_distance_per_min_mean, y1 = md_distance_desc$high_speed_distance_per_min_mean, yref = "y", line = list(color = "red", dash = "dash")),
          list(type = "line", layer = "below", x0 = 0, x1 = 1, xref = "paper",  y0 = md_distance_desc$high_speed_distance_per_min_mean+(2*md_distance_desc$high_speed_distance_per_min_sd), y1 =  md_distance_desc$high_speed_distance_per_min_mean+(2*md_distance_desc$high_speed_distance_per_min_sd), yref = "y", line = list(color = "blue", dash = "dash")),
          list(type = "line", layer = "below", x0 = 0, x1 = 1, xref = "paper",  y0 =  md_distance_desc$high_speed_distance_per_min_mean-(2*md_distance_desc$high_speed_distance_per_min_sd), y1 =  md_distance_desc$high_speed_distance_per_min_mean-(2*md_distance_desc$high_speed_distance_per_min_sd), yref = "y", line = list(color = "blue", dash = "dash"))
        ),
        images=image_list
      )
    
  })
  
  md_comparison_sprint_distance_per_min <- reactive({
    
    shiny::validate(need(!is.null(input$athlete8), "Select one or more players"))
    
    md_distance_team_total <- md_distance_team_total()
    
    md_distance_desc <- md_distance_desc()
    
    range_x <- length(unique(md_distance_team_total$activity_name))+1
    
    
    max_y <- if_else(max(md_distance_team_total$sprint_distance_per_min) > (md_distance_desc$sprint_distance_per_min_mean+(2*md_distance_desc$sprint_distance_per_min_sd)),
                     max(md_distance_team_total$sprint_distance_per_min),
                     md_distance_desc$sprint_distance_per_min_mean+(2*md_distance_desc$sprint_distance_per_min_sd)) 
    min_y <- if_else(min(md_distance_team_total$sprint_distance_per_min) < (md_distance_desc$sprint_distance_per_min_mean-(2*md_distance_desc$sprint_distance_per_min_sd)),
                     min(md_distance_team_total$sprint_distance_per_min),
                     md_distance_desc$sprint_distance_per_min_mean-(2*md_distance_desc$sprint_distance_per_min_sd))
    
    range_y <- max_y-min_y    
    
    image_list <- pmap(md_distance_team_total %>% select(date,sprint_distance_per_min,logo), function(date, sprint_distance_per_min, logo) {
      list(
        source = logo,
        xref = "x",        # Aligns the image horizontally with the x-axis scale
        yref = "y",        # Aligns the image vertically with the y-axis scale
        x = date,             # Horizontal center position (matches the data point)
        y = sprint_distance_per_min,             # Vertical center position (matches the data point)
        sizex = range_x*0.1,       # Width of the image in data units
        sizey = range_y*0.1,       # Height of the image in data units
        xanchor = "center",# Centers the image horizontally on the coordinate
        yanchor = "middle",# Centers the image vertically on the coordinate
        opacity = 1,     # Image opacity
        layer = "above"    # Forces the logo to render on top of the grid lines
      )
    })
    
    md_distance_team_total %>% 
      plot_ly() %>%
      add_trace(x = ~as.character(date), y = ~sprint_distance_per_min, customdata=~activity_name,
                type = "scatter", mode = 'lines', line=list(color="#572C5F"),
                hovertemplate = paste0(
                  "<b>Match:</b> %{customdata}<br>",
                  "<b>Sprint Distance Per Min (m/min):</b> %{y:.1f}",
                  "<extra></extra>"))%>% 
      config(displaylogo = FALSE, scrollZoom = FALSE, displayModeBar = FALSE) %>%
      layout(
        xaxis = list(range = c(-0.5,range_x-1.5), showline=TRUE,showgrid = FALSE,showticklabels = FALSE,title=""),
        yaxis = list(showline=TRUE,showgrid = FALSE, title="Sprint Distance Per Min (m/min)"),        
        plot_bgcolor  = rgb(0,0,0,0),
        paper_bgcolor = rgb(0,0,0,0),
        shapes = list(
          list(type = "line", layer = "below", x0 = 0, x1 = 1, xref = "paper",  y0 = md_distance_desc$sprint_distance_per_min_mean, y1 = md_distance_desc$sprint_distance_per_min_mean, yref = "y", line = list(color = "red", dash = "dash")),
          list(type = "line", layer = "below", x0 = 0, x1 = 1, xref = "paper",  y0 = md_distance_desc$sprint_distance_per_min_mean+(2*md_distance_desc$sprint_distance_per_min_sd), y1 =  md_distance_desc$sprint_distance_per_min_mean+(2*md_distance_desc$sprint_distance_per_min_sd), yref = "y", line = list(color = "blue", dash = "dash")),
          list(type = "line", layer = "below", x0 = 0, x1 = 1, xref = "paper",  y0 =  md_distance_desc$sprint_distance_per_min_mean-(2*md_distance_desc$sprint_distance_per_min_sd), y1 =  md_distance_desc$sprint_distance_per_min_mean-(2*md_distance_desc$sprint_distance_per_min_sd), yref = "y", line = list(color = "blue", dash = "dash"))
        ),
        images=image_list
      )
    
  })
  
  md_distance_15min <- reactive({
    
    stats_period %>%
      dplyr::filter(athlete_name %in% input$athlete8 & activity_name == input$md_input & str_detect(period_name, "^\\d{1,2}\\. [[:alpha:]]{5,6} Half$")) %>%
      # dplyr::filter(activity_name == "12th April 2026 - MD vs Portsmouth (A)" & str_detect(period_name, "^\\d{1,2}\\. [[:alpha:]]{5,6} Half$")) %>%
      select(activity_name, date, athlete_name, period_name,start_time, field_time, total_distance, high_speed_distance, sprint_distance) %>%
      mutate(across(where(is.numeric), ~if_else(field_time == 0 & total_distance == 0, NA_real_, .x))) %>%
      group_by(activity_name, date, period_name, start_time) %>%
      summarize(across(where(is.numeric), ~sum(.x,na.rm=T))) %>%
      ungroup %>% 
      mutate(across(where(is.numeric) & !field_time, ~.x/(field_time/60), .names="{.col}_per_min")) %>% 
      arrange(start_time) %>% 
      mutate(
        # period_name = paste(str_remove(period_name, "\\d{1,2}\\.\\s"), (row_number()-1)*(90/n()), "-", row_number()*(90/n()), "min"),
             period=paste0((row_number()-1)*(90/n()), "-", row_number()*(90/n()), "min"))
    
  })
  
  md_distance_15min_by_position <- reactive({
    
    stats_period %>%
      dplyr::filter(athlete_name %in% input$athlete8 & activity_name == input$md_input & str_detect(period_name, "^\\d{1,2}\\. [[:alpha:]]{5,6} Half$")) %>%
      # dplyr::filter(activity_name == "12th April 2026 - MD vs Portsmouth (A)" & str_detect(period_name, "^\\d{1,2}\\. [[:alpha:]]{5,6} Half$")) %>%
      select(activity_name, date, athlete_name, position_name, period_name,start_time, field_time, total_distance, high_speed_distance, sprint_distance) %>%
      mutate(across(where(is.numeric), ~if_else(field_time == 0 & total_distance == 0, NA_real_, .x)),
             position_name = if_else(str_detect(position_name, "Back"), "Defender", position_name), 
             position_name = if_else(str_detect(position_name, "Midfielder"), "Midfielder", position_name),
             position_name = if_else(str_detect(position_name, "Winger") | str_detect(position_name, "Striker") , "Attacker", position_name)) %>%
      group_by(activity_name, date, period_name, start_time, position_name) %>%
      summarize(across(where(is.numeric), ~sum(.x,na.rm=T))) %>%
      ungroup %>% 
      mutate(across(where(is.numeric) & !field_time, ~.x/(field_time/60), .names="{.col}_per_min")) %>% 
      arrange(start_time) %>% 
      group_by(position_name) %>% 
      mutate(
        # period_name = paste(str_remove(period_name, "\\d{1,2}\\.\\s"), (row_number()-1)*(90/n()), "-", row_number()*(90/n()), "min"),
        period=paste0((row_number()-1)*(90/n()), "-", row_number()*(90/n()), "min")) %>% 
      ungroup 
    
  })
  
  md_total_distance_15min <- reactive({
    
    shiny::validate(need(!is.null(input$athlete8), "Select one or more players"))
    
    md_distance_15min <- md_distance_15min()
    md_distance_15min_by_position <- md_distance_15min_by_position()
    
    # md_distance_15min <- rbind(md_distance_15min %>% mutate(position_name="All"), md_distance_15min_by_position)
    # 
    # md_distance_15min %>%
    #   plot_ly() %>%
    #   add_trace(data = md_distance_15min, x = ~period, y = ~total_distance, customdata=~paste0(activity_name, "<br><b>Period:</b> ", period, "<br><b>Position:</b> ", position_name),
    #             type = "scatter", mode="lines+markers",color=~position_name,
    #             hovertemplate = paste0(
    #               "<b>Match:</b> %{customdata}<br>",
    #               "<b>Total Distance (m):</b> %{y:.1f}",
    #               "<extra></extra>"))%>%
    #   config(displaylogo = FALSE, scrollZoom = FALSE, displayModeBar = FALSE) %>%
    #   layout(
    #     legend = list(orientation = 'h',xanchor = "center", x = 0.5,y = -0.15),
    #     xaxis = list(showline=TRUE,showgrid = FALSE,title=""),
    #     yaxis = list(zeroline = F, showline=TRUE,showgrid = FALSE, title="Total Distance (m)"),
    #     plot_bgcolor  = rgb(0,0,0,0),
    #     paper_bgcolor = rgb(0,0,0,0))
    
    plot_ly() %>%
      add_trace(data = md_distance_15min, x = ~period, y = ~total_distance, name = "All", customdata=~paste0(activity_name, "<br><b>Period:</b> ", period),
                type = "scatter", mode="lines+markers", color=I("#572C5F"),
                hovertemplate = paste0(
                  "<b>Match:</b> %{customdata}<br>",
                  "<b>Total Distance (m):</b> %{y:.1f}",
                  "<extra></extra>"))%>% 
      add_trace(data = md_distance_15min_by_position, x = ~period, y = ~total_distance, customdata=~paste0(activity_name, "<br><b>Period:</b> ", period, "<br><b>Position:</b> ", position_name),
                type = "scatter", mode="lines+markers", color=~position_name,                                                                                                                             
                hovertemplate = paste0(
                  "<b>Match:</b> %{customdata}<br>",
                  "<b>Total Distance (m):</b> %{y:.1f}",
                  "<extra></extra>"))%>% 
      config(displaylogo = FALSE, scrollZoom = FALSE, displayModeBar = FALSE) %>%
      layout(
        legend = list(orientation = 'h',xanchor = "center", x = 0.5,y = -0.15),
        xaxis = list(showline=TRUE,showgrid = FALSE,title=""),
        yaxis = list(zeroline = F, showline=TRUE,showgrid = FALSE, title="Total Distance (m)"),        
        plot_bgcolor  = rgb(0,0,0,0),
        paper_bgcolor = rgb(0,0,0,0))
  })
  
  md_hsr_distance_15min <- reactive({
    
    shiny::validate(need(!is.null(input$athlete8), "Select one or more players"))
    
    md_distance_15min <- md_distance_15min()
    md_distance_15min_by_position <- md_distance_15min_by_position()
    
    plot_ly() %>%
      add_trace(data = md_distance_15min, x = ~period, y = ~high_speed_distance, name = "All", customdata=~paste0(activity_name, "<br><b>Period:</b> ", period),
                type = "scatter", mode="lines+markers", color=I("#572C5F"),
                hovertemplate = paste0(
                  "<b>Match:</b> %{customdata}<br>",
                  "<b>HSR Distance (m):</b> %{y:.1f}",
                  "<extra></extra>"))%>% 
      add_trace(data = md_distance_15min_by_position, x = ~period, y = ~high_speed_distance, customdata=~paste0(activity_name, "<br><b>Period:</b> ", period, "<br><b>Position:</b> ", position_name),
                type = "scatter", mode="lines+markers", color=~position_name,                                                                                                                             
                hovertemplate = paste0(
                  "<b>Match:</b> %{customdata}<br>",
                  "<b>HSR Distance (m):</b> %{y:.1f}",
                  "<extra></extra>"))%>% 
      config(displaylogo = FALSE, scrollZoom = FALSE, displayModeBar = FALSE) %>%
      layout(
        legend = list(orientation = 'h',xanchor = "center", x = 0.5,y = -0.15),
        xaxis = list(showline=TRUE,showgrid = FALSE,title=""),
        yaxis = list(zeroline = F, showline=TRUE,showgrid = FALSE, title="HSR Distance (m)"),        
        plot_bgcolor  = rgb(0,0,0,0),
        paper_bgcolor = rgb(0,0,0,0))
  })
  
  md_sprint_distance_15min <- reactive({
    
    shiny::validate(need(!is.null(input$athlete8), "Select one or more players"))
    
    md_distance_15min <- md_distance_15min()
    md_distance_15min_by_position <- md_distance_15min_by_position()
    
    plot_ly() %>%
      add_trace(data = md_distance_15min, x = ~period, y = ~sprint_distance, name = "All", customdata=~paste0(activity_name, "<br><b>Period:</b> ", period),
                type = "scatter", mode="lines+markers", color=I("#572C5F"),
                hovertemplate = paste0(
                  "<b>Match:</b> %{customdata}<br>",
                  "<b>Sprint Distance (m):</b> %{y:.1f}",
                  "<extra></extra>"))%>% 
      add_trace(data = md_distance_15min_by_position, x = ~period, y = ~sprint_distance, customdata=~paste0(activity_name, "<br><b>Period:</b> ", period, "<br><b>Position:</b> ", position_name),
                type = "scatter", mode="lines+markers", color=~position_name,
                hovertemplate = paste0(
                  "<b>Match:</b> %{customdata}<br>",
                  "<b>Sprint Distance (m):</b> %{y:.1f}",
                  "<extra></extra>"))%>% 
      config(displaylogo = FALSE, scrollZoom = FALSE, displayModeBar = FALSE) %>%
      layout(
        legend = list(orientation = 'h',xanchor = "center", x = 0.5,y = -0.15),
        xaxis = list(showline=TRUE,showgrid = FALSE,title=""),
        yaxis = list(zeroline = F, showline=TRUE,showgrid = FALSE, title="Sprint Distance (m)"),        
        plot_bgcolor  = rgb(0,0,0,0),
        paper_bgcolor = rgb(0,0,0,0))
  })
  
  
  md_total_distance_per_min_15min <- reactive({
    
    shiny::validate(need(!is.null(input$athlete8), "Select one or more players"))
    
    md_distance_15min <- md_distance_15min()
    md_distance_15min_by_position <- md_distance_15min_by_position()
    
    plot_ly() %>%
      add_trace(data = md_distance_15min, x = ~period, y = ~total_distance_per_min, name = "All", customdata=~paste0(activity_name, "<br><b>Period:</b> ", period),
                type = "scatter", mode="lines+markers", color=I("#572C5F"),
                hovertemplate = paste0(
                  "<b>Match:</b> %{customdata}<br>",
                  "<b>Total Distance Per Min (m/min):</b> %{y:.1f}",
                  "<extra></extra>"))%>% 
      add_trace(data = md_distance_15min_by_position, x = ~period, y = ~total_distance_per_min, customdata=~paste0(activity_name, "<br><b>Period:</b> ", period, "<br><b>Position:</b> ", position_name),
                type = "scatter", mode="lines+markers", color=~position_name,
                hovertemplate = paste0(
                  "<b>Match:</b> %{customdata}<br>",
                  "<b>Total Distance Per Min (m/min):</b> %{y:.1f}",
                  "<extra></extra>"))%>% 
      config(displaylogo = FALSE, scrollZoom = FALSE, displayModeBar = FALSE) %>%
      layout(
        legend = list(orientation = 'h',xanchor = "center", x = 0.5,y = -0.15),
        xaxis = list(showline=TRUE,showgrid = FALSE,title=""),
        yaxis = list(zeroline = F, showline=TRUE,showgrid = FALSE, title="Total Distance Per Min (m/min)"),        
        plot_bgcolor  = rgb(0,0,0,0),
        paper_bgcolor = rgb(0,0,0,0))
  })
  
  md_hsr_distance_per_min_15min <- reactive({
    
    shiny::validate(need(!is.null(input$athlete8), "Select one or more players"))
    
    md_distance_15min <- md_distance_15min()
    md_distance_15min_by_position <- md_distance_15min_by_position()
    
    plot_ly() %>%
      add_trace(data = md_distance_15min, x = ~period, y = ~high_speed_distance_per_min, name = "All", customdata=~paste0(activity_name, "<br><b>Period:</b> ", period),
                type = "scatter", mode="lines+markers", color=I("#572C5F"),
                hovertemplate = paste0(
                  "<b>Match:</b> %{customdata}<br>",
                  "<b>HSR Distance Per Min (m/min):</b> %{y:.1f}",
                  "<extra></extra>"))%>% 
      add_trace(data = md_distance_15min_by_position, x = ~period, y = ~high_speed_distance_per_min, customdata=~paste0(activity_name, "<br><b>Period:</b> ", period, "<br><b>Position:</b> ", position_name),
                type = "scatter", mode="lines+markers", color=~position_name,
                hovertemplate = paste0(
                  "<b>Match:</b> %{customdata}<br>",
                  "<b>HSR Distance Per Min (m/min):</b> %{y:.1f}",
                  "<extra></extra>"))%>% 
      config(displaylogo = FALSE, scrollZoom = FALSE, displayModeBar = FALSE) %>%
      layout(
        legend = list(orientation = 'h',xanchor = "center", x = 0.5,y = -0.15),
        xaxis = list(showline=TRUE,showgrid = FALSE,title=""),
        yaxis = list(zeroline = F, showline=TRUE,showgrid = FALSE, title="HSR Distance Per Min (m/min)"),        
        plot_bgcolor  = rgb(0,0,0,0),
        paper_bgcolor = rgb(0,0,0,0))
  })
  
  md_sprint_distance_per_min_15min <- reactive({
    
    shiny::validate(need(!is.null(input$athlete8), "Select one or more players"))
    
    md_distance_15min <- md_distance_15min()
    md_distance_15min_by_position <- md_distance_15min_by_position()
    
    plot_ly() %>%
      add_trace(data = md_distance_15min, x = ~period, y = ~sprint_distance_per_min, name = "All", customdata=~paste0(activity_name, "<br><b>Period:</b> ", period),
                type = "scatter", mode="lines+markers", color=I("#572C5F"),
                hovertemplate = paste0(
                  "<b>Match:</b> %{customdata}<br>",
                  "<b>Sprint Distance Per Min (m/min):</b> %{y:.1f}",
                  "<extra></extra>"))%>% 
      add_trace(data = md_distance_15min_by_position, x = ~period, y = ~sprint_distance_per_min, customdata=~paste0(activity_name, "<br><b>Period:</b> ", period, "<br><b>Position:</b> ", position_name),
                type = "scatter", mode="lines+markers", color=~position_name,
                hovertemplate = paste0(
                  "<b>Match:</b> %{customdata}<br>",
                  "<b>Sprint Distance Per Min (m/min):</b> %{y:.1f}",
                  "<extra></extra>"))%>% 
      config(displaylogo = FALSE, scrollZoom = FALSE, displayModeBar = FALSE) %>%
      layout(
        legend = list(orientation = 'h',xanchor = "center", x = 0.5,y = -0.15),
        xaxis = list(showline=TRUE,showgrid = FALSE,title=""),
        yaxis = list(zeroline = F, showline=TRUE,showgrid = FALSE, title="Sprint Distance Per Min (m/min)"),        
        plot_bgcolor  = rgb(0,0,0,0),
        paper_bgcolor = rgb(0,0,0,0))
  })
  
  acute_chronic_load_plot <- reactive({
    
    shiny::validate(need(!is.null(input$athlete1), "Select one or more players"))
    
    player_load_stats <- stats %>%
      select(athlete_name | date | tag_name | field_time | total_distance | high_speed_distance | sprint_distance | accel_efforts | decel_efforts | accel_decel_efforts | meterage_per_minute | max_vel_kph | mean_heart_rate | max_heart_rate | dive_count | total_dive_load | explosive_efforts | ((starts_with("acwr_ewma") | starts_with("cl_ewma") | starts_with("al_ewma")) & !contains("wellness") & !contains("rpe") & !contains("RSI"))) %>% 
      rename_with(~ paste0("daily_", .x), .cols = field_time | total_distance | high_speed_distance | sprint_distance | accel_efforts | decel_efforts | accel_decel_efforts | meterage_per_minute | max_vel_kph |  mean_heart_rate | max_heart_rate | dive_count | total_dive_load | explosive_efforts) %>% 
      pivot_longer(cols = starts_with("daily") | starts_with("al_ewma") | starts_with("acwr_ewma") | starts_with("cl_ewma"), names_to = c(".value", "param"), names_pattern = "(daily|al_ewma|cl_ewma|acwr_ewma)_(.*)") %>% 
      dplyr::filter(athlete_name %in% input$athlete1 & date >= input$date_range1[1] & date <= input$date_range1[2] & param == input$acwr_param) %>%
      # dplyr::filter(athlete_name %in% c("Sydney Kennedy") & date >= (Sys.Date()-weeks(4)) & date <= Sys.Date() & param == "total_distance") %>%
      group_by(date, tag_name) %>% 
      summarize(across(where(is.numeric), ~mean(.x,na.rm=T))) %>% 
      ungroup %>% 
      mutate(daily_workload_color = if_else(tag_name == "MD", "#572C5F", "#00B0B9"))
    
    shiny::validate(need(sum(!is.na(player_load_stats$cl_ewma)) > 1 & sum(!is.na(player_load_stats$acwr_ewma)) > 1, "Insufficient Data"))
    
    player_load_stats %>% 
      plot_ly() %>%
      add_trace(x = ~date, y = ~cl_ewma, type = "scatter", mode = "lines", fill = "tozeroy", 
                name = "Chronic Workload", customdata = ~tag_name,
                fillcolor = rgb(178, 201, 212,round(0.6 * 255),maxColorValue = 255),
                line=list(color= rgb(178, 201, 212,round(0.6 * 255),maxColorValue = 255)),
                hovertemplate = paste0(
                  "<b>%{fullData.name}</b><br>",
                  "<b>Date:</b> %{x|%b %d, %Y}<br>",
                  "<b>MD Code:</b> %{customdata}<br>",
                  "<b>%{yaxis.title.text}:</b> %{y:.1f}",
                  "<extra></extra>")) %>%
      add_trace(x = ~date, y = ~daily, type = "bar", name="Daily Workload",customdata = ~tag_name,
                marker = list(color = ~I(daily_workload_color)),
                hovertemplate = paste0(
                  "<b>%{fullData.name}</b><br>",
                  "<b>Date:</b> %{x|%b %d, %Y}<br>",
                  "<b>MD Code:</b> %{customdata}<br>",
                  "<b>%{yaxis.title.text}:</b> %{y:.1f}",
                  "<extra></extra>"))%>% 
      add_trace(x = ~date, y = ~acwr_ewma, type = "scatter", mode = "lines", 
                name = "Acute:Chronic Workload Ratio", yaxis = "y2", line=list(color="#221C35"),
                customdata = ~tag_name,
                hovertemplate = paste0(
                  "<b>%{fullData.name}</b><br>",
                  "<b>Date:</b> %{x|%b %d, %Y}<br>",
                  "<b>MD Code:</b> %{customdata}<br>",
                  "<b>ACWR:</b> %{y:.2f}",
                  "<extra></extra>")) %>% 
      config(displaylogo = FALSE, scrollZoom = FALSE, displayModeBar = FALSE) %>%
      layout(
        shapes = list(
          list(type = "rect", xref='paper', yref='y2', x0 = 0, x1 = 1, y0 = 0.9, y1 = 1.3, layer = "below", fillcolor = rgb(229, 225, 230, round(0.3 * 255),maxColorValue = 255), line = list(color = rgb(229, 225, 230,round(0.3 * 255),maxColorValue = 255))),
          list(type = "rect", xref='paper', yref='y2', x0 = 0, x1 = 1, y0 = 0.8, y1 = 0.9, layer = "below", fillcolor = rgb(229, 225, 230, round(0.6 * 255),maxColorValue = 255),  line = list(color = rgb(229, 225, 230,round(0.6 * 255),maxColorValue = 255))),
          list(type = "rect", xref='paper', yref='y2', x0 = 0, x1 = 1, y0 = 1.3, y1 = 1.4, layer = "below", fillcolor = rgb(229, 225, 230,round(0.6 * 255),maxColorValue = 255),  line = list(color = rgb(229, 225, 230,round(0.6 * 255),maxColorValue = 255)))
        ),
        yaxis2 = list(range=c(0,2),showline=TRUE,showgrid = FALSE, tickformat = ".1f",overlaying = "y", automargin = TRUE, side = "right", title = "Acute:Chronic Workload Ratio"),
        xaxis = list(showline=TRUE,showgrid = FALSE,type = 'date', tickformat = "%b %d", dtick=604800000, title=""),
        yaxis = list(showline=TRUE,showgrid = FALSE,title=case_when(input$acwr_param == "field_time"~ "Field Time (s)", 
                                                                    input$acwr_param == "total_distance"~"Total Distance (m)", 
                                                                    input$acwr_param == "high_speed_distance"~ "High Speed Distance (m)", 
                                                                    input$acwr_param == "sprint_distance"~ "Sprint Distance (m)",
                                                                    input$acwr_param == "accel_efforts"~ "Accel Efforts",
                                                                    input$acwr_param == "decel_efforts" ~"Decel Efforts", 
                                                                    input$acwr_param == "accel_decel_efforts" ~"Accel + Decel Efforts", 
                                                                    input$acwr_param == "meterage_per_minute"~"Meterage per Minute (m/min)", 
                                                                    input$acwr_param == "max_vel_kph"~"Max Velocity (km/h)", 
                                                                    input$acwr_param == "max_heart_rate"~"Max HR (bpm)", 
                                                                    input$acwr_param == "mean_heart_rate"~"Avg HR (bpm)", 
                                                                    input$acwr_param == "dive_count"~"Dive Count", 
                                                                    input$acwr_param == "total_dive_load"~"Total Dive Load", 
                                                                    input$acwr_param == "explosive_efforts"~"Explosive Efforts", 
                                                                    .default = "")),
        legend = list(orientation = 'h',xanchor = "center", x = 0.5,y = -0.15),
        plot_bgcolor  = rgb(0,0,0,0),
        paper_bgcolor = rgb(0,0,0,0))
    
  })
  
  
  player_load_stats2 <- reactive({
    
    shiny::validate(need(!is.null(input$athlete2), "Select one or more players"))
    
    stats %>%
      select(athlete_name | date | (starts_with("zscore_7_28") & !contains("wellness") & !contains("RSI"))) %>% 
      rename(internal_load=zscore_7_28_max_heart_rate, subjective_load = zscore_7_28_rpe) %>% 
      rename_with(~str_replace(.x,"zscore_7_28", "external_load")) %>% 
      pivot_longer(cols = starts_with("external_load"), names_to = "external_load_param", values_to = "external_load") %>% 
      dplyr::filter(date == input$date_input1 & athlete_name %in% input$athlete2 & external_load_param == input$ext_load_param) %>% 
      group_by(date) %>% 
      summarize(across(where(is.numeric), ~mean(.x,na.rm=T))) %>% 
      ungroup
  })
  
  
  
  text <- data.frame(x=c(-1.5,-1.5, 1.5, 1.5), 
                     y= c(-1.5,1.5, -1.5,1.5), 
                     label=c("Increase Load", "Maladaptation", "Adaptation", "Decrease Load"))
  
  
  text2 <- data.frame(x=c(-1.5,-1.5, 1.5, 1.5), 
                      y= c(-1.5,1.5, -1.5,1.5), 
                      label=c("Investigate\nExternal Factors", "Increase Workload", "Decrease Workload", "Continue Training"))
  
  text3 <- data.frame(x=c(-1.5,-1.5, 1.5, 1.5), 
                      y= c(-1.5,1.5, -1.5,1.5), 
                      label=c("Extra Recovery", "Increase Mental\nPreparation", "Increase Physical\nPreparation", "Ready to Train/Play"))
  
  
  int_ext_load_plot <- reactive({
    
    
    
    plot_ly() %>% 
      add_annotations(xref='x', yref='y', x=text$x, y=text$y, text=text$label, showarrow = FALSE, align="center",font = list(color = rgb(0, 0, 0, 0.4),weight=600, size = 12))%>%
      add_trace(x=player_load_stats2()$external_load, y=player_load_stats2()$internal_load, type="scatter", mode="markers",opacity=1, marker=list(color="#00B0B9", size=14),
                hovertemplate = paste0(
                  "<b>Z-Score (3-day avg vs. 28-day avg):</b><br>",
                  "<b>%{xaxis.title.text}:</b> %{x:.2f}<br>",
                  "<b>%{yaxis.title.text}:</b> %{y:.2f}",
                  "<extra></extra>")) %>% 
      config(displaylogo = FALSE, scrollZoom = FALSE, displayModeBar = FALSE) %>%
      layout(
        shapes = list(
          list(type = "line", xref='x', yref='y', x0 = -3, x1 = 3, y0 = 0, y1 = 0, layer = "below"),
          list(type = "line", xref='x', yref='y', x0 = 0, x1 = 0, y0 = -3, y1 = 3, layer = "below"),
          list(type = "line", xref='x', yref='y', x0 = -3, x1 = -3, y0 = -3, y1 = 3, layer = "below"),
          list(type = "line", xref='x', yref='y', x0 = 3, x1 = 3, y0 = -3, y1 = 3, layer = "below"),
          list(type = "line", xref='x', yref='y', x0 = -3, x1 = 3, y0 = -3, y1 = -3, layer = "below"),
          list(type = "line", xref='x', yref='y', x0 = -3, x1 = 3, y0 = 3, y1 = 3, layer = "below")),
        xaxis = list(range=c(-3,3),scaleanchor = "y", scaleratio = 1, constrain="domain",constraintoward="center", zeroline=FALSE, showticklabels = FALSE,showline=FALSE,showgrid = FALSE,
                     title = paste("External Workload -",case_when(input$ext_load_param == "external_load_field_time"~ "Field Time", 
                                                                   input$ext_load_param == "external_load_total_distance"~"Total Distance", 
                                                                   input$ext_load_param == "external_load_high_speed_distance"~ "High Speed Distance", 
                                                                   input$ext_load_param == "external_load_sprint_distance"~ "Sprint Distance",
                                                                   input$ext_load_param == "external_load_accel_efforts"~ "Accel Efforts",
                                                                   input$ext_load_param == "external_load_decel_efforts" ~"Decel Efforts", 
                                                                   input$ext_load_param == "external_load_dive_count"~"Dive Count", 
                                                                   input$ext_load_param == "external_load_total_dive_load"~"Total Dive Load", 
                                                                   input$ext_load_param == "external_load_explosive_efforts"~"Explosive Efforts", 
                                                                   .default = ""))),
        yaxis = list(range=c(-3,3),scaleanchor = "x", scaleratio = 1,constrain="domain", constraintoward="center", zeroline=FALSE, showticklabels = FALSE,showline=FALSE,showgrid = FALSE,title = "Internal Workload - Max HR"),
        plot_bgcolor  = rgb(0,0,0,0),
        paper_bgcolor = rgb(0,0,0,0))
    
  })
  
  
  sub_ext_load_plot <- reactive({
    
    plot_ly() %>%
      add_annotations(xref='x', yref='y', x=text$x, y=text$y, text=text$label, showarrow = FALSE, align="center",font = list(color = rgb(0, 0, 0, 0.4),weight=600, size = 12))%>%
      add_trace(x=player_load_stats2()$external_load, y=player_load_stats2()$subjective_load, type="scatter", mode="markers",opacity=1, marker=list(color="#00B0B9", size=14),
                hovertemplate = paste0(
                  "<b>Z-Score (3-day avg vs. 28-day avg):</b><br>",
                  "<b>%{xaxis.title.text}:</b> %{x:.2f}<br>",
                  "<b>%{yaxis.title.text}:</b> %{y:.2f}",
                  "<extra></extra>")) %>%
      config(displaylogo = FALSE, scrollZoom = FALSE, displayModeBar = FALSE) %>%
      layout(
        shapes = list(
          list(type = "line", xref='x', yref='y', x0 = -3, x1 = 3, y0 = 0, y1 = 0, layer = "below"),
          list(type = "line", xref='x', yref='y', x0 = 0, x1 = 0, y0 = -3, y1 = 3, layer = "below"),
          list(type = "line", xref='x', yref='y', x0 = -3, x1 = -3, y0 = -3, y1 = 3, layer = "below"),
          list(type = "line", xref='x', yref='y', x0 = 3, x1 = 3, y0 = -3, y1 = 3, layer = "below"),
          list(type = "line", xref='x', yref='y', x0 = -3, x1 = 3, y0 = -3, y1 = -3, layer = "below"),
          list(type = "line", xref='x', yref='y', x0 = -3, x1 = 3, y0 = 3, y1 = 3, layer = "below")),
        xaxis = list(range=c(-3,3),scaleanchor = "y", scaleratio = 1, constrain="domain", constraintoward="center", zeroline=FALSE, showticklabels = FALSE,showline=FALSE,showgrid = FALSE,
                     title = paste("External Workload -",case_when(input$ext_load_param == "external_load_field_time"~ "Field Time",
                                                                   input$ext_load_param == "external_load_total_distance"~"Total Distance",
                                                                   input$ext_load_param == "external_load_high_speed_distance"~ "High Speed Distance",
                                                                   input$ext_load_param == "external_load_sprint_distance"~ "Sprint Distance",
                                                                   input$ext_load_param == "external_load_accel_efforts"~ "Accel Efforts",
                                                                   input$ext_load_param == "external_load_decel_efforts" ~"Decel Efforts",
                                                                   input$ext_load_param == "external_load_dive_count"~"Dive Count",
                                                                   input$ext_load_param == "external_load_total_dive_load"~"Total Dive Load",
                                                                   input$ext_load_param == "external_load_explosive_efforts"~"Explosive Efforts",
                                                                   .default = ""))),
        yaxis = list(range=c(-3,3),scaleanchor = "x", scaleratio = 1, constrain="domain",constraintoward="center", zeroline=FALSE, showticklabels = FALSE,showline=FALSE,showgrid = FALSE,title = "Subjective Workload - RPE"),
        plot_bgcolor  = rgb(0,0,0,0),
        paper_bgcolor = rgb(0,0,0,0))
    
    
  })
  
  wellness_workload_plot <- reactive({
    
    shiny::validate(need(!is.null(input$athlete2), "Select one or more players"))
    
    player_load_stats4 <- stats %>%
      select(athlete_name | date | starts_with("zscore_7_28")) %>% 
      rename(wellness = zscore_7_28_wellness) %>%
      rename_with(~ str_replace(.x, "zscore_7_28", "workload")) %>% 
      pivot_longer(cols = starts_with("workload"), names_to = "workload_param", values_to = "workload") %>% 
      dplyr::filter(workload_param == input$workload_param) %>% 
      arrange(athlete_name, date) %>% 
      # group_by(athlete_name) %>% 
      # mutate(workload2 = dplyr::lag(workload)) %>% 
      # ungroup %>% 
      dplyr::filter(date == input$date_input1 & athlete_name %in% input$athlete2) %>% 
      group_by(date) %>% 
      summarize(across(where(is.numeric), ~mean(.x,na.rm=T))) %>% 
      ungroup
    
    
    plot_ly() %>% 
      add_annotations(xref='x', yref='y', x=text2$x, y=text2$y, text=text2$label, showarrow = FALSE, align="center",font = list(color = rgb(0, 0, 0, 0.4),weight=600, size = 12))%>%
      add_trace(x=player_load_stats4$workload, y=player_load_stats4$wellness, type="scatter", mode="markers",opacity=1, marker=list(color="#00B0B9", size=14),
                hovertemplate = paste0(
                  "<b>Z-Score (3-day avg vs. 28-day avg):</b><br>",
                  "<b>%{xaxis.title.text}:</b> %{x:.2f}<br>",
                  "<b>%{yaxis.title.text}:</b> %{y:.2f}",
                  "<extra></extra>")) %>% 
      config(displaylogo = FALSE, scrollZoom = FALSE, displayModeBar = FALSE) %>%
      layout(
        shapes = list(
          list(type = "line", xref='x', yref='y', x0 = -3, x1 = 3, y0 = 0, y1 = 0, layer = "below"),
          list(type = "line", xref='x', yref='y', x0 = 0, x1 = 0, y0 = -3, y1 = 3, layer = "below"),
          list(type = "line", xref='x', yref='y', x0 = -3, x1 = -3, y0 = -3, y1 = 3, layer = "below"),
          list(type = "line", xref='x', yref='y', x0 = 3, x1 = 3, y0 = -3, y1 = 3, layer = "below"),
          list(type = "line", xref='x', yref='y', x0 = -3, x1 = 3, y0 = -3, y1 = -3, layer = "below"),
          list(type = "line", xref='x', yref='y', x0 = -3, x1 = 3, y0 = 3, y1 = 3, layer = "below")),
        xaxis = list(range=c(-3,3),scaleanchor = "y", scaleratio = 1,constrain="domain", constraintoward="center", zeroline=FALSE, showticklabels = FALSE,showline=FALSE,showgrid = FALSE,
                     title = paste("Workload -",
                                   case_when(input$workload_param == "workload_field_time"~ "Field Time", 
                                             input$workload_param == "workload_total_distance"~"Total Distance",
                                             input$workload_param == "workload_high_speed_distance"~ "High Speed Distance",
                                             input$workload_param == "workload_sprint_distance"~ "Sprint Distance",
                                             input$workload_param == "workload_accel_efforts"~ "Accel Efforts",
                                             input$workload_param == "workload_decel_efforts" ~"Decel Efforts",
                                             input$workload_param == "workload_dive_count"~"Dive Count",
                                             input$workload_param == "workload_total_dive_load"~"Total Dive Load",
                                             input$workload_param == "workload_explosive_efforts"~"Explosive Efforts",
                                             input$workload_param == "workload_rpe"~"RPE",
                                             input$workload_param == "workload_max_heart_rate"~"Max HR",
                                             .default = ""))),
        yaxis = list(range=c(-3,3),scaleanchor = "x", scaleratio = 1, constrain="domain",constraintoward="center", zeroline=FALSE, showticklabels = FALSE,showline=FALSE,showgrid = FALSE,title = "Wellness"),
        plot_bgcolor  = rgb(0,0,0,0),
        paper_bgcolor = rgb(0,0,0,0))
    
  })
  
  
  # readiness_wellness_plot <- reactive({
  #   
  #   shiny::validate(need(!is.null(input$athlete2), "Select one or more players"))
  #   
  #   player_load_stats4 <- stats %>%
  #     select(athlete_name | date | zscore_7_28_wellness | zscore_7_28_RSI) %>% 
  #     rename(wellness = zscore_7_28_wellness, readiness = zscore_7_28_RSI) %>%
  #     arrange(athlete_name, date) %>% 
  #     group_by(athlete_name) %>% 
  #     mutate(readiness2 = dplyr::lag(readiness)) %>% 
  #     ungroup %>% 
  #     dplyr::filter(date == input$date_input1 & athlete_name %in% input$athlete2) %>%
  #     group_by(date) %>% 
  #     summarize(across(where(is.numeric), ~mean(.x,na.rm=T))) %>% 
  #     ungroup
  #   
  #   plot_ly() %>% 
  #     add_annotations(xref='x', yref='y', x=text3$x, y=text3$y, text=text3$label, showarrow = FALSE, align="center",font = list(color = rgb(0, 0, 0, 0.4),weight=600, size = 12))%>%
  #     add_trace(x=player_load_stats4$wellness, y=player_load_stats4$readiness2, type="scatter", mode="markers",opacity=1, marker=list(color="#00B0B9", size=14),
  #               hovertemplate = paste0(
  #                 "<b>Z-Score (3-day avg vs. 28-day avg):</b><br>",
  #                 "<b>%{xaxis.title.text}:</b> %{x:.2f}<br>",
  #                 "<b>%{yaxis.title.text}:</b> %{y:.2f}",
  #                 "<extra></extra>")) %>% 
  #     config(displaylogo = FALSE, scrollZoom = FALSE, displayModeBar = FALSE) %>%
  #     layout(
  #       shapes = list(
  #         list(type = "line", xref='x', yref='y', x0 = -3, x1 = 3, y0 = 0, y1 = 0, layer = "below"),
  #         list(type = "line", xref='x', yref='y', x0 = 0, x1 = 0, y0 = -3, y1 = 3, layer = "below"),
  #         list(type = "line", xref='x', yref='y', x0 = -3, x1 = -3, y0 = -3, y1 = 3, layer = "below"),
  #         list(type = "line", xref='x', yref='y', x0 = 3, x1 = 3, y0 = -3, y1 = 3, layer = "below"),
  #         list(type = "line", xref='x', yref='y', x0 = -3, x1 = 3, y0 = -3, y1 = -3, layer = "below"),
  #         list(type = "line", xref='x', yref='y', x0 = -3, x1 = 3, y0 = 3, y1 = 3, layer = "below")),
  #       xaxis = list(range=c(-3,3),scaleanchor = "y", scaleratio = 1,constrain="domain",constraintoward="center", zeroline=FALSE, showticklabels = FALSE,showline=FALSE,showgrid = FALSE,title = "Wellness"),
  #       yaxis = list(range=c(-3,3),scaleanchor = "x", scaleratio = 1, constrain="domain",constraintoward="center", zeroline=FALSE, showticklabels = FALSE,showline=FALSE,showgrid = FALSE,title = "Readiness - Reactive Strength Index"),
  #       plot_bgcolor  = rgb(0,0,0,0),
  #       paper_bgcolor = rgb(0,0,0,0))
  #   
  # })
  
  wellness_plot <- reactive({
    
    shiny::validate(need(!is.null(input$athlete3), "Select one or more players"))
    
    wellness_stats <- wellness_scores %>%
      dplyr::filter(date == input$date_input2 & athlete_name %in% input$athlete3) %>%
      group_by(date, category, name) %>% 
      summarize(across(where(is.numeric), ~mean(.x,na.rm=T))) %>% 
      ungroup
    
    plot_ly(data=wellness_stats, x = ~category, y = ~category_item_ratio, type = "bar", color=~name,
            colors = c("#572C5F","#00B0B9", "#B2C9D4"),
            text = ~I(item_ratio), customdata = paste0(scales::percent(wellness_stats$category_ratio,accuracy = 0.1), "\n<b>", wellness_stats$name, ":</b> "), textposition = "inside",
            hovertemplate = paste0(
              "<b>%{x}:</b> %{customdata}",
              "%{text: .1%}",
              "<extra></extra>")) %>%
      config(displaylogo = FALSE, scrollZoom = FALSE, displayModeBar = FALSE) %>%
      layout(barmode = "stack",
             title = list(text="0% = Unwell; 100% = Optimal Well-being",font=list(size=12)),
             xaxis = list(showline=TRUE,showgrid = FALSE, title = ""),
             yaxis = list(showline=TRUE,showgrid = TRUE, range=c(0,1),tickformat = ".0%",title = ""),
             plot_bgcolor  = rgb(0,0,0,0),
             paper_bgcolor = rgb(0,0,0,0))
    
  })
  
  
  historical_wellness_plot <- reactive({
    
    shiny::validate(need(!is.null(input$athlete3), "Select one or more players"))
    

    total_wellness <- wellness_scores %>%
      select(athlete_name, date,total_ratio) %>% 
      unique %>% 
      rename(ratio=total_ratio) %>% 
      mutate(category = "Wellness", name = "Total Wellness") %>% 
      relocate(category, name, .before=ratio)
    
    category_wellness <- wellness_scores %>%
      select(athlete_name, date, category, category_ratio) %>% 
      unique %>% 
      mutate(name = paste("Total", category)) %>% 
      rename(ratio=category_ratio) %>% 
      relocate(name, .before=ratio)
    
    item_wellness <- wellness_scores %>%
      select(athlete_name, date, category, name, item_ratio) %>% 
      unique %>% 
      rename(ratio=item_ratio)
    
    wellness_stats <- rbind(total_wellness, category_wellness, item_wellness) %>%
      dplyr::filter(date >= input$date_range3[1] & date <= input$date_range3[2] & athlete_name %in% input$athlete3 & name == input$wellness_param) %>%
      group_by(date) %>% 
      summarize(ratio = mean(ratio,na.rm=T)) %>% 
      ungroup
    
    plot_ly(data=wellness_stats,x = ~date, y = ~ratio, type = "bar", color=I("#00B0B9"),
            hovertemplate = paste0(
              "<b>Date:</b> %{x|%b %d, %Y}<br>",
              "<b>%{yaxis.title.text}:</b> %{y:.1%}",
              "<extra></extra>")) %>%
      config(displaylogo = FALSE, scrollZoom = FALSE, displayModeBar = FALSE) %>%
      layout(title = list(text="0% = Unwell; 100% = Optimal Well-being",font=list(size=12)),
             xaxis = list(showline=TRUE,showgrid = FALSE, type = 'date', tickformat = "%b %d", dtick=604800000, title=""),
             yaxis = list(showline=TRUE,showgrid = TRUE, range=c(0,1),tickformat = ".0%",title = input$wellness_param),
             plot_bgcolor  = rgb(0,0,0,0),
             paper_bgcolor = rgb(0,0,0,0))
    
  })
  
  
  
  rpe_plot <- reactive({
    
    shiny::validate(need(!is.null(input$athlete5), "Select one or more players"))
    
    rpe_stats <- RPE_all %>%
      mutate(name = str_replace(name, "Game / Competition", "Match"),
             name = factor(name, levels = c("Match","Team Training","Strength Training", "Recovery"))) %>%
      dplyr::filter(date >= input$date_range2[1] & date <= input$date_range2[2] & athlete_name %in% input$athlete5) %>%
      group_by(date, name) %>%
      summarize(across(where(is.numeric), ~mean(.x,na.rm=T))) %>%
      ungroup 
    
    plot_ly(data=rpe_stats,x = ~date, y = ~session_rpe, type = "bar", color = ~name, customdata = ~paste0(round(daily_rpe), "\n<b>", name, "</b>\n<b>RPE:</b> ", round(rpe), "\n<b>Duration (min):</b> ", round(minutes)), colors=c("#572C5F", "#00B0B9", "#B2C9D4","#E5E1E6"),
            hovertemplate = paste0(
              "<b>Date:</b> %{x|%b %d, %Y}<br>",
              "<b>%{yaxis.title.text}:</b> %{customdata}<br>",
              "<b>Session RPE:</b> %{y:.0f}",
              "<extra></extra>")) %>%
      config(displaylogo = FALSE, scrollZoom = FALSE, displayModeBar = FALSE) %>%
      layout(barmode = "stack",
             xaxis = list(showline=TRUE,showgrid = FALSE, type = 'date', tickformat = "%b %d", dtick=604800000, title=""),
             yaxis = list(showline=TRUE,showgrid = TRUE, title = "Daily RPE"),
             legend = list(orientation = 'h', xanchor = "center", x = 0.5,y = -0.15),
             plot_bgcolor  = rgb(0,0,0,0),
             paper_bgcolor = rgb(0,0,0,0))
    
  })
  
 
  
  player_daily_summary_table  <- reactive({ 
    
    player_daily_summary <- stats %>% 
      dplyr::filter(
        athlete_name %in% input$athlete6 &
        date == input$date_input5
      ) %>%
      # dplyr::filter(
      #   athlete_name %in% c("Sydney Kennedy", "Saorla Miller", "Karima Lemire", "Rylee Foster") &
      #   date == Sys.Date()-days(4)
      # ) %>%
      select(position_name, athlete_name,total_distance, high_speed_distance, sprint_distance, accel_efforts, decel_efforts,meterage_per_minute,max_vel_kph, percentage_max_velocity, field_time) %>% 
      mutate(across(where(is.numeric), ~if_else(field_time == 0 & total_distance == 0, NA_real_, .x)),
             field_time=field_time/60,
             percentage_max_velocity=percentage_max_velocity/100
             ) %>% 
      rename(Position=position_name, Player = athlete_name, `Total Distance (m)` = total_distance, `HSR Distance (m)` = high_speed_distance, `Sprint Distance (m)` = sprint_distance, `Accel Efforts` = accel_efforts, `Decel Efforts`=decel_efforts, `Avg Speed (m/min)` = meterage_per_minute, `Max Speed (km/h)` = max_vel_kph, `Max Speed (%Max)` = percentage_max_velocity, `Field Time (min)`=field_time) %>% 
      drop_na(Position)
    
    
    footer_mean = function(values) {sprintf("%.0f", mean(values))}
    
    footer_mean_decimal = function(values) {sprintf("%.1f", mean(values))}

    footer_mean_percent = function(values) {paste0(sprintf("%.1f", mean(values)*100),"%")}
    
      reactable(
        player_daily_summary,
        striped = F,
        outline=F,
        bordered = T,
        compact = T,
        highlight = F,
        defaultPageSize =nrow(player_daily_summary)+1,
        columns = list(
          Position = colDef(show = FALSE),
          Player = colDef(minWidth = 145, 
                          style = list(fontWeight = 600, whiteSpace = "nowrap", textOverflow = "ellipsis"),
                          footer="Average"),
          #   cell = function(value, index) {
          #   position <- player_daily_summary$Position[index]
          #   position <- if (!is.na(position)) position else ""
          #   div(
          #     div(style = "font-weight: 600", value),
          #     div(style = "font-size: 0.75rem", position)
          #   )
          # },
          `Field Time (min)` = colDef(footer=footer_mean, format = colFormat(digits = 0), align = "center"),
          `Total Distance (m)` = colDef(footer=footer_mean, format = colFormat(digits = 0),align = "center"),
          `HSR Distance (m)` = colDef(footer=footer_mean, format = colFormat(digits = 0),align = "center"),
          `Sprint Distance (m)` = colDef(footer=footer_mean, format = colFormat(digits = 0),align = "center"),
          `Accel Efforts` = colDef(footer=footer_mean, format = colFormat(digits = 0),align = "center"),
          `Decel Efforts` = colDef(footer=footer_mean, format = colFormat(digits = 0),align = "center"),
          `Avg Speed (m/min)` = colDef(footer=footer_mean_decimal,  format = colFormat(digits = 1),align = "center"),
          `Max Speed (km/h)` = colDef(footer=footer_mean_decimal,  format = colFormat(digits = 1),align = "center"),
          `Max Speed (%Max)` = colDef(footer=footer_mean_percent,  format = colFormat(digits = 1, percent=T),align = "center")
          ),
        defaultColDef = colDef(footerStyle = list(fontWeight = "bold"))
        
      )
      
  })

  
  
  
  
  drill_summary_table  <- reactive({ 
    
    
    drill_daily_summary <- stats_period %>% 
      dplyr::filter(athlete_name %in% input$athlete7 & date == input$date_input6 & period_name %in% input$period_input) %>%
      # dplyr::filter(athlete_name %in% c("Sydney Kennedy", "Saorla Miller", "Karima Lemire", "Rylee Foster") &
      #  date == Sys.Date()-days(1) & period_name %in% c("2. First Half", "3. Second Half")) %>%
      select(position_name, athlete_name, period_name, total_distance, high_speed_distance, sprint_distance, accel_efforts, decel_efforts,meterage_per_minute,max_vel_kph, percentage_max_velocity, field_time) %>% 
      mutate(across(where(is.numeric), ~if_else(field_time == 0 & total_distance == 0, NA_real_, .x)),
             field_time=field_time/60,
             percentage_max_velocity= percentage_max_velocity/100) %>% 
      rename(Player = athlete_name, Position = position_name, Period = period_name,`Total Distance (m)` = total_distance, `HSR Distance (m)` = high_speed_distance, `Sprint Distance (m)` = sprint_distance, 
             `Accel Efforts` = accel_efforts, `Decel Efforts`=decel_efforts, `Avg Speed (m/min)` = meterage_per_minute, `Max Speed (km/h)` = max_vel_kph, `Max Speed (%Max)` = percentage_max_velocity, `Field Time (min)` = field_time) %>% 
      drop_na(Period)
  
  
    footer_mean_speed <-JS("function(column, state) { 
      var totalDistance = {}
      var totalTime = {}
      
      state.data.forEach(function(row) {
        var player = row['Player']
        if (!totalDistance[player]) { totalDistance[player] = 0 }
        if (!totalTime[player]) { totalTime[player] = 0 }
        
        totalDistance[player] += row['Total Distance (m)']
        totalTime[player] += row['Field Time (min)']
        
      })
      
      var avgSpeed = {}
      
      Object.keys(totalDistance).forEach(key => {
        // Check if key exists in the second object
        if (totalTime.hasOwnProperty(key)) {
          // Convert time to minutes and perform division:
          avgSpeed[key] = totalDistance[key] / totalTime[key]
        }
      })
      
      var avgSpeedValues = Object.values(avgSpeed)
      
      if (avgSpeedValues.length === 0) return ''
      
      var meanSum = avgSpeedValues.reduce((a, b) => a + b, 0) / avgSpeedValues.length
      
      return meanSum.toFixed(1)
    }")
    
    
    footer_max <- JS("function(column, state) {
        // 1. Group data by player and find the max for each
        var maxValues = {};
        state.data.forEach(function(row) {
          var player = row['Player'];
          var value = row[column.id];
          if (!(player in maxValues) || value > maxValues[player]) {
            maxValues[player] = value;
          }
        });

        // 2. Calculate the mean of those maximums
        var maxArray = Object.values(maxValues);
        
        if (maxArray.length === 0) return ''

        var sumMax = maxArray.reduce(function(a, b) { return a + b }, 0);
        var meanMax = sumMax / maxArray.length;

        // Return the formatted result
        return meanMax.toFixed(1);
      }")
    
    
    footer_max_percent <- JS("function(column, state) {
        // 1. Group data by player and find the max for each
        var maxValues = {};
        state.data.forEach(function(row) {
          var player = row['Player'];
          var value = row[column.id];
          if (!(player in maxValues) || value > maxValues[player]) {
            maxValues[player] = value;
          }
        });

        // 2. Calculate the mean of those maximums
        var maxArray = Object.values(maxValues);
               
        if (maxArray.length === 0) return ''

        var sumMax = maxArray.reduce(function(a, b) { return a + b }, 0);
        var meanMax = (sumMax / maxArray.length)*100;

        // Return the formatted result
        return meanMax.toFixed(1) + '%';
      }")
    
    footer_total <- JS("function(column, state) {
        var totals = {}
    
    // state.data contains every leaf row regardless of expansion
    // Note: state.data does not include aggregated rows
        
    state.data.forEach(function(row) {
    var player = row['Player']
    if (!totals[player]) { totals[player] = 0 }
      totals[player] += row[column.id]
      })
        
       var sumValues = Object.values(totals)
       if (sumValues.length === 0) return ''
        
        var meanSum = sumValues.reduce((a, b) => a + b, 0) / sumValues.length
        
        return meanSum.toFixed(0)
      }")
    
    
    footer_total_decimal <-  JS("function(column, state) {
        var totals = {}
    
    // state.data contains every leaf row regardless of expansion
    // Note: state.data does not include aggregated rows
        
    state.data.forEach(function(row) {
    var player = row['Player']
    if (!totals[player]) { totals[player] = 0 }
      totals[player] += row[column.id]
      })
        
       var sumValues = Object.values(totals)
       if (sumValues.length === 0) return ''
        
        var meanSum = sumValues.reduce((a, b) => a + b, 0) / sumValues.length
        
        return meanSum.toFixed(1)
      }")
  
        
  

  
    reactable(
      drill_daily_summary,
      groupBy = "Period",
      striped = F,
      outline=F,
      bordered = T,
      compact = T,
      highlight = F,
      defaultPageSize =nrow(drill_daily_summary)+length(unique(drill_daily_summary$Period))+1,
      columns = list(
        Period = colDef(footer="All", grouped = JS("function(cellInfo) {return cellInfo.value}"), 
                        minWidth = 145, style = list(fontWeight = 600)),
        Player = colDef(minWidth = 145, style = list(fontWeight = 600, whiteSpace = "nowrap", textOverflow = "ellipsis")),
        Position = colDef(show=F),
        `Field Time (min)` = colDef(aggregate = "mean", footer=footer_total,format = colFormat(digits = 0),align = "center"),
        `Total Distance (m)` = colDef(aggregate = "mean", footer=footer_total, format = colFormat(digits = 0),align = "center"),
        `HSR Distance (m)` = colDef(aggregate = "mean", footer=footer_total, format = colFormat(digits = 0),align = "center"),
        `Sprint Distance (m)` = colDef(aggregate = "mean",footer=footer_total, format = colFormat(digits = 0),align = "center"),
        `Accel Efforts` = colDef(aggregate = "mean", footer=footer_total, format = colFormat(digits = 0),align = "center"),
        `Decel Efforts` = colDef(aggregate = "mean", footer=footer_total, format = colFormat(digits = 0),align = "center"),
        `Avg Speed (m/min)` = colDef(aggregate = "mean",footer=footer_mean_speed,  format = colFormat(digits = 1),align = "center"),
        `Max Speed (km/h)` = colDef(aggregate = "mean",footer=footer_max,  format = colFormat(digits = 1),align = "center"),
        `Max Speed (%Max)` = colDef(aggregate = "mean",footer=footer_max_percent,  format = colFormat(digits = 1, percent=T),align = "center")
        ),
      defaultColDef = colDef(footerStyle = list(fontWeight = "bold"))
      
    )
    
  
    
  })
  

  match_day_table  <- reactive({ 
    
    
    match_day_summary <- stats_period %>% 
      dplyr::filter(athlete_name %in% input$athlete8 & activity_name == input$md_input & str_detect(period_name, "^\\d{1,2}\\. [[:alpha:]]{5,6} Half$")) %>%
      # dplyr::filter(activity_name == "18th May 2026 - MD 4  vs Vancouver (H)" & period_name %in% c("2. First Half", "3. Second Half")) %>%
      select(activity_name, position_name, athlete_name, period_name, field_time, total_distance, high_speed_distance, sprint_distance, accel_efforts, decel_efforts,max_vel_kph) %>% 
      mutate(across(where(is.numeric), ~if_else(field_time == 0 & total_distance == 0, NA_real_, .x))) %>% 
      pivot_wider(names_from = period_name, names_glue = "{period_name}_{.value}", values_from = c(field_time, total_distance, high_speed_distance, sprint_distance, accel_efforts, decel_efforts,max_vel_kph)) %>% 
      mutate(Sub = if_else(is.na(`2. First Half_field_time`) | `2. First Half_field_time` < (10*60), T, F)) %>% 
      pivot_longer(cols=contains(". "), names_to = c("period_name", ".value"), names_pattern = "(\\d{1,2}\\. [[:alpha:]]{5,6} Half)_(.*)") %>% 
      group_by(activity_name, athlete_name, position_name, Sub) %>% 
      summarize(across(where(is.numeric) & !max_vel_kph, ~sum(.x,na.rm=T)), max_vel_kph = max(max_vel_kph, na.rm=T)) %>% 
      ungroup %>% 
      mutate(`% of HSR + Sprint Distance` = ((high_speed_distance+sprint_distance)/total_distance),
             field_time=field_time/60) %>% 
      relocate(`% of HSR + Sprint Distance`, .before=max_vel_kph) %>% 
      relocate(field_time, .before=total_distance) %>% 
      rename(Match = activity_name, Player = athlete_name, Position = position_name,`Total Distance (m)` = total_distance, `HSR Distance (m)` = high_speed_distance, `Sprint Distance (m)` = sprint_distance, 
             `Accel Efforts` = accel_efforts, `Decel Efforts`=decel_efforts, `Max Speed (km/h)` = max_vel_kph, `Field Time (min)` = field_time) %>% 
      arrange(Sub, Position) 
    
    
    # Render a bar chart with a label on the left
    # bar_chart <- function(label, width = "100%", height = "1rem", fill = "#00B0B9", background = NULL) {
    #   bar <- div(style = list(background = fill, width = width, height = height))
    #   chart <- div(style = list(flexGrow = 1, marginLeft = "0.5rem", background = background), bar)
    #   div(style = list(display = "flex", alignItems = "center"), label, chart)
    # }
 
    bar_style <- function(width = 1, fill = "#00B0B9", height = "100%",
                          align = c("left", "right"), color = NULL) {
      align <- match.arg(align)
      if (align == "left") {
        position <- paste0(width * 100, "%")
        image <- sprintf("linear-gradient(90deg, %1$s %2$s, transparent %2$s)", fill, position)
      } else {
        position <- paste0(100 - width * 100, "%")
        image <- sprintf("linear-gradient(90deg, transparent %1$s, %2$s %1$s)", position, fill)
      }
      list(
        backgroundImage = image,
        backgroundSize = paste("100%", height),
        backgroundRepeat = "no-repeat",
        backgroundPosition = "center",
        color = color
      )
    }
    
    reactable(
      match_day_summary,
      striped = F,
      outline=F,
      bordered = T,
      compact = T,
      highlight = F,
      rowStyle = function(index) {
        if (match_day_summary[index, "Sub"]==TRUE) {
          list(background = rgb(229, 225, 230, round(0.6 * 255),maxColorValue = 255))}},
      defaultPageSize =nrow(match_day_summary),
      columns = list(
        Match = colDef(show = F),
        Sub = colDef(show = F),
        Position = colDef(show = F),
        Player = colDef(minWidth = 145, 
                        style = list(fontWeight = 600, whiteSpace = "nowrap", textOverflow = "ellipsis")),
        `Field Time (min)` = colDef(format = colFormat(digits = 0), align = "center"),
        # `Total Distance (m)` = colDef(format = colFormat(digits = 0),align = "center",cell = function(value) {
        #   width <- paste0(value / max(match_day_summary$`Total Distance (m)`) * 100, "%")
        #   bar_chart(round(value), width = width, fill = rgb(0, 176, 185,alpha=(0.5*255), maxColorValue = 255),  background = "#E5E1E6")
        # }),
        `Total Distance (m)` = colDef(format = colFormat(digits = 0),align = "center", style = function(value) {
          bar_style(width = value / max(match_day_summary$`Total Distance (m)`), fill = rgb(0, 176, 185,alpha=(0.5*255), maxColorValue = 255), color = "#221C35")}),
        `HSR Distance (m)` = colDef(format = colFormat(digits = 0),align = "center", style = function(value) {
          bar_style(width = value / max(match_day_summary$`HSR Distance (m)`), fill = rgb(87, 44, 95,alpha=(0.5*255), maxColorValue = 255), color = "#221C35")}),
        `Sprint Distance (m)` = colDef(format = colFormat(digits = 0),align = "center", style = function(value) {
          bar_style(width = value / max(match_day_summary$`Sprint Distance (m)`), fill = rgb(0, 176, 185,alpha=(0.5*255), maxColorValue = 255), color = "#221C35")}),
        `Accel Efforts` = colDef(format = colFormat(digits = 0),align = "center", style = function(value) {
          bar_style(width = value / max(match_day_summary$`Accel Efforts`), fill = rgb(87, 44, 95,alpha=(0.5*255), maxColorValue = 255), color = "#221C35")}),
        `Decel Efforts` = colDef(format = colFormat(digits = 0),align = "center", style = function(value) {
          bar_style(width = value / max(match_day_summary$`Decel Efforts`), fill = rgb(0, 176, 185,alpha=(0.5*255), maxColorValue = 255), color = "#221C35")}),
        `% of HSR + Sprint Distance`  = colDef(format = colFormat(percent = TRUE, digits = 2),align = "center"),
        `Max Speed (km/h)` = colDef(format = colFormat(digits = 1),align = "center")
      ) 
    )
    
    
    
  })
  
  # player_summary_table  <- reactive({ 
  #   
  #   player_summary <- stats %>% 
  #     dplyr::filter(date == input$date_input3 & position_name != "Goal Keeper") %>%
  #     select(athlete_name,total_distance, high_speed_distance, sprint_distance, accel_efforts, decel_efforts,meterage_per_minute,max_vel_kph, field_time) %>% 
  #     mutate(across(where(is.numeric), ~if_else(field_time == 0 & total_distance == 0, NA_real_, .x))) %>% 
  #     rename(Name = athlete_name, `Total Distance (m)` = total_distance, `High Speed Distance (m)` = high_speed_distance, `Sprint Distance (m)` = sprint_distance, `Accel Efforts` = accel_efforts, `Decel Efforts`=decel_efforts, `Avg Speed (m/min)` = meterage_per_minute, `Max Speed (km/h)` = max_vel_kph, `Field Time (h:m:s)`=field_time) 
  #   
  #   player_avg <- player_summary  %>%
  #     summarize(across(where(is.numeric), ~mean(.x, na.rm=T))) %>%
  #     mutate(Name = "Average")
  # 
  #   rbind(player_summary,player_avg) %>%
  #     mutate(across(where(is.numeric) & !`Max Speed (km/h)` & !`Avg Speed (m/min)`, round),
  #            `Max Speed (km/h)`=round(`Max Speed (km/h)`,1),
  #            `Avg Speed (m/min)`=round(`Avg Speed (m/min)`,1),
  #            `Field Time (h:m:s)` = as.character(as_hms(`Field Time (h:m:s)`)))
  #   
  # 
  # })
  # 
  # player_summary_footer <- reactive({ withTags(table(
  #   tableHeader(colnames(player_summary_table())),
  #   tableFooter(as.character(player_summary_table()[nrow(player_summary_table()),]))))
  #   
  # })
  # 
  # 
  # keeper_summary_table  <- reactive({ 
  #   
  #   keeper_summary <- stats %>% 
  #     dplyr::filter(date == input$date_input3 & position_name == "Goal Keeper") %>%
  #     select(athlete_name,dive_count, total_distance, total_dive_load,average_time_to_feet,accel_efforts, decel_efforts, explosive_efforts, field_time) %>% 
  #     mutate(across(where(is.numeric), ~if_else(field_time == 0 & total_distance == 0, NA_real_, .x))) %>% 
  #     rename(Name = athlete_name, `Total Distance (m)` = total_distance, `Dive Count`= dive_count, `Dive Load`= total_dive_load,`Avg Time to Feet (s)` = average_time_to_feet, `Accel Efforts` = accel_efforts, `Decel Efforts`=decel_efforts, `Explosive Efforts`=explosive_efforts, `Field Time (h:m:s)`=field_time)  
  #   
  #   
  #   keeper_avg <- keeper_summary  %>% 
  #     summarize(across(where(is.numeric), ~mean(.x, na.rm=T))) %>% 
  #     mutate(Name = "Average") 
  #   
  #   rbind(keeper_summary,keeper_avg) %>%  
  #     mutate(across(where(is.numeric) & !`Avg Time to Feet (s)`, round), 
  #            `Avg Time to Feet (s)`=round(`Avg Time to Feet (s)`,2),
  #            `Field Time (h:m:s)` = as.character(as_hms(`Field Time (h:m:s)`)))
  # })
  # 
  # keeper_summary_footer <- reactive({ withTags(table(
  #   tableHeader(colnames(keeper_summary_table())),
  #   tableFooter(as.character(keeper_summary_table()[nrow(keeper_summary_table()),]))))
  # })
  # 
  # 
  # player_load_table  <- reactive({ 
  #   
  #   player_load_summary <- stats %>% 
  #     dplyr::filter(date == input$date_input3 & position_name != "Goal Keeper") %>%
  #     rename(Name = athlete_name,  
  #            `Session RPE` = rpe,
  #            `Field Time (h:m:s)` = field_time,
  #            `Total Distance (m)` = total_distance,
  #            `High Speed Distance (m)` = high_speed_distance,
  #            `Avg Speed (m/min)`= meterage_per_minute,
  #            `Avg HR (bpm)` = mean_heart_rate,
  #            `Avg HR (%MaxHR)` = percentage_avg_heart_rate, 
  #            `Max HR (bpm)` = max_heart_rate,
  #            `Max HR (%MaxHR)` = percentage_max_heart_rate) %>% 
  #     select(Name, `Session RPE`,  `Field Time (h:m:s)`, `Total Distance (m)`,`High Speed Distance (m)`, `Avg Speed (m/min)`, 
  #            `Avg HR (bpm)`, `Avg HR (%MaxHR)`,`Max HR (bpm)`, `Max HR (%MaxHR)`) %>%
  #     mutate(across(where(is.numeric), ~if_else(.x==0, NA_real_, .x)))
  #   
  #   player_load_avg <- player_load_summary %>% 
  #     summarize(across(where(is.numeric), ~mean(.x, na.rm=T))) %>% 
  #     mutate(Name = "Average")
  #   
  #   rbind(player_load_summary,player_load_avg) %>%  
  #     mutate(across(where(is.numeric) & !`Avg Speed (m/min)`, round),
  #            `Avg Speed (m/min)`=round(`Avg Speed (m/min)`,1),
  #            `Field Time (h:m:s)` = as.character(as_hms(`Field Time (h:m:s)`)))
  # })
  # 
  # player_load_footer <- reactive({ withTags(table(
  #   tableHeader(colnames(player_load_table())),
  #   tableFooter(str_replace_all(str_replace_all(as.character(player_load_table()[nrow(player_load_table()),]), "NaN", ""), "NA", ""))))
  #   
  # })
  # 
  # 
  # keeper_load_table  <- reactive({ 
  #   
  #   keeper_load_summary <- stats %>% 
  #     dplyr::filter(date == input$date_input3 & position_name == "Goal Keeper") %>% 
  #     rename(Name = athlete_name,  
  #            `Session RPE` = rpe,
  #            `Field Time (h:m:s)` = field_time,
  #            `Total Distance (m)` = total_distance,
  #            `Dive Count` = dive_count,
  #            `Dive Load`= total_dive_load,
  #            `Avg HR (bpm)` = mean_heart_rate,
  #            `Avg HR (%MaxHR)` = percentage_avg_heart_rate, 
  #            `Max HR (bpm)` = max_heart_rate,
  #            `Max HR (%MaxHR)` = percentage_max_heart_rate) %>% 
  #     select(Name, `Session RPE`,  `Field Time (h:m:s)`, `Total Distance (m)`,`Dive Count`, `Dive Load`,
  #            `Avg HR (bpm)`, `Avg HR (%MaxHR)`,`Max HR (bpm)`, `Max HR (%MaxHR)`)  %>%
  #     mutate(across(where(is.numeric), ~if_else(.x==0, NA_real_, .x)))
  #   
  #   keeper_load_avg <- keeper_load_summary %>% 
  #     summarize(across(where(is.numeric), ~mean(.x, na.rm=T))) %>% 
  #     mutate(Name = "Average")
  #   
  #   rbind(keeper_load_summary,keeper_load_avg) %>%  
  #     mutate(across(where(is.numeric), round),
  #            `Field Time (h:m:s)` = as.character(as_hms(`Field Time (h:m:s)`)))
  # })
  # 
  # keeper_load_footer <- reactive({ withTags(table(
  #   tableHeader(colnames(keeper_load_table())),
  #   tableFooter(str_replace_all(str_replace_all(as.character(keeper_load_table()[nrow(keeper_load_table()),]), "NaN", ""), "NA", ""))))
  # })
  # 
  # 
  planned_load <- reactive({

    shiny::validate(need(!is.null(input$athlete4), "Select one or more players"))


    # player_load_stats3 <- stats %>%
    #   dplyr::filter(athlete_name %in% input$athlete4) %>%
    #   group_by(date) %>%
    #   summarize(across(where(is.numeric), ~mean(.x,na.rm=T))) %>%
    #   ungroup %>%
    #   select(c(date, input$acwr_param2))
    #
    # uniroot.all(function(x) ((sum(player_load_stats3 %>% dplyr::filter(date > max(date)-days(6)) %>% select(!date) %>% pull())+x)/7)/((sum(player_load_stats3 %>% dplyr::filter(date > max(date)-days(27)) %>% select(!date) %>% pull())+x)/28)-input$acwr_input,lower=0, upper = max(player_load_stats3 %>% select(!date) %>% pull(),na.rm=T)*2)

    player_load_stats3 <- stats %>%
      dplyr::filter(athlete_name %in% input$athlete4) %>%
      group_by(date) %>%
      summarize(across(where(is.numeric), ~mean(.x,na.rm=T))) %>%
      ungroup %>%
      select(date | !!rlang::sym(input$acwr_param2) | ((contains("al_ewma") | contains("cl_ewma")) & contains(input$acwr_param2)))

    uniroot.all(function(x) ((1-BETA(7))*(player_load_stats3 %>% dplyr::filter(date == max(date)) %>% select(contains("al_ewma")) %>% pull()) + BETA(7)*x)/((1-BETA(28))*(player_load_stats3 %>% dplyr::filter(date == max(date)) %>% select(contains("cl_ewma")) %>% pull()) + BETA(28)*x)-input$acwr_input,lower=0, upper = max(player_load_stats3 %>% select(!date & !contains("al_ewma") & ! contains("cl_ewma")) %>% pull(),na.rm=T)*2)
  })
  
  output$TotalDistanceGroupAvg <- renderPlotly(distance_group_avg_plot())
  
  output$HSDistanceGroupAvg <- renderPlotly(hs_distance_group_avg_plot())
  
  output$DistanceByPlayer <- renderPlotly(distance_by_player_plot())
  
  output$TotalDistanceDrillGroupAvg <- renderPlotly(distance_drill_group_avg_plot())
  
  output$HSDistanceDrillGroupAvg <- renderPlotly(hs_distance_drill_group_avg_plot())
  
  output$DistanceDrillByPlayer <- renderPlotly(distance_drill_by_player_plot())
  
  output$MDDistancePerHalf <- renderPlotly(md_distance_per_half_plot())
  
  output$MDTotalDistanceByPlayer <- renderPlotly(md_total_distance_by_player_plot())
  
  output$MDHSRDistanceByPlayer <- renderPlotly(md_hsr_distance_by_player_plot())
  
  output$MDSprintDistanceByPlayer <- renderPlotly(md_sprint_distance_by_player_plot())
  
  output$MDTotalDistancePerMinByPlayer <- renderPlotly(md_total_distance_per_min_by_player_plot())
  
  output$MDHSRDistancePerMinByPlayer <- renderPlotly(md_hsr_distance_per_min_by_player_plot())
  
  output$MDSprintDistancePerMinByPlayer <- renderPlotly(md_sprint_distance_per_min_by_player_plot())
  
  output$MDComparisonTotalDistance <- renderPlotly(md_comparison_total_distance())
  
  output$MDComparisonHSRDistance <- renderPlotly(md_comparison_hsr_distance())
  
  output$MDComparisonSprintDistance <- renderPlotly(md_comparison_sprint_distance())
  
  output$MDComparisonTotalDistancePerMin <- renderPlotly(md_comparison_total_distance_per_min())
  
  output$MDComparisonHSRDistancePerMin <- renderPlotly(md_comparison_hsr_distance_per_min())
  
  output$MDComparisonSprintDistancePerMin <- renderPlotly(md_comparison_sprint_distance_per_min())
  
  output$MDTotalDistance15min <- renderPlotly(md_total_distance_15min())
  
  output$MDHSRDistance15min <- renderPlotly(md_hsr_distance_15min())
  
  output$MDSprintDistance15min <- renderPlotly(md_sprint_distance_15min())
  
  output$MDTotalDistancePerMin15min <- renderPlotly(md_total_distance_per_min_15min())
  
  output$MDHSRDistancePerMin15min <- renderPlotly(md_hsr_distance_per_min_15min())
  
  output$MDSprintDistancePerMin15min <- renderPlotly(md_sprint_distance_per_min_15min())
  
  output$AcuteChronicLoad <- renderPlotly(acute_chronic_load_plot())
  
  output$IntExtLoad <- renderPlotly(int_ext_load_plot())
  
  output$SubExtLoad <- renderPlotly(sub_ext_load_plot())
  
  output$WellnessWorkload <- renderPlotly(wellness_workload_plot())
  
  # output$ReadinessWellness <- renderPlotly(readiness_wellness_plot())
  
  output$Wellness <- renderPlotly(wellness_plot())
  
  output$HistoricalWellness <- renderPlotly(historical_wellness_plot())
  
  output$RPE <- renderPlotly(rpe_plot())

  
  output$PlayerDailySummaryTable <- renderReactable({player_daily_summary_table()})
  
  
  output$DrillSummaryTable <- renderReactable({drill_summary_table()})

  output$MatchDayTable <- renderReactable({match_day_table()})
  
  
  # output$PlannedvsActualTable <-renderDT({planned_actual_table()},
  #                                        container = planned_actual_header(), extensions = 'Buttons',
  #                                        options = list(dom = 'lfrti',info =F, lengthChange = F, pageLength = nrow(planned_actual_table()), searching = F),  
  #                                        rownames= FALSE)
  
# 
#   
#   output$PlayerSummaryTable <-renderDT({player_summary_table()[-nrow(player_summary_table()),]},
#                                        container = player_summary_footer(), extensions = 'Buttons',
#                                        options = list(dom = 'lfrti',info =F, lengthChange = F, pageLength = nrow(player_summary_table()), searching = F),  
#                                        rownames= FALSE)
#   
#   output$KeeperSummaryTable <-renderDT({keeper_summary_table()[-nrow(keeper_summary_table()),]},
#                                        container = keeper_summary_footer(), extensions = 'Buttons',
#                                        options = list(dom = 'lfrti',info =F, lengthChange = F, pageLength = nrow(keeper_summary_table()), searching = F),  
#                                        rownames= FALSE)  
#   
#   output$PlayerLoadTable <-renderDT({player_load_table()[-nrow(player_load_table()),]},
#                                     container = player_load_footer(), extensions = 'Buttons',
#                                     options = list(dom = 'lfrti',info =F, lengthChange = F, pageLength = nrow(player_load_table()), searching = F),  
#                                     rownames= FALSE)
#   # caption = tags$caption("Player Load Summary", style="caption-side:top;font-weight:bold;font-size:18px;color:black")
#   # buttons = list(list(extend = 'collection', buttons = c('csv', 'excel', 'pdf'), text = 'Download'))
#   
#   output$KeeperLoadTable <-renderDT({keeper_load_table()[-nrow(keeper_load_table()),]},
#                                     container = keeper_load_footer(), extensions = 'Buttons',
#                                     options = list(dom = 'lfrti',info =F, lengthChange = F, pageLength = nrow(keeper_load_table()), searching = F),  
#                                     rownames= FALSE)

  
  # output$HydrationValueBoxes <- renderUI({
  #   output_list <- list()
  #   for(athlete in unique(hydration_data$athlete_name)){
  #     output_list[[athlete]] <- valueBoxOutput(outputId = athlete, width=3)
  #   }
  #   return(output_list)})
  # 
  # observe({
  #   for(athlete in unique(hydration_data$athlete_name)){
  #     local({
  #       athlete <- athlete
  #       input_date <- input$date_input4
  #       weight_change <- hydration_data %>% dplyr::filter(athlete_name == athlete & date == input_date) %>% pull(weight_change)
  #       
  #       output[[athlete]] <-  renderValueBox({
  #         
  #         # shiny::validate(need(!is_empty(weight_change), paste0(athlete, ": No Data")))
  #         
  #         valueBox(value = scales::percent(weight_change, 0.1),subtitle= athlete,color = case_when(is.na(weight_change) ~"purple", weight_change > -0.01 ~ "green", weight_change <= -0.01 & weight_change > -0.02 ~ "yellow", weight_change <= -0.02 & weight_change > -0.03 ~ "orange",weight_change <= -0.03 ~ "red",.default = "purple"), width=12)
  #       })})}
  # }) 
  
  
  
  output$total_distance_valuebox <- renderUI({
    
    total_distance <- stats %>%
      mutate(across(where(is.numeric), ~if_else(field_time == 0 & total_distance == 0, NA_real_, .x))) %>% 
      select(athlete_name, date, total_distance) %>% 
      dplyr::filter(athlete_name %in% input$athlete6 & date == input$date_input5) %>%
      summarize(total_distance=mean(total_distance,na.rm=T)) %>% 
      pull(total_distance)
    
    value_box(title="Total Distance (m)",
              value = round(total_distance), 
              # showcase = bsicons::bs_icon("activity"),
              theme_color = "success")
    
  })
  
  output$high_speed_distance_valuebox <- renderUI({
    
    high_speed_distance <- stats %>%
      mutate(across(where(is.numeric), ~if_else(field_time == 0 & total_distance == 0, NA_real_, .x))) %>% 
      select(athlete_name, date, high_speed_distance) %>% 
      dplyr::filter(athlete_name %in% input$athlete6 & date == input$date_input5) %>%
      summarize(high_speed_distance=mean(high_speed_distance,na.rm=T)) %>% 
      pull(high_speed_distance)
    
    value_box(title="HSR Distance (m)",
              value = round(high_speed_distance), 
              # showcase = bsicons::bs_icon("activity"),
              theme_color = "success")
    
  })
  
  output$sprint_distance_valuebox <- renderUI({
    
    sprint_distance <- stats %>%
      mutate(across(where(is.numeric), ~if_else(field_time == 0 & total_distance == 0, NA_real_, .x))) %>% 
      select(athlete_name, date, sprint_distance) %>% 
      dplyr::filter(athlete_name %in% input$athlete6 & date == input$date_input5) %>%
      summarize(sprint_distance=mean(sprint_distance,na.rm=T)) %>% 
      pull(sprint_distance)
    
    value_box(title="Sprint Distance (m)",
              value = round(sprint_distance), 
              # showcase = bsicons::bs_icon("activity"),
              theme_color = "success")
    
  })
  
  
  output$accel_efforts_valuebox <- renderUI({
    
    accel_efforts <- stats %>%
      mutate(across(where(is.numeric), ~if_else(field_time == 0 & total_distance == 0, NA_real_, .x))) %>% 
      select(athlete_name, date, accel_efforts) %>% 
      dplyr::filter(athlete_name %in% input$athlete6 & date == input$date_input5) %>%
      summarize(accel_efforts=mean(accel_efforts,na.rm=T)) %>% 
      pull(accel_efforts)
    
    value_box(title="Accel Efforts (#)",
              value = round(accel_efforts), 
              # showcase = bsicons::bs_icon("activity"),
              theme_color = "success")
    
  })
  
  output$decel_efforts_valuebox <- renderUI({
    
    decel_efforts <- stats %>%
      mutate(across(where(is.numeric), ~if_else(field_time == 0 & total_distance == 0, NA_real_, .x))) %>% 
      select(athlete_name, date, decel_efforts) %>% 
      dplyr::filter(athlete_name %in% input$athlete6 & date == input$date_input5) %>%
      summarize(decel_efforts=mean(decel_efforts,na.rm=T)) %>% 
      pull(decel_efforts)
    
    value_box(title="Decel Efforts (#)",
              value = round(decel_efforts), 
              # showcase = bsicons::bs_icon("activity"),
              theme_color = "success")
    
  })
  
  output$max_vel_valuebox <- renderUI({
    
    max_vel <- stats %>%
      mutate(across(where(is.numeric), ~if_else(field_time == 0 & total_distance == 0, NA_real_, .x))) %>% 
      select(athlete_name, date, max_vel_kph) %>% 
      dplyr::filter(athlete_name %in% input$athlete6 & date == input$date_input5) %>%
      summarize(max_vel_kph=mean(max_vel_kph,na.rm=T)) %>% 
      pull(max_vel_kph)
    
    value_box(title="Max Velocity (km/h)",
              value = round(max_vel,1), 
              # showcase = bsicons::bs_icon("activity"),
              theme_color = "success")
    
  })
  
  
  
  output$total_distance_drill_valuebox <- renderUI({
    
    total_distance <- stats_period %>%
      mutate(across(where(is.numeric), ~if_else(field_time == 0 & total_distance == 0, NA_real_, .x))) %>% 
      select(athlete_name, date, period_name, total_distance) %>% 
      dplyr::filter(athlete_name %in% input$athlete7 & date == input$date_input6 & period_name %in% input$period_input) %>%
      group_by(athlete_name) %>%
      summarize(total_distance=sum(total_distance,na.rm=T)) %>% 
      ungroup %>% 
      summarize(total_distance=mean(total_distance,na.rm=T)) %>% 
      pull(total_distance)
    
    value_box(title="Total Distance (m)",
              value = round(total_distance), 
              # showcase = bsicons::bs_icon("activity"),
              theme_color = "success")
    
  })
  
  output$high_speed_distance_drill_valuebox <- renderUI({
    
    high_speed_distance <- stats_period %>%
      mutate(across(where(is.numeric), ~if_else(field_time == 0 & total_distance == 0, NA_real_, .x))) %>% 
      select(athlete_name, date, period_name, high_speed_distance) %>% 
      dplyr::filter(athlete_name %in% input$athlete7 & date == input$date_input6 & period_name %in% input$period_input) %>%
      group_by(athlete_name) %>%
      summarize(high_speed_distance=sum(high_speed_distance,na.rm=T)) %>% 
      ungroup %>% 
      summarize(high_speed_distance=mean(high_speed_distance,na.rm=T)) %>% 
      pull(high_speed_distance)
    
    
    value_box(title="HSR Distance (m)",
              value = round(high_speed_distance), 
              # showcase = bsicons::bs_icon("activity"),
              theme_color = "success")
    
  })
  
  output$sprint_distance_drill_valuebox <- renderUI({
    
    sprint_distance <- stats_period %>%
      mutate(across(where(is.numeric), ~if_else(field_time == 0 & total_distance == 0, NA_real_, .x))) %>% 
      select(athlete_name, date, period_name, sprint_distance) %>% 
      dplyr::filter(athlete_name %in% input$athlete7 & date == input$date_input6 & period_name %in% input$period_input) %>%
      group_by(athlete_name) %>%      
      summarize(sprint_distance=sum(sprint_distance,na.rm=T)) %>% 
      ungroup %>% 
      summarize(sprint_distance=mean(sprint_distance,na.rm=T)) %>% 
      pull(sprint_distance)
    
    value_box(title="Sprint Distance (m)",
              value = round(sprint_distance), 
              # showcase = bsicons::bs_icon("activity"),
              theme_color = "success")
    
  })
  
  
  output$accel_efforts_drill_valuebox <- renderUI({
    
    accel_efforts <- stats_period %>%
      mutate(across(where(is.numeric), ~if_else(field_time == 0 & total_distance == 0, NA_real_, .x))) %>% 
      select(athlete_name, date, period_name, accel_efforts) %>% 
      dplyr::filter(athlete_name %in% input$athlete7 & date == input$date_input6 & period_name %in% input$period_input) %>%
      group_by(athlete_name) %>%
      summarize(accel_efforts=sum(accel_efforts,na.rm=T)) %>% 
      ungroup %>% 
      summarize(accel_efforts=mean(accel_efforts,na.rm=T)) %>%
      pull(accel_efforts)
    
    value_box(title="Accel Efforts (#)",
              value = round(accel_efforts), 
              # showcase = bsicons::bs_icon("activity"),
              theme_color = "success")
    
  })
  
  output$decel_efforts_drill_valuebox <- renderUI({
    
    decel_efforts <- stats_period %>%
      mutate(across(where(is.numeric), ~if_else(field_time == 0 & total_distance == 0, NA_real_, .x))) %>% 
      select(athlete_name, date, period_name, decel_efforts) %>% 
      dplyr::filter(athlete_name %in% input$athlete7 & date == input$date_input6 & period_name %in% input$period_input) %>%
      group_by(athlete_name) %>%
      summarize(decel_efforts=sum(decel_efforts,na.rm=T)) %>% 
      ungroup %>% 
      summarize(decel_efforts=mean(decel_efforts,na.rm=T)) %>% 
      pull(decel_efforts)
    
    value_box(title="Decel Efforts (#)",
              value = round(decel_efforts), 
              # showcase = bsicons::bs_icon("activity"),
              theme_color = "success")
    
  })
  
  output$max_vel_drill_valuebox <- renderUI({
    
    max_vel <- stats_period %>%
      mutate(across(where(is.numeric), ~if_else(field_time == 0 & total_distance == 0, NA_real_, .x))) %>% 
      select(athlete_name, date, period_name, max_vel_kph) %>% 
      dplyr::filter(athlete_name %in% input$athlete7 & date == input$date_input6 & period_name %in% input$period_input) %>%
      group_by(athlete_name) %>%
      summarize(max_vel_kph=max(max_vel_kph,na.rm=T)) %>% 
      ungroup %>% 
      summarize(max_vel_kph=mean(max_vel_kph,na.rm=T)) %>% 
      pull(max_vel_kph)
    
    value_box(title="Max Velocity (km/h)",
              value = round(max_vel,1), 
              # showcase = bsicons::bs_icon("activity"),
              theme_color = "success")
    
  })
  
  output$PlannedLoad <- renderUI({
    value_box(title=case_when(is_empty(planned_load()) ~"Load is < 0 or > 2 x player's max load",
                              input$acwr_param2 == "field_time"~ "Field Time (s)", 
                              input$acwr_param2 == "total_distance"~"Total Distance (m)", 
                              input$acwr_param2 =="high_speed_distance"~ "High Speed Distance (m)", 
                              input$acwr_param2 == "sprint_distance"~ "Sprint Distance (m)",
                              input$acwr_param2 == "accel_efforts"~ "Accel Efforts",
                              input$acwr_param2 == "decel_efforts" ~"Decel Efforts", 
                              input$acwr_param2 == "max_heart_rate"~"Max HR (bpm)", 
                              input$acwr_param2 == "dive_count"~"Dive Count", 
                              input$acwr_param2 == "total_dive_load"~"Total Dive Load",                                                              
                              input$acwr_param2 == "explosive_efforts"~"Explosive Efforts", 
                              .default = ""),
              value = round(planned_load()), 
              showcase = bsicons::bs_icon("activity"),
              # case_when(str_detect(input$acwr_param2, "dive")~shiny::icon("person-falling", lib="font-awesome"),
              #                    str_detect(input$acwr_param2, "heart")~shiny::icon("heart-pulse", lib="font-awesome"),
              #                    str_detect(input$acwr_param2, "time")~shiny::icon("stopwatch", lib="font-awesome"),
              #                    .default = shiny::icon("person-running", lib="font-awesome")),
              theme_color = "success")
    
  })
  
  # icon = icon("person-running"), 
  
 
  
  output$download_pdf <- downloadHandler(
    filename = function() {
      paste0("Tides Match Report ",input$md_input, ".pdf")
    },
    content = function(file) {
      
      req(input$images)
      
      temp_dir <- tempdir()
      tempReport <- file.path(temp_dir, "TidesMatchReport.Rmd")
      file.copy("TidesMatchReport.Rmd", tempReport, overwrite = TRUE)
      tempStyle <- file.path(temp_dir, "style.css")
      file.copy("style.css", tempStyle, overwrite = TRUE)
      tempImage1 <- file.path(temp_dir, "Halifax.png")
      file.copy("Halifax.png", tempImage1, overwrite = TRUE)
      tempImage2 <- file.path(temp_dir, "Montreal.png")
      file.copy("Montreal.png", tempImage2, overwrite = TRUE)
      tempImage3 <- file.path(temp_dir, "Ottawa.png")
      file.copy("Ottawa.png", tempImage3, overwrite = TRUE)
      tempImage4 <- file.path(temp_dir, "Toronto.png")
      file.copy("Toronto.png", tempImage4, overwrite = TRUE)
      tempImage5 <- file.path(temp_dir, "Calgary.png")
      file.copy("Calgary.png", tempImage5, overwrite = TRUE)
      tempImage6 <- file.path(temp_dir, "Vancouver.png")
      file.copy("Vancouver.png", tempImage6, overwrite = TRUE)
      tempImage7 <- file.path(temp_dir, "Portsmouth.png")
      file.copy("Portsmouth.png", tempImage7, overwrite = TRUE)
      tempImage8 <- file.path(temp_dir, "Everton.png")
      file.copy("Everton.png", tempImage8, overwrite = TRUE)
      tempImage9 <- file.path(temp_dir, "West Ham.png")
      file.copy("West Ham.png", tempImage9, overwrite = TRUE)
      tempImage10 <- file.path(temp_dir, "AUS.png")
      file.copy("AUS.png", tempImage10, overwrite = TRUE)
      tempImage11 <- file.path(temp_dir, "vs.png")
      file.copy("vs.png", tempImage11, overwrite = TRUE)
      tempImage12 <- file.path(temp_dir, "TidesFCImage2.jpg")
      file.copy("TidesFCImage2.jpg", tempImage12, overwrite = TRUE)
      tempImage13 <- file.path(temp_dir, "TidesFCImage3.jpg")
      file.copy("TidesFCImage3.jpg", tempImage13, overwrite = TRUE)
      tempImage14 <- file.path(temp_dir, "TidesFCImage4.jpg")
      file.copy("TidesFCImage4.jpg", tempImage14, overwrite = TRUE)
      tempImage15 <- file.path(temp_dir, "TidesFCImage5.jpg")
      file.copy("TidesFCImage5.jpg", tempImage15, overwrite = TRUE)
      

      # 1. Convert to Base64 strings (Same optimized collection logic)
      b64_strings <- c()
      for (i in 1:nrow(input$images)) {
        filepath <- input$images$datapath[i]
        ext <- tools::file_ext(input$images$name[i])
        mime_type <- ifelse(tolower(ext) == "png", "image/png", "image/jpeg")
        encoded <- base64enc::base64encode(filepath)
        b64_strings <- c(b64_strings, paste0("data:", mime_type, ";base64,", encoded))
      }
      
      # 5. Prevent timeouts: Apply the fixes discussed previously
      old_timeout <- getOption("pagedown.timeout")
      options(pagedown.timeout = 120) 
      on.exit(options(pagedown.timeout = old_timeout), add = TRUE)
      
      
      # Set up parameters to pass to Rmd document
      report_params <- list(md_input = input$md_input, 
                            image1 = b64_strings[1],
                            image2 = b64_strings[2],
                            image3 = b64_strings[3],
                            stats_period = stats_period)
      
      # Render to intermediate HTML file
      temp_html <- rmarkdown::render(
        input = tempReport,
        params = report_params,
        envir = new.env(parent = globalenv())
      )
      
      # Convert intermediate HTML to the final target PDF file path via headless Chrome
      pagedown::chrome_print(
        input = temp_html,
        output = file,
        timeout = 120,
        extra_args = c("--disable-gpu", "--no-sandbox")
      )
      
      # extra_args = c("--disable-gpu", "--no-sandbox", "--js-flags=--max-old-space-size=4096",  "--virtual-time-budget=10000")
    }
  )
  
}

shinyApp(ui, server)





