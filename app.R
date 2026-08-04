library(shiny)
library(shinyWidgets)
library(bslib)
library(DT)
library(reactable)
library(reactable.extras)
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
library(RColorBrewer)


options(digits = 12, 
        reactable.theme = reactableTheme(
          color = "#221C35",
          stripedColor =rgb(229, 225, 230, round(0.4 * 255),maxColorValue = 255),
          highlightColor = rgb(0, 176, 185,alpha=(0.6*255), maxColorValue = 255),
          style = list(fontFamily = "-apple-system, BlinkMacSystemFont, Segoe UI, Helvetica, Arial, sans-serif"),
          headerStyle = list(borderColor = "#221C35", backgroundColor = "#221C35", color = "white", fontWeight = "bold"),
          borderColor = "#221C35",
          borderWidth = "1.5px",
          groupHeaderStyle = list(borderColor = "#221C35", backgroundColor = "#00B0B9", color = "white", fontWeight = "bold")
        )
)



ui <- uiOutput("page_content") # Placeholder for either login or dashboard UI



server <- function(input, output, session) {
  
  

  
  googledrive::drive_auth(path=gargle::secret_decrypt_json(here::here(".secrets", "halifaxtidesdashboard-serviceaccount-encrypted.json"), "googledrive_token"))
  
  googledrive::drive_download(googledrive::as_id(Sys.getenv("load_plan_file_id")), path="Loading Plan.csv", overwrite=T)
  
  loading_plan <- read_csv("Loading Plan.csv", show_col_types =F) 
  
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
  
  # Download catapult data from database
  
  catapult_url <- "https://connect-us.catapultsports.com"
  
  
  googledrive::drive_auth(path=gargle::secret_decrypt_json(here::here(".secrets", "halifaxtidesdashboard-serviceaccount-encrypted.json"), "googledrive_token"))
  
  googledrive::drive_download(googledrive::as_id(Sys.getenv("stats_period_file_id")), path="Catapult Stats By Period.csv", overwrite=T)
  googledrive::drive_download(googledrive::as_id(Sys.getenv("stats_activity_file_id")), path="Catapult Stats By Activity.csv", overwrite=T)
  googledrive::drive_download(googledrive::as_id(Sys.getenv("activities_file_id")), path="Catapult Activities.csv", overwrite=T)
 
  stats_activity_db <- read_csv("Catapult Stats By Activity.csv", show_col_types =F) %>% 
    mutate(modified_at = with_tz(modified_at,tzone="UTC"))
  
  stats_period_db <- read_csv("Catapult Stats By Period.csv", show_col_types =F) %>% 
    mutate(modified_at = with_tz(modified_at,tzone="UTC"))
  
  activities_db <- read_csv("Catapult Activities.csv", show_col_types =F)  %>% 
    mutate(modified_at = with_tz(modified_at,tzone="UTC"))
  
  
  
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
    mutate(modified_at = as.POSIXct(modified_at, tz="UTC")) %>% 
    select(id, name, modified_at, start_time, end_time, tag_list) %>%
    rename(activity_id=id, activity_name=name) %>% 
    unnest(tag_list) %>% 
    rename(tag_id=id) %>% 
    dplyr::filter(tag_type_name == "DayCode")

  
  date_from <- max(activities_db$modified_at)
  
  
  if (max(activities$modified_at) > date_from) {
    
    athletes_filter <- athletes_catapult$id
    activities_filter <- activities %>% dplyr::filter(modified_at > date_from) %>% pull(activity_id)
    
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
      left_join(activities %>% select(c(activity_id, modified_at, tag_name)), by=join_by(activity_id), relationship="many-to-many")
    
    
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
      left_join(activities %>% select(c(activity_id, modified_at, tag_name)), by=join_by(activity_id), relationship="many-to-many")
    
    
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
  
  stats_activity_db_localtime <- stats_activity_db %>% 
    mutate(across(c(start_time, end_time), ~ as.POSIXct(.x, tz="")),
           date= as.Date(date, "%d/%m/%Y"))
  
  stats_period_db_localtime <- stats_period_db %>% 
    mutate(across(c(start_time, end_time), ~ with_tz(.x, tzone="")),
           date= as.Date(date, "%d/%m/%Y"))
  
  metrics <- data.frame(
    athlete_name = athletes_catapult %>% pull(athlete_name),
    bench_time=0, field_time=0, total_distance=0, meterage_per_minute=0, velocity_band5_total_distance=0,
    velocity_band6_total_distance=0, velocity2_band6_total_distance=0, max_vel=0, percentage_max_velocity=0,
    gen2_acceleration_band7plus_total_effort_count=0, gen2_acceleration_band2plus_total_effort_count=0,
    heart_rate_band5_average_duration_session=0, heart_rate_band6_average_duration_session=0,heart_rate_band7_average_duration_session=0,
    heart_rate_band8_average_duration_session=0,mean_heart_rate=0,percentage_avg_heart_rate=0,percentage_max_heart_rate=0,max_heart_rate=0,athlete_max_hr=0,
    total_goalkeeping_dives=0,total_dives_left=0,total_dives_right=0, total_dives_centre=0,`low_dive_load_(avg)`=0, `med_dive_load_(avg)`=0, `high_dive_load_(avg)`=0,
    diveloadleft_band1_average_count_session=0, diveloadright_band1_average_count_session=0,diveloadcentre_band1_average_count_session=0,
    diveloadleft_band2_average_count_session=0, diveloadright_band2_average_count_session=0, diveloadcentre_band2_average_count_session=0,
    diveloadleft_band3_average_count_session=0, diveloadright_band3_average_count_session=0, diveloadcentre_band3_average_count_session=0,
    total_dive_load=0,total_dive_load_left=0,total_dive_load_right=0,total_dive_load_centre=0, total_dive_load_low_intensity=0, total_dive_load_med_intensity=0,total_dive_load_high_intensity=0,
    total_dive_load_left_low_intensit=0,total_dive_load_right_low_intensity=0, total_dive_load_centre_low_intensity=0,total_dive_load_left_med_intensity=0,total_dive_load_right_med_intensity=0, 
    total_dive_load_centre_med_intensity=0,total_dive_load_left_high_intensity=0,total_dive_load_right_high_intensity=0, total_dive_load_centre_high_intensity=0,
    median_time_to_feet=0, average_time_to_feet=0, average_time_to_feeet_left=0, average_time_to_feeet_right=0, average_time_to_feet_centre=0,
    explosive_efforts=0, total_jumps=0, ima_accels=0, ima_decels=0)
  
  # metrics <- stats_activity_db_localtime %>% 
  #   mutate(across(where(is.numeric),~replace(.x,1:nrow(stats_activity_db_localtime),0))) %>% 
  #   select(athlete_name | where(is.numeric)) %>% 
  #   distinct
  

  dates <- data.frame(athlete_name = athletes_catapult$athlete_name) %>% 
    group_by(athlete_name) %>% 
    reframe(date =seq.Date(min(stats_activity_db_localtime$date), if_else(max(stats_activity_db_localtime$date)>=Sys.Date(), max(stats_activity_db_localtime$date),Sys.Date()), by='days')) %>% 
    left_join(metrics, by=join_by(athlete_name)) %>% 
    mutate(name_date = paste0(athlete_name, date)) %>% 
    filter(!(name_date %in% paste0(stats_activity_db_localtime$athlete_name,stats_activity_db_localtime$date))) %>% 
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
  
  stats <- stats_activity_db_localtime %>% 
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
    mutate(tag_name=if_else(is.na(tag_name) & date != Sys.Date(), "OFF",tag_name))
  
  stats_period <- stats_period_db_localtime %>% 
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
           tag_name=if_else(is.na(tag_name) & date != Sys.Date(), "OFF",tag_name)) %>%
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
    multiple = F)
  
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
  
  # observe({
  #   
  #   req(input$athlete2)
  #   
  #   if ("Goal Keeper" %in% (stats %>% filter(athlete_name == input$athlete2) %>% drop_na(position_name) %>% pull(position_name) %>% unique)){
  #     
  #     updateSelectInput(
  #       inputId="ext_load_param", 
  #       label = "External Workload Parameter",
  #       choices = c("Total Distance" = "external_load_total_distance", "Field Time" = "external_load_field_time", 
  #         "Dive Count" = "external_load_dive_count", "Total Dive Load" = "external_load_total_dive_load", 
  #         "Explosive Efforts" = "external_load_explosive_efforts"),
  #       selected = "Total Distance")
  #     
  #     updateSelectInput(
  #       inputId="workload_param", 
  #       label = "Workload Parameter",
  #       choices = c("Total Distance" = "workload_total_distance", "Field Time" = "workload_field_time", 
  #         "Dive Count" = "workload_dive_count", "Total Dive Load" = "workload_total_dive_load", 
  #         "Explosive Efforts" = "workload_explosive_efforts", "Avg HR"="workload_mean_heart_rate", 
  #         "Max HR"="workload_max_heart_rate","RPE" = "workload_rpe"),
  #       selected = "Total Distance")
  #     
  #   } else {
  #     
  #     updateSelectInput(
  #       inputId="ext_load_param", 
  #       label = "External Workload Parameter",
  #       choices = c("Total Distance" = "external_load_total_distance", "High Speed Distance" = "external_load_high_speed_distance", "Sprint Distance" = "external_load_sprint_distance", 
  #         "Field Time" = "external_load_field_time", "Meterage per Minute" = "external_load_meterage_per_minute", "Max Velocity" = "external_load_max_vel_kph", 
  #         "Accel Efforts" = "external_load_accel_efforts", "Decel Efforts" = "external_load_decel_efforts","Accel + Decel Efforts" = "external_load_accel_decel_efforts"),
  #       selected = "Total Distance")
  #     
  #     updateSelectInput(
  #       inputId="workload_param", 
  #       label = "Workload Parameter",
  #       choices = c("Total Distance" = "workload_total_distance", "High Speed Distance" = "workload_high_speed_distance", "Sprint Distance" = "workload_sprint_distance", 
  #         "Field Time" = "workload_field_time", "Meterage per Minute" = "workload_meterage_per_minute", "Max Velocity" = "workload_max_vel_kph", 
  #         "Accel Efforts" = "workload_accel_efforts", "Decel Efforts" = "workload_decel_efforts","Accel + Decel Efforts" = "workload_accel_decel_efforts",
  #         "Avg HR"="workload_mean_heart_rate", "Max HR"="workload_max_heart_rate","RPE" = "workload_rpe"),
  #       selected = "Total Distance")
  #   }
  #   
  # }) %>% bindEvent(input$athlete2)
  
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
      title = div(img(src = "HfxTidesFC.png", height = "40px", style = "margin-right: 10px;"), "Tides FC Load Monitoring"),
      sidebar=NULL,
      fillable = T,
      nav_spacer(),
      nav_panel(title="Daily Report", 
                layout_sidebar(      
                  sidebar = sidebar(athlete6, date_input5, 
                                    actionButton("build_pdf2", "Generate Report", icon = icon("file-lines"), class = "btn-primary"),
                                    uiOutput("download_wrapper2"),
                                    bg = "#E5E1E6"),
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
      nav_panel(title="Match Report",
                layout_sidebar(
                  sidebar = sidebar(athlete8, md_input, 
                                    fileInput("images","Select Image Files", multiple = T,accept = "image/*", width="100%"),
                                    # downloadButton("download_pdf", "Download Match Report"), 
                                    actionButton("build_pdf", "Generate Report", icon = icon("file-lines"), class = "btn-primary"),
                                    uiOutput("download_wrapper"),
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
      ),
      nav_panel(title="Acute:Chronic Workload", 
                layout_sidebar(      
                  sidebar = sidebar(athlete1, date_range1, acwr_param, bg = "#E5E1E6"),
                  card(
                    height = 400,
                    full_screen = TRUE,
                    card_header("Acute:Chronic Workload"),
                    card_body(min_height = 200, plotlyOutput("AcuteChronicLoad"))
                  ),
                  card(
                    full_screen = TRUE,
                    card_header("Workload Planning"),
                    card_body(min_height = 200,
                              div(style = "padding-bottom: 5px;",
                                  actionButton("update_table_btn", "Update Table", icon = icon("sync")),
                                  actionButton("clear_table_btn", "Clear Edits", icon=icon("eraser"),class = "btn-danger"),
                                  prettyRadioButtons("select_week", label="", choices=c("Current Week", "Next Week"), selected="Current Week", inline=T),
                              ),
                              reactableOutput("AcuteChronicTable"))
                  ),
                  card(
                    full_screen = TRUE,
                    card_header("Goal Keeper Workload Planning"),
                    card_body(min_height = 200,
                              div(style = "padding-bottom: 5px;",
                                  actionButton("update_gk_table_btn", "Update Table", icon = icon("sync")),
                                  actionButton("clear_gk_table_btn", "Clear Edits", icon=icon("eraser"),class = "btn-danger"),
                                  prettyRadioButtons("select_week_gk",label="", choices=c("Current Week", "Next Week"), selected="Current Week", inline=T),
                              ),
                              reactableOutput("AcuteChronicGKTable"))
                  )
                )
      ),
      nav_panel(title="Quadrant Graphs",
                layout_sidebar(
                  sidebar = sidebar(athlete2, date_input1, ext_load_param, workload_param, bg = "#E5E1E6"),
                  layout_column_wrap(
                    width=1/2,
                    # card(
                    #   height = 400,
                    #   full_screen = TRUE,
                    #   card_header("Internal vs. External Workload"),
                    #   card_body(min_height = 200, plotlyOutput("IntExtLoad"))
                    # ),
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
                    )
                    # ,
                    # card(
                    #   height = 400,
                    #   full_screen = TRUE,
                    #   card_header("Readiness vs. Wellness"),
                    #   card_body(min_height = 200, plotlyOutput("ReadinessWellness"))
                    # )
                  ),
                  card(
                    full_screen = TRUE,
                    card_header("Z-Score Summary Table"),
                    card_body(min_height = 200, reactableOutput("ZScoreHeatmapTable"))
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
                  sidebar = sidebar(athlete3, date_range3, bg = "#E5E1E6"),
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
                    card_header("Wellness History"),
                    layout_sidebar(
                      sidebar = sidebar(wellness_param, bg = "#E5E1E6"),
                      card_body(min_height = 200,plotlyOutput("HistoricalWellness"))
                    )
                  ),
                  card(
                    full_screen = TRUE,
                    card_header("Wellness History Table"),
                    card_body(min_height = 200,reactableOutput("WellnessHistoryTable"))
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
      # nav_panel(title="Load Planning",
      #           layout_sidebar(sidebar = sidebar(athlete4, acwr_param2, acwr_input, bg = "#E5E1E6"),
      #                          card(
      #                            height = 400,
      #                            full_screen = TRUE,
      #                            card_header("Load Planning"),
      #                            card_body(min_height = 200, uiOutput("PlannedLoad"))
      #                          )
      #           )        
      # ),
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
      dplyr::filter(athlete_name %in% input$athlete8 & activity_name == input$md_input & str_detect(period_name, "^\\d{1,2}\\. [[:alpha:]]{5,6} (?i)Half$")) %>%
      # dplyr::filter(activity_name == "18th May 2026 - MD 4  vs Vancouver (H)" & str_detect(period_name, "^\\d{1,2}\\. [[:alpha:]]{5,6} (?i)Half$")) %>%
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
    dplyr::filter(athlete_name %in% input$athlete8 & activity_name == input$md_input & str_detect(period_name, "^\\d{1,2}\\. [[:alpha:]]{5,6} (?i)Half$")) %>%
    # dplyr::filter(activity_name == "18th May 2026 - MD 4  vs Vancouver (H)" & str_detect(period_name, "^\\d{1,2}\\. [[:alpha:]]{5,6} (?i)Half$")) %>%
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
    #   dplyr::filter(athlete_name %in% input$athlete8 & activity_name == input$md_input & str_detect(period_name, "^\\d{1,2}\\. [[:alpha:]]{5,6} (?i)Half$")) %>%
    #   # dplyr::filter(activity_name == "18th May 2026 - MD 4  vs Vancouver (H)" & str_detect(period_name, "^\\d{1,2}\\. [[:alpha:]]{5,6} (?i)Half$")) %>%
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
      dplyr::filter(athlete_name %in% input$athlete8 & tag_name == "MD" & date >= as.Date("2026-04-01") & date <= (stats_period %>% filter(activity_name == input$md_input) %>% pull(date) %>% unique) & str_detect(period_name, "^\\d{1,2}\\. [[:alpha:]]{5,6} (?i)Half$")) %>%
      # dplyr::filter(tag_name == "MD" & date >= as.Date("2026-04-01") & date <= (stats_period %>% filter(activity_name ==  "24th May 2026 - MD 5 vs Calgary (A)") %>% pull(date) %>% unique) & str_detect(period_name, "^\\d{1,2}\\. [[:alpha:]]{5,6} (?i)Half$")) %>%
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
      dplyr::filter(athlete_name %in% input$athlete8 & activity_name == input$md_input & str_detect(period_name, "^\\d{1,2}\\. [[:alpha:]]{5,6} (?i)Half$")) %>%
      # dplyr::filter(activity_name == "12th April 2026 - MD vs Portsmouth (A)" & str_detect(period_name, "^\\d{1,2}\\. [[:alpha:]]{5,6} (?i)Half$")) %>%
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
      dplyr::filter(athlete_name %in% input$athlete8 & activity_name == input$md_input & str_detect(period_name, "^\\d{1,2}\\. [[:alpha:]]{5,6} (?i)Half$")) %>%
      # dplyr::filter(activity_name == "12th April 2026 - MD vs Portsmouth (A)" & str_detect(period_name, "^\\d{1,2}\\. [[:alpha:]]{5,6} (?i)Half$")) %>%
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
          list(type = "rect", xref='paper', yref='y2', x0 = 0, x1 = 1, y0 = 0.8, y1 = 1.2, layer = "below", fillcolor = rgb(229, 225, 230, round(0.3 * 255),maxColorValue = 255), line = list(color = rgb(229, 225, 230,round(0.3 * 255),maxColorValue = 255))),
          list(type = "rect", xref='paper', yref='y2', x0 = 0, x1 = 1, y0 = 0.7, y1 = 0.8, layer = "below", fillcolor = rgb(229, 225, 230, round(0.6 * 255),maxColorValue = 255),  line = list(color = rgb(229, 225, 230,round(0.6 * 255),maxColorValue = 255))),
          list(type = "rect", xref='paper', yref='y2', x0 = 0, x1 = 1, y0 = 1.2, y1 = 1.3, layer = "below", fillcolor = rgb(229, 225, 230,round(0.6 * 255),maxColorValue = 255),  line = list(color = rgb(229, 225, 230,round(0.6 * 255),maxColorValue = 255)))
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
  
  
  # 1. Initialize a reactive tracking value for user modifications
  editable_daily <- reactiveVal(NULL)
  
  # 2. Re-populate the 7-day baseline grid when the date picker changes
  observe({
    req(input$select_week)
    
    if (input$select_week == "Current Week") {

    # Pull base stats records for the selected week
    daily_base <- stats %>%
      filter(athlete_name %in% athletes_catapult$athlete_name & position_name != "Goal Keeper") %>%
      select(athlete_name | date | total_distance | high_speed_distance | sprint_distance | accel_decel_efforts) %>%
      filter(date >= floor_date(Sys.Date(), unit = "week", week_start = 1) & date <= Sys.Date())
    
    # Generate explicit filler slots for dates missing logs this week
    dates_seq <- seq(from = floor_date(Sys.Date(), unit = "week", week_start = 1), by = "day", length.out = 7)
    
    
    metrics_placeholder <- data.frame(
      athlete_name = athletes_catapult %>% filter(position_name != "Goal Keeper") %>% pull(athlete_name),
      total_distance = 0, high_speed_distance = 0, sprint_distance = 0, accel_decel_efforts = 0
    )
    
    dates_grid <- data.frame(athlete_name = athletes_catapult %>% filter(position_name != "Goal Keeper") %>% pull(athlete_name)) %>%
      group_by(athlete_name) %>%
      reframe(date = dates_seq) %>%
      ungroup %>% 
      left_join(metrics_placeholder, by = join_by(athlete_name)) %>%
      mutate(name_date = paste0(athlete_name, date)) %>%
      filter(!(name_date %in% paste0(daily_base$athlete_name, daily_base$date))) %>%
      select(!name_date)
    
    # Merge active logs with the empty tracking row matrix slots
    combined_daily <- daily_base %>%
      full_join(dates_grid, by = c("athlete_name", "date", "total_distance", "high_speed_distance", "sprint_distance", "accel_decel_efforts")) %>%
      arrange(athlete_name, date) 
    
    } else {
      
      # Generate explicit filler slots for dates missing logs this week
      dates_seq <- seq(from = ceiling_date(Sys.Date(), unit = "week", week_start = 1), by = "day", length.out = 7)
      
      metrics_placeholder <- data.frame(
        athlete_name = athletes_catapult %>% filter(position_name != "Goal Keeper") %>% pull(athlete_name),
        total_distance = 0, high_speed_distance = 0, sprint_distance = 0, accel_decel_efforts = 0
      )
      
      dates_grid <- data.frame(athlete_name = athletes_catapult %>% filter(position_name != "Goal Keeper") %>% pull(athlete_name)) %>%
        group_by(athlete_name) %>%
        reframe(date = dates_seq) %>%
        ungroup %>% 
        left_join(metrics_placeholder, by = join_by(athlete_name))
      
      combined_daily <- dates_grid %>% 
        arrange(athlete_name, date) 
      
    }
    
    team_avg <- combined_daily %>%
      group_by(date) %>% 
      summarise(across(where(is.numeric), ~mean(.x,na.rm=T))) %>% 
      mutate(athlete_name = "Team Average") %>% 
      relocate(athlete_name)
    
    # Append to the main data frame
    combined_daily <- bind_rows(combined_daily, team_avg)
    
    
    editable_daily(combined_daily)
  }) %>% bindEvent(input$select_week)
  
  # 3. MANUAL TRIGGER: Scrape cell changes and compute data changes ONLY when the button is clicked
  observe({
    df <- editable_daily()
    req(df, input$select_week)
    changed <- FALSE
    metrics_cols <- c("total_distance", "high_speed_distance", "sprint_distance", "accel_decel_efforts")
    
    for (i in 1:nrow(df)) {
      p_id <- stringr::str_replace_all(df$athlete_name[i], " ", "_")
      d_id <- df$date[i]
      
      for (col in metrics_cols) {
        input_id <- paste("inp", col, p_id, d_id, sep = "__")
        val <- input[[input_id]]
        
        # Scrape the user entries into our memory matrix
        if (!is.null(val) && !is.na(val) && val != df[i, col]) {
          df[i, col] <- val
          changed <- TRUE
        }
      }
    }
    
    if (changed) {
      editable_daily(df)
      
      if (input$select_week == "Current Week") {
        
      # Inline generation of updated plan data to avoid helper calls
      chronic <- stats %>%
        filter(athlete_name %in% athletes_catapult$athlete_name & position_name != "Goal Keeper") %>%
        select(athlete_name | date | total_distance | high_speed_distance | sprint_distance | accel_decel_efforts) %>%
        filter(date >= (floor_date(Sys.Date(), unit = "week", week_start = 1) - weeks(3)) & date < floor_date(Sys.Date(), unit = "week", week_start = 1)) %>%
        group_by(athlete_name) %>%
        summarize(across(where(is.numeric), ~sum(.x)/3))
      } else {
        
        # Inline generation of updated plan data to avoid helper calls
        chronic <- stats %>%
          filter(athlete_name %in% athletes_catapult$athlete_name & position_name != "Goal Keeper") %>%
          select(athlete_name | date | total_distance | high_speed_distance | sprint_distance | accel_decel_efforts) %>%
          filter(date >= (ceiling_date(Sys.Date(), unit = "week", week_start = 1) - weeks(3)) & date < ceiling_date(Sys.Date(), unit = "week", week_start = 1)) %>%
          group_by(athlete_name) %>%
          summarize(across(where(is.numeric), ~sum(.x)/3))
        
      }
      
      
      thresholds <- chronic %>%
        mutate(across(!athlete_name, ~ 0.7 * .x, .names = "{.col}_lower"),
               across(!athlete_name & !contains("_lower"), ~ 1.3 * .x, .names = "{.col}_upper")) %>% 
        rename_with(~paste0(.x,"_chronic"),.cols=where(is.numeric) & !contains("_lower") &!contains("_upper"))
      
      acute <- df %>%
        group_by(athlete_name) %>%
        summarize(across(where(is.numeric), sum)) %>% 
        filter(athlete_name != "Team Average")
      
      updated_plan <- acute %>%
        rename_with(~paste0(.x,"_acute"),.cols=where(is.numeric)) %>% 
        full_join(thresholds, by = join_by(athlete_name)) %>%
        mutate(total_distance_acwr = total_distance_acute/total_distance_chronic,
               high_speed_distance_acwr = high_speed_distance_acute/high_speed_distance_chronic,
               sprint_distance_acwr = sprint_distance_acute/sprint_distance_chronic,
               accel_decel_efforts_acwr = accel_decel_efforts_acute/accel_decel_efforts_chronic,
               total_distance_remaining = total_distance_upper - total_distance_acute,
               high_speed_distance_remaining = high_speed_distance_upper - high_speed_distance_acute,
               sprint_distance_remaining = sprint_distance_upper - sprint_distance_acute,
               accel_decel_efforts_remaining = accel_decel_efforts_upper - accel_decel_efforts_acute) %>%
        relocate(contains("total_distance"), contains("high_speed_distance"), contains("sprint_distance"), contains("accel_decel_efforts"), .after = athlete_name) %>%
        rename(Player = athlete_name) %>%
        left_join(athletes_catapult %>% select(athlete_name, position_name), by=join_by(Player==athlete_name)) %>% 
        arrange(position_name, Player) %>% 
        select(!position_name)
      
      team_avg_load <- updated_plan %>%
        summarise(across(where(is.numeric), ~mean(.x,na.rm=T))) %>% 
        mutate(Player = "Team Average") %>% 
        relocate(Player)
      
      # Append to the main data frame
      updated_plan_table <- bind_rows(updated_plan, team_avg_load)
      
      
      # Push data changes cleanly without resetting row visibility
      updateReactable("AcuteChronicTable", data = updated_plan_table)
    }
  }) %>% bindEvent(input$update_table_btn) 
  
  
  observe({
    req(input$select_week)
    
    if (input$select_week == "Current Week") {
      
    daily_base <- stats %>%
      filter(athlete_name %in% athletes_catapult$athlete_name & position_name != "Goal Keeper") %>%
      select(athlete_name | date | total_distance | high_speed_distance | sprint_distance | accel_decel_efforts) %>%
      filter(date >= floor_date(Sys.Date(), unit = "week", week_start = 1) & date <= Sys.Date())
    
    dates_seq <- seq(from = floor_date(Sys.Date(), unit = "week", week_start = 1), by = "day", length.out = 7)
    
    metrics_placeholder <- data.frame(
      athlete_name = athletes_catapult %>% filter(position_name != "Goal Keeper") %>% pull(athlete_name),
      total_distance = 0, high_speed_distance = 0, sprint_distance = 0, accel_decel_efforts = 0
    )
    
    dates_grid <- data.frame(athlete_name = athletes_catapult %>% filter(position_name != "Goal Keeper") %>% pull(athlete_name)) %>%
      group_by(athlete_name) %>%
      reframe(date = dates_seq) %>%
      ungroup %>% 
      left_join(metrics_placeholder, by = join_by(athlete_name)) %>%
      mutate(name_date = paste0(athlete_name, date)) %>%
      filter(!(name_date %in% paste0(daily_base$athlete_name, daily_base$date))) %>%
      select(!name_date)
    
    combined_daily <- daily_base %>%
      full_join(dates_grid, by = c("athlete_name", "date", "total_distance", "high_speed_distance", "sprint_distance", "accel_decel_efforts")) %>%
      arrange(athlete_name, date)
    
    } else {
      
      # Generate explicit filler slots for dates missing logs this week
      dates_seq <- seq(from = ceiling_date(Sys.Date(), unit = "week", week_start = 1), by = "day", length.out = 7)
      
      metrics_placeholder <- data.frame(
        athlete_name = athletes_catapult %>% filter(position_name != "Goal Keeper") %>% pull(athlete_name),
        total_distance = 0, high_speed_distance = 0, sprint_distance = 0, accel_decel_efforts = 0
      )
      
      dates_grid <- data.frame(athlete_name = athletes_catapult %>% filter(position_name != "Goal Keeper") %>% pull(athlete_name)) %>%
        group_by(athlete_name) %>%
        reframe(date = dates_seq) %>%
        ungroup %>% 
        left_join(metrics_placeholder, by = join_by(athlete_name))
      
      combined_daily <- dates_grid %>% 
        arrange(athlete_name, date) 
      
    }
    
    team_avg <- combined_daily %>%
      group_by(date) %>% 
      summarise(across(where(is.numeric), ~mean(.x,na.rm=T))) %>% 
      mutate(athlete_name = "Team Average") %>% 
      relocate(athlete_name)
    
    # Append to the main data frame
    combined_daily <- bind_rows(combined_daily, team_avg)
    
    # Reset the reactive memory to original values
    editable_daily(combined_daily)
    
    # Crucial: Since sub-table input text fields hold onto browser states, 
    # we must trigger a structural UI reload by calling shinyjs::refresh() or updating the output slot
    shinyjs::runjs("Shiny.setInputValue('acute_chronic_table_state', Math.random());") 
    
  }) %>% bindEvent(input$clear_table_btn)
  
  # 4. Table Structure Definition Output (Matches your layout preferences)
  output$AcuteChronicTable <- renderReactable({
    daily_current <- editable_daily()
    req(daily_current, input$select_week)
    
    # Listens to our random trigger above to force sub-table input redraws when cleared
    input$acute_chronic_table_state 
    
    if (input$select_week == "Current Week") {
      
    # Inline generation of layout datasets to avoid helper calls
    chronic <- stats %>%
      filter(athlete_name %in% athletes_catapult$athlete_name & position_name != "Goal Keeper") %>%
      select(athlete_name | date | total_distance | high_speed_distance | sprint_distance | accel_decel_efforts) %>%
      filter(date >= (floor_date(Sys.Date(), unit = "week", week_start = 1) - weeks(3)) & date < floor_date(Sys.Date(), unit = "week", week_start = 1)) %>%
      group_by(athlete_name) %>%
      summarize(across(where(is.numeric), ~sum(.x)/3))
    
    } else {
      
      # Inline generation of updated plan data to avoid helper calls
      chronic <- stats %>%
        filter(athlete_name %in% athletes_catapult$athlete_name & position_name != "Goal Keeper") %>%
        select(athlete_name | date | total_distance | high_speed_distance | sprint_distance | accel_decel_efforts) %>%
        filter(date >= (ceiling_date(Sys.Date(), unit = "week", week_start = 1) - weeks(3)) & date < ceiling_date(Sys.Date(), unit = "week", week_start = 1)) %>%
        group_by(athlete_name) %>%
        summarize(across(where(is.numeric), ~sum(.x)/3))
      
    }
    thresholds <- chronic %>%
      mutate(across(!athlete_name, ~ 0.7 * .x, .names = "{.col}_lower"),
             across(!athlete_name & !contains("_lower"), ~ 1.3 * .x, .names = "{.col}_upper")) %>% 
      rename_with(~paste0(.x,"_chronic"),.cols=where(is.numeric) & !contains("_lower") &!contains("_upper"))
    
    acute <- daily_current %>%
      group_by(athlete_name) %>%
      summarize(across(where(is.numeric), sum)) %>% 
      filter(athlete_name != "Team Average")
      
    
    load_plan <- acute %>%
      rename_with(~paste0(.x,"_acute"),.cols=where(is.numeric)) %>% 
      full_join(thresholds, by = join_by(athlete_name)) %>%
      mutate(total_distance_acwr = total_distance_acute/total_distance_chronic,
             high_speed_distance_acwr = high_speed_distance_acute/high_speed_distance_chronic,
             sprint_distance_acwr = sprint_distance_acute/sprint_distance_chronic,
             accel_decel_efforts_acwr = accel_decel_efforts_acute/accel_decel_efforts_chronic,
             total_distance_remaining = total_distance_upper - total_distance_acute,
             high_speed_distance_remaining = high_speed_distance_upper - high_speed_distance_acute,
             sprint_distance_remaining = sprint_distance_upper - sprint_distance_acute,
             accel_decel_efforts_remaining = accel_decel_efforts_upper - accel_decel_efforts_acute) %>%
      relocate(contains("total_distance"), contains("high_speed_distance"), contains("sprint_distance"), contains("accel_decel_efforts"), .after = athlete_name) %>%
      rename(Player = athlete_name)  %>% 
      left_join(athletes_catapult %>% select(athlete_name, position_name), by=join_by(Player==athlete_name)) %>% 
      arrange(position_name, Player) %>% 
      select(!position_name)
    
    
    team_avg_load <- load_plan %>%
      summarise(across(where(is.numeric), ~mean(.x,na.rm=T))) %>% 
      mutate(Player = "Team Average") %>% 
      relocate(Player)
    
    # Append to the main data frame
    load_plan_table <- bind_rows(load_plan, team_avg_load)
    
    weekly_details <- daily_current %>%
      arrange(athlete_name, date) %>%
      mutate(formatted_date = format(date, format = "%a, %b %d")) %>%
      rename(Player = athlete_name) %>% 
      relocate(formatted_date,.after=Player)
    
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
      load_plan_table,
      striped = F, outline = F, bordered = T, compact = T, highlight = F,
      defaultPageSize = nrow(load_plan_table),
      onClick = "expand",  
      style = list(overflowX = "auto", display = "block"),
      rowStyle = function(index) {
        if (load_plan_table[index, "Player"] == "Team Average") {
          list(fontWeight = "bold")
        }
      },
      defaultColDef = colDef(align = "center",format = colFormat(digits = 0)), 
      columns = list(
        Player = colDef(minWidth = 150,
                        align = "left",
                        sticky = "left", # Locks column during horizontal scrolling
                        style = list(fontWeight = 600, backgroundColor = "#fff", zIndex = 1)),
        total_distance_acute = colDef(name = "Acute"),
        total_distance_chronic = colDef(name = "Chronic"),
        total_distance_acwr = colDef(name = "ACWR", style = function(value) {
          if (!is.na(value) && (value > 1.3 || value < 0.7)) {
            list(background = rgb(255, 0, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
          } else if (!is.na(value) && (value > 1.2 || value < 0.8)) {
            list(background = rgb(255, 165, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
          } else if (!is.na(value) && value >= 0.8 && value <= 1.2) {
            list(background = rgb(0, 255, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
          } else {
            list(background = NULL) # Default style for any missing cases
          }
        }, format = colFormat(digits = 2)),
        total_distance_lower = colDef(name = "Lower", style = function(value, index) {
          acute <- load_plan_table$total_distance_acute[index]
          # Calculate relative width (ratio between 0 and 1)
          # min/max constraints ensure the bar fills up nicely even if acute > value
          percentage_width <- min(max(acute / value, 0), 1) 
          bar_style(width = percentage_width, fill = rgb(0, 176, 185,alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
        }
        ),
        total_distance_upper = colDef(name = "Upper", style = function(value, index) {
          acute <- load_plan_table$total_distance_acute[index]
          # Calculate relative width (ratio between 0 and 1)
          # min/max constraints ensure the bar fills up nicely even if acute > value
          percentage_width <- min(max(acute / value, 0), 1) 
          bar_style(width = percentage_width, fill = rgb(87, 44, 95,alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
        }
        ),
        total_distance_remaining = colDef(name = "Remaining"),
        high_speed_distance_acute = colDef(name = "Acute"),
        high_speed_distance_chronic = colDef(name = "Chronic"),
        high_speed_distance_acwr = colDef(name = "ACWR", style = function(value) {
          if (!is.na(value) && (value > 1.3 || value < 0.7)) {
            list(background = rgb(255, 0, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
          } else if (!is.na(value) && (value > 1.2 || value < 0.8)) {
            list(background = rgb(255, 165, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
          } else if (!is.na(value) && value >= 0.8 && value <= 1.2) {
            list(background = rgb(0, 255, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
          } else {
            list(background = NULL) # Default style for any missing cases
          }
        }, format = colFormat(digits = 2)),
        high_speed_distance_lower = colDef(name = "Lower", style = function(value, index) {
          acute <- load_plan_table$high_speed_distance_acute[index]
          # Calculate relative width (ratio between 0 and 1)
          # min/max constraints ensure the bar fills up nicely even if acute > value
          percentage_width <- min(max(acute / value, 0), 1) 
          bar_style(width = percentage_width, fill = rgb(0, 176, 185,alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
        }
        ),
        high_speed_distance_upper = colDef(name = "Upper", style = function(value, index) {
          acute <- load_plan_table$high_speed_distance_acute[index]
          # Calculate relative width (ratio between 0 and 1)
          # min/max constraints ensure the bar fills up nicely even if acute > value
          percentage_width <- min(max(acute / value, 0), 1) 
          bar_style(width = percentage_width, fill = rgb(87, 44, 95,alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
        }
        ),
        high_speed_distance_remaining = colDef(name = "Remaining"),
        sprint_distance_acute = colDef(name = "Acute"),
        sprint_distance_chronic = colDef(name = "Chronic"),
        sprint_distance_acwr = colDef(name = "ACWR", style = function(value) {
          if (!is.na(value) && (value > 1.3 || value < 0.7)) {
            list(background = rgb(255, 0, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
          } else if (!is.na(value) && (value > 1.2 || value < 0.8)) {
            list(background = rgb(255, 165, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
          } else if (!is.na(value) && value >= 0.8 && value <= 1.2) {
            list(background = rgb(0, 255, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
          } else {
            list(background = NULL) # Default style for any missing cases
          }
        }, format = colFormat(digits = 2)),
        sprint_distance_lower = colDef(name = "Lower", style = function(value, index) {
          acute <- load_plan_table$sprint_distance_acute[index]
          # Calculate relative width (ratio between 0 and 1)
          # min/max constraints ensure the bar fills up nicely even if acute > value
          percentage_width <- min(max(acute / value, 0), 1) 
          bar_style(width = percentage_width, fill = rgb(0, 176, 185,alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
        }
        ),
        sprint_distance_upper = colDef(name = "Upper", style = function(value, index) {
          acute <- load_plan_table$sprint_distance_acute[index]
          # Calculate relative width (ratio between 0 and 1)
          # min/max constraints ensure the bar fills up nicely even if acute > value
          percentage_width <- min(max(acute / value, 0), 1) 
          bar_style(width = percentage_width, fill = rgb(87, 44, 95,alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
        }
        ),
        sprint_distance_remaining = colDef(name = "Remaining"),
        accel_decel_efforts_acute = colDef(name = "Acute"),
        accel_decel_efforts_chronic = colDef(name = "Chronic"),
        accel_decel_efforts_acwr = colDef(name = "ACWR", style = function(value) {
          if (!is.na(value) && (value > 1.3 || value < 0.7)) {
            list(background = rgb(255, 0, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
          } else if (!is.na(value) && (value > 1.2 || value < 0.8)) {
            list(background = rgb(255, 165, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
          } else if (!is.na(value) && value >= 0.8 && value <= 1.2) {
            list(background = rgb(0, 255, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
          } else {
            list(background = NULL) # Default style for any missing cases
          }
        }, format = colFormat(digits = 2)),
        accel_decel_efforts_lower = colDef(name = "Lower", style = function(value, index) {
          acute <- load_plan_table$accel_decel_efforts_acute[index]
          # Calculate relative width (ratio between 0 and 1)
          # min/max constraints ensure the bar fills up nicely even if acute > value
          percentage_width <- min(max(acute / value, 0), 1) 
          bar_style(width = percentage_width, fill = rgb(0, 176, 185,alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
        }
        ),
        accel_decel_efforts_upper = colDef(name = "Upper", style = function(value, index) {
          acute <- load_plan_table$accel_decel_efforts_acute[index]
          # Calculate relative width (ratio between 0 and 1)
          # min/max constraints ensure the bar fills up nicely even if acute > value
          percentage_width <- min(max(acute / value, 0), 1) 
          bar_style(width = percentage_width, fill = rgb(87, 44, 95,alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
        }
        ),
        accel_decel_efforts_remaining = colDef(name = "Remaining")
      ),
      columnGroups = list(
        colGroup(name = "Total Distance (m)", columns = c("total_distance_acute", "total_distance_chronic","total_distance_acwr", "total_distance_lower",  "total_distance_upper","total_distance_remaining")),
        colGroup(name = "HSR Distance (m)", columns = c("high_speed_distance_acute","high_speed_distance_chronic","high_speed_distance_acwr", "high_speed_distance_lower",  "high_speed_distance_upper","high_speed_distance_remaining")),
        colGroup(name = "Sprint Distance (m)", columns =c("sprint_distance_acute", "sprint_distance_chronic","sprint_distance_acwr", "sprint_distance_lower",  "sprint_distance_upper","sprint_distance_remaining")),
        colGroup(name = "Accel + Decel Efforts", columns = c("accel_decel_efforts_acute","accel_decel_efforts_chronic","accel_decel_efforts_acwr", "accel_decel_efforts_lower",  "accel_decel_efforts_upper","accel_decel_efforts_remaining"))
      ),
      details = function(index) {
        player_name <- load_plan_table$Player[index]
        weekly_info <- weekly_details %>% filter(Player == player_name)
        
        htmltools::div(
          style = "padding: 1rem; background-color: #fcfcfc;",
          reactable(
            weekly_info %>% select(!c(Player, date)),
            striped = F, outline = F, bordered = T, compact = T, highlight = F, fullWidth = F,
            defaultColDef = colDef(align = "center"), 
            columns = list(
              formatted_date = colDef(name = "Date", align="left",sortable = FALSE),
              total_distance = colDef(name = "Total Distance (m)", cell = function(val, r_idx) {
                p_clean <- stringr::str_replace_all(player_name, " ", "_")
                as.character(numericInput(paste("inp", "total_distance", p_clean, weekly_info$date[r_idx], sep = "__"), NULL, value = round(val),min = 0,  width = "100px"))
              }, html = TRUE),
              high_speed_distance = colDef(name = "HSR Distance (m)", cell = function(val, r_idx) {
                p_clean <- stringr::str_replace_all(player_name, " ", "_")
                as.character(numericInput(paste("inp", "high_speed_distance", p_clean, weekly_info$date[r_idx], sep = "__"), NULL, value = round(val), min = 0, width = "100px"))
              }, html = TRUE),
              sprint_distance = colDef(name = "Sprint Distance (m)", cell = function(val, r_idx) {
                p_clean <- stringr::str_replace_all(player_name, " ", "_")
                as.character(numericInput(paste("inp", "sprint_distance", p_clean, weekly_info$date[r_idx], sep = "__"), NULL, value = round(val), min = 0, width = "100px"))
              }, html = TRUE),
              accel_decel_efforts = colDef(name = "Accel + Decel Efforts", cell = function(val, r_idx) {
                p_clean <- stringr::str_replace_all(player_name, " ", "_")
                as.character(numericInput(paste("inp", "accel_decel_efforts", p_clean, weekly_info$date[r_idx], sep = "__"), NULL, value = round(val), min = 0, width = "100px"))
              }, html = TRUE)
            )
          )
        )
      }
    )
  })
  
  
  
  
  # 1. Initialize a reactive tracking value for user modifications
  editable_daily_gk <- reactiveVal(NULL)
  
  # 2. Re-populate the 7-day baseline grid when the date picker changes
  observe({
    req(input$select_week_gk)
    
    if (input$select_week_gk == "Current Week") {
      
    # Pull base stats records for the selected week
    daily_base <- stats %>%
      filter(athlete_name %in% athletes_catapult$athlete_name & position_name == "Goal Keeper") %>%
      select(athlete_name | date | total_distance | dive_count | total_dive_load | explosive_efforts) %>%
      filter(date >= floor_date(Sys.Date(), unit = "week", week_start = 1) & date <= Sys.Date())
    
    # Generate explicit filler slots for dates missing logs this week
    dates_seq <- seq(from = floor_date(Sys.Date(), unit = "week", week_start = 1), by = "day", length.out = 7)
    
    metrics_placeholder <- data.frame(
      athlete_name = athletes_catapult %>% filter(position_name == "Goal Keeper") %>% pull(athlete_name),
      total_distance = 0, dive_count = 0, total_dive_load = 0, explosive_efforts = 0
    )
    
    dates_grid <- data.frame(athlete_name = athletes_catapult %>% filter(position_name == "Goal Keeper") %>% pull(athlete_name)) %>%
      group_by(athlete_name) %>%
      reframe(date = dates_seq) %>%
      ungroup %>% 
      left_join(metrics_placeholder, by = join_by(athlete_name)) %>%
      mutate(name_date = paste0(athlete_name, date)) %>%
      filter(!(name_date %in% paste0(daily_base$athlete_name, daily_base$date))) %>%
      select(!name_date)
    
    # Merge active logs with the empty tracking row matrix slots
    combined_daily <- daily_base %>%
      full_join(dates_grid, by = c("athlete_name", "date", "total_distance", "dive_count", "total_dive_load", "explosive_efforts")) %>%
      arrange(athlete_name, date) 
    
    } else {
      
      # Generate explicit filler slots for dates missing logs this week
      dates_seq <- seq(from = ceiling_date(Sys.Date(), unit = "week", week_start = 1), by = "day", length.out = 7)
      
      metrics_placeholder <- data.frame(
        athlete_name = athletes_catapult %>% filter(position_name == "Goal Keeper") %>% pull(athlete_name),
        total_distance = 0, dive_count = 0, total_dive_load = 0, explosive_efforts = 0
      )
      
      dates_grid <- data.frame(athlete_name = athletes_catapult %>% filter(position_name == "Goal Keeper") %>% pull(athlete_name)) %>%
        group_by(athlete_name) %>%
        reframe(date = dates_seq) %>%
        ungroup %>% 
        left_join(metrics_placeholder, by = join_by(athlete_name))
      
      combined_daily <- dates_grid %>% 
        arrange(athlete_name, date) 
      
    }
    
    gk_avg <- combined_daily %>%
      group_by(date) %>% 
      summarise(across(where(is.numeric), ~mean(.x,na.rm=T))) %>% 
      mutate(athlete_name = "GK Average") %>% 
      relocate(athlete_name)
    
    # Append to the main data frame
    combined_daily <- bind_rows(combined_daily, gk_avg)
    
    editable_daily_gk(combined_daily)
  }) %>% bindEvent(input$select_week_gk)
  
  # 3. MANUAL TRIGGER: Scrape cell changes and compute data changes ONLY when the button is clicked
  observe({
    df <- editable_daily_gk()
    req(df, input$select_week_gk)
    changed <- FALSE
    metrics_cols <- c("total_distance", "dive_count", "total_dive_load", "explosive_efforts")
    
    for (i in 1:nrow(df)) {
      p_id <- stringr::str_replace_all(df$athlete_name[i], " ", "_")
      d_id <- df$date[i]
      
      for (col in metrics_cols) {
        input_id <- paste("inp", col, p_id, d_id, sep = "__")
        val <- input[[input_id]]
        
        # Scrape the user entries into our memory matrix
        if (!is.null(val) && !is.na(val) && val != df[i, col]) {
          df[i, col] <- val
          changed <- TRUE
        }
      }
    }
    
    if (changed) {
      editable_daily_gk(df)
      
      if (input$select_week_gk == "Current Week") {
        
      # Inline generation of updated plan data to avoid helper calls
      chronic <- stats %>%
        filter(athlete_name %in% athletes_catapult$athlete_name & position_name == "Goal Keeper") %>%
        select(athlete_name | date | total_distance | dive_count | total_dive_load | explosive_efforts) %>%
        filter(date >= (floor_date(Sys.Date(), unit = "week", week_start = 1) - weeks(3)) & date < floor_date(Sys.Date(), unit = "week", week_start = 1)) %>%
        group_by(athlete_name) %>%
        summarize(across(where(is.numeric), ~sum(.x)/3))
      
      } else {
        # Inline generation of updated plan data to avoid helper calls
        chronic <- stats %>%
          filter(athlete_name %in% athletes_catapult$athlete_name & position_name == "Goal Keeper") %>%
          select(athlete_name | date | total_distance | dive_count | total_dive_load | explosive_efforts) %>%
          filter(date >= (ceiling_date(Sys.Date(), unit = "week", week_start = 1) - weeks(3)) & date < ceiling_date(Sys.Date(), unit = "week", week_start = 1)) %>%
          group_by(athlete_name) %>%
          summarize(across(where(is.numeric), ~sum(.x)/3))
        
      }
      
      thresholds <- chronic %>%
        mutate(across(!athlete_name, ~ 0.7 * .x, .names = "{.col}_lower"),
               across(!athlete_name & !contains("_lower"), ~ 1.3 * .x, .names = "{.col}_upper")) %>% 
        rename_with(~paste0(.x,"_chronic"),.cols=where(is.numeric) & !contains("_lower") &!contains("_upper"))
      
      acute <- df %>%
        group_by(athlete_name) %>%
        summarize(across(where(is.numeric), sum)) %>% 
        filter(athlete_name != "GK Average")
      
      updated_plan <- acute %>%
        rename_with(~paste0(.x,"_acute"),.cols=where(is.numeric)) %>% 
        full_join(thresholds, by = join_by(athlete_name)) %>%
        mutate(total_distance_acwr = total_distance_acute/total_distance_chronic,
               dive_count_acwr = dive_count_acute/dive_count_chronic,
               total_dive_load_acwr = total_dive_load_acute/total_dive_load_chronic,
               explosive_efforts_acwr = explosive_efforts_acute/explosive_efforts_chronic,
               total_distance_remaining = total_distance_upper - total_distance_acute,
               dive_count_remaining = dive_count_upper - dive_count_acute,
               total_dive_load_remaining = total_dive_load_upper - total_dive_load_acute,
               explosive_efforts_remaining = explosive_efforts_upper - explosive_efforts_acute) %>%
        relocate(contains("total_distance"), contains("dive_count"), contains("total_dive_load"), contains("explosive_efforts"), .after = athlete_name) %>%
        rename(Player = athlete_name) %>%
        arrange(Player) 
      
      gk_avg_load <- updated_plan %>%
        summarise(across(where(is.numeric), ~mean(.x,na.rm=T))) %>% 
        mutate(Player = "GK Average") %>% 
        relocate(Player)
      
      # Append to the main data frame
      updated_plan_table <- bind_rows(updated_plan, gk_avg_load)
      
      
      # Push data changes cleanly without resetting row visibility
      updateReactable("AcuteChronicGKTable", data = updated_plan_table)
    }
  }) %>% bindEvent(input$update_gk_table_btn) 
  
  
  observe({
    req(input$select_week_gk)
    
    
    if (input$select_week_gk == "Current Week") {
      
      
    daily_base <- stats %>%
      filter(athlete_name %in% athletes_catapult$athlete_name & position_name == "Goal Keeper") %>%
      select(athlete_name | date | total_distance | dive_count | total_dive_load | explosive_efforts) %>%
      filter(date >= floor_date(Sys.Date(), unit = "week", week_start = 1) & date <= Sys.Date())
    
    dates_seq <- seq(from = floor_date(Sys.Date(), unit = "week", week_start = 1), by = "day", length.out = 7)
    
    metrics_placeholder <- data.frame(
      athlete_name = athletes_catapult %>% filter(position_name == "Goal Keeper") %>% pull(athlete_name),
      total_distance = 0, dive_count = 0, total_dive_load = 0, explosive_efforts = 0
    )
    
    dates_grid <- data.frame(athlete_name = athletes_catapult %>% filter(position_name == "Goal Keeper") %>% pull(athlete_name)) %>%
      group_by(athlete_name) %>%
      reframe(date = dates_seq) %>%
      ungroup %>% 
      left_join(metrics_placeholder, by = join_by(athlete_name)) %>%
      mutate(name_date = paste0(athlete_name, date)) %>%
      filter(!(name_date %in% paste0(daily_base$athlete_name, daily_base$date))) %>%
      select(!name_date)
    
    combined_daily <- daily_base %>%
      full_join(dates_grid, by = c("athlete_name", "date", "total_distance", "dive_count", "total_dive_load", "explosive_efforts")) %>%
      arrange(athlete_name, date)
    
    } else {
      
      # Generate explicit filler slots for dates missing logs this week
      dates_seq <- seq(from = ceiling_date(Sys.Date(), unit = "week", week_start = 1), by = "day", length.out = 7)
      
      metrics_placeholder <- data.frame(
        athlete_name = athletes_catapult %>% filter(position_name == "Goal Keeper") %>% pull(athlete_name),
        total_distance = 0, dive_count = 0, total_dive_load = 0, explosive_efforts = 0
      )
      
      dates_grid <- data.frame(athlete_name = athletes_catapult %>% filter(position_name == "Goal Keeper") %>% pull(athlete_name)) %>%
        group_by(athlete_name) %>%
        reframe(date = dates_seq) %>%
        ungroup %>% 
        left_join(metrics_placeholder, by = join_by(athlete_name))
      
      combined_daily <- dates_grid %>% 
        arrange(athlete_name, date) 
      
    }
    
  
    gk_avg <- combined_daily %>%
      group_by(date) %>% 
      summarise(across(where(is.numeric), ~mean(.x,na.rm=T))) %>% 
      mutate(athlete_name = "GK Average") %>% 
      relocate(athlete_name)
    
    # Append to the main data frame
    combined_daily <- bind_rows(combined_daily, gk_avg)
    
    # Reset the reactive memory to original values
    editable_daily_gk(combined_daily)
    
    # Crucial: Since sub-table input text fields hold onto browser states, 
    # we must trigger a structural UI reload by calling shinyjs::refresh() or updating the output slot
    shinyjs::runjs("Shiny.setInputValue('acute_chronic_gk_table_state', Math.random());") 
    
  }) %>% bindEvent(input$clear_gk_table_btn)
  
  # 4. Table Structure Definition Output (Matches your layout preferences)
  output$AcuteChronicGKTable <- renderReactable({
    daily_current <- editable_daily_gk()
    req(daily_current, input$select_week_gk)
    
    # Listens to our random trigger above to force sub-table input redraws when cleared
    input$acute_chronic_gk_table_state 
    
    
    if (input$select_week_gk == "Current Week") {
      
    # Inline generation of layout datasets to avoid helper calls
    chronic <- stats %>%
      filter(athlete_name %in% athletes_catapult$athlete_name & position_name == "Goal Keeper") %>%
      select(athlete_name | date | total_distance | dive_count | total_dive_load | explosive_efforts) %>%
      filter(date >= (floor_date(Sys.Date(), unit = "week", week_start = 1) - weeks(3)) & date < floor_date(Sys.Date(), unit = "week", week_start = 1)) %>%
      group_by(athlete_name) %>%
      summarize(across(where(is.numeric), ~sum(.x)/3))
    
    } else {
      
      # Inline generation of layout datasets to avoid helper calls
      chronic <- stats %>%
        filter(athlete_name %in% athletes_catapult$athlete_name & position_name == "Goal Keeper") %>%
        select(athlete_name | date | total_distance | dive_count | total_dive_load | explosive_efforts) %>%
        filter(date >= (ceiling_date(Sys.Date(), unit = "week", week_start = 1) - weeks(3)) & date < ceiling_date(Sys.Date(), unit = "week", week_start = 1)) %>%
        group_by(athlete_name) %>%
        summarize(across(where(is.numeric), ~sum(.x)/3))
    }
    
    thresholds <- chronic %>%
      mutate(across(!athlete_name, ~ 0.7 * .x, .names = "{.col}_lower"),
             across(!athlete_name & !contains("_lower"), ~ 1.3 * .x, .names = "{.col}_upper")) %>% 
      rename_with(~paste0(.x,"_chronic"),.cols=where(is.numeric) & !contains("_lower") &!contains("_upper"))
    
    acute <- daily_current %>%
      group_by(athlete_name) %>%
      summarize(across(where(is.numeric), sum)) %>% 
      filter(athlete_name != "GK Average")
    
    
    load_plan <- acute %>%
      rename_with(~paste0(.x,"_acute"),.cols=where(is.numeric)) %>% 
      full_join(thresholds, by = join_by(athlete_name)) %>%
      mutate(total_distance_acwr = total_distance_acute/total_distance_chronic,
             dive_count_acwr = dive_count_acute/dive_count_chronic,
             total_dive_load_acwr = total_dive_load_acute/total_dive_load_chronic,
             explosive_efforts_acwr = explosive_efforts_acute/explosive_efforts_chronic,
             total_distance_remaining = total_distance_upper - total_distance_acute,
             dive_count_remaining = dive_count_upper - dive_count_acute,
             total_dive_load_remaining = total_dive_load_upper - total_dive_load_acute,
             explosive_efforts_remaining = explosive_efforts_upper - explosive_efforts_acute) %>%
      relocate(contains("total_distance"), contains("dive_count"), contains("total_dive_load"), contains("explosive_efforts"), .after = athlete_name) %>%
      rename(Player = athlete_name) %>%
      arrange(Player) 
    
    
    gk_avg_load <- load_plan %>%
      summarise(across(where(is.numeric), ~mean(.x,na.rm=T))) %>% 
      mutate(Player = "GK Average") %>% 
      relocate(Player)
    
    # Append to the main data frame
    load_plan_table <- bind_rows(load_plan, gk_avg_load)
    
    weekly_details <- daily_current %>%
      arrange(athlete_name, date) %>%
      mutate(formatted_date = format(date, format = "%a, %b %d")) %>%
      rename(Player = athlete_name) %>% 
      relocate(formatted_date,.after=Player)
    
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
      load_plan_table,
      striped = F, outline = F, bordered = T, compact = T, highlight = F,
      defaultPageSize = nrow(load_plan_table),
      onClick = "expand",  
      style = list(overflowX = "auto", display = "block"),
      rowStyle = function(index) {
        if (load_plan_table[index, "Player"] == "GK Average") {
          list(fontWeight = "bold")
        }
      },
      defaultColDef = colDef(align = "center",format = colFormat(digits = 0)), 
      columns = list(
        Player = colDef(minWidth = 150,
                        align = "left",
                        sticky = "left", # Locks column during horizontal scrolling
                        style = list(fontWeight = 600, backgroundColor = "#fff", zIndex = 1)),
        total_distance_acute = colDef(name = "Acute"),
        total_distance_chronic = colDef(name = "Chronic"),
        total_distance_acwr = colDef(name = "ACWR", style = function(value) {
          if (!is.na(value) && (value > 1.3 || value < 0.7)) {
            list(background = rgb(255, 0, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
          } else if (!is.na(value) && (value > 1.2 || value < 0.8)) {
            list(background = rgb(255, 165, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
          } else if (!is.na(value) && value >= 0.8 && value <= 1.2) {
            list(background = rgb(0, 255, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
          } else {
            list(background = NULL) # Default style for any missing cases
          }
        }, format = colFormat(digits = 2)),
        total_distance_lower = colDef(name = "Lower", style = function(value, index) {
          acute <- load_plan_table$total_distance_acute[index]
          # Calculate relative width (ratio between 0 and 1)
          # min/max constraints ensure the bar fills up nicely even if acute > value
          percentage_width <- min(max(acute / value, 0), 1) 
          bar_style(width = percentage_width, fill = rgb(0, 176, 185,alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
        }
        ),
        total_distance_upper = colDef(name = "Upper", style = function(value, index) {
          acute <- load_plan_table$total_distance_acute[index]
          # Calculate relative width (ratio between 0 and 1)
          # min/max constraints ensure the bar fills up nicely even if acute > value
          percentage_width <- min(max(acute / value, 0), 1) 
          bar_style(width = percentage_width, fill = rgb(87, 44, 95,alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
        }
        ),
        total_distance_remaining = colDef(name = "Remaining"),
        dive_count_acute = colDef(name = "Acute"),
        dive_count_chronic = colDef(name = "Chronic"),
        dive_count_acwr = colDef(name = "ACWR", style = function(value) {
          if (!is.na(value) && (value > 1.3 || value < 0.7)) {
            list(background = rgb(255, 0, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
          } else if (!is.na(value) && (value > 1.2 || value < 0.8)) {
            list(background = rgb(255, 165, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
          } else if (!is.na(value) && value >= 0.8 && value <= 1.2) {
            list(background = rgb(0, 255, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
          } else {
            list(background = NULL) # Default style for any missing cases
          }
        }, format = colFormat(digits = 2)),
        dive_count_lower = colDef(name = "Lower", style = function(value, index) {
          acute <- load_plan_table$dive_count_acute[index]
          # Calculate relative width (ratio between 0 and 1)
          # min/max constraints ensure the bar fills up nicely even if acute > value
          percentage_width <- min(max(acute / value, 0), 1) 
          bar_style(width = percentage_width, fill = rgb(0, 176, 185,alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
        }
        ),
        dive_count_upper = colDef(name = "Upper", style = function(value, index) {
          acute <- load_plan_table$dive_count_acute[index]
          # Calculate relative width (ratio between 0 and 1)
          # min/max constraints ensure the bar fills up nicely even if acute > value
          percentage_width <- min(max(acute / value, 0), 1) 
          bar_style(width = percentage_width, fill = rgb(87, 44, 95,alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
        }
        ),
        dive_count_remaining = colDef(name = "Remaining"),
        total_dive_load_acute = colDef(name = "Acute"),
        total_dive_load_chronic = colDef(name = "Chronic"),
        total_dive_load_acwr = colDef(name = "ACWR", style = function(value) {
          if (!is.na(value) && (value > 1.3 || value < 0.7)) {
            list(background = rgb(255, 0, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
          } else if (!is.na(value) && (value > 1.2 || value < 0.8)) {
            list(background = rgb(255, 165, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
          } else if (!is.na(value) && value >= 0.8 && value <= 1.2) {
            list(background = rgb(0, 255, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
          } else {
            list(background = NULL) # Default style for any missing cases
          }
        }, format = colFormat(digits = 2)),
        total_dive_load_lower = colDef(name = "Lower", style = function(value, index) {
          acute <- load_plan_table$total_dive_load_acute[index]
          # Calculate relative width (ratio between 0 and 1)
          # min/max constraints ensure the bar fills up nicely even if acute > value
          percentage_width <- min(max(acute / value, 0), 1) 
          bar_style(width = percentage_width, fill = rgb(0, 176, 185,alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
        }
        ),
        total_dive_load_upper = colDef(name = "Upper", style = function(value, index) {
          acute <- load_plan_table$total_dive_load_acute[index]
          # Calculate relative width (ratio between 0 and 1)
          # min/max constraints ensure the bar fills up nicely even if acute > value
          percentage_width <- min(max(acute / value, 0), 1) 
          bar_style(width = percentage_width, fill = rgb(87, 44, 95,alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
        }
        ),
        total_dive_load_remaining = colDef(name = "Remaining"),
        explosive_efforts_acute = colDef(name = "Acute"),
        explosive_efforts_chronic = colDef(name = "Chronic"),
        explosive_efforts_acwr = colDef(name = "ACWR", style = function(value) {
          if (!is.na(value) && (value > 1.3 || value < 0.7)) {
            list(background = rgb(255, 0, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
          } else if (!is.na(value) && (value > 1.2 || value < 0.8)) {
            list(background = rgb(255, 165, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
          } else if (!is.na(value) && value >= 0.8 && value <= 1.2) {
            list(background = rgb(0, 255, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
          } else {
            list(background = NULL) # Default style for any missing cases
          }
        }, format = colFormat(digits = 2)),
        explosive_efforts_lower = colDef(name = "Lower", style = function(value, index) {
          acute <- load_plan_table$explosive_efforts_acute[index]
          # Calculate relative width (ratio between 0 and 1)
          # min/max constraints ensure the bar fills up nicely even if acute > value
          percentage_width <- min(max(acute / value, 0), 1) 
          bar_style(width = percentage_width, fill = rgb(0, 176, 185,alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
        }
        ),
        explosive_efforts_upper = colDef(name = "Upper", style = function(value, index) {
          acute <- load_plan_table$explosive_efforts_acute[index]
          # Calculate relative width (ratio between 0 and 1)
          # min/max constraints ensure the bar fills up nicely even if acute > value
          percentage_width <- min(max(acute / value, 0), 1) 
          bar_style(width = percentage_width, fill = rgb(87, 44, 95,alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
        }
        ),
        explosive_efforts_remaining = colDef(name = "Remaining")
      ),
      columnGroups = list(
        colGroup(name = "Total Distance (m)", columns = c("total_distance_acute", "total_distance_chronic","total_distance_acwr", "total_distance_lower",  "total_distance_upper","total_distance_remaining")),
        colGroup(name = "Dive Count", columns = c("dive_count_acute","dive_count_chronic","dive_count_acwr", "dive_count_lower",  "dive_count_upper","dive_count_remaining")),
        colGroup(name = "Total Dive Load", columns =c("total_dive_load_acute", "total_dive_load_chronic","total_dive_load_acwr", "total_dive_load_lower",  "total_dive_load_upper","total_dive_load_remaining")),
        colGroup(name = "Explosive Efforts", columns = c("explosive_efforts_acute","explosive_efforts_chronic","explosive_efforts_acwr", "explosive_efforts_lower",  "explosive_efforts_upper","explosive_efforts_remaining"))
      ),
      details = function(index) {
        player_name <- load_plan_table$Player[index]
        weekly_info <- weekly_details %>% filter(Player == player_name)
        
        htmltools::div(
          style = "padding: 1rem; background-color: #fcfcfc;",
          reactable(
            weekly_info %>% select(!c(Player, date)),
            striped = F, outline = F, bordered = T, compact = T, highlight = F, fullWidth = F,
            defaultColDef = colDef(align = "center"), 
            columns = list(
              formatted_date = colDef(name = "Date", align="left",sortable = FALSE),
              total_distance = colDef(name = "Total Distance (m)", cell = function(val, r_idx) {
                p_clean <- stringr::str_replace_all(player_name, " ", "_")
                as.character(numericInput(paste("inp", "total_distance", p_clean, weekly_info$date[r_idx], sep = "__"), NULL, value = round(val),min = 0,  width = "100px"))
              }, html = TRUE),
              dive_count = colDef(name = "Dive Count", cell = function(val, r_idx) {
                p_clean <- stringr::str_replace_all(player_name, " ", "_")
                as.character(numericInput(paste("inp", "dive_count", p_clean, weekly_info$date[r_idx], sep = "__"), NULL, value = round(val), min = 0, width = "100px"))
              }, html = TRUE),
              total_dive_load = colDef(name = "Total Dive Load", cell = function(val, r_idx) {
                p_clean <- stringr::str_replace_all(player_name, " ", "_")
                as.character(numericInput(paste("inp", "total_dive_load", p_clean, weekly_info$date[r_idx], sep = "__"), NULL, value = round(val), min = 0, width = "100px"))
              }, html = TRUE),
              explosive_efforts = colDef(name = "Explosive Efforts", cell = function(val, r_idx) {
                p_clean <- stringr::str_replace_all(player_name, " ", "_")
                as.character(numericInput(paste("inp", "explosive_efforts", p_clean, weekly_info$date[r_idx], sep = "__"), NULL, value = round(val), min = 0, width = "100px"))
              }, html = TRUE)
            )
          )
        )
      }
    )
  })
  
  player_load_stats2 <- reactive({
    
    shiny::validate(need(!is.null(input$athlete2), "Select a player"))
    
    stats %>%
      select(athlete_name | date | tag_name | (starts_with("zscore_7_28") & !contains("wellness") & !contains("RSI"))) %>% 
      rename(internal_load=zscore_7_28_max_heart_rate, subjective_load = zscore_7_28_rpe) %>% 
      rename_with(~str_replace(.x,"zscore_7_28", "external_load")) %>% 
      pivot_longer(cols = starts_with("external_load"), names_to = "external_load_param", values_to = "external_load") %>% 
      dplyr::filter(date <= input$date_input1 & date > input$date_input1 - days(5) & athlete_name == input$athlete2 & external_load_param == input$ext_load_param) 
    # %>% 
      # group_by(date, tag_name) %>%  
      # summarize(across(where(is.numeric), ~mean(.x,na.rm=T))) %>% 
      # ungroup
  })

  
  # text <- data.frame(x=c(-1.5,-1.5, 1.5, 1.5), 
  #                    y= c(-1.5,1.5, -1.5,1.5), 
  #                    label=c("Increase Load", "Maladaptation", "Adaptation", "Decrease Load"))
  # 
  # 
  # text2 <- data.frame(x=c(-1.5,-1.5, 1.5, 1.5), 
  #                     y= c(-1.5,1.5, -1.5,1.5), 
  #                     label=c("Investigate\nExternal Factors", "Increase Workload", "Decrease Workload", "Continue Training"))
  # 
  # text3 <- data.frame(x=c(-1.5,-1.5, 1.5, 1.5), 
  #                     y= c(-1.5,1.5, -1.5,1.5), 
  #                     label=c("Extra Recovery", "Increase Mental\nPreparation", "Increase Physical\nPreparation", "Ready to Train/Play"))


  # int_ext_load_plot <- reactive({
  #   
  #   
  #   
  #   plot_ly() %>% 
  #     add_annotations(xref='x', yref='y', x=text$x, y=text$y, text=text$label, showarrow = FALSE, align="center",font = list(color = rgb(0, 0, 0, 0.4),weight=600, size = 12))%>%
  #     add_trace(x=player_load_stats2()$external_load, y=player_load_stats2()$internal_load, type="scatter", mode="markers",opacity=1, marker=list(color="#00B0B9", size=14),
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
  #       xaxis = list(range=c(-3,3),scaleanchor = "y", scaleratio = 1, constrain="domain",constraintoward="center", zeroline=FALSE, showticklabels = FALSE,showline=FALSE,showgrid = FALSE,
  #                    title = case_when(input$ext_load_param == "external_load_field_time"~ "Field Time", 
  #                                                                  input$ext_load_param == "external_load_total_distance"~"Total Distance", 
  #                                                                  input$ext_load_param == "external_load_high_speed_distance"~ "High Speed Distance", 
  #                                                                  input$ext_load_param == "external_load_sprint_distance"~ "Sprint Distance",
  #                                                                  input$ext_load_param == "external_load_accel_efforts"~ "Accel Efforts",
  #                                                                  input$ext_load_param == "external_load_decel_efforts" ~"Decel Efforts", 
  #                                                                  input$ext_load_param == "external_load_dive_count"~"Dive Count", 
  #                                                                  input$ext_load_param == "external_load_total_dive_load"~"Total Dive Load", 
  #                                                                  input$ext_load_param == "external_load_explosive_efforts"~"Explosive Efforts", 
  #                                                                  .default = "")),
  #       yaxis = list(range=c(-3,3),scaleanchor = "x", scaleratio = 1,constrain="domain", constraintoward="center", zeroline=FALSE, showticklabels = FALSE,showline=FALSE,showgrid = FALSE,title = "Max HR"),
  #       plot_bgcolor  = rgb(0,0,0,0),
  #       paper_bgcolor = rgb(0,0,0,0))
  #   
  # })
  

  
  sub_ext_load_plot <- reactive({
    
    # plot_ly() %>%
    #   add_annotations(xref='x', yref='y', x=text$x, y=text$y, text=text$label, showarrow = FALSE, align="center",font = list(color = rgb(0, 0, 0, 0.4),weight=600, size = 12))%>%
    #   add_trace(x=player_load_stats2()$external_load, y=player_load_stats2()$subjective_load, type="scatter", mode="markers",opacity=1, marker=list(color="#00B0B9", size=14),
    #             hovertemplate = paste0(
    #               "<b>Z-Score (3-day avg vs. 28-day avg):</b><br>",
    #               "<b>%{xaxis.title.text}:</b> %{x:.2f}<br>",
    #               "<b>%{yaxis.title.text}:</b> %{y:.2f}",
    #               "<extra></extra>")) %>%
    #   config(displaylogo = FALSE, scrollZoom = FALSE, displayModeBar = FALSE) %>%
    #   layout(
    #     shapes = list(
    #       list(type = "line", xref='x', yref='y', x0 = -3, x1 = 3, y0 = 0, y1 = 0, layer = "below"),
    #       list(type = "line", xref='x', yref='y', x0 = 0, x1 = 0, y0 = -3, y1 = 3, layer = "below"),
    #       list(type = "line", xref='x', yref='y', x0 = -3, x1 = -3, y0 = -3, y1 = 3, layer = "below"),
    #       list(type = "line", xref='x', yref='y', x0 = 3, x1 = 3, y0 = -3, y1 = 3, layer = "below"),
    #       list(type = "line", xref='x', yref='y', x0 = -3, x1 = 3, y0 = -3, y1 = -3, layer = "below"),
    #       list(type = "line", xref='x', yref='y', x0 = -3, x1 = 3, y0 = 3, y1 = 3, layer = "below")),
    #     xaxis = list(range=c(-3,3),scaleanchor = "y", scaleratio = 1, constrain="domain", constraintoward="center", zeroline=FALSE, showticklabels = FALSE,showline=FALSE,showgrid = FALSE,
    #                  title = case_when(input$ext_load_param == "external_load_field_time"~ "Field Time",
    #                                                                input$ext_load_param == "external_load_total_distance"~"Total Distance",
    #                                                                input$ext_load_param == "external_load_high_speed_distance"~ "High Speed Distance",
    #                                                                input$ext_load_param == "external_load_sprint_distance"~ "Sprint Distance",
    #                                                                input$ext_load_param == "external_load_accel_efforts"~ "Accel Efforts",
    #                                                                input$ext_load_param == "external_load_decel_efforts" ~"Decel Efforts",
    #                                                                input$ext_load_param == "external_load_dive_count"~"Dive Count",
    #                                                                input$ext_load_param == "external_load_total_dive_load"~"Total Dive Load",
    #                                                                input$ext_load_param == "external_load_explosive_efforts"~"Explosive Efforts",
    #                                                                .default = "")),
    #     yaxis = list(range=c(-3,3),scaleanchor = "x", scaleratio = 1, constrain="domain",constraintoward="center", zeroline=FALSE, showticklabels = FALSE,showline=FALSE,showgrid = FALSE,title = "RPE"),
    #     plot_bgcolor  = rgb(0,0,0,0),
    #     paper_bgcolor = rgb(0,0,0,0))
    
    player_load_stats2() %>% 
      plot_ly() %>% 
      add_trace(x=~external_load, 
                y=~subjective_load, 
                type="scatter", 
                mode="markers",
                color=~factor(date),
                colors= brewer.pal(n = nrow(player_load_stats2()),name="Blues"),
                # colors= viridis(n = nrow(responses_filtered),direction=1,option = "G"),
                customdata=~paste0(format(date,"%b %d, %Y"), "<br><b>MD Code:</b> ", tag_name),
                marker=list(size=12),
                hovertemplate = paste0(
                  "<b>Date:</b> %{customdata}<br>",
                  "<b>Z-Score</b> (3-day avg vs. 28-day avg)<br>",
                  "<b>%{xaxis.title.text}:</b> %{x:.2f}<br>",
                  "<b>%{yaxis.title.text}:</b> %{y:.2f}",
                  "<extra></extra>")) %>% 
      config(displaylogo = FALSE,
             scrollZoom = FALSE,
             displayModeBar = FALSE) %>%     
      layout(
        legend = list(traceorder = "reversed"),
        shapes = list(
          list(type="rect", xref='x', yref='y', x0 = -2, x1 = 2, y0 = -2, y1 = 2,layer = "below", line = list(width=0), fillcolor="red",opacity = 0.2),
          list(type="rect", xref='x', yref='y', x0 = -1.5, x1 =1.5, y0 = -1.5, y1 = 1.5,layer = "below", line = list(width=0), fillcolor="yellow",opacity = 0.2),
          list(type="rect", xref='x', yref='y', x0 = -1, x1 = 1, y0 = -1, y1 = 1,layer = "below", line = list(width=0), fillcolor="green",opacity = 0.2)),
        xaxis = list(range=c(-2.1,2.1),scaleanchor = "y", scaleratio = 1,constrain="domain",constraintoward="center", zeroline=T, showticklabels = T,showline=F,showgrid = T,ticks = "",tickvals = seq(-3,3,by=0.5), title = case_when(input$ext_load_param == "external_load_field_time"~ "Field Time",
                                                                                                                                                                                                                                                                   input$ext_load_param == "external_load_total_distance"~"Total Distance",
                                                                                                                                                                                                                                                                   input$ext_load_param == "external_load_high_speed_distance"~ "High Speed Distance",
                                                                                                                                                                                                                                                                   input$ext_load_param == "external_load_sprint_distance"~ "Sprint Distance",
                                                                                                                                                                                                                                                                   input$ext_load_param == "external_load_accel_efforts"~ "Accel Efforts",
                                                                                                                                                                                                                                                                   input$ext_load_param == "external_load_decel_efforts" ~"Decel Efforts",
                                                                                                                                                                                                                                                                   input$ext_load_param == "external_load_dive_count"~"Dive Count",
                                                                                                                                                                                                                                                                   input$ext_load_param == "external_load_total_dive_load"~"Total Dive Load",
                                                                                                                                                                                                                                                                   input$ext_load_param == "external_load_explosive_efforts"~"Explosive Efforts",
                                                                                                                                                                                                                                                                   .default = "")),
        yaxis = list(range=c(-2.1,2.1),scaleanchor = "x", scaleratio = 1,constrain = "domain",constraintoward="center", zeroline=T, showticklabels = T,showline=F,showgrid = T, ticks = "",tickvals= seq(-3,3,by=0.5),  title = "Total Daily Session RPE"),
        plot_bgcolor  = rgb(0,0,0,0),
        paper_bgcolor = rgb(0,0,0,0))
    
  })
  
  wellness_workload_plot <- reactive({
    
    shiny::validate(need(!is.null(input$athlete2), "Select a player"))
    
    player_load_stats4 <- stats %>%
      select(athlete_name | date | tag_name | starts_with("zscore_7_28")) %>% 
      rename(wellness = zscore_7_28_wellness) %>%
      rename_with(~ str_replace(.x, "zscore_7_28", "workload")) %>% 
      pivot_longer(cols = starts_with("workload"), names_to = "workload_param", values_to = "workload") %>% 
      dplyr::filter(workload_param == input$workload_param) %>% 
      arrange(athlete_name, date) %>% 
      # group_by(athlete_name) %>% 
      # mutate(workload2 = dplyr::lag(workload)) %>% 
      # ungroup %>% 
      dplyr::filter(date <= input$date_input1 & date > input$date_input1 - days(5) & athlete_name == input$athlete2) 
    # %>% 
    #   group_by(date, tag_name) %>% 
    #   summarize(across(where(is.numeric), ~mean(.x,na.rm=T))) %>% 
    #   ungroup
    
    # plot_ly() %>% 
    #   add_annotations(xref='x', yref='y', x=text2$x, y=text2$y, text=text2$label, showarrow = FALSE, align="center",font = list(color = rgb(0, 0, 0, 0.4),weight=600, size = 12))%>%
    #   add_trace(x=player_load_stats4$workload, y=player_load_stats4$wellness, type="scatter", mode="markers",opacity=1, marker=list(color="#00B0B9", size=14),
    #             hovertemplate = paste0(
    #               "<b>Z-Score (3-day avg vs. 28-day avg):</b><br>",
    #               "<b>%{xaxis.title.text}:</b> %{x:.2f}<br>",
    #               "<b>%{yaxis.title.text}:</b> %{y:.2f}",
    #               "<extra></extra>")) %>% 
    #   config(displaylogo = FALSE, scrollZoom = FALSE, displayModeBar = FALSE) %>%
    #   layout(
    #     shapes = list(
    #       list(type = "line", xref='x', yref='y', x0 = -3, x1 = 3, y0 = 0, y1 = 0, layer = "below"),
    #       list(type = "line", xref='x', yref='y', x0 = 0, x1 = 0, y0 = -3, y1 = 3, layer = "below"),
    #       list(type = "line", xref='x', yref='y', x0 = -3, x1 = -3, y0 = -3, y1 = 3, layer = "below"),
    #       list(type = "line", xref='x', yref='y', x0 = 3, x1 = 3, y0 = -3, y1 = 3, layer = "below"),
    #       list(type = "line", xref='x', yref='y', x0 = -3, x1 = 3, y0 = -3, y1 = -3, layer = "below"),
    #       list(type = "line", xref='x', yref='y', x0 = -3, x1 = 3, y0 = 3, y1 = 3, layer = "below")),
    #     xaxis = list(range=c(-3,3),scaleanchor = "y", scaleratio = 1,constrain="domain", constraintoward="center", zeroline=FALSE, showticklabels = FALSE,showline=FALSE,showgrid = FALSE,
    #                  title = case_when(input$workload_param == "workload_field_time"~ "Field Time", 
    #                                          input$workload_param == "workload_total_distance"~"Total Distance",
    #                                          input$workload_param == "workload_high_speed_distance"~ "High Speed Distance",
    #                                          input$workload_param == "workload_sprint_distance"~ "Sprint Distance",
    #                                          input$workload_param == "workload_accel_efforts"~ "Accel Efforts",
    #                                          input$workload_param == "workload_decel_efforts" ~"Decel Efforts",
    #                                          input$workload_param == "workload_dive_count"~"Dive Count",
    #                                          input$workload_param == "workload_total_dive_load"~"Total Dive Load",
    #                                          input$workload_param == "workload_explosive_efforts"~"Explosive Efforts",
    #                                          input$workload_param == "workload_rpe"~"RPE",
    #                                          input$workload_param == "workload_max_heart_rate"~"Max HR",
    #                                          .default = "")),
    #     yaxis = list(range=c(-3,3),scaleanchor = "x", scaleratio = 1, constrain="domain",constraintoward="center", zeroline=FALSE, showticklabels = FALSE,showline=FALSE,showgrid = FALSE,title = "Wellness"),
    #     plot_bgcolor  = rgb(0,0,0,0),
    #     paper_bgcolor = rgb(0,0,0,0))
    # 
    
    player_load_stats4 %>% 
      plot_ly() %>% 
      add_trace(x=~workload, 
                y=~wellness, 
                type="scatter", 
                mode="markers",
                color=~factor(date),
                colors= brewer.pal(n = nrow(player_load_stats4),name="Blues"),
                # colors= viridis(n = nrow(responses_filtered),direction=1,option = "G"),
                customdata=~paste0(format(date,"%b %d, %Y"), "<br><b>MD Code:</b> ", tag_name),
                marker=list(size=12),
                hovertemplate = paste0(
                  "<b>Date:</b> %{customdata}<br>",
                  "<b>Z-Score</b> (3-day avg vs. 28-day avg)<br>",
                  "<b>%{xaxis.title.text}:</b> %{x:.2f}<br>",
                  "<b>%{yaxis.title.text}:</b> %{y:.2f}",
                  "<extra></extra>")) %>% 
      config(displaylogo = FALSE,
             scrollZoom = FALSE,
             displayModeBar = FALSE) %>%     
      layout(
        legend = list(traceorder = "reversed"),
        shapes = list(
          list(type="rect", xref='x', yref='y', x0 = -2, x1 = 2, y0 = -2, y1 = 2,layer = "below", line = list(width=0), fillcolor="red",opacity = 0.2),
          list(type="rect", xref='x', yref='y', x0 = -1.5, x1 =1.5, y0 = -1.5, y1 = 1.5,layer = "below", line = list(width=0), fillcolor="yellow",opacity = 0.2),
          list(type="rect", xref='x', yref='y', x0 = -1, x1 = 1, y0 = -1, y1 = 1,layer = "below", line = list(width=0), fillcolor="green",opacity = 0.2)),
        xaxis = list(range=c(-2.1,2.1),scaleanchor = "y", scaleratio = 1,constrain="domain",constraintoward="center", zeroline=T, showticklabels = T,showline=F,showgrid = T,ticks = "",tickvals = seq(-3,3,by=0.5), title = case_when(input$workload_param == "workload_field_time"~ "Field Time", 
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
                                                                                                                                                                                                                                             .default = "")),
        yaxis = list(range=c(-2.1,2.1),scaleanchor = "x", scaleratio = 1,constrain = "domain",constraintoward="center", zeroline=T, showticklabels = T,showline=F,showgrid = T, ticks = "",tickvals= seq(-3,3,by=0.5),  title = "Wellness"),
        plot_bgcolor  = rgb(0,0,0,0),
        paper_bgcolor = rgb(0,0,0,0))
    
    
  })
  
  
  # readiness_wellness_plot <- reactive({
  # 
  #   shiny::validate(need(!is.null(input$athlete2), "Select a player"))
  # 
  #   player_load_stats4 <- stats %>%
  #     select(athlete_name | date | zscore_7_28_wellness | zscore_7_28_RSI) %>%
  #     rename(wellness = zscore_7_28_wellness, readiness = zscore_7_28_RSI) %>%
  #     arrange(athlete_name, date) %>%
  #     group_by(athlete_name) %>%
  #     mutate(readiness2 = dplyr::lag(readiness)) %>%
  #     ungroup %>%
  #     dplyr::filter(date == input$date_input1 & athlete_name == input$athlete2) 
  #   # %>%
  #   #   group_by(date) %>%
  #   #   summarize(across(where(is.numeric), ~mean(.x,na.rm=T))) %>%
  #   #   ungroup
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
  #       yaxis = list(range=c(-3,3),scaleanchor = "x", scaleratio = 1, constrain="domain",constraintoward="center", zeroline=FALSE, showticklabels = FALSE,showline=FALSE,showgrid = FALSE,title = "Reactive Strength Index"),
  #       plot_bgcolor  = rgb(0,0,0,0),
  #       paper_bgcolor = rgb(0,0,0,0))
  # 
  # })
  
  
  zscore_heatmap_table  <- reactive({ 
    
    
    if ("Goal Keeper" %in% (stats %>% filter(athlete_name == input$athlete2) %>% drop_na(position_name) %>% pull(position_name) %>% unique)){
    player_load_stats <- stats %>%
      select(athlete_name | date | tag_name | starts_with("zscore_7_28")) %>% 
      rename_with(~str_remove(.x,"zscore_7_28_")) %>%
      dplyr::filter(date <= input$date_input1 & date > input$date_input1 - days(14) & athlete_name == input$athlete2)%>% 
      select(!c(athlete_name, high_speed_distance, sprint_distance,meterage_per_minute,max_vel_kph, accel_efforts, decel_efforts, accel_decel_efforts)) %>% 
      rename(Date=date, `MD Code` = tag_name, `Total Distance` = total_distance, `Field Time`=field_time, `Dive Count` = dive_count, `Dive Load` = total_dive_load, `Explosive Efforts` = explosive_efforts, `Avg HR` = mean_heart_rate, `Max HR` = max_heart_rate,`Daily sRPE` = rpe, Wellness=wellness) %>% 
      arrange(desc(Date))
    
    reactable(
      player_load_stats,
      striped = F,
      outline=F,
      bordered = T,
      compact = T,
      highlight = F,
      defaultPageSize =nrow(player_load_stats),
      columns = list(
        `Field Time` = colDef(style = function(value) {
          if (value > 1.5 || value < -1.5) {
            list(background = rgb(255, 0, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
          } else if ((value >= 1 && value <= 1.5) || (value >= -1.5 && value <= -1)) {
            list(background = rgb(255, 165, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
          } else if (value > -1 && value < 1) {
            list(background = rgb(0, 255, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
          } else {
            list(background = NULL) # Default style for any missing cases
          }
        }, format = colFormat(digits = 2), align = "center"),
        `Total Distance` = colDef(style = function(value) {
          if (value > 1.5 || value < -1.5) {
            list(background = rgb(255, 0, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
          } else if ((value >= 1 && value <= 1.5) || (value >= -1.5 && value <= -1)) {
            list(background = rgb(255, 165, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
          } else if (value > -1 && value < 1) {
            list(background = rgb(0, 255, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
          } else {
            list(background = NULL) # Default style for any missing cases
          }
        }, format = colFormat(digits = 2),align = "center"),
        `Dive Count` = colDef(style = function(value) {
          if (value > 1.5 || value < -1.5) {
            list(background = rgb(255, 0, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
          } else if ((value >= 1 && value <= 1.5) || (value >= -1.5 && value <= -1)) {
            list(background = rgb(255, 165, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
          } else if (value > -1 && value < 1) {
            list(background = rgb(0, 255, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
          } else {
            list(background = NULL) # Default style for any missing cases
          }
        }, format = colFormat(digits = 2),align = "center"),
        `Dive Load` = colDef(style = function(value) {
          if (value > 1.5 || value < -1.5) {
            list(background = rgb(255, 0, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
          } else if ((value >= 1 && value <= 1.5) || (value >= -1.5 && value <= -1)) {
            list(background = rgb(255, 165, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
          } else if (value > -1 && value < 1) {
            list(background = rgb(0, 255, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
          } else {
            list(background = NULL) # Default style for any missing cases
          }
        }, format = colFormat(digits = 2),align = "center"),
        `Explosive Efforts` = colDef(style = function(value) {
          if (value > 1.5 || value < -1.5) {
            list(background = rgb(255, 0, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
          } else if ((value >= 1 && value <= 1.5) || (value >= -1.5 && value <= -1)) {
            list(background = rgb(255, 165, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
          } else if (value > -1 && value < 1) {
            list(background = rgb(0, 255, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
          } else {
            list(background = NULL) # Default style for any missing cases
          }
        }, format = colFormat(digits = 2),align = "center"),
        `Avg HR` = colDef(style = function(value) {
          if (value > 1.5 || value < -1.5) {
            list(background = rgb(255, 0, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
          } else if ((value >= 1 && value <= 1.5) || (value >= -1.5 && value <= -1)) {
            list(background = rgb(255, 165, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
          } else if (value > -1 && value < 1) {
            list(background = rgb(0, 255, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
          } else {
            list(background = NULL) # Default style for any missing cases
          }
        }, format = colFormat(digits = 2),align = "center"),
        `Max HR` = colDef(style = function(value) {
          if (value > 1.5 || value < -1.5) {
            list(background = rgb(255, 0, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
          } else if ((value >= 1 && value <= 1.5) || (value >= -1.5 && value <= -1)) {
            list(background = rgb(255, 165, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
          } else if (value > -1 && value < 1) {
            list(background = rgb(0, 255, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
          } else {
            list(background = NULL) # Default style for any missing cases
          }
        }, format = colFormat(digits = 2),align = "center"),
        `Daily sRPE` = colDef(style = function(value) {
          if (value > 1.5 || value < -1.5) {
            list(background = rgb(255, 0, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
          } else if ((value >= 1 && value <= 1.5) || (value >= -1.5 && value <= -1)) {
            list(background = rgb(255, 165, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
          } else if (value > -1 && value < 1) {
            list(background = rgb(0, 255, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
          } else {
            list(background = NULL) # Default style for any missing cases
          }
        }, format = colFormat(digits = 2),align = "center"),
        Wellness = colDef(style = function(value) {
          if (value > 1.5 || value < -1.5) {
            list(background = rgb(255, 0, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
          } else if ((value >= 1 && value <= 1.5) || (value >= -1.5 && value <= -1)) {
            list(background = rgb(255, 165, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
          } else if (value > -1 && value < 1) {
            list(background = rgb(0, 255, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
          } else {
            list(background = NULL) # Default style for any missing cases
          }
        }, format = colFormat(digits = 2),align = "center")
      )
    )
    
    } else {
      
      player_load_stats <- stats %>%
        select(athlete_name | date | tag_name | starts_with("zscore_7_28")) %>% 
        rename_with(~str_remove(.x,"zscore_7_28_")) %>%
        dplyr::filter(date <= input$date_input1 & date > input$date_input1 - days(5) & athlete_name == input$athlete2)%>% 
        select(!c(athlete_name, dive_count, total_dive_load,explosive_efforts)) %>% 
        rename(Date=date, `MD Code` = tag_name, `Total Distance` = total_distance, `HSR Distance` = high_speed_distance, `Sprint Distance` = sprint_distance, `Accel Efforts` = accel_efforts, `Decel Efforts`=decel_efforts, `Accel+Decel Efforts`=accel_decel_efforts, `Avg Speed` = meterage_per_minute, `Max Speed` = max_vel_kph, `Avg HR` = mean_heart_rate, `Max HR` = max_heart_rate, `Field Time`=field_time, `Daily sRPE` = rpe, Wellness=wellness) %>% 
        arrange(desc(Date))
   
      reactable(
        player_load_stats,
        striped = F,
        outline=F,
        bordered = T,
        compact = T,
        highlight = F,
        defaultPageSize =nrow(player_load_stats),
        columns = list(
          `Field Time` = colDef(style = function(value) {
            if (value > 1.5 || value < -1.5) {
              list(background = rgb(255, 0, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
            } else if ((value >= 1 && value <= 1.5) || (value >= -1.5 && value <= -1)) {
              list(background = rgb(255, 165, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
            } else if (value > -1 && value < 1) {
              list(background = rgb(0, 255, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
            } else {
              list(background = NULL) # Default style for any missing cases
            }
          }, format = colFormat(digits = 2), align = "center"),
          `Total Distance` = colDef(style = function(value) {
            if (value > 1.5 || value < -1.5) {
              list(background = rgb(255, 0, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
            } else if ((value >= 1 && value <= 1.5) || (value >= -1.5 && value <= -1)) {
              list(background = rgb(255, 165, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
            } else if (value > -1 && value < 1) {
              list(background = rgb(0, 255, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
            } else {
              list(background = NULL) # Default style for any missing cases
            }
          }, format = colFormat(digits = 2),align = "center"),
          `HSR Distance` = colDef(style = function(value) {
            if (value > 1.5 || value < -1.5) {
              list(background = rgb(255, 0, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
            } else if ((value >= 1 && value <= 1.5) || (value >= -1.5 && value <= -1)) {
              list(background = rgb(255, 165, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
            } else if (value > -1 && value < 1) {
              list(background = rgb(0, 255, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
            } else {
              list(background = NULL) # Default style for any missing cases
            }
          }, format = colFormat(digits = 2),align = "center"),
          `Sprint Distance` = colDef(style = function(value) {
            if (value > 1.5 || value < -1.5) {
              list(background = rgb(255, 0, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
            } else if ((value >= 1 && value <= 1.5) || (value >= -1.5 && value <= -1)) {
              list(background = rgb(255, 165, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
            } else if (value > -1 && value < 1) {
              list(background = rgb(0, 255, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
            } else {
              list(background = NULL) # Default style for any missing cases
            }
          }, format = colFormat(digits = 2),align = "center"),
          `Accel Efforts` = colDef(style = function(value) {
            if (value > 1.5 || value < -1.5) {
              list(background = rgb(255, 0, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
            } else if ((value >= 1 && value <= 1.5) || (value >= -1.5 && value <= -1)) {
              list(background = rgb(255, 165, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
            } else if (value > -1 && value < 1) {
              list(background = rgb(0, 255, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
            } else {
              list(background = NULL) # Default style for any missing cases
            }
          }, format = colFormat(digits = 2),align = "center"),
          `Decel Efforts` = colDef(style = function(value) {
            if (value > 1.5 || value < -1.5) {
              list(background = rgb(255, 0, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
            } else if ((value >= 1 && value <= 1.5) || (value >= -1.5 && value <= -1)) {
              list(background = rgb(255, 165, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
            } else if (value > -1 && value < 1) {
              list(background = rgb(0, 255, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
            } else {
              list(background = NULL) # Default style for any missing cases
            }
          }, format = colFormat(digits = 2),align = "center"),
          `Accel+Decel Efforts` = colDef(style = function(value) {
            if (value > 1.5 || value < -1.5) {
              list(background = rgb(255, 0, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
            } else if ((value >= 1 && value <= 1.5) || (value >= -1.5 && value <= -1)) {
              list(background = rgb(255, 165, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
            } else if (value > -1 && value < 1) {
              list(background = rgb(0, 255, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
            } else {
              list(background = NULL) # Default style for any missing cases
            }
          }, format = colFormat(digits = 2),align = "center"),
          `Avg Speed` = colDef(style = function(value) {
            if (value > 1.5 || value < -1.5) {
              list(background = rgb(255, 0, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
            } else if ((value >= 1 && value <= 1.5) || (value >= -1.5 && value <= -1)) {
              list(background = rgb(255, 165, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
            } else if (value > -1 && value < 1) {
              list(background = rgb(0, 255, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
            } else {
              list(background = NULL) # Default style for any missing cases
            }
          }, format = colFormat(digits = 2),align = "center"),
          `Max Speed` = colDef(style = function(value) {
            if (value > 1.5 || value < -1.5) {
              list(background = rgb(255, 0, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
            } else if ((value >= 1 && value <= 1.5) || (value >= -1.5 && value <= -1)) {
              list(background = rgb(255, 165, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
            } else if (value > -1 && value < 1) {
              list(background = rgb(0, 255, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
            } else {
              list(background = NULL) # Default style for any missing cases
            }
          }, format = colFormat(digits = 2),align = "center"),
          `Avg HR` = colDef(style = function(value) {
            if (value > 1.5 || value < -1.5) {
              list(background = rgb(255, 0, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
            } else if ((value >= 1 && value <= 1.5) || (value >= -1.5 && value <= -1)) {
              list(background = rgb(255, 165, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
            } else if (value > -1 && value < 1) {
              list(background = rgb(0, 255, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
            } else {
              list(background = NULL) # Default style for any missing cases
            }
          }, format = colFormat(digits = 2),align = "center"),
          `Max HR` = colDef(style = function(value) {
            if (value > 1.5 || value < -1.5) {
              list(background = rgb(255, 0, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
            } else if ((value >= 1 && value <= 1.5) || (value >= -1.5 && value <= -1)) {
              list(background = rgb(255, 165, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
            } else if (value > -1 && value < 1) {
              list(background = rgb(0, 255, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
            } else {
              list(background = NULL) # Default style for any missing cases
            }
          }, format = colFormat(digits = 2),align = "center"),
          `Daily sRPE` = colDef(style = function(value) {
            if (value > 1.5 || value < -1.5) {
              list(background = rgb(255, 0, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
            } else if ((value >= 1 && value <= 1.5) || (value >= -1.5 && value <= -1)) {
              list(background = rgb(255, 165, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
            } else if (value > -1 && value < 1) {
              list(background = rgb(0, 255, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
            } else {
              list(background = NULL) # Default style for any missing cases
            }
          }, format = colFormat(digits = 2),align = "center"),
          Wellness = colDef(style = function(value) {
            if (value > 1.5 || value < -1.5) {
              list(background = rgb(255, 0, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
            } else if ((value >= 1 && value <= 1.5) || (value >= -1.5 && value <= -1)) {
              list(background = rgb(255, 165, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
            } else if (value > -1 && value < 1) {
              list(background = rgb(0, 255, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
            } else {
              list(background = NULL) # Default style for any missing cases
            }
          }, format = colFormat(digits = 2),align = "center")
          )
      )
       }
    

    
  })
  
  
  
  wellness_plot <- reactive({
    
    shiny::validate(need(!is.null(input$athlete3), "Select one or more players"))
    
    wellness_stats <- wellness_scores %>%
      dplyr::filter(date == input$date_input2 & athlete_name %in% input$athlete3) %>%
      group_by(date, category, name) %>% 
      summarize(across(where(is.numeric), ~mean(.x,na.rm=T))) %>% 
      ungroup
    
    plot_ly(data=wellness_stats, x = ~category, y = ~category_item_ratio, type = "bar", color=~name,
            colors = c("#403A60", "#572C5F","#00B0B9", "#B2C9D4","#E5E1E6"),
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
  
  wellness_table <- reactive({
    
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

    wellness_stats <- rbind(total_wellness, category_wellness) %>%
      dplyr::filter(date >= input$date_range3[1] & date <= input$date_range3[2] & athlete_name %in% input$athlete3) %>%
      group_by(date, category, name) %>% 
      summarize(ratio = mean(ratio,na.rm=T)) %>% 
      ungroup %>% 
      select(!category) %>% 
      pivot_wider(names_from=name, values_from=ratio) %>% 
      relocate(`Total Wellness`, .after=date) %>% 
      arrange(desc(date))
    
    
    # wellness_stats <- rbind(total_wellness, category_wellness) %>%
    #   dplyr::filter(date >= (Sys.Date()-weeks(3)) & date <= Sys.Date() & athlete_name %in% c("Saorla Miller")) %>%
    #   group_by(date, category, name) %>% 
    #   summarize(ratio = mean(ratio,na.rm=T)) %>% 
    #   ungroup %>% 
    #   select(!category) %>% 
    #   pivot_wider(names_from=name, values_from=ratio) %>% 
    #   relocate(`Total Wellness`, .after=date) %>% 
    #   arrange(desc(date))
    
    reactable(
      wellness_stats,
      striped = F,
      outline=F,
      bordered = T,
      compact = T,
      highlight = F,
      defaultPageSize =nrow(wellness_stats),
      defaultColDef = colDef(align = "center"), 
      columns = list(
        date = colDef(name="Date", align = "left"), 
        `Total Wellness` = colDef(format = colFormat(percent=T, digits = 1), style = function(value) {
          if (value >= 0 && value < 0.5) {
            list(background = rgb(255, 0, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
          } else if (value >= 0.5 && value < 0.6) {
            list(background = rgb(255, 165, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
          } else if (value >= 0.6 && value < 0.7) {
            list(background = rgb(255, 255, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
          } else if (value >= 0.7 && value < 0.8) {
            list(background = rgb(144, 238, 144, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
          } else if (value >= 0.8 && value <= 1) {
            list(background = rgb(0, 255, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
          } else {
            list(background = NULL) # Default style for any missing cases
          }
        }),
        `Total Health` = colDef(format = colFormat(percent=T, digits = 1),style = function(value) {
          if (value >= 0 && value < 0.5) {
            list(background = rgb(255, 0, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
          } else if (value >= 0.5 && value < 0.6) {
            list(background = rgb(255, 165, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
          } else if (value >= 0.6 && value < 0.7) {
            list(background = rgb(255, 255, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
          } else if (value >= 0.7 && value < 0.8) {
            list(background = rgb(144, 238, 144, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
          } else if (value >= 0.8 && value <= 1) {
            list(background = rgb(0, 255, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
          } else {
            list(background = NULL) # Default style for any missing cases
          }
        }),
        `Total Mental` = colDef(format = colFormat(percent=T, digits = 1), style = function(value) {
          if (value >= 0 && value < 0.5) {
            list(background = rgb(255, 0, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
          } else if (value >= 0.5 && value < 0.6) {
            list(background = rgb(255, 165, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
          } else if (value >= 0.6 && value < 0.7) {
            list(background = rgb(255, 255, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
          } else if (value >= 0.7 && value < 0.8) {
            list(background = rgb(144, 238, 144, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
          } else if (value >= 0.8 && value <= 1) {
            list(background = rgb(0, 255, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
          } else {
            list(background = NULL) # Default style for any missing cases
          }
        }),
        `Total Nutrition` = colDef(format = colFormat(percent=T, digits = 1),style = function(value) {
          if (value >= 0 && value < 0.5) {
            list(background = rgb(255, 0, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
          } else if (value >= 0.5 && value < 0.6) {
            list(background = rgb(255, 165, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
          } else if (value >= 0.6 && value < 0.7) {
            list(background = rgb(255, 255, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
          } else if (value >= 0.7 && value < 0.8) {
            list(background = rgb(144, 238, 144, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
          } else if (value >= 0.8 && value <= 1) {
            list(background = rgb(0, 255, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
          } else {
            list(background = NULL) # Default style for any missing cases
          }
        }),
        `Total Physical` = colDef(format = colFormat(percent=T, digits = 1),style = function(value) {
          if (value >= 0 && value < 0.5) {
            list(background = rgb(255, 0, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
          } else if (value >= 0.5 && value < 0.6) {
            list(background = rgb(255, 165, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
          } else if (value >= 0.6 && value < 0.7) {
            list(background = rgb(255, 255, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
          } else if (value >= 0.7 && value < 0.8) {
            list(background = rgb(144, 238, 144, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
          } else if (value >= 0.8 && value <= 1) {
            list(background = rgb(0, 255, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
          } else {
            list(background = NULL) # Default style for any missing cases
          }
        }),
        `Total Sleep` = colDef(format = colFormat(percent=T, digits = 1), style = function(value) {
          if (value >= 0 && value < 0.5) {
            list(background = rgb(255, 0, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
          } else if (value >= 0.5 && value < 0.6) {
            list(background = rgb(255, 165, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
          } else if (value >= 0.6 && value < 0.7) {
            list(background = rgb(255, 255, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
          } else if (value >= 0.7 && value < 0.8) {
            list(background = rgb(144, 238, 144, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
          } else if (value >= 0.8 && value <= 1) {
            list(background = rgb(0, 255, 0, alpha=(0.5*255), maxColorValue = 255), color = "#221C35")
          } else {
            list(background = NULL) # Default style for any missing cases
          }
        })
      )
    )
    
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
             yaxis = list(showline=TRUE,showgrid = TRUE, title = "Total Daily Session RPE"),
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
          Player = colDef(minWidth = 150, 
                          style = list(fontWeight = 600),
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
                        minWidth = 150, style = list(fontWeight = 600)),
        Player = colDef(minWidth = 150, style = list(fontWeight = 600)),
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
      dplyr::filter(athlete_name %in% input$athlete8 & activity_name == input$md_input & str_detect(period_name, "^\\d{1,2}\\. [[:alpha:]]{5,6} (?i)Half$")) %>%
      # dplyr::filter(activity_name == "18th May 2026 - MD 4  vs Vancouver (H)" & period_name %in% c("2. First Half", "3. Second Half")) %>%
      select(activity_name, position_name, athlete_name, period_name, field_time, total_distance, high_speed_distance, sprint_distance, accel_efforts, decel_efforts,max_vel_kph) %>% 
      mutate(across(where(is.numeric), ~if_else(field_time == 0 & total_distance == 0, NA_real_, .x))) %>% 
      pivot_wider(names_from = period_name, names_glue = "{period_name}_{.value}", values_from = c(field_time, total_distance, high_speed_distance, sprint_distance, accel_efforts, decel_efforts,max_vel_kph)) %>% 
      mutate(Sub = if_else(is.na(`2. First Half_field_time`) | `2. First Half_field_time` < (10*60), T, F)) %>% 
      pivot_longer(cols=contains(". "), names_to = c("period_name", ".value"), names_pattern = "(\\d{1,2}\\. [[:alpha:]]{5,6} (?i)Half)_(.*)") %>% 
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
        Player = colDef(minWidth = 150, 
                        style = list(fontWeight = 600)),
        `Field Time (min)` = colDef(format = colFormat(digits = 0), align = "center"),
        # `Total Distance (m)` = colDef(format = colFormat(digits = 0),align = "center",cell = function(value) {
        #   width <- paste0(value / max(match_day_summary$`Total Distance (m)`) * 100, "%")
        #   bar_chart(round(value), width = width, fill = rgb(0, 176, 185,alpha=(0.5*255), maxColorValue = 255),  background = "#E5E1E6")
        # }),
        `Total Distance (m)` = colDef(format = colFormat(digits = 0),align = "center", style = function(value) {
          bar_style(width = (value - min(match_day_summary$`Total Distance (m)`))/(max(match_day_summary$`Total Distance (m)`)-min(match_day_summary$`Total Distance (m)`)), fill = rgb(0, 176, 185,alpha=(0.5*255), maxColorValue = 255), color = "#221C35")}),
        `HSR Distance (m)` = colDef(format = colFormat(digits = 0),align = "center", style = function(value) {
          bar_style(width = (value - min(match_day_summary$`HSR Distance (m)`))/(max(match_day_summary$`HSR Distance (m)`)-min(match_day_summary$`HSR Distance (m)`)), fill = rgb(87, 44, 95,alpha=(0.5*255), maxColorValue = 255), color = "#221C35")}),
        `Sprint Distance (m)` = colDef(format = colFormat(digits = 0),align = "center", style = function(value) {
          bar_style(width = (value - min(match_day_summary$`Sprint Distance (m)`))/(max(match_day_summary$`Sprint Distance (m)`)-min(match_day_summary$`Sprint Distance (m)`)), fill = rgb(0, 176, 185,alpha=(0.5*255), maxColorValue = 255), color = "#221C35")}),
        `Accel Efforts` = colDef(format = colFormat(digits = 0),align = "center", style = function(value) {
          bar_style(width = (value - min(match_day_summary$`Accel Efforts`))/(max(match_day_summary$`Accel Efforts`)-min(match_day_summary$`Accel Efforts`)), fill = rgb(87, 44, 95,alpha=(0.5*255), maxColorValue = 255), color = "#221C35")}),
        `Decel Efforts` = colDef(format = colFormat(digits = 0),align = "center", style = function(value) {
          bar_style(width = (value - min(match_day_summary$`Decel Efforts`))/(max(match_day_summary$`Decel Efforts`)-min(match_day_summary$`Decel Efforts`)), fill = rgb(0, 176, 185,alpha=(0.5*255), maxColorValue = 255), color = "#221C35")}),
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
  # planned_load <- reactive({
  # 
  #   shiny::validate(need(!is.null(input$athlete4), "Select one or more players"))
  # 
  # 
  #   # player_load_stats3 <- stats %>%
  #   #   dplyr::filter(athlete_name %in% input$athlete4) %>%
  #   #   group_by(date) %>%
  #   #   summarize(across(where(is.numeric), ~mean(.x,na.rm=T))) %>%
  #   #   ungroup %>%
  #   #   select(c(date, input$acwr_param2))
  #   #
  #   # uniroot.all(function(x) ((sum(player_load_stats3 %>% dplyr::filter(date > max(date)-days(6)) %>% select(!date) %>% pull())+x)/7)/((sum(player_load_stats3 %>% dplyr::filter(date > max(date)-days(27)) %>% select(!date) %>% pull())+x)/28)-input$acwr_input,lower=0, upper = max(player_load_stats3 %>% select(!date) %>% pull(),na.rm=T)*2)
  # 
  #   player_load_stats3 <- stats %>%
  #     dplyr::filter(athlete_name %in% input$athlete4) %>%
  #     group_by(date) %>%
  #     summarize(across(where(is.numeric), ~mean(.x,na.rm=T))) %>%
  #     ungroup %>%
  #     select(date | !!rlang::sym(input$acwr_param2) | ((contains("al_ewma") | contains("cl_ewma")) & contains(input$acwr_param2)))
  # 
  #   uniroot.all(function(x) ((1-BETA(7))*(player_load_stats3 %>% dplyr::filter(date == max(date)) %>% select(contains("al_ewma")) %>% pull()) + BETA(7)*x)/((1-BETA(28))*(player_load_stats3 %>% dplyr::filter(date == max(date)) %>% select(contains("cl_ewma")) %>% pull()) + BETA(28)*x)-input$acwr_input,lower=0, upper = max(player_load_stats3 %>% select(!date & !contains("al_ewma") & ! contains("cl_ewma")) %>% pull(),na.rm=T)*2)
  # })
  
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
  
  # output$IntExtLoad <- renderPlotly(int_ext_load_plot())
  
  output$SubExtLoad <- renderPlotly(sub_ext_load_plot())
  
  output$WellnessWorkload <- renderPlotly(wellness_workload_plot())
  
  # output$ReadinessWellness <- renderPlotly(readiness_wellness_plot())
  
  output$Wellness <- renderPlotly(wellness_plot())
  
  output$HistoricalWellness <- renderPlotly(historical_wellness_plot())
  
  output$RPE <- renderPlotly(rpe_plot())

  
  output$ZScoreHeatmapTable <- renderReactable({zscore_heatmap_table()})
  
  
  output$WellnessHistoryTable <- renderReactable({wellness_table()})
  
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
              theme = "success")
    
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
              theme = "success")
    
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
              theme = "success")
    
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
              theme = "success")
    
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
              theme = "success")
    
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
              theme = "success")
    
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
              theme = "success")
    
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
              theme = "success")
    
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
              theme = "success")
    
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
              theme = "success")
    
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
              theme = "success")
    
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
              theme = "success")
    
  })
  
  # output$PlannedLoad <- renderUI({
  #   value_box(title=case_when(is_empty(planned_load()) ~"Load is < 0 or > 2 x player's max load",
  #                             input$acwr_param2 == "field_time"~ "Field Time (s)", 
  #                             input$acwr_param2 == "total_distance"~"Total Distance (m)", 
  #                             input$acwr_param2 =="high_speed_distance"~ "High Speed Distance (m)", 
  #                             input$acwr_param2 == "sprint_distance"~ "Sprint Distance (m)",
  #                             input$acwr_param2 == "accel_efforts"~ "Accel Efforts",
  #                             input$acwr_param2 == "decel_efforts" ~"Decel Efforts", 
  #                             input$acwr_param2 == "max_heart_rate"~"Max HR (bpm)", 
  #                             input$acwr_param2 == "dive_count"~"Dive Count", 
  #                             input$acwr_param2 == "total_dive_load"~"Total Dive Load",                                                              
  #                             input$acwr_param2 == "explosive_efforts"~"Explosive Efforts", 
  #                             .default = ""),
  #             value = round(planned_load()), 
  #             showcase = bsicons::bs_icon("activity"),
  #             # case_when(str_detect(input$acwr_param2, "dive")~shiny::icon("person-falling", lib="font-awesome"),
  #             #                    str_detect(input$acwr_param2, "heart")~shiny::icon("heart-pulse", lib="font-awesome"),
  #             #                    str_detect(input$acwr_param2, "time")~shiny::icon("stopwatch", lib="font-awesome"),
  #             #                    .default = shiny::icon("person-running", lib="font-awesome")),
  #             theme = "success")
  #   
  # })

 
  
  # output$download_pdf <- downloadHandler(
  #   filename = function() {
  #     paste0("Tides Match Report ",input$md_input, ".pdf")
  #   },
  #   contentType = "application/pdf", 
  #   content = function(file) {
  #     
  #     req(input$images)
  #     
  #     showModal(modalDialog("Compiling PDF Report...", footer = NULL))
  #     on.exit(removeModal())
  #     
  #     temp_dir <- tempdir()
  #     # tempReport <- file.path(temp_dir, "TidesMatchReport.Rmd")
  #     # file.copy("TidesMatchReport.Rmd", tempReport, overwrite = TRUE)
  #     # tempStyle <- file.path(temp_dir, "style.css")
  #     # file.copy("style.css", tempStyle, overwrite = TRUE)
  #     tempReport <- file.path(temp_dir, 'TidesMatchReportTemplate.qmd')
  #     file.copy(here('QuartoReport', 'TidesMatchReportTemplate.qmd'), tempReport, overwrite = TRUE)
  #     tempStyle <- file.path(temp_dir, 'header.typ')
  #     file.copy(here('QuartoReport', 'header.typ'), tempStyle, overwrite = TRUE)
  #     tempBrand <- file.path(temp_dir, '_brand.yml')
  #     file.copy(here('QuartoReport', '_brand.yml'), tempBrand, overwrite = TRUE)
  #     tempImage1 <- file.path(temp_dir, "Halifax.png")
  #     file.copy("Halifax.png", tempImage1, overwrite = TRUE)
  #     tempImage2 <- file.path(temp_dir, "Montreal.png")
  #     file.copy("Montreal.png", tempImage2, overwrite = TRUE)
  #     tempImage3 <- file.path(temp_dir, "Ottawa.png")
  #     file.copy("Ottawa.png", tempImage3, overwrite = TRUE)
  #     tempImage4 <- file.path(temp_dir, "Toronto.png")
  #     file.copy("Toronto.png", tempImage4, overwrite = TRUE)
  #     tempImage5 <- file.path(temp_dir, "Calgary.png")
  #     file.copy("Calgary.png", tempImage5, overwrite = TRUE)
  #     tempImage6 <- file.path(temp_dir, "Vancouver.png")
  #     file.copy("Vancouver.png", tempImage6, overwrite = TRUE)
  #     tempImage7 <- file.path(temp_dir, "Portsmouth.png")
  #     file.copy("Portsmouth.png", tempImage7, overwrite = TRUE)
  #     tempImage8 <- file.path(temp_dir, "Everton.png")
  #     file.copy("Everton.png", tempImage8, overwrite = TRUE)
  #     tempImage9 <- file.path(temp_dir, "West Ham.png")
  #     file.copy("West Ham.png", tempImage9, overwrite = TRUE)
  #     tempImage10 <- file.path(temp_dir, "AUS.png")
  #     file.copy("AUS.png", tempImage10, overwrite = TRUE)
  #     tempImage11 <- file.path(temp_dir, "vs.png")
  #     file.copy("vs.png", tempImage11, overwrite = TRUE)
  #     # tempImage12 <- file.path(temp_dir, "TidesFCImage2.jpeg")
  #     # file.copy("TidesFCImage2.jpeg", tempImage12, overwrite = TRUE)
  #     # tempImage13 <- file.path(temp_dir, "TidesFCImage3.jpeg")
  #     # file.copy("TidesFCImage3.jpeg", tempImage13, overwrite = TRUE)
  #     # tempImage14 <- file.path(temp_dir, "TidesFCImage4.jpeg")
  #     # file.copy("TidesFCImage4.jpeg", tempImage14, overwrite = TRUE)
  #     tempImage15 <- file.path(temp_dir, "TidesFCImage5.jpg")
  #     file.copy("TidesFCImage5.jpg", tempImage15, overwrite = TRUE)
  #     tempImage16 <- file.path(temp_dir, "TidesFCImage6.jpg")
  #     file.copy("TidesFCImage6.jpg", tempImage16, overwrite = TRUE)
  #     tempImage17 <- file.path(temp_dir, "TidesFCImage7.jpg")
  #     file.copy("TidesFCImage7.jpg", tempImage17, overwrite = TRUE)
  #     tempImage18 <- file.path(temp_dir, input$images$name[1])
  #     file.copy(input$images$datapath[1], tempImage18, overwrite = TRUE)
  #     tempImage19 <- file.path(temp_dir, input$images$name[2])
  #     file.copy(input$images$datapath[2], tempImage19, overwrite = TRUE)
  #     tempImage20 <- file.path(temp_dir, input$images$name[3])
  #     file.copy(input$images$datapath[3], tempImage20, overwrite = TRUE)
  #     tempDataPath <- file.path(temp_dir, "stats_period.rds")
  #     saveRDS(stats_period, file = tempDataPath)
  #     
  # 
  #     # 1. Convert to Base64 strings (Same optimized collection logic)
  #     # b64_strings <- c()
  #     # for (i in 1:nrow(input$images)) {
  #     #   filepath <- input$images$datapath[i]
  #     #   ext <- tools::file_ext(input$images$name[i])
  #     #   mime_type <- ifelse(tolower(ext) == "png", "image/png", "image/jpeg")
  #     #   encoded <- base64enc::base64encode(filepath)
  #     #   b64_strings <- c(b64_strings, paste0("data:", mime_type, ";base64,", encoded))
  #     # }
  #     # 
  #     # # 5. Prevent timeouts: Apply the fixes discussed previously
  #     # old_timeout <- getOption("pagedown.timeout")
  #     # options(pagedown.timeout = 120) 
  #     # on.exit(options(pagedown.timeout = old_timeout), add = TRUE)
  #     # 
  #     
  #     # Set up parameters to pass to Rmd document
  #     report_params <- list(md_input = input$md_input, 
  #                           # image1 = b64_strings[1],
  #                           # image2 = b64_strings[2],
  #                           # image3 = b64_strings[3],
  #                           image1 = input$images$name[1],
  #                           image2 = input$images$name[2],
  #                           image3 = input$images$name[3], 
  #                           data_path = tempDataPath)
  #     
  #     # param_yaml <- file.path(temp_dir, "params.yml")
  #     # yaml::write_yaml(report_params, param_yaml)[1]
  #     # 
  #     # tryCatch({
  #     #   output_filename <- "TidesMatchReportTemplate.pdf"
  #     #   
  #     #   # 3. Use system2 to trigger Quarto directly.
  #     #   # This strips away reactive hooks and stops the dual-execution bug.
  #     #   system2(
  #     #     command = "quarto", 
  #     #     args = c(
  #     #       "render", shQuote(tempReport), 
  #     #       "--to", "typst", 
  #     #       "--output", shQuote(output_filename),
  #     #       "--execute-params", shQuote(param_yaml) # <-- Passes sidecar file
  #     #     ),
  #     #     stdout = TRUE,
  #     #     stderr = TRUE
  #     #   )
  #     #   
  #     #   compiled_pdf <- file.path(temp_dir, output_filename)
  #     #   
  #     #   # 4. Stream verified binary out to browser download folder
  #     #   if (file.exists(compiled_pdf) && file.info(compiled_pdf)$size > 0) {
  #     #     file.copy(compiled_pdf, file, overwrite = TRUE)
  #     #   } else {
  #     #     stop("Quarto completed via system process, but the expected target PDF was missing.")
  #     #   }
  #     #   
  #     # }, error = function(e) {
  #     #   showNotification(paste("Render Failed:", e$message), type = "error")
  #     #   writeLines(paste("Cloud Render Error Log:\n", e$message), file)
  #     # })
  #     
  #     tryCatch({
  # 
  #       output_filename <- "TidesMatchReportTemplate.pdf"
  # 
  #       quarto::quarto_render(
  #         input = tempReport,
  #         execute_params = report_params,
  #         output_format = "typst",
  #         output_file = output_filename
  #       )
  # 
  #       compiled_pdf <- file.path(temp_dir, output_filename)
  #       # 5. Hand the compiled PDF to the browser stream
  #       if (file.exists(compiled_pdf) && file.info(compiled_pdf)$size > 0) {
  #         file.copy(compiled_pdf, file, overwrite = T)
  #       } else {
  #         stop("Quarto completed but no PDF was generated.")
  #       }
  # 
  #     }, error = function(e) {
  #       # Fallback: Save the exact system error text to a downloadable file
  #       # so you can see exactly why the cloud is rejecting your render.
  #       showNotification(paste("Render Failed:", e$message), type = "error")
  #       writeLines(paste("Cloud Render Error Log:\n", e$message), file)
  #     })
  # 
  #     # # 3. Compile the PDF using Quarto and pass the UI inputs into params
  #     # quarto::quarto_render(
  #     #   input = tempReport,
  #     #   execute_params = report_params,
  #     #   output_format = "typst"
  #     # )
  #     # 
  #     # # 4. Quarto saves the output adjacent to the input file as 'report.pdf'
  #     # # Locate that compiled file and copy it to Shiny's final target path
  #     # compiled_pdf <- file.path(temp_dir, "TidesMatchReportTemplate.pdf")
  #     # file.copy(compiled_pdf, file)
  #     
  #     
  #     # # Render to intermediate HTML file
  #     # temp_html <- rmarkdown::render(
  #     #   input = tempReport,
  #     #   params = report_params,
  #     #   envir = new.env(parent = globalenv())
  #     # )
  #     # 
  #     # # Convert intermediate HTML to the final target PDF file path via headless Chrome
  #     # pagedown::chrome_print(
  #     #   input = temp_html,
  #     #   output = file,
  #     #   timeout = 120,
  #     #   outline = F,
  #     #   extra_args = c("--disable-gpu", "--no-sandbox")
  #     # )
  #     
  #   }
  # )
  
  
  
  # Step 1: Compile the PDF securely inside a standard action observer
  observe({
    req(input$images, input$md_input)
    
    # Pop up a loading notification so the user knows it's working
    showModal(modalDialog("Compiling PDF Report... Please wait.", footer = NULL))
    on.exit(removeModal(), add = TRUE)
    
    temp_dir <- tempdir()
    tempReport <- file.path(temp_dir, 'TidesMatchReportTemplate.qmd')
    file.copy(here('QuartoReport', 'TidesMatchReportTemplate.qmd'), tempReport, overwrite = TRUE)
    tempStyle <- file.path(temp_dir, 'header.typ')
    file.copy(here('QuartoReport', 'header.typ'), tempStyle, overwrite = TRUE)
    tempBrand <- file.path(temp_dir, '_brand.yml')
    file.copy(here('QuartoReport', '_brand.yml'), tempBrand, overwrite = TRUE)
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
    tempImage15 <- file.path(temp_dir, "TidesFCImage5.jpg")
    file.copy("TidesFCImage5.jpg", tempImage15, overwrite = TRUE)
    tempImage16 <- file.path(temp_dir, "TidesFCImage6.jpg")
    file.copy("TidesFCImage6.jpg", tempImage16, overwrite = TRUE)
    tempImage17 <- file.path(temp_dir, "TidesFCImage7.jpg")
    file.copy("TidesFCImage7.jpg", tempImage17, overwrite = TRUE)
    tempImage18 <- file.path(temp_dir, input$images$name[1])
    file.copy(input$images$datapath[1], tempImage18, overwrite = TRUE)
    tempImage19 <- file.path(temp_dir, input$images$name[2])
    file.copy(input$images$datapath[2], tempImage19, overwrite = TRUE)
    tempImage20 <- file.path(temp_dir, input$images$name[3])
    file.copy(input$images$datapath[3], tempImage20, overwrite = TRUE)
    tempDataPathStatsPeriod <- file.path(temp_dir, "stats_period.rds")
    saveRDS(stats_period, file = tempDataPathStatsPeriod)
    tempDataPathStats <- file.path(temp_dir, "stats.rds")
    saveRDS(stats, file = tempDataPathStats)
    
    # Set up parameters to pass to Rmd document
    report_params <- list(md_input = input$md_input, 
                          image1 = input$images$name[1],
                          image2 = input$images$name[2],
                          image3 = input$images$name[3], 
                          data_path_stats = tempDataPathStats,
                          data_path_stats_period = tempDataPathStatsPeriod)
    
    tryCatch({
      # Run the verified render command
      quarto::quarto_render(
        input          = tempReport,
        execute_params = report_params,
        output_format  = "typst",
        output_file    = "TidesMatchReportTemplate.pdf"
      )
      
      # Remove the loading screen and show success
      removeModal()
      showNotification("PDF Successfully Generated!", type = "message", duration = 5)
      
      # 🌟 CRITICAL: Render the actual download button now that the file exists!
      output$download_wrapper <- renderUI({
        downloadButton("execute_download", "Download PDF", class = "btn-success")
      })
      
    }, error = function(e) {
      removeModal()
      showModal(modalDialog(title = "Compilation Error", pre(e$message), easyClose = TRUE))
    })
  }) %>% bindEvent(input$build_pdf)
  
  # Step 2: Purely stream the pre-built file to the local Downloads folder
  output$execute_download <- downloadHandler(
    filename = function() {
      paste0("Tides Match Report ", input$md_input, ".pdf")
    },
    contentType = "application/pdf",
    content = function(file) {
      # Point directly to the file that was already created in Step 1
      compiled_pdf <- file.path(tempdir(), "TidesMatchReportTemplate.pdf")
      
      if (file.exists(compiled_pdf)) {
        file.copy(compiled_pdf, file, overwrite = TRUE)
      } else {
        showNotification("File lost in server scratch space. Please compile again.", type = "error")
      }
    }
  )
 
  
  # Step 1: Compile the PDF securely inside a standard action observer
  observe({
    
    req(input$date_input5)
    
    # Pop up a loading notification so the user knows it's working
    showModal(modalDialog("Compiling PDF Report... Please wait.", footer = NULL))
    on.exit(removeModal(), add = TRUE)
    
    temp_dir <- tempdir()
    tempReport <- file.path(temp_dir, 'TidesTrainingReportTemplate.qmd')
    file.copy(here('QuartoReport', 'TidesTrainingReportTemplate.qmd'), tempReport, overwrite = TRUE)
    tempBrand <- file.path(temp_dir, '_brand.yml')
    file.copy(here('QuartoReport', '_brand.yml'), tempBrand, overwrite = TRUE)
    tempImage1 <- file.path(temp_dir, "Halifax.png")
    file.copy("Halifax.png", tempImage1, overwrite = TRUE)
    tempDataPathStatsPeriod <- file.path(temp_dir, "stats_period.rds")
    saveRDS(stats_period, file = tempDataPathStatsPeriod)
    tempDataPathStats <- file.path(temp_dir, "stats.rds")
    saveRDS(stats, file = tempDataPathStats)
    tempDataPathLoadPlan <- file.path(temp_dir, "loading_plan.rds")
    saveRDS(loading_plan, file = tempDataPathLoadPlan)
    
    # Set up parameters to pass to Rmd document
    report_params <- list(training_date = input$date_input5,
                          data_path_stats = tempDataPathStats,
                          data_path_stats_period = tempDataPathStatsPeriod,
                          data_path_load_plan = tempDataPathLoadPlan)
    
    tryCatch({
      # Run the verified render command
      quarto::quarto_render(
        input          = tempReport,
        execute_params = report_params,
        output_format  = "typst",
        output_file    = "TidesTrainingReportTemplate.pdf"
      )
      
      # Remove the loading screen and show success
      removeModal()
      showNotification("PDF Successfully Generated!", type = "message", duration = 5)
      
      # 🌟 CRITICAL: Render the actual download button now that the file exists!
      output$download_wrapper2 <- renderUI({
        downloadButton("execute_download2", "Download PDF", class = "btn-success")
      })
      
    }, error = function(e) {
      removeModal()
      showModal(modalDialog(title = "Compilation Error", pre(e$message), easyClose = TRUE))
    })
  }) %>% bindEvent(input$build_pdf2)
  
  # Step 2: Purely stream the pre-built file to the local Downloads folder
  output$execute_download2 <- downloadHandler(
    filename = function() {
      
      paste0("Tides Training Report ", 
             stats %>% 
               filter(date == input$date_input5) %>% 
               drop_na(activity_name) %>% 
               pull(activity_name) %>% 
               unique, 
             ".pdf")
    },
    contentType = "application/pdf",
    content = function(file) {
      # Point directly to the file that was already created in Step 1
      compiled_pdf <- file.path(tempdir(), "TidesTrainingReportTemplate.pdf")
      
      if (file.exists(compiled_pdf)) {
        file.copy(compiled_pdf, file, overwrite = TRUE)
      } else {
        showNotification("File lost in server scratch space. Please compile again.", type = "error")
      }
    }
  )
  
}

shinyApp(ui, server)





