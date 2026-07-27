library(tidyverse)
library(httr2)


# Get athlete info XPS
call_xps <- "https://www4.sidelinesports.com"


athletes_xps <- request(call_xps) %>%
  req_url_path(path = "xpsweb/xpsapi/listathletes") %>%
  req_headers(access_key = Sys.getenv("xps_token")) %>%
  req_body_json(list(withGroupAccess = F)) %>%
  req_perform() %>%
  resp_body_json(flatten = T, simplifyDataFrame=T) %>%
  pluck("data")

# # Get historical data
# 
# googledrive::drive_auth(path=gargle::secret_decrypt_json(here::here(".secrets", "halifaxtidesdashboard-serviceaccount-encrypted.json"), "googledrive_token"))
# 
# googledrive::drive_download(googledrive::as_id(Sys.getenv("rpe_file_id")), path="XPS RPE.csv", overwrite=T)
# 
# xps_rpe_db <- read_csv("XPS RPE.csv", show_col_types =F) 


# Read exported data XPS (tab delimited)

xps_rpe_export <- read_tsv("Tides RPE 2026-01-01 to 2026-07-20.csv", show_col_types =F) 
 

xps_rpe <- xps_rpe_export %>% 
  mutate(Time = mdy_hms(Time),
         date=as.Date(Time)) %>% 
  rename(athlete_name=Athlete,localTime=Time,name=Template, rpe=R.P.E,minutes=Duration) %>%  
  left_join(athletes_xps %>% select(id, name) %>% rename(athleteId=id, athlete_name=name), by = join_by(athlete_name)) %>% 
  select(athleteId, athlete_name, localTime, date, name, rpe, minutes)


googledrive::drive_auth(path=gargle::secret_decrypt_json(here::here(".secrets", "halifaxtidesdashboard-serviceaccount-encrypted.json"), "googledrive_token"))
# xps_rpe <- rbind(xps_rpe_db, xps_rpe) #if don't export all historical data and want to append new export to database
write_csv(xps_rpe,file="XPS RPE.csv")
googledrive::drive_put("XPS RPE.csv", path=googledrive::as_id(Sys.getenv("drive_folder_id")), name = "XPS RPE.csv")
