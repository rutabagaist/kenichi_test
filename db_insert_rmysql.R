library(DBI)
library(RMySQL)
library(tidyverse)
library(lubridate)


filesRoot <- "C:\\Users\\rudzphil\\OneDrive\\OneDrive - University of Toronto"

# Set up headers
MainHeader                    <- c("TIMESTAMP","RECORD","Batt_volt_Min","PTemp","AirTemp_Avg","RH_Avg","DewPt_Avg","Wind_Speed_m_s","Wind_Direction_deg","Wind_Direction_STD","WindGust_at_15m_Max","WindSpeed_at_2m_Avg","WindSpeed_at_2m_Max","WindSpeed_at_10m_Avg","WindSpeed_at_10m_Max","Stn_Baro_kpa_Avg","MSL_Baro_kpa_Avg","Precip_mm_Tot","RainRate_hrmax_Max","RainRate_hrmax_TMx","Snow_Depth_m_Avg","SR50A_SignalQuality_Avg","SnowFall_past6h","SnowFall_past24h","CO2_conc_Avg","CO2_conc_Std","SR01up_Avg","SR01down_Avg","IR01up_Avg","IR01down_Avg","NR01TC_Avg","NR01TK_Avg","NetRS_Avg","NetRl_Avg","Albedo_Avg","UpTot_Avg","DnTot_Avg","NetTot_Avg","IR01upCo_Avg","IR01downCo_Avg","VWC_20cm_Avg","VWC_40cm_Avg","VWC_60cm_Avg","VWC_80cm_Avg","VWC_100cm_Avg","BulkEC_20cm_Avg","BulkEC_40cm_Avg","BulkEC_60cm_Avg","BulkEC_80cm_Avg","BulkEC_100cm_Avg","SoilTemp_20cm_Avg","SoilTemp_40cm_Avg","SoilTemp_60cm_Avg","SoilTemp_80cm_Avg","SoilTemp_100cm_Avg","SwMatricPot_Tension_Avg","SwMatricPot_Temp_Avg")
DailyHeader                   <- c("TIMESTAMP","RECORD","Batt_volt_Min","PTemp","AirTemp_Avg","AirTemp_Std","AirTemp_Max","AirTemp_TMx","AirTemp_Min","AirTemp_TMn","RH_Avg","RH_Std","RH_Max","RH_TMx","RH_Min","RH_TMn","DewPt_Avg","DewPt_Std","DewPt_Max","DewPt_TMx","DewPt_Min","DewPt_TMn","Wind_Speed_m_s","Wind_Direction_deg","Wind_Direction_STD","WindGust_at_15m_Max","WindGust_at_15m_TMx","WindSpeed_at_15m_Std","WindSpeed_at_2m_Avg","WindSpeed_at_2m_Std","WindGust_at_2m_Max","WindGust_at_2m_TMx","WindSpeed_at_10m_Avg","WindSpeed_at_10m_Std","WindGust_at_10m_Max","WindGust_at_10m_TMx","Stn_Baro_kpa_Avg","Stn_Baro_kpa_Std","Stn_Baro_kpa_Max","Stn_Baro_kpa_TMx","Stn_Baro_kpa_Min","Stn_Baro_kpa_TMn","MSL_Baro_kpa_Avg","MSL_Baro_kpa_Std","MSL_Baro_kpa_Max","MSL_Baro_kpa_TMx","MSL_Baro_kpa_Min","MSL_Baro_kpa_TMn","Precip_mm_Tot","RainRate_daymax_Max","RainRate_daymax_TMx","Snow_Depth_m_Avg","SR50A_SignalQuality_Avg","SnowFall_past24h","CO2_conc_Avg","CO2_conc_Max","CO2_conc_TMx","CO2_conc_Min","CO2_conc_TMn","CO2_conc_Std","SR01up_Avg","SR01up_Max","SR01up_TMx","SR01down_Avg","SR01down_Max","SR01down_TMx","IR01up_Avg","IR01up_Min","IR01up_TMn","IR01down_Avg","IR01down_Max","IR01down_TMx","IR01down_Min","IR01down_TMn","NetRS_Avg","NetRS_Max","NetRS_TMx","NetRl_Avg","NetRl_Max","NetRl_TMx","NetRl_Min","NetRl_TMn","Albedo_Avg","UpTot_Avg","UpTot_Max","UpTot_TMx","DnTot_Avg","DnTot_Max","DnTot_TMx","NetTot_Avg","NetTot_Max","NetTot_TMx","NetTot_Min","NetTot_TMn","VWC_20cm_Avg","VWC_40cm_Avg","VWC_60cm_Avg","VWC_80cm_Avg","VWC_100cm_Avg","BulkEC_20cm_Avg","BulkEC_40cm_Avg","BulkEC_60cm_Avg","BulkEC_80cm_Avg","BulkEC_100cm_Avg","SoilTemp_20cm_Avg","SoilTemp_40cm_Avg","SoilTemp_60cm_Avg","SoilTemp_80cm_Avg","SoilTemp_100cm_Avg","SwMatricPot_Tension_Avg","SwMatricPot_Tension_Max","SwMatricPot_Tension_TMx","SwMatricPot_Tension_Min","SwMatricPot_Tension_TMn","SwMatricPot_Temp_Avg")
LiveHeader                    <- c("TIMESTAMP","RECORD","AirTemp_Avg","RH_Avg","DewPt_Avg","Wind_Speed_m_s","Wind_Direction_deg","Wind_Direction_STD","WindGust_at_15m_Max","WindSpeed_at_2m_Avg","WindSpeed_at_2m_Max","WindSpeed_at_10m_Avg","WindSpeed_at_10m_Max","Stn_Baro_kpa_Avg","MSL_Baro_kpa_Avg","Precip_Rate_mm_hr","Precip_Rate_mm_m","Snow_Depth_m_Avg","SR50A_SignalQuality_Avg","CO2_conc_Avg","CO2_conc_Std","SR01up_Avg","SR01down_Avg","IR01up_Avg","IR01down_Avg","NR01TC_Avg","NR01TK_Avg","NetRS_Avg","NetRl_Avg","Albedo_Avg","UpTot_Avg","DnTot_Avg","NetTot_Avg","IR01upCo_Avg","IR01downCo_Avg","VWC_20cm_Avg","VWC_40cm_Avg","VWC_60cm_Avg","VWC_80cm_Avg","VWC_100cm_Avg","BulkEC_20cm_Avg","BulkEC_40cm_Avg","BulkEC_60cm_Avg","BulkEC_80cm_Avg","BulkEC_100cm_Avg","SoilTemp_20cm_Avg","SoilTemp_40cm_Avg","SoilTemp_60cm_Avg","SoilTemp_80cm_Avg","SoilTemp_100cm_Avg","SwMatricPot_Tension_Avg","SwMatricPot_Temp_Avg")

# Specify the data interval used for the data tables.
UTMMSData_Interval            <- dminutes(30)
UTMMSLiveData_Interval        <- dminutes(1)


UTMMSData                       <- read_csv(paste0(filesRoot, "\\_geo_lab\\UTMMS\\__incoming\\CR1000XSeries_Main.dat"),  
                                          col_names = MainHeader, 
                                          skip = 4,
                                          guess_max = 10)


DailyData                     <- read_csv(paste0(filesRoot, "\\_geo_lab\\UTMMS\\__incoming\\CR1000XSeries_DailyMain.dat"),
                                          col_names = DailyHeader,
                                          skip = 4,
                                          guess_max = 10)

LiveData                     <- read_csv(paste0(filesRoot, "\\_geo_lab\\UTMMS\\__incoming\\CR1000XSeries_Live.dat"),
                                         col_names = LiveHeader,
                                         skip = 4,
                                         guess_max = 10) %>% 
                                                dplyr::filter(TIMESTAMP >= (max(TIMESTAMP) - days(1)))



# Connect to the database
UTMMSdb                       <- dbConnect(RMySQL::MySQL(), dbname = "default", host = "localhost", user = "utmms", password = "J6$4cLTwx@#nzHXs", port = 3306)


# Grab the timestamp of the last record in the DB for comparison, convert to POSIX, coerce the DB timestamp to Eastern Standard Time
LatestHalfHourlyDBRecord      <- as.POSIXct(dbGetQuery(UTMMSdb, "SELECT TIMESTAMP FROM `default`.halfhourlyUTMMSData ORDER BY TIMESTAMP DESC LIMIT 1;")[1,], tz = "UTC")

LatestDailyDBRecord           <- as.POSIXct(dbGetQuery(UTMMSdb, "SELECT TIMESTAMP FROM `default`.dailyUTMMSData ORDER BY TIMESTAMP DESC LIMIT 1;")[1,], tz = "UTC")


LatestLiveDBRecord            <- as.POSIXct(dbGetQuery(UTMMSdb, "SELECT TIMESTAMP FROM `default`.liveUTMMSData ORDER BY TIMESTAMP DESC LIMIT 1;")[1,], tz = "UTC")



# Check for time difference between imported data and the DB records, coerce the timestamp in Eastern Standard Time; note that imported data in CSV is already forced to be EST
UTMMSHalfHourlyData_Disparity <- (tail(UTMMSData$TIMESTAMP, 1) -  LatestHalfHourlyDBRecord) / UTMMSData_Interval
UTMMSDailyData_Disparity      <- (tail(DailyData$TIMESTAMP, 1) -  LatestDailyDBRecord) / ddays(1)

# If there are records to be updated, ie. disparity exceeds 0 records, create the datatable and insert it into the DB.

dbWriteTable(UTMMSdb, "liveUTMMSData", LiveData, append = TRUE, row.names = FALSE)

if(UTMMSHalfHourlyData_Disparity > 0)     {
      UTMMSData_UpdateRows    <- tail(UTMMSData, UTMMSHalfHourlyData_Disparity)
      dbWriteTable(UTMMSdb, "halfhourlyUTMMSData", UTMMSData_UpdateRows, append = TRUE, row.names = FALSE)
}

if(UTMMSDailyData_Disparity > 0)    {
      DailyData_UpdateRows    <- tail(DailyData, UTMMSDailyData_Disparity)
      dbWriteTable(UTMMSdb, "dailyUTMMSData", DailyData_UpdateRows, append = TRUE, row.names = FALSE)
}

dbDisconnect(UTMMSdb)
