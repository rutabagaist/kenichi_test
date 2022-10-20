


# UTMMSdb_read                       <- dbConnect(RMariaDB::MariaDB(), dbname = "default", host = "localhost", user = "utmms_read", password = "Z9b2ZvsN6*Nhp4@6", port = 3306)
UTMMSdb_read                       <- dbConnect(RMariaDB::MariaDB(), dbname = "default", host = "192.168.56.101", user = "utmms_read", password = "UTMMSDataRead", port = 3306)


# Get 31 days of half hourly data, display most recent 7 by default
HalfHourlyNumDays                   <- 31
HalfHourlyDisplayDaysDefault        <- 7
HalfHourlyNumRecords                <- HalfHourlyNumDays * 48   # 7 Days worth of half hourlies
DailyNumRecords                     <- 365      # THe past year worth of dailies
LiveNumRecords                      <- 24 * 60  # The past day worth of minute records

# The DB code inlcudes a stupid hack to rectify the stored timestamps. In the DB all time stamps are stored as-is from the weather station. This means
# they are in the "EST" time zone. Always, daylight savings is not observed by the weather station. All observation times are stored in EST. 
# The RMariaDB connector assumes that storage is in UTC format, so to fix this an two step conversion is done to the stored dates to force them into EST 
# within R. An update to the driver might fix this (by allowing specification of the time-zone), but for now the hack will have to do.

HalfHourlyDisplay_UTMMSdb           <- as_tibble(
                                                dbGetQuery(UTMMSdb_read, paste0("SELECT * FROM `default`.halfhourlyUTMMSData ORDER BY TIMESTAMP DESC LIMIT ", HalfHourlyNumRecords)) %>%
                                                modify_if(., is.POSIXt, with_tz, "UTC") %>% 
                                                modify_if(., is.POSIXt, force_tz, "EST")
                                                )     # This is a fix/hack documented in the header.

DailyDisplay_UTMMSdb                <- as_tibble(
                                                dbGetQuery(UTMMSdb_read, paste0("SELECT * FROM `default`.dailyUTMMSData ORDER BY TIMESTAMP DESC LIMIT ", DailyNumRecords)) %>%
                                                modify_if(., is.POSIXt, with_tz, "UTC") %>% 
                                                modify_if(., is.POSIXt, force_tz, "EST")
                                                )     # This is a fix/hack documented in the header.

LiveDisplay_UTMMSdb                <- as_tibble(
                                                dbGetQuery(UTMMSdb_read, paste0("SELECT * FROM `default`.liveUTMMSData ORDER BY TIMESTAMP DESC LIMIT ", LiveNumRecords)) %>%
                                                modify_if(., is.POSIXt, with_tz, "UTC") %>% 
                                                modify_if(., is.POSIXt, force_tz, "EST")
                                                )     # This is a fix/hack documented in the header.

# Disconnect from the database
dbDisconnect(UTMMSdb_read)

# This operation groups by day, creates a mean daily albedo value for each day

HalfHourlyDisplay_UTMMSdb %>%
      group_by(floor_date(TIMESTAMP, unit = 'day')) %>% 
      summarise(avg = round(mean(Albedo_Avg, na.rm = T), 3), 
                sd = round(sd(Albedo_Avg, na.rm = T), 3))         -> Daily_AlbedoDisplay

# Add column names
colnames(Daily_AlbedoDisplay)        <- c("date", "avg", "sd")

# Add 12 hours to each daily value to centre it behind the radiation data.
Daily_AlbedoDisplay$date             <- Daily_AlbedoDisplay$date + hours (12)

# Create the cumulative precipitation plot

PrecipCumulative <- tibble(
      TimeSince = c(
            "Past 30 minutes",
            "Past hour",
            "Past 6 hours",
            "Past 12 hours",
            "Past 24 hours",
            "Past 2 days",
            "Past 7 days",
            "Past 30 days"),
      mm_Precip = c(
            head(HalfHourlyDisplay_UTMMSdb$Precip_mm_Tot,1),
            sum(head(HalfHourlyDisplay_UTMMSdb$Precip_mm_Tot,2)),
            sum(head(HalfHourlyDisplay_UTMMSdb$Precip_mm_Tot,12)),
            sum(head(HalfHourlyDisplay_UTMMSdb$Precip_mm_Tot,24)),
            sum(head(HalfHourlyDisplay_UTMMSdb$Precip_mm_Tot,48)),
            sum(head(HalfHourlyDisplay_UTMMSdb$Precip_mm_Tot,96)),
            sum(head(HalfHourlyDisplay_UTMMSdb$Precip_mm_Tot,384)),
            sum(head(HalfHourlyDisplay_UTMMSdb$Precip_mm_Tot,1440))),
      Colour = c('rgba(196, 213, 223, 1)',
                 'rgba(170, 202, 223, 1)',
                 'rgba(155, 197, 223, 1)',
                 'rgba(140, 191, 223, 1)',
                 'rgba(118, 182, 223, 1)',
                 'rgba(105, 177, 223, 1)',
                 'rgba(94, 173, 223, 1)',
                 'rgba(51,  156, 223, 1)')
      )

