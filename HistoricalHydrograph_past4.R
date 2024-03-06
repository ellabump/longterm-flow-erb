### R code from Abby's longterm_flow.Rmd
### Adding previous four water years to the historical hydrograph
### This won't work without running the longterm_flow first

Daily$Year <- trunc(Daily$DecYear)
cfs_historical <- Daily %>% select(Day, cfs, Year)
cfs_historical <- arrange(cfs_historical, Day)
cfs_historical <- pivot_wider(cfs_historical, names_from = Day, values_from = cfs)
quants <- c(0.25,0.50, 0.75)
Percentiles <- apply(cfs_historical[2:dim(cfs_historical)[2]], 2, quantile, probs = quants, na.rm = TRUE)
Mean <- apply(cfs_historical[2:dim(cfs_historical)[2]], 2, mean, na.rm = TRUE)
historical_stats <- t(rbind(Percentiles, Mean))
historical_stats <- as.data.frame(historical_stats)
historical_stats$Day <- c(1:366)

current_yr <- format(Sys.Date(), "%Y")
current_yr_start <- paste0(current_yr, "-01-01")
current_yr_Daily <- readNWISDaily(siteID,"00060", current_yr_start,"")   # For PECO, ends 01/11/2024 due to icing over - does this happen every season?
current_yr_Daily$cfs_2023 <- current_yr_Daily$Q*35.314666212661  # remove "_2023"?
current_yr_Daily <- current_yr_Daily %>% select(Day, cfs_2023)
historical_stats <- merge(historical_stats, current_yr_Daily, by = 'Day', all.x= TRUE)

## Previous Water Years ##
wy_23_start <- "2022-10-01"
wy_23_end <- "2023-09-30"
wy_23_Daily <- readNWISDaily(siteID,"00060", wy_23_start, wy_23_end)
wy_23_Daily$cfs_WY23 <- wy_23_Daily$Q*35.314666212661  # remove "_2023"?
wy_23_Daily <- wy_23_Daily %>% select(Day, cfs_WY23)
historical_stats <- merge(historical_stats, wy_23_Daily, by = 'Day', all.x= TRUE)

wy_22_start <- "2021-10-01"
wy_22_end <- "2022-09-30"
wy_22_Daily <- readNWISDaily(siteID,"00060", wy_22_start, wy_22_end)
wy_22_Daily$cfs_WY22 <- wy_22_Daily$Q*35.314666212661  # remove "_2023"?
wy_22_Daily <- wy_22_Daily %>% select(Day, cfs_WY22)
historical_stats <- merge(historical_stats, wy_22_Daily, by = 'Day', all.x= TRUE)

wy_21_start <- "2020-10-01"
wy_21_end <- "2021-09-30"
wy_21_Daily <- readNWISDaily(siteID,"00060", wy_21_start, wy_21_end)
wy_21_Daily$cfs_WY21 <- wy_21_Daily$Q*35.314666212661  # remove "_2023"?
wy_21_Daily <- wy_21_Daily %>% select(Day, cfs_WY21)
historical_stats <- merge(historical_stats, wy_21_Daily, by = 'Day', all.x= TRUE)


historical_stats %>% ggplot(aes(x=Day)) +
  geom_ribbon(aes(ymin=`25%`, ymax=`75%`), fill = "#6BD7AF", alpha = 0.5) +
  geom_line(aes(y=Mean, color="Mean", lty="Mean"), lwd=1) +
  geom_line(aes(y=cfs_WY23, color="WY 2023" , lty="WY 2023"), lwd=1) +
  geom_line(aes(y=`25%`, color="25th Percentile", lty="25th Percentile"), lwd=1) +
  geom_line(aes(y=`75%`, color="75th Percentile", lty="75th Percentile"), lwd=1) +
  theme_bw() + 
  labs(x="Day", y="Discharge (cfs)", title=paste0("Historical vs. Current Year (", current_yr, ") Hydrographs \n for ", siteName)) +
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        legend.title = element_text(size=14),
        legend.text = element_text(size=10),
        legend.title.align=0.5) +
  scale_color_manual(name = "Legend", 
                     values = c("Mean" = "black", "WY 2021" = "orange", "WY 2023" = "blue", "WY 2022" = "purple", "WY 2021" = "green", "WY 2020" = "yellow", "WY 2019" = "red", "25th Percentile" = "gray", "75th Percentile" = "gray60")) +
  scale_linetype_manual(name = "Legend",
                        values = c("Mean" = 6, "WY 2023" = 1, "WY 2022" = 1, "WY 2021" = 1, "25th Percentile" = 2, "75th Percentile" = 2)) +
        # Add another water year
        geom_line(aes(y=cfs_WY22, color="WY 2022" , lty="WY 2022"), lwd=1) +
        geom_line(aes(y=cfs_WY21, color="WY 2021" , lty="WY 2021"), lwd=1) +   
        geom_line(aes(y=cfs_WY20, color="WY 2020" , lty="WY 2022"), lwd=1) +
        geom_line(aes(y=cfs_WY19, color="WY 2019" , lty="WY 2021"), lwd=1)  


# Make a loop 
# For select time span, change the iteration?
# This loop works and prints out the cfs of every water year, however definitely needs to be checked to make sure everything joined correctly
# Next is to loop these in a plot
current_yr <- format(Sys.Date(), "%Y")
for (i in 1:10) {
  yr <- as.numeric(current_yr) - i
  start <- paste0(yr-1, "-10-01")
  end <- paste0(yr, "-09-30")
  flow_dat <- readNWISDaily(siteID,"00060", start, end)
  flow_dat$cfs <- flow_dat$Q*35.314666212661
  flow_dat <- flow_dat %>% select(Day, cfs)
  historical_stats <- merge(historical_stats, flow_dat, by = 'Day', all.x= TRUE)
  names(historical_stats)[names(historical_stats) == 'cfs'] <- paste0("cfs_WY", substr(yr, 3,4))
}


# Repeat of code earlier in this doc
current_yr_start <- paste0(current_yr, "-01-01")
current_yr_Daily <- readNWISDaily(siteID,"00060", current_yr_start,"")   # For PECO, ends 01/11/2024 due to icing over - does this happen every season?
current_yr_Daily$cfs_2023 <- current_yr_Daily$Q*35.314666212661  # remove "_2023"?
current_yr_Daily <- current_yr_Daily %>% select(Day, cfs_2023)
historical_stats <- merge(historical_stats, current_yr_Daily, by = 'Day', all.x= TRUE)









