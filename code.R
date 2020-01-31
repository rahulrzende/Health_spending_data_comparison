# load required libraries
library(data.table)
library(ggplot2)
library(openxlsx)
library(gridExtra)
library(cowplot)
library(scales)
setwd("~")

# create a test data table for later usage (if needed)
test <- data.table(x = seq(0, 10000))
test$upper <- test$x * 1.1
test$lower <- test$x * 0.9

########################################################################################################################################################################################################################################################################
########################################################################################################################################################################################################################################################################

# ready WHO data
thepc_data <- data.table(read.xlsx("/ihme/homes/rahulrz6/THE_data_USD2017_all_years.xlsx"))[, Indicators := NULL]
thepc_data <- thepc_data[, X3 := NULL]
thepc_data <- thepc_data[!is.na(Countries)]
thepc_data_long <- melt(thepc_data, id.vars = c("Countries"), measure.vars = c("2000","2001","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016","2017"), variable.name = "year", value.name = "value")
thepc_data_long$value <- as.numeric(thepc_data_long$value)
colnames(thepc_data_long) <- c("location_name","year","value")

# ready IHME data
source("/ihme/cc_resources/libraries/current/r/get_location_metadata.R")
iso3_dat <- get_location_metadata(location_set_id=22, gbd_round_id = 6)[level == 3]
iso3_dat[, location_name:= iconv(location_name)]
iso3_dat[grepl("Bahamas", location_name), location_name := "Bahamas"]
iso3_dat[grepl("Bolivia", location_name), location_name := "Bolivia Plurinational States of"]
iso3_dat[grepl("Brunei", location_name), location_name := "Brunei Darussalam"]
iso3_dat[grepl("Verde", location_name), location_name := "Cabo Verde Republic of"]
iso3_dat[grepl("d'Ivoire", location_name), location_name := "Cote d'Ivoire"]
iso3_dat[grepl("Gambia", location_name), location_name := "Gambia"]
iso3_dat[grepl("Lao", location_name), location_name := "Lao People's Democratic Republic"]
iso3_dat[grepl("Micronesia", location_name), location_name := "Micronesia (Federated States of)"]
iso3_dat[grepl("South Korea", location_name), location_name := "Republic of Korea"]
iso3_dat[grepl("Moldova", location_name), location_name := "Republic of Moldova"]
iso3_dat[grepl("Russia", location_name), location_name := "Russian Federation"]
iso3_dat[grepl("Macedonia", location_name), location_name := "The former Yugoslav Republic of Macedonia"]
iso3_dat[grepl("Tanzania", location_name), location_name := "United Republic of Tanzania"]
iso3_dat[grepl("United States", location_name), location_name := "United States of America"]
iso3_dat[grepl("Venezuela", location_name), location_name := "Venezuela (Bolivarian Republic of)"]
iso3_dat[grepl("Vietnam", location_name), location_name := "Viet Nam"]
iso3_dat[, location_name:= iconv(location_name)]

# merge the WHO and IHME data tables to get ISO3 values for each country
thepc_raw <- merge(thepc_data_long, iso3_dat[, .(location_name, ihme_loc_id, super_region_name)], by=c("location_name"), all.x = T)

# fix remaining issues with locations for efficient plotting
thepc_raw[location_name == "Bolivia Plurinational States of ", ihme_loc_id:= "BOL"]
thepc_raw[location_name == "Czech Republic", ihme_loc_id:= "CZE"]
thepc_raw[location_name == "Iran", ihme_loc_id:= "IRN"]
thepc_raw[location_name == "The Republic of North Macedonia", ihme_loc_id:= "MKD"]
thepc_raw[is.na(ihme_loc_id), ihme_loc_id:= "CIV"]
thepc_raw[ihme_loc_id %in% c("CZE", "MKD"), super_region_name := "Central Europe, Eastern Europe, and Central Asia"]
thepc_raw[ihme_loc_id == "IRN", super_region_name := "North Africa and Middle East"]
thepc_raw[ihme_loc_id == "CIV", super_region_name := "Sub-Saharan Africa"]
thepc_raw[ihme_loc_id == "BOL", super_region_name := "Latin America and Caribbean"]

# fix column names for convenience later on
colnames(thepc_raw) <- c("location_name", "year", "who_value", "iso3", "super_region")



# get the IHME fits
thepc_ihme <- fread("/home/j/Project/IRH/HE_Imputation/data/output/draws_stats_sorted/thepc_stats.csv")[, .(iso3, year, mean)]
colnames(thepc_ihme) <- c("iso3","year","ihme_value")
thepc_ihme$year <- as.factor(thepc_ihme$year)


# currency convert USD 2019 estimates to USD 2017
source("/ihme/homes/rahulrz6/fgh_resubmission_develop/fgh/FGH_2019/04_functions/currency_conversion.R")
thepc_ihme_converted <- currency_conversion(thepc_ihme, col.loc = "iso3", col.value = "ihme_value", currency = "usd", currency.year = 2019, base.year = 2017, base.unit = "usd")


# merge the WHO estimates with IHME estimates for plotting
thepc_merged <- merge(thepc_raw, thepc_ihme_converted, by=c("iso3","year"), all.x = T)

# plot panel 1
plot_1_a <- ggplot() + 
  geom_point(data = thepc_merged[year == 2017 & iso3 != "VIR"], aes(x = who_value, y = ihme_value, color = super_region), size = 1.5, show.legend = F) + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", size = 0.3, color = "black", alpha = 0.3) + 
  #geom_line(data = test, aes(x = x, y = upper), linetype = "dashed", size = 0.3, color = "red", alpha = 0.3) + 
  #geom_line(data = test, aes(x = x, y = lower), linetype = "dashed", size = 0.3, color = "red", alpha = 0.3) + 
  geom_text(data = thepc_merged[year == 2017 & iso3 != "VIR"], aes(y = ihme_value, x = who_value, label = ifelse(abs((who_value - ihme_value)/who_value)*100 > 10, as.character(iso3), '')), size = 1.5, hjust = -0.25, vjust = -0.25) +
  ggtitle(label = "THE per capita in 2017") + ylab("IHME estimate (USD 2017)") + xlab("WHO estimate (USD 2017)") + theme_bw()

plot_1_b <- ggplot(data = thepc_merged[year == 2017 & iso3 != "VIR"], aes(x = who_value, y = ihme_value)) + 
  geom_point(data = thepc_merged[year == 2017 & iso3 != "VIR"], aes(x = who_value, y = ihme_value, color = super_region), size = 1.5) + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", size = 0.3, color = "black", alpha = 0.3) +
  #geom_line(data = test, aes(x = x, y = upper), linetype = "dashed", size = 0.3, color = "red", alpha = 0.3) + 
  #geom_line(data = test, aes(x = x, y = lower), linetype = "dashed", size = 0.3, color = "red", alpha = 0.3) + 
  geom_text(data = thepc_merged[year == 2017 & iso3 != "VIR"], aes(y = ihme_value, x = who_value, label = ifelse(abs((who_value - ihme_value)/who_value)*100 > 10, as.character(iso3), '')), size = 1.5, hjust = -0.25, vjust = -0.25) +
  ggtitle(label = "THE per capita in 2017") + ylab("IHME estimate (USD 2017)") + xlab("WHO estimate (USD 2017)") + theme_bw() + 
  theme(legend.position = c(0.765,0.18), legend.text = element_text(size = 7), legend.title = element_text(size = 7), legend.background = element_blank()) + 
  scale_y_log10() + scale_x_log10() 

# write panels to disk
pdf("slide_1.pdf", onefile = TRUE, width = 13, height = 7)
grid.arrange(plot_1_a, plot_1_b, nrow=1, top=paste("THE per capita in 2017, using normal and log axes", sep = ''))
dev.off()

########################################################################################################################################################################################################################################################################
########################################################################################################################################################################################################################################################################

# create subsetted data tables for convenience while plotting
thepc_merged_2000 <- thepc_merged[year == 2000 & iso3 != "VIR"]
thepc_merged_2006 <- thepc_merged[year == 2006 & iso3 != "VIR"]
thepc_merged_2012 <- thepc_merged[year == 2012 & iso3 != "VIR"]
thepc_merged_2017 <- thepc_merged[year == 2017 & iso3 != "VIR"]

# plot panel 2
plot_2_a <- ggplot() + 
  geom_point(data = thepc_merged_2000, aes(x = (who_value), y = (ihme_value), color = super_region), size = 1.5) + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", size = 0.3, color = "black", alpha = 0.3) + 
  #geom_line(data = test, aes(x = x, y = upper), linetype = "dashed", size = 0.3, color = "red", alpha = 0.3) + 
  #geom_line(data = test, aes(x = x, y = lower), linetype = "dashed", size = 0.3, color = "red", alpha = 0.3) + 
  geom_text(data = thepc_merged_2000, aes(y = (ihme_value), x = (who_value), label = ifelse(abs(((who_value) - (ihme_value))/(who_value))*100 > 10, as.character(iso3), '')), size = 1.5, hjust = -0.25, vjust = -0.25) +
  ggtitle(label = "2000") + ylab("IHME estimate (USD 2017)") + xlab("WHO estimate (USD 2017)") + theme_bw() + 
  theme(legend.position = c(0.16,0.65), legend.text = element_text(size = 5), legend.title = element_text(size = 5), legend.background = element_blank()) + #expand_limits(y = 0, x = 0) +
  scale_y_log10() + scale_x_log10()

plot_2_b <- ggplot() + 
  geom_point(data = thepc_merged_2006, aes(x = (who_value), y = (ihme_value), color = super_region), size = 1.5, show.legend = F) + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", size = 0.3, color = "black", alpha = 0.3) + 
  #geom_line(data = test, aes(x = x, y = upper), linetype = "dashed", size = 0.3, color = "red", alpha = 0.3) + 
  #geom_line(data = test, aes(x = x, y = lower), linetype = "dashed", size = 0.3, color = "red", alpha = 0.3) + 
  geom_text(data = thepc_merged_2006, aes(y = (ihme_value), x = (who_value), label = ifelse(abs(((who_value) - (ihme_value))/(who_value))*100 > 10, as.character(iso3), '')), size = 1.5, hjust = -0.25, vjust = -0.25) +
  ggtitle(label = "2006") + ylab("IHME estimate (USD 2017)") + xlab("WHO estimate (USD 2017)") + theme_bw() + 
  theme(legend.position = c(0.775,0.2), legend.text = element_text(size = 5), legend.title = element_text(size = 5)) + expand_limits(y = 0, x = 0) + 
  scale_y_log10() + scale_x_log10()

plot_2_c <- ggplot() + 
  geom_point(data = thepc_merged_2012, aes(x = (who_value), y = (ihme_value), color = super_region), size = 1.5, show.legend = F) + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", size = 0.3, color = "black", alpha = 0.3) + 
  #geom_line(data = test, aes(x = x, y = upper), linetype = "dashed", size = 0.3, color = "red", alpha = 0.3) + 
  #geom_line(data = test, aes(x = x, y = lower), linetype = "dashed", size = 0.3, color = "red", alpha = 0.3) + 
  geom_text(data = thepc_merged_2012, aes(y = (ihme_value), x = (who_value), label = ifelse(abs(((who_value) - (ihme_value))/(who_value))*100 > 10, as.character(iso3), '')), size = 1.5, hjust = -0.25, vjust = -0.25) +
  ggtitle(label = "2012") + ylab("IHME estimate (USD 2017)") + xlab("WHO estimate (USD 2017)") + theme_bw() + 
  theme(legend.position = c(0.775,0.2), legend.text = element_text(size = 7), legend.title = element_text(size = 7)) + expand_limits(y = 0, x = 0) + 
  scale_y_log10() + scale_x_log10()

plot_2_d <- ggplot() + 
  geom_point(data = thepc_merged_2017, aes(x = (who_value), y = (ihme_value), color = super_region), size = 1.5, show.legend = F) + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", size = 0.3, color = "black", alpha = 0.3) + 
  #geom_line(data = test, aes(x = x, y = upper), linetype = "dashed", size = 0.3, color = "red", alpha = 0.3) + 
  #geom_line(data = test, aes(x = x, y = lower), linetype = "dashed", size = 0.3, color = "red", alpha = 0.3) + 
  geom_text(data = thepc_merged_2017, aes(y = (ihme_value), x = (who_value), label = ifelse(abs(((who_value) - (ihme_value))/(who_value))*100 > 10, as.character(iso3), '')), size = 1.5, hjust = -0.25, vjust = -0.25) +
  ggtitle(label = "2017") + ylab("IHME estimate (USD 2017)") + xlab("WHO estimate (USD 2017)") + theme_bw() + 
  theme(legend.position = c(0.775,0.2), legend.text = element_text(size = 7), legend.title = element_text(size = 7)) + expand_limits(y = 0, x = 0) + 
  scale_y_log10() + scale_x_log10()

# write panels to disk
pdf("slide_2.pdf", onefile = TRUE, width = 13, height = 7)
grid.arrange(plot_2_a, plot_2_b, plot_2_c, plot_2_d, nrow=2, ncol=2, top=paste("THE per capita", sep = ''))
dev.off()

########################################################################################################################################################################################################################################################################
########################################################################################################################################################################################################################################################################

# create subsetted data tables for convenience when plotting
thepc_merged_1 <- thepc_merged[super_region == "Central Europe, Eastern Europe, and Central Asia" & iso3 != "VIR"]
thepc_merged_2 <- thepc_merged[super_region == "High-income" & iso3 != "VIR"]
thepc_merged_3 <- thepc_merged[super_region == "Latin America and Caribbean" & iso3 != "VIR"]
thepc_merged_4 <- thepc_merged[super_region == "North Africa and Middle East" & iso3 != "VIR"]
thepc_merged_5 <- thepc_merged[super_region == "South Asia" & iso3 != "VIR"]
thepc_merged_6 <- thepc_merged[super_region == "Southeast Asia, East Asia, and Oceania" & iso3 != "VIR"]
thepc_merged_7 <- thepc_merged[super_region == "Sub-Saharan Africa" & iso3 != "VIR"]

# plot panel 3
plot_3_a <- ggplot() + 
  geom_point(data = thepc_merged_1, aes(x = (who_value), y = (ihme_value), color = as.integer(as.character(year))), size = 1.3, show.legend = F) + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", size = 0.3, color = "black", alpha = 0.3) + 
  #geom_line(data = test, aes(x = x, y = upper), linetype = "dashed", size = 0.3, color = "red", alpha = 0.3) + 
  #geom_line(data = test, aes(x = x, y = lower), linetype = "dashed", size = 0.3, color = "red", alpha = 0.3) + 
  geom_text(data = thepc_merged_1, aes(y = (ihme_value), x = (who_value), label = ifelse(abs(((who_value) - (ihme_value))/(who_value))*100 > 10, as.character(iso3), '')), size = 1.5, hjust = -0.25, vjust = -0.25) +
  ggtitle(label = "Central Europe, Eastern Europe, and Central Asia") + ylab("IHME estimate (USD 2017)") + xlab("WHO estimate (USD 2017)") + theme_bw() + expand_limits(y = 0, x = 0) + scale_color_gradient(low = "grey", high = "blue") + 
  theme(plot.title = element_text(size = 8)) + 
  scale_y_log10() + scale_x_log10()

plot_3_b <- ggplot() + 
  geom_point(data = thepc_merged_2, aes(x = (who_value), y = (ihme_value), color = as.integer(as.character(year))), size = 1.3, show.legend = F) + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", size = 0.3, color = "black", alpha = 0.3) + 
  #geom_line(data = test, aes(x = x, y = upper), linetype = "dashed", size = 0.3, color = "red", alpha = 0.3) + 
  #geom_line(data = test, aes(x = x, y = lower), linetype = "dashed", size = 0.3, color = "red", alpha = 0.3) + 
  geom_text(data = thepc_merged_2, aes(y = (ihme_value), x = (who_value), label = ifelse(abs(((who_value) - (ihme_value))/(who_value))*100 > 10, as.character(iso3), '')), size = 1.5, hjust = -0.25, vjust = -0.25) +
  ggtitle(label = "GBD High-income") + ylab("IHME estimate (USD 2017)") + xlab("WHO estimate (USD 2017)") + theme_bw() + expand_limits(y = 0, x = 0) + scale_color_gradient(low = "grey", high = "blue") + 
  theme(plot.title = element_text(size = 8)) + 
  scale_y_log10() + scale_x_log10()

plot_3_c <- ggplot() + 
  geom_point(data = thepc_merged_3, aes(x = (who_value), y = (ihme_value), color = as.integer(as.character(year))), size = 1.3, show.legend = F) + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", size = 0.3, color = "black", alpha = 0.3) + 
  #geom_line(data = test, aes(x = x, y = upper), linetype = "dashed", size = 0.3, color = "red", alpha = 0.3) + 
  #geom_line(data = test, aes(x = x, y = lower), linetype = "dashed", size = 0.3, color = "red", alpha = 0.3) + 
  geom_text(data = thepc_merged_3, aes(y = (ihme_value), x = (who_value), label = ifelse(abs(((who_value) - (ihme_value))/(who_value))*100 > 10, as.character(iso3), '')), size = 1.5, hjust = -0.25, vjust = -0.25) +
  ggtitle(label = "Latin America and Caribbean") + ylab("IHME estimate (USD 2017)") + xlab("WHO estimate (USD 2017)") + theme_bw() + expand_limits(y = 0, x = 0) + scale_color_gradient(low = "grey", high = "blue") + 
  theme(plot.title = element_text(size = 8)) + 
  scale_y_log10() + scale_x_log10()

plot_3_d <- ggplot() + 
  geom_point(data = thepc_merged_4, aes(x = (who_value), y = (ihme_value), color = as.integer(as.character(year))), size = 1.3, show.legend = F) + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", size = 0.3, color = "black", alpha = 0.3) + 
  #geom_line(data = test, aes(x = x, y = upper), linetype = "dashed", size = 0.3, color = "red", alpha = 0.3) + 
  #geom_line(data = test, aes(x = x, y = lower), linetype = "dashed", size = 0.3, color = "red", alpha = 0.3) + 
  geom_text(data = thepc_merged_4, aes(y = (ihme_value), x = (who_value), label = ifelse(abs(((who_value) - (ihme_value))/(who_value))*100 > 10, as.character(iso3), '')), size = 1.5, hjust = -0.25, vjust = -0.25) +
  ggtitle(label = "North Africa and Middle East") + ylab("IHME estimate (USD 2017)") + xlab("WHO estimate (USD 2017)") + theme_bw() + expand_limits(y = 0, x = 0) + scale_color_gradient(low = "grey", high = "blue") + 
  theme(plot.title = element_text(size = 8)) + 
  scale_y_log10() + scale_x_log10()

plot_3_e <- ggplot() + 
  geom_point(data = thepc_merged_5, aes(x = (who_value), y = (ihme_value), color = as.integer(as.character(year))), size = 1.3, show.legend = F) + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", size = 0.3, color = "black", alpha = 0.3) + 
  #geom_line(data = test, aes(x = x, y = upper), linetype = "dashed", size = 0.3, color = "red", alpha = 0.3) + 
  #geom_line(data = test, aes(x = x, y = lower), linetype = "dashed", size = 0.3, color = "red", alpha = 0.3) + 
  geom_text(data = thepc_merged_5, aes(y = (ihme_value), x = (who_value), label = ifelse(abs(((who_value) - (ihme_value))/(who_value))*100 > 10, as.character(iso3), '')), size = 1.5, hjust = -0.25, vjust = -0.25) +
  ggtitle(label = "South Asia") + ylab("IHME estimate (USD 2017)") + xlab("WHO estimate (USD 2017)") + theme_bw() + expand_limits(y = 0, x = 0) + scale_color_gradient(low = "grey", high = "blue") + 
  theme(plot.title = element_text(size = 8)) + 
  scale_y_log10() + scale_x_log10()

plot_3_f <- ggplot() + 
  geom_point(data = thepc_merged_6, aes(x = (who_value), y = (ihme_value), color = as.integer(as.character(year))), size = 1.3, show.legend = F) + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", size = 0.3, color = "black", alpha = 0.3) + 
  #geom_line(data = test, aes(x = x, y = upper), linetype = "dashed", size = 0.3, color = "red", alpha = 0.3) + 
  #geom_line(data = test, aes(x = x, y = lower), linetype = "dashed", size = 0.3, color = "red", alpha = 0.3) + 
  geom_text(data = thepc_merged_6, aes(y = (ihme_value), x = (who_value), label = ifelse(abs(((who_value) - (ihme_value))/(who_value))*100 > 10, as.character(iso3), '')), size = 1.5, hjust = -0.25, vjust = -0.25) +
  ggtitle(label = "Southeast Asia, East Asia, and Oceania") + ylab("IHME estimate (USD 2017)") + xlab("WHO estimate (USD 2017)") + theme_bw() + expand_limits(y = 0, x = 0) + scale_color_gradient(low = "grey", high = "blue") + 
  theme(plot.title = element_text(size = 8)) + 
  scale_y_log10() + scale_x_log10()

plot_3_g <- ggplot() + 
  geom_point(data = thepc_merged_7, aes(x = (who_value), y = (ihme_value), color = as.integer(as.character(year))), size = 1.3, show.legend = F) + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", size = 0.3, color = "black", alpha = 0.3) + 
  #geom_line(data = test, aes(x = x, y = upper), linetype = "dashed", size = 0.3, color = "red", alpha = 0.3) + 
  #geom_line(data = test, aes(x = x, y = lower), linetype = "dashed", size = 0.3, color = "red", alpha = 0.3) + 
  geom_text(data = thepc_merged_7, aes(y = (ihme_value), x = (who_value), label = ifelse(abs(((who_value) - (ihme_value))/(who_value))*100 > 10, as.character(iso3), '')), size = 1.5, hjust = -0.25, vjust = -0.25) +
  ggtitle(label = "Sub-Saharan Africa") + ylab("IHME estimate (USD 2017)") + xlab("WHO estimate (USD 2017)") + theme_bw() + expand_limits(y = 0, x = 0) + scale_color_gradient(low = "grey", high = "blue") + 
  theme(plot.title = element_text(size = 8)) + 
  scale_y_log10() + scale_x_log10()

plot_3_g_dummy <- ggplot() + 
  geom_point(data = thepc_merged_7, aes(x = (who_value), y = (ihme_value), color = as.integer(as.character(year))), size = 1.3, show.legend = T) + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", size = 0.3, color = "black", alpha = 0.3) + 
  #geom_line(data = test, aes(x = x, y = upper), linetype = "dashed", size = 0.3, color = "red", alpha = 0.3) + 
  #geom_line(data = test, aes(x = x, y = lower), linetype = "dashed", size = 0.3, color = "red", alpha = 0.3) + 
  geom_text(data = thepc_merged_7, aes(y = (ihme_value), x = (who_value), label = ifelse(abs(((who_value) - (ihme_value))/(who_value))*100 > 10, as.character(iso3), '')), size = 1.5, hjust = -0.25, vjust = -0.25) +
  ggtitle(label = "Sub-Saharan Africa") + ylab("IHME estimate (USD 2017)") + xlab("WHO estimate (USD 2017)") + theme_bw() + expand_limits(y = 0, x = 0) + 
  theme(legend.direction = "horizontal", legend.position = c(0.5,0.5), legend.text = element_text(size = 6)) + scale_color_gradient(low = "grey", high = "blue", name = "Year")

plot_legend <- get_legend(plot_3_g_dummy)

# write panels to disk
pdf("slide_3.pdf", onefile = TRUE, width = 13, height = 7)
grid.arrange(plot_3_a, plot_3_b, plot_3_c, plot_3_d, plot_3_e, plot_3_f, plot_3_g, plot_legend, nrow=2, ncol=4, top=paste("THE per capita in 2017", sep = ''))
dev.off()

########################################################################################################################################################################################################################################################################
########################################################################################################################################################################################################################################################################

# ready WHO GHE data
ghepc_data <- data.table(read.xlsx("/ihme/homes/rahulrz6/GHE_data_USD2017.xlsx"))[, Indicators := NULL]
ghepc_data <- ghepc_data[, X3 := NULL]
ghepc_data <- ghepc_data[!is.na(Countries)]
ghepc_data_long <- melt(ghepc_data, id.vars = c("Countries"), measure.vars = c("2017"), variable.name = "year", value.name = "value")
ghepc_data_long$value <- as.numeric(ghepc_data_long$value)
ghepc_data_smm <- ghepc_data_long[, .(value_totes = sum(value, na.rm = T)), by = Countries]
colnames(ghepc_data_smm) <- c("location_name","value")

ghepc_raw <- merge(ghepc_data_smm, iso3_dat[, .(location_name, ihme_loc_id, super_region_name)], by=c("location_name"), all.x = T)

ghepc_raw[location_name == "Bolivia Plurinational States of ", ihme_loc_id:= "BOL"]
ghepc_raw[location_name == "Czech Republic", ihme_loc_id:= "CZE"]
ghepc_raw[location_name == "Iran", ihme_loc_id:= "IRN"]
ghepc_raw[location_name == "The Republic of North Macedonia", ihme_loc_id:= "MKD"]
ghepc_raw[is.na(ihme_loc_id), ihme_loc_id:= "CIV"]

ghepc_raw[ihme_loc_id %in% c("CZE", "MKD"), super_region_name := "Central Europe, Eastern Europe, and Central Asia"]
ghepc_raw[ihme_loc_id == "IRN", super_region_name := "North Africa and Middle East"]
ghepc_raw[ihme_loc_id == "CIV", super_region_name := "Sub-Saharan Africa"]
ghepc_raw[ihme_loc_id == "BOL", super_region_name := "Latin America and Caribbean"]

colnames(ghepc_raw) <- c("location_name", "who_value", "iso3", "super_region")

# get the IHME fits
ghepc_ihme <- fread("/home/j/Project/IRH/HE_Imputation/data/output/draws_stats_sorted/ghespc_stats.csv")[year == 2017, .(iso3, mean)]
colnames(ghepc_ihme) <- c("iso3","ihme_value")

# currency convert USD 2019 estimates to USD 2017
ghepc_ihme_converted <- currency_conversion(ghepc_ihme, col.loc = "iso3", col.value = "ihme_value", currency = "usd", currency.year = 2019, base.year = 2017, base.unit = "usd")

ghepc_merged <- merge(ghepc_raw, ghepc_ihme_converted, by=c("iso3"), all.x = T)

##########################################################################################################################################################################################

# ready WHO PPP data
ppppc_data <- data.table(read.xlsx("/ihme/homes/rahulrz6/PPP_data_USD2017.xlsx"))[, Indicators := NULL]
ppppc_data <- ppppc_data[, X3 := NULL]
ppppc_data <- ppppc_data[!is.na(Countries)]
ppppc_data_long <- melt(ppppc_data, id.vars = c("Countries"), measure.vars = c("2017"), variable.name = "year", value.name = "value")
ppppc_data_long$value <- as.numeric(ppppc_data_long$value)
ppppc_data_smm <- ppppc_data_long[, .(value_totes = sum(value, na.rm = T)), by = Countries]
ppppc_data_smm[value_totes == 0, value_totes := NA]
colnames(ppppc_data_smm) <- c("location_name","value")

ppppc_raw <- merge(ppppc_data_smm, iso3_dat[, .(location_name, ihme_loc_id, super_region_name)], by=c("location_name"), all.x = T)

ppppc_raw[location_name == "Bolivia Plurinational States of ", ihme_loc_id:= "BOL"]
ppppc_raw[location_name == "Czech Republic", ihme_loc_id:= "CZE"]
ppppc_raw[location_name == "Iran", ihme_loc_id:= "IRN"]
ppppc_raw[location_name == "The Republic of North Macedonia", ihme_loc_id:= "MKD"]
ppppc_raw[is.na(ihme_loc_id), ihme_loc_id:= "CIV"]

ppppc_raw[ihme_loc_id %in% c("CZE", "MKD"), super_region_name := "Central Europe, Eastern Europe, and Central Asia"]
ppppc_raw[ihme_loc_id == "IRN", super_region_name := "North Africa and Middle East"]
ppppc_raw[ihme_loc_id == "CIV", super_region_name := "Sub-Saharan Africa"]
ppppc_raw[ihme_loc_id == "BOL", super_region_name := "Latin America and Caribbean"]

colnames(ppppc_raw) <- c("location_name", "who_value", "iso3", "super_region")

# get the IHME fits
ppppc_ihme <- fread("/home/j/Project/IRH/HE_Imputation/data/output/draws_stats_sorted/ppppc_stats.csv")[year == 2017, .(iso3, mean)]
colnames(ppppc_ihme) <- c("iso3","ihme_value")

# currency convert USD 2019 estimates to USD 2017
ppppc_ihme_converted <- currency_conversion(ppppc_ihme, col.loc = "iso3", col.value = "ihme_value", currency = "usd", currency.year = 2019, base.year = 2017, base.unit = "usd")

ppppc_merged <- merge(ppppc_raw, ppppc_ihme_converted, by=c("iso3"), all.x = T)

##########################################################################################################################################################################################

# ready WHO OOP data
ooppc_data <- data.table(read.xlsx("/ihme/homes/rahulrz6/OOP_data_USD2017.xlsx"))[, Indicators := NULL]
ooppc_data <- ooppc_data[, X3 := NULL]
ooppc_data <- ooppc_data[!is.na(Countries)]
ooppc_data_long <- melt(ooppc_data, id.vars = c("Countries"), measure.vars = c("2017"), variable.name = "year", value.name = "value")
ooppc_data_long$value <- as.numeric(ooppc_data_long$value)
ooppc_data_smm <- ooppc_data_long[, .(Countries, value)]
colnames(ooppc_data_smm) <- c("location_name","value")

ooppc_raw <- merge(ooppc_data_smm, iso3_dat[, .(location_name, ihme_loc_id, super_region_name)], by=c("location_name"), all.x = T)

ooppc_raw[location_name == "Bolivia Plurinational States of ", ihme_loc_id:= "BOL"]
ooppc_raw[location_name == "Czech Republic", ihme_loc_id:= "CZE"]
ooppc_raw[location_name == "Iran", ihme_loc_id:= "IRN"]
ooppc_raw[location_name == "The Republic of North Macedonia", ihme_loc_id:= "MKD"]
ooppc_raw[is.na(ihme_loc_id), ihme_loc_id:= "CIV"]

ooppc_raw[ihme_loc_id %in% c("CZE", "MKD"), super_region_name := "Central Europe, Eastern Europe, and Central Asia"]
ooppc_raw[ihme_loc_id == "IRN", super_region_name := "North Africa and Middle East"]
ooppc_raw[ihme_loc_id == "CIV", super_region_name := "Sub-Saharan Africa"]
ooppc_raw[ihme_loc_id == "BOL", super_region_name := "Latin America and Caribbean"]

colnames(ooppc_raw) <- c("location_name", "who_value", "iso3", "super_region")

# get the IHME fits
ooppc_ihme <- fread("/home/j/Project/IRH/HE_Imputation/data/output/draws_stats_sorted/ooppc_stats.csv")[year == 2017, .(iso3, mean)]
colnames(ooppc_ihme) <- c("iso3","ihme_value")

# currency convert USD 2019 estimates to USD 2017
ooppc_ihme_converted <- currency_conversion(ooppc_ihme, col.loc = "iso3", col.value = "ihme_value", currency = "usd", currency.year = 2019, base.year = 2017, base.unit = "usd")

ooppc_merged <- merge(ooppc_raw, ooppc_ihme_converted, by=c("iso3"), all.x = T)

##########################################################################################################################################################################################

# ready WHO DAH data
dahpc_data <- data.table(read.xlsx("/ihme/homes/rahulrz6/DAH_data_USD2017.xlsx"))[, Indicators := NULL]
dahpc_data <- dahpc_data[, X3 := NULL]
dahpc_data <- dahpc_data[!is.na(Countries)]
dahpc_data_long <- melt(dahpc_data, id.vars = c("Countries"), measure.vars = c("2017"), variable.name = "year", value.name = "value")
dahpc_data_long$value <- as.numeric(dahpc_data_long$value)
dahpc_data_smm <- dahpc_data_long[, .(Countries, value)]
colnames(dahpc_data_smm) <- c("location_name","value")

dahpc_raw <- merge(dahpc_data_smm, iso3_dat[, .(location_name, ihme_loc_id, super_region_name)], by=c("location_name"), all.x = T)

dahpc_raw[location_name == "Bolivia Plurinational States of ", ihme_loc_id:= "BOL"]
dahpc_raw[location_name == "Czech Republic", ihme_loc_id:= "CZE"]
dahpc_raw[location_name == "Iran", ihme_loc_id:= "IRN"]
dahpc_raw[location_name == "The Republic of North Macedonia", ihme_loc_id:= "MKD"]
dahpc_raw[is.na(ihme_loc_id), ihme_loc_id:= "CIV"]

dahpc_raw[ihme_loc_id %in% c("CZE", "MKD"), super_region_name := "Central Europe, Eastern Europe, and Central Asia"]
dahpc_raw[ihme_loc_id == "IRN", super_region_name := "North Africa and Middle East"]
dahpc_raw[ihme_loc_id == "CIV", super_region_name := "Sub-Saharan Africa"]
dahpc_raw[ihme_loc_id == "BOL", super_region_name := "Latin America and Caribbean"]

colnames(dahpc_raw) <- c("location_name", "who_value", "iso3", "super_region")

# get the IHME fits
dahpc_ihme <- fread("/home/j/Project/IRH/HE_Imputation/data/output/draws_stats_sorted/dahpc_stats.csv")[year == 2017, .(iso3, mean)]
colnames(dahpc_ihme) <- c("iso3","ihme_value")

# currency convert USD 2019 estimates to USD 2017
dahpc_ihme_converted <- currency_conversion(dahpc_ihme, col.loc = "iso3", col.value = "ihme_value", currency = "usd", currency.year = 2019, base.year = 2017, base.unit = "usd")

dahpc_merged <- merge(dahpc_raw, dahpc_ihme_converted, by=c("iso3"), all.x = T)

##########################################################################################################################################################################################

# plot slide 4
plot_ghe <- ggplot() + 
  geom_point(data = ghepc_merged[iso3 != "VIR"], aes(x = (who_value), y = (ihme_value), color = super_region), size = 1.5) + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", size = 0.3, color = "black", alpha = 0.3) + 
  #geom_line(data = test, aes(x = x, y = upper), linetype = "dashed", size = 0.3, color = "red", alpha = 0.3) + 
  #geom_line(data = test, aes(x = x, y = lower), linetype = "dashed", size = 0.3, color = "red", alpha = 0.3) + 
  geom_text(data = ghepc_merged[iso3 != "VIR"], aes(y = (ihme_value), x = (who_value), label = ifelse(abs(((who_value) - (ihme_value))/(who_value))*100 > 10, as.character(iso3), '')), size = 1.5, hjust = -0.25, vjust = -0.25) +
  ggtitle(label = "GOV per capita in 2017") + ylab("IHME estimate (USD 2017)") + xlab("WHO estimate (USD 2017)") + theme_bw() + 
  theme(legend.position = c(0.16,0.65), legend.text = element_text(size = 5), legend.title = element_text(size = 5), legend.background = element_blank()) + expand_limits(y = 0, x = 0) + 
  scale_y_log10() + scale_x_log10()

plot_ppp <- ggplot() + 
  geom_point(data = ppppc_merged[!iso3 %in% c("VIR", "FSM")], aes(x = (who_value), y = (ihme_value), color = super_region), size = 1.5, show.legend = F) + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", size = 0.3, color = "black", alpha = 0.3) + 
  #geom_line(data = test, aes(x = x, y = upper), linetype = "dashed", size = 0.3, color = "red", alpha = 0.3) + 
  #geom_line(data = test, aes(x = x, y = lower), linetype = "dashed", size = 0.3, color = "red", alpha = 0.3) + 
  geom_text(data = ppppc_merged[!iso3 %in% c("VIR", "FSM")], aes(y = (ihme_value), x = (who_value), label = ifelse(abs(((who_value) - (ihme_value))/(who_value))*100 > 10, as.character(iso3), '')), size = 1.5, hjust = -0.25, vjust = -0.25) +
  ggtitle(label = "Prepaid per capita in 2017") + ylab("IHME estimate (USD 2017)") + xlab("WHO estimate (USD 2017)") + theme_bw() + 
  theme(legend.position = c(0.775,0.2), legend.text = element_text(size = 7), legend.title = element_text(size = 7)) + expand_limits(y = 0, x = 0) + 
  scale_y_log10(labels = function(x) format(x, scientific = FALSE)) + scale_x_log10()

plot_oop <- ggplot() + 
  geom_point(data = ooppc_merged[iso3 != "VIR"], aes(x = (who_value), y = (ihme_value), color = super_region), size = 1.5, show.legend = F) + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", size = 0.3, color = "black", alpha = 0.3) + 
  #geom_line(data = test, aes(x = x, y = upper), linetype = "dashed", size = 0.3, color = "red", alpha = 0.3) + 
  #geom_line(data = test, aes(x = x, y = lower), linetype = "dashed", size = 0.3, color = "red", alpha = 0.3) + 
  geom_text(data = ooppc_merged[iso3 != "VIR"], aes(y = (ihme_value), x = (who_value), label = ifelse(abs(((who_value) - (ihme_value))/(who_value))*100 > 10, as.character(iso3), '')), size = 1.5, hjust = -0.25, vjust = -0.25) +
  ggtitle(label = "OOP per capita in 2017") + ylab("IHME estimate (USD 2017)") + xlab("WHO estimate (USD 2017)") + theme_bw() + 
  theme(legend.position = c(0.775,0.2), legend.text = element_text(size = 7), legend.title = element_text(size = 7)) + expand_limits(y = 0, x = 0) + 
  scale_y_log10() + scale_x_log10()

plot_dah <- ggplot() + 
  geom_point(data = dahpc_merged[iso3 != "VIR"], aes(x = (who_value), y = (ihme_value), color = super_region), size = 1.5, show.legend = F) + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", size = 0.3, color = "black", alpha = 0.3) + 
  #geom_line(data = test, aes(x = x, y = upper), linetype = "dashed", size = 0.3, color = "red", alpha = 0.3) + 
  #geom_line(data = test, aes(x = x, y = lower), linetype = "dashed", size = 0.3, color = "red", alpha = 0.3) + 
  geom_text(data = dahpc_merged[iso3 != "VIR"], aes(y = (ihme_value), x = (who_value), label = ifelse(abs(((who_value) - (ihme_value))/(who_value))*100 > 10, as.character(iso3), '')), size = 1.5, hjust = -0.25, vjust = -0.25) +
  ggtitle(label = "DAH per capita in 2017") + ylab("IHME estimate (USD 2017)") + xlab("WHO estimate (USD 2017)") + theme_bw() + 
  theme(legend.position = c(0.775,0.2), legend.text = element_text(size = 7), legend.title = element_text(size = 7)) + expand_limits(y = 0, x = 0) + 
  scale_y_log10(labels = function(x) format(x, scientific = FALSE)) + scale_x_log10()

pdf("slide_4.pdf", onefile = TRUE, width = 13, height = 7)
grid.arrange(plot_ghe, plot_ppp, plot_oop, plot_dah, nrow=2, top=paste("Health spending in 2017", sep = ''))
dev.off()

########################################################################################################################################################################################################################################################################
########################################################################################################################################################################################################################################################################