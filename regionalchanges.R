library(renv)
library(data.table)
library(magrittr)
library(lubridate)
library(ggplot2)
library(scales)

oxcgrt_changes <- fread("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest_allchanges.csv")


# define WB regions ----------------------------------------------
East_Asia_Pacific <- c("ASM", "AUS", "BRN", "CHN", "FJI", "FSM", "GUM", "HKG", "IDN", "JPN", "KHM", "KIR", "KOR", "LAO", "MAC", "MHL", "MMR", "MNG", "MNP", "MYS", "NCL", "NRU", "NZL", "PHL", "PLW", "PNG", "PRK", "PYF", "SGP", "SLB", "THA", "TLS", "TON", "TUV", "TWN", "VNM", "VUT", "WSM")
Europe_Central_Asia <- c("ALB", "AND", "ARM", "AUT", "AZE", "BEL", "BGR", "BIH", "BLR", "CHE", "CHI", "CYP", "CZE", "DEU", "DNK", "ESP", "EST", "FIN", "FRA", "FRO", "GBR", "GEO", "GIB", "GRC", "GRL", "HRV", "HUN", "IMN", "IRL", "ISL", "ITA", "KAZ", "KGZ", "LIE", "LTU", "LUX", "LVA", "MCO", "MDA", "MKD", "MNE", "NLD", "NOR", "POL", "PRT", "ROU", "RUS", "SMR", "SRB", "SVK", "SVN", "SWE", "TJK", "TKM", "TUR", "UKR", "UZB", "RKS")
Latin_America_Caribbean <- c("ABW", "ARG", "ATG", "BHS", "BLZ", "BOL", "BRA", "BRB", "CHL", "COL", "CRI", "CUB", "CUW", "CYM", "DMA", "DOM", "ECU", "GRD", "GTM", "GUY", "HND", "HTI", "JAM", "KNA", "LCA", "MAF", "MEX", "NIC", "PAN", "PER", "PRI", "PRY", "SLV", "SUR", "SXM", "TCA", "TTO", "URY", "VCT", "VEN", "VGB", "VIR")
Middle_East_North_Africa <- c("ARE", "BHR", "DJI", "DZA", "EGY", "IRN", "IRQ", "ISR", "JOR", "KWT", "LBN", "LBY", "MAR", "MLT", "OMN", "PSE", "QAT", "SAU", "SYR", "TUN", "YEM")
North_America <- c("BMU", "CAN", "USA")
South_Asia <- c("AFG", "BGD", "BTN", "IND", "LKA", "MDV", "NPL", "PAK")
sub_Saharan_Africa <- c("AGO", "BDI", "BEN", "BFA", "BWA", "CAF", "CIV", "CMR", "COD", "COG", "COM", "CPV", "ERI", "ETH", "GAB", "GHA", "GIN", "GMB", "GNB", "GNQ", "KEN", "LBR", "LSO", "MDG", "MLI", "MOZ", "MRT", "MUS", "MWI", "NAM", "NER", "NGA", "RWA", "SDN", "SEN", "SLE", "SOM", "SSD", "STP", "SWZ", "SYC", "TCD", "TGO", "TZA", "UGA", "ZAF", "ZMB", "ZWE")
region_list <- c("East_Asia_Pacific", "Europe_Central_Asia", "Latin_America_Caribbean", "Middle_East_North_Africa", "North_America", "South_Asia", "sub_Saharan_Africa")

oxcgrt_changes$region <- NA

for(reg in region_list){
  regional <- get(reg)
  oxcgrt_changes[, region := ifelse(CountryCode %in% regional, reg, region)]
}

# shortlist all changes within the last 5 weeks and create current and previous code ------------------
oxcgrt_changes <-
  oxcgrt_changes %>%
  .[, FlagCode := ifelse(Flag == 0, "T", ifelse(Flag == 1, "G", NA))] %>%
  .[, PolicyCode := ifelse(!is.na(FlagCode), paste0(as.character(PolicyValue), FlagCode), as.character(PolicyValue))] %>%
  .[order(CountryCode, PolicyType, Date)] %>%
  .[, `:=`(previous_PolicyCode = shift(PolicyCode, n = 1L, type = "lag")), by = .(CountryCode, PolicyType)] %>%
  .[,`:=` (Date = ymd(Date))] %>%
  .[Date > Sys.Date() - 36] %>%
  .[,!c("PolicyValue", "Flag", "FlagCode")]

# subset by region and crate regional changes csv ---------------------
for(i in region_list){
  temp_dt <- oxcgrt_changes[region == as.name(i)]
  fwrite(temp_dt, file = paste0("latest_", i, ".csv"))
}

temp_dt <- temp_dt[order(CountryCode, PolicyType,Date)]

# Cases v/s Containment Health Index graphs ------------------
url_oxcgrt <- "https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_latest.csv"

oxcgrtdata <- fread(url_oxcgrt,
                    select = c("CountryName", "CountryCode", "RegionName", "RegionCode",
                                          "Jurisdiction", "Date", "ConfirmedCases", "ConfirmedDeaths",
                                          "ContainmentHealthIndexForDisplay"),
                    colClasses = list(character = c("RegionName", "RegionCode")))

oxcgrtdata <-
  oxcgrtdata %>%
  .[RegionName == ""] %>%
  .[, `:=`(Date = ymd(Date))] %>%
  .[order(CountryCode, Date)] %>%
  .[, ConfirmedCases := lapply(.SD, nafill, "locf"), .SDcols = "ConfirmedCases", by = CountryCode] %>%
  .[, log10_cases := log10(ConfirmedCases + 1)]

oxcgrtdata$region <- NA

for(reg in region_list){
  regional <- get(reg)
  oxcgrtdata[, region := ifelse(CountryCode %in% regional, reg, region)]
}


for(i in region_list){
  # subset the data by region
  temp_dt <- oxcgrtdata[region == as.name(i)]

  # find total number of countries in region and define other locals
  total_countries <- length(unique(temp_dt$CountryCode))
  max_logcases <- ceiling(max(temp_dt$log10_cases, na.rm = T))
  coeff <- 100/max_logcases
  maxDate <- max(unique(temp_dt$Date))

  # create graph if only one page is needed i.e. total countries < 32 in region
  if(total_countries < 32){
    deficit <- 32 - total_countries
    # create the NA values to fill up the deficit number of countries
    for(j in seq(1:deficit)){
      agg_dt <-
        temp_dt[,.(CountryName, Date)] %>%
        .[, CountryName := NA] %>%
        unique() %>%
        .[, CountryName := paste0(rep(" ", times = j), collapse = "")]

      temp_dt <-
        rbindlist(list(temp_dt, agg_dt), use.names = T, fill = T)
    }

    # define levels for ordering the facet wrap
    levels <- unique(temp_dt$CountryName)
    temp_dt[, country_group := factor(CountryName, levels = levels)]

    # create plot
    plot <-
      ggplot(data = temp_dt, aes(x = Date)) +
      geom_line(aes(y = log10_cases), colour = "purple") +
      geom_line(aes(y = ContainmentHealthIndexForDisplay/coeff), colour = "red") +
      scale_y_continuous(name = "Reported cases",
                         breaks = seq(0, max_logcases),
                         labels = comma(unlist(lapply(seq(0, max_logcases), function(x){return(10^x)}))),
                         #labels = comma(do.call(rbind,lapply(seq(0, max_logcases), function(x){return(10^x)}))),
                         sec.axis = sec_axis(~.*coeff,
                                             name = "Containment and Health Index",
                                             breaks = c(0, 20, 40, 60, 80, 100),
                                             labels = c(0, 20, 40, 60, 80, 100))) +
      expand_limits(y = c(0, max_logcases)) +
      scale_x_date(breaks = seq.Date(from = ymd(as.Date("2020-01-01")), to = maxDate, by = "2 month"),
                   date_labels = "%d-%b") +
      labs(#title = paste0(i, "'s Covid-19 Trajectory"),
        caption = "Source: Oxford COVID-19 Government Response Tracker. More at https://github.com/OxCGRT/covid-policy-tracker
       or bsg.ox.ac.uk/covidtracker") +
      theme(
        # Remove panel border
        axis.text.y.right = element_text(colour = "red"),
        axis.title.y.right = element_text(colour = "red"),
        axis.text.y.left = element_text(colour = "purple"),
        axis.title.y.left = element_text(colour = "purple"),
        panel.border = element_blank(),
        # Remove panel grid lines
        panel.grid.major = element_line(size = 0.5, linetype = "dashed", colour = "grey"),
        #panel.grid.minor = element_blank(),
        # Remove panel background
        panel.background = element_blank(),
        # Add axis line
        axis.line = element_line(colour = "grey"),
        plot.caption = element_text(hjust = 0.5, face = "italic"),
        plot.title = element_text(hjust = 0.5)) +
      facet_wrap(~country_group, ncol = 4)
    ggsave(plot = plot,
           filename = paste0("charts_", i, ".eps"),
           device = "eps",
           height = 20,
           width = 15)

  } else{
    for(j in c(1,2)){
      if(j == 1){
        countries <- unique(temp_dt$CountryName)[1:32]
        plot <-
          ggplot(data = temp_dt[CountryName %in% countries], aes(x = Date)) +
          geom_line(aes(y = log10_cases), colour = "purple") +
          geom_line(aes(y = ContainmentHealthIndexForDisplay/coeff), colour = "red") +
          scale_y_continuous(name = "Reported cases",
                             breaks = seq(0, max_logcases),
                             labels = comma(unlist(lapply(seq(0, max_logcases), function(x){return(10^x)}))),
                             sec.axis = sec_axis(~.*coeff,
                                                 name = "Containment and Health Index",
                                                 breaks = c(0, 20, 40, 60, 80, 100),
                                                 labels = c(0, 20, 40, 60, 80, 100))) +
          expand_limits(y = c(0, max_logcases)) +
          scale_x_date(breaks = seq.Date(from = ymd(as.Date("2020-01-01")), to = maxDate, by = "2 month"),
                       date_labels = "%d-%b") +
          labs(#title = paste0(i, "'s Covid-19 Trajectory"),
            caption = "Source: Oxford COVID-19 Government Response Tracker. More at https://github.com/OxCGRT/covid-policy-tracker
       or bsg.ox.ac.uk/covidtracker") +
          theme(
            # Remove panel border
            axis.text.y.right = element_text(colour = "red"),
            axis.title.y.right = element_text(colour = "red"),
            axis.text.y.left = element_text(colour = "purple"),
            axis.title.y.left = element_text(colour = "purple"),
            panel.border = element_blank(),
            # Remove panel grid lines
            panel.grid.major = element_line(size = 0.5, linetype = "dashed", colour = "grey"),
            #panel.grid.minor = element_blank(),
            # Remove panel background
            panel.background = element_blank(),
            # Add axis line
            axis.line = element_line(colour = "grey"),
            plot.caption = element_text(hjust = 0.5, face = "italic"),
            plot.title = element_text(hjust = 0.5)) +
          facet_wrap(~CountryName, ncol = 4)
        ggsave(plot = plot,
               filename = paste0("charts_" ,i, j, ".eps"),
               device = "eps",
               height = 20,
               width = 15)

      }else{
        countries <- unique(temp_dt$CountryName)[33:length(unique(temp_dt$CountryName))]
        total_countries <- length(countries)
        deficit <- 32 - total_countries
        temp_dt <- temp_dt[CountryName %in% countries]

        for(k in seq(1:deficit)){
          agg_dt <-
            temp_dt %>%
            .[,.(CountryName, Date)] %>%
            .[, CountryName := NA] %>%
            unique() %>%
            .[, CountryName := paste0(rep(" ", times = k), collapse = "")]

          temp_dt <-
            rbindlist(list(temp_dt, agg_dt), use.names = T, fill = T)
        }
        levels <- unique(temp_dt$CountryName)
        temp_dt <- temp_dt[, country_group := factor(CountryName, levels = levels)]

        plot <-
          ggplot(data = temp_dt, aes(x = Date)) +
          geom_line(aes(y = log10_cases), colour = "purple") +
          geom_line(aes(y = ContainmentHealthIndexForDisplay/coeff), colour = "red") +
          scale_y_continuous(name = "Reported cases",
                             breaks = seq(0, max_logcases),
                             labels = comma(unlist(lapply(seq(0, max_logcases), function(x){return(10^x)}))),
                             sec.axis = sec_axis(~.*coeff,
                                                 name = "Containment and Health Index",
                                                 breaks = c(0, 20, 40, 60, 80, 100),
                                                 labels = c(0, 20, 40, 60, 80, 100))) +
          expand_limits(y = c(0, max_logcases)) +
          scale_x_date(breaks = seq.Date(from = ymd(as.Date("2020-01-01")), to = maxDate, by = "2 month"),
                       date_labels = "%d-%b") +
          labs(#title = paste0(i, "'s Covid-19 Trajectory"),
            caption = "Source: Oxford COVID-19 Government Response Tracker. More at https://github.com/OxCGRT/covid-policy-tracker
       or bsg.ox.ac.uk/covidtracker") +
          theme(
            # Remove panel border
            axis.text.y.right = element_text(colour = "red"),
            axis.title.y.right = element_text(colour = "red"),
            axis.text.y.left = element_text(colour = "purple"),
            axis.title.y.left = element_text(colour = "purple"),
            panel.border = element_blank(),
            # Remove panel grid lines
            panel.grid.major = element_line(size = 0.5, linetype = "dashed", colour = "grey"),
            #panel.grid.minor = element_blank(),
            # Remove panel background
            panel.background = element_blank(),
            # Add axis line
            axis.line = element_line(colour = "grey"),
            plot.caption = element_text(hjust = 0.5, face = "italic"),
            plot.title = element_text(hjust = 0.5)) +
          facet_wrap(~country_group, ncol = 4)
        ggsave(plot = plot,
               filename = paste0("charts_" ,i, j, ".eps"),
               device = "eps",
               height = 20,
               width = 15)
      }
    }
  }


}
