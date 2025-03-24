#NOTE population denominators account for proportion of population at risk
#NOTE funding date represents the nearest whole $ rounded up. Ex. $3 in dataset is actually $2-3in WMR.


source("00_setup.R")

data <- read_xlsx("data/wmr2024_annex_4f.xlsx") %>% 
  #filter(country == "African") %>% 
  mutate(cases_millions = cases/1000000,
         cases_lower = as.numeric(cases_lower),
         cases_upper = as.numeric(cases_upper),
         deaths_lower = as.numeric(deaths_lower),
         deaths_upper = as.numeric(deaths_upper),
         #calculate cases per 1,000 persons at risk
         cases_lower_rate = 1000*(cases_lower/population)
         ,cases_rate = 1000*(cases/population)
         ,cases_upper_rate = 1000*(cases_upper/population)
         #calculate deaths per 100,000 persons at risk
         ,deaths_lower_rate = 100000*(deaths_lower/population)
         ,deaths_rate = 100000*(deaths/population)
         ,deaths_upper_rate = 100000*(deaths_upper/population))


data_combined <- data %>% 
  filter(country == "African") %>% 
#add column for deaths at baseline to serve as boundary for plot below %>% 
  mutate(deaths_baseline = c(804683),
         deaths_averted = deaths_baseline - deaths)
pop_atrisk <- data_combined %>% 
  dplyr::select(c(year,population)) %>% 
  mutate(date = as.Date(with(ITN_BV, paste(year, "01", "01", sep="-")), "%Y-%m-%d"))

#subset to >2001 so as to not highlight the increase in deaths in 2001 w plot shading
interim <- data.frame(country = "African", year = 2001.5, 
                      deaths_baseline = 804684, deaths = 804683)  #if this looks weird then try values at 2001 and 2002/2
data_shading <- data_combined %>% 
  filter(year >2001) %>% as.data.frame()
data_shading <- rbind.fill(data_shading, interim)

plot_1 <- ggplot(data_combined, aes(year, deaths)) +
  geom_line(size = 0.5, color = "darkblue", fill = "darkblue") +
  geom_ribbon(aes(ymin = deaths_lower, ymax = deaths_upper), alpha = 0.2, fill = "darkblue") +
  theme_bw() + 
  scale_y_continuous(labels = label_comma(),
                     limits = c(0,860000)) +
 
  #geom_segment(x = 2000, xend = 2023, y = 804683, yend = 804683, linetype = "dashed") +
  ylab("total malaria deaths per year, WHO African Region\n") +
  xlab("\nyear") +
  theme(axis.text = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16)); plot_1

plot_2 <- ggplot(data_combined, aes(year, deaths)) +
  scale_y_continuous(labels = label_comma(),
                     limits = c(500000, 860000)) +
  scale_x_continuous(limits = c(2000,2025.5)) +
 
  ylab("Total malaria deaths per year,\n WHO African Region\n") +
  xlab("\nyear") +
  geom_ribbon_pattern(data = data_shading, 
                      aes(ymin = deaths, ymax = deaths_baseline),
   #pattern = level, pattern_angle = level, pattern_spacing = level), 
   fill = 'white', colour = 'lightgrey', 
   pattern_spacing = 0.02,
   pattern_density = 0.1, 
   pattern_angle = 60,
   pattern_fill    = 'lightblue', alpha = 0.5,
   linetype = 0) +
  geom_line(size = 1, color = "darkblue") +
  geom_segment(x = 2000, xend = 2023, y = 804683, yend = 804683, 
               linetype = "dashed", size = 1) +
  geom_segment(x = 2023.5, xend = 2023.5, y = 568749, yend = 804683, 
               linetype = "solid", size = 0.75) +
  annotate(
    geom = "text",
    x = 2023.8,
    y = 700000,
    label = "3.9 million \ncumulative \ndeaths averted",
    lineheight = 1,
    label.size = 0,
    hjust = 0,
    size = 4
  ) +
  theme_bw() +
  theme(legend.title=element_blank(),
        axis.text = element_text(size = 16),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20)); plot_2

#sum(data_combined$deaths_averted)

ggsave("plots/WHO African region deaths per year shading.jpg", plot_2, height = 5, width = 14)


### same plot as above without shading, baseline deaths line, and deaths averted text

plot_2.1 <- ggplot(data_combined, aes(year, deaths)) +
  scale_y_continuous(labels = label_comma(),
                     limits = c(500000, 860000)) +
  scale_x_continuous(limits = c(2000,2023)) +
  
  ylab("Total malaria deaths per year,\n WHO African Region\n") +
  xlab("\nyear") +
  geom_line(size = 1, color = "darkblue") +
  theme_bw() +
  theme(legend.title=element_blank(),
        axis.text = element_text(size = 16),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20)); plot_2.1

#sum(data_combined$deaths_averted)

ggsave("plots/WHO African region deaths per year minimal.jpg", plot_2.1, height = 7, width = 14)


### plot of cases 

plot_3 <- ggplot(data_combined, aes(year, cases_millions)) +
  scale_y_continuous(labels = label_comma(),
                     limits = c(200,250)) +
  scale_x_continuous(limits = c(2000,2023)) +
  
  ylab("Total malaria cases (millions)\n per year,\n WHO African Region\n") +
  xlab("\nyear") +
  geom_line(size = 1, color = "darkblue") +
  theme_bw() +
  theme(legend.title=element_blank(),
        axis.text = element_text(size = 16),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20)); plot_3

#sum(data_combined$deaths_averted)

ggsave("plots/WHO African region cases per year.jpg", plot_3, height = 7, width = 14)


### plot of incidence - cases per population at risk

plot_4 <- ggplot(data_combined, aes(year, cases_rate)) +
  scale_y_continuous(labels = label_comma(),
                     limits = c(200, 400)) +
  scale_x_continuous(limits = c(2000,2023)) +
  
  ylab("Total malaria cases per year\n per 1,000 persons at risk,\n WHO African Region\n") +
  xlab("\nyear") +
  geom_line(size = 1, color = "darkblue") +
  theme_bw() +
  theme(legend.title=element_blank(),
        axis.text = element_text(size = 16),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20)); plot_4

#sum(data_combined$deaths_averted)          figure does not account for population growth

ggsave("plots/WHO African region cases per 1000 persons at risk per year.jpg", plot_4, height = 7, width = 14)



### plot ITN use over time - data taken from
# https://www.nature.com/articles/s41467-021-23707-7/figures/2
# https://github.com/bertozzivill/map-itn-cube/blob/publication-2021/paper_figures/figure_data/fig_2_access_use_timeseries.csv

#get ITN coverage data from paper
ITN_BV <- read.csv("data/fig_2_access_use_timeseries.csv") %>% 
  #data is at country-level; filtering to SSA aggregate data:
  filter(iso3 == "AFR",
         variable == "use",
         year <2010
         ) %>% 
  group_by(year) %>% 
  dplyr::summarise(use = mean(mean_among_atrisk)) %>% 
  mutate(date = as.Date(with(ITN_BV, paste(year, "01", "01", sep="-")), "%Y-%m-%d"),
         source = "Bertozzi-Villa") %>% dplyr::select(-c(year))

#get proportion at risk from paper - don't really need this as WHO pop denominator is at-risk population
prop_atrisk <- read.csv("data/fig_2_access_use_timeseries.csv") %>% 
  filter(iso3 == "AFR",
         variable == "use") %>% 
  group_by(year) %>% 
  dplyr::summarise(prop_atrisk = mean(prop_atrisk)) %>% 
  mutate(date = as.Date(with(ITN_BV, paste(year, "01", "01", sep="-")), "%Y-%m-%d"),
         source = "Bertozzi-Villa") %>% dplyr::select(-c(year))


#get 2010 - 2022 data from MAP - 
ITN_MAP <- read.csv("data/MAP ITN metrics 2010-2022.csv") %>% 
  filter(Metric == "Use") %>% 
  group_by(Year) %>% 
  dplyr::summarise(use = mean(Value)/100) %>% 
  mutate(date = lubridate::ymd(Year, truncated = 2),
         #iso3 = "AFR", 
         source = "MAP") %>% dplyr::select(-c(Year))

ITN <- rbind(ITN_BV, ITN_MAP) %>% 
 left_join(pop_atrisk, by = "date") %>% 
  mutate(pop_cov = population*use)

#updates to above data frame prior to merging w/ITN data
data_combined <- data_combined %>% mutate(date = lubridate::ymd(year, truncated = 2),
                                          iso3 = "AFR") %>% 
  left_join(ITN)

ITN <- data_combined %>% dplyr::select(c(year, deaths, use, pop_cov))

data_long <- ITN %>% 
  pivot_longer(-c(year)) %>% 
  mutate(label = str_replace_all(name, c("deaths" = "Total malaria deaths per year,\n WHO African Region",
                          "use" = "ITN coverage per person at risk,\n WHO African Region",
                          "pop_cov" = "Population protected by ITN,\n WHO African Region"))) %>% 
  #reorder so plot of deaths is on top:
  mutate(label = fct_relevel(label, "Total malaria deaths per year,\n WHO African Region",
                             "ITN coverage per person at risk,\n WHO African Region",
                             "Population protected by ITN,\n WHO African Region"))

plot_data <-data_long %>% 
  filter(label != "ITN coverage per person at risk,\n WHO African Region")

plot_5 <- ggplot(plot_data, aes(year, value)) +
  xlab("\nyear") +
  geom_line(size = 1, color = "darkblue") +
  facet_grid(rows = vars(label), scales = "free") + 
  
  theme_bw() +   
  facetted_pos_scales(y = list(
    label == "Total malaria deaths per year,\n WHO African Region" ~ 
      scale_y_continuous(breaks = c(600000, 700000, 800000), labels = label_comma()),
    label ==  "Population protected by ITN,\n WHO African Region" ~ 
      scale_y_continuous(breaks = c(0,200000000,400000000), labels = label_comma()))) +
    theme(legend.title = element_blank(),
        axis.text = element_text(size = 16),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_blank(),
        strip.text.y = element_text(size = 12)); plot_5

ggsave("plots/WHO African region deaths ITN coverage population.jpg", plot_5, height = 6, width = 9)














#coeff <-  0.000000575
#data_combined$ITNuse_transformed <- data_combined$use/coeff
#plot_5 <- ggplot(data_combined, aes(year)) +
#  geom_line(aes(y = deaths)
#            #,data = ~transform(., deaths = scales::rescale(mean_among_atrisk, range(deaths)))
#            ,color = "darkblue", size = 1) +
#  geom_line(aes(y = ITNuse_transformed), size = 1, color = "#D41159") +
#  scale_y_continuous(labels = label_comma(),
#                     #limits = c(300000, 850000),
#                     sec.axis = sec_axis(~scales::rescale(.,range(data_combined$mean_among_atrisk),
#                                                          range(data_combined$deaths)))) +
#  scale_x_continuous(limits = c(2000,2023)) +
  
#  ylab("Total malaria deaths per year,\n WHO African Region\n") +
#  xlab("\nyear") +
#   theme_bw() +
#  theme(legend.title=element_blank(),
#        axis.text = element_text(size = 16),
#        axis.title.x = element_text(size = 20),
#        axis.title.y = element_text(size = 20)); plot_5


#coeff <- 0.0000015625
#data_combined$ITNuse_transformed <- data_combined$mean_among_atrisk/coeff
#plot_5 <- ggplot(data_combined, aes(year)) +
#  geom_line(aes(y = deaths)
#            #,data = ~transform(., deaths = scales::rescale(mean_among_atrisk, range(deaths)))
#            ,color = "darkblue", size = 1) +
#  geom_line(aes(y = ITNuse_transformed), size = 1, color = "#D41159") +
#  scale_y_continuous(
#                     #limits = c(300000, 850000),
#                     sec.axis = ~(.-500000)/coeff
#                     ,labels = label_comma()) +
#  scale_x_continuous(limits = c(2000,2023)) +
#  
#  ylab("Total malaria deaths per year,\n WHO African Region\n") +
#  xlab("\nyear") +
#  
#  theme_bw() +
#  theme(legend.title=element_blank(),
#        axis.text = element_text(size = 16),
#        axis.title.x = element_text(size = 20),
#        axis.title.y = element_text(size = 20)); plot_5


#sum(data_combined$deaths_averted)











#############################################################################
#############################################################################
############################    ARCHIVE    ##################################

###ADD MB FUNDING DATA - 
budget <- read_xlsx("data/MB Funding History.xlsx") %>% 
  rename(year = Year) %>% 
  dplyr::select(c(1:2))

### Global spending data from IHME: 2000 - 2020, in 2021 USD:
global <- read.csv("data/IHME_GLOBAL_MALARIA_SPENDING_2000_2020_Y2024M11D15.csv") %>% 
  filter(location_name == "Sub-Saharan Africa") %>% 
  rename(funding_2021 = the_total_mean,
         year = year_id) %>% 
  #convert 2021 dollars to 2023 dollars
  mutate(funding = funding_2021*1.14) %>% 
  dplyr::select(c(year,funding)) %>% 
  mutate(source = "IHME")

### Global spending data from WMR: 2021 - 2023, in 2023 USD:
global_2021 <- read_xlsx("data/wmr2024_annex_4c.xlsx") %>% 
  group_by(year) %>% 
  summarise(funding = sum(total)/1000) %>% 
  mutate(source = "WMR")
global_funding <- rbind(global, global_2021) %>% #pivot_wider(names_from = source, values_from = fundi
  rename(`Global malaria funding (x1000 USD)` = funding)
  #left_join(budget, by = c("year")) %>% 
  #mutate(metric = "funding") %>% 
  #dplyr::select(-c(`PMI USAID IAA Transfer Funds`)) %>% 
  #pivot_longer(-c(year, source, metric))
  
data_simple <- data_combined %>% 
  dplyr::select(c("year","deaths"))
  
plot_data <- data_simple %>% 
  left_join(global_funding) %>% 
  left_join(budget) %>% 
  pivot_longer(-c(year, source)) %>% 
  mutate(name = str_replace_all(name, "deaths", "Total malaria deaths per year, WHO African Region"),
         name = str_replace_all(name, "DPDM Malaria Branch Appropriated Funds", "Malaria Branch Appropriated Funds (x1000 USD)"))


plot_data$name <- factor(plot_data$name, levels=c("Total malaria deaths per year, WHO African Region",
                                                        "Global malaria funding (x1000 USD)",
                                                        "Malaria Branch Appropriated Funds (x1000 USD)"))


plot_2.1 <-  ggplot(plot_data, aes(year, value, color = name, fill = name)) +
  geom_line(size = 0.7, aes(color = name)) +
  #geom_ribbon(aes(ymin = deaths_lower, ymax = deaths_upper), 
  #            alpha = 0.2, color = NA, show.legend = FALSE) +
  theme_bw() +
  xlab("\nyear") +
  scale_y_continuous(labels = label_comma()) +
  facet_grid(rows = vars(name), scales = "free") + 
  theme(legend.position="none",
        axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_blank(),
        strip.text = element_text(size = 10))
plot_2.1




plot_2.2 <-  ggplot(data_combined, aes(year, value, color = name, fill = name)) +
  geom_line(size = 0.7, aes(color = name)) +
  #geom_ribbon(aes(ymin = deaths_lower, ymax = deaths_upper), 
  #            alpha = 0.2, color = NA, show.legend = FALSE) +
  theme_bw() +
  xlab("\nyear") +
  scale_y_continuous(labels = label_comma()) +
  facet_grid(rows = vars(name), scales = "free") + 
  theme(legend.position="none",
        axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_blank(),
        strip.text = element_text(size = 12))
plot_2.1


#add 2024 GDP per capita per 2024 International Monetary Fund
annex_4f <- read_xlsx("data/wmr2024_annex_4f_WHOAFRO.xlsx") 
gdp <- data.frame(unique(annex_4f$country)) %>% rename(country =1) %>% 
  mutate(gdp_2024 = c(5579,2961,1510,7341,908,321,5388,1821,530,1013,1630,2384,
                      2720,702,8102,567,4375,1350,9257,989,2232,1652,1106,2218,
                      855,563,464,898,2376,645,4410,698,877,986,3425,1805,856,
                      6377,341,1051,1187,1224,1226,2114))


data <- data %>% left_join(gdp, by = "country") %>% 
  #exclude small islands:
  filter(country != "Cabo Verde") %>% 
  filter(country != "Sao Tome and Principe") %>% 
  filter(country != "Comoros") %>% 
  #exclude missing funding:
  mutate(funding_2021_2023 = as.numeric(funding_2021_2023)) %>%  
  filter(!is.na(funding_2021_2023))

hist(data$gdp_2024)
hist(data$funding_2021_2023)

median(data$funding_2021_2023)  #3
median(data$gdp_2024)           #1200

data <- data %>% 
  #add funding categories:
  mutate(funding_category = case_when(funding_2021_2023 <= 3 ~ "low (<=3 USD)",
                                      funding_2021_2023 >3 ~ "high (>3 USD)")) %>% 
  #add GDP categories:
  mutate(gdp_category = case_when(gdp_2024 <= 1300 ~ "low",
                                  gdp_2024 > 1300 ~ "high")) 



data_by_funding <- data %>% 
  filter(!is.na(funding_category)) %>% 
  group_by(year,funding_category) %>% 
  summarise(deaths = sum(deaths),
            deaths_upper = sum(deaths_upper),
            deaths_lower = sum(deaths_lower))

data_by_funding$funding_category <- factor(data_by_funding$funding_category, levels=c("low (<=3 USD)", "high (>3 USD)"))



plot_3 <-  ggplot(data_by_funding, aes(year, deaths, color = funding_category, fill = funding_category)) +
  geom_line(size = 0.7) +
  geom_ribbon(aes(ymin = deaths_lower, ymax = deaths_upper), 
              alpha = 0.2, color = NA, show.legend = FALSE) +
  theme_bw() +
  ylab("total malaria deaths per year\n") + 
  xlab("\nyear") +
  guides(color = guide_legend(title = "funding per person at-risk")) +
  scale_color_manual(values = c("low (<=3 USD)" = "#238A8DFF",
                                "high (>3 USD)" = "#404788FF")) + 
  scale_fill_manual(values = c("low (<=3 USD)" = "#238A8DFF",
                               "high (>3 USD)" = "#404788FF")) +
  
  scale_y_continuous(labels = label_comma()) +
  theme(axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
         strip.text = element_text(size = 22)); plot_3


###
data_by_funding_gdp <- data %>% 
  filter(!is.na(funding_category)) %>% 
  group_by(year,funding_category,gdp_category) %>% 
  summarise(deaths = sum(deaths),
            population = sum(population),
            deaths_upper = sum(deaths_upper),
            deaths_lower = sum(deaths_lower))

data_by_funding_gdp$gdp_category <- factor(data_by_funding_gdp$gdp_category, levels=c("low", "high"))

# New facet labels
gdp.labs <- c("lower GDP: <1200 USD annual per capita", 
              "higher GDP: >1200 USD annual per capita")

names(gdp.labs) <- c("low", "high")

plot_4 <-  ggplot(data_by_funding_gdp, aes(year, deaths, color = funding_category, fill = funding_category)) +
  geom_line(size = 0.7) +
  geom_ribbon(aes(ymin = deaths_lower, ymax = deaths_upper), 
              alpha = 0.2, color = NA, show.legend = FALSE) +
  theme_bw() +
  ylab("total malaria deaths per year\n") + 
  xlab("\nyear") +
  guides(color = guide_legend(title = "funding per person at-risk")) +
  scale_color_manual(values = c("low (<=3 USD)" = "#238A8DFF",
                                "high (>3 USD)" = "#404788FF")) + 
  scale_fill_manual(values = c("low (<=3 USD)" = "#238A8DFF",
                               "high (>3 USD)" = "#404788FF")) +
  scale_y_continuous(labels = label_comma()) +
  facet_grid(cols = vars(gdp_category), #scales = "free",
            labeller = labeller(gdp_category = gdp.labs)) + 
  theme(axis.text = element_text(size = 12),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        strip.text = element_text(size = 12)); plot_4
