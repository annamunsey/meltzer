#NOTE population denominators account for proportion of population at risk
#NOTE funding date represents the nearest whole $ rounded up. Ex. $3 in dataset is actually $2-3in WMR.


source("00_setup.R")

data <- read_xlsx("data/wmr2024_annex_4f.xlsx") %>% 
  #filter(country == "African") %>% 
  mutate(cases_lower = as.numeric(cases_lower),
         cases_upper = as.numeric(cases_upper),
         deaths_lower = as.numeric(deaths_lower),
         deaths_upper = as.numeric(deaths_upper),
         cases_lower_rate = 1000*(cases_lower/population)
         ,cases_rate = 1000*(cases/population)
         ,cases_upper_rate = 1000*(cases_upper/population)
         ,deaths_lower_rate = 100000*(deaths_lower/population)
         ,deaths_rate = 100000*(deaths/population)
         ,deaths_upper_rate = 100000*(deaths_upper/population))


data_combined <- data %>% 
  filter(country == "African") 
plot_1 <- ggplot(data_combined, aes(year, deaths)) +
  geom_line(size = 0.5, color = "darkblue", fill = "darkblue") +
  geom_ribbon(aes(ymin = deaths_lower, ymax = deaths_upper), alpha = 0.2, fill = "darkblue") +
  theme_bw() + 
  scale_y_continuous(labels = label_comma(),
                     limits = c(500000,860000)) +
  #geom_segment(x = 2000, xend = 2023, y = 804683, yend = 804683, linetype = "dashed") +
  ylab("total malaria deaths per year, WHO African Region\n") +
  xlab("\nyear") +
  theme(axis.text = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16)); plot_1

plot_2 <- ggplot(data_combined, aes(year, deaths)) +
  geom_line(size = 0.5, color = "darkblue") +
  theme_bw() +
  
  theme(legend.title=element_blank(),
        #axis.title.y = element_blank()
  ) + 
  scale_y_continuous(labels = label_comma(),
                     limits = c(500000,860000)) +
  geom_segment(x = 2000, xend = 2023, y = 804683, yend = 804683, linetype = "dashed") +
  ylab("total malaria deaths per year, WHO African Region\n") +
  xlab("\nyear") +
  theme(axis.text = element_text(size = 14),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16)); plot_2


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
