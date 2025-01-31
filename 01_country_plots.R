#NOTE population denominators account for proportion of population at risk

source("00_setup.R")

annex_4f <- read_xlsx("data/wmr2024_annex_4f_WHOAFRO.xlsx") 

unique(annex_4f$country)

#add 2024 GDP per capita per wikipedia
gdp <- data.frame(unique(annex_4f$country)) %>% rename(country =1) %>% 
  mutate(gdp_2024 = c(5579,2961,1510,7341,908,321,5388,1821,530,1013,1630,2384,
                      2720,702,8102,567,4375,1350,9257,989,2232,1652,1106,2218,
                      855,563,464,898,2376,645,4410,698,877,986,3425,1805,856,
                      6377,341,1051,1187,1224,1226,2114))

table <- gdp %>% 
  filter(!is.na(gdp_2024))

  #add funding categories:
  mutate(funding_category = case_when(funding_2021_2023 <= 3 ~ "low",
                                      funding_2021_2023 >3 ~ "high")) %>% 
  dplyr::select(-c(funding_2021_2023))

  #exclude north africa:
 #   filter(country != "Algeria") %>% 

  #exclude islands: 
#  filter(country != "Cabo Verde") %>% 
#  filter(country != "Sao Tome and Principe") %>% 
#  filter(country != "Comoros") %>% 
#  filter(country != "Madagascar") %>% 
#  mutate(gdp_tertile = ntile(gdp_2024, 3)) %>% 
  
  #exclude low transmission countries:
#  filter(country != "South Africa") %>% 
#  filter(country != "Botswana") %>% 
#  filter(country != "Namibia") %>% 
#  filter(country != "Eswatini")

#write.csv(countries, "countries_partial.csv")





cases <- annex_4f %>% left_join(countries, by = "country") %>% 
  mutate(cases_lower_rate = 1000*(cases_lower/population)
         ,cases_rate = 1000*(cases/population)
         ,cases_upper_rate = 1000*(cases_upper/population)
         ,deaths_lower_rate = 100000*(deaths_lower/population)
         ,deaths_rate = 100000*(deaths/population)
         ,deaths_upper_rate = 100000*(deaths_upper/population)) %>% 
  #exclude north africa:
  filter(country != "Algeria") %>% 
  
  #exclude islands: 
  filter(country != "Cabo Verde") %>% 
  filter(country != "Sao Tome and Principe") %>% 
  filter(country != "Comoros") %>% 
  filter(country != "Madagascar") %>% 
  
#  %>% filter(country %in% select_coutries)
#exclude low transmission countries:
filter(country != "South Africa") %>% 
  filter(country != "Botswana") %>% 
  filter(country != "Namibia") %>% 
  filter(country != "Eswatini")


plot_data_cases <- cases %>% dplyr::select(1,2,11:14) %>% 
  rename(lower = cases_lower_rate,
         point = cases_rate,
         upper = cases_upper_rate) %>% 
  mutate(metric_category = "cases per 1,000 persons")

plot_data_deaths <- cases %>% dplyr::select(1,2,11,15:17) %>% 
  rename(lower = deaths_lower_rate,
         point = deaths_rate,
         upper = deaths_upper_rate) %>% 
  mutate(metric_category = "deaths per 100,000 persons")

plot_data <- rbind(plot_data_cases, plot_data_deaths)


#plot 1st tertile
plot_data_1 <- plot_data %>% filter(gdp_tertile == 1)
plot_1 <- ggplot(plot_data_1, aes(year, point, color = country, fill = country)) +
  geom_line(size = 0.5) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  theme_bw() +
  theme(legend.title=element_blank(),
        axis.title.y = element_blank()) +
  facet_grid(vars(metric_category), vars(gdp_tertile), scales = "free",
             labeller = labeller(gdp_tertile = gdp.labs)) + 
  theme(strip.text = element_text(size = 10)); plot_1


select_coutries_1 <- c(
  #tertile 1, higher funding:
  "Liberia",
  #tertile 1, lower funding:
  "Niger")
plot_data_1.1 <- plot_data %>% filter(country %in% select_coutries_1)
plot_1.1 <- ggplot(plot_data_1.1, aes(year, point, color = country, fill = country)) +
  geom_line(size = 0.5) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  theme_bw() +
  theme(legend.title=element_blank(),
        axis.title.y = element_blank()) +
  facet_grid(vars(metric_category), vars(gdp_tertile), scales = "free",
             labeller = labeller(gdp_tertile = gdp.labs)) + 
  theme(strip.text = element_text(size = 10)); plot_1.1




#plot 2nd tertile
plot_data_2 <- plot_data %>% filter(gdp_tertile == 2) %>% 
  #remove d/t huge peak in cases makes hard to compare
  filter(country!= "Rwanda")
plot_2 <- ggplot(plot_data_2, aes(year, point, color = country, fill = country)) +
  geom_line(size = 0.5) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  theme_bw() +
  theme(legend.title=element_blank(),
        axis.title.y = element_blank()) +
  facet_grid(vars(metric_category), vars(gdp_tertile), scales = "free",           #ignore facet labels
             labeller = labeller(gdp_tertile = gdp.labs)) + 
  theme(strip.text = element_text(size = 10)); plot_2


select_coutries_2 <- c(
  #tertile 2, higher funding:
  "Cameroon",
  #tertile 2, lower funding:
  "Chad")
plot_data_2.1 <- plot_data %>% filter(country %in% select_coutries_2)
plot_2.1 <- ggplot(plot_data_2.1, aes(year, point, color = country, fill = country)) +
  geom_line(size = 0.5) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  theme_bw() +
  theme(legend.title=element_blank(),
        axis.title.y = element_blank()) +
  facet_grid(vars(metric_category), vars(gdp_tertile), scales = "free",            #ignore facet labels
             labeller = labeller(gdp_tertile = gdp.labs)) + 
  theme(strip.text = element_text(size = 10)); plot_2.1



#plot 3rd tertile
plot_data_3 <- plot_data %>% filter(gdp_tertile == 3) %>% 
  #remove countries from regions already covered
  #filter(country!="Côte dʼIvoire", country!="Mauritania")
plot_3 <- ggplot(plot_data_3, aes(year, point, color = country, fill = country)) +
  geom_line(size = 0.5) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  theme_bw() +
  theme(legend.title=element_blank(),
        axis.title.y = element_blank()) +
  facet_grid(vars(metric_category), vars(gdp_tertile), scales = "free",           #ignore facet labels
             labeller = labeller(gdp_tertile = gdp.labs)) + 
  theme(strip.text = element_text(size = 10)); plot_3


select_coutries_3 <- c(
  #tertile 3, higher funding:
  "Congo",
  #tertile 3, lower funding:
  "Angola")
plot_data_3.1 <- plot_data %>% filter(country %in% select_coutries_3)
plot_3.1 <- ggplot(plot_data_3.1, aes(year, point, color = country, fill = country)) +
  geom_line(size = 0.5) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  theme_bw() +
  theme(legend.title=element_blank(),
        axis.title.y = element_blank()) +
  facet_grid(vars(metric_category), vars(gdp_tertile), scales = "free",            #ignore facet labels
             labeller = labeller(gdp_tertile = gdp.labs)) + 
  theme(strip.text = element_text(size = 10)); plot_3.1






select_coutries <- c(
  #tertile 1, higher funding:
  "Liberia",
  #tertile 1, lower funding:
  "Niger",
  #tertile 2, higher funding:
  "Cameroon",
  #terile 2, lower funding:
  "Chad",
  #tertile 3, higher funding:
  "Congo",
  #tertile 3, lower funding:
  "Angola"
)


# New facet labels
gdp.labs <- c("1st tertile GDP: 
                 higher per capita malaria spending: Liberia 
                 lower per capita malaria spending: Niger", 
              "2nd tertile GDP: 
                 higher per capita malaria spending: Cameroon 
                 lower per capita malaria spending: Chad",
              "3rd tertile GDP: 
                 higher per capita malaria spending: Congo
                 lower per capita malaria spending: Angola")
names(gdp.labs) <- c("1", "2", "3")

plot_data_combined <- plot_data %>% filter(country %in% select_coutries)
plot <- ggplot(plot_data_combined, aes(year, point, color = country, fill = country)) +
  geom_line(size = 0.5) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  theme_bw() +
  theme(legend.title=element_blank(),
        axis.title.y = element_blank()) +
  facet_grid(vars(metric_category), vars(gdp_tertile), scales = "free",
             labeller = labeller(gdp_tertile = gdp.labs)) + 
  theme(strip.text = element_text(size = 10)); plot

