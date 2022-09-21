######################################################################
## Descripción: Tidy Challenge
## Autor: Sharon Trejo
## Fecha: 28/ 02 / 2022
######################################################################

# Load Libraries ----------------------------------------------------------

pacman::p_load(tidyverse, magrittr, janitor, viridis, hrbrthemes, forcats)
library(ggalluvial)
library(lubridate)
library(zoo)
library(stargazer)

options(pillar.print_min = 20, pillar.print_max = 35)

# Load Data  --------------------------------------------------------------

colony_w_usa <- readr::read_csv(file = "~/Documents/2022/LQL-tidyChallenge/data/01_colony.csv")

stressor_w_usa <- readr::read_csv(file = "~/Documents/2022/LQL-tidyChallenge/data/02_stressor.csv")

colony_w_usa %>% glimpse()

stressor_w_usa %>% glimpse()

colony <- colony_w_usa %>% filter(state != "United States")

stressor <- stressor_w_usa %>% filter(state != "United States")

colony %>% glimpse()

stressor %>% glimpse()

# Agregar los datos por año

colony_yearly <- colony %>%
  group_by(year, state) %>%
  summarise(across(!months, ~mean(., na.rm = TRUE)))

colony_avg <- colony %>% 
  group_by(state) %>% 
  summarise(across(starts_with("colony"), ~mean(., na.rm = TRUE)))


# 1. ¿Cuáles son los 10 estados con más colonias de abejas? ---------------

# Colony_max: corresponde al numero de colonias al inicio del periodo más
#             todas las colonias que se movieron al Estado durante el trimestre.

# 1.1 Por año ---------------------------------------------------------------

top_n <- colony_yearly %>%
  select(year, state, colony_max) %>%
  group_by(year)%>%
  slice_max(colony_max, n = 10) %>%
  arrange(year, desc(colony_max))

top_plot <- top_n %>%
  group_by(year)%>%
  mutate(state = fct_reorder2(state,year, colony_max)) %>%
  ggplot(aes(fill=state, y=colony_max, x=state)) +
  geom_bar(position="dodge", stat="identity") +
  coord_flip()+
  # scale_fill_viridis(discrete = T, option = "E") +
  ggtitle("Top States per year") +
  facet_wrap(~year, scale = "free_y")+
  theme_ipsum()+
  theme(
    legend.position="none",
    panel.spacing = unit(0, "lines"),
    strip.text.x = element_text(size = 8),
    plot.title = element_text(size=13)) +
  xlab("")

print(top_plot)


# 1.1.1 Sankey chart ---------------------------------------------------------

time_period <- sort(rep(c(2015,2016,2017,2018,2019,2020,2021),10))

top_id <- top_n %>%
  arrange(year, desc(colony_max)) %>%
  group_by(year) %>%
  mutate(id = row_number()) %>%
  arrange(year)

id <-  top_id$id

state <- top_id$state

data <- data.frame(time_period, id = as.factor(id), state = state)

sankey_top <- ggplot(data, aes(x = time_period, stratum = id, alluvium = state, fill = state, label = id)) +
  geom_flow(stat = "alluvium", lode.guidance = "rightleft", color = "darkgray") +
  geom_stratum() +
  theme_ipsum()+
  theme(legend.position = "bottom") +
  ggtitle("Top states by colonies across the time")

print(sankey_top)

# 1.2 Promedio de todos los años ------------------------------------------
top_n_avg <- colony_avg %>%
  select(state, colony_max) %>%
  slice_max(colony_max, n = 10) %>%
  arrange(desc(colony_max))

(top_avg_plot <- top_n_avg %>%
  mutate(state = fct_reorder(state, colony_max)) %>%
  ggplot(aes(fill=state, y=colony_max, x=state)) +
  geom_bar(position="dodge", stat="identity") +
  # scale_fill_viridis(discrete = T, option = "E") +
  ggtitle("Top States")+
  coord_flip()+
  theme(legend.position = "none")+
  xlab("")+
  theme_ipsum())


# 2. ¿Cuáles son los 10 estados con mayores pérdidas? ---------------------

# Para obtener los Estados con más pérdidas usaremos las pérdidas porcentuales
# que están dadas por la variable colony_lost_pct, definida como:

# colony_lost_pct = 100* (colony_lost / colony_max)

# 2.1 Estados con mayores pérdidas por año --------------------------------

anual_lost <- colony_yearly %>%
  select(year, state, colony_lost) %>%
  group_by(year)%>%
  slice_max(colony_lost, n = 10) %>%
  arrange(year, desc(colony_lost))

(anual_lost_plot <- anual_lost %>%
  filter(state != "Other States") %>%
  mutate(state = fct_reorder2(state,year, colony_lost, .desc = FALSE)) %>%
  ggplot(aes(fill=state, y=colony_lost, x=state)) +
  geom_bar(position="dodge", stat="identity") +
  coord_flip()+
  ggtitle("States with top loss rate per year") +
  theme_ipsum()+
  facet_wrap(~year, scale = "free_y")+
  theme(
    legend.position="none",
    panel.spacing = unit(0, "lines"),
    strip.text.x = element_text(size = 8),
    plot.title = element_text(size=13) ) +
  ylab(""))



# 2.1.1 Sankey plot -------------------------------------------------------

lost_id <- anual_lost %>%
  arrange(year, desc(colony_lost)) %>%
  group_by(year) %>%
  mutate(id = row_number()) %>%
  arrange(year)

time_period <- lost_id$year

id <-  lost_id$id

state <- lost_id$state

data <- data.frame(time_period, id = as.factor(id), state = state)

(sankey_lost <- ggplot(data, aes(x = time_period, stratum = id, alluvium = state, fill = state, label = id)) +
  geom_flow(stat = "alluvium", lode.guidance = "rightleft", color = "darkgray") +
  geom_stratum() +
  theme_ipsum()+
  theme(legend.position = "bottom") +
  ggtitle("Top states by losses across the time"))


# 2.2 Top 10 estados con mayores pérdidas promedio --------------------------

lost_avg <- colony_avg %>%
  select(state, colony_lost) %>%
  filter(state != "United States") %>%
  slice_max(colony_lost, n = 10) %>%
  arrange(desc(colony_lost))

(top_avg_plot <- lost_avg %>%
  mutate(state = fct_reorder(state, colony_lost)) %>%
  ggplot(aes(fill=state, y=colony_lost, x=state)) +
  geom_bar(position="dodge", stat="identity") +
  # scale_fill_viridis(discrete = T, option = "E") +
  ggtitle("Top States: Losses")+
  coord_flip()+
  theme(legend.position = "none")+
  xlab("")+
  ylab("Colony lost")+
  theme_ipsum())


# 3.  Estado con mayores pérdidas de colonias -----------------------------

# Por incisos anteriores sabemos que el Estado con mayores pérdias fue California

max_lost <- colony %>%
  filter(state == "California" ) %>%
  select(year,months,state,colony_lost)

others_lost <- colony %>%
  group_by(year, months) %>% 
  summarise(colony_lost = mean(colony_lost, na.rm = TRUE)) %>% 
  mutate(state = "Average") %>%
  select(year, months, state, colony_lost) %>%
  mutate_all(~replace(., is.nan(.), NA))
  

data_aux <- rbind(max_lost, others_lost)

data_aux <- data_aux %>%
  mutate(yr_qrt = as.yearqtr(paste0(year,"-",
                                    match(sub("\\-.*", "", months), month.name)),
                             format = "%Y-%m"))

data_aux %>% ggplot(aes(x = yr_qrt, y = colony_lost, col= state))+
  geom_line()+
  xlab("Year-Quarter")+
  ggtitle("California Losses vs. USA average colony losses")+
  theme_ipsum()


# 4.  Estado con mayores incrementos de colonias --------------------------

colony_growth <- colony %>% 
  group_by(year, state) %>% 
  summarise(colony_added = mean(colony_added, na.rm = TRUE)) %>%
  ungroup() %>% 
  filter(state != "United States") %>%
  arrange(year, desc(colony_added)) %>%
  group_by(year)


pander::pander(colony_growth %>% slice_max(colony_added, n = 1))

# Podemos ver que cada año el Estado con más incrementos es California.

max_inc <- colony %>%
  filter(state == "California") %>%
  select(year, months, state, colony_added)

others_inc <- colony %>% 
  group_by(year, months) %>% 
  summarise(colony_added = mean(colony_added, na.rm = TRUE)) %>% 
  mutate(state = "Average") %>%
  select(year, months, state, colony_added) %>%
  mutate_all(~replace(., is.nan(.), NA))

data_inc <- rbind(max_inc, others_inc)

data_inc <- data_inc %>%
  mutate(yr_qrt = as.yearqtr(paste0(year,"-",
                                    match(sub("\\-.*", "", months), month.name)),
                             format = "%Y-%m"))

data_inc %>% ggplot(aes( x = yr_qrt, y = colony_added, col = state))+
  geom_line()+
  xlab("Year - Quarter")+
  ggtitle("California Colony Increase vs USA average ")+
  theme_ipsum()



# 5. ¿Cuál es el stressor que ocasiona mayores pérdidas? ------------------

stressor_anual <- stressor %>% 
  group_by(year, stressor) %>% 
  summarise(stress_pct = mean(stress_pct, na.rm = TRUE)) %>% 
  ungroup() %>% 
  arrange(year,desc(stress_pct)) %>%
  group_by(year) %>%
  slice_max(stress_pct, n = 1)
  

pander::pander(stressor_anual)


# Tomando en cuenta lo anterior podemos decir que el stressor que ocasiona
# mayores pérdidas son las Varroa Mites.


# 6.  ¿Qué región es la más propicia para las colonias de abejas? ---------
# Usando la clasificación por region que se encuentra en la carpeta cache

# Podríamos decir que la región más propicia para las abejas es aquella en la que
# hay menor porcentaje de estrés

regions <- readr::read_csv(file = "~/Documents/2022/LQL-tidyChallenge/cache/regions.csv")

colony_ext <- colony %>%
  inner_join(regions, by = "state") %>%
  group_by(year,region)


stressor_ext <- stressor %>%
  inner_join(regions, by = "state") %>%
  group_by(region,year) %>%
  summarise_at(vars(stress_pct),mean, na.rm = TRUE ) %>%
  arrange(year,stress_pct)


pander::pander(stressor_ext
               %>% slice_min(stress_pct, n = 1) %>%
                 select(year, everything()))

aux <- stressor_ext %>%
  arrange(year,stress_pct) %>%
  group_by(year) %>%
  mutate(id = row_number()) %>%
  mutate(id = ifelse(id == 1 , 1, 0)) %>%
  arrange(year,region)

# Increase bottom margin
par(mar=c(6,5,5,5))

bar_plot <- barplot(aux$stress_pct, border=F , names.arg=aux$region ,
                  las=2 ,
                  col= ifelse(aux$id == 1, "#76D7C4", "#E5E8E8") ,
                  main="Stressor % per region ")

# Add abline
abline(v=c(6.1, 12.1, 18.1,24.1,30.1,36.1) , col="black")

# Add the text
text(bar_plot, aux$stress_pct/2, paste(round(aux$stress_pct,2),"%", sep="") ,cex=.5, srt = 90)
text(3, max(aux$stress_pct)-1, c("2015"))
text(9, max(aux$stress_pct)-1, c("2016"))
text(15, max(aux$stress_pct)-1, c("2017"))
text(21, max(aux$stress_pct)-1, c("2018"))
text(27, max(aux$stress_pct)-1, c("2019"))
text(33, max(aux$stress_pct)-1, c("2020"))
text(39, max(aux$stress_pct)-1, c("2021"))

# Not yearly

stressor_ext %>%
  group_by(region) %>%
  summarise_at(vars(stress_pct), mean, na.rm = TRUE) %>%
  arrange(stress_pct)

# Entonces la mejor región sería el Noreste.


# 7.  Datos faltantes  ----------------------------------------------------

# Yo le daría un tratamiento a los NA's de acuerdo a la variable, por ejemplo,
#  para las variables de colony_reno y colony_reno_pct les asignaria el valor de
# cero, esto siempre y cuando las demás columnas tengan valores distintos de NA.

# En los casos en los que todo el renglón es NA, utilizaría algun metodo de
# interpolación para asignarle un valor a los datos. Considerando la naturaleza
# de los datos, es muy difícil que no hubiera colonias de abeja más bien pareciera
# que ese periodo no hubo toma de datos, por lo que asignar ceros a los NA no ç
# parece una buena opción.








