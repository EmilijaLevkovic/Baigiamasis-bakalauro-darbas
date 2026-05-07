library(dplyr)
library(leaflet)
library(ggplot2)
library(readxl)
library(sf)
library(lubridate)
library(tidytext)
library(tidyverse)
library(scales)
library(gridExtra)
library(shiny)
library(stringr)
library(DT)
library(forecast)

getwd()
setwd("C:/Users/emili/Desktop/Bakalauro darbas/interaktyvus")
#Duomenys:
mokyklos <- read_excel("C:/Users/emili/Desktop/Bakalauro darbas/duomenys.xlsx", 
                       sheet = "Patekimas į mokyklas")
Mok_koord <- read.csv("C:/Users/emili/Desktop/Bakalauro darbas/Mokyklų koordinatės.csv")
Mok_koord<-na.omit(Mok_koord)
prasymai <- read.csv("C:/Users/emili/Desktop/Bakalauro darbas/Prašymai į mokyklas.csv")
prasymai <- prasymai %>%mutate(metai = year(as.Date(desired_start_date)))%>%filter(metai!=2026)
mokiniu_sk <- read_excel("C:/Users/emili/Desktop/Bakalauro darbas/duomenys.xlsx", 
                         sheet = "Mokinių sk.", range = "A1:R13", col_types = ("numeric"))
duomenys <- read_excel("C:/Users/emili/Desktop/Bakalauro darbas/duomenys.xlsx", 
                       sheet = "Duomenys metiniai")

#### Mokyklų žemėlapis ####
Mok_koord<-Mok_koord%>% filter (MOKSSK!=0)
koord <- Mok_koord%>% st_as_sf(coords = c("X", "Y"), crs = 3346)%>% st_transform(4326) 
leaflet(koord)%>% addTiles()%>% addCircleMarkers(radius = ~sqrt(MOKSSK) / 5, 
                                                 popup = ~paste0("<b>", VARDAS, "</b><br>",
                                                                 "Mokinių sk.: ", MOKSSK),
                                                 color = "#78003F",fillOpacity = 0.7)

#### Patekimas i mokyklas 2026 metais vertinant klases ####
pagal_klase <- mokyklos%>% group_by(Klasė)%>%
  summarise(`Laisvų vietų skaičius` = sum(`Laisvų vietų skaičius`),
            `Pateiktų prašymų skaičius` = sum(`Pateikta prašymų iš viso`))

galutine_long <- pagal_klase%>% pivot_longer(cols = c(`Laisvų vietų skaičius`, `Pateiktų prašymų skaičius`), 
               names_to = "Tipas", values_to = "Skaičius")

ggplot(galutine_long, aes(x = factor(Klasė), y = Skaičius, fill = Tipas)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Klasė",y = "Kiekis", fill = "") +
  scale_y_continuous(labels = scales::label_number(big.mark = ' '))+
    scale_fill_manual(values = c("Laisvų vietų skaičius" = "#E64164",
      "Pateiktų prašymų skaičius" = "#78003F")) +
  theme_minimal()+
  theme(axis.text = element_text(size = 12),axis.title = element_text(size = 12),
        legend.text = element_text(size = 12),legend.title = element_text(size = 12))


#### Mokyklų tipas, kalba ####
Tipas <- Mok_koord%>% mutate(TIPAS_GR = case_when(
    TIPAST %in% c("DARŽELIAI - MOKYKLOS") ~ "Darželiai-mokyklos",
    TIPAST %in% c("PRADINĖS MOKYKLOS") ~ "Pradinės mokyklos",
    TIPAST %in% c("PAGRINDINĖS MOKYKLOS") ~ "Pagrindinės mokyklos",
    TIPAST %in% c("PROGIMNAZIJOS") ~ "Progimnazijos",
    TIPAST %in% c("GIMNAZIJOS") ~ "Gimnazijos",
    TIPAST %in% c("VIDURINĖS MOKYKLOS") ~ "Vidurinės mokyklos",
    TRUE ~ "Spec. mokyklos ir Kita"))%>% count(TIPAS_GR, name = "KIEKIS")%>%
  mutate(Procentas = KIEKIS / sum(KIEKIS), Legenda = paste0(TIPAS_GR, " (", percent(Procentas), ")"))

tipas<-ggplot(Tipas, aes(x = "", y =KIEKIS, fill = Legenda)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  geom_text(aes(label = ifelse(Procentas >= 0.03, percent(Procentas),"")),
            position = position_stack(vjust = 0.5), color = "white",size = 3) +
  scale_fill_manual(values = c('#78003F', '#D3597B', '#E64164','#414141', '#8F8F8F','#DCDCDC','black'))+
  labs(fill = "Grupė", title = "Įstaigų pasiskirstymas pagal grupę") +
  theme_void()+
  theme(plot.title = element_text(hjust = 0.5))
tipas

Kalba <- Mok_koord%>% mutate(KALBA_GR = case_when(
  KALBA %in% c("lietuvių") ~ "Lietuvių",
  KALBA %in% c("rusų") ~ "Rusų",
  KALBA %in% c("lenkų") ~ "Lenkų",
  KALBA %in% c("prancūzų") ~ "Prancūzų",
  TRUE ~ "Mišrios"))%>%count(KALBA_GR, name = "KIEKIS")%>%
  mutate(Procentas = KIEKIS / sum(KIEKIS), Legenda = paste0(KALBA_GR, " (", percent(Procentas), ")"))

kalba<-ggplot(Kalba, aes(x = "", y =KIEKIS, fill = Legenda)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  geom_text(aes(label = ifelse(Procentas >= 0.05,percent(Procentas),"")),
            position = position_stack(vjust = 0.5), color = "white",size = 3) +
  scale_fill_manual(values = c('#78003F', '#D3597B', '#E64164','#414141', '#8F8F8F'))+
  labs(fill = "Kalba", title = "Įstaigų pasiskirstymas pagal mokyklos kalbą") +
  theme_void()+
  theme(plot.title = element_text(hjust = 0.5))
kalba
grid.arrange(tipas, kalba, ncol=1)

#### Prašymai į mokyklas ####
prasymai<- prasymai %>%mutate(institution_eldership = ifelse(institution_eldership == "", "Nepriskirta", institution_eldership))
ui <- fluidPage(titlePanel("Prašymai į mokyklas"),
  fluidRow(column(2,selectInput("metai","Pasirinkite metus:",
                       choices = sort(unique(prasymai$metai)),
                       selected = max(prasymai$metai))),
    column(10,fluidRow(column(4, plotOutput("pie_plot")),
             column(8, plotOutput("bar_plot"))))))

server <- function(input, output, session){
  filtered_data <- reactive({prasymai %>%filter(metai == input$metai)})
  output$pie_plot <- renderPlot({
    df <- filtered_data() %>%
      count(study_level, name = "kiekis") %>%
      mutate(study_level = as.factor(study_level),
             proc = kiekis / sum(kiekis),
             label = percent(proc, accuracy = 0.1),
             legenda = paste0(study_level, " (", label, ")"))
    
    ggplot(df, aes(x = "", y = proc, fill = legenda)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y") +
      geom_text(aes(label = ifelse(proc >= 0.05, label, "")),
                position = position_stack(vjust = 0.5),
                color = "white",size = 4) +
      theme_void() +
      labs(title = "Prašymai pagal klases",fill = "Klasė") +
      theme(plot.title = element_text(hjust = 0.5))})

  output$bar_plot <- renderPlot({
    df <- filtered_data() %>%
      count(institution_eldership, name = "kiekis") %>%
      arrange(desc(kiekis))
    
    ggplot(df,aes(x = reorder(institution_eldership, kiekis),y = kiekis)) +
      geom_col(fill = "#78003F") +
      coord_flip() +
      labs(title = "Prašymai pagal seniūniją",
           x = "Seniūnija",y = "Prašymų skaičius") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))})}

shinyApp(ui, server)


#### Prasymai pagal klases ####
klases_metai <- prasymai%>% group_by(metai, study_level)%>%
  summarise(kiekis = n(), .groups = "drop")%>% group_by(metai) %>%
  mutate(study_level = as.factor(study_level),
         proc = kiekis / sum(kiekis),label = percent(proc, accuracy = 0.1))

skaicius<-ggplot(klases_metai,aes(x = metai,y = kiekis, color= study_level)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq(2017, 2025, by = 1))+
  scale_y_continuous(labels = scales::label_number(big.mark = " "))+
  labs(title = "",x = "",y = "Prašymų skaičius", color = "Klasė") +
  theme_minimal()

procentas<-ggplot(klases_metai,aes(x = metai,y = proc, color= study_level)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq(2017, 2025, by = 1))+
  scale_y_continuous(labels = scales::label_percent(accuracy = 1),
      breaks = seq(0, 1, by = 0.05)) +
  labs(title = "",x = "Metai",y = "Prašymų procentas", color = "Klasė") +
  theme_minimal()+
  theme(legend.position = "none")

grid.arrange(skaicius, procentas, nrow=2)


#### Vidutinis prašymų skaičius kiekvienoje seniūnijoje ####
vidurkis_seniuniju <- prasymai%>% group_by(metai, institution_eldership)%>%
  summarise(kiekis = n(), .groups = "drop")%>% group_by(institution_eldership) %>%
  summarise(Vidutinis_prasymu_skaicius = round(mean(kiekis), 0),.groups = "drop") %>%
  arrange(desc(Vidutinis_prasymu_skaicius))

istaigu_skaicius <- prasymai%>% distinct(institution_eldership, institution_name)%>%
  group_by(institution_eldership)%>% summarise(Istaigu_skaicius = n(),.groups = "drop")

vidutiniai_prasymai <- vidurkis_seniuniju%>%
  left_join(istaigu_skaicius, by = "institution_eldership")%>%
  arrange(desc(Vidutinis_prasymu_skaicius))%>%
  mutate(Vidutinis_vienai_mokyklai = round(Vidutinis_prasymu_skaicius / Istaigu_skaicius, 1))


#### Gimusių sk. vs 1 klases mokiniu sk.####
klase1 <- mokiniu_sk%>% filter(Klasė == 1)%>% 
  pivot_longer(cols = -Klasė,names_to = "mokslo_metai", values_to = "mokiniu_sk")%>%
  mutate(metai = as.numeric(str_sub(mokslo_metai, 1, 4)), gimimo_metai = metai - 7)%>%
  select(mokslo_metai, mokiniu_sk, gimimo_metai)
klase1

gyv_seniun <- read.csv("C:/Users/emili/Desktop/Bakalauro darbas/gyventojai seniūnijose.csv")
gimimai <- gyv_seniun%>% group_by(birth_year)%>%
  summarise(vaiku_sk = n())%>% rename(`gimimo_metai` = birth_year)

klase1 <- klase1%>% left_join(gimimai, by = "gimimo_metai")%>%
  mutate(dalis = (0.94*mokiniu_sk) / vaiku_sk)%>%
  select(mokslo_metai, mokiniu_sk, vaiku_sk, dalis)
klase1 <- klase1%>% mutate(nubyrejimas = 1-dalis)%>%
  select(mokslo_metai, mokiniu_sk, vaiku_sk, dalis,nubyrejimas)
klase1
#### Gimusių sk. vs  12 klases mokiniu sk.####
#### Mokinių atkritimas į pirmą klasę####
klase12 <- mokiniu_sk%>% filter(Klasė == 12)%>% 
  pivot_longer(cols = -Klasė,names_to = "mokslo_metai", values_to = "mokiniu_sk")%>%
  mutate(metai = as.numeric(str_sub(mokslo_metai, 1, 4)), gimimo_metai = metai - 18)%>%
  select(mokslo_metai, mokiniu_sk, gimimo_metai)
klase12

klase12 <- klase12%>% left_join(gimimai, by = "gimimo_metai")%>%
  mutate(dalis = (0.89*mokiniu_sk)/vaiku_sk)%>%
  select(mokslo_metai, mokiniu_sk, vaiku_sk, dalis )
klase12 <- klase12%>% mutate(nubyrejimas = 1-dalis)%>%
  select(mokslo_metai, mokiniu_sk, vaiku_sk, dalis,nubyrejimas)
klase12
#2026 metais (2025-2026m.): 2007-19m.baigs 12klase. 2008- 18m. baigs 12klase



#### Problemu identifikavimas####
unique(gyv_seniun$eldership_name)

####Prognozė####
births <- gyv_seniun%>% filter(birth_year>1999)%>% filter(birth_year<2026)%>%
  group_by(eldership_name, birth_year)%>%
  summarise(births = n(), .groups = "drop")

rmse <- function(actual, predicted){
  sqrt(mean((actual - predicted)^2))}

evaluate_model <- function(data, p, d, q){
  actual_all <- c()
  pred_all   <- c()
  groups <- data %>% group_split(eldership_name)
  for(df in groups){
    df <- df %>% arrange(birth_year)
    n <- nrow(df)
    train_size <- floor(0.8 * n)
    train <- df[1:train_size, ]
    test  <- df[(train_size + 1):n, ]
    ts_train <- ts(train$births, start = min(train$birth_year), frequency = 1)
    model <- tryCatch(
      Arima(ts_train, order = c(p,d,q)),
      error = function(e) NULL)
    if(is.null(model)) next
    fc <- forecast(model, h = nrow(test))
    actual_all <- c(actual_all, test$births)
    pred_all   <- c(pred_all, as.numeric(fc$mean))}
  return(rmse(actual_all, pred_all))}
results <- data.frame()
for(p in 0:3){
  for(d in c(0,2)){
    for(q in 0:3){error <- evaluate_model(births, p, d, q)
      results <- rbind(results, data.frame(p=p, d=d, q=q, RMSE=error))}}}

results %>% arrange(RMSE) %>% head()
results <- results %>% arrange(RMSE)
(best_model <- results %>% arrange(RMSE) %>% slice(1))
p <- best_model$p
d <- best_model$d
q <- best_model$q

forecast_births_best <- function(df, p, d, q){
  df <- df %>% arrange(birth_year)
  ts_data <- ts(df$births,start = 2000,frequency = 1)
  model <- Arima(ts_data, order = c(p,d,q))
  fc <- forecast(model, h = 5)
  data.frame(birth_year = max(df$birth_year) + 1:5,
    births = round(as.numeric(fc$mean)))}

results <- births %>% group_by(eldership_name) %>%
  group_modify(~forecast_births_best(.x, p, d, q))

gimusieji <- bind_rows(births, results)%>%
  arrange(eldership_name, birth_year)

####UI#####
skaiciuoti_klases <- function(duomenys, metai) {
  metai <- as.numeric(metai)
  duomenys%>%
    mutate(eldership_name = ifelse(eldership_name == "", "Nepriskirta", eldership_name))%>%
    group_by(Seniūnija = eldership_name)%>%
    summarise(`Gimę į 1-mą klasę` = sum(births[birth_year == (metai - 8)], na.rm = TRUE),
              `Gimę į 12-tą klasę` =sum(births[birth_year == (metai - 20)], na.rm = TRUE),
              `Mokiniai 1 klasė` = round(`Gimę į 1-mą klasę` * 0.92, 0),
              `Mokiniai 12 klasė` = round(`Gimę į 12-tą klasę` * 0.64, 0),
              `Skirtumas` = round(`Mokiniai 12 klasė` - `Mokiniai 1 klasė`, 0))%>%
    arrange(Seniūnija)}

ui <- fluidPage(titlePanel("Prognozuojamas mokinių skaičius pagal seniūnijas"),
  sidebarLayout(sidebarPanel(selectInput("metai","Pasirinkite mokslo metus",
        choices = setNames(2027:2038,paste0(2026:2037, "-", 2027:2038)),selected = 2027)),
    mainPanel(h3("Gimusių vaikų ir prognozuojamo mokinių skaičiaus lentelė"),
      DTOutput("lentele"))))

server <- function(input, output){
  rezultatai <- reactive({skaiciuoti_klases(gimusieji, input$metai)})
  output$lentele <- renderDT({
    datatable(as.data.frame(rezultatai()), options = list(pageLength = -1, dom = 't'))})}

shinyApp(ui, server)

metai_vec <- 2027:2038
visu_metai <- lapply(metai_vec, function(m) {
  df <- skaiciuoti_klases(gimusieji, m)
  data.frame(Seniūnija = df$Seniūnija, Skirtumas = df$Skirtumas, Metai = m)}) %>%
  bind_rows() %>%
  mutate(Spalva = ifelse(Skirtumas >= 0, "Teigiamas", "Neigiamas")) %>%
  filter(Seniūnija != "Nepriskirta")

ggplot(visu_metai, aes(x = Metai, y = Skirtumas, fill = Spalva)) +
  geom_col() +
  scale_fill_manual(values = c("Teigiamas" = "palegreen3", "Neigiamas" = "firebrick2")) +
  scale_x_continuous(breaks = 2027:2038, labels = paste0(2026:2037, "-", 2027:2038)) +
  theme_minimal() +
  labs(x = "Mokslo metai", y = "Pirmų klasių laisvų vietų skaičius") +
  facet_wrap(~Seniūnija, scales = "free_y", ncol = 3) +
  theme(legend.position = "none",
    panel.border = element_rect(colour = "black", fill = NA, size = 0.8),
    strip.text = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
