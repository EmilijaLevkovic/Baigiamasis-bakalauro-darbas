#Naudojamos bibliotekos
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)
library(readr)
library(ggrepel)
library(writexl)
library(stringr)
library(gridExtra)
library(shiny)
library(plotly)
library(scales)

duomenys <- read_excel('C:/Users/emili/Desktop/Bakalauro darbas/duomenys.xlsx', sheet = 'Duomenys metiniai bakalaurui')
str(duomenys)

#### 1 grafikas: Gimstamumas ir mirtingumas pagal metus ####
ggplot(duomenys, aes(x = Metai)) +
  geom_bar(aes(y = `Gimusių kūdikių sk.`, fill = 'Gyvų gimusių kūdikių sk.'), stat = 'identity', show.legend = TRUE) +
  geom_line(aes(y = `Mirusių sk.`, color = 'Mirusių žmonių sk.'),
            linewidth = 1,, group = 1, show.legend = TRUE) +
  geom_point(aes(y = `Mirusių sk.`), color = '#E64164', size = 2) +
  scale_fill_manual(name='',values = c('Gyvų gimusių kūdikių sk.' = '#78003F')) +
  scale_color_manual(name='',values = c('Mirusių žmonių sk.' = '#E64164')) +
  scale_y_continuous(labels = scales::label_number(big.mark = ' '))+
  scale_x_continuous(breaks = seq(1996, 2025, by = 2))+
  labs(x = 'Metai',y = 'Asmenys') +
  guides(fill = guide_legend(override.aes = list(color = NA)),
         color = guide_legend(override.aes = list(fill = NA))) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        legend.position = 'bottom',legend.direction = 'horizontal',
        axis.text = element_text(size = 12),axis.title = element_text(size = 12),
        legend.text = element_text(size = 12),legend.title = element_text(size = 12))


#### 2 grafikas: Senatves koeficientai ####
ggplot(duomenys%>% filter(Metai >= 2001), aes(x = Metai)) +
  geom_bar(aes(y =  `Senatvės koeficientas (viso)`, fill = 'Bendras senatvės koeficientas'), stat = 'identity', show.legend = TRUE) +
  geom_line(aes(y = `Senatvės koeficientas vyrai`, color = 'Vyrų koeficientas'),linewidth = 1) +
  geom_point(aes(y = `Senatvės koeficientas vyrai`, color = 'Vyrų koeficientas'),size = 2) +
  geom_line(aes(y = `Senatvės koeficientas moterys`, color = 'Moterų koeficientas'),linewidth = 1) +
  geom_point(aes(y = `Senatvės koeficientas moterys`, color = 'Moterų koeficientas'), size = 2) +
  scale_fill_manual(values = c('Bendras senatvės koeficientas' = '#78003F')) +
  scale_color_manual(values = c('Vyrų koeficientas' = '#8F8F8F','Moterų koeficientas' = '#E64164')) +
  scale_x_continuous(breaks = seq(2001, 2025, by = 2))+
  labs(title = '',x = 'Metai',y = 'Senatvės koeficientas',fill = '',color = '') +
  guides(fill = guide_legend(override.aes = list(color = NA)),
         color = guide_legend(override.aes = list(fill = NA))) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        legend.position = 'bottom',legend.direction = 'horizontal',
        axis.text = element_text(size = 12),axis.title = element_text(size = 12),
        legend.text = element_text(size = 12),legend.title = element_text(size = 12))


#### 3 grafikas: Migracija ####
imigrantai <- ggplot(duomenys,aes(x = Metai,y = `Atvykusieji ir imigrantai`)) +
  geom_bar(stat = 'identity',fill = '#78003F') +
  labs(title = 'Atvykusieji ir imigrantai pagal metus',x = '',y = 'Asmenys') +
  scale_y_continuous(labels = scales::label_number(big.mark = ' '))+
  scale_x_continuous(breaks = seq(1994, 2025, by = 2))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.text = element_text(size = 12),axis.title = element_text(size = 12),
        legend.text = element_text(size = 12),legend.title = element_text(size = 12))
imigrantai
emigrantai <- ggplot(duomenys,aes(x = Metai,y = `Išvykusieji ir emigrantai`)) +
  geom_bar(stat = 'identity',fill = '#78003F') +
  labs(title = 'Išvykusieji ir emigrantai pagal metus',x = 'Metai',y = 'Asmenys') +
  scale_y_continuous(labels = scales::label_number(big.mark = ' '))+
  scale_x_continuous(breaks = seq(1994, 2025, by = 2))+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.text = element_text(size = 12),axis.title = element_text(size = 12),
        legend.text = element_text(size = 12),legend.title = element_text(size = 12))
emigrantai
kaita<-ggplot(duomenys,aes(x = Metai,y = `Neto migracija`)) +
  geom_line(color = '#78003F',linewidth = 1) +
  geom_point(color = '#78003F',size = 2) +
  geom_hline(yintercept = 0,color = 'grey50',linewidth = 0.7) +
  scale_y_continuous(breaks = seq(-10000, 30000, by = 5000),
    labels = scales::label_number(big.mark = ' '))+
  scale_x_continuous(breaks = seq(1994, 2025, by = 2))+
  labs(title = 'Neto migracija',x = '',y = 'Asmenys') +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        axis.text = element_text(size = 12),axis.title = element_text(size = 12),
        legend.text = element_text(size = 12),legend.title = element_text(size = 12))
kaita
grid.arrange(imigrantai, kaita, emigrantai, nrow=3)


#### 4 grafikas: Interaktyvus grafikas Registrų centro ####
registru <- read.table('01_gr_open_amzius_lytis_pilietybes_sav_r1.csv',
                       sep = '|',header = TRUE,quote = '',stringsAsFactors = FALSE)
registru<-registru%>% filter(sav_kodas==13, deklarav_metai<2026, fiz_asm_amzius!='0-6', fiz_asm_amzius!='7-17')

df_amzius <- registru%>% count(fiz_asm_amzius)%>% mutate(proc = n / sum(n))
ggplot(df_amzius, aes(x = "", y = proc, fill = factor(fiz_asm_amzius))) +
  geom_col(width = 1) +
  coord_polar("y") +
  scale_fill_manual(values = c('#78003F', '#D3597B', '#E64164','#414141', '#8F8F8F','#DCDCDC','black'))+
  geom_text(aes(label = ifelse(proc >= 0.03, scales::percent(proc, accuracy = 0.1),"")),
    position = position_stack(vjust = 0.5), color = "white", size = 4) +
  labs(fill = "Amžiaus grupė") +
  theme_void()

registru<-registru%>% filter(sav_kodas==13, deklarav_metai>2000, deklarav_metai<2026, 
                            fiz_asm_amzius!='0-6', fiz_asm_amzius!='7-17')

ui <- fluidPage(titlePanel('Vilniaus miesto savivaldybės gyventojų lyties ir amžiaus analizė'),
  fluidRow(column(3,wellPanel(selectInput('metai','Metai:',
                                          choices = c('Bendra', sort(unique(registru$deklarav_metai))),
                                          selected = 'Bendra')))),
  conditionalPanel(condition = 'input.metai == "Bendra"',
                   fluidRow(column(12, plotOutput('barLytis'))),
                   fluidRow(column(12, plotOutput('barAmzius')))),
  conditionalPanel(condition = 'input.metai != "Bendra"',
                   fluidRow(column(6, plotOutput('pieLytis')),column(6, plotOutput('pieAmzius')))))

server <- function(input, output) {
  output$barLytis <- renderPlot({ 
    df <- registru%>% count(deklarav_metai, fiz_asm_lyt)%>%
      group_by(deklarav_metai)%>% mutate(proc = n / sum(n))
    ggplot(df, aes(x = deklarav_metai,y = proc,fill = fiz_asm_lyt)) +
      geom_bar(stat = 'identity') +
      geom_text(aes(label = percent(proc, accuracy = 0.1)),
                position = position_stack(vjust = 0.5),
                size=3, color = 'white') +
      scale_y_continuous(labels = percent) +
      scale_x_continuous(breaks = breaks_extended(15))+
      scale_fill_manual(values = c('M' = '#E64164', 'V' = '#414141'),
                        labels = c('M' = 'Moterys', 'V' = 'Vyrai')) +
      labs(title = 'Lyties pasiskirstymas pagal metus',x = 'Metai',y = 'Procentai',fill = 'Lytis') +
      theme_minimal()})
  
  output$barAmzius <- renderPlot({
    df <- registru%>% count(deklarav_metai, fiz_asm_amzius)%>%
      group_by(deklarav_metai)%>% mutate(proc = n / sum(n))
    ggplot(df, aes(x = deklarav_metai,y = proc,fill = factor(fiz_asm_amzius))) +
      geom_bar(stat = 'identity') +
      scale_y_continuous(labels = percent) +
      scale_x_continuous(breaks = breaks_extended(15))+
      scale_fill_manual(values = c('#78003F', '#D3597B', '#E64164','#414141', '#8F8F8F','#DCDCDC','black','blue','green'))+
      labs(title = 'Amžiaus pasiskirstymas pagal metus',
           x = 'Metai',
           y = 'Procentai',
           fill = 'Amžiaus grupė') +
      theme_minimal()})
  
  output$pieLytis <- renderPlot({
    df <- registru%>% filter(deklarav_metai == input$metai)%>%
      count(fiz_asm_lyt)%>%  mutate(proc = n / sum(n), label = scales::percent(proc, accuracy = 0.1))
    ggplot(df, aes(x = '', y = n, fill = fiz_asm_lyt)) +
      geom_bar(stat = 'identity', width = 1) +
      coord_polar('y') +
      scale_fill_manual(values = c('M' = '#E64164', 'V' = '#414141'),
                        labels = c('M' = 'Moterys', 'V' = 'Vyrai')) +
      geom_text(aes(label = label),position = position_stack(vjust = 0.5),
                color = 'white',size = 5) +
      theme_void() +
      labs(fill = 'Lytis')})
  
  output$pieAmzius <- renderPlot({
    df <- registru%>% filter(deklarav_metai == input$metai)%>%
      count(fiz_asm_amzius)%>% mutate(proc = n / sum(n), label = scales::percent(proc, accuracy = 0.1))
    ggplot(df, aes(x = '', y = n, fill = factor(fiz_asm_amzius))) +
      geom_bar(stat = 'identity', width = 1) +
      coord_polar('y') +
      scale_fill_manual(values = c('#78003F', '#D3597B', '#E64164','#414141', '#8F8F8F','#DCDCDC','black','green','blue'))+
      geom_text(aes(label = label),position = position_stack(vjust = 0.5),
                color = 'white',size = 3) +
      theme_void() +
      labs(fill = 'Amžiaus grupė')})}
shinyApp(ui, server)


#### 5 grafikas: Amžiaus žmonių koeficientas ####
amzius <- read_excel('duomenys.xlsx', sheet = 'Amžiaus koeficientas')
ggplot(amzius,aes(x = Metai)) +
  geom_bar(aes(y = `Iš viso`, fill = 'Iš viso pagal amžių'), stat = 'identity', show.legend = TRUE) +
  geom_line(aes(y = `Iki 14 metų`, color = 'Iki 15 metų'),linewidth = 1) +
  geom_point(aes(y = `Iki 14 metų`, color = 'Iki 15 metų'), size = 2) +
  geom_line(aes(y = `65 ir vyresni`, color = '65 ir vyresni'),linewidth = 1) +
  geom_point(aes(y = `65 ir vyresni`, color = '65 ir vyresni'), size = 2) +
  scale_fill_manual(values = c('Iš viso pagal amžių' = '#78003F')) +
  scale_color_manual(values = c('Iki 15 metų' = '#8F8F8F', '65 ir vyresni' = '#E64164')) +
  scale_y_continuous(breaks = seq(0, 60, by = 10))+
  scale_x_continuous(breaks = seq(2001, 2025, by = 2))+
  labs(title = '',x = 'Metai',y = 'Asmenys',fill = '',color = '') +
  guides(fill = guide_legend(override.aes = list(color = NA)),
         color = guide_legend(override.aes = list(fill = NA))) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        legend.position = 'bottom',legend.direction = 'horizontal',
        axis.text = element_text(size = 12),axis.title = element_text(size = 12),
        legend.text = element_text(size = 12),legend.title = element_text(size = 12))


#### 6 grafikas: Bruto atlyginimas ir BVP vienam gyventojui ####
max_bvp <- max(duomenys$`BVP vienam gyventojui`, na.rm = TRUE)
max_bruto <- max(duomenys$`Bruto vidutinis atlyginimas`, na.rm = TRUE)

ggplot(duomenys%>% filter(Metai >2009), aes(x = Metai)) +
  geom_bar(aes(y = `Bruto vidutinis atlyginimas`, fill = 'Bruto atlyginimas'), 
           stat = 'identity', show.legend = TRUE) +
  geom_line(aes(y = `BVP vienam gyventojui` / max_bvp * max_bruto, color = 'BVP vienam gyventojui'),
            linewidth = 1, group = 1, show.legend = TRUE) +
  scale_y_continuous(name = 'Bruto atlyginimas Eur', labels = scales::label_number(big.mark = ' '),
                     sec.axis = sec_axis( ~ . * max_bvp / max_bruto, name = 'BVP vienam gyventojui tūkst. Eur',
                       labels = scales::label_number(big.mark = ' ')))+
  scale_x_continuous(breaks = seq(2010, 2025, by = 2))+
  scale_fill_manual(name='',values = c('Bruto atlyginimas' = '#78003F')) +
  scale_color_manual(name='',values = c('BVP vienam gyventojui' = '#E64164')) +
  guides(fill = guide_legend(override.aes = list(color = NA)),
         color = guide_legend(override.aes = list(fill = NA))) +
  labs(x = 'Metai') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        legend.position = 'bottom',legend.direction = 'horizontal',
        axis.text = element_text(size = 12),axis.title = element_text(size = 12),
        legend.text = element_text(size = 12),legend.title = element_text(size = 12))


#### 7 grafikas: Gyventojų sk. pagal lytį, gimimo metus, seniūnija####
gyv_seniun <- read.csv('gyventojai seniūnijose.csv')
gimimo_metai <- gyv_seniun%>% count(birth_year)
lytis <- gyv_seniun%>% count(sex)%>% mutate(percent = round(n / sum(n) * 100, 1))
seniunija <- gyv_seniun%>% mutate(eldership_name = ifelse(is.na(eldership_name) | eldership_name == "",
                                                          "Nepriskirta",eldership_name))%>%
  count(eldership_name)%>% arrange(desc(n))

gimimo<- ggplot(gimimo_metai, aes(x = birth_year, y = n)) +
  geom_bar(stat = 'identity', fill = '#78003F') +
  scale_y_continuous(labels = scales::label_number(big.mark = ' '))+
  labs(title = 'Gyventojų skaičius pagal gimimo metus', x = 'Gimimo metai', y = 'Gyventojų skaičius') +
  theme_minimal()

lytis <- ggplot(lytis, aes(x = '', y = n, fill = sex)) +
  geom_bar(stat = 'identity', width = 1) +
  coord_polar(theta = 'y') +
  geom_text(aes(label = (percent)), position = position_stack(vjust = 0.5),
            size=3, color = 'white') +
  scale_fill_manual(values = c('M' = '#E64164', 'V' = '#414141'),
                    labels = c('M' = 'Moterys', 'V' = 'Vyrai')) +
  labs(title = 'Gyventojų pasiskirstymas pagal lytį', fill='Lytis') +
  theme_void() +
  theme(legend.position = 'right')

seniunija <- ggplot(seniunija, aes(x = n, y = reorder(eldership_name, n))) +
  geom_bar(stat = 'identity', fill = '#78003F') +
  scale_x_continuous(labels = scales::label_number(big.mark = ' '))+
  labs(title = 'Gyventojų skaičius pagal seniūniją', x = 'Gyventojų skaičius', y = '') +
  theme_minimal()

grid.arrange(seniunija,arrangeGrob(lytis, gimimo, ncol = 2),nrow = 2, heights = c(2, 1))


ui <- fluidPage(titlePanel('Gyventojai pagal gimimo metus, lytį ir seniūniją'),
  fluidRow(column(3,selectInput('birth_year', 'Pasirinkite gimimo metus:',
                       choices = c('Visi metai' = 'all', sort(unique(gyv_seniun$birth_year))),
                       selected = 'all')),
    column(9,fluidRow(column(12,conditionalPanel(
                  condition = 'input.birth_year == "all"',
                  plotlyOutput('gimimo_plot')))),
       br(),     
       fluidRow(column(5, plotlyOutput('lytis_plot')),
         column(7, plotlyOutput('seniunija_plot'))))))

server <- function(input, output, session) {
  filtered_data <- reactive({
    if(input$birth_year == 'all'){gyv_seniun} 
    else {gyv_seniun%>% filter(birth_year == as.numeric(input$birth_year))}})
  
  output$gimimo_plot <- renderPlotly({
    plot_ly(filtered_data(), x = ~birth_year, type = 'histogram', 
            marker = list(color = '#78003F'))%>%
      layout(title = 'Gyventojai pagal gimimo metus',
             xaxis = list(title = 'Gimimo metai'),
             yaxis = list(title = 'Gyventojų skaičius'))})
  
  output$lytis_plot <- renderPlotly({
    df <- filtered_data()%>% count(sex)%>% mutate(sex = case_when(
      sex == "M" ~ "Moterys", sex == "V" ~ "Vyrai",))
    plot_ly(df, labels = ~sex, values = ~n, type = 'pie', textinfo = 'label+percent',
            marker = list(colors = c('#E64164', '#414141')))%>%
      layout(title = 'Gyventojai pagal lytį', showlegend = FALSE)})

  output$seniunija_plot <- renderPlotly({
    df <- filtered_data()%>% count(eldership_name)%>% arrange(n)
    plot_ly(df, x = ~n, y = ~reorder(eldership_name, n), type = 'bar', 
            marker = list(color = '#78003F'), orientation = 'h')%>%
      layout(title = 'Gyventojai pagal seniūniją',
             xaxis = list(title = 'Gyventojų skaičius'),
             yaxis = list(title = ''))})}
shinyApp(ui, server)

##################### MODELIS #########################
library(forecast)
library(urca)
library(car)
library(class)
library(caret)
library(ppcor)
library(lmtest)
library(effects)
library(tseries)
library(GGally)
library(corrplot)
library(glmnet)
ggplot(duomenys, aes(x=Metai, y=`Vidutinis gyventojų sk.`)) + geom_line() + geom_point()
modeliui<-duomenys%>% filter(duomenys$Metai>2009)%>% arrange(Metai)
names(modeliui) <- make.names(names(duomenys))

####Tiesinės regresijos modelis su mokymo ir testavimo aibem#####
train_data <- modeliui[1:12, ]
test_data  <- modeliui[13:16, ]

modelis_lm <- lm(`Vidutinis.gyventojų.sk.` ~ `Gimusių.kūdikių.sk.` + `Mirusių.sk.` + `Neto.migracija` + 
                 `Gyventojų.tankis` + `Bruto.vidutinis.atlyginimas` + 
                 `BVP.vienam.gyventojui` + `Skurdo.rizikos.lygis`, data=train_data)
summary(modelis_lm)

#Išskirtys
cook_distances <- data.frame(index = 1:length(cooks.distance(modelis_lm)), 
                             cook_distance = cooks.distance(modelis_lm))
(influential_obs <- cook_distances[cook_distances$cook_distance > 1, ])
kukas<-ggplot(cook_distances, aes(x = index, y = cook_distance)) +
  geom_point(color = 'blue', size=2) +
  xlab('Stebėjimo indeksas') + 
  ylab('Kuko matas') + 
  theme_minimal()+
  scale_x_continuous(breaks = seq(0, 13, by = 2.5),expand = expansion(mult = c(0.05, 0.1)))+
  theme(axis.line.x = element_line(color = 'black'),
        axis.line.y = element_line(color = 'black'),
        axis.text.x = element_text(color = 'black'),
        axis.text.y = element_text(color = 'black'))
standardized_residuals <- data.frame(index = 1:length(rstandard(modelis_lm)), 
                                     standardized_residual = rstandard(modelis_lm))
stand<-ggplot(standardized_residuals, aes(x = index, y = standardized_residual)) +
  geom_point(color = 'firebrick1', size=2) +
  xlab('Stebėjimo indeksas') + 
  ylab('Standartizuotos liekanos') +
  scale_x_continuous(breaks = seq(0, 13, by = 2.5),expand = expansion(mult = c(0.05, 0.1)))+
  theme_minimal()+
  theme(axis.line.x = element_line(color = 'black'),
        axis.line.y = element_line(color = 'black'),
        axis.text.x = element_text(color = 'black'),
        axis.text.y = element_text(color = 'black'))
grid.arrange(kukas,stand, nrow=1)

#Normalumas
#H0: X~ N(mu, sigma^2)
#H1: nėra normalusis skirstinys
shapiro.test(modelis_lm$residuals)
residuals_df <- data.frame(residuals = modelis_lm$residuals)
ggplot(residuals_df, aes(sample = residuals)) +
  geom_qq(shape=1) + 
  geom_qq_line(color = 'red') +
  xlab('Teoriniai kvantiliai') +
  ylab('Imties kvantiliai') + 
  theme_minimal()
#kadangi p=0,7344> aplha, todėl H0 neatmetame, 
#vadiansi duomenys pasiskirste pagal normaluji skirstini.

# Liekanos/ homoskedastiškumas
#H0: yra komoskedastiškumas
#H1: nera homoskedastiškumo
bptest(modelis_lm)
residuals_df <- data.frame(index = 1:length(modelis_lm$residuals),residuals = modelis_lm$residuals)
ggplot(residuals_df, aes(x = index, y = residuals)) +
  geom_point(color = 'red', shape = 16) +
  geom_hline(yintercept = 0, linetype = 'dashed', color = 'black') +
  scale_x_continuous(breaks = seq(0, 13, by = 2.5),expand = expansion(mult = c(0.05, 0.1)))+
  xlab('Stebėjimo indeksas') + 
  ylab('Liekanos') +
  theme_minimal() + 
  theme(
    axis.line.x = element_line(color = 'black'),
    axis.line.y = element_line(color = 'black'), 
    axis.text.x = element_text(color = 'black'), 
    axis.text.y = element_text(color = 'black'))
#Kadangi p=0,1876 > alpha taigi H0 neatmetame. Vadinasi yra homoskedastiškumas. 
#Dispersijos tarp grupių yra lygios.

#Multikolinearumas
vif(modelis_lm)

#Be bruto, bvp
geras_lm <- lm(`Vidutinis.gyventojų.sk.` ~ `Gimusių.kūdikių.sk.` + `Mirusių.sk.` + `Neto.migracija` + 
                 `Gyventojų.tankis` +  `Skurdo.rizikos.lygis`, data=train_data)
summary(geras_lm)
#Multikolinearumas
vif(geras_lm)

#Tik reikšmingi kintamieji : buvo išimtas gimūsių kūdikių sk.
pirmas_lm<-lm(`Vidutinis.gyventojų.sk.` ~ `Mirusių.sk.` + `Neto.migracija` + 
                `Gyventojų.tankis` +  `Skurdo.rizikos.lygis`, data=train_data)
summary(pirmas_lm)

#išimtas kintamasis: Skurdo rizikos lygis
antras_lm<-lm(`Vidutinis.gyventojų.sk.` ~ `Mirusių.sk.` + `Neto.migracija` + 
                `Gyventojų.tankis`, data=train_data)
summary(antras_lm)

#isšimtas kintamasis: mirusių sk.
galutinis<-lm(`Vidutinis.gyventojų.sk.` ~`Neto.migracija` + `Gyventojų.tankis`, data=train_data)
summary(galutinis)

#Prognozė:
prognozuoti_arima <- function(serie, h = 10){
  fit <- auto.arima(serie)
  f <- forecast(fit, h = h)
  return(as.numeric(f$mean))}

nauji_duomenys <- data.frame(
  `Neto migracija` = prognozuoti_arima(modeliui$`Neto.migracija`),
  `Mirusių sk.` = prognozuoti_arima(modeliui$`Mirusių.sk.`),
  `Gyventojų tankis` = prognozuoti_arima(modeliui$`Gyventojų.tankis`),
  `Skurdo rizikos lygis` = prognozuoti_arima(modeliui$`Skurdo.rizikos.lygis`))
(prognozes_10m <- predict(galutinis, newdata = nauji_duomenys, interval = 'prediction'))


pred <- predict(galutinis, newdata = test_data)
actual <- test_data$`Vidutinis.gyventojų.sk.`
(rmse <- sqrt(mean((actual - pred)^2)))
(mae <- mean(abs(actual - pred)))
(pearson_lm <- cor(test_data$`Vidutinis.gyventojų.sk.`, pred, method = 'pearson'))
#Vizualiai:
metai <- 2010:2025
istoriniai <- data.frame(Metai = metai, Vidutinis_gyventoju_sk = modeliui$`Vidutinis.gyventojų.sk.`)
prognozuojami_metai <- 2026:2035
prognozes_df <- data.frame(Metai = prognozuojami_metai, Fit = prognozes_10m[,'fit'])
ggplot() +
  geom_line(data = istoriniai, aes(x = Metai, y = Vidutinis_gyventoju_sk, color = 'Istoriniai'), linewidth = 1) +
  geom_line(data = prognozes_df, aes(x = Metai, y = Fit, color = 'Prognozė'), linewidth = 1) +
  scale_color_manual(values = c('Istoriniai' = 'blue', 'Prognozė' = 'red')) +
  scale_y_continuous(labels = scales::label_number(big.mark = ' '))+
  labs(x = 'Metai', y = 'Vidutinis gyventojų sk.', color = '') +
  theme_minimal() +
  theme(legend.position = 'bottom')



#### ARIMA modelis #####
laiko <- duomenys%>% filter(Metai > 1995)%>% arrange(Metai)%>% dplyr::select(Metai, `Vidutinis gyventojų sk.`)
DT<- ts(laiko$'Vidutinis gyventojų sk.', frequency = 1, start = c(1996))
ggtsdisplay(DT, theme = theme_bw(), xlab = 'Metai',ylab = 'Gyventojų sk.')

#H_0: \rho(1)=\rho(2)=...=\rho(5)=0$;
#H_A: bent viena lygybė negalioja.
ljung_box_y <- Box.test(DT, lag =5 , type = 'Ljung-Box')
print(ljung_box_y)
#p< 0,05, vadinasi atmetame H_0, t.y. Y_t nėra baltasis triukšmas (white noise).

#ADF testas
#$H_0: Laiko eilutė turi vienetinę šaknį;
#H_A: Laiko eilutė neturi vienetinės šaknies.
adf_y_trend <- urca::ur.df(DT, type = 'trend', lags = 2)
summary(adf_y_trend)
#testinė statistika τ = 2,962 yra didesnė už kritinę reikšmę -3,50, 
#todėl H_0 neatmetama, laiko eilute turi vienetine sakni, laiko eilute nestacionari

#Modelis ARIMA (p,d,q)
#Mokymo ir testavimo aibe 80:20
n <- length(DT)
train_size <- floor(0.8 * n) 
y_train <- DT[1:train_size] 
y_test <- DT[(train_size+1):n]
max_p <- 3
max_d <- 2
max_q <- 3
results <- data.frame(p=integer(),d=integer(),q=integer(),RMSE=double())
for(d_val in 1:max_d){
  for(p_val in 0:max_p){
    for(q_val in 0:max_q){
      model <- try(Arima(y_train, order=c(p_val,d_val,q_val), include.constant = TRUE), silent=TRUE)
      if(class(model)[1] != 'try-error'){
        fcast <- forecast(model, h=length(y_test))
        pred <- fcast$mean
        rmse <- sqrt(mean((y_test - pred)^2))
        results <- rbind(results, data.frame(p=p_val, d=d_val, q=q_val, RMSE=rmse))}}}}

results <- results[order(results$RMSE),]
(best_model <- results[1,])
(model_best <- Arima(y_train, order=c(0,2,1)))

#Autokoreliacija:
library(patchwork)
acf_plot <- ggAcf(model_best$residuals) + theme_bw()+ labs(title = NULL)
pacf_plot <- ggPacf(model_best$residuals) + theme_bw()+ labs(title = NULL)
acf_plot / pacf_plot #liekanos nekoreliuoja, nes visi stebejimai papuola i pasikliovimo intervalus
#H_0: liekanos neturi reikšmingos autokoreliacijos
#H_A: liekanos turi reikšmingą autokoreliaciją
checkresiduals(model_best)
# p reiksme >0,05, todel neatmetame H_0, neturi autokoreliacijos.


# Modelio tikslumas:
(forecast_test <- forecast(model_best, h = length(y_test)))
(pred_test <- forecast_test$mean)
(rmse <- sqrt(mean((y_test - pred_test)^2)))
(mae  <- mean(abs(y_test - pred_test)))
(pearson_arima <- cor(y_test, pred_test, method = 'pearson'))


#### LASSO ####
n <- nrow(modeliui)
train_size <- floor(0.8 * n)
train_data <- modeliui[1:train_size, ]
test_data  <- modeliui[(train_size + 1):n, ]

#Mokymo ir testavimo aibė
y_train <- train_data$Vidutinis.gyventojų.sk.
X_train <- model.matrix(Vidutinis.gyventojų.sk. ~ 
                          Gimusių.kūdikių.sk. + 
                          Mirusių.sk. + 
                          Neto.migracija + 
                          Gyventojų.tankis + 
                          Bruto.vidutinis.atlyginimas + 
                          BVP.vienam.gyventojui + 
                          Skurdo.rizikos.lygis,
                        data = train_data)[, -1]
y_test <- test_data$Vidutinis.gyventojų.sk.
X_test <- model.matrix(Vidutinis.gyventojų.sk. ~ 
                         Gimusių.kūdikių.sk. + 
                         Mirusių.sk. + 
                         Neto.migracija + 
                         Gyventojų.tankis + 
                         Bruto.vidutinis.atlyginimas + 
                         BVP.vienam.gyventojui + 
                         Skurdo.rizikos.lygis,
                       data = test_data)[, -1]

cv_model <- cv.glmnet(X_train, y_train, alpha = 1)
(best_lambda <- cv_model$lambda.min)
y_pred <- predict(cv_model, s = "lambda.min", newx = X_test)
(rmse <- sqrt(mean((y_test - y_pred)^2)))
(mae <- mean(abs(y_test - y_pred)))
(pearson <- cor(y_test, y_pred))

#LASSOO su lag'ais
df <- modeliui %>%
  arrange(Metai) %>%
  mutate(
    lag1_pop = lag(Vidutinis.gyventojų.sk., 1),
    lag2_pop = lag(Vidutinis.gyventojų.sk., 2),
    
    lag1_birth = lag(Gimusių.kūdikių.sk., 1),
    lag1_death = lag(Mirusių.sk., 1),
    lag1_migration = lag(Neto.migracija, 1)
  )
df <- na.omit(df)
n <- nrow(df)
train_size <- floor(0.8 * n)
train_data <- df[1:train_size, ]
test_data  <- df[(train_size + 1):n, ]


# Mokymo aibė
y_train <- train_data$Vidutinis.gyventojų.sk.
X_train <- model.matrix(Vidutinis.gyventojų.sk. ~ 
                          lag1_pop + lag2_pop +
                          Gimusių.kūdikių.sk. +
                          Mirusių.sk. +
                          Neto.migracija +
                          lag1_birth +
                          lag1_death +
                          lag1_migration +
                          Gyventojų.tankis +
                          Bruto.vidutinis.atlyginimas +
                          BVP.vienam.gyventojui +
                          Skurdo.rizikos.lygis,
                        data = train_data)[, -1]

# Testavimo aibė
y_test <- test_data$Vidutinis.gyventojų.sk.
X_test <- model.matrix(Vidutinis.gyventojų.sk. ~ 
                         lag1_pop + lag2_pop +
                         Gimusių.kūdikių.sk. +
                         Mirusių.sk. +
                         Neto.migracija +
                         lag1_birth +
                         lag1_death +
                         lag1_migration +
                         Gyventojų.tankis +
                         Bruto.vidutinis.atlyginimas +
                         BVP.vienam.gyventojui +
                         Skurdo.rizikos.lygis,
                       data = test_data)[, -1]

cv_model <- cv.glmnet(X_train, y_train, alpha = 1)
(best_lambda <- cv_model$lambda.min)
y_pred <- predict(cv_model, s = "lambda.min", newx = X_test)
# Tikslumas
(rmse <- sqrt(mean((y_test - y_pred)^2)))
(mae <- mean(abs(y_test - y_pred)))
(pearson <- cor(y_test, y_pred))
coef(cv_model)
