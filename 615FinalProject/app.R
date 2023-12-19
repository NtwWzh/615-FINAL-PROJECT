
library(shiny)
library(leaflet)
library(DT) 
library(sf)
library(readxl)
library(scales)
library(ggplot2)
library(tidyr)
library(dplyr)

ui <- fluidPage(
  navlistPanel(
        id = "tabs",
        "Lion City - Singapore",
        tabPanel("Map", value = "map"),
        tabPanel("Key Fact", value = "keyfacts"),
        tabPanel("Description", value = "descriptions"),
        "What's about Singapore",
        tabPanel("Key Demographics", value = "demographics"),
        tabPanel("Education", value="educationplot"),
        tabPanel("Health", value="health"),
        tabPanel("Conclusion", value = "conclusion"),
        tabPanel("Comparison", value = "comparison"),
        tabPanel("SWOT Analysis", value = "swot"),
        tabPanel("Reference", value = "references"),
    
    mainPanel(
      # 使用uiOutput可以在server端控制显示哪个输出
      uiOutput("tabContent"),
      uiOutput("reference")
    )
  )
)


server <- function(input, output, session) {
  
  output$tabContent <- renderUI({
    # 根据选择的标签页，返回不同的UI组件
    switch(input$tabs,
           map = {
             list(
               leafletOutput("map")
             )
           },
           keyfacts = {
             list(
               dataTableOutput("keyFactsTable")
             )
           },
           
           descriptions = {
             list(
               textOutput("description")
             )
           },
           
           demographics = {
             list(
               dataTableOutput("DemographicTable")
             )
           },
           
           conclusion = {
             list(
               textOutput("demographicetext")
             )
           },
           
           educationplot = {
             list(plotOutput("educationPlot",width = "600px"))
           },
           health = {
             list(plotOutput("healthplot", width = "600px"))
           },
           
           comparison = {
             list(
               dataTableOutput("comparisontablesocial"),
               textOutput("comparisontext"),
               dataTableOutput("comparisontableenvironment")
             )
           },
           swot = {
             list(
               textOutput("description")
             )
           },
           references = {
             list(
               textOutput("reference")
             )
           }
    )
  })
  
  
  singapore_data <- st_read("gadm41_SGP.gpkg")
  
  education_data <- read.csv("Government Expenditure on Education.csv")
  
  health_data <- read.csv("Government Expenditure on Education.csv")
  
  # 渲染地图
  output$map <- renderLeaflet({
    if (input$tabs == "map") {
      leaflet() %>%
        addProviderTiles("OpenStreetMap.Mapnik") %>%
        addPolygons(data = singapore_data, fillOpacity = 0.5)
    }
  })
  
  # 渲染"Map"下的关键事实表格和描述
  output$keyFactsTable <- renderDataTable({
    if(input$tabs == "keyfacts"){
    key_facts_df <- data.frame(
      "General Information" = c("Region", "Capital city", "UN membership date", "National currency", "Official language"),
      "Key Facts" = c("South-eastern Asia", "Singapore", "21-Sep-65", "Singapore Dollar (SGD)", "English/Chinese/Malay/Tamil")
    )}
  })
  
  output$description <- renderText({
    if(input$tabs == "descriptions"){
    "Singapore is an island state located in Southeastern Asia. In Malay, 
  the country name is Singapura and it means Lion City. It is known for its modernity, 
  high population density, and diverse culture. The country has a rich history and is a 
  major financial and business hub in Southeastern Asia."
    }
  })
 
  output$DemographicTable <- renderDataTable({
    if (input$tabs == "demographics") {
      demographic_df <- data.frame(
        Information = c("Population (000, 2021)", "Pop. density (per km2, 2021)", "Capital city pop. (000, 2021)",
                        "Surface area (km2)", "Sex ratio (m per 100 f)", "Exchange rate (per US$)",
                        "GDP: Gross domestic product (million current US$)",
                        "GDP growth rate (annual %, const. 2015 prices)", 
                        "Economy: Agriculture (% of Gross Value Added)"
                        , "Economy: Industry (% of Gross Value Added)"
                        , "Economy: Services and other activity (% of GVA)"
                        , "Employment in agriculture (% of employed)"
                        , "Employment in industry (% of employed)"
                        , "Employment in services (% employed)", 
                        "Unemployment (% of labour force)",
                        "Labour force participation rate (female/male pop. %) - Female / Male",
                        "CPI: Consumer Price Index (2010=100)",
                        "International trade: exports (million current US$)",
                        "International trade: imports (million current US$)",
                         "Balance of payments, current account (million US$)"),
        Value = c("5,897", "8,423.8", "5,868.1","725","109.8", "1.3","372,074","0.7","-0.0","25.8",
                  "74.2","0.7","15.2","84.1","4.4","61.8/78.2","114","362,928","329,032","59,786")
      )
      datatable(demographic_df,options = list(scrollY = '1080px', scrollCollapse = TRUE))
    }
  })
  
  output$demographicetext <- renderText({
    if(input$tabs == "conclusion"){
      "In conclusion, Singapore's Gross Domestic Product (GDP) reached $372074 million, 
      showing stable growth, especially with an average annual growth rate of 0.7% based 
      on constant prices in 2015. Singapore's economy is mainly dominated by the service industry,
      accounting for 74.2% of the gross domestic product. The service industry is the main sector
      of employment in Singapore, accounting for the highest proportion of employment at 84.1%. 
      The unemployment rate can be considered relatively low. However, the consumption level is
      high."
    }
  })
  
  output$comparisontablesocial <- renderDataTable({
    if(input$tabs == "comparison"){
      comparison_df_social <- data.frame(
        Information = c("Population growth ratei (average annual %)", 
                        "Urban population (% of total population)", 
                        "Urban population growth ratei (average annual %)",
                        "Fertility rate, totali (live births per woman)", 
                        "Life expectancy at birthi (females/males, years)", 
                        "Population age distribution (0-14/60+ years old, %)", 
                        "International migrant stock (000/% of total pop.)", 
                        "Refugees and others of concern to UNHCR (000)", 
                        "Infant mortality ratei (per 1 000 live births)", 
                        "Health: Current expenditurek,l (% of GDP)", 
                        "Health: Physicians (per 1 000 pop.)",
                        "Education: Government expenditure (% of GDP)",
                        "Education: Primary gross enrol. ratio (f/m per 100 pop.)",
                        "Education: Secondary gross enrol. ratio (f/m per 100 pop.)",
                        "Education: Upper secondary gross enrol. ratio (f/m per 100 pop.)",
                        "Intentional homicide rate (per 100 000 pop.)", 
                        "Seats held by women in national parliaments (%)",
                        "Individuals using the Internet (per 100 inhabitants)",
                        "Research & Development expenditure (% of GDP)",
                        "Threatened species (number)","Forested areah (% of land area)",
                        "CO2 emission estimates (million tons/tons per capita)",
                        "Energy production, primary (Petajoules)",
                        "Energy supply per capita (Gigajoules)",
                        "Tourist/visitor arrivals at national borderss (000)",
                        "Important sites for terrestrial biodiversity protected (%)"),
        Singapore = c("0.9","100","NULL","1.2","85.5/81.2","12.4/21.9","2,523.6/43.1",
                      "1.3","1.6","4.5","2.3","NULL","100.3","106.3","111.7", "0.2", "29.5",
                      "88.9","1.9","350","22.5","47.4/8.4","26","186","15,119","21.1"),
        Malaysia = c("1.3","76.6","NULL","2","78.1/74.0", "23.3/11.3","3,476.6/10.7","188.1",
                     "5.9","3.8","NULL","4.2","106.0/104.6","87.0/80.7","83.8/73.4","NULL","14.9",
                     "84.2","1","1,928","58.5","228.0/7.2","3,952","122","26,101","28.5")
      )
      datatable(comparison_df_social)
    }
    
  })
  
  long_data <- pivot_longer(education_data, cols = starts_with("X"), 
                            names_to = "Year", values_to = "Expenditure")
  
  # 转换年份列，去除前面的"X"并转换为整数型
  long_data$Year <- as.integer(sub("X", "", long_data$Year))
  
  filtered_data <- long_data %>%
    filter(Year >= 1992 & Year <= 2022) %>%
    filter(`Data.Series` == "Total Government Expenditure On Education (Thousand Dollars)") %>%
    mutate(Expenditure = gsub(",", "", Expenditure), # 移除逗号
           Expenditure = as.numeric(Expenditure)) %>% # 转换为数值型
    select(-Data.Series)
  
  # 绘制图表
  output$educationPlot <- renderPlot({
    if(input$tabs == "educationplot"){
    ggplot(filtered_data, aes(x = Year, y = Expenditure)) +
      geom_line(shape = 1, colour = "blue") +
      theme_minimal()+scale_y_continuous(labels = label_bytes())+
      labs(title = "Total Government Expenditure on Education (1992-2022)",
           x = "Year", y = "Expenditure (Thousand Dollars)")
    }
  })
  
  
  
  longhealth_data <- pivot_longer(health_data, cols = starts_with("X"), 
                            names_to = "Year", values_to = "Value")
  
  # 转换年份列，去除前面的"X"并转换为整数型
  longhealth_data$Year <- as.integer(sub("X", "", longhealth_data$Year))
  
  filteredhealth_data <- longhealth_data %>%
    filter(Year >= 1992 & Year <= 2022) %>%
    filter(`Data.Series` == "Government Health Expenditure (Million Dollars)") %>%
    mutate(Value = gsub(",", "", Value), # 移除逗号
           Value = as.numeric(Value)) %>% # 转换为数值型
    select(-Data.Series)
  
  # 绘制图表
  output$healthplot <- renderPlot({
    if(input$tabs == "health"){
      ggplot(filteredhealth_data, aes(x = Year, y = Value)) +
        geom_line(shape = 1, colour = "blue") +
        theme_minimal()+scale_y_continuous(labels = label_bytes())+
        labs(title = "Total Government Expenditure On Education (Million Dollars)",
             x = "Year", y = "Expenditure (Million Dollars)")
    }
  })
  
  
  
  output$comparisontext <- renderText({
    if(input$tabs == "comparison"){
      "Singapore and Malaysia, neighboring countries in Southeast Asia, have similarities in
      their past, culture, and location. Once part of Malaysia, Singapore became independent
      in 1965 and grew into a prosperous nation. Both places have diverse populations, 
      including Malays, Chinese, and Indians. They use languages like English and Chinese. 
      Economically, they are important in the region, being centers for global business. 
      Malaysia and Singapore have good tourism since people love to visit Kuala Lumpur and
      Penang in Malaysia and Marina Bay and Sentosa Island in Singapore."
    }
  })
  
  output$reference <- renderUI({
    if(input$tabs == "references"){
      HTML("Code guide: chatgpt<br>
           Map data: <a href='https://gadm.org/download_country.html' target='_blank'>
           https://gadm.org/download_country.html</a><br>
           Singapore data: <a herf='https://data.un.org/en/iso/sg.html' target='_blank'>
           https://data.un.org/en/iso/sg.html</a><br>
           Malaysia data: <a herf='https://data.un.org/en/iso/my.html' target='_blank'>
           https://data.un.org/en/iso/my.html</a><br>
           Education data: <a herf='https://www.singstat.gov.sg/publications/
           reference/ebook/population/education-and-literacy' target ='_blank'>
           https://www.singstat.gov.sg/publications/reference/ebook/population
           /education-and-literacy</a><br>
           ")
    }
  })
  
  
}

shinyApp(ui = ui, server = server)

