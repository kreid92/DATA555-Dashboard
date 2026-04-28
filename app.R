pacman::p_load(tidyverse, plotly, shiny, rsconnect, haven, here)

# -----------------------
# LOAD DATA
# -----------------------

data <- haven::read_sas(here::here("data","final_data.sas7bdat"))
sui_count_demo <- haven::read_sas(here::here("data","sui_count_demo.sas7bdat"))

data <- data %>%
  mutate(
    count = replace_na(count, 0),
    fup_2nd = replace_na(fup_2nd, 0),
    age_group = cut(
      age_at_index,
      breaks = c(3, 15, 21, 41, Inf),
      labels = c("3-14", "15-21", "21-40", "41+"),
      include.lowest = TRUE
    )
  ) %>%
  filter(age_at_index >= 3)

dt_all <- data %>%
  mutate(dataset = case_when(
    tm == 1 ~ "TM",
    tf == 1 ~ "TF",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(dataset)) %>%
  mutate(
    subgroup = factor(subgroup,
                      levels = c("TF", "TM", "CM", "CF"),
                      labels = c("Transfeminine", "Transmasculine",
                                 "Cismasculine", "Cisfeminine"))
  )

# -----------------------
# DATASET DESCRIPTION
# -----------------------

dataset_info <- list(
  title = "Study Overview",
  text = "
This dashboard examines self-harm rates and mental health diagnoses among transgender and cisgender individuals using electronic health record data from the Kaiser Permanente STRONG cohort. 
The dataset is confidential. 
The cohort spans four Kaiser Permanente regions and includes longitudinal follow-up through 2024.
"
)

# -----------------------
# UI
# -----------------------

ui <- fluidPage(
  
  titlePanel("Self-Harm and Mental Health in Transgender Cohorts"),
  
  tabsetPanel(
    
    # =======================
    # TAB 1 - DESCRIPTION
    # =======================
    tabPanel(
      "Dataset Description",
      fluidPage(
        h3(dataset_info$title),
        tags$hr(),
        
        HTML(gsub("\n", "<br>", dataset_info$text)),
        
        tags$hr(),
        
        p("This dashboard helps identify difference in self-harm and mental health outcomes across transgender and cisgender populations using real-world clinical data. These insights can inform more equitable healthcare delivery, targeted prevention efforts, and improved mental health support.")
      )
    ),
    
    # =======================
    # TAB 2 - MENTAL HEALTH
    # =======================
    tabPanel(
      "Mental Health Diagnoses",
      
      sidebarLayout(
        
        sidebarPanel(
          selectInput(
            "dataset",
            "Cohort",
            choices = c("Transmasculine (TM)" = "TM",
                        "Transfeminine (TF)" = "TF")
          ),
          
          selectInput(
            "xvar",
            "Demographic Variable",
            choices = c(
              "Age Group" = "age_group",
              "Study Site" = "site",
              "Race/Ethnicity" = "raceeth"
            )
          )
        ),
        
        mainPanel(
          plotlyOutput("plot2", height = "600px")
        )
      )
    ),
    
    # =======================
    # TAB 3 - SELF HARM (RESTORED)
    # =======================
    tabPanel(
      "Self-Harm Rates",
      plotlyOutput("plot1", height = "600px")
    )
  )
)

# -----------------------
# SERVER
# -----------------------

server <- function(input, output) {
  
  # -----------------------
  # PLOT 1 - SELF HARM
  # -----------------------
  
  output$plot1 <- renderPlotly({
    
    base_data <- sui_count_demo %>%
      filter(!is.na(studyid), fup_time > 0)
    
    tf_sets <- base_data %>% filter(tf == 1) %>% pull(matchkey)
    tm_sets <- base_data %>% filter(tm == 1) %>% pull(matchkey)
    
    df <- base_data %>%
      mutate(
        cohort = case_when(
          matchkey %in% tf_sets ~ "Transfeminine Cohort",
          matchkey %in% tm_sets ~ "Transmasculine Cohort"
        ),
        GAHT_label = ifelse(GAHT == 1, "GAHT", "No GAHT"),
        group_label = case_when(
          subgroup == "TF" ~ "Transfeminine",
          subgroup == "TM" ~ "Transmasculine",
          TRUE ~ "Cisgender Controls"
        ),
        rate = sh_count_fup / fup_time,
        log_rate = log10(pmax(rate, 1e-5)),
        x_group = paste(cohort, GAHT_label, sep = " - ")
      ) %>%
      filter(!is.na(cohort))
    
    plot_ly(
      df,
      x = ~x_group,
      y = ~log_rate,
      color = ~group_label,
      type = "box",
      boxpoints = "all",
      jitter = 0.25,
      pointpos = 0,
      text = ~paste(
        "Cohort:", cohort,
        "<br>Group:", group_label,
        "<br>Rate:", round(rate, 2)
      ),
      hoverinfo = "text"
    ) %>%
      layout(
        title = "Self-Harm Rates by GAHT Usage",
        yaxis = list(title = "Log10(Self-harm rate)"),
        xaxis = list(tickangle = -20)
      )
  })
  
  # -----------------------
  # PLOT 2 - MENTAL HEALTH
  # -----------------------
  
  output$plot2 <- renderPlotly({
    
    x_label <- case_when(
      input$xvar == "age_group" ~ "Age Group (years)",
      input$xvar == "site" ~ "Study Site",
      input$xvar == "raceeth" ~ "Race / Ethnicity"
    )
    
    df <- dt_all %>%
      filter(dataset == input$dataset) %>%
      mutate(x_var = .data[[input$xvar]]) %>%
      group_by(subgroup, x_var) %>%
      summarise(dx = mean(dx_count, na.rm = TRUE), .groups = "drop")
    
    plot_ly(
      df,
      x = ~x_var,
      y = ~dx,
      color = ~subgroup,
      type = "bar",
      text = ~paste(
        subgroup,
        "<br>", x_var,
        "<br>Mean:", round(dx, 2)
      ),
      hoverinfo = "text"
    ) %>%
      layout(
        title = paste0(
          "Mean Mental Health Diagnoses Between ",
          ifelse(input$dataset == "TM", "Transmasculine", "Transfeminine"),
          " People and Referents by ",
          x_label
        ),
        barmode = "group",
        xaxis = list(title = x_label, tickangle = -30),
        yaxis = list(title = "Mean Mental Health Diagnoses")
      )
  })
}

# -----------------------
# RUN APP
# -----------------------

shinyApp(ui, server)