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
# 🔥 PRE-AGGREGATION (CRITICAL FIX)
# -----------------------

sui_prepped <- sui_count_demo %>%
  filter(!is.na(studyid), !is.na(fup_time), fup_time > 0) %>%
  mutate(
    cohort = case_when(
      tf == 1 ~ "Transfeminine Cohort",
      tm == 1 ~ "Transmasculine Cohort",
      TRUE ~ NA_character_
    ),
    GAHT_label = ifelse(GAHT == 1, "GAHT", "No GAHT"),
    group_label = case_when(
      tf == 1 ~ "Transfeminine",
      tm == 1 ~ "Transmasculine",
      TRUE ~ "Cisgender Controls"
    ),
    rate = sh_count_fup / fup_time,
    log_rate = log10(pmax(rate, 1e-5)),
    x_group = paste(cohort, GAHT_label, sep = " - ")
  ) %>%
  filter(!is.na(cohort))

# optional safety: reduce plotting load
sui_plot_data <- sui_prepped %>%
  sample_n(min(n(), 50000))  # prevents server crash

# -----------------------
# DATASET DESCRIPTION
# -----------------------

dataset_info <- list(
  title = "Dataset Overview",
  text = "
This dashboard examines self-harm rates and mental health diagnoses among transgender and cisgender individuals using electronic health record data from the Kaiser Permanente Study of Transition, Outcomes and Gender (STRONG) cohort. The dataset is confidential. The STRONG cohort includes individuals enrolled in four participating Kaiser Permanente health system regions in Georgia, the Mid-Atlantic States, Northern California, and Southern California. The original cohort enrolled (index date) participants from only Georgia, Northern California, and Southern California seeking care between 2006 and 2014 with follow-up through 2016 and the current cohort has index dates through 2022 and follow-up through 2024."
)

# -----------------------
# UI
# -----------------------

ui <- fluidPage(
  
  titlePanel("How is Self-Harm Affected for Transgender Individuals?"),
  
  tabsetPanel(
    
    # TAB 1
    tabPanel(
      "Dataset Description",
      fluidPage(
        h3(dataset_info$title),
        tags$hr(),
        HTML(gsub("\n", "<br>", dataset_info$text)),
        tags$hr(),
        
        p("This dashboard helps identify difference in self-harm and mental health outcomes across transgender and cisgender populations using real-world clinical data. These insights can inform more equitable healthcare delivery, targeted prevention efforts, and improved mental health support."),
        
        tags$hr(),
        
        p(
          "Code documentation available at: ",
          tags$a(
            href = "https://github.com/kreid92/DATA555-Dashboard",
            target = "_blank",
            "GitHub Repository"
          )
        )
      )
    ),
    
    # TAB 2
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
    
    # TAB 3
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
  # PLOT 1 (FIXED + SAFE)
  # -----------------------
  
  output$plot1 <- renderPlotly({
    
    d <- sui_plot_data
    
    validate(need(nrow(d) > 0, "No data available"))
    
    plot_ly(
      d,
      x = ~x_group,
      y = ~log_rate,
      color = ~group_label,
      type = "box",
      boxpoints = FALSE,   # 🔥 CRITICAL FIX (prevents crash)
      jitter = 0.2,
      pointpos = 0
    ) %>%
      layout(
        title = "Self-Harm Rates by GAHT Usage",
        yaxis = list(title = "Log10(Self-harm rate)"),
        xaxis = list(tickangle = -20)
      )
  })
  
  # -----------------------
  # PLOT 2 (UNCHANGED BUT SAFE)
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
      type = "bar"
    ) %>%
      layout(
        title = paste0(
          "Mean Mental Health Diagnoses Between ",
          ifelse(input$dataset == "TM", "Transmasculine", "Transfeminine"),
          " People and Referents by ",
          x_label
        ),
        barmode = "group",
        xaxis = list(title = x_label),
        yaxis = list(title = "Mean Mental Health Diagnoses")
      )
  })
}

# -----------------------
# RUN
# -----------------------

shinyApp(ui, server)