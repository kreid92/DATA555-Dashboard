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
    # TAB 3 - SELF HARM
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
  
  # =======================
  # PLOT 1 - SELF HARM (FIXED INCIDENCE RATE)
  # =======================
  
  output$plot1 <- renderPlotly({
    
    base_data <- sui_count_demo %>%
      filter(!is.na(studyid), !is.na(fup_time), fup_time > 0)
    
    df_summary <- base_data %>%
      mutate(
        
        group = case_when(
          tf == 1 ~ "Transfeminine",
          tm == 1 ~ "Transmasculine",
          subgroup == "CF" ~ "Cisfeminine",
          subgroup == "CM" ~ "Cismasculine",
          TRUE ~ NA_character_
        ),
        
        GAHT_label = ifelse(GAHT == 1, "GAHT", "No GAHT")
      ) %>%
      filter(!is.na(group)) %>%
      
      group_by(group, GAHT_label) %>%
      summarise(
        total_events = sum(sh_count_fup, na.rm = TRUE),
        total_py = sum(fup_time, na.rm = TRUE),
        rate = total_events / total_py,
        .groups = "drop"
      )
    
    plot_ly(
      df_summary,
      x = ~group,
      y = ~rate,
      color = ~GAHT_label,
      type = "bar",
      barmode = "group",
      text = ~paste0(
        "Group: ", group,
        "<br>GAHT: ", GAHT_label,
        "<br>Events: ", total_events,
        "<br>Person-years: ", round(total_py, 2),
        "<br>Rate: ", round(rate, 4), " per person-year"
      ),
      hoverinfo = "text"
    ) %>%
      layout(
        title = "Self-Harm Incidence Rates (Events per Person-Year)",
        xaxis = list(title = "Subgroup"),
        yaxis = list(title = "Incidents per Person-Year"),
        barmode = "group"
      )
  })
  
  # =======================
  # PLOT 2 - MENTAL HEALTH
  # =======================
  
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