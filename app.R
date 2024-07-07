library(shiny)
library(ggplot2)
library(gridlayout)
library(bslib)
library(DT)
library(metafor)
library(collapse)
library(stringr)
library(plotly)

initial_df <- readRDS("Drexl2024_yemama_processed.rds") %>% 
  mtt(
    author = str_extract(record_id,"[^_]+")
  ) %>% 
  colorder(author,year,first_pub) %>% 
  fselect(
    author,year,sample_country1,acceptance_reported,acceptance_rate_extr,
    compliance_reported,compl_m,retention_reported,compl_cutoff_strict_bin, 
    compl_cutoff,retention_rate_extr,
    age, female.prc, nonwhite.prc, clin_sample.mix, clin_sompsy.mix, 
    treat_status.any_vs_na, treat_setting.inout_nomix, symptom_ema,
    n_ema_days,prompt_dfreq,item_n,resp_dur_m_sec,
    enhancement, obj_addon, incent_compliance, 
    inc_val_pot_max.usd, ema_train_vs_nonrep, part_care_minact, 
    parent_involvement
  ) %>% 
  mtt(
    author = author %>% setLabels("Author"),
    acceptance_reported = acceptance_reported %>% setLabels("Acceptance reported"),
    compliance_reported = compliance_reported %>% setLabels("Compliance reported"),
    retention_reported = retention_reported %>% setLabels("Retention reported"),
    compl_cutoff = compl_cutoff %>% setLabels("Compliance cutoff"),
    compl_cutoff = compl_cutoff %>% setLabels("Compliance cutoff"),
    clin_sompsy.mix = clin_sompsy.mix %>% setLabels("Diagnostic groups"),
    treat_status.any_vs_na = treat_status.any_vs_na %>% setLabels("EMA in treatment"),
    treat_setting.inout_nomix = treat_setting.inout_nomix %>% setLabels("Treatment status")
  )

ui <- page_navbar(
  title = "Youth EMA Meta-Analysis (YEMAMA) Update",
  selected = "Filter",
  collapsible = TRUE,
  theme = bslib::bs_theme(
    bootswatch = "yeti",
    navbar_bg = "#25443B"
  ),
  nav_panel(
    title = "Filter",
    grid_container(
      layout = c("description","table"),
      row_sizes = c(
        "100px",
        "1fr"
      ),
    grid_card(
      area = "description",
      card_body(
        markdown(
          mds = c(
            "Welcome to the companion app of Drexl, K., Ralisa, V., Rosselet-Amoussou, J., Wen, C.K.F., Urben, S., Plessen, K.J., and Glaus, J. (under review). *Readdressing the ongoing challenge of missing data in youth Ecological Momentary Assessment studies: A meta-analysis update.*",
            "<br>This app lets you filter and download of the meta-analytic dataset to the subset of your interest among the 285 included samples.",
            "Additionally, you can select and visualize variable pairs based on the filtered dataset.",
            "For open data and code, please visit <a href=\"https://osf.io/8nkeu/?view_only=f87987d1419d4b3b9f2b6499a3686460\">our OSF repository</a>."
          )
        )
      )
    ),
    grid_card(
      area = "table",
      card_body(
        gap = "0px",
        div(DTOutput("table"), width = "100%", style = "font-size:8pt")
      )
    )
    )
  ),
  nav_panel(
    "Plot",
    grid_container(
      row_sizes = c(
        "1fr"
      ),
      col_sizes = c(
        "250px",
        "1fr"
      ),
      gap_size = "10px",
      layout = c(
        "plot_ui plot"
      ),
      grid_card(
        area = "plot_ui",
        card_header("Plot"),
        card_body(
          gap = "0px",
          selectizeInput("x_var", "X-Axis", choices = NULL),
          selectizeInput("y_var", "Y-Axis", choices = NULL),
          selectizeInput("col_var", "Color", choices = NULL)
          
          # radioButtons(
          #   inputId = "suppl_layer_choice",
          #   label = "Layers",
          #   choices = list(
          #     "only plain data" = "none",
          #     "descriptive summary" = "desc",
          #     "meta-analytic synthesis" = "meta"
          #   )
          # )
        )
      ),
      grid_card(
        area = "plot",
        card_body(plotlyOutput(outputId = "filtplot"))
      )
    )
  )
)


server <- function(input, output, session) {
  
  in_react_frame<-reactiveVal(initial_df)
  
  filtered_frame <-  reactive({
    frame <- req(in_react_frame())
    indexes <- req(input$table_rows_all)
    
    frame[indexes,]
  })
  
  updateSelectizeInput(
    session, "x_var", 
    choices = initial_df %>% 
      {setNames(names(.),vlabels(.))[!is.na(vlabels(.))]},
    selected = "age"
    )
  
  updateSelectizeInput(
    session, "y_var", 
    choices = initial_df %>% 
      {setNames(names(.),vlabels(.))[!is.na(vlabels(.))]},
    selected = "compl_m"
  )
  
  updateSelectizeInput(
    session, "col_var", 
    choices = initial_df %>% 
      {setNames(names(.),vlabels(.))[!is.na(vlabels(.))]},
    selected = "clin_sample.mix"
  )
  
  output$table <- renderDT(
    datatable(
      in_react_frame(),
      filter = "top",
      rownames = FALSE,
      escape=FALSE,
      colnames = initial_df %>% 
         {setNames(names(.),vlabels(.))[!is.na(vlabels(.))]},
      extensions = "Buttons",
      options = list(
        pageLength = 5,
        dom = "Bfrtip",
        buttons = c("copy", "csv", "excel")
      )
    )
  )
  output$filtplot <- renderPlotly({
    fig <- req(filtered_frame()) %>% 
      ggplot(
        aes_string(
          label = "author",
          label2 = "year",
          color = input$col_var,
          x = input$x_var, 
          y = input$y_var
        )
      ) +
      geom_point(aes_string(x = input$x_var, y = input$y_var)) +
      labs(
        color = vlabels(gv(initial_df,input$col_var)),
        x = vlabels(gv(initial_df,input$x_var)),
        y = vlabels(gv(initial_df,input$y_var))
      )
    fig <- ggplotly(fig,tooltip = c("author","year","x","y"))
  })
  
 output$values <- renderPrint(list(x_var = input$x_var, y_var = input$y_var))
}

shinyApp(ui, server)
  

