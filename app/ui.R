

ui <- fluidPage(
    theme = shinytheme("flatly"),
    br(),br(),
    titlePanel(
        title = div(
            h3(HTML('<div c>
             PRIVATE AND CONFIDENTIAL
             <p >Wealth Profile</p>
             <img id="logo"src="https://www.dropbox.com/s/6d7d15iiuar4rou/fs_bw.png?dl=1" top="25px">
             </div>'), align = "center",
               style='background-color:black'))),
    div(style="text-align:center;position:relative;
    font-family: 'Gotham Thin',Gotham;font-size:24px;
    color: #000000;", textOutput("test")),
    tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style_fs.css")),
    br(),br(),br(),
    tabsetPanel(
        tabPanel(
            "Net Worth",
            fluidRow(
                column(4,
                       div(style="text-align:Left;position:relative;
                ont-family: 'Gotham Thin',Gotham;font-size:  20px;
                color: #000000;border: 2px solid #989898;",
                           textOutput("nworth")))
            ),
            fluidRow(
                column(3, h4("Assets"), reactableOutput("assets")),
                column(6, h4("Asset Allocation"), apexchartOutput("chartpie")),
                column(3, h4(textOutput("total")), plotOutput("direct_invest")) 
            ),
            fluidRow(
                column(3, h4("Liabilities"), reactableOutput("liabilities")),
                column(6, h4("Time-Weighted Returns"), apexchartOutput("twreturns")),
                column(3, h4("Significant Holdings"), reactableOutput("sigholds"))
            ),
            fluidRow(
                column(12, h4("Financial Updates"), 
                       bs_accordion(id = "statment") %>%
                           bs_set_opts(panel_type = "info", use_heading_link = TRUE) %>%
                           bs_append(title="Assets", show="", 
                                     content = htmltools::tagList(
                                         HTML('<div class="col-md-12">'),
                                         HTML(paste(mtdat)), HTML('</div>'),
                                         HTML('<br>'))) %>% 
                           bs_append(title = "Liabilities", show="",
                                     content = htmltools::tagList(
                                         HTML('<div class="col-md-12">'),
                                         HTML(paste(mtdat)), HTML('</div>'))))
            ),
            fluidRow(
                column(12, h4("News Updates"),
                       bs_accordion(id = "statment1") %>%
                           bs_set_opts(panel_type = "info", use_heading_link = TRUE) %>%
                           bs_append(title="Bitcoin", show = "", content = htmltools::tagList(
                               HTML('<div class="col-md-12">'),
                               DT::datatable(
                                   newsbc, escape = FALSE, rownames = FALSE, 
                                   options = list(
                                       scrolly = "50px", ordering = FALSE, info = FALSE, 
                                       lengthChange = FALSE)) %>%
                                   DT::formatStyle(columns = names(newsbc), background = 'black'), 
                               HTML('</div>'), HTML('<br>'))) %>% 
                           bs_append(title = "Apple", show = "", content = htmltools::tagList(
                               HTML('<div class="col-md-12">'), DT::datatable(
                                   newsapple, escape = FALSE, rownames = FALSE, 
                                   options=list(scrolly = "50px", ordering = FALSE, 
                                                info = FALSE, lengthChange = FALSE)) %>%
                                   DT::formatStyle(columns = names(newsapple), background = 'black'),
                               HTML('</div>'))) 
                )
            ),
            fluidRow(
                column(12, h4("Asset Details"), 
                       bs_accordion(id = "statment2") %>%
                           bs_set_opts(panel_type = "success", use_heading_link = TRUE) %>%
                           bs_append(title = "Cash & Equivalents", show = "",
                                     fluidRow(
                                         column(6, reactableOutput("cash_equ")),
                                         column(6, apexchartOutput("cash_equ2")))
                           ) %>% 
                           bs_append(title = "Merrill Lynch Savings", show = "",
                                     fluidRow(
                                         column(6, textOutput("ml_1")),
                                         column(6, tableOutput("ml_2")))
                           )
                )
            )
        ),
        tabPanel("Cash Flow Forecast",
                 bsCollapse(id = "collapseExample", open = "",
                            bsCollapsePanel("Cash Flow 2020", 
                                            reactableOutput("cash_flow20"),
                                            style = "success"),
                            bsCollapsePanel("Cash Flow 2021", 
                                            reactableOutput("cash_flow21"), 
                                            style = "success")
                 )
        ),
        tabPanel("Structure",
                 collapsibleTreeOutput("structure")
        )
    )
)