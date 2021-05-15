

ui <- fluidPage(
    includeCSS("style_fs.css"),
    HTML(
        '<article>
        <img id="logo"src="https://www.dropbox.com/s/6d7d15iiuar4rou/fs_bw.png?dl=1"
        top="50px">
        </article>'
    ),
    div(
        br(),
        class = "titleback",
        br(),
        div(class = "watermark", "PRIVATE AND CONFIDENTIAL"),
        br(), br(),
        div(class = "a", textOutput("test")),
        br(), br(),
        p(class = "b", "Wealth Profile"),
        br()
    ),
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
                column(12, 
                       p(class = "d", "Asset Details"),
                       tags$details(
                           tags$summary(
                               "Cash & Equivalents"
                           ),
                           br(),
                           div(class = "row",
                               div(class = "col-md-5",
                               div(id = "class-tables", reactableOutput("cash_equ"))),
                               div(class = "col-md-2"),
                               div(class = "col-md-6", apexchartOutput("cash_equ2"))),
                           br(),
                           div(id = "box-shadow",
                               div(id="details-content",
                                   tags$details(
                                       tags$summary(
                                           "Merrill Lynch Savings"
                                       ),
                                       br(),
                                       div(class = "row",
                                           div(class = "col-md-4",
                                               div(class = "class-tables", textOutput("ml_1"))),
                                           div(class = "col-md-5", tableOutput("ml_2")))
                                   ),
                                   br()
                           ))
                       )
                )
            )
        ),
        tabPanel("Cash Flow Forecast",
            br(), br(),
            tags$details(
                tags$summary(
                    "Cash Flow 2020"
                ),
                reactableOutput("cash_flow20")
            ),
            br(),br(),
            tags$details(
                tags$summary(
                    "Cash Flow 2021"
                ),
                reactableOutput("cash_flow21")
            )
        ),
        tabPanel("Structure",
                 div(class = "row",
                     div(class = "col-md-1"),
                     div(class = "col-md-10",
                         collapsibleTreeOutput("structure"),
                         div(class = "col-md-1")
                    ))
        )
    )
)