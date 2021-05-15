

server <- function(input, output) {
    title_change <- reactive({
        as.character(paste("Sample Client"))
    })
    output$test <- renderText({ title_change() })
    net_worth <- reactive({
        as.character(paste("Net Worth:", mtcars[1,3]))
    })
    output$nworth <- renderText({ net_worth()})
    output$assets <- renderReactable({
        reactable(mtcars[1:20,])
    })
    output$chartpie <- renderApexchart({
        apex(data=mtcars,
             type="donut",mapping=aes(x=rownames(mtcars),y=mpg))
    })
    output$direct_invest <- renderPlot({
        ggplot(data=mtcars)+
            geom_point(aes(x=hp, y=mpg, color=as.factor(am),size=cyl))
    })
    output$liabilities <- renderReactable({
        reactable(mtcars[1:10,])
    })
    net_worth <- reactive({
        as.character(paste("Net Worth:", mtcars[1,3]))
    })
    output$nworth <- renderText({ net_worth()})
    total <- reactive({
        as.character(paste("Total Direct Investments ", 
                           format.money(sum(mtcars$disp)),
                           " million",sep=""))
    })
    output$total <- renderText({ total()})
    output$direct_invest <- renderPlot({
        ggplot(data=mtcars)+
            geom_point(aes(x=hp, y=mpg, color=as.factor(am),size=cyl))
    })
    output$liabilities <- renderReactable({
        reactable(mtcars[1:10,])
    })
    output$twreturns <- renderApexchart({
        apex(mtcars[1:10,],
             type="column",
             mapping = aes(x = rownames(mtcars[1:10,]), y = mpg, fill = cyl), 
             height=300,
             width=600)
    })
    output$sigholds <- renderReactable({
        reactable(mtcars[1:10,])
    })
    total <- reactive({
        as.character(paste("Total Direct Investments ", 
                           format.money(sum(mtcars$disp)),
                           " million",sep=""))
    })
    output$total <- renderText({ total()})
    output$cash_equ <- renderReactable({
        reactable(mtdat)
    })
    ml_1 <- reactive({
        bankdf <- mtcars %>% slice(1) %>% rownames(.)
    })
    output$ml_1 <- renderText({ ml_1()})
    output$cash_equ2 <- renderApexchart({
        apex(mtdat, type = "donut",
             height = '200', width = '500',
             mapping = aes(x = rownames(mtcars), y = mpg))
    })
    output$ml_2 <- function() {
        kable(mtdat,escape = FALSE) %>% kable_styling()
    }
    output$cash_flow20 <- renderReactable({
        reactable(mtcars)
    })
    output$cash_flow21 <- renderReactable({
        reactable(mtcars)
    })
    output$structure <- renderCollapsibleTree({
        org <- data.frame(
            Manager = c(
                NA, "Ana", "Ana", "Bill", "Bill", "Bill", "Claudette", "Claudette", "Danny",
                "Fred", "Fred", "Grace", "Larry", "Larry", "Nicholas", "Nicholas"
            ),
            Employee = c(
                "Ana", "Bill", "Larry", "Claudette", "Danny", "Erika", "Fred", "Grace",
                "Henri", "Ida", "Joaquin", "Kate", "Mindy", "Nicholas", "Odette", "Peter"
            ),
            Title = c(
                "President", "VP Operations", "VP Finance", "Director", "Director", "Scientist",
                "Manager", "Manager", "Jr Scientist", "Operator", "Operator", "Associate",
                "Analyst", "Director", "Accountant", "Accountant"
            )
        )
        collapsibleTree(org, c("Manager", "Employee"), 
                        fill = "lightsteelblue",
                        collapsed = FALSE)
    })
}

