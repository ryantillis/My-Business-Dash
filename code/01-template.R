library(shiny)
library(shinyjs)
library(shinydashboard)
ui <- fluidPage(
       headerPanel('Assumptions'),
       
       sliderInput('B2', 'Discount Rate (%)', 6,
                    min = 0, max = 100),
       sliderInput('B3', 'Tax Rate (%)', 40,
                    min = 0, max = 100),
       
       h2("DEBUGG CALC"),
       tableOutput('debug'),
       
       h2("Income Statement"),
       tableOutput('income'),
       
       h2("Cash Flow Statement"),
       tableOutput('cash'), 
       
       h2("Balance Sheet"),
       tableOutput('balance'), 
       
       column(3,
       h3("Startup Phase", style = "color:green"),
       numericInput('B5', 'Inital Investment in PPE ($)', 70000),
       numericInput('B6', 'Residual Value for Depreciation Purposes ($)', 0),
       numericInput('B7', 'Useful Life in Years', 7,
                    min = 1),
       numericInput('B9', 'R&D per year During Startup Phase ($)', 20000)
       ),
       

       
       column(3,
              h3("Operating Phase - Sales", style = "color:#cccc00"),
              numericInput('B12', 'Inital Sales Volume (units)', 2000),
              sliderInput('B13', 'Sales Growth per Year (%)', 0, min=0, max=100),
              numericInput('B15', 'Sales Price Per Unit($)', 100,
                           min = 0),
              sliderInput('B16', 'Product Gross margin Pct (%)', 55, min=0,
                           max = 100),
              sliderInput('B17', 'Inflation Rate for Sales & COGS (%)', 0, min=0,
                          max = 100),
              numericInput('B19', 'SG & A - Fixed costs per year ($) (starts in year 1)', 25000,
                           min = 0),
              numericInput('B20', 'SG & A - Variable Costs ($) (starts year 3)', 15,
                           min = 0)
              
       ),
       
       column(3,
              h3("Working Capital - Timing Issues", style = "color:#cccc00"),
              sliderInput('B24', 'Collections in Year of Sale (%)', 90, min=0, max=100),
              sliderInput('B25', 'Collections Following Year (%)', 10, min=0, max=100),
              sliderInput('B27', 'Units of Inventory as Percent yearly sales', 10,
                           min = 0,100),
              sliderInput('B30', 'Percent Suppliers Paid This Year (%)', 95, min=0,
                          max = 100),
              sliderInput('B31', 'Percent Suppliers Paid next year (%)', 5, min=0,
                          max = 100),
              sliderInput('B33', 'Percent Employees Paid This Year (%)', 70, min=0,
                          max = 100),
              sliderInput('B34', 'Percent Employees Paid next year (%)', 30, min=0,
                          max = 100)
              
       ),
       
       column(3,
              h3("Termination/Project Shutdown", style = "color:red"),
              numericInput('B37', 'Proceeds from Disposal of PPE', 5000, min=0),
              numericInput('B38', 'Disposal or Cleanup costs', 2000, min=0),
              sliderInput('B39', 'Markup Over Cost for Sale of Ending Inventory (%)', 0, min=0,
                          max = 100),
              sliderInput('B40', 'Fixed SG&A', 0, min=0,
                          max = 100),
              sliderInput('B41', 'Variable SG&A', 0, min=0,
                          max = 100)
       )
)

server <- function(input, output) {
       
       #Book Depreciation per Year
       Book_dep <- reactive({c(rep((input$B5-input$B6)/input$B7,7), 0)})
       
       #Tax Depreciation per Year
       Tax_dep <- reactive({c(input$E44*input$B5,
                              input$F44*input$B5,
                              input$G44*input$B5,
                              input$H44*input$B5,
                              input$I44*input$B5,
                              input$J44*input$B5,
                              input$K44*input$B5,
                              0)})
       
       #Sales Volume
       sales_vol <- reactive({c(0,
                                0,
                                input$B12,
                                input$B12*(1+input$B13/100),
                                input$B12*(1+input$B13/100)^2,
                                input$B12*(1+input$B13/100)^3,
                                input$B12*(1+input$B13/100)^4,
                                input$B12*(1+input$B13/100)^4*(input$B27/100))})
       #Sales Price
       sales_price <- reactive({c(0,
                                  0,
                                  input$B15,
                                  input$B15*(1+input$B17/100),
                                  input$B15*(1+input$B17/100)^2,
                                  input$B15*(1+input$B17/100)^3,
                                  input$B15*(1+input$B17/100)^4,
                                  input$B15*(1+input$B17/100)^4*(1-input$B16/100))})
       
       #Revenue
       Revenue <- reactive({c(sales_vol()[1]*sales_price()[1],
                              sales_vol()[2]*sales_price()[2],
                              sales_vol()[3]*sales_price()[3],
                              sales_vol()[4]*sales_price()[4],
                              sales_vol()[5]*sales_price()[5],
                              sales_vol()[6]*sales_price()[6], 
                              sales_vol()[7]*sales_price()[7], 
                              sales_vol()[8]*sales_price()[8])})
       
       #Desired Inventory in Units at end of year
       desiredinv <- reactive({c(0, 
                                 0,
                                 sales_vol()[3]*input$B27/100,
                                 sales_vol()[4]*input$B27/100,
                                 sales_vol()[5]*input$B27/100,
                                 sales_vol()[6]*input$B27/100,
                                 sales_vol()[7]*input$B27/100,
                                 0)})
       
       #Inventory in Units at beginning of the year
       invbegyear <- reactive({c(0,
                                 desiredinv()[1:7])})
       
       #Forecasted Sales in units
       forecastsales <- sales_vol
       
       #Production in Units
       productionunits <- reactive({c(desiredinv()[1]-invbegyear()[1]+forecastsales()[1],
                                      desiredinv()[2]-invbegyear()[2]+forecastsales()[2],
                                      desiredinv()[3]-invbegyear()[3]+forecastsales()[3],
                                      desiredinv()[4]-invbegyear()[4]+forecastsales()[4],
                                      desiredinv()[5]-invbegyear()[5]+forecastsales()[5],
                                      desiredinv()[6]-invbegyear()[6]+forecastsales()[6],
                                      desiredinv()[7]-invbegyear()[7]+forecastsales()[7],
                                      desiredinv()[8]-invbegyear()[8]+forecastsales()[8])})
       
       #Production Costs per Unit (inflation adjusted)
       pcostinf <- reactive({c(sales_price()[1]*(1-input$B16/100),
                               sales_price()[2]*(1-input$B16/100),
                               sales_price()[3]*(1-input$B16/100),
                               sales_price()[4]*(1-input$B16/100),
                               sales_price()[5]*(1-input$B16/100),
                               sales_price()[6]*(1-input$B16/100),
                               sales_price()[7]*(1-input$B16/100),
                               0)})
       
       #Inventory in Dollars Beginning of Year
       invdolbeg <- reactive({c(invbegyear()[1]*pcostinf()[1],
                                invbegyear()[2]*pcostinf()[2],
                                invbegyear()[3]*pcostinf()[3],
                                invbegyear()[4]*pcostinf()[4],
                                invbegyear()[5]*pcostinf()[5],
                                invbegyear()[6]*pcostinf()[6],
                                invbegyear()[7]*pcostinf()[7],
                                invbegyear()[8]*sales_price()[8])})
       
       #Production costs by year
       prodcostyear<- reactive({c(pcostinf()[1]*productionunits()[1],
                                  pcostinf()[2]*productionunits()[2],
                                  pcostinf()[3]*productionunits()[3],
                                  pcostinf()[4]*productionunits()[4],
                                  pcostinf()[5]*productionunits()[5],
                                  pcostinf()[6]*productionunits()[6],
                                  pcostinf()[7]*productionunits()[7],
                                  pcostinf()[8]*productionunits()[8])})
       
       #COGS
       cogs <- reactive({c(pcostinf()[1]*forecastsales()[1],
                           pcostinf()[2]*forecastsales()[2],
                           pcostinf()[3]*forecastsales()[3],
                           pcostinf()[4]*forecastsales()[4],
                           pcostinf()[5]*forecastsales()[5],
                           pcostinf()[6]*forecastsales()[6],
                           pcostinf()[7]*forecastsales()[7],
                           pcostinf()[7]*invbegyear()[7])})
       
       #Inventory in Dollars at the end of the year
       invdollendyear <- reactive({c(0,
                                     0,
                                     desiredinv()[1]*pcostinf()[1],
                                     desiredinv()[2]*pcostinf()[2],
                                     desiredinv()[3]*pcostinf()[3],
                                     desiredinv()[4]*pcostinf()[4],
                                     desiredinv()[5]*pcostinf()[5],
                                     desiredinv()[6]*pcostinf()[6],
                                     desiredinv()[7]*pcostinf()[7],
                                     desiredinv()[8]*pcostinf()[8])})
       
       #Gross Margin
       gm <- reactive({Revenue()-cogs()})
       
       #Research and Development
       rd <- reactive({c(input$B9,input$B9,0,0,0,0,0,0)})
       
       #SG & A
       sga <- reactive({
              sga <- c(input$B19+input$B20*sales_vol()[1],
                       input$B19+input$B20*sales_vol()[2],
                       input$B19+input$B20*sales_vol()[3],
                       input$B19+input$B20*sales_vol()[4],
                       input$B19+input$B20*sales_vol()[5],
                       input$B19+input$B20*sales_vol()[6],
                       input$B19+input$B20*sales_vol()[7],
                       input$B40+input$B41*sales_vol()[8])
              })

       #Long Term Assets, initial PPE investment less depreciation
       
       LTA <- reactive({c(input$B5,input$B5-Book_dep())})
       
       #Other Loses (gains) ALERT: Need to add k93 to this portion for recursive asset changes
       ol <- reactive({c(0,0,0,0,0,0,0,(-1)*((input$B37-LTA()[9])-input$B38))})       
       
       #Pre-Tax Income
       pti <- reactive({c(gm()-(Book_dep()+rd()+sga()+ol()))})
       
       #Tax Benefit
       te <- reactive({c(input$B3*(gm()-(Tax_dep()+rd()+sga()+ol())))})

       #Net Income
       ni <- reactive({c(pti()-te())})
       
       output$income <- renderTable({
              income <- rbind(Revenue(),cogs(),gm(),Book_dep(), Tax_dep(),rd(), sga(),ol(),pti(),te(),ni())
              colnames(income) <- c("Year 1","Year 2","Year 3","Year 4","Year 5","Year 6","Year 7","Year 8")
              row.names(income) <- c("REVENUE", "COGS","GROSS MARGIN", "BOOK DEPRECIATION", "TAX DEPRECIATION", "R & D", "SG & A", "Other Losses", "Pre-Tax Income", "Tax Benefit", "Net Income")
              income <- format(income,digits=2)
              income}, rownames = TRUE)
       
       
       ##Begin Cash Flow Statement
       
       #Accounts Receivable
       AR <- reactive({c(0,Revenue()[1:7]*input$B25/100,0)})
       
       #Change in Accounts Receivable
       CAR <- reactive({c(AR()[2]-AR()[1],
                          AR()[3]-AR()[2],
                          AR()[4]-AR()[3],
                          AR()[5]-AR()[4],
                          AR()[6]-AR()[5],
                          AR()[7]-AR()[6],
                          AR()[8]-AR()[7],
                          AR()[9]-AR()[8])})
       
       #Change in Inventory
       
       CINV <- reactive({c(invdollendyear()[2]-invdollendyear()[1],
                           invdollendyear()[3]-invdollendyear()[2],
                           invdollendyear()[4]-invdollendyear()[3],
                           invdollendyear()[5]-invdollendyear()[4],
                           invdollendyear()[6]-invdollendyear()[5],
                           invdollendyear()[7]-invdollendyear()[6],
                           invdollendyear()[8]-invdollendyear()[7],
                           invdollendyear()[9]-invdollendyear()[8])})
       
       #Accounts Payable
       
       AP <- reactive({c(0,prodcostyear()[1:7]*input$B31/100,0)})
       
       #Change in Accounts Payable
       
       CAP <- reactive({c(AP()[2]-AP()[1],
                          AP()[3]-AP()[2],
                          AP()[4]-AP()[3],
                          AP()[5]-AP()[4],
                          AP()[6]-AP()[5],
                          AP()[7]-AP()[6],
                          AP()[8]-AP()[7],
                          AP()[9]-AP()[8])})
       
       #Wages payable
       
       WP <- reactive({c(0,sga()[1:7]*input$B34/100,0)})
       
       #Changes in Wages Payable
       
       CWP <- reactive({c(WP()[2]-WP()[1],
                          WP()[3]-WP()[2],
                          WP()[4]-WP()[3],
                          WP()[5]-WP()[4],
                          WP()[6]-WP()[5],
                          WP()[7]-WP()[6],
                          WP()[8]-WP()[7],
                          WP()[9]-WP()[8])})
       
       #Other
       Oth <- reactive({c(0,0,0,0,0,0,0,input$B37-LTA()[8])})
       
       #Cash From Operations
       CFO <- reactive({c(ni()+Book_dep()+CAR()+CAP()+CWP()+Oth())})
       
       #PPE INVEST
       PPVEST <- reactive({c((-1)*input$B5,0,0,0,0,0,0,0,0)})
       
       #Disposal of PPE !!!!ALERT possibility for advanced tabes to adjust PPE and other per year 
       DISP <- reactive({c(0,0,0,0,0,0,0,0,input$B37)})
       
       #Net Cash Flow
       
       NCF <- reactive({c(CFO()+DISP()+PPVEST())})
       
       output$DEBUG <- renderTable({
              DEBUG <- list(Book_dep(),Tax_dep(),sales_vol(),sales_price(),Revenue(),desiredinv(), invbegyear(),forecastsales(),productionunits(),pcostinf(),invdolbeg(),prodcostyear(),cogs(),invdollendyear())
              #colnames(DEBUG) <- c("Year 0", "Year 1","Year 2","Year 3","Year 4","Year 5","Year 6","Year 7","Year 8")
              #row.names(DEBUG) <- c("NET INCOME", "ADD DEPRECIATION","GROSS MARGIN", "- Change Accts Rec", "- Change Inv", "+ Change in Accts Pay", "+Change in Wage paya", "Other", "Cash from Operations", "PPE Investment", "PPE Disposal","NET CASH FLOW")
              #DEBUG <- format(cash,digits=2)
              DEBUG})
       
       output$cash <- renderTable({
              cash <- rbind(ni(),Book_dep(),gm(),CAR(),CINV(), CAP(),CWP(),Oth(),CFO(),PPVEST(),DISP(),NCF())
              colnames(cash) <- c("Year 0", "Year 1","Year 2","Year 3","Year 4","Year 5","Year 6","Year 7","Year 8")
              row.names(cash) <- c("NET INCOME", "ADD DEPRECIATION","GROSS MARGIN", "- Change Accts Rec", "- Change Inv", "+ Change in Accts Pay", "+Change in Wage paya", "Other", "Cash from Operations", "PPE Investment", "PPE Disposal","NET CASH FLOW")
              cash <- format(cash,digits=2)
              cash}, rownames = TRUE)
       
       bal_0 <- reactive({c((-1)*input$B5,
                            input$B25*Revenue()[1],
                            invdollendyear()[1],
                            (-1)*input$B5-input$B25*Revenue()[1]-invdollendyear()[1],
                            input$B5,
                            0,
                            input$B5,
                            (-1)*input$B5-input$B25*Revenue()[1]-invdollendyear()[1]+input$B5
                            )})

       bal_1 <- reactive({c((-1)*input$B5,
                            input$B25*Revenue()[2],
                            invdollendyear()[2],
                            (-1)*input$B5-input$B25*Revenue()[2]-invdollendyear()[2],
                            input$B5,
                            (-1)*sum(Book_dep()[1]),
                            input$B5-sum(Book_dep()[1]),
                            (-1)*input$B5-input$B25*Revenue()[1]-invdollendyear()[1]+input$B5-sum(Book_dep()[1])
       )})
       

       
                            output$balance <- renderTable({
                                   balance <- cbind(bal_0(),bal_1())
                                   colnames(balance) <- c("Year 1","Year 2")
                                   row.names(balance) <- c("CASH", "ACC REC","INVENTORY", "CURRENT ASSENTS", "PPE", "ACC DEPRECIATION", "LONG TERM ASSETS", "TOTAL ASSETS")
                                   balance <- format(balance,digits=2)
                                   balance}, rownames = TRUE)       

}

shinyApp(ui = ui, server = server)