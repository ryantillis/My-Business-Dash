library(shiny)
library(shinyjs)
library(FinCal)
ui <- fluidPage(
       headerPanel('Assumptions'),
       
       sliderInput('B2', 'Discount Rate (%)', 6,
                    min = 0, max = 100),
       sliderInput('B3', 'Tax Rate (%)', 40,
                    min = 0, max = 100),
       textOutput('NPVo'),
       textOutput('NPV'),
       textOutput('irr'),
       h2("Cash Flow Statement"),
       tableOutput('cash'), 
       
       h2("Income Statement"),
       tableOutput('income'),
       
       h2("Balance Sheet"),
       h3("Assets"),
       tableOutput('balance'),
       h3("Liabilities"),
       tableOutput('lia'),
       h3("Owner's Equity"),
       tableOutput('OE'),
       
       
       column(3,
       h3("Startup Phase", style = "color:green"),
       numericInput('B5', 'Inital Investment in PPE ($)', 70000),
       numericInput('B6', 'Residual Value for Depreciation Purposes ($)', 0),
       numericInput('B7', 'Useful Life in Years', 7,
                    min = 1),
       numericInput('B9', 'R&D per year During Startup Phase ($)', 20000),
              column(5, numericInput('E44','1',.29),
                     numericInput('F44','2',.2),
                     numericInput('G44','3',.15),
                     numericInput('H44','4',.1),
                     numericInput('I44','5',.08),
                     numericInput('J44','6',.06),
                     numericInput('K44','7',.05)
                     )
       
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
       invdollendyear <- reactive({c(0,0,
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
       
       #Other Loses (gains) ALERT: Need to add k93 -LTA()[9] to this portion for recursive asset changes
       ol <- reactive({c(0,0,0,0,0,0,0,(-1)*(input$B37-input$B38))})       
       
       #Pre-Tax Income
       pti <- reactive({pti <- c(gm()-(Book_dep()+rd()+sga()+ol()))})
       #output$pti<-renderText({pti})
       
       #Tax Benefit
       te <- reactive({c(input$B3/100*(gm()-(Tax_dep()+rd()+sga()+ol())))})

       #Net Income
       ni <- reactive({c(pti()-te())})
       ni2 <-reactive({c(0,ni())})
       
       #Add Depreciation
       
       AD <- reactive({c(0, Book_dep())})
       
       output$income <- renderTable({
              income <- rbind(Revenue(),cogs(),gm(),Book_dep(), Tax_dep(),rd(), sga(),ol(),pti(),te(),ni())
              colnames(income) <- c("Year 1","Year 2","Year 3","Year 4","Year 5","Year 6","Year 7","Year 8")
              row.names(income) <- c("REVENUE", "COGS","GROSS MARGIN", "BOOK DEPRECIATION", "TAX DEPRECIATION", "R & D", "SG & A", "Other Losses", "Pre-Tax Income", "Tax Benefit", "Net Income")
              income <- format(income,digits=2)
              income}, rownames = TRUE)
       
       
       ##Begin Cash Flow Statement
       
       BD2 <- reactive({c(Book_dep(),0)})
       
       #Accounts Receivable
       AR <- reactive({c(0,0,(-1)*Revenue()[1:7]*input$B25/100,Revenue()[8]-cogs()[8])})
       
       #Change in Accounts Receivable
       CAR <- reactive({c(AR()[2]-AR()[1],
                          AR()[3]-AR()[2],
                          AR()[4]-AR()[3],
                          AR()[5]-AR()[4],
                          AR()[6]-AR()[5],
                          AR()[7]-AR()[6],
                          AR()[8]-AR()[7],
                          AR()[9]-AR()[8],
                          AR()[10]-AR()[9])})
       
       #Change in Inventory
       
       IDEY <- reactive({invdolendyear})
       
       CINV <- reactive({(-1)*c(0,
                           invdollendyear()[3]-invdolbeg()[1],
                           invdollendyear()[4]-invdolbeg()[2],
                           invdollendyear()[5]-invdolbeg()[3],
                           invdollendyear()[6]-invdolbeg()[4],
                           invdollendyear()[7]-invdolbeg()[5],
                           invdollendyear()[8]-invdolbeg()[6],
                           invdollendyear()[9]-invdolbeg()[7],
                           invdollendyear()[10]-invdolbeg()[8])})
       
       #Accounts Payable
       
       AP <- reactive({c(0,prodcostyear()[1:7]*input$B31/100,0)})
       
       #Change in Accounts Payable
       
       CAP <- reactive({c(0,
                          AP()[2]-AP()[1],
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
       
       CWP <- reactive({c(0,
                          WP()[2]-WP()[1],
                          WP()[3]-WP()[2],
                          WP()[4]-WP()[3],
                          WP()[5]-WP()[4],
                          WP()[6]-WP()[5],
                          WP()[7]-WP()[6],
                          WP()[8]-WP()[7],
                          WP()[9]-WP()[8])})
       
       #Other
       Oth <- reactive({c(0,0,0,0,0,0,0,0,(-1)*input$B37)})#-LTA()[8]?
       
       #Cash From Operations
       CFO <- reactive({c(ni2()+AD()+CAR()+CINV()+CAP()+CWP()+Oth())})#9999
       
       #PPE INVEST
       PPVEST <- reactive({c((-1)*input$B5,0,0,0,0,0,0,0,0)})
       
       #Disposal of PPE !!!!ALERT possibility for advanced tabes to adjust PPE and other per year 
       DISP <- reactive({c(0,0,0,0,0,0,0,0,input$B37)})
       
       #Net Cash Flow
       
       NCF <- reactive({c(CFO()+DISP()+PPVEST())})
       
       #NPV and IRR Calculations
       
       output$NPVo <- renderText({npv(input$B2/100,NCF())+input$B5})
       output$NPV <- renderText({npv(input$B2/100,NCF())})
       output$irr <- renderText({irr(NCF())})
       
       output$cash <- renderTable({
              cash <- rbind(ni2(),AD(),CAR(),CINV(), CAP(),CWP(),Oth(),CFO(),PPVEST(),DISP(),NCF())
              colnames(cash) <- c("Start", "Year 1","Year 2","Year 3","Year 4","Year 5","Year 6","Year 7","Year 8")
              row.names(cash) <- c("NET INCOME", "ADD DEPRECIATION", "- Change Accts Rec", "- Change Inv", "+ Change in Accts Pay", "+Change in Wage paya", "Other", "Cash from Operations", "PPE Investment", "PPE Disposal","NET CASH FLOW")
              cash <- format(cash,digits=2)
              cash}, rownames = TRUE)
       
       #Current Assets: Cash
       curcash <- reactive({c((-1)*input$B5,
                              (-1)*input$B5+sum(NCF()[2]),
                              (-1)*input$B5+sum(NCF()[2:3]),
                              (-1)*input$B5+sum(NCF()[2:4]),
                              (-1)*input$B5+sum(NCF()[2:5]),
                              (-1)*input$B5+sum(NCF()[2:6]),
                              (-1)*input$B5+sum(NCF()[2:7]),
                              (-1)*input$B5+sum(NCF()[2:8]),
                              (-1)*input$B5+sum(NCF()[2:9])
                              )})
       
       #Total Current Assents
       
       TCA <- reactive({c(curcash()+(-1)*AR()[2:10]+invdollendyear()[2:10])})
       
       #Property Plant and Equipment
       PPE <-reactive({c(rep(input$B5,8),0)})
       
       #Less Accumulated Depreciation
       
       LAD <- reactive({c(0,
                          Book_dep()[1], 
                          sum(Book_dep()[1:2]), 
                          sum(Book_dep()[1:3]), 
                          sum(Book_dep()[1:4]), 
                          sum(Book_dep()[1:5]),
                          sum(Book_dep()[1:6]),
                          sum(Book_dep()[1:7]),
                          0
                          )})
       
       #Long Term Assets
       LTA <- reactive({PPE()-LAD()})
       
       #Total Assets
       TA <- reactive({TCA()+LTA()})
       
       #Other Liabilities ALERT: Future Variable
       
       oth <- rep(0,9)
       
       #Total Liabilituies
       
       TL <- reactive({c(AP()+WP()+oth)})
       
       #Contributed Capital ALERT: Future Variable
       
       CC <- rep(0,9)
       
       #Retained Earning
       
       RE <- reactive({c(sum(ni()[1]),
                         sum(ni()[1:2]),
                         sum(ni()[1:3]),
                         sum(ni()[1:4]),
                         sum(ni()[1:5]),
                         sum(ni()[1:6]),
                         sum(ni()[1:7]),
                         sum(ni()[1:8]),
                         sum(ni()[1:9])
       )})
       
       #Total Owner Equity
       
       TOE <- reactive({c(CC+RE())})
       
       #Total Liabilities and Owner Equity
       
       TLOE <- reactive({c(TL()+TOE())})
       
       output$balance <- renderTable({
              balance <- rbind(curcash(),(-1)*AR()[2:10],invdollendyear()[2:10],TCA(),PPE(),LAD(),LTA(),TA(),AP(), WP())
              colnames(balance) <- c("Start","Year 1","Year 2","Year 3","Year 4","Year 5","Year 6","Year 7","Year 8")
              row.names(balance) <- c("CASH","ACC RECEIVABLE", "INVENTORY","Total Current Assets", "PPE", "Less Acc Depr", "Long Term Assets","Total Assets","Acc PAY","Wages Pay")
              balance <- format(balance,digits=2)
              balance}, rownames = TRUE)   
       
       output$lia <- renderTable({
              balance <- rbind(AP(), WP(),oth,TL())
              colnames(balance) <- c("Start","Year 1","Year 2","Year 3","Year 4","Year 5","Year 6","Year 7","Year 8")
              row.names(balance) <- c("Acc PAY","Wages Pay", "Other", "Total Liabilities")
              balance <- format(balance,digits=2)
              balance}, rownames = TRUE)   
       
       output$OE <- renderTable({
              balance <- rbind(CC,RE(),TOE(),TLOE())
              colnames(balance) <- c("Start","Year 1","Year 2","Year 3","Year 4","Year 5","Year 6","Year 7","Year 8")
              row.names(balance) <- c("CONTRIBUTED CAPITAL","RETAINED EARNINGS", "TOTAL OWNER'S EQUITY", "TOTAL LIA and OE")
              balance <- format(balance,digits=2)
              balance}, rownames = TRUE)   
       
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
       

}

shinyApp(ui = ui, server = server)