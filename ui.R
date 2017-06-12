shinyUI(fluidPage(
  titlePanel("Saving cacultor"),
  sidebarLayout(
    sidebarPanel(
      textInput("target", "Target(NTD):", 0,width=300),
      div(style="display:inline-block",textInput("savings", "Savings(NTD):", 0,width=200)),
      div(style="display:inline-block",textInput("savingratio", "Saving ratio(%):", 0,width=200)),
      div(),
      div(style="display:inline-block",textInput("tool1", "Tool 1(%):", value = 0,width=120)),
      div(style="display:inline-block",textInput("tool2", "Tool 2(%):", value = 0,width=120)),
      div(style="display:inline-block",textInput("tool3", "Tool 3(%):", value = 0,width=120)),
      
      div(style="display:inline-block",textInput("ratio1", "ratio 1/year(%):", value = 0,width=120)),
      div(style="display:inline-block",textInput("ratio2", "ratio 2/year(%):", value = 0,width=120)),
      div(style="display:inline-block",textInput("ratio3", "ratio 3/year(%):", value = 0,width=120)),
      
      textInput("period", "saving period(year):", 0,width=200),
      radioButtons("radio", label = "Compound interest by", choices = list("Every Year" = 1, "Every Month" = 2)),
      actionButton("goButton", label = "Go!",width=150),
      
      h4('P.S.'),
      
      p('1. Enter the correspond number above'),
      
      p('2. press GO! and output the report')
      
      
    
    ),
    mainPanel(
      
      tabsetPanel(
      tabPanel("Visualization",
               #Wrapping plots in a fluidRow provides easy control over individual plot attributes
               fluidRow(
                 textOutput("text1"),
                 tags$style(type="text/css", "#text1 { height: 50px; width: 100%; text-align:center; font-size: 30px; display: block;}"),
                 column(12,dataTableOutput('contents')),
                 column(width=12,height=30,plotOutput("plot1")),
                 column(width=12,height=30,plotOutput("plot2"))
               ) 
               
      )
      
    )
    
  )
  )
)
)
