shinyUI(fluidPage(
  titlePanel("Saving cacultor"),
  sidebarLayout(
    sidebarPanel(
      textInput("target", "目標儲蓄金額(元):", 0,width=300),
      div(style="display:inline-block",textInput("savings", "目前存款金額(元):", 0)),
      div(style="display:inline-block",textInput("savingratio", "存款年利率(%):", 0)),
      div(),
      div(style="display:inline-block",textInput("tool1", "第一投資工具比例(%):", value = 0)),
      div(style="display:inline-block",textInput("tool2", "第二投資工具比例(%):", value = 0)),
      div(style="display:inline-block",textInput("tool3", "第三投資工具比例(%):", value = 0)),
      
      div(style="display:inline-block",textInput("ratio1", "第一投資報酬率/年(%):", value = 0)),
      div(style="display:inline-block",textInput("ratio2", "第二投資報酬率/年(%):", value = 0)),
      div(style="display:inline-block",textInput("ratio3", "第三投資報酬率/年(%):", value = 0)),
      
      #textInput("loan", "預計貸款金額(元):", 0,width=200),
      #textInput("loanrate", "預計貸款利率(%):", 0,width=200),
      textInput("period", "預估儲蓄時間(年):", 0,width=200),
      radioButtons("radio", label = "複利次數", choices = list("每年" = 1, "每月" = 2)),
      actionButton("goButton", label = "Go!",width=150),
      
      h4('說明:'),
      
      p('1. 請依照上列要求填入對照金額'),
      
      p('2. 按下Go!則輸出報表')
      
      
    
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
