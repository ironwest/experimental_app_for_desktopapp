library(shiny)
library(readr)
library(ggplot2)
library(DT)
library(plotly)
library(dplyr)
library(stringr)

do  <- readr::read_rds("data/overall.rds")
nendod <- readr::read_rds("data/master/meidicine_nendo.rds")

do <- do %>% left_join(nendod,by="ndb") |> 
  mutate(overall_total = as.numeric(overall_total))


ui <- fluidPage(sidebarLayout(
  sidebarPanel = sidebarPanel(width = 4,
    sliderInput("keta","YJコードの桁を指定",value=11,min = 7,max=12,step = 1),
    dataTableOutput("search")
  ),
  mainPanel = mainPanel(width=8,
    fluidRow(
      column(width=4,selectInput("inout","外来/入院",choices=c("外来","入院"))),
      column(width=4,selectInput("med_type","外用/注射/内服",choices=c("内服","注射","外用")))
    ),
    plotlyOutput("plot"),
    textOutput("ttt")
  )
))

server <- function(input, output, session) {
  mm <- read_rds("data/master/medmaster.rds")
  output$search <- renderDataTable({
    datatable(data = mm, selection="single")
  })
  
  target_code <- reactive({
    req(input$search_rows_selected)
    v <- mm |> 
      slice(input$search_rows_selected) |> 
      pull(yj_code) |> 
      str_sub(1,input$keta)
    return(v)
  })
  
  output$ttt <- renderText({
    req(input$search_rows_selected)
    str_c("現在選択されているYJコード:",target_code(),"桁")
  })
  
  output$plot <- renderPlotly({
    req(input$search_rows_selected)
    
    tdo <- do %>% 
      filter(str_detect(yj_code,target_code())) |> 
      filter(inout == input$inout) |> 
      filter(med_type == input$med_type)
    
    if(nrow(tdo)==0){
      gg <- ggplot()+
        geom_text(aes(x=1,y=1,label="0件抽出されました")) +
        theme_void()
    }else{
      
      gg <- ggplot(tdo) +
        geom_line(aes(x = nendo, y = overall_total, group = yj_code,color=iyakuhin_name)) +
        geom_point(aes(x = nendo, y = overall_total, group = yj_code,color=iyakuhin_name)) +
        scale_color_discrete(name="医薬品名") +
        scale_y_continuous(labels = scales::comma) +
        labs(x = "年度", y = "")
      
      if(input$inout == "外来"){
        gg <- gg + facet_wrap(~ out_ishp)  
      }
    }
    
    gg <- ggplotly(gg)
    return(gg)
  })
}

shinyApp(ui, server)