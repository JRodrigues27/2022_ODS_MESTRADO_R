library(tidyverse)
library(shinydashboard) # elaborações de paineis no ambiente shiny
library(readxl)
library(splitstackshape)  #divide colunas
library(ggalluvial)
library(plotly)
library(lubridate)
library(DT)
library(highcharter) #https://rpubs.com/techanswers88/sankey

#CRIACAO DOS OBJETOS BANCO DE DADOS ----

#IMPORTA A ABA PAVS:
bd.pavs <- read_excel("2022_MESTRADO_ODS_BD.xlsx", 
                               sheet = "PAVS", col_types = c("text", 
                                                                            "text", "text", "text", "text", "text", 
                                                                            "text", "text", "text", "text", "text"))


#IMPORTA A ABA PLANO_SAUDE_2022_25:
bd.plano.saude <- read_excel("2022_MESTRADO_ODS_BD.xlsx", 
                             sheet = "PLANO_SAUDE_2022_25", col_types = c("text", 
                                                                          "text", "text", "text", "text", "text", 
                                                                          "text", "text", "text", "text", 
                                                                          "text", "text", "text", "text", "text"))
#IMPORTA A ABA PLAMEP:
bd.plamep <- read_excel("2022_MESTRADO_ODS_BD.xlsx", 
                        sheet = "PLAMEP", col_types = c("text", 
                                                        "text", "text", "text", "text", "text", 
                                                        "text", "text", "text", "text", "text", 
                                                        "text", "text", "text", "text", "text", 
                                                        "text", "text", "text", "text", 
                                                        "text", "text", "text", "text", "text"))

#IMPORTA A ABA ODS_SAO_METAS:
bd.metas <- read_excel("2022_MESTRADO_ODS_BD.xlsx", 
                       sheet = "ODS_SAO_METAS", col_types = c("text", "text", "text","text",
                                                              "text", "text", "text", "text", "text", 
                                                              "text", "text", "text", "text"))
#IMPORTA A ABA ODS_MUNIC_INDICE:
bd.metas.ind <- read_excel("2022_MESTRADO_ODS_BD.xlsx", 
                           sheet = "ODS_MUNIC_INDICE",
                           col_types = c("text","text", "text", "text", "text", 
                                         "text", "text"))
#IMPORTA A ABA PAVS_ACOES:
bd.acoes <- read_excel("2022_MESTRADO_ODS_BD.xlsx", 
                       sheet = "PAVS_ACOES", 
                       col_types = c("text", "text", "text")) %>% 
  cSplit("Doencas/ Agravos", ",", "long", type.convert= FALSE) %>% 
  drop_na(`Doencas/ Agravos`)

#IMPORTA A ABA DOENCAS_AGRAVOS:
bd.agravos <- read_excel("2022_MESTRADO_ODS_BD.xlsx", 
                         sheet = "DOENCAS_AGRAVOS", 
                         col_types = c("text", "text", "numeric", "text", "text"))

#UNINDO OS BANCO DE DADOS DE PAVS_ACOES E AGRAVOS
bd.acoes.agravos <- merge(bd.acoes, bd.agravos, all = TRUE) %>% 
  drop_na(`Risco ambiental`)

#UNINDO OS BANCO DE DADOS E METAS SAÚDE ----
bd.1 <- bd.pavs %>% 
  select(`ODS - NUMERO`, `ODS - NOME`, `OBJETIVO - ONU`,
         `OBJETIVO - OMS`, `ACOES PARA CONTRIBUICAO NO ATENDIMENTO DAS METAS`,
         `Meta ODS Municipal - NUMERO`, `Indicadores selecionado - NUMERO`, INDICADOR, `REFERENCIA`, `ESTRATEGIA-ETAPA`)

bd.2 <- bd.plano.saude %>% 
  select(`ODS - NUMERO`, `ODS - NOME`, `OBJETIVO - ONU`,
         `OBJETIVO - OMS`, `ACOES PARA CONTRIBUICAO NO ATENDIMENTO DAS METAS`,
         `Meta ODS Municipal - NUMERO`, `Indicadores selecionado - NUMERO`, INDICADOR, `REFERENCIA`)


bd.3 <- bd.plamep %>% 
  select(`ODS - NUMERO`, `ODS - NOME`, `OBJETIVO - ONU`,
         `OBJETIVO - OMS`, `ACOES PARA CONTRIBUICAO NO ATENDIMENTO DAS METAS`,
         `Meta ODS Municipal - NUMERO`, `Indicadores selecionado - NUMERO`, INDICADOR, `REFERENCIA`)

bd.ods <- bind_rows(bd.1, bd.2, bd.3)

bd.ods <- cSplit(bd.ods, "Meta ODS Municipal - NUMERO", ",", "long", type.convert= FALSE)

bd.ods.merge <- merge(bd.ods, bd.metas.ind, all = TRUE) %>% 
  drop_na(`ACOES PARA CONTRIBUICAO NO ATENDIMENTO DAS METAS`, REFERENCIA)

# CORRELACOES ----
correlacoes_ods_inst <- bd.ods.merge %>% 
  select(REFERENCIA, `ODS - NOME`)%>% 
  arrange(desc(REFERENCIA))

correlacoes_ods_pavs <- bd.ods.merge %>% 
  select(-INDICADOR) %>% 
  filter(REFERENCIA == "PAVS") %>%
  cSplit("Indicadores selecionado - NUMERO", ",", "long", type.convert= FALSE) %>% 
  filter(map2_lgl(`Indicadores selecionado - NUMERO`,`Meta ODS Municipal - NUMERO`,   str_detect)) %>% 
  distinct() %>% 
  merge(bd.metas, by = "Indicadores selecionado - NUMERO") %>% #all = TRUE) %>% #
  drop_na(INDICADOR) %>% 
  transmute(`ODS - NUMERO` = `ODS - NUMERO.x`,
            `ODS - NOME` = `ODS - NOME.x`,
            `OBJETIVO - ONU` = `OBJETIVO - ONU.x`,
            `OBJETIVO - OMS` = `OBJETIVO - OMS.x`,
            `Meta ODS Municipal - NUMERO` = `Meta ODS Municipal - NUMERO.x`,
            `Meta ODS Global - DESCRICAO` = `Meta ODS Global - DESCRICAO.x`,
            `Meta ODS Municipal - DESCRICAO` = `Meta ODS Municipal - DESCRICAO.x`,
            `ACOES PARA CONTRIBUICAO NO ATENDIMENTO DAS METAS`= `ACOES PARA CONTRIBUICAO NO ATENDIMENTO DAS METAS`,
            `Indicadores selecionado - NUMERO` = `Indicadores selecionado - NUMERO`,
            INDICADOR = INDICADOR,
            `Fórmula de cálculo` = `Fórmula de cálculo`,
            `REFERENCIA.x` = `REFERENCIA.x`,
            `REFERENCIA.y` = `REFERENCIA.y`
  )

bd.ods.etapa <- bd.ods.merge %>% 
    mutate(`ESTRATEGIA-ETAPA` = if_else(is.na(`ESTRATEGIA-ETAPA`), 
                 REFERENCIA,
                 `ESTRATEGIA-ETAPA`, missing = NULL)) %>% 
  select(`ODS - NOME`,
         `OBJETIVO - OMS`,
         REFERENCIA,
         `ESTRATEGIA-ETAPA`) %>% 
  distinct()

#6. UI - Interface do Usuario ----
loadingLogo <- function(href, src, loadingsrc, height = NULL, width = NULL, alt = NULL) {
  tagList(
    tags$head(
      tags$script(
        "setInterval(function(){
                     if ($('html').attr('class')=='shiny-busy') {
                     $('div.busy').show();
                     $('div.notbusy').hide();
                     } else {
                     $('div.busy').hide();
                     $('div.notbusy').show();
           }
         },100)")
    ),
    tags$a(href=href,
           div(class = "busy",  
               img(src=loadingsrc,height = height, width = width, alt = alt)),
           div(class = 'notbusy',
               img(src = src, height = 39, width = 300, alt = 30))
           #img(src = src, height = height, width = width, alt = alt))
    )
  )
}

ui <- dashboardPage(title = "PAINEL SAUDE AMBIENTAL - ODS SANTA MARCELINA SAÚDE",
                    skin = "blue",
                    header = dashboardHeader(
                      title = loadingLogo('https://aps.santamarcelina.org/saude-e-meio-ambiente/',
                                          'logo_mestrado.png',
                                          'carregando.png'), #"MONITORAMENTO PAVS - STS", 
                      titleWidth = 407,
                      dropdownMenu(
                        type = "messages",
                        messageItem(
                          from = "PAVS",
                          message = "Saiba mais sobre o PAVS",
                          href = "https://www.prefeitura.sp.gov.br/cidade/secretarias/saude/atencao_basica/pavs/index.php?p=215712",
                          icon = icon("book-medical")
                        ),
                        messageItem(
                          from = "GEOSAMPA",
                          message = "Conheça o Geosampa",
                          href = "http://geosampa.prefeitura.sp.gov.br/PaginasPublicas/_SBC.aspx",
                          icon = icon("globe")
                        )
                      ) #dropdownMenu
                    ), #dashboardHeader
                    
                    sidebar = dashboardSidebar(
                      h2("PAINEL SAÚDE AMBIENTAL - ODS SANTA MARCELINA"),
                      selectInput("estrategias", "SELECIONE A ESTRATÉGIA DE ATUAÇÃO:", 
                                  sort(unique(bd.ods.merge$REFERENCIA)),selected = "Programa Selo Socioambiental Santa Marcelina"),
                      uiOutput("acoes"),
                      width = 450,
                      tags$head(tags$style(HTML(".selectize-input {height: 50px; width: 400px; font-size: 14px;}"))), #ajusta o tamanho das caixas de seleção
                      tags$style(type="text/css", "#relatorio_unidade {background-color:white;color: black;font-family: Courier New}"),
                      hr(style = "border-top: 2px solid white;",
                         h4(textOutput("titulo_pavs_download")),
                         downloadButton(
                           "relatorio_unidade",
                           "DOWNLOAD",
                           class = "butt1")
                      )
                    ), # fecha sidebar
                    
                    body = dashboardBody(
                      fluidPage( 
                        tabBox(
                          width = 12,
                          #title = "Painel ODS Santa Marcelina Saúde",
                          tabPanel("ODS - Estratégias e ações",
                                   fluidRow(
                                     box(title = "CORRELAÇÕES ODS ESTRATÉGIAS INSTITUCIONAIS",
                                         status = "primary",
                                         solidHeader = TRUE,
                                         width = 12,
                                         highchartOutput("correlacoes_institucionais",height="500px"),
                                         style = "height:500px; overflow-y: scroll;overflow-x: scroll;",
                                         collapsible = TRUE
                                     ),
                                     box(title = textOutput("titulo_estrategias_ods"),
                                         status = "primary",
                                         solidHeader = TRUE,
                                         width = 12,
                                         DTOutput('etapa.ods'),
                                         style = "height:500px; overflow-y: scroll;overflow-x: scroll;",
                                         collapsible = TRUE    
                                     ),
                                     box(title = textOutput("titulo_correlacoes"),
                                         status = "primary",
                                         solidHeader = TRUE,
                                         width = 12,
                                         highchartOutput("correlacoes_estrategias",height="500px"),
                                         style = "height:500px; overflow-y: scroll;overflow-x: scroll;",
                                         collapsible = TRUE
                                     )
                                   ) #fecha fluidRow
                          ),#ODS - Estratégias e ações
                          
                          tabPanel("ODS - Objetivos e Indicadores",
                                   fluidRow(
                                     box(title = textOutput("titulo_metas"),
                                         status = "primary",
                                         solidHeader = TRUE,
                                         width = 12,
                                         DTOutput('detalhamento'),
                                         style = "height:500px; overflow-y: scroll;overflow-x: scroll;",
                                         collapsible = TRUE
                                     ),
                                     box(title = textOutput("titulo_indicadores"),
                                         status = "primary",
                                         solidHeader = TRUE,
                                         width = 12,
                                         DTOutput('indicadores'),
                                         style = "height:500px; overflow-y: scroll;overflow-x: scroll;",
                                         collapsible = TRUE
                                     )
                                     
                                   )
                          ),
                          tabPanel("PAVS - Ações e Indicadores",
                                   fluidRow(
                                     box(title = textOutput("titulo_pavs_ods"),
                                         status = "primary",
                                         solidHeader = TRUE,
                                         width = 12,
                                         DTOutput('pavs.ods'),
                                         style = "height:250px; overflow-y: scroll;overflow-x: scroll;",
                                         collapsible = TRUE    
                                     ),
                                     box(title = textOutput("titulo_acoes"),
                                         status = "primary",
                                         solidHeader = TRUE,
                                         width = 12,
                                         highchartOutput("correlacoes_pavs",height="500px"), collapsible = TRUE
                                     ),
                                     box(title = textOutput("titulo_acoes_ods"),
                                         status = "primary",
                                         solidHeader = TRUE,
                                         width = 12,
                                         DTOutput('tabela.indicadores'),
                                         style = "height:500px; overflow-y: scroll;overflow-x: scroll;",
                                         collapsible = TRUE    
                                     ),
                                     box(title = "Fração atribuída aos riscos ambientais",
                                         status = "primary",
                                         solidHeader = TRUE,
                                         width = 12,
                                         plotly::plotlyOutput("grafico.agravos"),collapsible = TRUE    
                                     ),
                                     box(title = "Ações sugeridas",
                                         status = "primary",
                                         solidHeader = TRUE,
                                         width = 12,
                                         DTOutput('tabela.agravos'),
                                         style = "height:500px; overflow-y: scroll;overflow-x: scroll;",
                                         collapsible = TRUE    
                                     )
                                   )
                          )
                          
                        ) #fecha tabBox
                      ) #FECHA fluidPage
                      
                    ) #FECHA DASHBOARD BODY
) # FECHA UI




# Define server logic required to draw a histogram
server <- function(input, output) {
  
  indicadores_filtrados <- reactive({
    bd.ods.merge %>% filter(`ACOES PARA CONTRIBUICAO NO ATENDIMENTO DAS METAS` %in% input$acoes) %>% 
      drop_na(`Meta ODS Municipal - DESCRICAO`) %>% 
      select(`Meta ODS Municipal - DESCRICAO`)
  })
  
  
  
  indicadores_unir <- reactive({
    bd.ods.merge %>% filter(`ACOES PARA CONTRIBUICAO NO ATENDIMENTO DAS METAS` %in% input$acoes) %>% 
      filter(!is.na(INDICADOR)) %>% 
      select(`Meta ODS Municipal - DESCRICAO`, INDICADOR, REFERENCIA) %>% 
      distinct(INDICADOR)
  })
  
  corr_pavs <- reactive({
    correlacoes_ods_pavs %>% filter(`ACOES PARA CONTRIBUICAO NO ATENDIMENTO DAS METAS` %in% input$acoes) %>% 
      filter(!is.na(INDICADOR))
  })
  
  pavs_agravos <- reactive({
    bd.acoes.agravos %>% 
      filter(`ACOES PARA CONTRIBUICAO NO ATENDIMENTO DAS METAS` %in% input$acoes)
  })
  
  
  output$grafico.agravos <- plotly::renderPlotly({
    plot_ly(pavs_agravos(), x = ~`Doencas/ Agravos`, y = ~`Risco ambiental`*100, 
            color = ~ CATEGORIAS, 
            size = ~`Risco ambiental`, 
            type = 'scatter', mode = 'markers', 
            marker = list(symbol = 'circle', sizemode = 'diameter',
                          line = list(width = 2, color = '#FFFFFF')), 
            hoverinfo = 'text',
            text = ~paste('<b>AGRAVO:</b>', `Doencas/ Agravos`, '<br>',
                          '<br>Fração ambiental atribuída:', `Risco ambiental`*100,'%',
                          '<br>Método utilizado:', Metodo)) %>% 
      layout(annotations=list(
        list(text=paste0(unique(corr_pavs()$`ACOES PARA CONTRIBUICAO NO ATENDIMENTO DAS METAS`), 
                         '<br>(doenças ou agravos relacionados e fração ambiental atribuída)'),
             xref="paper",x=0.5,
             yref="paper",y=1,yshift=30,showarrow=FALSE, 
             font=list(size=15,color='rgb(0,0,0)')),
        list(text= 'fonte: WHO,2016' ,
             xref="paper",x=1,xshift= 100,showarrow=FALSE, 
             yref="paper",y=0,yshift= -20,showarrow=FALSE, 
             font=list(size=10 ,color='rgb(0,0,0)'))),
        yaxis = list(title = FALSE,
                     ticksuffix = "%", range = c(0, 150)),
        xaxis = list(title = FALSE)
        
      )
  })
  
  output$titulo_pavs_download <- renderText({
    text_pavs_button <- unique(corr_pavs()$`ACOES PARA CONTRIBUICAO NO ATENDIMENTO DAS METAS`)
    paste("RELATÓRIO PAVS - ODS: ", text_pavs_button)
  }) 
  
  output$titulo_estrategias_ods <- renderText({
    text_correlacoes <- input$estrategias
    paste("ODS relacionadas a estratégia", text_correlacoes)
  }) 
   
  output$titulo_correlacoes <- renderText({
    text_correlacoes <- input$estrategias
    paste("CORRELAÇÕES ODS - AÇÕES ", text_correlacoes)
  })
  
  output$titulo_metas <- renderText({
    text_metas <- input$acoes
    paste("ODS E METAS RELACIONADAS (GLOBAL E MUNICIPAL) ", text_metas)
  })
  
  output$titulo_indicadores <- renderText({
    text_indicadores <- input$acoes
    paste("INDICADORES ODS SUGERIDOS PARA ", text_indicadores)
  })
  
  output$titulo_pavs_ods <- renderText({
    text_pavs <- unique(corr_pavs()$`ACOES PARA CONTRIBUICAO NO ATENDIMENTO DAS METAS`)
    paste(text_pavs, " - ODS")
  }) 
  
   output$titulo_acoes <- renderText({
    text_acoes <- unique(corr_pavs()$`ACOES PARA CONTRIBUICAO NO ATENDIMENTO DAS METAS`)
    paste(text_acoes, " - ODS: correlações")
  })
  
  output$titulo_acoes_ods <- renderText({
    text_acoes <- unique(corr_pavs()$`ACOES PARA CONTRIBUICAO NO ATENDIMENTO DAS METAS`)
    paste("Indicadores ODS possivelmente associados com ", text_acoes)
  })
  
 
  output$correlacoes_institucionais <- renderHighchart({
    highchart() %>%
      hc_add_series(data = data_to_sankey(correlacoes_ods_inst), type = "sankey", 
                    name = "Correlações ODS e estratégias institucionais Santa Marcelina Saúde",
                    hcaes(from = from, to = to, weight = weight),
                    nodes = list(list(id = "01 ERRADICAÇÃO DA POBREZA", color = "#E5243B"),
                                 list(id = "02 FOME ZERO E AGRICULTURA SUSTENTÁVEL", color = "#DDA63A"),
                                 list(id = "03 SAÚDE E BEM ESTAR", color = "#4C9F38"),
                                 list(id = "04 EDUCAÇÃO DE QUALIDADE", color = "#C5192D"),
                                 list(id = "05 IGUALDADE DE GÊNERO", color = "#FF3A21"),
                                 list(id = "06 ÁGUA POTÁVEL E SANEAMENTO", color = "#26BDE2"),
                                 list(id = "07 ENERGIA LIMPA E ACESSÍVEL", color = "#FCC30B"),
                                 list(id = "08 TRABALHO DECENTE E CRESCIMENTO ECONÔMICO", color = "#A21942"),
                                 list(id = "09 INDÚSTRIA, INOVAÇÃO E INFRAESTRUTURA", color = "#FD6925"),
                                 list(id = "10 REDUÇÃO DAS DESIGUALDADES", color = "#DD1367"),
                                 list(id = "11 CIDADES E COMUNIDADES SUSTENTÁVEIS", color = "#FD9D24"),
                                 list(id = "12 CONSUMO E PRODUÇÃO RESPONSÁVEIS", color = "#BF8B2E"),
                                 list(id = "13 AÇÃO CONTRA A MUDANÇA GLOBAL DO CLIMA", color = "#3F7E44"),
                                 list(id = "15 VIDA TERRESTRE", color = "#56C02B"),
                                 list(id = "16 PAZ, JUSTIÇA E INSTITUIÇÕES EFICAZES", color = "#00689D"),
                                 list(id = "17 PARCERIAS E MEIOS DE IMPLEMENTAÇÃO", color = "#19486A"),
                                 list(id = "PAVS", color = "green"),
                                 list(id = "PLAMEP", color = "blue"),
                                 list(id = "Plano Municipal de saúde 2022 - 2025", color = "orange")
                    )) 
  })
  
  
  output$correlacoes_estrategias <- renderHighchart({
    highchart() %>%
      hc_add_series(data = data_to_sankey(bd.ods.merge[bd.ods.merge$REFERENCIA == input$estrategias] %>%  #FILTRANDO O BD A PARTIR DO SELECT INPUT
                                            select(`ODS - NOME`, `ACOES PARA CONTRIBUICAO NO ATENDIMENTO DAS METAS`)
      ), type = "sankey", 
      name = paste0("Correlações ODS e estratégias institucionais Santa Marcelina Saúde - ",input$estrategias),
      hcaes(from = from, to = to, weight = weight),
      nodes = list(list(id = "01 ERRADICAÇÃO DA POBREZA", color = "#E5243B"),
                   list(id = "02 FOME ZERO E AGRICULTURA SUSTENTÁVEL", color = "#DDA63A"),
                   list(id = "03 SAÚDE E BEM ESTAR", color = "#4C9F38"),
                   list(id = "04 EDUCAÇÃO DE QUALIDADE", color = "#C5192D"),
                   list(id = "05 IGUALDADE DE GÊNERO", color = "#FF3A21"),
                   list(id = "06 ÁGUA POTÁVEL E SANEAMENTO", color = "#26BDE2"),
                   list(id = "07 ENERGIA LIMPA E ACESSÍVEL", color = "#FCC30B"),
                   list(id = "08 TRABALHO DECENTE E CRESCIMENTO ECONÔMICO", color = "#A21942"),
                   list(id = "09 INDÚSTRIA, INOVAÇÃO E INFRAESTRUTURA", color = "#FD6925"),
                   list(id = "10 REDUÇÃO DAS DESIGUALDADES", color = "#DD1367"),
                   list(id = "11 CIDADES E COMUNIDADES SUSTENTÁVEIS", color = "#FD9D24"),
                   list(id = "12 CONSUMO E PRODUÇÃO RESPONSÁVEIS", color = "#BF8B2E"),
                   list(id = "13 AÇÃO CONTRA A MUDANÇA GLOBAL DO CLIMA", color = "#3F7E44"),
                   list(id = "15 VIDA TERRESTRE", color = "#56C02B"),
                   list(id = "16 PAZ, JUSTIÇA E INSTITUIÇÕES EFICAZES", color = "#00689D"),
                   list(id = "17 PARCERIAS E MEIOS DE IMPLEMENTAÇÃO", color = "#19486A")  
      )
      )
  })   
  
  output$correlacoes_pavs <-  renderHighchart({
    highchart() %>%
      hc_add_series(data = data_to_sankey(correlacoes_ods_pavs[correlacoes_ods_pavs$`ACOES PARA CONTRIBUICAO NO ATENDIMENTO DAS METAS` == input$acoes] %>%  #FILTRANDO O BD A PARTIR DO SELECT INPUT
                                            select(`ODS - NOME`, `ACOES PARA CONTRIBUICAO NO ATENDIMENTO DAS METAS`)
      ), type = "sankey", 
      name = paste0("Correlações ODS e ",input$acoes),
      hcaes(from = from, to = to, weight = weight),
      nodes = list(list(id = "01 ERRADICAÇÃO DA POBREZA", color = "#E5243B"),
                   list(id = "02 FOME ZERO E AGRICULTURA SUSTENTÁVEL", color = "#DDA63A"),
                   list(id = "03 SAÚDE E BEM ESTAR", color = "#4C9F38"),
                   list(id = "04 EDUCAÇÃO DE QUALIDADE", color = "#C5192D"),
                   list(id = "05 IGUALDADE DE GÊNERO", color = "#FF3A21"),
                   list(id = "06 ÁGUA POTÁVEL E SANEAMENTO", color = "#26BDE2"),
                   list(id = "07 ENERGIA LIMPA E ACESSÍVEL", color = "#FCC30B"),
                   list(id = "08 TRABALHO DECENTE E CRESCIMENTO ECONÔMICO", color = "#A21942"),
                   list(id = "09 INDÚSTRIA, INOVAÇÃO E INFRAESTRUTURA", color = "#FD6925"),
                   list(id = "10 REDUÇÃO DAS DESIGUALDADES", color = "#DD1367"),
                   list(id = "11 CIDADES E COMUNIDADES SUSTENTÁVEIS", color = "#FD9D24"),
                   list(id = "12 CONSUMO E PRODUÇÃO RESPONSÁVEIS", color = "#BF8B2E"),
                   list(id = "13 AÇÃO CONTRA A MUDANÇA GLOBAL DO CLIMA", color = "#3F7E44"),
                   list(id = "15 VIDA TERRESTRE", color = "#56C02B"),
                   list(id = "16 PAZ, JUSTIÇA E INSTITUIÇÕES EFICAZES", color = "#00689D"),
                   list(id = "17 PARCERIAS E MEIOS DE IMPLEMENTAÇÃO", color = "#19486A")  
      )
      )
  })   
  
  output$acoes <- renderUI({
    selectInput("acoes", "Escolha o tipo de ação", choices = bd.ods.merge[bd.ods.merge$REFERENCIA==input$estrategias,"ACOES PARA CONTRIBUICAO NO ATENDIMENTO DAS METAS"])
  })
  
  output$etapa.ods <-  renderDT(
    bd.ods.etapa %>% filter(REFERENCIA %in% input$estrategias) %>% 
      mutate( ODS = case_when(
        `ODS - NOME` == "01 ERRADICAÇÃO DA POBREZA" ~ paste0("<img src='ods_01.png' height='70' data-toggle= 'tooltip' data-placement= 'right' title=\"", `OBJETIVO - OMS`,"\"></img>"), 
        `ODS - NOME` == "02 FOME ZERO E AGRICULTURA SUSTENTÁVEL" ~ paste0("<img src='ods_02.png' height='70' data-toggle= 'tooltip' data-placement= 'right' title=\"", `OBJETIVO - OMS`,"\"></img>"),
        `ODS - NOME` == "03 SAÚDE E BEM ESTAR" ~ paste0("<img src='ods_03.png' height='70' data-toggle= 'tooltip' data-placement= 'right' title=\"", `OBJETIVO - OMS`,"\"></img>"),
        `ODS - NOME` == "04 EDUCAÇÃO DE QUALIDADE" ~ paste0("<img src='ods_04.png' height='70' data-toggle= 'tooltip' data-placement= 'right' title=\"", `OBJETIVO - OMS`,"\"></img>"), 
        `ODS - NOME` == "05 IGUALDADE DE GÊNERO" ~ paste0("<img src='ods_05.png' height='70' data-toggle= 'tooltip' data-placement= 'right' title=\"", `OBJETIVO - OMS`,"\"></img>"),
        `ODS - NOME` == "06 ÁGUA POTÁVEL E SANEAMENTO" ~ paste0("<img src='ods_06.png' height='70' data-toggle= 'tooltip' data-placement= 'right' title=\"", `OBJETIVO - OMS`,"\"></img>"),
        `ODS - NOME` == "07 ENERGIA LIMPA E ACESSÍVEL" ~ paste0("<img src='ods_07.png' height='70' data-toggle= 'tooltip' data-placement= 'right' title=\"", `OBJETIVO - OMS`,"\"></img>"),
        `ODS - NOME` == "08 TRABALHO DECENTE E CRESCIMENTO ECONÔMICO" ~ paste0("<img src='ods_08.png' height='70' data-toggle= 'tooltip' data-placement= 'right' title=\"", `OBJETIVO - OMS`,"\"></img>"), 
        `ODS - NOME` == "09 INDÚSTRIA, INOVAÇÃO E INFRAESTRUTURA" ~ paste0("<img src='ods_09.png' height='70' data-toggle= 'tooltip' data-placement= 'right' title=\"", `OBJETIVO - OMS`,"\"></img>"), 
        `ODS - NOME` == "10 REDUÇÃO DAS DESIGUALDADES" ~ paste0("<img src='ods_10.png' height='70' data-toggle= 'tooltip' data-placement= 'right' title=\"", `OBJETIVO - OMS`,"\"></img>"),
        `ODS - NOME` == "11 CIDADES E COMUNIDADES SUSTENTÁVEIS" ~ paste0("<img src='ods_11.png' height='70' data-toggle= 'tooltip' data-placement= 'right' title=\"", `OBJETIVO - OMS`,"\"></img>"),
        `ODS - NOME` == "12 CONSUMO E PRODUÇÃO RESPONSÁVEIS" ~ paste0("<img src='ods_12.png' height='70' data-toggle= 'tooltip' data-placement= 'right' title=\"", `OBJETIVO - OMS`,"\"></img>"),
        `ODS - NOME` == "13 AÇÃO CONTRA A MUDANÇA GLOBAL DO CLIMA" ~ paste0("<img src='ods_13.png' height='70' data-toggle= 'tooltip' data-placement= 'right' title=\"", `OBJETIVO - OMS`,"\"></img>"),
        `ODS - NOME` == "15 VIDA TERRESTRE" ~ paste0("<img src='ods_15.png' height='70' data-toggle= 'tooltip' data-placement= 'right' title=\"", `OBJETIVO - OMS`,"\"></img>"),
        `ODS - NOME` == "16 PAZ, JUSTIÇA E INSTITUIÇÕES EFICAZES" ~ paste0("<img src='ods_16.png' height='70' data-toggle= 'tooltip' data-placement= 'right' title=\"", `OBJETIVO - OMS`,"\"></img>"), 
        `ODS - NOME` == "17 PARCERIAS E MEIOS DE IMPLEMENTAÇÃO" ~ paste0("<img src='ods_17.png' height='70' data-toggle= 'tooltip' data-placement= 'right' title=\"", `OBJETIVO - OMS`,"\"></img>"), 
        TRUE ~ "NA"
      )
      ) %>% 
      select(`ESTRATEGIA-ETAPA`, ODS) %>% 
      group_by(`ESTRATEGIA-ETAPA`) %>% 
      arrange(ODS) %>% 
      mutate(ODS = paste0(ODS, collapse = "")) %>% 
      distinct(),
    options = list(paging = FALSE),
    escape = FALSE,
    rownames = FALSE
  )
  
  output$pavs.ods <-  renderDT(
    bd.ods.etapa %>% filter(`ESTRATEGIA-ETAPA` %in% input$acoes) %>% 
      mutate( ODS = case_when(
        `ODS - NOME` == "01 ERRADICAÇÃO DA POBREZA" ~ paste0("<img src='ods_01.png' height='100' data-toggle= 'tooltip' data-placement= 'right' title=\"", `OBJETIVO - OMS`,"\"></img>"), 
        `ODS - NOME` == "02 FOME ZERO E AGRICULTURA SUSTENTÁVEL" ~ paste0("<img src='ods_02.png' height='100' data-toggle= 'tooltip' data-placement= 'right' title=\"", `OBJETIVO - OMS`,"\"></img>"),
        `ODS - NOME` == "03 SAÚDE E BEM ESTAR" ~ paste0("<img src='ods_03.png' height='100' data-toggle= 'tooltip' data-placement= 'right' title=\"", `OBJETIVO - OMS`,"\"></img>"),
        `ODS - NOME` == "04 EDUCAÇÃO DE QUALIDADE" ~ paste0("<img src='ods_04.png' height='100' data-toggle= 'tooltip' data-placement= 'right' title=\"", `OBJETIVO - OMS`,"\"></img>"), 
        `ODS - NOME` == "05 IGUALDADE DE GÊNERO" ~ paste0("<img src='ods_05.png' height='100' data-toggle= 'tooltip' data-placement= 'right' title=\"", `OBJETIVO - OMS`,"\"></img>"),
        `ODS - NOME` == "06 ÁGUA POTÁVEL E SANEAMENTO" ~ paste0("<img src='ods_06.png' height='100' data-toggle= 'tooltip' data-placement= 'right' title=\"", `OBJETIVO - OMS`,"\"></img>"),
        `ODS - NOME` == "07 ENERGIA LIMPA E ACESSÍVEL" ~ paste0("<img src='ods_07.png' height='100' data-toggle= 'tooltip' data-placement= 'right' title=\"", `OBJETIVO - OMS`,"\"></img>"),
        `ODS - NOME` == "08 TRABALHO DECENTE E CRESCIMENTO ECONÔMICO" ~ paste0("<img src='ods_08.png' height='100' data-toggle= 'tooltip' data-placement= 'right' title=\"", `OBJETIVO - OMS`,"\"></img>"), 
        `ODS - NOME` == "09 INDÚSTRIA, INOVAÇÃO E INFRAESTRUTURA" ~ paste0("<img src='ods_09.png' height='100' data-toggle= 'tooltip' data-placement= 'right' title=\"", `OBJETIVO - OMS`,"\"></img>"), 
        `ODS - NOME` == "10 REDUÇÃO DAS DESIGUALDADES" ~ paste0("<img src='ods_10.png' height='100' data-toggle= 'tooltip' data-placement= 'right' title=\"", `OBJETIVO - OMS`,"\"></img>"),
        `ODS - NOME` == "11 CIDADES E COMUNIDADES SUSTENTÁVEIS" ~ paste0("<img src='ods_11.png' height='100' data-toggle= 'tooltip' data-placement= 'right' title=\"", `OBJETIVO - OMS`,"\"></img>"),
        `ODS - NOME` == "12 CONSUMO E PRODUÇÃO RESPONSÁVEIS" ~ paste0("<img src='ods_12.png' height='100' data-toggle= 'tooltip' data-placement= 'right' title=\"", `OBJETIVO - OMS`,"\"></img>"),
        `ODS - NOME` == "13 AÇÃO CONTRA A MUDANÇA GLOBAL DO CLIMA" ~ paste0("<img src='ods_13.png' height='100' data-toggle= 'tooltip' data-placement= 'right' title=\"", `OBJETIVO - OMS`,"\"></img>"),
        `ODS - NOME` == "15 VIDA TERRESTRE" ~ paste0("<img src='ods_15.png' height='100' data-toggle= 'tooltip' data-placement= 'right' title=\"", `OBJETIVO - OMS`,"\"></img>"),
        `ODS - NOME` == "16 PAZ, JUSTIÇA E INSTITUIÇÕES EFICAZES" ~ paste0("<img src='ods_16.png' height='100' data-toggle= 'tooltip' data-placement= 'right' title=\"", `OBJETIVO - OMS`,"\"></img>"), 
        `ODS - NOME` == "17 PARCERIAS E MEIOS DE IMPLEMENTAÇÃO" ~ paste0("<img src='ods_17.png' height='100' data-toggle= 'tooltip' data-placement= 'right' title=\"", `OBJETIVO - OMS`,"\"></img>"), 
        TRUE ~ "NA"
      )
      ) %>% 
      select(ODS) %>% 
      arrange(ODS) %>% 
      mutate(ODS = paste0(ODS, collapse = "")) %>% 
      distinct(),
    options = list(paging = FALSE,
                   dom = "t"),
    escape = FALSE,
    rownames = FALSE
  )
  
  output$detalhamento <-  renderDT(
    bd.ods.merge %>% filter(`ACOES PARA CONTRIBUICAO NO ATENDIMENTO DAS METAS` %in% input$acoes) %>% 
      drop_na(`Meta ODS Global - DESCRICAO`) %>%
      mutate( ODS = case_when(
        `ODS - NOME` == "01 ERRADICAÇÃO DA POBREZA" ~ paste0("<img src='ods_01.png' height='100' data-toggle= 'tooltip' data-placement= 'right' title=\"", `OBJETIVO - OMS`,"\"></img>"), 
        `ODS - NOME` == "02 FOME ZERO E AGRICULTURA SUSTENTÁVEL" ~ paste0("<img src='ods_02.png' height='100' data-toggle= 'tooltip' data-placement= 'right' title=\"", `OBJETIVO - OMS`,"\"></img>"),
        `ODS - NOME` == "03 SAÚDE E BEM ESTAR" ~ paste0("<img src='ods_03.png' height='100' data-toggle= 'tooltip' data-placement= 'right' title=\"", `OBJETIVO - OMS`,"\"></img>"),
        `ODS - NOME` == "04 EDUCAÇÃO DE QUALIDADE" ~ paste0("<img src='ods_04.png' height='100' data-toggle= 'tooltip' data-placement= 'right' title=\"", `OBJETIVO - OMS`,"\"></img>"), 
        `ODS - NOME` == "05 IGUALDADE DE GÊNERO" ~ paste0("<img src='ods_05.png' height='100' data-toggle= 'tooltip' data-placement= 'right' title=\"", `OBJETIVO - OMS`,"\"></img>"),
        `ODS - NOME` == "06 ÁGUA POTÁVEL E SANEAMENTO" ~ paste0("<img src='ods_06.png' height='100' data-toggle= 'tooltip' data-placement= 'right' title=\"", `OBJETIVO - OMS`,"\"></img>"),
        `ODS - NOME` == "07 ENERGIA LIMPA E ACESSÍVEL" ~ paste0("<img src='ods_07.png' height='100' data-toggle= 'tooltip' data-placement= 'right' title=\"", `OBJETIVO - OMS`,"\"></img>"),
        `ODS - NOME` == "08 TRABALHO DECENTE E CRESCIMENTO ECONÔMICO" ~ paste0("<img src='ods_08.png' height='100' data-toggle= 'tooltip' data-placement= 'right' title=\"", `OBJETIVO - OMS`,"\"></img>"), 
        `ODS - NOME` == "09 INDÚSTRIA, INOVAÇÃO E INFRAESTRUTURA" ~ paste0("<img src='ods_09.png' height='100' data-toggle= 'tooltip' data-placement= 'right' title=\"", `OBJETIVO - OMS`,"\"></img>"), 
        `ODS - NOME` == "10 REDUÇÃO DAS DESIGUALDADES" ~ paste0("<img src='ods_10.png' height='100' data-toggle= 'tooltip' data-placement= 'right' title=\"", `OBJETIVO - OMS`,"\"></img>"),
        `ODS - NOME` == "11 CIDADES E COMUNIDADES SUSTENTÁVEIS" ~ paste0("<img src='ods_11.png' height='100' data-toggle= 'tooltip' data-placement= 'right' title=\"", `OBJETIVO - OMS`,"\"></img>"),
        `ODS - NOME` == "12 CONSUMO E PRODUÇÃO RESPONSÁVEIS" ~ paste0("<img src='ods_12.png' height='100' data-toggle= 'tooltip' data-placement= 'right' title=\"", `OBJETIVO - OMS`,"\"></img>"),
        `ODS - NOME` == "13 AÇÃO CONTRA A MUDANÇA GLOBAL DO CLIMA" ~ paste0("<img src='ods_13.png' height='100' data-toggle= 'tooltip' data-placement= 'right' title=\"", `OBJETIVO - OMS`,"\"></img>"),
        `ODS - NOME` == "15 VIDA TERRESTRE" ~ paste0("<img src='ods_15.png' height='100' data-toggle= 'tooltip' data-placement= 'right' title=\"", `OBJETIVO - OMS`,"\"></img>"),
        `ODS - NOME` == "16 PAZ, JUSTIÇA E INSTITUIÇÕES EFICAZES" ~ paste0("<img src='ods_16.png' height='100' data-toggle= 'tooltip' data-placement= 'right' title=\"", `OBJETIVO - OMS`,"\"></img>"), 
        `ODS - NOME` == "17 PARCERIAS E MEIOS DE IMPLEMENTAÇÃO" ~ paste0("<img src='ods_17.png' height='100' data-toggle= 'tooltip' data-placement= 'right' title=\"", `OBJETIVO - OMS`,"\"></img>"), 
        TRUE ~ "NA"
      )
      ) %>% 
      select(ODS, `Meta ODS Global - DESCRICAO`, `Meta ODS Municipal - DESCRICAO`) %>% 
      arrange(ODS),
    options = list(paging = FALSE),
    escape = FALSE,
    rownames = FALSE #list(lengthChange = FALSE)
  )
  
  output$indicadores <-    renderDT(
    bd.metas %>% filter(`Meta ODS Municipal - DESCRICAO` %in% indicadores_filtrados()$`Meta ODS Municipal - DESCRICAO`) %>% 
      filter(!is.na(INDICADOR)) %>%
      mutate( ODS = case_when(
        `ODS - NOME` == "01 ERRADICAÇÃO DA POBREZA" ~ paste0("<img src='ods_01.png' height='100' data-toggle= 'tooltip' data-placement= 'right' title=\"", `OBJETIVO - OMS`,"\"></img>"), 
        `ODS - NOME` == "02 FOME ZERO E AGRICULTURA SUSTENTÁVEL" ~ paste0("<img src='ods_02.png' height='100' data-toggle= 'tooltip' data-placement= 'right' title=\"", `OBJETIVO - OMS`,"\"></img>"),
        `ODS - NOME` == "03 SAÚDE E BEM ESTAR" ~ paste0("<img src='ods_03.png' height='100' data-toggle= 'tooltip' data-placement= 'right' title=\"", `OBJETIVO - OMS`,"\"></img>"),
        `ODS - NOME` == "04 EDUCAÇÃO DE QUALIDADE" ~ paste0("<img src='ods_04.png' height='100' data-toggle= 'tooltip' data-placement= 'right' title=\"", `OBJETIVO - OMS`,"\"></img>"), 
        `ODS - NOME` == "05 IGUALDADE DE GÊNERO" ~ paste0("<img src='ods_05.png' height='100' data-toggle= 'tooltip' data-placement= 'right' title=\"", `OBJETIVO - OMS`,"\"></img>"),
        `ODS - NOME` == "06 ÁGUA POTÁVEL E SANEAMENTO" ~ paste0("<img src='ods_06.png' height='100' data-toggle= 'tooltip' data-placement= 'right' title=\"", `OBJETIVO - OMS`,"\"></img>"),
        `ODS - NOME` == "07 ENERGIA LIMPA E ACESSÍVEL" ~ paste0("<img src='ods_07.png' height='100' data-toggle= 'tooltip' data-placement= 'right' title=\"", `OBJETIVO - OMS`,"\"></img>"),
        `ODS - NOME` == "08 TRABALHO DECENTE E CRESCIMENTO ECONÔMICO" ~ paste0("<img src='ods_08.png' height='100' data-toggle= 'tooltip' data-placement= 'right' title=\"", `OBJETIVO - OMS`,"\"></img>"), 
        `ODS - NOME` == "09 INDÚSTRIA, INOVAÇÃO E INFRAESTRUTURA" ~ paste0("<img src='ods_09.png' height='100' data-toggle= 'tooltip' data-placement= 'right' title=\"", `OBJETIVO - OMS`,"\"></img>"), 
        `ODS - NOME` == "10 REDUÇÃO DAS DESIGUALDADES" ~ paste0("<img src='ods_10.png' height='100' data-toggle= 'tooltip' data-placement= 'right' title=\"", `OBJETIVO - OMS`,"\"></img>"),
        `ODS - NOME` == "11 CIDADES E COMUNIDADES SUSTENTÁVEIS" ~ paste0("<img src='ods_11.png' height='100' data-toggle= 'tooltip' data-placement= 'right' title=\"", `OBJETIVO - OMS`,"\"></img>"),
        `ODS - NOME` == "12 CONSUMO E PRODUÇÃO RESPONSÁVEIS" ~ paste0("<img src='ods_12.png' height='100' data-toggle= 'tooltip' data-placement= 'right' title=\"", `OBJETIVO - OMS`,"\"></img>"),
        `ODS - NOME` == "13 AÇÃO CONTRA A MUDANÇA GLOBAL DO CLIMA" ~ paste0("<img src='ods_13.png' height='100' data-toggle= 'tooltip' data-placement= 'right' title=\"", `OBJETIVO - OMS`,"\"></img>"),
        `ODS - NOME` == "15 VIDA TERRESTRE" ~ paste0("<img src='ods_15.png' height='100' data-toggle= 'tooltip' data-placement= 'right' title=\"", `OBJETIVO - OMS`,"\"></img>"),
        `ODS - NOME` == "16 PAZ, JUSTIÇA E INSTITUIÇÕES EFICAZES" ~ paste0("<img src='ods_16.png' height='100' data-toggle= 'tooltip' data-placement= 'right' title=\"", `OBJETIVO - OMS`,"\"></img>"), 
        `ODS - NOME` == "17 PARCERIAS E MEIOS DE IMPLEMENTAÇÃO" ~ paste0("<img src='ods_17.png' height='100' data-toggle= 'tooltip' data-placement= 'right' title=\"", `OBJETIVO - OMS`,"\"></img>"), 
        TRUE ~ "NA"
      )
      ) %>% 
      select(ODS, `Meta ODS Municipal - DESCRICAO`, INDICADOR, REFERENCIA) %>% 
      bind_rows(indicadores_unir()) %>% 
      arrange(ODS), 
    options = list(paging = FALSE),
    escape = FALSE,
    rownames = FALSE #list(lengthChange = FALSE)
  )
  
  output$tabela.agravos <- renderDT(
    pavs_agravos() %>% 
      transmute(
        CATEGORIAS = CATEGORIAS,
        `AGRAVOS/DOENÇAS` = `Doencas/ Agravos`,
        `FRAÇÃO AMBIENTAL ATRIBUÍDA` = paste0(`Risco ambiental` * 100, "%"),
        `EIXO PAVS` = `EIXO PAVS`,
        `Ações para enfrentamento sugeridas` = `Ações para enfrentamento sugeridas`,
        `Método para estabelecimento da FAA` = Metodo
      ), 
    options = list(paging = FALSE,
                   columnDefs = list(list(className = 'dt-center', targets = 3))
    )#list(lengthChange = FALSE)
  )
  
  output$tabela.indicadores <- renderDT(
    corr_pavs() %>% 
      mutate( ODS = case_when(
        `ODS - NOME` == "01 ERRADICAÇÃO DA POBREZA" ~ paste0("<img src='ods_01.png' height='100' data-toggle= 'tooltip' data-placement= 'right' title=\"", `OBJETIVO - OMS`,"\"></img>"), 
        `ODS - NOME` == "02 FOME ZERO E AGRICULTURA SUSTENTÁVEL" ~ paste0("<img src='ods_02.png' height='100' data-toggle= 'tooltip' data-placement= 'right' title=\"", `OBJETIVO - OMS`,"\"></img>"),
        `ODS - NOME` == "03 SAÚDE E BEM ESTAR" ~ paste0("<img src='ods_03.png' height='100' data-toggle= 'tooltip' data-placement= 'right' title=\"", `OBJETIVO - OMS`,"\"></img>"),
        `ODS - NOME` == "04 EDUCAÇÃO DE QUALIDADE" ~ paste0("<img src='ods_04.png' height='100' data-toggle= 'tooltip' data-placement= 'right' title=\"", `OBJETIVO - OMS`,"\"></img>"), 
        `ODS - NOME` == "05 IGUALDADE DE GÊNERO" ~ paste0("<img src='ods_05.png' height='100' data-toggle= 'tooltip' data-placement= 'right' title=\"", `OBJETIVO - OMS`,"\"></img>"),
        `ODS - NOME` == "06 ÁGUA POTÁVEL E SANEAMENTO" ~ paste0("<img src='ods_06.png' height='100' data-toggle= 'tooltip' data-placement= 'right' title=\"", `OBJETIVO - OMS`,"\"></img>"),
        `ODS - NOME` == "07 ENERGIA LIMPA E ACESSÍVEL" ~ paste0("<img src='ods_07.png' height='100' data-toggle= 'tooltip' data-placement= 'right' title=\"", `OBJETIVO - OMS`,"\"></img>"),
        `ODS - NOME` == "08 TRABALHO DECENTE E CRESCIMENTO ECONÔMICO" ~ paste0("<img src='ods_08.png' height='100' data-toggle= 'tooltip' data-placement= 'right' title=\"", `OBJETIVO - OMS`,"\"></img>"), 
        `ODS - NOME` == "09 INDÚSTRIA, INOVAÇÃO E INFRAESTRUTURA" ~ paste0("<img src='ods_09.png' height='100' data-toggle= 'tooltip' data-placement= 'right' title=\"", `OBJETIVO - OMS`,"\"></img>"), 
        `ODS - NOME` == "10 REDUÇÃO DAS DESIGUALDADES" ~ paste0("<img src='ods_10.png' height='100' data-toggle= 'tooltip' data-placement= 'right' title=\"", `OBJETIVO - OMS`,"\"></img>"),
        `ODS - NOME` == "11 CIDADES E COMUNIDADES SUSTENTÁVEIS" ~ paste0("<img src='ods_11.png' height='100' data-toggle= 'tooltip' data-placement= 'right' title=\"", `OBJETIVO - OMS`,"\"></img>"),
        `ODS - NOME` == "12 CONSUMO E PRODUÇÃO RESPONSÁVEIS" ~ paste0("<img src='ods_12.png' height='100' data-toggle= 'tooltip' data-placement= 'right' title=\"", `OBJETIVO - OMS`,"\"></img>"),
        `ODS - NOME` == "13 AÇÃO CONTRA A MUDANÇA GLOBAL DO CLIMA" ~ paste0("<img src='ods_13.png' height='100' data-toggle= 'tooltip' data-placement= 'right' title=\"", `OBJETIVO - OMS`,"\"></img>"),
        `ODS - NOME` == "15 VIDA TERRESTRE" ~ paste0("<img src='ods_15.png' height='100' data-toggle= 'tooltip' data-placement= 'right' title=\"", `OBJETIVO - OMS`,"\"></img>"),
        `ODS - NOME` == "16 PAZ, JUSTIÇA E INSTITUIÇÕES EFICAZES" ~ paste0("<img src='ods_16.png' height='100' data-toggle= 'tooltip' data-placement= 'right' title=\"", `OBJETIVO - OMS`,"\"></img>"), 
        `ODS - NOME` == "17 PARCERIAS E MEIOS DE IMPLEMENTAÇÃO" ~ paste0("<img src='ods_17.png' height='100' data-toggle= 'tooltip' data-placement= 'right' title=\"", `OBJETIVO - OMS`,"\"></img>"), 
        TRUE ~ "NA"
      )
      ) %>% 
      transmute(#`ODS - NOME` = `ODS - NOME`,
        ODS = ODS,
        `Meta ODS Municipal - DESCRICAO` = `Meta ODS Municipal - DESCRICAO`,
        INDICADOR = INDICADOR, 
        REFERENCIA = REFERENCIA.y) %>% 
      arrange(ODS),
    options = list(paging = FALSE),
    escape = FALSE,
    rownames = FALSE#list(lengthChange = FALSE)
  )
  
  
  ####BOTAO DOWNLOAD 
  output$relatorio_unidade <- downloadHandler(
    filename = function() {         #nome do arquivo que sera salvo    
      paste0(today(),"_relatorio_", input$acoes, ".pdf")
    },  
    content = function(file){
      params <- list(acoes = input$acoes,
                     bd_pavs = corr_pavs(),
                     bd_pavs_agravos = pavs_agravos()
      )
      
      rmarkdown::render(
        input = "www/relatorio_ods.Rmd",
        output_file = file,
        params = params,
        envir = new.env(parent = globalenv())
      )
    }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
