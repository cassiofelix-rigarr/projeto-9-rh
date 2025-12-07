# mod_dashboard.R — RH Geral + Desligamentos + Análise Individual

mod_dashboard_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    
    # HEADER ---------------------------------------------------------
    div(
      class = "header",
      div(
        class = "logo-container",
        img(
          src = "Logo Grupo Rigarr.png",
          class = "logo-image",
          style = "height:40px; margin-right:10px;"
        )
      ),
      div(
        class = "title-block",
        div(
          class = "subtitle-rigarr",
          "Dashboard de Recursos Humanos: visão geral de contratações, desligamentos, funcionários ativos, folha salarial e desempenho individual, com foco em decisões baseadas em dados."
        )
      ),
      div(
        class = "user-area",
        textOutput(ns("user_email")),
        actionButton(ns("logout"), "Sair", class = "btn btn-light")
      )
    ),
    
    # PAINEIS --------------------------------------------------------
    tabsetPanel(
      id   = ns("tabs"),
      type = "tabs",
      
      # ===================== PAINEL 1 – GERAL =======================
      tabPanel(
        title = "Painel RH - Geral",
        
        # KPIs --------------------------------------------------------
        div(
          class = "kpi-row",
          div(
            class = "kpi-card",
            div(class = "kpi-header", icon("users", class = "kpi-icon"), span("Funcionários Ativos")),
            div(class = "kpi-value", textOutput(ns("kpi_ativos"))),
            div(class = "kpi-comment-kpi", textOutput(ns("kpi_ativos_comment")))
          ),
          div(
            class = "kpi-card",
            div(class = "kpi-header", icon("user-plus", class = "kpi-icon"), span("Contratações")),
            div(class = "kpi-value", textOutput(ns("kpi_contratacoes"))),
            div(class = "kpi-comment-kpi", textOutput(ns("kpi_contratacoes_comment")))
          ),
          div(
            class = "kpi-card",
            div(class = "kpi-header", icon("user-times", class = "kpi-icon"), span("Desligamentos")),
            div(class = "kpi-value", textOutput(ns("kpi_desligamentos"))),
            div(class = "kpi-comment-kpi", textOutput(ns("kpi_desligamentos_comment")))
          ),
          div(
            class = "kpi-card",
            div(class = "kpi-header", icon("percent", class = "kpi-icon"), span("% Demissão")),
            div(class = "kpi-value", textOutput(ns("kpi_turnover"))),
            div(class = "kpi-comment-kpi", textOutput(ns("kpi_turnover_comment")))
          )
        ),
        
        # FILTROS -----------------------------------------------------
        uiOutput(ns("filtros_rh")),
        
        # GRID PRINCIPAL (topo) --------------------------------------
        div(
          class = "layout",
          
          # Coluna esquerda
          div(
            class = "col-left",
            div(
              class = "card",
              h3(icon("bar-chart", class = "card-title-icon"),
                 "Contratações e Desligamentos por Ano"),
              p(class = "card-comment",
                "Gráfico de colunas que compara, ano a ano, o volume de contratações e desligamentos da empresa segundo os filtros de nível e área."),
              p(class = "card-comment-data",
                "Use este gráfico para responder perguntas como: “em que anos a empresa mais cresceu?”, “houve algum período de aumento de desligamentos sem aumento proporcional de contratações?” ou “como o movimento de pessoas se comportou ao longo do tempo?”."),
              highchartOutput(ns("chart_contr_desl"), height = "280px")
            ),
            div(
              class = "card",
              h3(icon("line-chart", class = "card-title-icon"),
                 "% Demissão durante os Anos"),
              p(class = "card-comment",
                "Linha que mostra o percentual de demissões em relação às contratações de cada ano (turnover histórico)."),
              p(class = "card-comment-data",
                "Valores mais altos indicam anos em que a empresa perdeu proporcionalmente mais colaboradores. Combine esta leitura com o gráfico de contratações x desligamentos para entender se o aumento da taxa foi por crescimento de desligamentos, queda de admissões ou ambos."),
              highchartOutput(ns("chart_turnover"), height = "260px")
            )
          ),
          
          # Coluna direita
          div(
            class = "col-right",
            div(
              class = "card",
              style = "min-height: 560px;",   # <-- AUMENTEI O CARD
              h3(icon("table", class = "card-title-icon"),
                 "Funcionários e Salários"),
              p(class = "card-comment",
                "Tabela ordenada do maior para o menor salário, exibindo os funcionários filtrados e o valor atual da remuneração."),
              p(class = "card-comment-data",
                "A lista ajuda a identificar rapidamente os maiores salários da empresa, analisar a distribuição por nível e área, além de apoiar estudos de equidade e planejamento orçamentário de folha."),
              reactableOutput(ns("tabela_salarios")),
              p(class = "card-comment-data", textOutput(ns("comment_salarios")))
            )
          )
        ),
        
        # BLOCO: DESLIGAMENTOS (resumo do painel 2) ------------------
        div(
          class = "layout",
          
          div(
            class = "col-left",
            div(
              class = "card",
              h3(icon("map-marker", class = "card-title-icon"),
                 "Desligamentos por Estado"),
              p(class = "card-comment",
                "Mapa do Brasil destacando, por estado, o número de desligamentos e sua participação percentual no total."),
              p(class = "card-comment-data",
                "Passe o mouse sobre cada estado para ver quantos desligamentos ocorreram ali e qual a representatividade em relação ao total. É útil para identificar concentrações regionais de desligamento e apoiar decisões de alocação de esforços de clima, liderança e retenção."),
              highchartOutput(ns("chart_deslig_estado_p1"), height = "420px")
            )
          ),
          
          div(
            class = "col-right",
            div(
              class = "card",
              h3(icon("bar-chart", class = "card-title-icon"),
                 "Desligamentos por Faixa de Idade"),
              p(class = "card-comment",
                "Colunas que mostram como os desligamentos se distribuem entre as principais faixas etárias."),
              p(class = "card-comment-data",
                "Compare o peso relativo das faixas etárias para entender se a empresa perde mais talentos jovens, profissionais em meia carreira ou perfis mais seniores. Padrões anormais podem indicar problemas específicos em benefícios, carreira, liderança ou clima em determinados ciclos de vida."),
              highchartOutput(ns("chart_deslig_idade_p1"), height = "200px")
            ),
            
            div(
              class = "subrow",
              div(
                class = "card",
                h3(icon("users", class = "card-title-icon"),
                   "Desligamentos por Nível"),
                p(class = "card-comment",
                  "Comparativo de desligamentos entre níveis hierárquicos (Estagiário, Analista, Coordenador e Gerente)."),
                p(class = "card-comment-data",
                  "Uma concentração elevada em um nível pode indicar desafios específicos: por exemplo, rotatividade alta em Analistas pode apontar para falta de plano de carreira, enquanto saídas em Coordenadores e Gerentes podem sinalizar problemas de alinhamento estratégico ou de cultura."),
                highchartOutput(ns("chart_deslig_nivel_p1"), height = "220px")
              ),
              div(
                class = "card",
                h3(icon("building", class = "card-title-icon"),
                   "Desligamentos por Área"),
                p(class = "card-comment",
                  "Barras horizontais que mostram o número de desligamentos em cada área de atuação (RH, Financeiro, Operações etc.)."),
                p(class = "card-comment-data",
                  "Use esse gráfico para comparar rapidamente áreas críticas. Uma área com desligamentos consistentemente altos merece investigação mais profunda em liderança, carga de trabalho, remuneração ou aderência cultural."),
                highchartOutput(ns("chart_deslig_area_p1"), height = "220px")
              )
            )
          )
        ),
        
        # BLOCO: RESUMO ANÁLISE INDIVIDUAL ---------------------------
        div(
          class = "layout",
          
          div(
            class = "col-left",
            div(
              class = "card",
              h3(icon("tachometer", class = "card-title-icon"),
                 "Avaliação Individual (resumo)"),
              p(class = "card-comment",
                "Gauge com a nota geral de desempenho do funcionário selecionado, replicando a leitura do painel de Análise Individual."),
              p(class = "card-comment-data",
                "A escala vai de 0 a 10. Como referência rápida: de 0 a 4 indica necessidade de atenção imediata, entre 4 e 7 representa desempenho satisfatório com pontos de melhoria, e acima de 7 indica alta performance. Combine essa leitura com as notas por competência para entender quais dimensões puxam a nota para cima ou para baixo."),
              h4(textOutput(ns("info_nome_p1"))),
              p(class = "card-comment-data", textOutput(ns("info_nivel_p1"))),
              p(class = "card-comment-data", textOutput(ns("info_area_p1"))),
              highchartOutput(ns("chart_gauge_p1"), height = "320px")
            )
          ),
          
          div(
            class = "col-right",
            div(
              class = "card",
              h3(icon("pie-chart", class = "card-title-icon"),
                 "Notas por Competência (resumo)"),
              p(class = "card-comment",
                "Gráfico em rosca com as notas por competência (Comunicação, Trabalho em Equipe, Organização, Liderança e Iniciativa) do funcionário selecionado."),
              p(class = "card-comment-data",
                "Áreas com notas mais baixas indicam oportunidades de desenvolvimento individuais ou até mesmo temas para trilhas de treinamento coletivas. Áreas com notas altas reforçam as fortalezas do colaborador e podem embasar planos de sucessão ou mentoring."),
              highchartOutput(ns("chart_competencias_p1"), height = "240px")
            ),
            div(
              class = "card",
              h3(icon("info-circle", class = "card-title-icon"),
                 "Informações do Funcionário (resumo)"),
              p(class = "card-comment",
                "Quadro com as principais informações do colaborador (nome completo, data de nascimento, estado, salário, idade, horas extras e férias acumuladas)."),
              p(class = "card-comment-data",
                "Essas informações funcionam como um “cartão de identidade” do colaborador, facilitando consultas rápidas sem a necessidade de abrir sistemas transacionais, como folha ou cadastro de pessoal."),
              tableOutput(ns("tabela_info_func_p1"))
            )
          )
        )
      ),
      
      # ================== PAINEL 2 – DESLIGAMENTOS ==================
      tabPanel(
        title = "Painel RH - Desligamentos",
        
        uiOutput(ns("filtros_deslig")),
        
        div(
          class = "layout",
          
          div(
            class = "col-left",
            # Card do MAPA -------------------------------------------
            div(
              class = "card",
              h3(icon("map-marker", class = "card-title-icon"),
                 "Desligamentos por Estado"),
              p(class = "card-comment",
                "Mapa do Brasil mostrando o total de desligamentos por estado e a participação de cada um no total do período selecionado."),
              p(class = "card-comment-data",
                "Ao passar o mouse, você visualiza a quantidade de desligamentos e o percentual que cada unidade da federação representa. Esse mapa é ideal para direcionar ações regionais de engajamento, desenvolvimento de liderança local e políticas específicas de retenção."),
              highchartOutput(ns("chart_deslig_estado"), height = "420px")
            ),
            # Card explicativo abaixo do mapa ------------------------
            div(
              class = "card",
              h3(icon("lightbulb-o", class = "card-title-icon"),
                 "Como usar esta tela de desligamentos"),
              p(
                class = "card-comment",
                "Este painel reúne, em um só lugar, a leitura geográfica 🌎, demográfica 👥 e organizacional 🏢 dos desligamentos da empresa."
              ),
              p(
                class = "card-comment-data",
                "Use o mapa para localizar ‘pontos quentes’ de rotatividade por estado e, em seguida, complemente a análise com os gráficos de faixa etária, nível e área ao lado. A combinação dessas visões ajuda a responder perguntas como:"
              ),
              tags$ul(
                tags$li("Quais regiões concentram mais saídas de colaboradores?"),
                tags$li("Estamos perdendo mais pessoas em algum nível hierárquico específico?"),
                tags$li("Existe alguma área da empresa com desligamentos recorrentes ao longo do tempo?")
              ),
              p(
                class = "card-comment-data",
                "A partir dessas respostas, o RH pode priorizar ações de clima, carreira e desenvolvimento onde o risco de perda de talentos é maior, tornando a gestão de desligamentos mais estratégica e menos reativa. ✨"
              )
            )
          ),
          
          div(
            class = "col-right",
            div(
              class = "card",
              h3(icon("bar-chart", class = "card-title-icon"),
                 "Desligamentos por Faixa de Idade"),
              p(class = "card-comment",
                "Gráfico de colunas que detalha a quantidade de desligamentos por faixa de idade."),
              p(class = "card-comment-data",
                "Use este painel para entender se a empresa está perdendo talentos em fases críticas da carreira, como jovens em início de jornada ou profissionais mais maduros. Esse diagnóstico pode apoiar ajustes em planos de carreira, benefícios e programas de desenvolvimento."),
              highchartOutput(ns("chart_deslig_idade"), height = "200px")
            ),
            div(
              class = "subrow",
              div(
                class = "card",
                h3(icon("users", class = "card-title-icon"),
                   "Desligamentos por Nível"),
                p(class = "card-comment",
                  "Barras horizontais com o volume de desligamentos por nível hierárquico."),
                p(class = "card-comment-data",
                  "Níveis com alta saída podem indicar falta de reconhecimento, desbalanceamento de carga de trabalho, desalinhamento de expectativas ou remuneração pouco competitiva. Esta visão ajuda a priorizar iniciativas específicas por camada organizacional."),
                highchartOutput(ns("chart_deslig_nivel"), height = "220px")
              ),
              div(
                class = "card",
                h3(icon("building", class = "card-title-icon"),
                   "Desligamentos por Área"),
                p(class = "card-comment",
                  "Barras horizontais com o volume de desligamentos em cada área da organização."),
                p(class = "card-comment-data",
                  "Uma área com desligamentos recorrentes gera impacto direto em produtividade, custo de reposição e perda de conhecimento. Acompanhe este gráfico ao longo do tempo para verificar se ações de retenção estão surtindo efeito."),
                highchartOutput(ns("chart_deslig_area"), height = "220px")
              )
            )
          )
        )
      ),
      
      # ================== PAINEL 3 – ANÁLISE INDIVIDUAL =============
      tabPanel(
        title = "Painel RH - Análise Individual",
        
        # Topo: seleção + gauge + infos ------------------------------
        div(
          class = "layout",
          
          div(
            class = "col-left",
            div(
              class = "card",
              h3(icon("user", class = "card-title-icon"),
                 "Perfil do Funcionário"),
              p(class = "card-comment",
                "Resumo do perfil do colaborador selecionado: nome completo, nível atual e área de atuação."),
              p(class = "card-comment-data",
                "Este bloco ajuda a contextualizar as métricas de desempenho com a realidade do colaborador: cargo, senioridade e área são peças fundamentais para uma avaliação justa e comparativa."),
              p(class = "card-comment-data",
                "Além dos dados exibidos, você pode usar este espaço como um ‘painel de contexto’ durante comitês de pessoas, calibragens de desempenho e discussões de sucessão. A leitura conjunta de perfil + avaliação apoia decisões mais equilibradas."),
              p(class = "card-comment-data",
                "Algumas perguntas que este card ajuda a responder:"),
              tags$ul(
                tags$li("O nível do colaborador é compatível com a nota de desempenho?"),
                tags$li("A área em que ele atua sofre mais pressão de desligamentos ou de carga de trabalho?"),
                tags$li("Há coerência entre o histórico de perfil e os planos de desenvolvimento traçados?")
              ),
              p(class = "card-comment-data",
                "Use essas respostas para registrar percepções na avaliação formal, planejar conversas de feedback e alinhar expectativas de crescimento com o colaborador."),
              p(class = "card-comment-data", "Nome completo"),
              h3(textOutput(ns("info_nome"))),
              p(class = "card-comment-data", "Nível"),
              h4(textOutput(ns("info_nivel"))),
              p(class = "card-comment-data", "Área"),
              h4(textOutput(ns("info_area")))
            )
          ),
          
          div(
            class = "col-right",
            div(
              class = "card",
              h3(icon("tachometer", class = "card-title-icon"),
                 "Avaliação do funcionário"),
              p(class = "card-comment",
                "Gauge com a nota geral de avaliação do colaborador (0 a 10), baseada na coluna de avaliação da base de dados."),
              p(class = "card-comment-data",
                "Interprete a escala da seguinte forma: notas abaixo de 4 indicam risco de desempenho e exigem plano de ação; de 4 a 7 sugerem desempenho adequado com oportunidades claras de desenvolvimento; acima de 7 indicam alta performance e possíveis candidatos a sucessão, promoções ou programas de reconhecimento."),
              uiOutput(ns("select_funcionario_ui")),
              highchartOutput(ns("chart_gauge"), height = "320px"),
              p(class = "card-comment-data", textOutput(ns("comment_gauge")))
            )
          )
        ),
        
        # Baixo: competências + tabela de info ------------------------
        div(
          class = "layout",
          
          div(
            class = "col-left",
            div(
              class = "card",
              h3(icon("pie-chart", class = "card-title-icon"),
                 "Notas por Competência"),
              p(class = "card-comment",
                "Gráfico em rosca com as notas detalhadas por competência para o funcionário selecionado."),
              p(class = "card-comment-data",
                "Competências com notas mais baixas podem orientar planos de desenvolvimento individual (PDI), treinamentos e feedbacks direcionados. Já competências fortes podem ser exploradas em papéis de liderança, mentoring e projetos estratégicos."),
              highchartOutput(ns("chart_competencias"), height = "280px")
            )
          ),
          
          div(
            class = "col-right",
            div(
              class = "card",
              h3(icon("info-circle", class = "card-title-icon"),
                 "Informações do Funcionário"),
              p(class = "card-comment",
                "Tabela com os principais dados cadastr
                ais e trabalhistas do colaborador (data de nascimento, estado, salário, idade, horas extras e férias acumuladas)."),
              p(class = "card-comment-data",
                "Use esses dados para complementar decisões de promoção, movimentação interna ou elegibilidade em programas específicos, evitando necessidade de consulta em vários sistemas diferentes."),
              tableOutput(ns("tabela_info_func"))
            )
          )
        )
      )
    )
  )
}

# --------------------------------------------------------------------
# SERVER
# --------------------------------------------------------------------

mod_dashboard_server <- function(input, output, session, user_rv) {
  ns <- session$ns
  
  # Formatadores -----------------------------------------------------
  format_currency <- function(x) {
    if (is.null(x)) return("R$ 0,00")
    res <- scales::dollar(x, prefix = "R$ ", big.mark = ".", decimal.mark = ",")
    res[is.na(x)] <- "R$ 0,00"
    res
  }
  format_percent <- function(x) {
    if (is.null(x)) return("0%")
    res <- scales::percent(x, accuracy = 0.01, decimal.mark = ",")
    res[is.na(x)] <- "0%"
    res
  }
  
  # Base -------------------------------------------------------------
  dados <- reactive({
    df <- readr::read_csv("data/base_rh.csv", show_col_types = FALSE)
    
    df <- df %>%
      dplyr::mutate(
        data_nascimento   = lubridate::ymd(data_nascimento),
        data_admissao     = lubridate::ymd(data_admissao),
        data_desligamento = lubridate::ymd(data_desligamento),
        ano_admissao      = lubridate::year(data_admissao),
        ano_desligamento  = lubridate::year(data_desligamento)
      )
    
    # Garante colunas usadas nos paineis -----------------------------
    if (!"avaliacao_geral" %in% names(df))      df$avaliacao_geral      <- NA_real_
    if (!"horas_extras" %in% names(df))         df$horas_extras         <- NA_real_
    if (!"ferias_acumuladas" %in% names(df))    df$ferias_acumuladas    <- NA_real_
    if (!"nota_comunicacao" %in% names(df))     df$nota_comunicacao     <- NA_real_
    if (!"nota_trabalho_equipe" %in% names(df)) df$nota_trabalho_equipe <- NA_real_
    if (!"nota_organizacao" %in% names(df))     df$nota_organizacao     <- NA_real_
    if (!"nota_lideranca" %in% names(df))       df$nota_lideranca       <- NA_real_
    if (!"nota_iniciativa" %in% names(df))      df$nota_iniciativa      <- NA_real_
    
    df
  })
  
  output$user_email <- renderText(paste("Olá,", user_rv$email))
  
  # --------------------- FILTROS PAINEL 1 ---------------------------
  output$filtros_rh <- renderUI({
    df <- dados()
    if (nrow(df) == 0) return(NULL)
    
    nivel_choices <- sort(unique(df$nivel))
    area_choices  <- sort(unique(df$area))
    
    fluidRow(
      class = "filters-row",
      column(
        width = 3,
        selectInput(
          ns("filtro_nivel"), "Nível",
          choices = c("Todos", nivel_choices), selected = "Todos"
        )
      ),
      column(
        width = 3,
        selectInput(
          ns("filtro_area"), "Área",
          choices = c("Todas", area_choices), selected = "Todas"
        )
      )
    )
  })
  
  dados_filtrados <- reactive({
    df <- dados()
    if (!is.null(input$filtro_nivel) && input$filtro_nivel != "Todos") {
      df <- df %>% dplyr::filter(nivel == input$filtro_nivel)
    }
    if (!is.null(input$filtro_area) && input$filtro_area != "Todas") {
      df <- df %>% dplyr::filter(area == input$filtro_area)
    }
    df
  })
  
  # ---------------------- KPIs PAINEL 1 -----------------------------
  kpis_rh <- reactive({
    df <- dados_filtrados()
    n_ativos       <- df %>% dplyr::filter(is.na(data_desligamento)) %>% nrow()
    n_contratacoes <- df %>% dplyr::filter(!is.na(data_admissao)) %>% nrow()
    n_deslig       <- df %>% dplyr::filter(!is.na(data_desligamento)) %>% nrow()
    turnover       <- ifelse(n_contratacoes > 0, n_deslig / n_contratacoes, NA_real_)
    
    list(
      ativos        = n_ativos,
      contratacoes  = n_contratacoes,
      desligamentos = n_deslig,
      turnover      = turnover
    )
  })
  
  output$kpi_ativos        <- renderText(kpis_rh()$ativos)
  output$kpi_contratacoes  <- renderText(kpis_rh()$contratacoes)
  output$kpi_desligamentos <- renderText(kpis_rh()$desligamentos)
  output$kpi_turnover      <- renderText(format_percent(kpis_rh()$turnover))
  
  output$kpi_ativos_comment <- renderText({
    df <- dados_filtrados()
    n_ativos <- df %>% dplyr::filter(is.na(data_desligamento)) %>% nrow()
    paste0("Número de colaboradores atualmente ativos considerando o filtro: ", n_ativos, ".")
  })
  output$kpi_contratacoes_comment <- renderText({
    df <- dados_filtrados()
    n_adm <- df %>% dplyr::filter(!is.na(data_admissao)) %>% nrow()
    paste0("Total de contratações registradas no recorte atual: ", n_adm, ".")
  })
  output$kpi_desligamentos_comment <- renderText({
    df <- dados_filtrados()
    n_desl <- df %>% dplyr::filter(!is.na(data_desligamento)) %>% nrow()
    if (n_desl == 0) "Nenhum desligamento nos filtros atuais."
    else paste0("Total de desligamentos registrados na base filtrada: ", n_desl, ".")
  })
  output$kpi_turnover_comment <- renderText({
    tv <- kpis_rh()$turnover
    if (is.na(tv)) "Turnover não pôde ser calculado com os dados filtrados."
    else paste0("Turnover (desligamentos / contratações) no período analisado: ",
                format_percent(tv), ".")
  })
  
  # --------- CONTRATAÇÕES x DESLIGAMENTOS / % DEMISSÃO -------------
  output$chart_contr_desl <- renderHighchart({
    df <- dados_filtrados()
    if (nrow(df) == 0) return(highchart() %>% hc_title(text = "Sem dados para exibir."))
    
    df_contr <- df %>% dplyr::filter(!is.na(ano_admissao)) %>%
      dplyr::count(ano_admissao, name = "contratacoes")
    df_desl  <- df %>% dplyr::filter(!is.na(ano_desligamento)) %>%
      dplyr::count(ano_desligamento, name = "desligamentos")
    
    anos <- sort(unique(c(df_contr$ano_admissao, df_desl$ano_desligamento)))
    base <- tibble::tibble(ano = anos) %>%
      dplyr::left_join(df_contr, by = c("ano" = "ano_admissao")) %>%
      dplyr::left_join(df_desl,  by = c("ano" = "ano_desligamento")) %>%
      tidyr::replace_na(list(contratacoes = 0L, desligamentos = 0L))
    
    highchart() %>%
      hc_chart(type = "column") %>%
      hc_xAxis(categories = base$ano, title = list(text = "Ano")) %>%
      hc_yAxis(title = list(text = "Quantidade")) %>%
      hc_add_series(name = "Contratações", data = base$contratacoes, color = "#1f3b73") %>%
      hc_add_series(name = "Desligamentos", data = base$desligamentos, color = "#d9534f") %>%
      hc_tooltip(shared = TRUE)
  })
  
  output$chart_turnover <- renderHighchart({
    df <- dados_filtrados()
    if (nrow(df) == 0) return(highchart() %>% hc_title(text = "Sem dados para exibir."))
    
    df_contr <- df %>% dplyr::filter(!is.na(ano_admissao)) %>%
      dplyr::count(ano_admissao, name = "contratacoes")
    df_desl  <- df %>% dplyr::filter(!is.na(ano_desligamento)) %>%
      dplyr::count(ano_desligamento, name = "desligamentos")
    
    anos <- sort(unique(c(df_contr$ano_admissao, df_desl$ano_desligamento)))
    base <- tibble::tibble(ano = anos) %>%
      dplyr::left_join(df_contr, by = c("ano" = "ano_admissao")) %>%
      dplyr::left_join(df_desl,  by = c("ano" = "ano_desligamento")) %>%
      tidyr::replace_na(list(contratacoes = 0L, desligamentos = 0L)) %>%
      dplyr::mutate(perc_deslig = dplyr::if_else(contratacoes > 0,
                                                 desligamentos / contratacoes,
                                                 NA_real_))
    
    base <- base %>% dplyr::filter(!is.na(perc_deslig))
    if (nrow(base) == 0) return(highchart() %>% hc_title(text = "Turnover não calculado para o recorte atual."))
    
    highchart() %>%
      hc_chart(type = "line") %>%
      hc_xAxis(categories = base$ano, title = list(text = "Ano")) %>%
      hc_yAxis(title = list(text = "% Demissão"), labels = list(format = "{value}%")) %>%
      hc_add_series(name = "% Demissão",
                    data  = round(base$perc_deslig * 100, 2),
                    color = "#1f3b73") %>%
      hc_tooltip(pointFormat = "<b>{point.y:.2f}%</b>")
  })
  
  # ------------------- TABELA SALÁRIOS ------------------------------
  output$tabela_salarios <- renderReactable({
    df <- dados_filtrados()
    if (nrow(df) == 0) return(NULL)
    
    df_tab <- df %>%
      dplyr::mutate(salario_fmt = format_currency(salario)) %>%
      dplyr::arrange(dplyr::desc(salario)) %>%
      dplyr::select(`Funcionário` = nome_completo,
                    `Salário`     = salario_fmt)
    
    reactable(
      df_tab,
      searchable  = TRUE,
      pagination  = FALSE,   # sem paginação
      height      = 520      # altura maior para preencher o card
    )
  })
  
  output$comment_salarios <- renderText({
    df <- dados_filtrados()
    if (nrow(df) == 0) return("Nenhum funcionário nos filtros atuais.")
    total_folha <- sum(df$salario, na.rm = TRUE)
    paste0("Total da folha de pagamento dos funcionários filtrados: ",
           format_currency(total_folha), ".")
  })
  
  # ------------------- FILTRO E DADOS DESLIGAMENTOS ----------------
  output$filtros_deslig <- renderUI({
    df <- dados() %>% dplyr::filter(!is.na(data_desligamento))
    if (nrow(df) == 0) return(NULL)
    
    data_min <- min(df$data_desligamento, na.rm = TRUE)
    data_max <- max(df$data_desligamento, na.rm = TRUE)
    
    fluidRow(
      class = "filters-row",
      column(
        width = 3, offset = 9,
        dateRangeInput(
          ns("filtro_data_desl"), "Datas",
          start = data_min, end = data_max,
          min = data_min, max = data_max,
          format = "dd/mm/yyyy", language = "pt-BR"
        )
      )
    )
  })
  
  dados_deslig <- reactive({
    df <- dados() %>% dplyr::filter(!is.na(data_desligamento))
    if (!is.null(input$filtro_data_desl) &&
        length(input$filtro_data_desl) == 2 &&
        !any(is.na(input$filtro_data_desl))) {
      df <- df %>%
        dplyr::filter(
          data_desligamento >= input$filtro_data_desl[1],
          data_desligamento <= input$filtro_data_desl[2]
        )
    }
    df %>%
      dplyr::mutate(
        idade = as.integer(
          floor(as.numeric(difftime(data_desligamento, data_nascimento,
                                    units = "days")) / 365.25)
        ),
        faixa_idade = dplyr::case_when(
          idade < 30                ~ "18-30 anos",
          idade >= 30 & idade <= 40 ~ "31-40 anos",
          idade > 40                ~ "41-60 anos",
          TRUE                      ~ NA_character_
        )
      )
  })
  
  # ------- GRÁFICOS DESLIGAMENTOS (helpers) -------------------------
  make_chart_deslig_estado <- function() {
    df <- dados_deslig()
    if (nrow(df) == 0)
      return(highchart() %>% hc_title(text = "Sem desligamentos no período selecionado."))
    
    base <- df %>%
      dplyr::count(estado, name = "desligamentos") %>%
      dplyr::mutate(
        total = sum(desligamentos),
        perc  = round(desligamentos / total * 100, 2),
        code  = paste0("br-", tolower(estado))
      )
    
    hcmap(
      "countries/br/br-all",
      data        = base,
      value       = "desligamentos",
      joinBy      = c("hc-key", "code"),
      name        = "Desligamentos",
      borderColor = "#ffffff",
      borderWidth = 0.4
    ) %>%
      hc_colorAxis(
        minColor = "#fde0dd",
        maxColor = "#d9534f"
      ) %>%
      hc_chart(
        margin  = 0,
        spacing = 0
      ) %>%
      hc_mapNavigation(enabled = FALSE) %>%
      hc_legend(enabled = FALSE) %>%
      hc_title(text = NULL) %>%
      hc_tooltip(
        headerFormat = "",
        pointFormat  = paste0(
          "<b>{point.name}</b><br/>",
          "Desligamentos: <b>{point.desligamentos}</b><br/>",
          "Participação: <b>{point.perc}%</b>"
        )
      )
  }
  
  make_chart_deslig_idade <- function() {
    df <- dados_deslig() %>% dplyr::filter(!is.na(faixa_idade))
    if (nrow(df) == 0)
      return(highchart() %>% hc_title(text = "Sem dados de faixa etária no período selecionado."))
    
    base <- df %>% dplyr::count(faixa_idade, name = "desligamentos") %>%
      dplyr::arrange(match(faixa_idade,
                           c("18-30 anos","31-40 anos","41-60 anos")))
    
    highchart() %>%
      hc_chart(type = "column") %>%
      hc_xAxis(categories = base$faixa_idade, title = list(text = NULL)) %>%
      hc_yAxis(title = list(text = "Desligamentos")) %>%
      hc_add_series(name = "Desligamentos",
                    data  = base$desligamentos,
                    color = "#d9534f")
  }
  
  make_chart_deslig_nivel <- function() {
    df <- dados_deslig()
    if (nrow(df) == 0)
      return(highchart() %>% hc_title(text = "Sem desligamentos no período selecionado."))
    
    base <- df %>% dplyr::count(nivel, name = "desligamentos") %>%
      dplyr::arrange(dplyr::desc(desligamentos))
    
    highchart() %>%
      hc_chart(type = "bar") %>%
      hc_xAxis(categories = base$nivel, title = list(text = NULL)) %>%
      hc_yAxis(title = list(text = "Desligamentos")) %>%
      hc_add_series(name = "Desligamentos",
                    data  = base$desligamentos,
                    color = "#d9534f")
  }
  
  make_chart_deslig_area <- function() {
    df <- dados_deslig()
    if (nrow(df) == 0)
      return(highchart() %>% hc_title(text = "Sem desligamentos no período selecionado."))
    
    base <- df %>% dplyr::count(area, name = "desligamentos") %>%
      dplyr::arrange(dplyr::desc(desligamentos))
    
    highchart() %>%
      hc_chart(type = "bar") %>%
      hc_xAxis(categories = base$area, title = list(text = NULL)) %>%
      hc_yAxis(title = list(text = "Desligamentos")) %>%
      hc_add_series(name = "Desligamentos",
                    data  = base$desligamentos,
                    color = "#d9534f")
  }
  
  # ---- Outputs Desligamentos – Painel 2 ----------------------------
  output$chart_deslig_estado <- renderHighchart(make_chart_deslig_estado())
  output$chart_deslig_idade  <- renderHighchart(make_chart_deslig_idade())
  output$chart_deslig_nivel  <- renderHighchart(make_chart_deslig_nivel())
  output$chart_deslig_area   <- renderHighchart(make_chart_deslig_area())
  
  # ---- Outputs Desligamentos – Painel 1 (IDs diferentes) -----------
  output$chart_deslig_estado_p1 <- renderHighchart(make_chart_deslig_estado())
  output$chart_deslig_idade_p1  <- renderHighchart(make_chart_deslig_idade())
  output$chart_deslig_nivel_p1  <- renderHighchart(make_chart_deslig_nivel())
  output$chart_deslig_area_p1   <- renderHighchart(make_chart_deslig_area())
  
  # -------------------- ANÁLISE INDIVIDUAL --------------------------
  output$select_funcionario_ui <- renderUI({
    df <- dados()
    if (nrow(df) == 0) return(NULL)
    choices <- sort(unique(df$nome_completo))
    selectInput(
      ns("funcionario_select"),
      "Funcionário",
      choices  = choices,
      selected = choices[1]
    )
  })
  
  func_sel <- reactive({
    df <- dados()
    req(input$funcionario_select)
    f <- df %>% dplyr::filter(nome_completo == input$funcionario_select)
    if (nrow(f) == 0) df[0, ] else f[1, ]
  })
  
  # Infos texto painel Análise Individual ----------------------------
  output$info_nome <- renderText({
    f <- func_sel()
    if (nrow(f) == 0) return("Selecione um funcionário")
    f$nome_completo
  })
  output$info_nivel <- renderText({
    f <- func_sel()
    if (nrow(f) == 0) return("")
    paste("Nível:", f$nivel)
  })
  output$info_area <- renderText({
    f <- func_sel()
    if (nrow(f) == 0) return("")
    paste("Área:", f$area)
  })
  
  # Infos texto resumo Painel 1 --------------------------------------
  output$info_nome_p1 <- renderText({
    f <- func_sel()
    if (nrow(f) == 0) return("Selecione um funcionário")
    f$nome_completo
  })
  output$info_nivel_p1 <- renderText({
    f <- func_sel()
    if (nrow(f) == 0) return("")
    paste("Nível:", f$nivel)
  })
  output$info_area_p1 <- renderText({
    f <- func_sel()
    if (nrow(f) == 0) return("")
    paste("Área:", f$area)
  })
  
  # ---------------------- GAUGE -------------------------------------
  make_chart_gauge <- function() {
    f <- func_sel()
    if (nrow(f) == 0) {
      nota <- 0
    } else {
      nota <- f$avaliacao_geral[1]
      if (is.null(nota) || is.na(nota)) nota <- 0
    }
    
    highchart() %>%
      hc_chart(
        type = "gauge",
        plotBackgroundColor = NULL,
        plotBorderWidth = 0,
        plotShadow = FALSE
      ) %>%
      hc_pane(
        startAngle = -90,
        endAngle   = 90,
        center     = list("50%", "80%"),
        background = list(
          list(
            outerRadius     = "100%",
            innerRadius     = "60%",
            shape           = "arc",
            borderWidth     = 0,
            backgroundColor = "#f3f4f6"
          )
        )
      ) %>%
      hc_yAxis(
        min = 0,
        max = 10,
        lineWidth = 0,
        tickWidth = 0,
        minorTickInterval = NULL,
        labels = list(
          distance = -30,
          style = list(fontSize = "11px", color = "#6b7280")
        ),
        plotBands = list(
          list(
            from      = 0,
            to        = 10,
            color     = "#D4AF37",
            thickness = "18%"
          )
        )
      ) %>%
      hc_add_series(
        name = "Avaliação",
        data = list(nota),
        dataLabels = list(
          borderWidth = 0,
          useHTML     = TRUE,
          y           = -40,
          format = paste0(
            '<div style="text-align:center;">',
            '<span style="font-size:28px; font-weight:700; color:#111827;">{y:.2f}</span><br/>',
            '<span style="font-size:12px; color:#6b7280;">Avaliação do funcionário</span>',
            '</div>'
          )
        ),
        tooltip = list(valueSuffix = " de 10")
      ) %>%
      hc_plotOptions(
        gauge = list(
          dial = list(
            radius          = "80%",
            backgroundColor = "#111827",
            baseWidth       = 8,
            topWidth        = 4,
            baseLength      = "0%",
            rearLength      = "0%"
          ),
          pivot = list(
            radius          = 7,
            backgroundColor = "#111827"
          )
        )
      ) %>%
      hc_tooltip(enabled = FALSE)
  }
  
  output$chart_gauge    <- renderHighchart(make_chart_gauge())
  output$chart_gauge_p1 <- renderHighchart(make_chart_gauge())
  
  output$comment_gauge <- renderText({
    f <- func_sel()
    if (nrow(f) == 0) return("")
    nota <- f$avaliacao_geral[1]
    if (is.null(nota) || is.na(nota)) {
      return("Este funcionário ainda não possui avaliação registrada na base de dados.")
    }
    paste0("Nota geral de desempenho registrada na base: ",
           round(nota, 2),
           " em uma escala de 0 a 10.")
  })
  
  # --------------------- COMPETÊNCIAS -------------------------------
  make_chart_comp <- function() {
    f <- func_sel()
    if (nrow(f) == 0) {
      return(highchart() %>% hc_title(text = "Selecione um funcionário para visualizar as notas."))
    }
    
    comps <- tibble::tibble(
      comp  = c("Comunicação", "Trabalho em Equipe", "Organização",
                "Liderança", "Iniciativa"),
      valor = c(
        f$nota_comunicacao[1],
        f$nota_trabalho_equipe[1],
        f$nota_organizacao[1],
        f$nota_lideranca[1],
        f$nota_iniciativa[1]
      )
    )
    
    if (all(is.na(comps$valor))) {
      return(highchart() %>% hc_title(text = "Funcionário sem notas de competência cadastradas na base."))
    }
    
    comps <- comps %>% tidyr::replace_na(list(valor = 0))
    
    hchart(comps, "pie", hcaes(name = comp, y = valor)) %>%
      hc_plotOptions(pie = list(innerSize = "60%")) %>%
      hc_title(text = NULL)
  }
  
  output$chart_competencias    <- renderHighchart(make_chart_comp())
  output$chart_competencias_p1 <- renderHighchart(make_chart_comp())
  
  # Tabela informações helper ----------------------------------------
  make_table_info <- function() {
    f <- func_sel()
    if (nrow(f) == 0) return(NULL)
    
    idade <- if (!is.na(f$data_nascimento[1])) {
      as.integer(floor(as.numeric(difftime(Sys.Date(), f$data_nascimento[1],
                                           units = "days")) / 365.25))
    } else NA_integer_
    
    horas_extras_val <- if ("horas_extras" %in% names(f)) f$horas_extras[1] else NA
    ferias_val       <- if ("ferias_acumuladas" %in% names(f)) f$ferias_acumuladas[1] else NA
    
    data.frame(
      `Nome Completo`      = f$nome_completo[1],
      `Data de Nascimento` = if (!is.na(f$data_nascimento[1]))
        format(f$data_nascimento[1], "%d/%m/%Y") else NA,
      Estado               = f$estado[1],
      Salário              = format_currency(f$salario[1]),
      Idade                = idade,
      `Horas Extras`       = horas_extras_val,
      `Férias Acumuladas`  = ferias_val,
      check.names = FALSE
    )
  }
  
  output$tabela_info_func    <- renderTable(make_table_info())
  output$tabela_info_func_p1 <- renderTable(make_table_info())
  
  # --------------------------- LOGOUT -------------------------------
  observeEvent(input$logout, {
    user_rv$logged_in <- FALSE
    user_rv$email     <- NULL
  })
}
