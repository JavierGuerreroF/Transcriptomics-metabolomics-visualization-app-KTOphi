
## TRANSCRIPTOMICS - METABOLOMICS APP
# JAVIER GUERRERO FLORES
# 11/12/23


library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(ggpubr)
library(RColorBrewer)
library('plotly')
library(DT)
library(stringr)
library(waiter)

#error_plot

error_plot_df <- data.frame(message = "NO INFO AVAILABLE")

error_plot <-  ggplot(error_plot_df, aes(x = 1, y = 1, label = message)) + 
  geom_text(size = 10, color = "black", vjust = 0.5, hjust = 0.5) +
  theme_void()

#pie 
final_table <- read.csv("data/all_ph_with_classes.csv")
print(str(final_table))
conditions_all <- grep("\\.m",colnames(final_table),value=T)
conditions <- grep("BWCOO\\.1w",conditions_all,value=T)
print(conditions)

classes_all <- grep("\\.m",colnames(final_table)[-1],invert=T,value = T)
classes_all <- grep("Compound",classes_all,invert = T,value =T)
classes_all <- grep("feature_",classes_all,invert = T,value = T)

# palletes
palette_list <- c("Spectral","Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1", "Set2", "Set3", 
                  "Sequential", "Blues", "BuGn", "BuPu", "GnBu", "Greens", "Greys", "Oranges", 
                  "OrRd", "PuBu", "PuBuGn", "PuRd", "Purples", "RdPu", "Reds", "YlGn", "YlGnBu", 
                  "YlOrBr", "YlOrRd")
# Gene lists
KT_genes <- read.csv(paste0("data/diff_expressed_genes_tsv/KT_all_merged.csv"))
KT_genes <- unique(KT_genes$id)

Ophiostoma_genes <- read.csv(paste0("data/diff_expressed_genes_tsv/Ophiostoma_all_merged.csv"))
Ophiostoma_genes <- unique(Ophiostoma_genes$id)

# Constants
sideWidth <- 300

##########################

ui <- dashboardPage(
  # useWaiter(),
  # skin="purple",
  dashboardHeader(
    title = "Visualization app",
    titleWidth = sideWidth
  ),
  dashboardSidebar(
    width = sideWidth,
    #####
    sidebarMenu(id="tabs",
                menuItem("Transcriptomics", tabName = "transcriptomics_tab",icon=icon("dna"),
                         startExpanded = T,
                         menuSubItem("Volcano plots",
                                     tabName = "volcano_plots",
                                     icon=icon("chart-bar")),
                         menuSubItem("Gene search",
                                     tabName = "gene_search",
                                     icon=icon("magnifying-glass"))),
                menuItem("Metabolomics",tabName = "metabolomics_tab",icon=icon("chart-pie"))#,icon=icon("magnifying-glass")),
    ),
    hr(),
    
    
    ###> TRANSCRIPTOMIC SIDEBAR
    
    conditionalPanel(
      condition = "input.tabs == 'volcano_plots'",
      radioGroupButtons(
        inputId = "organism",
        label = "Reference genome (Organism):",
        choices = c("Ophiostoma","KT"),
        justified = TRUE,
        size="lg",
        checkIcon = list(
          yes = tags$i(class = "fa fa-check-square",
                       style = "color: steelblue"),
          no = tags$i(class = "fa fa-square-o",
                      style = "color: steelblue")
        )
      ),
      hr(),
      radioGroupButtons(
        inputId = "analysis",
        label = "Analysis:",
        choices = list(
          "Organism VS consortium"="OVSC",
          "Treatment: <b>FaOH vs CTL</b>"="Treatment",
          "Time: <b>early vs late</b>"="Time"
          # "Medium: <b>GPP vs BWP</b>"="Medium",
         ),
        justified = TRUE,
        direction = "vertical",
        size = "normal",
        checkIcon = list(
          yes = tags$i(class = "fa fa-check-square",
                       style = "color: steelblue"),
          no = tags$i(class = "fa fa-square-o",
                      style = "color: steelblue")
        )
      ),
      
      hr(),
      h4("Conditions selection",align="center"),
      conditionalPanel(
        condition = "input.analysis == 'Treatment' | input.analysis == 'Time' | input.analysis == 'Medium'",
        radioGroupButtons(
          inputId = "consortium",
          label = "Consortium:",
          choices = list("Yes"=TRUE,"No"=FALSE),
          justified = TRUE,
          size = "normal",
          checkIcon = list(
            yes = tags$i(class = "fa fa-check-square",
                         style = "color: steelblue"),
            no = tags$i(class = "fa fa-square-o",
                        style = "color: steelblue")
          )
        )
      ),
      
      # # changed
      # conditionalPanel(
      #   condition = "input.analysis == 'Treatment' | input.analysis == 'Time' | input.analysis == 'OVSC'",
      #   radioGroupButtons(
      #     inputId = "contrast_medium",
      #     label = "Medium:",
      #     choices = c("GPP","BWP"),
      #     size = "normal",
      #     direction =  "horizontal",
      #     justified = TRUE,
      #     checkIcon = list(
      #       yes = tags$i(class = "fa fa-check-square",
      #                    style = "color: steelblue"),
      #       no = tags$i(class = "fa fa-square-o",
      #                   style = "color: steelblue")
      #     ))
      # ),
      conditionalPanel(
        condition = "input.analysis == 'Medium' | input.analysis == 'Time' | input.analysis == 'OVSC'",
        radioGroupButtons(
          inputId = "contrast_treatment",
          label = "Treatment:",
          choices = c("FaOH","CTL"),
          size = "normal",
          direction =  "horizontal",
          justified = TRUE,
          selected=NULL,
          checkIcon = list(
            yes = tags$i(class = "fa fa-check-square",
                         style = "color: steelblue"),
            no = tags$i(class = "fa fa-square-o",
                        style = "color: steelblue")
          )
        )
      ),
      
      # changed
      conditionalPanel(
        condition = "input.analysis == 'Medium' | input.analysis == 'Treatment'  | input.analysis == 'OVSC'",
        radioGroupButtons(
          inputId = "contrast_time",
          label = "Time:",
          choices = c("early","late"),
          size = "normal",
          direction =  "horizontal",
          selected=NULL,
          justified = TRUE,
          checkIcon = list(
            yes = tags$i(class = "fa fa-check-square",
                         style = "color: steelblue"),
            no = tags$i(class = "fa fa-square-o",
                        style = "color: steelblue")
          )
        ))
    )
    
    ###< TRANSCRIPTOMIC SIDEBAR
    
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "volcano_plots",
              # TITLE
              box(width = NULL,
                  status="primary",
                  h2(textOutput("condition"),align="center"),
                  h1(textOutput("contrast"),align="center"),
              ),
              
              # ROW 1

              fluidRow(
                column(
                  8,
                  box(width = 12,
                      title="Volcano plot:",
                      plotOutput('plot_volcano')
                  )),
                column(
                  4,box(width = 12,
                        title = "Values selection:", status = "primary", solidHeader = TRUE,
                        collapsible = TRUE,
                        sliderInput("logfc", "Log Fold-Change:",
                                    min = 0, max = 10,step = 0.1,
                                    value = 2),

                        # sliderTextInput(inputId = "pval",
                        #                 label = "P-value:",
                        #                 choices = c(0.001,0.005,0.01,0.05)
                        #                 ),
                        # sliderInput("pval", "P-value:",
                        #             min = 0, max = 1,step = 0.01,
                        #             value = 0.5)),
                        sliderTextInput("pval", "P-value:",
                                        choices = c(0.001,0.005,0.01,0.05),
                                        selected = 0.05,
                                        grid = TRUE
                        )
                  ),
                  fluidRow(
                    column(6,
                           valueBoxOutput('valuebox_over',width = 12)
                    ),
                    column(6,
                           valueBoxOutput('valuebox_under',width = 12)
                    )
                  )

                 )
              ),

              fluidRow(
                column(6,box(status="danger",#background = "red",
                             width = 12,
                             title = "Over:",
                             dataTableOutput('genes_over')
                )),
                column(6,box(status="primary",#background = "blue",
                             width = 12,
                             title = "Under:",
                             dataTableOutput('genes_under')
                ))

              )
              
              ######
              ),
      
        tabItem(tabName = "gene_search",
                
                fluidRow(
                  column(6,
                         box(status="primary",
                             width = NULL,
                             title = "Selected gene barplot:",
                             plotOutput('selected_plot')),
                  ),
                  column(6,
                         
                         box(
                           width = NULL,
                           title = "Gene selection:",
                           status = "primary", 
                           solidHeader = TRUE,
                           fluidRow(
                             column(6,
                                    conditionalPanel(
                                      condition = "input.organism_search == 'KT'",
                                      pickerInput(
                                        inputId = "KT_selected_gene",
                                        label = "Select gene to display:",
                                        choices = KT_genes,
                                        options = list(
                                          `actions-box` =TRUE,
                                          `live-search` = TRUE
                                        ),
                                        multiple = FALSE
                                      )
                                      
                                    ),
                                    conditionalPanel(
                                      condition = "input.organism_search == 'Ophiostoma'",
                                      pickerInput(
                                        inputId = "Ophiostoma_selected_gene",
                                        label = "Select gene to display:",
                                        choices = Ophiostoma_genes,
                                        options = list(
                                          `actions-box` =TRUE,
                                          `live-search` = TRUE
                                        ),
                                        multiple = FALSE
                                      ) 
                                    )
                             ),
                             column(6,
                                    radioGroupButtons(
                                      inputId = "organism_search",
                                      label = "Reference genome (Organism):",
                                      choices = c("Ophiostoma","KT"),
                                      justified = TRUE,
                                      size="lg",
                                      checkIcon = list(
                                        yes = tags$i(class = "fa fa-check-square", 
                                                     style = "color: steelblue"),
                                        no = tags$i(class = "fa fa-square-o", 
                                                    style = "color: steelblue")
                                      )
                                    )
                             )
                             
                             
                             
                           )
                           
                         ), 
                         
                         box(width = NULL,
                             title = "Values selection:",
                             status = "primary", 
                             solidHeader = TRUE,
                             # collapsible = TRUE,
                             # collapsed = T,
                             fluidRow(
                               column(6,
                                      sliderInput("logfc_search", "Log Fold-Change:",
                                                  min = 0, max = 10,step = 0.1,
                                                  value = 2)),
                               column(6,
                                      sliderTextInput("pval_search", "P-value:",
                                                      choices = c(0.001,0.005,0.01,0.05,1),
                                                      selected = 1,
                                                      grid = TRUE)
                                      )
                             )
                             
                             
                         )
                         
                  )
                ),
                
         
                box(width = NULL,
                    title = "Gene information:",
                    status = "primary", 
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    collapsed = T,
                    dataTableOutput('selected_table_geneinfo')),
                
                
 
                
                # box(status="primary",
                #     width = NULL,
                #     title = "Selected gene:",
                #     dataTableOutput('selected_table'))
                
        ),
        tabItem(tabName = "metabolomics_tab",
                
                ###
                fluidRow(
                  column(7,
                         box(width = NULL,
                             title = "Pie Chart:",
                             plotOutput("pie_chart"),
                             plotOutput("pie_chart_legend")
                         )
                  ),
                  column(5,
                         box(width = NULL,
                             collapsible = T,
                             # collapsed = T,
                             title = "Parameters:", status = "primary", solidHeader = TRUE,
                             sliderTextInput("filter_ph_value",
                                             "Peak height filter:",
                                             choices=c(
                                               10^4, 
                                               10^5, 
                                               10^6, 
                                               10^7),
                                             selected=10^6,
                                             grid = T),
                             pickerInput(
                               inputId = "condition_selected",
                               label = "Sample:",
                               choices = c(
                                "No organism - BWCOO - 1 week"= conditions[1],
                                "KT - BWCOO - 1 week"=conditions[3],
                                "Ophiostoma - BWCOO - 1 week"=conditions[5],
                                "Consortium - BWCOO - 1 week"=conditions[7],
                                "No organism - FaOH - BWCOO - 1 week"= conditions[2],
                                "KT - FaOH - BWCOO - 1 week"=conditions[4],
                                "Ophiostoma - FaOH - BWCOO - 1 week"=conditions[6],
                                "Consortium - FaOH - BWCOO - 1 week"=conditions[8]

                               ),
                               selected = conditions[1],
                               options = list(
                                 size = 7)
                             ),
                             
                             # checkboxGroupButtons(
                             #   inputId = "condition_selected",
                             #   label = "Condition:",
                             #   choices = conditions,
                             #   selected = conditions[1]
                             # )
                             
                             pickerInput(
                               inputId = "selected_class",
                               label = "Classes selection:",
                               choices = classes_all[c(1,2,3)],
                               selected = classes_all[1]
                             ),
                             sliderInput("percent_filter",
                                         "Percent filter:",
                                         0,
                                         5,
                                         1,
                                         step = 0.1),
                             fluidRow(
                               column(6,
                                      pickerInput(
                                        inputId = "palette_selected",
                                        label = "Palette list:",
                                        choices = palette_list,
                                        selected = "Spectral",
                                        options = list(
                                          size = 7)
                                      )
                               ),
                               column(6,
                                      # align = "center", 
                                      materialSwitch(
                                        status = "primary",
                                        inputId = "NAs_show",
                                        label = "Show N/As or no matches", 
                                        right = TRUE,
                                      )
                                      # switchInput(
                                      #   labelWidth = "700px",
                                      #   inputId = "NAs_show",
                                      #   onLabel = "Show",
                                      #   offLabel = "Hide",
                                      #   label = "N/As or no matches"
                                      # )
                               )
                             )
                         ),
                         box(width = NULL,
                             style = "overflow-x: auto;",
                             title = "Frequency of features in category:",
                             status = "primary",
                             solidHeader = TRUE,
                             collapsible = TRUE,
                             htmlOutput("total_metabolites_sum"),
                             dataTableOutput('pie_chart_df_selected_to_DT'))
                         
                         
                  )
                )
                ###
        )

    ),
    

    
    
  )
)


server <- function(input, output, session) {
  
  # waiter
  # w = Waiter$new() # 2. Initialize
  # w$show() # 3. Program
  # Sys.sleep(3)
  # w$hide() # 4. Hide
  
  # reactive values
  vals <- reactiveValues()
  tables <- reactiveValues()
  plots <- reactiveValues()
  
  ## Transcriptomic
  ###> Volcanos
  observe({

    vals$organism <- input$organism
    #if(input$analysis == "OVSC"){vals$culture<-""}
    if(input$consortium){vals$culture<-"consortium"}else{vals$culture<-""}
    vals$analysis <- input$analysis
    
    vals$parameter <- input$parameter
    
    
    vals$contrast_time <- input$contrast_time
    vals$contrast_treatment <- input$contrast_treatment
    vals$contrast_medium <- "BWP"
    
  
    # vals$contrast_medium <- input$contrast_medium
    # vals$contrast_treatment <- input$contrast_treatment
    
    # names
    if(vals$analysis=="Medium"){ vals$contrast_medium <- "";vals$anal_contrast<-"GPP vs BWP"}
    if(vals$analysis=="Treatment"){ vals$contrast_treatment<- "";vals$anal_contrast<-"FaOH vs Control"}
    if(vals$analysis=="Time"){ vals$contrast_time <- "";vals$anal_contrast<-"early vs late"}
    if(vals$analysis=="OVSC"){ vals$contrast_OVSC<- "";vals$anal_contrast<-"Organism vs Consortium";vals$culture<-""}
    
    
    # conditions
    
    if(vals$analysis=="Medium"){condition_1react=vals$contrast_treatment;condition_2react=vals$contrast_time}
    if(vals$analysis=="Treatment"){condition_1react=vals$contrast_medium;condition_2react=vals$contrast_time}
    if(vals$analysis=="Time"){condition_1react=vals$contrast_medium;condition_2react=vals$contrast_treatment}
    
    if(vals$analysis=="OVSC"){condition_1react=vals$contrast_treatment;condition_2react=vals$contrast_medium;condition_3react=vals$contrast_time}
    if(vals$analysis=="OVSC"){condition_1react=vals$contrast_treatment;condition_2react=vals$contrast_medium;condition_3react=vals$contrast_time}
    
    
    if(vals$analysis=="OVSC"){
      vals$sample <- paste0(paste0(condition_1react,".",condition_2react,".",condition_3react))
    }else{
      vals$sample <- paste0(paste0(condition_1react,".",condition_2react))
    }
    
    print(vals$contrast_time)
    print(vals$contrast_treatment)
    print(vals$contrast_medium)
    
    print(vals$sample)
  })
  
  
  observe({
    #"/adg_",organism,"_",culture,"_",analysis,"_",sample,".tsv"
    
    #"/adg_",organism,"_",culture,"_OVSC_",sample,".tsv")
    pval <- input$pval
    logfc <- input$logfc
    if(is.null(pval)){pval <- 0.05}
    if(is.null(logfc)){logfc <- 2}
    
    
    print("filters pval logfc")
    print(pval)
    print(logfc)
    
    ophi_best_hits <- read.csv(paste0("data/Parse_JGI/best_hits_data.csv"), header = F)
    colnames(ophi_best_hits) <- c("id","blasp_besthit")
    
    ophi_domains_info <- read.csv(paste0("data/ophi_domains_annotations/domain_info_ophi_html.tsv"), header = T,sep="\t")
    
    genes_filename <- paste0("data/diff_expressed_genes_tsv/adg_",vals$organism,"_",vals$culture,"_",vals$analysis,"_",vals$sample,".tsv")
   
    cat("\n\n")
    print("genes_filename")
    print(genes_filename)
    cat("\n\n")
    
    genes <- read.csv(genes_filename,sep = "\t", header = T)
    
    
    genes <- unique(genes)
    genes <- genes %>% filter(padj < pval)
    
    genes <- merge(x = genes, y = ophi_best_hits, by = "id", all.x = TRUE)
    genes <- merge(x = genes, y = ophi_domains_info, by = "id", all.x = TRUE)
 
    genes_over <- genes %>% filter(log2FoldChange > logfc)
    genes_over <- arrange(genes_over,desc(log2FoldChange))
    
    genes_under <- genes %>% filter(log2FoldChange < -logfc)
    genes_under <- arrange(genes_under,log2FoldChange)
    
    genes_under_ori <- genes_under
    genes_over_ori  <- genes_over
    
    if(vals$organism=="Ophiostoma"){
      genes_over[,1]<-sapply(genes_over[,1],function(x){
        paste0(
          '<a href="https://mycocosm.jgi.doe.gov/cgi-bin/dispGeneModel?db=OphpiCECT20416_2&id=',x,'" target="_blank">',x,'</a>'
        )
      })
      genes_under[,1]<-sapply(genes_under[,1],function(x){
        paste0(
          '<a href="https://mycocosm.jgi.doe.gov/cgi-bin/dispGeneModel?db=OphpiCECT20416_2&id=',x,'" target="_blank">',x,'</a>'
        )
      })
    }
    
    if(vals$organism == "KT"){
      genes_over[,1]<-sapply(genes_over[,1],function(x){
        paste0(
          '<a href="https://www.genome.jp/dbget-bin/www_bget?ppu:',x,'" target="_blank">',x,'</a>'
        )
      })
      genes_under[,1]<-sapply(genes_under[,1],function(x){
        paste0(
          '<a href="https://www.genome.jp/dbget-bin/www_bget?ppu:',x,'" target="_blank">',x,'</a>'
        )
      })
    }
    
    if(nrow(genes_over)==0){genes_over <- genes_over_ori}
    if(nrow(genes_under)==0){genes_under <- genes_under_ori}
    
    if(vals$organism == "KT"){
      tables$genes_over <- select(genes_over,id,log2FoldChange,	product,prot_ID,GO_function,	GO_process	,GO_component	,KEGG_definitions)
      tables$genes_under <- select(genes_under,id,log2FoldChange,	product,prot_ID,GO_function,	GO_process	,GO_component	,KEGG_definitions)
      
    }
    if(vals$organism == "Ophiostoma"){
      tables$genes_over <- select(genes_over,id,	log2FoldChange,	blasp_besthit,domain_info,	GO_terms,	KEGG	,kogdefline,	InterPro)
      tables$genes_under <- select(genes_under,id,	log2FoldChange,blasp_besthit,	domain_info,	GO_terms,	KEGG	,kogdefline,	InterPro)
    }
    
    
    # genes_volcano <- select(genes,id,log2FoldChange,padj)
    
    # genes <- genes %>% filter(log2FoldChange < -logfc | log2FoldChange > logfc)
    genes_volcano <- select(genes,id,log2FoldChange,padj)
    
    
    #genes_volcano <- select(genes,id,log2FoldChange,padj)
    genes_volcano$padj <- -log10(genes_volcano$padj)
    
 
      
    
    if(nrow(genes_volcano)==0){
      
      genes_volcano[nrow(genes_volcano) + 1,] <- list("No Genes",0, 0)
      
    }
    
 
    
    # add a column of NAs
    genes_volcano$diffexpressed <- "NO"
    genes_volcano$color <- "black"
    
    # if log2Foldchange > 0.6 and pvalue < 0.05, set as "UP"
    genes_volcano$diffexpressed[genes_volcano$log2FoldChange > logfc & genes_volcano$padj > -log10(pval)] <- "UP"
    genes_volcano$color[genes_volcano$log2FoldChange > logfc & genes_volcano$padj > -log10(pval)] <- "red"
    # if log2Foldchange < -0.6 and pvalue < 0.05, set as "DOWN"<
    genes_volcano$diffexpressed[genes_volcano$log2FoldChange < -logfc & genes_volcano$padj > -log10(pval)] <- "DOWN"
    genes_volcano$color[genes_volcano$log2FoldChange < -logfc & genes_volcano$padj > -log10(pval)] <- "blue"
    
    colors_volcano <- levels(as.factor(genes_volcano$color))
    
    plots$plot_volcano <- ggplot(data=genes_volcano, aes(x=log2FoldChange, y=padj , col=diffexpressed, label=id)) +
      geom_point() +
      theme_bw() +
      ylab("-log10(p-value)")+
      xlab("log2(FoldChange)")+
      
      # He quitado lo de mostrar las etiquetas porque ralentiza mucho el tiempo de carga del plot
      #geom_text_repel() +
      scale_color_manual(breaks= genes_volcano$diffexpressed, values= genes_volcano$color) +
      geom_vline(xintercept=c(-logfc, logfc), linetype = "dashed",col="black") +
      geom_hline(yintercept=-log10(pval), linetype = "dotted",col="black")
    
  
    
    tables$genes_volcano <- genes_volcano
    
    genes["padj"] <- NULL
    tables$genes <- genes
    
    #print(str(tables$genes_volcano))
    
  })
  
  
  output$plot_volcano <- renderPlot({plots$plot_volcano})
  
  output$valuebox_over <- renderValueBox({
    valueBox(
      paste0(nrow(tables$genes_over)), "Over", icon = icon("arrow-up"),
      color = "red"
    )
  })
  
  output$valuebox_under <- renderValueBox({
    valueBox(
      nrow(tables$genes_under), "Under", icon = icon("arrow-down"),
      color = "blue"
    )
  })
  
  observe({
    under <- colnames(tables$genes_under)
    over <- colnames(tables$genes_over)
    under[2]<-"fc"
    over[2]<-"fc"
    
    tables$colnames_genes_under <- under
    tables$colnames_genes_over <- over
    
  })
  
  output$genes_over <- renderDataTable({
    req(data())
    datatable(
      tables$genes_over,
      colnames = tables$colnames_genes_over,
      
      #data_underexpressed(),
      escape=FALSE,
      rownames = FALSE,
      selection = "single",
      extensions = 'Buttons',
      options = list(dom = 'Bfrtip',
                     scrollX = T,
                     buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                     pageLength = 15,
                     autoWidth = TRUE,scrollX=FALSE,
                     columnDefs = list(list(width = '20px', targets = 2 ))
      )
    ) %>%
      formatSignif(columns = c( "log2FoldChange"), digits = 3)
    
  })
  
  
  
  output$genes_under <- renderDataTable({
    req(data())
    datatable(
      tables$genes_under,
      colnames = tables$colnames_genes_under,
      #data_underexpressed(),
      escape=FALSE,
      rownames = FALSE,
      selection = "single",
      extensions = 'Buttons',
      options = list(dom = 'Bfrtip',
                     scrollX = T,
                     buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                     pageLength = 15,
                     autoWidth = TRUE,scrollX=FALSE,
                     columnDefs = list(list(width = '20px', targets = 2 ))
      )
    ) %>%
      formatSignif(columns = c( "log2FoldChange"), digits = 3)
    
  })
  # Title text
  output$condition  <- renderText({
    paste0(vals$organism ,' ',
           vals$culture,' ',
           vals$contrast_time,' ',
           vals$contrast_treatment,' ',
           vals$contrast_medium)
    
  })
  
  output$contrast <- renderText({
    paste0(vals$anal_contrast)
  })
  
  
  ###< Volcanos  
  
  
  ###> Gene search
  
  observe({
    vals$organism_search <- input$organism_search
    vals$Ophiostoma_selected_gene <- input$Ophiostoma_selected_gene
    vals$KT_selected_gene <- input$KT_selected_gene
    
    vals$logfc_search <- input$logfc_search
    vals$pval_search <- input$pval_search
    
    if(vals$organism_search=="Ophiostoma"){
      ophiostoma_all_genes <- read.csv("data/diff_expressed_genes_tsv/Ophiostoma_all_merged.csv")
      ophi_selected <- grep(vals$Ophiostoma_selected_gene,ophiostoma_all_genes$id)
      selected_table <- ophiostoma_all_genes[ophi_selected,]
      selected_table[,1] <- selected_table$file_data
      selected_table$file_data <- NULL
      colnames(selected_table)[1] <- "conditions"
      print(str(selected_table))
    }else{
      kt_all_genes <- read.csv("data/diff_expressed_genes_tsv/KT_all_merged.csv")
      kt_selected <- grep(vals$KT_selected_gene,kt_all_genes$id)
      selected_table <- kt_all_genes[kt_selected,]
      selected_table[,1] <- selected_table$file_data
      selected_table$file_data <- NULL
      colnames(selected_table)[1] <- "conditions"
    }
    
    cat("\n## SELECTED TABLE STR")
    print(str(selected_table))
    
    
    
    selected_table_geneinfo <- subset(selected_table, select = -c(conditions,log2FoldChange,padj))
    selected_table_geneinfo <- unique(selected_table_geneinfo)
    tables$selected_table_geneinfo <- selected_table_geneinfo
    
    # print(selected_table_geneinfo)
    
    selected_table[c("contrast","conditions")] <- str_split_fixed(selected_table$conditions,"_",2)
    # selected_table$conditions <-  gsub('_','\n',selected_table$conditions)
    selected_table$conditions <-  gsub('\\.','\n',selected_table$conditions)
    selected_table$conditions <-  gsub('organism','org',selected_table$conditions)
    selected_table$conditions <-  gsub('consortium','cons',selected_table$conditions)
    
    
    selected_table$contrast <- ifelse(selected_table$contrast=="Medium","GPP vs BWP",
                                      ifelse(selected_table$contrast=="Treatment","FaOH vs CTL",
                                             ifelse(selected_table$contrast=="Time","Early vs Late","Org vs Cons")))
    selected_table$overexpressed <- ifelse(selected_table$log2FoldChange>=0,
                                           str_split_fixed(selected_table$contrast," vs ",2)[,1],
                                           str_split_fixed(selected_table$contrast," vs ",2)[,2])
    
    # search pval filter
    selected_table <- selected_table %>% filter(padj < vals$pval_search)

    #selected_table$color <- which(selected_table$lo)
    tables$selected_table <- selected_table
    

    
    
    ## PLOT
    
    # create graph
    error_plot <- data.frame(message = "NO INFO AVAILABLE",
                             message_pval = paste0("p-value = ",vals$pval_search))
    
    if(nrow(selected_table)==0){
      plots$selected_plot <-  ggplot(error_plot, aes(x = 1, y = 1, label = message)) + 
        geom_text(size = 10, color = "black", vjust = 0.5, hjust = 0.5) +
        theme_void()
    }else{
      plots$selected_plot <- ggplot(data = selected_table, aes(x = conditions, y = log2FoldChange)) +
        geom_bar(stat = 'identity', aes(fill = log2FoldChange)) +
        geom_text(aes(
          label = round(log2FoldChange,2),
          x=conditions,
          y=log2FoldChange+ifelse(log2FoldChange>=0,-0.5, 0.5),
          fontface=2
        ),
        vjust=1,
        colour = "black")+
        geom_text(aes(
          label = overexpressed,
          x=conditions,
          y=log2FoldChange+ifelse(log2FoldChange>=0,-0.5, 0.5),
          fontface=2
        ),
        vjust=-1.2,
        colour = "black")+
        geom_hline(yintercept=vals$logfc_search, linetype="dashed", color = "gray70") +
        geom_hline(yintercept=-vals$logfc_search, linetype="dashed", color = "gray70") +
        scale_fill_gradient2(low='blue', mid='snow', high='red') +
        theme_bw()+
        theme(
          strip.background = element_rect(fill = "white"),
          strip.text=element_text(size=14,face="bold"),
          axis.text=element_text(size=12),
          axis.title=element_text(size=14,face="bold"))+
        facet_grid(~contrast, scales = "free_x",space="free_x")
      
      
    }
  })
  
  # outputs
  output$selected_plot <- renderPlot({plots$selected_plot})
  
  
  output$selected_table <- renderDataTable({
    req(data())
    datatable(
      tables$selected_table,
      # data_overexpressed(),
      escape=FALSE,
      rownames=FALSE,
      selection = "single",
      extensions = 'Buttons',
      options = list(dom = 'Bfrtip', 
                     buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                     pageLength = 100)
    )
    #%>%
    #   formatStyle(names(df), backgroundColor = styleInterval(brks, clrs))
    #   formatStyle(
    #     'log2FoldChange',
    #     backgroundColor = styleInterval(c(-20,20), clrs)
    #   )
    # 
    
    
  })
  
  output$selected_table_geneinfo <- renderDataTable({
    req(data())
    datatable(
      tables$selected_table_geneinfo,
      escape=FALSE,
      rownames=FALSE,
      options = list(dom = 't')
    )
    
  })
  
  ###< Gene search
  
  ## Metabolomics
  ###> Metabolomics
  ### PIE CHART
  
  observe({
    # Pie chart con los metabolitos porn encima de X altura de pico
    condition_selected <- input$condition_selected
    selected_class <- input$selected_class
    filter_ph_value <- input$filter_ph_value
    percent_filter <- input$percent_filter
    percent_filter <- percent_filter/100
    
    vals$sel_row_index <- input$pie_chart_df_selected_to_DT_rows_selected
    selected_class_from_table <- tables$pie_chart_df_selected_to_DT[vals$sel_row_index,]$class
    
    
    # print(paste(condition_selected,
    #       selected_class,
    #       filter_ph_value,
    #       percent_filter))
    
    
    feature_classification <- final_table[,grep("\\.m",colnames(final_table),invert=T)]
    
    
    conditions_all <- grep("\\.m",colnames(final_table),value=T)
    
    # conditions <- grep("CTL\\.BWCOO\\.1w",conditions_all,value=T)
    conditions <- grep("BWCOO\\.1w",conditions_all,value=T)
    
    # condition_selected <- conditions[1]
    # filter_ph_value <- 10^6
    # selected_class <- "class"
    # 
    
    
    pie_chart_df <- final_table[,c("feature_info",condition_selected)]
    
    metabolite_list_selected <- final_table[,c("feature_info","Compound_Name",selected_class,condition_selected)]
    
    
    ## TABLA METABOLITOS
    
    if(!is.null(vals$sel_row_index)){
      # if(logical_test){
      if(ncol(metabolite_list_selected)>1){
        
        metabolite_list_selected <- metabolite_list_selected[metabolite_list_selected[,3]==selected_class_from_table,]
        metabolite_list_selected <- metabolite_list_selected[metabolite_list_selected[,4]>filter_ph_value,]#filter(pie_chart_df, 2 > filter_ph_value)
        colnames(metabolite_list_selected)[4] <- "log10_peak_height"
        
        
        
        if(nrow(metabolite_list_selected)==0){
          summarize_df <- data.frame(table(metabolite_list_selected[,2]))
          colnames(metabolite_list_selected) <- c("Compound_Name", "freq")
        }else{
          summarize_df <- aggregate(log10_peak_height ~ Compound_Name, data = metabolite_list_selected, sum)
          summarize_df[,2] <- log10(summarize_df[,2])
        }
        
        
        # cat("\n")
        # print(str(metabolite_list_selected))
        # 
        # print(str(summarize_df))
        # cat("TESTESTSE\n\n\n")
        
        # metabolite_list_selected <- metabolite_list_selected[,-4]
        
        metabolite_list_selected <- data.frame(table(metabolite_list_selected[,2]))
        
        # metabolite_list_selected <- metabolite_list_selected[metabolite_list_selected[,4]>filter_ph_value,]#filter(pie_chart_df, 2 > filter_ph_value)
        # metabolite_list_selected <- metabolite_list_selected[,-4]
        
        ## EROROR not enouihg attt
        colnames(metabolite_list_selected) <- c("Compound_Name", "freq")
        
        metabolite_list_selected <- merge(metabolite_list_selected,summarize_df,by="Compound_Name")
        
        metabolite_list_selected <- metabolite_list_selected[order(metabolite_list_selected$freq,decreasing = T),]
        
      }
      
      tables$metabolite_list_selected <- metabolite_list_selected#[,-3]
      vals$selected_class_from_table <- selected_class_from_table
      # }
    }
    
    
    pie_chart_df <- pie_chart_df[pie_chart_df[,2]>filter_ph_value,]#filter(pie_chart_df, 2 > filter_ph_value)
    pie_chart_df <- merge(pie_chart_df,feature_classification,by="feature_info")
    
    
    
    ## PEAK HEIGHT SUM
    df_all_columns_selected_class <- pie_chart_df[,c(1,2,grep(paste0("^",selected_class,"$"),colnames(pie_chart_df)))]
    colnames(df_all_columns_selected_class) <- c("feature_info","log10_peak_height","classes")
    summarize_df_classes <- aggregate(log10_peak_height ~ classes, data = df_all_columns_selected_class, sum)
    summarize_df_classes[,2] <- log10(summarize_df_classes[,2])
    
    
    
    # selected class
    # grep_class_patter <- paste0("^",vals$selected_pie_char_class,"$")
    
    pie_chart_df_selected <- pie_chart_df[selected_class]
    
    print("==============")
    print("pie_chart_df_selected")
    print(str(pie_chart_df_selected))
    
    if(input$NAs_show==FALSE){
      if ("N/A" %in% pie_chart_df_selected[,1]){
        pie_chart_df_selected <- pie_chart_df_selected[pie_chart_df_selected[,1] != "N/A", ]
      }
    }
    
    
    formating_classes <- function(classes){
      frequency <- table(classes)
      prop <- prop.table(frequency)
      data_frame <- cbind(
        frequency,
        prop
      )
      data_frame <- as.data.frame(data_frame)
      data_frame$class <- rownames(data_frame)
      data_frame <- data_frame[order(prop,decreasing = T),]
      rownames(data_frame) <- NULL
      
      return(data_frame)
    }
    
    pie_chart_df_selected_toplot <- formating_classes(pie_chart_df_selected)
    
    
    summarize_df_classes <- summarize_df_classes[order(summarize_df_classes[,1]),]
    
    pie_chart_df_selected_to_DT <- pie_chart_df_selected_toplot
    colnames(summarize_df_classes)[1] <- "class"
    
    cat("\n\nTEST\n")
    print(str(pie_chart_df_selected_to_DT))  
    print(str(summarize_df_classes))
    
    cat("TEST\n\n\n")
    
    pie_chart_df_selected_to_DT <- merge(pie_chart_df_selected_to_DT,summarize_df_classes,by="class")
    print("STR AFTER MERGE")
    print(str(pie_chart_df_selected_to_DT))
    cat("\n\n\n")
    
    pie_chart_df_selected_to_DT <- pie_chart_df_selected_to_DT[order(pie_chart_df_selected_to_DT[,3],decreasing = T),]
    
    
    pie_chart_df_selected_toplot <- pie_chart_df_selected_toplot[pie_chart_df_selected_toplot$prop > percent_filter,]#filter(pie_chart_df_selected_toplot,prop > percent_filter)
    
    
    
    
    tables$pie_chart_df_selected_to_DT <- pie_chart_df_selected_to_DT
    vals$total_metabolites_sum <- sum(tables$pie_chart_df_selected_to_DT$frequency)
    
    # colors
    
    
    palette_selected <- input$palette_selected
    colourCount = length(unique(pie_chart_df_selected_toplot$class))
    getPalette = colorRampPalette(brewer.pal(9,palette_selected ))
    palette_test <- getPalette(colourCount)
    
    
    library(ggplot2)
    chart_plot <- function(data_frame) {
      pie_plot <- ggplot(data_frame, aes(x = "", y = frequency, fill = class)) +
        geom_col() +
        geom_text(aes(label = round(prop*100,0)),
                  position = position_stack(vjust = 0.5),
                  size=5) +
        coord_polar(theta = "y") +
        scale_fill_manual(values = palette_test)+
        theme_void()+
        theme(
          # axis.ticks = element_blank(),
          #       axis.text = element_blank(),
          #       axis.title = element_blank(),
          legend.position = "right", 
          legend.text=element_text(size=15),
          panel.background = element_rect(fill = "white"))
    }
    
    
    pie_chart <- chart_plot(pie_chart_df_selected_toplot)
    
    # print("str(pie_chart_df_selected_toplot)")
    # print(str(pie_chart_df_selected_toplot))
    
    g_legend<-function(a.gplot){
      tmp <- ggplot_gtable(ggplot_build(a.gplot))
      leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
      legend <- tmp$grobs[[leg]]
      return(legend)}
    
    pie_chart_legend<- as_ggplot(g_legend(pie_chart))
    
    
    
    plots$pie_chart <- pie_chart + theme(legend.position = "none")
    plots$pie_chart_legend <- pie_chart_legend
    
    
  })
  
  
  
  output$pie_chart <- renderPlot({ 
    plots$pie_chart 
  })
  output$pie_chart_legend <- renderPlot(
    {plots$pie_chart_legend },
    height = "auto"
  )
  
  output$total_metabolites_sum <- renderUI({
    HTML(paste0("Total metabolites: <b>",vals$total_metabolites_sum),"</b>")
  })
  
  output$pie_chart_df_selected_to_DT <- renderDataTable({
    req(data())
    datatable(
      tables$pie_chart_df_selected_to_DT,
      escape=FALSE,
      rownames=FALSE,
      selection = "single",
      colnames=c(colnames(tables$pie_chart_df_selected_to_DT)[1:3],"log10_ph"),
      options = list(
        autoWidth = TRUE,
        scrollX = TRUE,
        dom = '',
        pageLength = 100
        # order = list(1, 'desc')
      )
    ) %>% formatStyle('frequency',
                      background = styleColorBar(select(tables$pie_chart_df_selected_to_DT,frequency), '#87CEFA',angle=-90),
                      backgroundSize = '98% 88%',
                      backgroundRepeat = 'no-repeat',
                      backgroundPosition = 'center') %>%  
      formatSignif(columns = 'prop', digits = 2)%>% 
      formatSignif(columns = 'log10_peak_height', digits = 2)%>% 
      formatStyle('log10_peak_height',
                  background = styleColorBar(select(tables$pie_chart_df_selected_to_DT,log10_peak_height), '#b382d1',angle=-90),
                  backgroundSize = '98% 88%',
                  backgroundRepeat = 'no-repeat',
                  backgroundPosition = 'center')
    
    
  })
  
  
  observeEvent(input$pie_chart_df_selected_to_DT_rows_selected,
               ignoreNULL = TRUE,
               {
                 showModal(
                   modalDialog(
                     # title = if (!is.null(vals$selected_class_from_table)) vals$selected_class_from_table else "Default Title",
                     # title= vals$selected_class_from_table,
                     {
                       renderDataTable({
                         req(data())
                         datatable(
                           caption=vals$selected_class_from_table,
                           tables$metabolite_list_selected ,
                           #tables$table_underexpressed,
                           #data_underexpressed(),
                           escape=FALSE,
                           rownames = FALSE,
                           options = list(dom = 'frtip', pageLength = 100)
                         )%>% formatStyle('freq',
                                          background = styleColorBar(select(tables$metabolite_list_selected,freq), '#87CE43',angle=-90),
                                          backgroundSize = '98% 88%',
                                          backgroundRepeat = 'no-repeat',
                                          backgroundPosition = 'center') %>%
                           formatSignif(columns = c('log10_peak_height'), digits = 5)%>% 
                           formatStyle('log10_peak_height',
                                       background = styleColorBar(select(tables$metabolite_list_selected,log10_peak_height), '#b382d1',angle=-90),
                                       backgroundSize = '98% 88%',
                                       backgroundRepeat = 'no-repeat',
                                       backgroundPosition = 'center')
                       }
                       )
                     },
                     header=tagList(
                       modalButton('Close')
                     ),
                     footer=tagList(
                       #downloadButton(outputId = "dwnld_data", "Download Data"),
                       modalButton('Close')),
                     
                     size = "l",
                     easyClose = TRUE,
                     fade = FALSE
                   )
                 )
               })
  
  ### PIE CHART
  
  ###< Metabolomics
  
  
}




shinyApp(ui, server)









