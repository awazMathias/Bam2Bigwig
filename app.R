library(shiny)
library(shinydashboard)
library(parallel)

options(shiny.errors = TRUE)
ui <- dashboardPage(
    
    dashboardHeader(title = "BAM to BigWig"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Options (default selected)", tabName = "option", icon = icon("cog", lib = "glyphicon")),
            menuItem("Bam to Bigwig", tabName = "BAM_bigwig", icon = icon("random", lib = "glyphicon"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "BAM_bigwig",
                    sidebarLayout(
                        sidebarPanel(
                            tabItem(tabName = "BAM_bigwig",
                                    fileInput("BAMFile",label = "1 - Upload the BAM file you want to treat",
                                              buttonLabel = "Browse...",
                                              placeholder = "No file selected")
                            ),
                            
                            tabItem(tabName = "BAM_bigwig",
                                    textInput("fileout", label = "2 - Enter the name of the bigwig file if you want an different name than your BAM file.", 
                                              value = "")
                            ),
                            
                            tabItem(tabName = "BAM_bigwig",
                                    actionButton("do", "3 - Click for convert")
                            ),
                            
                            tabItem(tabName = "BAM_bigwig",
                                    downloadButton("dlbigwig", label = "4 - Download Bigwig")
                            ),
                        ),
                        mainPanel(
                            imageOutput("pics1")
                        ))),
            tabItem( tabName = "option",
                     uiOutput("tab"),
                     sidebarLayout(
                         sidebarPanel(
                             tabItem(tabName = "option",
                                     radioButtons("norm", label = "1 - If you want to normalize, select one, else select None",
                                                  choices = list("None" = "", 
                                                                 "RPKM" = "--normalizeUsing RPKM",
                                                                 "CPM" = "--normalizeUsing CPM",
                                                                 "BPM" = "--normalizeUsing BPM",
                                                                 "RCPG" = "--normalizeUsing RCPG"),selected = "")
                             ),
                             
                             tabItem(tabName = "option",
                                     radioButtons("exsclaling", label = "2 - If you want to use the exact scaling option select YES",
                                                  choices = list("NO" = "", 
                                                                 "YES" = "--exactScaling"),selected = "")
                             ),
                             tabItem(tabName = "option",
                                     numericInput("binsize", label = "3 - Give the binsize you want (50 is the default value)", 
                                                  value = 50)
                             ),
                             tabItem(tabName = "option",
                                     textInput("region", label = "4 - IF YOU NEED : enter the region of the genome where you want to limit the operation, for example chr10 or chr10:456700:891000 for specifics regions. Else, let empty", 
                                               value = "")
                             ),
                         ),
                         mainPanel(
                             imageOutput("pics2")
                         ))))
    )           
)

server <- function(input, output, session) { 
    options(shiny.maxRequestSize = 1000000*1024^2) ## Augmenter la taille max possible des fichiers à charger, ici 1T
    
    url <- a("bamCoverage", href="https://deeptools.readthedocs.io/en/develop/content/tools/bamCoverage.html")
    output$tab <- renderUI({
        tagList("Click on URL link for help:", url)
    })
    
    output$pics1 <- renderImage({
        return(list(
            src = "LOGO-bigA-3101.png", 
            height = 190, 
            width = 400,
            filetype = "image/png"))
    }, deleteFile = FALSE)
    
    output$pics2 <- renderImage({
        return(list(
            src = "LOGO-bigA-3101.png", 
            height = 190, 
            width = 400,
            filetype = "image/png"))
    }, deleteFile = FALSE)
    
    observeEvent(input$do, {
        directory_path = input$BAMFile$datapath
        directory_path = gsub("/0.bam", "", directory_path)
        if (input$region == "" | is.null(input$region)){
            region = ""
        }else{
            region = paste("-r", input$region)
        }
        ncores = detectCores() - 3
        withProgress(message = 'Wait, indexation in progress', value = 0, {
            system(paste("samtools index", input$BAMFile$datapath))
            n <- 3
            for (i in 1:n) {
                incProgress(1/n)
                Sys.sleep(1)
            }
        })
        if (input$fileout == "" | is.null(input$fileout)){
            withProgress(message = 'Wait, convertion Bam to Bigwig in progress', value = 0, {
                n <- 3
                system(paste("bamCoverage -bs", input$binsize, "-p", ncores, region, input$norm, input$exsclaling,"-b", input$BAMFile$datapath, "-of bigwig -o", paste(directory_path,"/",input$BAMFile$name,".bw", sep="")))
                for (i in 1:n) {
                    incProgress(1/n)
                    Sys.sleep(1)
                }})}else{
                    withProgress(message = 'Wait, Convertion Bam to Bigwig in progress', value = 0, {
                        n <- 3
                        system(paste("bamCoverage -bs", input$binsize, "-p", ncores, region, input$norm, input$exsclaling,"-b", input$BAMFile$datapath, "-of bigwig -o", paste(directory_path,"/",input$fileout,".bw", sep="")))
                        for (i in 1:n) {
                            incProgress(1/n)
                            Sys.sleep(1)
                        }})
                }
    })
    
    output$dlbigwig <- downloadHandler(
        filename = function() {
            if (input$fileout == "" | is.null(input$fileout)){
                paste(input$BAMFile$name,"bw", sep=".")
              }else{
                paste(input$fileout, "bw", sep=".")}
        },
        content = function(file){
          directory_path = input$BAMFile$datapath
          directory_path = gsub("/0.bam", "", directory_path)
            if (input$fileout == "" | is.null(input$fileout)){
                bigwig = paste(input$BAMFile$name,".bw", sep = "")
                file.copy(paste(directory_path,"/", bigwig, sep= ""), file)
            }else{
                bigwig = paste(input$fileout,".bw", sep = "")
                file.copy(paste(directory_path, "/", bigwig, sep= ""), file)
            }
        }
    )
}

shinyApp(ui, server)