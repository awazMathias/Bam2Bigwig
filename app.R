## Author : Peries Mathias
## Date : Jully 2021

library(shiny)
library(shinydashboard)
library(rtracklayer)

ui <- dashboardPage(
  dashboardHeader(title = "BAM to BigWig"),
  ## Creations of the two pages: stranded and unstranded
  dashboardSidebar(sidebarMenu(
                      menuItem("Unstranded", tabName = "unstranded"),
                      menuItem("Stranded", tabName = "stranded")
                    )
  ),
  dashboardBody(
    tabItems(
      uiOutput("tab"),
      ################
      #### Page 1 ####
      ################
      
      tabItem(tabName = "unstranded",
              fluidRow(
                
                ## Column for improve display
                column(width = 12,
                       tabItem(tabName = "space1",
                               h2("                                                                                          ")
                       )
                ),
                
                ## Radiobutton for choose the possible Normalisation an numericInput for give a specific Normalisation factor
                box(width = 2,
                       tabItem(tabName = "normalize",
                               radioButtons("norm", label = "1 - If you want to normalize, select one, else select None",
                                            choices = list("None" = "", 
                                                           "RPKM" = "--normalizeUsing RPKM",
                                                           "CPM" = "--normalizeUsing CPM",
                                                           "BPM" = "--normalizeUsing BPM",
                                                           "RCPG" = "--normalizeUsing RCPG"),selected = ""),
                               numericInput("factor", label = "Else, if you want to use a specific Normalisation factor, put it there, else let 0.", 
                                            value = 0)
                               
                       )),
                
                ## Text input for give the possibility to exclude chromosomes or chromosomes regions. 
                box(width = 2,
                       tabItem(tabName = "ignore_normalization",
                               textInput("ignorenorm", label = "2 - For excluded chromosomes for the computing normalisation, whrite them here separate by spaces. (Example : chr1 chr2, exclude the chromosome 1 and 2)", 
                                         value = "")
                       )),
                
                ## radiobutton for use or no the exactscaling option of Bamcoverage. 
                box(width = 2,    
                       tabItem(tabName = "exact_scaling",
                               radioButtons("exsclaling", label = "3- If you want to use the exact scaling option select YES",
                                            choices = list("NO" = "", 
                                                           "YES" = "--exactScaling"),selected = "")
                       )),
                
                ## Tabitem for give the possibility to change the binsize
                box(width = 2,
                       tabItem(tabName = "bin_size",
                               numericInput("binsize", label = "4 - Give the binsize you want (50 is the default value)", 
                                            value = 50)
                       )),
                
                ## Textinput for give the possibility to restrict the creation of the Bigwig file to a specific chromosome or chromosome region. 
                box(width = 2,
                       tabItem(tabName = "specific_region",
                               textInput("region", label = "5 - if you need, enter the region of the genome where you want to limit the operation, for example chr10 or chr10:456700:891000 for specifics regions. Else, let empty", 
                                         value = "")
                       )),
                
                ## Give the possibility to skip the non covered regions.
                box(width = 2,
                       tabItem(tabName = "skip",
                               radioButtons("nonCovered", label = "6 - If you want to use the 'skip non covered regions' option select YES, for skip in the BAMfile the regions without overlapping reads. ",
                                            choices = list("NO" = "", 
                                                           "YES" = "--skipNonCoveredRegions"),selected = "")
                       )),
                
                column(width = 12,
                       tabItem(tabName = "--1",
                               h2("_________________________________________________________________________________________________________________________")
                       )
                ),
                column(width = 12,
                       tabItem(tabName = "space2",
                               h2("                                                                                          ")
                       )
                ),
                column(width = 12,
                       tabItem(tabName = "space3",
                               h2("                                                                                          ")
                       )
                )),
              
              sidebarLayout(
                sidebarPanel(
                  ## Uploader of the file 
                  tabItem(tabName = "BAM_bigwig",
                          fileInput("BAMFile",label = "7 - Upload the BAM file you want to treat",
                                    buttonLabel = "Browse...",
                                    placeholder = "No file selected")
                  ),
                  
                  ## Give the possibility to enter a specific name of the output Bigwig file.
                  tabItem(tabName = "BAM_bigwig",
                          textInput("fileout", label = "8 - Enter the name of the bigwig file if you want an different name than your BAM file.", 
                                    value = "")
                  ),
                  
                  tabItem(tabName = "space4",
                          h1("                                                                                          ")
                  ),
                  
                  ## Action button for make the conversion Bam to bigwig
                  tabItem(tabName = "BAM_bigwig",
                          actionButton("do", "9 - Click for convert")
                  ),
                  
                  tabItem(tabName = "space5",
                          h1("                                                                                          ")
                  ),
                  
                  tabItem(tabName = "space6",
                          h1("                                                                                          ")
                  ),
                  
                  ## Downloader of the Bigwig file. 
                  tabItem(tabName = "BAM_bigwig",
                          downloadButton("dlbigwig", label = "10 - Download Bigwig")
                  )
                ),
                mainPanel(
                  imageOutput("pics2")
                )
                )),
    
      
      ################
      #### Page 2 ####
      ################
      
    tabItem(tabName = "stranded",
            fluidRow(
              column(width = 12,
                     tabItem(tabName = "space1",
                             h2("                                                                                          ")
                     )
              ),
              
              ## radiobutton for choize the stranded method
              column(width =5.9,
                     align="center",
                  tabItem(tabName = "strand_choice",
                          radioButtons("choice_strand", label = "0 - Choose the stranded methods / Library type you want to use",
                                       choices = list("yes / fr-secondstrand" = "second", 
                                                      "reverse / fr-firststrand" = "first"),selected = "second")
                  )),
              
              column(width = 12,
                     tabItem(tabName = "spacex",
                             h2("                                                                                          ")
                     )
              ),
              
              box(width = 2,
                  tabItem(tabName = "normalize",
                          radioButtons("norm2", label = "1 - If you want to normalize, select one, else select None",
                                       choices = list("None" = "", 
                                                      "RPKM" = "--normalizeUsing RPKM",
                                                      "CPM" = "--normalizeUsing CPM",
                                                      "BPM" = "--normalizeUsing BPM",
                                                      "RCPG" = "--normalizeUsing RCPG"),selected = ""),
                          numericInput("factor", label = "Else, if you want to use a specific Normalisation factor, put it there, else let 0.", 
                                       value = 0)
                          
                  )),
              
              box(width = 2,
                  tabItem(tabName = "ignore_normalization",
                          textInput("ignorenorm2", label = "2 - For excluded chromosomes for the computing normalisation, whrite them here separate by spaces. (Example : chr1 chr2, exclude the chromosome 1 and 2)", 
                                    value = "")
                  )),
              box(width = 2,    
                  tabItem(tabName = "exact_scaling",
                          radioButtons("exsclaling2", label = "3- If you want to use the exact scaling option select YES",
                                       choices = list("NO" = "", 
                                                      "YES" = "--exactScaling"),selected = "")
                  )),
              box(width = 2,
                  tabItem(tabName = "bin_size",
                          numericInput("binsize2", label = "4 - Give the binsize you want (50 is the default value)", 
                                       value = 50)
                  )),
              box(width = 2,
                  tabItem(tabName = "specific_region",
                          textInput("region2", label = "5 - IF YOU NEED : enter the region of the genome where you want to limit the operation, for example chr10 or chr10:456700:891000 for specifics regions. Else, let empty", 
                                    value = "")
                  )),
              box(width = 2,
                  tabItem(tabName = "skip",
                          radioButtons("nonCovered2", label = "6 - If you want to use the 'skip non covered regions' option select YES, for skip in the BAMfile the regions without overlapping reads. ",
                                       choices = list("NO" = "", 
                                                      "YES" = "--skipNonCoveredRegions"),selected = "")
                  )),
              
              column(width = 12,
                     tabItem(tabName = "--1",
                             h2("_________________________________________________________________________________________________________________________")
                     )
              ),
              column(width = 12,
                     tabItem(tabName = "space2",
                             h2("                                                                                          ")
                     )
              ),
              column(width = 12,
                     tabItem(tabName = "space3",
                             h2("                                                                                          ")
                     )
              )),
            sidebarLayout(
              sidebarPanel(
                tabItem(tabName = "BAM_bigwig",
                        fileInput("BAMFile2",label = "7 - Upload the BAM file you want to treat",
                                  buttonLabel = "Browse...",
                                  placeholder = "No file selected")
                ),
                
                tabItem(tabName = "BAM_bigwig",
                        textInput("fileout2", label = "8 - Enter the name of the bigwig file if you want an different name than your BAM file.", 
                                  value = "")
                ),
                
                tabItem(tabName = "space4",
                        h1("                                                                                          ")
                ),
                
                tabItem(tabName = "BAM_bigwig",
                        actionButton("do2", "9 - Click for convert")
                ),
                
                tabItem(tabName = "space5",
                        h1("                                                                                          ")
                ),
                
                tabItem(tabName = "space6",
                        h1("                                                                                          ")
                ),
                
                ## Two download button for download reverse and forward
                tabItem(tabName = "BAM_bigwig",
                        downloadButton("dlbigwig_reverse", label = "10 - Download Bigwig reverse"),
                        downloadButton("dlbigwig_forward", label = "11 - Download Bigwig forward")
                )
              ),
              mainPanel(
                imageOutput("pics2")
              )
              ))
        )
      )
    )

server <- function(input, output, session) { 
  options(shiny.maxRequestSize = 1000000*1024^2) ## Increase the possible max weight of the uploaded file (here 1T) 
  
  ## Creation, at the top of the pages, of an weblink to the Bamcoverage tutorial
  url <- a("bamCoverage", href="https://deeptools.readthedocs.io/en/develop/content/tools/bamCoverage.html")
  output$tab <- renderUI({
    tagList("Click on URL link for help:", url)
  })
  
     #####################################
     ####### UNSTRANDED CONVERSION #######
     #####################################
  
  observeEvent(input$do, {
    
    ## Creation of variables after the upload of the BAM file. ## 
    
    directory_path = input$BAMFile$datapath ## Definition of the filepathn where the file is stored
    directory_path = gsub("/0.bam", "", directory_path) ## Deletion of the filename give by shiny 
    
    # Save of the ignore region option 
    if (input$region == "" | is.null(input$region)){
      region = ""
    }else{
      region = paste("-r", input$region)
    }
    ## Save of the ignore normalisation option
    if (input$ignorenorm =="" | is.null(input$ignorenorm)){
      ignorenormalisation = ""
    }else{
      ignorenormalisation = paste("-ignore", input$ignorenorm)
    }
    ncores = detectCores() / 2 ## determine the half of thread of the computer that going to be used in the Bamcoverage function
    
    
    
    ## Loop "with progress" for show an waiting window 
    withProgress(message = 'Wait, indexation in progress', value = 0, {
      ## Samtool command for create the index of the Bam file.
      system2(command ="samtools", args = paste("index", input$BAMFile$datapath))
      n <- 3 ## Progress Bar in 3 steps
      ## This for loop just show when the samtools command finish, she's not an real progress bar in real time.
      for (i in 1:n) {
        incProgress(1/n)
        Sys.sleep(1) ## Progression of the bar 1 second per steps
      }
    })
    ## if/else for take in count if the user give, or not, a specific name at the Bigwig file.
    if (input$fileout == "" | is.null(input$fileout)){
      withProgress(message = 'Wait, convertion Bam to Bigwig in progress', value = 0, {
        n <- 3
        ## Call to the bamCoverage function and all possibles arguments (if they are empty, they are ignored)
        system2(command = "bamCoverage", args = (paste("-bs", input$binsize,"-p", ncores, ignorenormalisation, region, input$norm, input$exsclaling, input$nonCovered, "-b", input$BAMFile$datapath, "-of bigwig -o", paste(directory_path,"/",input$BAMFile$name,".bw", sep = ""))))
        for (i in 1:n) {
          incProgress(1/n)
          Sys.sleep(1)
        }})}else{
          withProgress(message = 'Wait, Convertion Bam to Bigwig in progress', value = 0, {
            n <- 3
            system2(command = "bamCoverage", args = (paste("-bs", input$binsize,"-p", ncores, ignorenormalisation, region, input$norm, input$exsclaling, input$nonCovered, "-b", input$BAMFile$datapath, "-of bigwig -o", paste(directory_path,"/",input$fileout,".bw", sep=""))))
            for (i in 1:n) {
              incProgress(1/n)
              Sys.sleep(1)
            }})
        }
  })
  
  ## Downloader of the bigwig file
  output$dlbigwig <- downloadHandler(
    ## Creation of the bigwig file name, he take the original name or the given one by the user. 
    filename = function() {
      if (input$fileout == "" | is.null(input$fileout)){
        paste(input$BAMFile$name,"bw", sep=".")
      }else{
        paste(input$fileout, "bw", sep=".")}
    },
    
    ## Case 1 : if the user use he's own normalizing factor.  ##
    
    content = function(file){
      directory_path = input$BAMFile$datapath
        if (input$factor != 0){
          bigwig = paste(input$BAMFile$name,".bw", sep = "")
          bigwig = paste(gsub("/0.bam", "", directory_path),"/", bigwig, sep= "") ## determine the path of the bigwig and give the bigwig name
          NBreads = system(paste("samtools view -c", directory_path), intern=TRUE) ## determine the number of reads
          bw_file = import.bw(bigwig, as = c("RleList")) ## creaction of the bigwig
          bw_file = bw_file * (input$factor / as.numeric(NBreads)) ## Application of the specific factor
          export.bw(bw_file, file) ## real export. 
        }else{
          
          ## Case 2: if the user dont give a specific normalization factor ##
          
      directory_path = gsub("/0.bam", "", directory_path) # determine the directory path
      if (input$fileout == "" | is.null(input$fileout)){ ## if the user dont give a specific file name
        bigwig = paste(input$BAMFile$name,".bw", sep = "") 
        file.copy(paste(directory_path,"/", bigwig, sep= ""), file)
      }else{
        bigwig = paste(input$fileout,".bw", sep = "")
        file.copy(paste(directory_path, "/", bigwig, sep= ""), file)
      }}
    }
  )
  
  ####################################
  ####### STRANDED CONVERSION ########
  ####################################
  
  observeEvent(input$do2, {
    directory_path = input$BAMFile2$datapath
    directory_path = gsub("/0.bam", "", directory_path)
    if (input$region2 == "" | is.null(input$region2)){
      region = ""
    }else{
      region = paste("-r", input$region2)
    }
    if (input$ignorenorm2 =="" | is.null(input$ignorenorm2)){
      ignorenormalisation = ""
    }else{
      ignorenormalisation = paste("-ignore", input$ignorenorm2)
    }
    ncores = detectCores() / 2
    withProgress(message = 'Wait, indexation in progress', value = 0, {
      system2(command ="samtools", args = paste("index", input$BAMFile2$datapath))
      n <- 3
      for (i in 1:n) {
        incProgress(1/n)
        Sys.sleep(1)
      }
    })
    if (input$choice_strand == "first"){
      ## creation of the sam flags for exclude regions without interest and create after, two bam file. 
      exclude_forward = "--samFlagExclude 80 --samFlagExclude 160"
      exclude_reverse = "--samFlagExclude 96 --samFlagExclude 144"
    if (input$fileout2 == "" | is.null(input$fileout2)){
      withProgress(message = 'Wait, convertion Bam to Bigwig in progress', value = 0, {
        n <- 3
        ## Double Bamcoverage command for create two file, one with only reverse and the other forward strand (determine by bamcoverage option and flags give before)
        system2(command = "bamCoverage", args = (paste("-bs", input$binsize2,"-p", ncores, ignorenormalisation, region, input$norm2, input$exsclaling2, input$nonCovered2, exclude_reverse,"-b", input$BAMFile2$datapath, "-of bigwig -o", paste(directory_path,"/",input$BAMFile2$name,"_reverse",".bw", sep = ""))))
        system2(command = "bamCoverage", args = (paste("-bs", input$binsize2,"-p", ncores, ignorenormalisation, region, input$norm2, input$exsclaling2, input$nonCovered2, exclude_forward,"-b", input$BAMFile2$datapath, "-of bigwig -o", paste(directory_path,"/",input$BAMFile2$name,"_forward",".bw", sep = ""))))
        for (i in 1:n) {
          incProgress(1/n)
          Sys.sleep(1)
        }})}else{
          withProgress(message = 'Wait, Convertion Bam to Bigwig in progress', value = 0, {
            n <- 3
            system2(command = "bamCoverage", args = (paste("-bs", input$binsize2,"-p", ncores, ignorenormalisation, region, input$norm2, input$exsclaling2, input$nonCovered2, exclude_reverse,"-b", input$BAMFile2$datapath, "-of bigwig -o", paste(directory_path,"/",input$fileout2,"_reverse",".bw", sep=""))))
            system2(command = "bamCoverage", args = (paste("-bs", input$binsize2,"-p", ncores, ignorenormalisation, region, input$norm2, input$exsclaling2, input$nonCovered2, exclude_forward,"-b", input$BAMFile2$datapath, "-of bigwig -o", paste(directory_path,"/",input$fileout2,"_forward",".bw", sep=""))))
            for (i in 1:n) {
              incProgress(1/n)
              Sys.sleep(1)
            }})
        }}
    if (input$choice_strand == "second"){
      exclude_reverse = "--samFlagExclude 80 --samFlagExclude 160"
      exclude_forward= "--samFlagExclude 96 --samFlagExclude 144"
      if (input$fileout2 == "" | is.null(input$fileout2)){
        withProgress(message = 'Wait, convertion Bam to Bigwig in progress', value = 0, {
          n <- 3
          system2(command = "bamCoverage", args = (paste("-bs", input$binsize2,"-p", ncores, ignorenormalisation, region, input$norm2, input$exsclaling2, input$nonCovered2, exclude_reverse,"-b", input$BAMFile2$datapath, "-of bigwig -o", paste(directory_path,"/",input$BAMFile2$name,"_reverse",".bw", sep = ""))))
          system2(command = "bamCoverage", args = (paste("-bs", input$binsize2,"-p", ncores, ignorenormalisation, region, input$norm2, input$exsclaling2, input$nonCovered2, exclude_forward,"-b", input$BAMFile2$datapath, "-of bigwig -o", paste(directory_path,"/",input$BAMFile2$name,"_forward",".bw", sep = ""))))
          for (i in 1:n) {
            incProgress(1/n)
            Sys.sleep(1)
          }})}else{
            withProgress(message = 'Wait, Convertion Bam to Bigwig in progress', value = 0, {
              n <- 3
              system2(command = "bamCoverage", args = (paste("-bs", input$binsize2,"-p", ncores, ignorenormalisation, region, input$norm2, input$exsclaling2, input$nonCovered2, exclude_reverse,"-b", input$BAMFile2$datapath, "-of bigwig -o", paste(directory_path,"/",input$fileout2,"_reverse",".bw", sep=""))))
              system2(command = "bamCoverage", args = (paste("-bs", input$binsize2,"-p", ncores, ignorenormalisation, region, input$norm2, input$exsclaling2, input$nonCovered2, exclude_forward,"-b", input$BAMFile2$datapath, "-of bigwig -o", paste(directory_path,"/",input$fileout2,"_forward",".bw", sep=""))))
              for (i in 1:n) {
                incProgress(1/n)
                Sys.sleep(1)
          }})
      }}
  })

  output$dlbigwig_reverse <- downloadHandler(
    filename = function() {
      if (input$fileout2 == "" | is.null(input$fileout2)){
        paste(input$BAMFile2$name,"_reverse",".bw", sep="")
      }else{
        paste(input$fileout2,"_reverse",".bw", sep="")}
    },
    content = function(file){
      directory_path = input$BAMFile2$datapath
      if (input$factor != 0){
        bigwig = paste(input$BAMFile2$name,"_reverse",".bw", sep = "")
        bigwig = paste(gsub("/0.bam", "", directory_path),"/", bigwig, sep= "")
        NBreads = system(paste("samtools view -c", paste(directory_path,"/",bigwig, sep =""), intern=TRUE))
        bw_file = import.bw(bigwig, as = c("RleList"))
        bw_file = bw_file * (input$factor2 / as.numeric(NBreads))
        export.bw(bw_file, file)
      }else{
        directory_path = gsub("/0.bam", "", directory_path)
        if (input$fileout2 == "" | is.null(input$fileout2)){
          bigwig = paste(input$BAMFile2$name,"_reverse",".bw", sep = "")
          file.copy(paste(directory_path,"/", bigwig, sep= ""), file)
        }else{
          bigwig = paste(input$fileout2,"_reverse",".bw", sep = "")
          file.copy(paste(directory_path, "/", bigwig, sep= ""), file)
        }}
    }
  )
  output$dlbigwig_forward <- downloadHandler(
    filename = function() {
      if (input$fileout2 == "" | is.null(input$fileout2)){
        paste(input$BAMFile2$name,"_forward",".bw", sep="")
      }else{
        paste(input$fileout2,"_forward",".bw", sep="")}
    },
    content = function(file){
      directory_path = input$BAMFile2$datapath
      if (input$factor != 0){
        bigwig = paste(input$BAMFile2$name,"_forward",".bw", sep = "")
        bigwig = paste(gsub("/0.bam", "", directory_path),"/", bigwig, sep= "")
        NBreads = system(paste("samtools view -c", paste(directory_path,"/",bigwig, sep =""), intern=TRUE))
        bw_file = import.bw(bigwig, as = c("RleList"))
        bw_file = bw_file * (input$factor2 / as.numeric(NBreads))
        export.bw(bw_file, file)
      }else{
        directory_path = gsub("/0.bam", "", directory_path)
        if (input$fileout2 == "" | is.null(input$fileout2)){
          bigwig = paste(input$BAMFile2$name,"_forward",".bw", sep = "")
          file.copy(paste(directory_path,"/", bigwig, sep= ""), file)
        }else{
          bigwig = paste(input$fileout2,"_forward",".bw", sep = "")
          file.copy(paste(directory_path, "/", bigwig, sep= ""), file)
        }}
    }
  )
}
shinyApp(ui, server)
