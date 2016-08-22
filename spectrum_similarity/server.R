library(shiny)

# count the occurence of a single character in a string
# from https://techoverflow.net/blog/2012/11/10/r-count-occurrences-of-character-in-string/
count_char_occurrences <- function(char, string) {
  string2 <- gsub(char, "", string)
  return(nchar(string) - nchar(string2))
}

# count the tab characters in a string
count_tab_char <- function(string) {
  return(count_char_occurrences("\t", string))
}

round_up <- function(x, to = 10) {
  to * (x %/% to + as.logical(x %% to))
}

round_down <- function(x, to = 10) {
  to * (x %/% to)
}

# add text to the upper right hand corner of a plot
corner_text <- function(string, location = "topright"){
  legend(location, legend = string, bty = "n", pch = NA, inset = c(0.06, -0.02)) 
}

# read an MGF file and return the mz and intensity values in a data.frame
# works only with MGF files containing single spectra
# find all lines that start with a digit
# determine the number of columns
# read tab delimited values
mgf2df <- function(filename) {
  stopifnot(file.exists(filename))
  rawlines  <- readLines(filename)
  datlines  <- rawlines[grepl("^[0-9]", rawlines)]
  N_columns <- max(sapply(datlines, count_tab_char)) + 1
  nulls     <- rep("NULL", times = N_columns - 2)
  datstring <- paste(datlines, collapse = "\n")
  spectrum  <- read.delim(text = datstring, header = FALSE, 
                          colClasses = c("numeric", "integer", nulls))
  names(spectrum) <- c("mz", "intensity")
  return(spectrum)
}

# create head-to-tail plot and calculate spectrum similarity score
# the main function used in the R/shiny app
# output is a two element list with the plot and the score value
compare_spectra <- function(file1, name1, file2, name2, plot_title = "", 
                            mz_lolim = 600, mz_hilim = 800, 
                            tolerance = 0.25, baseline_threshold = 10) {
  
  # specify filenames
  # the actual files get random names from shiny
  file_A <- file1
  file_B <- file2
  # the names of the files uploaded by the user, remove file extension
  filename_A <- substring(name1, 1, nchar(name1) - 4)
  filename_B <- substring(name2, 1, nchar(name2) - 4)
  
  # specify parameters
  mz_range           <- c(mz_lolim, mz_hilim)
  #tolerance          <- 0.25  # used to align the m/z values of the two spectra
  #baseline_threshold <- 10    # used for peak identification, percent of max intensity
  
  # read files
  spec_A <- mgf2df(file_A)
  spec_B <- mgf2df(file_B)
  
  # compare spectra
  similarity_score <- OrgMassSpecR::SpectrumSimilarity(spec_A, spec_B, 
                                                       top.lab = filename_A, 
													   bottom.lab = filename_B, 
                                                       xlim = mz_range, 
                                                       t = tolerance, 
													   b = baseline_threshold)
  title(plot_title)
  corner_text(paste0("Mass Spectral Similarity Score: ", round(similarity_score, digits = 4)))
  theplot <- recordPlot()
  #graphics.off()
  
  #cat(paste0("\nThe mass spectral similarity score for \n\n  ", filename_A, "\n   VS \n  ", 
  #           filename_B, "\n\nis ", round(similarity_score, digits = 4), "\n\n"))
  
  return(list(plt = theplot, score = similarity_score))
}

server <- function(input, output, session) {
  RESULT <- NULL
  RESULT <- reactive({
    validate(
      need(input$mz_lolim < input$mz_hilim, 'Be careful with the m/z range!'),
      need(input$SS_t > 0, 'Be careful with the Tolerance! (> 0)'), 
      need(input$SS_b >= 0, 'Be careful with the Baseline threshold! (1-100)'), 
      need(input$SS_b <= 100, 'Be careful with the Baseline threshold! (1-100)')
    )
    file1 <- input$file1
    file2 <- input$file2
    if (is.null(file1)) return(NULL)
    if (is.null(file2)) return(NULL)

    if (input$auto_range) {
      spec1 <- mgf2df(file1$datapath)
      spec2 <- mgf2df(file2$datapath)
      all_mz <- c(spec1$mz, spec2$mz)
      mz_lolim <- round_down(min(all_mz))
      mz_hilim <- round_up(max(all_mz))
      updateNumericInput(session, "mz_lolim", value = mz_lolim)
      updateNumericInput(session, "mz_hilim", value = mz_hilim)
    }

    DAT <- compare_spectra(file1$datapath, 
                           file1$name, 
                           file2$datapath, 
                           file2$name, 
                           input$plot_title, 
                           input$mz_lolim, 
                           input$mz_hilim, 
                           input$SS_t, 
                           input$SS_b)
    DAT
  })
  output$SimPlot <- renderPlot({
    if (is.null(RESULT())) return(NULL)
    replayPlot(RESULT()$plt)
  })
  # if the renderText is active the Plot disappears from time to time after updating any of the reactive values
  # workaround is putting the score inside the plot (see corner_text function)
#   output$SimScore <- renderText({
#     if (is.null(RESULT())) return(NULL)
#     paste0("Mass Spectral Similarity Score: ", round(isolate(RESULT()$score), digits = 4))
#   })

  output$downloadPlot <- downloadHandler(
    filename = function() {
      if (input$plot_title != "") {
        paste0(input$plot_title, "_score", 
               round(RESULT()$score, digits = 4), "_", 
               format(Sys.time(), "_%Y%m%d_%H%M%S"), ".pdf")
      } else {
        paste0("OrgMassSpecR_score", 
               round(RESULT()$score, digits = 4), "_", 
               format(Sys.time(), "%Y%m%d_%H%M%S"), ".pdf")
      }
   },
    content = function(file) {
      if (is.null(RESULT())) {
        return(NULL)
      } else {
        # doesnt't seem to work as intended
        #tp <- RESULT()$plt
		#pdf(file = file, height = 8.4, width = 10.8, onefile = TRUE)
        #replayPlot(tp) # this works only sometimes, seems it's too slow
		#dev.off()
		
		# re-running the main script here, works so far, but DRY!
        file1 <- input$file1
        file2 <- input$file2
        if (is.null(file1)) return(NULL)
        if (is.null(file2)) return(NULL)

        pdf(file = file, height = 8.4, width = 10.8, onefile = TRUE)
        compare_spectra(file1$datapath, 
                        file1$name, 
                        file2$datapath, 
                        file2$name, 
                        input$plot_title, 
                        input$mz_lolim, 
                        input$mz_hilim, 
                        input$SS_t, 
                        input$SS_b)
        dev.off()
      }
    }
  )
}

