library(shiny)

fluidPage(
  titlePanel("Compare Spectra from MGF files", windowTitle = "SpectraSimilarity"), 
  p("Upload two MGF files and compare the spectra by means of the ", 
  a("OrgMassSpecR::SpectrumSimilarity()", href = "http://orgmassspec.github.io/", 
  target = "_blank"), "function. Each MGF file shall contain a single spectrum."), 
  p("Tolerance is used to align the m/z values of the two spectra. ",
  "Baseline threshold is used for peak identification - percent of max intensity."), 
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "MGF file Top", accept = c("text/plain", ".mgf", ".MGF")), 
      fileInput("file2", "MGF file Bottom", accept = c("text/plain", ".mgf", ".MGF")), 
      tags$hr(), 
      textInput("plot_title", "Title"), 
      checkboxInput("auto_range", "Auto-m/z-Range", value = TRUE), 
      numericInput("mz_lolim", "Min. m/z", value = 600, min = 0, step = 1), 
      numericInput("mz_hilim", "Max. m/z", value = 1800, min = 0, step = 1), 
      tags$hr(), 
      numericInput("SS_t", "Tolerance / Da", value = 0.25, 
	               min = 0.01, max = 2, step = 0.01), 
      numericInput("SS_b", "Baseline threshold / %", value = 10, 
	               min = 0, max = 100, step = 1), 
      tags$hr(), 
      downloadButton('downloadPlot', 'Download Plot as PDF'), 
    width = 3),
    mainPanel(
      plotOutput("SimPlot", width = "800px", height = "600px")#, 
      #textOutput("SimScore")
    )
  )
)

