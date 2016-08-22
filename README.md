# spectrum_similarity
R/shiny interface for the SpectrumSimilarity function from the OrgMassSpecR package

A simple webinterface for the 
[OrgMassSpecR::SpectrumSimilarity()](https://orgmassspec.github.io/) function 
used to create head-to-tail plots of mass spectra. The input data needs to be in 
*.mgf* file format or a text file with tab delimited data columns. The mass 
value should be in the first and the intensity value in the second column. Any 
further columns are ignored by the included file reader function. The interface 
is built with [shiny](http://shiny.rstudio.com/). Use the *.vbs* file to start 
the webservice on a Windows machine.
