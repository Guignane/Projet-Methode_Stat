

# Main file for tp2

f_install_load_lib = function(x) {
  # Function to load library x. It checks if library x is present in R.
  # If it is it loads it, otherwise it installs it then loads it.
  
  # input:
  # string x: vector of strings containing the library to be loaded
  
  if (!require(x, character.only = TRUE)) {
    # Check if library x is installed
    
    install.packages(x, repos = "http://cran.us.r-project.org")
  }
  # load required package
  require(x, character.only = TRUE)
}

lib_vec = c("here", "rmarkdown", "bookdown")
invisible(lapply(lib_vec, f_install_load_lib))

render(
  input = here("Code", "Analysis", "risk_analysis.Rmd") ,
  output_format = "html_document2",
  output_dir = here("Output", "Output")
)
