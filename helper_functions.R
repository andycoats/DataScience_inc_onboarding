### Convenience function that takes the name of a package as argument
# Checks if it's installed and installs it if not
# Loads the package
# suppressMessages() prevents the messages that are output during installation and importing from appearing in stdout
usePackage = function(p) {
    if (!is.element(p, installed.packages()[,1])) {
        suppressMessages(install.packages(p, dep = TRUE))
    }
    suppressMessages(require(p, character.only = TRUE))
}
### Set up plotting parameters
set_ds_theme = function() {
  theme_update(panel.background = element_rect(fill = "white"
                                               , colour = "grey50")
               , plot.title = element_text(hjust = 0.5))
  
  # Specify a color palette for continuous data
  cont_ds_palette <<- c('#f7fbff', '#d7ecfd', '#3ba3f8', '#328bd5', '#266aa2', '#000000')
  cont_gradient <<- colorRampPalette(cont_ds_palette)
  # Specify a color palette for discrete/categorical data
  cat_ds_palette <<- c("#3ba3f8", "#afb6bd", "#3eb642", "#6981ef", "#b353b5", "#e66867")
}
