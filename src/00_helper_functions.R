
# PACKAGES ----------------------------------------------------------------

library(pacman)
pacman::p_load(showtext,ggplot2,stringr)


# Corporate Identity ------------------------------------------------------

#colors
color_palette = c("#000000","#3c3c3b","#9d9d9c","#ededed")

#setup correct graphics visualization
showtext_auto()
showtext_opts(dpi=600)

#adding custom fonts
font_add(family = "ibm",
         regular = "resources/fonts/IBM/IBMPlexSans-Regular.ttf",
         bold="resources/fonts/IBM/IBMPlexSans-SemiBold.ttf")

#plot template theme
plot_template_theme = theme_classic() +
  theme(axis.line = element_line(linewidth=0.2),
        text=element_text(family = "ibm",color=color_palette[2]),
        axis.text.y=element_text(size=6),
        axis.text.x=element_text(size=6),
        axis.ticks.x = element_blank(),
        axis.title = element_text(face="bold",size=8),
        plot.title = element_text(face="bold",size=11,hjust = 0.5),
        plot.subtitle = element_text(size=11,hjust = 0.5))

# HELPER FUNCTIONS --------------------------------------------------------

#convert a numeric to a percentage. By default, this percentage is printed as String with an optional pre-text.
#This function is for easy percentage calculation and display as text.
as_percentage = function(decimal_number , digits=1 , as_String=TRUE, pre_text="") {
  percent = round(decimal_number,2+digits)*100
  if(as_String==T) {
    return(paste(pre_text,percent,"%",sep=""))
  } else {
    return(percent)
  }
  
}

#split a multiple choice variable (vector) into separate TRUE/FALSE-Dummy-variables, given a separator
split_multiple_choice_variable = function(vec , sep=";",prefix="var.",suffix=".dummy") {
  
  unique_values = stringr::str_flatten(unique(vec),collapse =sep)
  unique_values = strsplit(unique_values,split = sep)
  unique_values = unique(unique_values[[1]])
  
  unique_values = stringr::str_trim(unique_values)
  unique_values = unique_values[which(unique_values!="")]
  
  df = data.frame(matrix(nrow=length(vec),ncol=0))
  for(i in unique_values) {
    temp = str_detect(vec,pattern = i)
    col_name = paste(prefix,i,suffix)
    df[[col_name]] = temp
    
  }
  return(df)
  
}




















