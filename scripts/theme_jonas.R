theme_jonas <- function() {
  
  theme_light() %+replace%   
    theme(
      #get rid of grid borders
      panel.border = element_blank(),
      # add white space top, right, bottom, left
      plot.margin = unit(c(1, 1, 1, 1), "cm"), 
      # custom axis title/text/lines
      axis.title = element_text(            
        size = 14),               
      axis.text = element_text(              
        size = 12),   
      # margin pulls text away from axis
      axis.text.x = element_text(           
        margin=margin(5, b = 10)),
      # black lines
      axis.line = element_line(colour = "black", size = rel(1)), 
      # custom plot titles, subtitles, captions
      plot.title = element_text(             
        size = 18, 
        vjust = 4),
      plot.subtitle = element_text(          
        size = 14,
        vjust = 3),
      plot.caption = element_text(           
        size = 10,
        hjust = 0, 
        vjust = 3), 
      # custom legend 
      legend.title = element_text(          
        size = 12), 
      legend.text = element_text(          
        size = 10), 
      #no background on legend
      legend.key = element_blank())
}
