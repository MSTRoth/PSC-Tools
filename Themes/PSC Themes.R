#PSC based charts theme####

PSC_theme <- function() {
  theme(
  line = element_line(colour = "black", size = 0.5, 
                      linetype = 1, lineend = "butt"), 
  rect = element_rect(fill = "white", colour = "black",
                      size = 0.5, linetype = 1),
  text = element_text(family = "serif", face = "plain",
                      colour = "black", size = 11,
                      lineheight = 0.9,  hjust = 0.5,
                      vjust = 0.5, angle = 0,
                      margin = margin(), debug = FALSE),
  
  axis.line = element_blank(),
  axis.text = element_text(size = rel(1), colour = "grey30"),
  axis.text.x = element_text(margin = margin(t = 0.8*11/2),
                             vjust = 1),
  axis.text.y = element_text(margin = margin(r = 0.8*11/2),
                             hjust = 1),
  axis.ticks = element_line(colour = "grey60"),
  axis.ticks.length = unit(11/2, "pt"),
  axis.title.x = element_text(size = rel(1.2), margin = margin(t = 0.8 * 11,
                                                               b = 0.8 * 11/2)),
  axis.title.y = element_text(angle = 90,
                              margin = margin(r = 0.8 * 11,
                                              l = 0.8 * 11/2)),
  
  legend.background = element_rect(colour = NA),
  legend.margin = unit(0.2, "cm"),
  legend.key = element_rect(fill = "grey95", colour = "white"),
  legend.key.size = unit(1.2, "lines"),
  legend.key.height = NULL,
  legend.key.width = NULL,
  legend.text = element_text(size = rel(1.2), ),
  legend.text.align = NULL,
  legend.title = element_text(hjust = 0),
  legend.title.align = NULL,
  legend.position = "right",
  legend.direction = NULL,
  legend.justification = "center",
  legend.box = NULL,
  
  panel.background = element_rect(fill = "white", colour = "grey50"),
  panel.border = element_blank(),
  panel.grid.major.y = element_line(colour = "grey60"),
  panel.grid.major.x = element_line(colour = "grey87"),
  panel.grid.minor.y = element_line(colour = "grey72", size = 0.25),
  panel.grid.minor = element_blank(),
  panel.margin = unit(11, "pt"), panel.margin.x = NULL,
  panel.margin.y = NULL, panel.ontop = FALSE,
  
  strip.background = element_rect(fill = "grey85", colour = NA),
  strip.text = element_text(colour = "grey10", size = rel(1)),
  strip.text.x = element_text(margin = margin(t = 11,
                                              b = 11)),
  strip.text.y = element_text(angle = -90,
                              margin = margin(l = 11,
                                              r = 11)),
  strip.switch.pad.grid = unit(0.1, "cm"),
  strip.switch.pad.wrap = unit(0.1, "cm"),
  
  plot.background = element_rect(colour = "white")
  ,
  plot.title = element_text(size = rel(3),
                            margin = margin(b = 11 * 1.2), face = "bold"),
  plot.margin = margin(11, 11, 11, 11),
  complete = TRUE
)
}

#Market Briefing chart theme ####

PSC_theme <- function() {
  theme(
  )
}