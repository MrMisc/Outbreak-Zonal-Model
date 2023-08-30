
#! /usr/bin/Rscript

f<-file("stdin")
open(f)
record<-c()
while(length(line<-readLines(f,n=1))>0){
  #write(line,stderr())
  record<-c(record,line)  
}


numbers<-record[1:length(record)-1]
numbers_<-c()


print(getwd())

# Plot heatmap
library(ggplot2)
library(pandoc)
library(plotly)

# Extract x, y coordinates
coordinates <- strsplit(record, " ")

# Extract x, y, and time values
x <- as.numeric(sapply(coordinates, "[[", 1))
y <- as.numeric(sapply(coordinates, "[[", 2))
time <- as.numeric(sapply(coordinates, "[[", 3))
zone<- as.numeric(sapply(coordinates, "[[", 4))


# Create a data frame for each unique zone value
zone_unique <- unique(zone)
data_frames <- list()

for (z in zone_unique) {
  data_frames[[as.character(z)]] <- data.frame(x = x[zone == z], y = y[zone == z])
}



data <- data.frame(x = x, y = y,zone = zone)
heatmap_plot <- ggplot(data, aes(x, y, frame =factor(zone))) +
  geom_bin2d(bins = 20) +  # Adjust bins as needed
  scale_fill_viridis_c() +  # You can choose other color scales too
  labs(title = "Heatmap of Coordinates")

# Convert ggplot to ggplotly
heatmap_interactive <- ggplotly(heatmap_plot)

# Save as HTML using pandoc
htmlwidgets::saveWidget(heatmap_interactive, "heatmap_output.html", selfcontained = TRUE)
print("Heatmap generated successfully!")




#Another try at 2d hist using geom_tile


# # Create a data frame for the entire data
# data_all <- data.frame(x = x, y = y, zone = zone)

# # Create the animated heatmap
# heatmap_plot <- ggplot(data_all, aes(x, y, frame = factor(zone))) +
#   geom_tile(aes(fill = ..density..), bins = 5) +
#   scale_fill_gradient(low = "#34FCEC", high = "#B102E9") +
#   labs(title = "2D Histogram of Occurrences")

# # Convert ggplot to ggplotly
# heatmap_interactive <- ggplotly(heatmap_plot)

# # Save as HTML using pandoc
# htmlwidgets::saveWidget(heatmap_interactive, "animated_2dhistogram.html", selfcontained = TRUE)



##Heatmap (2d histogram)
custom_colorscale <- c("#34FCEC", "#00D3FF", "#0099FF", "#B102E9")
hist2d <- plot_ly(data = data, x = ~x, y = ~y, type = "histogram2d",xbins = list(start = min(data$x), end = max(data$x), size = 10),
                  ybins = list(start = min(data$y), end = max(data$y), size = 10),
                  marker = list(colorscale = custom_colorscale)) %>%
  layout(title = "2D Histogram of Occurrences",
         xaxis = list(title = "X"),
         yaxis = list(title = "Y"),
         plot_bgcolor = '#FFF8EE',
         xaxis = list(
           zerolinecolor = '#ffff',
           zerolinewidth = 0.5,
           gridcolor = '#F4F2F0'),
         yaxis = list(
           zerolinecolor = '#ffff',
           zerolinewidth = 0.5,
           gridcolor = '#F4F2F0'))
# Save interactive plot as HTML using pandoc
htmlwidgets::saveWidget(hist2d, "Heatmap_2dHistogram.html", selfcontained = TRUE)









#Hexbin heatmap plot?

hexbin_plot <- plot_ly(x = x, y = y, type = "scatter", mode = "markers",
                       marker = list(symbol = "hexagon", size = 10,
                                     opacity = 0.7, line = list(width = 0)),
                       colors = "rgba(255,255,255,0.0)") %>%
  layout(title = "Hexbin Plot Example",
         xaxis = list(title = "X"),
         yaxis = list(title = "Y"))

# Save interactive plot as HTML using pandoc
htmlwidgets::saveWidget(hexbin_plot, "hexbin_plot.html", selfcontained = TRUE)




# data <- data.frame(x = x, y = y, time = time)

# surface_plot <- plot_ly(data, x = ~x, y = ~y, z = ~time,
#                         type = "surface", colorscale = "Viridis",
#                         colorbar = list(title = "Time"),
#                         showscale = TRUE) %>%
#   layout(title = "Surface Plot Example",
#          scene = list(
#            xaxis = list(title = "X"),
#            yaxis = list(title = "Y"),
#            zaxis = list(title = "Time"))
#   )

# # Save interactive plot as HTML using pandoc
# htmlwidgets::saveWidget(surface_plot, "surface_plot.html", selfcontained = TRUE)






library("ggplot2")
library("plotly")
library("breakDown")
library(ggdark)
library(pracma)
library(comprehenr)
library(ggridges)
library(tidyverse)
library(ggplot2)
library(plotly)
library(thematic)
library(extrafont)
library(pandoc)
#library(pandoc)
#Get dem custom fonts
# font_import()
# loadfonts(device = "win")
# actual_pars<-as.data.frame(actual_pars)

setwd("C:/Users/Victus/OneDrive/Desktop/Outbreak_Zonal")
data<-read.csv("output.csv",header = FALSE)
colnames(data) <- c(
  "HitPct1", "TotalSamples1", "HitSamples1",
  "HitPct2", "TotalSamples2", "HitSamples2","Zone"
)

#Round up values
data$HitSamples1<-ceiling(data$HitSamples1)
data$HitSamples2<-ceiling(data$HitSamples2)
# Scatter plot for the first 2 sets of data
# Define custom theme colors
thematic_on(bg = "#FCE9D7", fg = "orange", accent = "purple",font = "Yu Gothic")
# 



# Create a unique identifier for each time unit
no_of_zones<-length(unique(data$Zone))
data$TimeUnit <- rep(seq_len(nrow(data) / no_of_zones), each = no_of_zones)

# Farm

fig_dots <- data %>%
  plot_ly(type = "scatter",
          mode = "markers+lines", line = list(width = 0.35)) %>%
  add_trace(x = ~TimeUnit,
            y = ~HitPct1,
            frame = ~Zone,  # Use TimeUnit for animation frames
            color = "Host",
            colors = c("#2A6074", "#00C9B1"),
            size = ~TotalSamples1,
            customdata = ~{
              zone_data <- data[data$TimeUnit == TimeUnit, ]
              paste(zone_data$HitSamples1, "out of ", zone_data$TotalSamples1, " hosts")
            },
            hovertemplate = "%{y} % of motile hosts <br> are infected  <br> ie %{customdata}") %>%
  add_trace(
    x = ~TimeUnit,
    y = ~HitPct2,
    frame = ~Zone,  # Use TimeUnit for animation frames
    color = "Deposits",
    colors = c("#FFF184", "#FFDD80"),
    size = ~TotalSamples2,
    customdata = ~{
              zone_data <- data[data$TimeUnit == TimeUnit, ]
              paste(zone_data$HitSamples2, "out of ", zone_data$TotalSamples2, " hosts")
            },
    hovertemplate = "%{y} % of sessile deposits <br> are infected  <br> ie %{customdata}",
    line = list(width = 0.35)
  ) %>%
  layout(title = "Infection Trend within cultivation",
         plot_bgcolor = '#FFF8EE',
         xaxis = list(
           title = "Zone",
           zerolinecolor = '#ffff',
           zerolinewidth = 0.5,
           gridcolor = '#F4F2F0'),
         yaxis = list(
           title = "Percentage of Infected",
           zerolinecolor = '#ffff',
           zerolinewidth = 0.5,
           gridcolor = '#F4F2F0')) %>%
  animation_slider(
    currentvalue = list(font = list(color = "darkgreen"))
  ) %>%
  animation_opts(mode = "next",
                 easing = "exp-in", redraw = FALSE
  )

# Save the animation
htmlwidgets::saveWidget(fig_dots, "scatter.html", selfcontained = TRUE)



# #Collection

# fig_dots<-data%>%plot_ly(type="scatter",
#           mode = "markers+lines",line = list(width=0.35))%>%
#   add_trace(x = time,
#           y = ~HitPct3,
#           color ="Host",
#           colors=c("#2A6074","#00C9B1"),
#           size = ~TotalSamples3,
#           customdata = ~paste(HitSamples3, "out of ", TotalSamples3," hosts"),
#           hovertemplate="%{y} % of motile hosts <br> are infected  <br> ie %{customdata}")


# fig_dots<-fig_dots %>%
#   add_trace(
#     x = ~time,
#     y = ~HitPct4,
#     color = "Deposits",
#     colors = c("#FFF184", "#FFDD80"),  # Reversed color order
#     size = ~TotalSamples4,
#     customdata = ~paste(HitSamples4, "out of ", TotalSamples4," deposits"),
#     hovertemplate = "%{y} % of sessile deposits <br> are infected  <br> ie %{customdata}",
#     line = list(width = 0.35)
#   ) %>%
#   layout(title = "Infection Trend within collection",
#          plot_bgcolor = '#FFF8EE',
#          xaxis = list(
#           title = "Time (Hours)",
#            zerolinecolor = '#ffff',
#            zerolinewidth = 0.5,
#            gridcolor = '#F4F2F0'),
#          yaxis = list(
#           title = "Percentage of Infected",
#            zerolinecolor = '#ffff',
#            zerolinewidth = 0.5,
#            gridcolor = '#F4F2F0'))


# htmlwidgets::saveWidget(fig_dots, "scatter_plot_2.html", selfcontained = TRUE)




# #Overall

# # print(data$HitSamples1)
# # print(data$HitSamples3)
# # print(data$HitSamples1+data$HitSamples3)

# data$totalhits_motile<-data$HitSamples1+data$HitSamples3
# data$totalhits_sessile<-data$HitSamples2+data$HitSamples4

# data$Total_motile<-data$TotalSamples1+data$TotalSamples3
# data$Total_sessile<-data$TotalSamples2+data$TotalSamples4

# data$totalperc_motile<-data$totalhits_motile/data$Total_motile*100
# data$totalperc_sessile<-data$totalhits_sessile/data$Total_sessile*100

# fig_dots<-data%>%plot_ly(type="scatter",
#           mode = "markers+lines",line = list(width=0.35))%>%
#   add_trace(x = time,
#           y = ~totalperc_motile,
#           color ="Host",
#           colors=c("#2A6074","#00C9B1"),
#           size = ~Total_motile,
#           customdata = ~paste(totalhits_motile, "out of ", Total_motile," hosts"),
#           hovertemplate="%{y} % of motile hosts <br> are infected  <br> ie %{customdata}")


# fig_dots<-fig_dots %>%
#   add_trace(
#     x = ~time,
#     y = ~totalperc_sessile,
#     color = "Deposits",
#     colors = c("#FFF184", "#FFDD80"),  # Reversed color order
#     size = ~Total_sessile,
#     customdata = ~paste(totalhits_sessile, "out of ", Total_sessile," deposits"),
#     hovertemplate = "%{y} % of sessile deposits <br> are infected  <br> ie %{customdata}",
#     line = list(width = 0.35)
#   ) %>%
#   layout(title = "Infection Trend across population",
#          plot_bgcolor = '#FFF8EE',
#          xaxis = list(
#           title = "Time (Hours)",
#            zerolinecolor = '#ffff',
#            zerolinewidth = 0.5,
#            gridcolor = '#F4F2F0'),
#          yaxis = list(
#           title = "Percentage of Infected",
#            zerolinecolor = '#ffff',
#            zerolinewidth = 0.5,
#            gridcolor = '#F4F2F0'))


# htmlwidgets::saveWidget(fig_dots, "scatter_plot_final.html", selfcontained = TRUE)