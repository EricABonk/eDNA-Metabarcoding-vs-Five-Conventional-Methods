library(plotly)
library(dplyr)
library(scales)
x2<-read_excel("Map/Detour Samples Tracking.xlsx")

# Assuming "sample" is the name of the column you want to modify
columnName <- "Sample"
x2[, columnName][x2[, columnName] == "08-07-23-SC-REF"] <- "08-07-23 - SunC-REF"
x2[, columnName][x2[, columnName] == "09-07-23-SC-REF"] <- "09-07-23 - SunC-REF"
x2[, columnName][x2[, columnName] == "08-07-23-SC-NF"] <- "08-07-23 - SunC-EXP"
x2[, columnName][x2[, columnName] == "10-07-23-KC-EXP"] <- "10-07-23 - KC-EXP"
x2[, columnName][x2[, columnName] == "11-07-23-KC-REF"] <- "11-07-23 - KC-REF"
x2[, columnName][x2[, columnName] == "12-07-23-SC-FF"] <- "12-07-23 - SunC-FF"
x2[, columnName][x2[, columnName] == "09-07-23-SC-NF"] <- "09-07-23 - SunC-EXP"
x2[, columnName][x2[, columnName] == "11-07-23-FISH-POND"] <- "12-07-23 - RJ-POND"
x2[, columnName][x2[, columnName] == "12-07-23-HC-REF"] <- "12-07-23 - HC-REF"
x2[, columnName][x2[, columnName] == "13-07-23-HC-REF"] <- "13-07-23 - HC-REF"
eDNA<-read_excel("Data/eDNA.xlsx")

x2<-subset(x2, select=c(Sample,Lat,Long, Rep))

z<-unique(eDNA$Sample)
z<-as.data.frame(z)
x2 <- x2 %>% filter(Sample %in% z$z)

x2 <- x2 %>% distinct(Sample, .keep_all = TRUE)

# Assume x2 has Sample, Lat, Long columns
x2 <- x2 %>% mutate(
  Sample = as.factor(Sample)
)

x2$Lat<-as.numeric(x2$Lat)
x2$Long<-as.numeric(x2$Long)
x2$Lat_jit <- jitter(x2$Lat, amount = 0.0005)
x2$Long_jit <- jitter(x2$Long, amount = 0.0005)

# Generate ggplot2 default colors for Sample levels
sample_levels <- levels(x2$Sample)
sample_colors <- hue_pal()(length(sample_levels))
names(sample_colors) <- sample_levels

# Map Sample to color hex codes
x2 <- x2 %>% mutate(color = sample_colors[Sample])

fig <- plot_ly(
  x2,
  lat = ~Lat_jit,
  lon = ~Long_jit,
  text = ~Sample,
  type = "scattermapbox",
  mode = "markers",
  marker = list(
    color = ~color,
    size = 15,
    line = list(color = "black", width = 1)
  )
) %>%
  layout(
    mapbox = list(
      style = "white-bg",
      zoom = 14,
      center = list(lat = mean(x2$Lat), lon = mean(x2$Long)),
      layers = list(
        list(
          sourcetype = "raster",
          source = list("https://services.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}"),
          below = "traces"
        )
      )
    )
  )



fig
test_point <- x2[3, ]  # the SunC-REF row

plot_ly(
  test_point,
  lat = ~Lat,
  lon = ~Long,
  text = ~Sample,
  type = "scattermapbox",
  mode = "markers",
  marker = list(
    color = ~color,
    size = 12,
    line = list(color = "black", width = 1)
  )
) %>%
  layout(
    mapbox = list(
      style = "white-bg",
      zoom = 16,
      center = list(lat = test_point$Lat, lon = test_point$Long),
      layers = list(
        list(
          sourcetype = "raster",
          source = list("https://services.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}"),
          below = "traces"
        )
      )
    )
  )



fig <- plot_ly(
  x2,
  lat = ~Lat_jit,
  lon = ~Long_jit,
  text = ~Sample,
  type = "scattermapbox",
  mode = "markers",
  marker = list(
    color = ~color,
    size = 10,
    line = list(color = "black", width = 1)
  )
) %>%
  layout(
    mapbox = list(
      style = "white-bg",
      zoom = 14,
      center = list(lat = mean(x2$Lat), lon = mean(x2$Long)),
      layers = list(
        list(
          sourcetype = "raster",
          source = list("https://services.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}"),
          below = "traces"
        )
      )
    )
  )

