# Library ----
# Load required libraries from an external script
source("./library.R", echo = FALSE)

# Import ----
source('install_packages.R')
source("./scripts/function.R", echo = FALSE)

# Load the dataset from the CSV file with proper encoding and delimiters
my_dataset <- "Bombus_alpinus.csv"
my_dataset <- "Megachile_sculpturalis.csv"

df <- fread(paste0("./data/dataset/", my_dataset), header = TRUE, 
            sep = ",", dec = ".", strip.white = FALSE, encoding = "Latin-1")

# Spatial data ----

## Shapefile World ----
## Will be used as background of the map
#  Projection : European Terrestrial Reference System 1989 (ETRS89-extended) EPSG:3035. 
projection_3035 <- st_crs("EPSG:3035")
# Transforming the shapefile to the ETRS89-extended projection
world_3035 <- ne_countries(scale = "large", returnclass = "sf") %>% # rnaturalearth package allows to import shapefile
  st_transform(., projection_3035) # transform projection world 

world_3035$country <- world_3035$sovereignt # rename() doesn't work with sf object

## Shapefile Europe ----
europe_3035 <- st_read("./data/shapefile/europe.shp") %>% 
  select(COUNTRY) # Keep only the country and geometry column 
# Transforming the shapefile to the ETRS89-extended projection
europe_3035 <- st_transform(europe_3035, projection_3035)

## /!\ You have to remove data without coordinates /!\
df_distribution <- df
# Remove rows with missing coordinates
df_distribution <- df_distribution[complete.cases(df_distribution$decimalLongitude, df_distribution$decimalLatitude), ]
# Check the number of rows lost
nrow(df)-nrow(df_distribution)

# Converting the df_distribution DataFrame into a simple feature
# If you import directly in CRS = 3035, there are some issues with the coordinates
df_map <- st_as_sf(df_distribution, coords = c("decimalLongitude", "decimalLatitude"), crs = 4326, remove = FALSE)  

# df_db will be used to output xlsx and csv files
# df_map will be used to generate maps
# Transformation of the simple feature df_distribution to the ETRS89-extended projection
df_map <- st_transform(df_map, projection_3035)




## Year interval ----
# Check the distribution of the years
ggplot(df_map, aes(x = endYear)) +
  geom_histogram(binwidth = 1, fill = "#CC99FF", color = "black", alpha = 0.7, na.rm = TRUE) + 
  labs(title = "Distribution of Years", x = "Year", y = "Frequency") +
  theme_minimal()

# Add a column with the intervals of the years
df_map <- df_map %>% 
  mutate(YEAR_INTERVAL = factor(
    case_when(
      is.na(endYear) ~ "No year",
      endYear < 1970 ~ "<1970",
      endYear >= 1970 & endYear <= 2000 ~ "1970-2000", 
      endYear > 2000 ~ ">2000"
    ),
    levels = c("No year", "<1970", "1970-2000", ">2000")
  ))


# Check values
sum(is.na(df_map$YEAR_INTERVAL))
table(df_map$YEAR_INTERVAL)

### Color palet ----
# Attribute a color to each interval of years
col_year <-             c("#FFFFFF", "#B22123", "#FFA200",   "#7EB02D")
year_interval_value <-  c("No year", "<1970",   "1970-2000", ">2000")
color_mapping <- setNames(col_year, year_interval_value)


# Dot map ----
  ## ggplot ---- 
p <- ggplot() +
  ### shp world ---- 
geom_sf(data = world_3035, fill = "#EBEBEB", color = "#C1C1C1", linewidth = 0.4, inherit.aes = FALSE) +
  ### shp WB ---- 
geom_sf(data = europe_3035, linewidth = 0.5,
        fill = "#FFFFF2", # country interior color
        color = "#C1C1C1", inherit.aes = FALSE) + # country border color
  ### dot ---- 
geom_sf(data = df_map,
        aes(fill = YEAR_INTERVAL), # associate color mapping and the dataframe by YEAR_INTERVAL
        size = 2,
        shape = 21,
        stroke = 0.2,
        color = "black",
        na.rm = FALSE, inherit.aes = FALSE) +
  ### limit ---- 
coord_sf(xlim = c(600000, 7700000), ylim = c(800000, 6800000), expand = FALSE) +  # Limit to Europe area
  ### color mapping ---- 
scale_fill_manual(values = color_mapping, na.value = "purple", name = "Year") + # Color dot with color mapping
  ### grid ---- 
scale_x_continuous(breaks = seq(-90, 90, by = 10)) + # grid longitude by 2
  scale_y_continuous(breaks = seq(-90, 90, by = 10)) + # grid latitude by 2
  ### legend ----
guides(fill = guide_legend(override.aes = list(size = 8))) + # Adjust the size of points in the legend
  ### title ----
labs(title = bquote(paste("Distribution of ", italic(.("XXX")))),
     caption = "Map of Europe (European Terrestrial Reference System 1989 extended projection)",
     fill = "Year") + # Legend title
  xlab(expression(paste("Longitude (", degree, ")"))) +
  ylab(expression(paste("Latitude (", degree, ")"))) +
  ### theme ----
theme_minimal() +
  theme(
    panel.ontop = FALSE,  # grid layer forward
    plot.margin = margin(t = 1,  # Top margin
                         r = 0,  # Right margin
                         b = 0,  # Bottom margin
                         l = 1), # Left margin
    plot.caption = element_text(color = "black", face = "plain", size = 7),
    axis.text.x = element_text(angle = 90, hjust = 1),
    axis.text = element_text(size = 9),
    axis.title = element_text(size = 8, face = "plain" ), # Latitude and Longitude
    legend.position = "right",
    legend.key.height = unit(0.8, "cm"),
    legend.key.width = unit(0.2, "cm"),
    legend.text = element_text(size = 9), # size text legend
    legend.title = element_text(size = 9, margin = margin(b = 10)), # size title legend (YEAR)
    plot.background = element_rect(fill = "white",
                                   color = "white", size = 0),
    panel.border = element_rect(color = "grey", fill = "transparent", size = 1),
    panel.background = element_rect(fill = "aliceblue"), # background color (water)
    panel.grid.major = element_line(color = "grey", linetype = "solid", size = 0.5)
  )

p

# Export png  ----
map_file_name <- "name_of_the_map" # Name of the map file
ggsave(filename = paste0("./output/", map_file_name, ".png"), 
       plot = p, device = "png", width = 14, height = 10, bg = "white", dpi = 120)


# Remove objects from memory
rm(list = ls())
gc()
