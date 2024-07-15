# Cargar las librerías necesarias
library(ggplot2)
library(viridis)
library(gridExtra)
library(FLCore)
library(FLBRP)

# Establecer directorio y cargar datos
dir <- getwd()
dir_RData <- paste0(dir, "/data/hake.RData") # Ajusta según el nombre correcto de tu archivo RData
load(dir_RData)

# Cortar los datos a años específicos
recy <- 1982:2022 
stk <- window(stk, recy[1], recy[length(recy)])

# Crear objeto FLSR
hake <- as.FLSR(stk)

# Vector de incrementos
incrementos <- seq(0.70, 1.30, 0.10)

# Ajustar modelo Ricker
model(hake) <- ricker()
hake_ric <- fmle(hake)

# Ajustar modelo Beverton-Holt
model(hake) <- bevholt()
hake_bh <- fmle(hake)

# Rango de SSB para Beverton-Holt
Max_bh <- max((ssb(hake_bh))) * 1.1
ssb_range_bh <- seq(0, Max_bh, length.out = 10000)
a_bh <- as.numeric(hake_bh@params[1])
b_bh <- as.numeric(hake_bh@params[2])
rec_range_bh <- (ssb_range_bh * a_bh) / (ssb_range_bh + b_bh)

# Rango de SSB para Ricker
Max_ric <- max((ssb(hake_ric))) * 1.1
ssb_range_ric <- seq(0, Max_ric, length.out = 10000)
a_ric <- as.numeric(hake_ric@params[1])
b_ric <- as.numeric(hake_ric@params[2])
rec_range_ric <- a_ric * ssb_range_ric * exp(-b_ric * ssb_range_ric)

# Crear data frames para ggplot
df_ric <- data.frame(ssb = ssb_range_ric, rec = rec_range_ric, tipo = "Ricker")
df_bh <- data.frame(ssb = ssb_range_bh, rec = rec_range_bh, tipo = "Beverton-Holt")

# Combinar data frames
ggplot_df_long <- bind_rows(df_ric, df_bh)

# Puntos para el gráfico
px <- an(ssb(hake_bh))
py <- an(rec(hake_bh))
points <- data.frame(ssb = px, rec = py)

# Gráfico usando ggplot2 y paleta viridis
plotSR <- ggplot(ggplot_df_long) +
  geom_line(aes(x = ssb, y = rec, color = tipo), size = 1.25) +
  scale_color_viridis(discrete = TRUE) +
  labs(title = "Relación de Stock Reclutamiento",
       subtitle = "Modelos de Beverton-Holt y Ricker",
       x = "Spawning Stock Biomass (SSB)",
       y = "Recruitment",
       color = "Tipo") +
  theme_minimal() +
  scale_x_continuous(limits = c(0, max(ssb_range_bh) * 1.1))  # Ajustar límites del eje x

# Guardar el gráfico como imagen PNG
output_dir <- "C:/Users/Tito/Documents/Tito CDS/MTE/TFM IEO/Case Studies/Imagenes/Generales"
output_file <- file.path(output_dir, "SR_Relationship.png")
png(output_file, width = 1800, height = 1400, res = 300)
print(plotSR)
dev.off()
