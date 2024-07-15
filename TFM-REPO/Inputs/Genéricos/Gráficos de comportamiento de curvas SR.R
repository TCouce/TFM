# Conversión gráficos analisis de curvas a ggplot

# Librerias
library(FLCore)
library(ggplotFL)
library(FLBRP)
library(viridis)
library(gridExtra)
library(dplyr)
library(cowplot)
library(ggplot2)

# Cargar los datos
dir <- getwd()
dir_RData <- paste0(dir, "/data/hake.RData")
load(dir_RData)

# Preparar los datos
recy <- 1982:2022 
stk <- window(stk, recy[1], recy[length(recy)])

# Crear objeto FLSR
hake <- as.FLSR(stk)

# Vector de incrementos
incrementos <- seq(0.70, 1.30, 0.10)

# Ajustar modelo Beverton-Holt
model(hake) <- bevholt()
hake_bh <- fmle(hake)

# Parámetros de Beverton-Holt
a <- as.numeric(hake_bh@params[1])
b <- as.numeric(hake_bh@params[2])

# Rango de SSB
Max <- max((ssb(hake_bh))) * 1.1
ssb_range <- seq(0, Max, length.out = 10000)

# Resultados para el parámetro a de Beverton-Holt
resultados_bha <- data.frame(
  ssb = rep(ssb_range, times = length(incrementos)),
  rec = unlist(lapply(incrementos, function(inc) (ssb_range * (a * inc)) / (ssb_range + b))),
  incremento = rep(incrementos, each = length(ssb_range))
)

# Gráfico con ggplot2
plot_bha <- ggplot(resultados_bha, aes(x = ssb, y = rec, color = as.factor(incremento))) +
  geom_line(size = 1) +
  scale_color_viridis(discrete = TRUE) +
  labs(title = "Análisis del efecto del parámetro a ",
       subtitle = "Modelo de Beverton-Holt",
       x = "SSB",
       y = "Reclutamiento",
       color = "Incremento") +
  theme_minimal()

# Ruta de la carpeta para guardar el plot
output_dir <- "C:/Users/Tito/Documents/Tito CDS/MTE/TFM IEO/Case Studies/Imagenes"

# Combinamos la carpeta con el nombre del archivo
output_file <- file.path(output_dir, "Parámetro BH a.png")

png(output_file, width = 1800, height = 1400,res=300)
print(plot_bha)
dev.off()


# Resultados para el parámetro b de Beverton-Holt
resultados_bhb <- data.frame(
  ssb = rep(ssb_range, times = length(incrementos)),
  rec = unlist(lapply(incrementos, function(inc) (ssb_range * a) / (ssb_range + (b * inc)))),
  incremento = rep(incrementos, each = length(ssb_range))
)

# Gráfico con ggplot2
plot_bhb <- ggplot(resultados_bhb, aes(x = ssb, y = rec, color = as.factor(incremento))) +
  geom_line(size = 1) +
  scale_color_viridis(discrete = TRUE) +
  labs(title = "Análisis del efecto del parámetro b",
       subtitle = "Modelo de Beverton-Holt",
       x = "SSB",
       y = "Reclutamiento",
       color = "Incremento") +
  theme_minimal()


output_file <- file.path(output_dir,"Parámetro BH b.png")
png(output_file, width = 1800, height = 1400,res=300)
print(plot_bhb)
dev.off()

# Ajustar modelo Ricker
model(hake) <- ricker()
hake_ric <- fmle(hake)

# Parámetros de Ricker
a <- as.numeric(hake_ric@params[1])
b <- as.numeric(hake_ric@params[2])

# Resultados para el parámetro a de Ricker
resultados_rica <- data.frame(
  ssb = rep(ssb_range, times = length(incrementos)),
  rec = unlist(lapply(incrementos, function(inc) (ssb_range * (a * inc)) * exp(-b * ssb_range))),
  incremento = rep(incrementos, each = length(ssb_range))
)

# Gráfico con ggplot2
plot_rica <- ggplot(resultados_rica, aes(x = ssb, y = rec, color = as.factor(incremento))) +
  geom_line(size = 1) +
  scale_color_viridis(discrete = TRUE) +
  labs(title = "Análisis del efecto del parámetro a",
       subtitle = "Modelo de Ricker",
       x = "SSB",
       y = "Reclutamiento",
       color = "Incremento") +
  theme_minimal()

output_file <- file.path(output_dir,"Parámetro Ricker a.png")
png(output_file, width = 1800, height = 1400,res=300)
print(plot_rica)
dev.off()

# Resultados para el parámetro b de Ricker
resultados_ricb <- data.frame(
  ssb = rep(ssb_range, times = length(incrementos)),
  rec = unlist(lapply(incrementos, function(inc) ssb_range * a * exp((-b * inc) * ssb_range))),
  incremento = rep(incrementos, each = length(ssb_range))
)

# Gráfico con ggplot2
plot_ricb <- ggplot(resultados_ricb, aes(x = ssb, y = rec, color = as.factor(incremento))) +
  geom_line(size = 1) +
  scale_color_viridis(discrete = TRUE) +
  labs(title = "Análisis del efecto del parámetro b",
       subtitle = "Modelo de Ricker",
       x = "SSB",
       y = "Reclutamiento",
       color = "Incremento") +
  theme_minimal()

output_file <- file.path(output_dir,"Parámetro Ricker b.png")
png(output_file, width = 1800, height = 1400,res=300)
print(plot_ricb)
dev.off()


