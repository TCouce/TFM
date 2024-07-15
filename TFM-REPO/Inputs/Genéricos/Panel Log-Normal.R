# Cargar las librerías necesarias
library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)
library(viridis)  # Para la paleta de colores viridis

# Parámetros conocidos
m <- 1  # Media de la distribución log-normal
cvs <- c(0.2, 0.4, 0.6)  # Coeficientes de variación

# Función para calcular mu y sigma a partir de m y cv
calc_mu_sigma <- function(m, cv) {
  sigma <- sqrt(log(cv^2 + 1))
  mu <- log(m) - (sigma^2 / 2)
  return(list(mu = mu, sigma = sigma))
}

# Generar datos para las curvas de densidad
x <- seq(0, 5, length.out = 100000)
density_data <- data.frame(x = x)

# Generar curvas de densidad para cada coeficiente de variación
curve_plots <- lapply(seq_along(cvs), function(i) {
  cv <- cvs[i]
  params <- calc_mu_sigma(m, cv)
  data <- data.frame(x = x, 
                     density = dlnorm(x, meanlog = params$mu, sdlog = params$sigma),
                     cv = paste0("CV = ", cv))
  
  ggplot(data, aes(x = x, y = density, color = cv)) +
    geom_line(size = 1) +
    labs(title = paste("CV =", cv), x = "Valores", y = "Densidad") +
    scale_color_viridis(discrete = TRUE) +  # Usar paleta viridis
    theme_light() +
    theme(legend.position = "none")
})

# Combinar los gráficos de curvas en un solo panel
curve_combined <- ggplot(density_data_long, aes(x = x, y = density, color = cv)) +
  geom_line(size = 1) +
  labs(title = "Curvas de Distribución Log-Normal", x = "Valores", y = "Densidad") +
  scale_color_viridis(discrete = TRUE) +  # Usar paleta viridis
  theme_light() +
  theme(legend.title = element_blank())

# Organizar los gráficos en un panel
grid_arrange <- grid.arrange(
  arrangeGrob(grobs = curve_plots, ncol = 3),
  arrangeGrob(curve_combined),
  ncol = 1, heights = c(2, 2)  # Primera fila para curvas individuales, segunda fila para curva combinada
)

# Guardar el panel combinado como imagen PNG
png_path <- "C:/Users/Tito/Documents/Tito CDS/MTE/TFM IEO/Case Studies/Imagenes/Generales/panel_lognormal.png"
png(png_path, width = 1800, height = 1400, res = 300)
print(grid_arrange <- grid.arrange(
  arrangeGrob(grobs = curve_plots, ncol = 3),
  arrangeGrob(curve_combined),
  ncol = 1, heights = c(2, 2)  # Primera fila para curvas individuales, segunda fila para curva combinada
))
dev.off()
