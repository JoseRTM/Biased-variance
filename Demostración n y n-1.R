# Demostración n-1
# El estimador de la varianza es sesgado en muestras pequeñas.

# Cargar la librería ggplot2
library(ggplot2)

# Crear una población
set.seed(1844)
pop <- rnorm(10000, 25, 5)
pop_var <- var(pop)

# Número de simulaciones
n_sim <- 1000

# Tamaños de muestra a probar
sample_sizes <- c(5, 10, 50, 100)

# Iniciar un dataframe vacío para almacenar los resultados
df <- data.frame(Estimate = numeric(), Type = character(), SampleSize = numeric())

# Realizar las simulaciones para cada tamaño de muestra
for (n in sample_sizes) {
  biased_estimates <- numeric()
  unbiased_estimates <- numeric()
  
  for (i in 1:n_sim) {
    sample_data <- sample(pop, n)
    biased_estimates[i] <- sum((sample_data - mean(sample_data))^2) / n
    unbiased_estimates[i] <- var(sample_data)
  }
  
  temp_df <- data.frame(
    Estimate = c(biased_estimates, unbiased_estimates),
    Type = c(rep("Biased", n_sim), rep("Unbiased", n_sim)),
    SampleSize = rep(n, 2 * n_sim)
  )
  
  df <- rbind(df, temp_df)
}

# Crear el gráfico con un enfoque en un rango más pequeño alrededor de la varianza poblacional

ggplot(df, aes(x = Estimate, fill = Type)) +
  geom_density(alpha = 0.5) +
  geom_vline(aes(xintercept = pop_var), color = "blue", linetype = "dashed", size = 1) +
  annotate("text", x = pop_var, y = Inf, label = "Population Variance", vjust = 2, color = "blue") +
  ggtitle("Distribution of Variance Estimates") +
  xlab("Variance Estimate") +
  ylab("Density") +
  xlim(min(df$Estimate), max(df$Estimate)) + # Fijar los límites del eje x
  scale_fill_manual(values = c("Biased" = "red", "Unbiased" = "green")) +
  facet_wrap(~SampleSize) + # Eliminado "scales = 'free'"
  geom_vline(data = subset(df, Type == "Biased"), aes(xintercept = mean(Estimate)), color = "red", linetype = "dotted", size = 1) +
  geom_vline(data = subset(df, Type == "Unbiased"), aes(xintercept = mean(Estimate)), color = "green", linetype = "dotted", size = 1)

