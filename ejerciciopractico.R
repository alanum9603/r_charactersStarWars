# Importamos las librerías 
library("dplyr")
library("tidyr")
library("ggplot2")

# Trabajo Práctico
# La fuente de datos del presente trabajo fue extraída de Kaggle
# Tema: Star Wars
# Link: https://www.kaggle.com/datasets/jsphyg/star-wars


# Extraemos el csv de personajes de Star Wars en un dataframe
df_characters <- read.csv("src/characters.csv")

# Observamos los datos de df_characters
glimpse(df_characters)

# Añadimos una columna Quantity que nos ayudará más adelante a hacer el conteo 
# de filas
df_characters <- df_characters |>
  mutate(Quantity = 1)

# Creamos un nuevo dataframe donde se sumen los valores de Quantity para obtener
# un conteo de los seres de cada genero de cada raza
df_species <- df_characters |>
  group_by(gender, species) |>
  summarise(
    sumQuantity = sum(Quantity)
  )

# Lo ordenamos por especie y género para facilitar la visualización
df_species <- df_species |>
  arrange(species, gender)

# Visualizamos los datos
glimpse(df_species)

# Podemos observar que hay varias razas que solo tienen 1 ser registrado,
# por ello, agruparemos todas las razas que cumplan esa condición y le daremos
# la categoría Others

# Para ello, primero pivotaremos los datos de la columna gender y sumQuantity 
# para obtener columnas que representen cada genero por separado y hagan un 
# conteo de cuantos seres vivos existen por cada genero y raza
df_species <- df_species |>
  pivot_wider(names_from = gender, values_from = sumQuantity)

# Para facilitar el uso de condicionales más adelante, reemplazaremos por cero
# los valores NA que representan que no hay seres vivos del genero de la 
# columna en la respectiva raza
df_species <- df_species |>
  mutate(
    Male = ifelse(is.na(Male),0,Male),
    Female = ifelse(is.na(Female),0,Female),
    None = ifelse(is.na(None),0,None)
  )

# A continuación, crearemos una nueva columna Species donde aplicaremos una 
# condicional que sume el conteo de cada genero por raza y si este es 1, se 
# agregará el texto "Others", en caso contrario, se agregará el valor de la 
# columna species de la misma fila
df_species <- df_species |>
  mutate(Species = if_else((Male + Female + None) == 1, "Others", species))

# Ahora, agruparemos los datos de la columna Species con el fin de agrupar los
# datos "Others" de la columna Species
df_species <- df_species |>
  group_by(Species) |>
  summarise(
    Male = sum(Male),
    Female = sum(Female),
    None = sum(None)
  )

# Ahora, volveremos a pivotar las columnas de géneros para volver a tener 2
# columnas Gender y Quantity
df_species <- df_species |>
  pivot_longer(cols = c(Male, Female, None),
               names_to = "Gender",
               values_to = "Quantity")

# Visualizamos el dataframe
glimpse(df_species)

# Como podemos observar, existen géneros en razas que tienen el valor 0, para
# evitar ello, filtraremos los resultados para eliminar las filas que tengan
# el valor 0 en la columna Quantity
df_species <- df_species |>
  filter(Quantity != 0)

# A continuación, creamos nuestro gráfico con ggplot
chart <- ggplot(df_species, aes(x=reorder(Species, -Quantity), y=Quantity, fill=Gender)) + 
  geom_bar(stat = "identity", position = position_dodge()) + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  geom_text(aes(Species, label = Quantity), color = "blue", size = 5, vjust = -0.15, position = position_dodge(width = 0.9)) +
  xlab("Species")

# Finalmente, guardamos nuestro gráfico como un archivo svg
ggsave("plot.svg", plot = chart, width = 8, height = 6)
