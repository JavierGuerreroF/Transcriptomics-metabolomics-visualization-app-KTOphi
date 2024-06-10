# creacion de archivos que faltan en la expression ficticios
# FALTARIA RECUPERAR LOS ORIGINALES

library(stringr)

files <- list.files()
adg_files <- files[grep("adg", files)]
adg_files <- files[grep("tsv", files)]
adg_files <- gsub(".tsv", "", adg_files)

# analisis de los campos 
adg_split <- str_split(adg_files, "_")

data_list <- adg_split
df <- do.call(rbind, lapply(data_list, function(x) as.data.frame(t(x), stringsAsFactors = FALSE)))
colnames(df) <- c("adg_col", "organism", "consortium_col", "contrast", "conditions")

## 

path_false_adg <- "/Users/jguerrero/Documents/CIB/Visualization_app_TEST/data/false_adg"

false_adg_files <- list.files(path_false_adg)


# ----------------------------------------------

# quiero que generar los arhivos que hay en false_adg_files pero con valores 
# aleatorios en lugar de los que hay en el tsv, esp si que sean valores parecidos pero ligeramentes diferentes

# Cargar las librerías necesarias
library(dplyr)

# Ruta donde se encuentran los archivos originales y donde se guardarán los nuevos archivos
path_false_adg <- "/Users/jguerrero/Documents/CIB/Visualization_app_TEST/data/false_adg"
path_new_files <- "/Users/jguerrero/Documents/CIB/Visualization_app_TEST/data/false_adg/random_values_adg_files"

# Obtener la lista de archivos
false_adg_files <- list.files(path = path_false_adg, pattern = "\\.tsv$", full.names = TRUE)

# Función para generar valores aleatorios similares
generate_similar_values <- function(data,modifier = 0.05) {
  noise <- runif(n = length(data), min = -modifier * abs(data), max = modifier * abs(data))
  return(data + noise)
}

# Leer, modificar y guardar archivos
for (file in false_adg_files) {
  print(basename(file))
  
  # Leer el archivo TSV
  data <- read.csv(file, header = TRUE, sep = "\t")
  
  print(basename(file))
  print(str(data))
  # Modificar los valores (suponiendo que las columnas numéricas sean las que queremos modificar)
  data_modified <- data # %>%
    # mutate(across(where(is.numeric), generate_similar_values))
  data_modified$log2FoldChange <- generate_similar_values(data$log2FoldChange,0.3)
  data_modified$padj <- generate_similar_values(data$padj,0.1)
  
  # Nombre del nuevo archivo
  new_file_name <- file.path(path_new_files, basename(file))
  
  # Guardar el nuevo archivo TSV
  write.table(data_modified, new_file_name, sep = "\t", row.names = FALSE)
}

print("Archivos generados con valores aleatorios similares.")
