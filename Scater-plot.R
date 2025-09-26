

setwd("C:/Users/Andreza/Documents/GitHub/Biogeography-Diversity-papper-supplementary")
getwd()

# BblioteCAS----
library(readr)
library(dplyr)
library(ggplot2)
library(geobr)
library(ggspatial)
library(rnaturalearth)
library(sf)
library(gridExtra)
library(writexl)
library(tidyverse)
library(devtools)
library(rnaturalearthhires)
library(vegan)
library(dplyr)
library(raster)
library(reshape2)
library(extrafont)


# GRAFICO DE GENERO                       -----
  
# 1. chamando dados----

top_10_generos<- readxl::read_xlsx("Data/top_10_gen.xlsx")

windows(12,9)

# 2. usa o factor para unir os generos de acordo com a ordem  
top_10_generos$Regions <- factor(top_10_generos$Regions, levels = c("CA", "BR", "UR/AR", "PA"))

top_10_generos <- top_10_generos %>%
  mutate(genero = ifelse(Ordem == "A", paste0(genero, "*"), genero))

# Ordenar pela soma total da contagem por gênero (ordem decrescente)
top_10_generos <- top_10_generos %>%
  group_by(genero) %>%
  mutate(total = sum(contagem)) %>%
  ungroup() %>%
  mutate(genero = reorder(genero, -total))
# 3. Plotando o grafico ----

plot.gen <- ggplot(top_10_generos, aes(y = contagem, x = genero, fill = Regions)) + 
  geom_bar(
    position = "stack", stat = "identity", size = 0.4
  ) +
  labs(y = "Species number", x = "Genus", fill = "Regions", color = "Regions") +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    rect = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, face = "italic"),
    text = element_text(size = 14),
    legend.position = "none"  # excluindo legenda para ficar apenas
    
  ) +
  scale_fill_manual(values = c("CA" = "#CD3A18", "BR" = "#1D6329", 
                               "UR/AR" = "#91D6DE", "PA" = "#213778")) +
  scale_color_manual(values = c("CA" = "#CD3A18", "BR" = "#1D6329", 
                                "UR/AR" = "#91D6DE", "PA" = "#213778")) +
  geom_text(
    aes(label = ifelse(round(contagem, 1) > 1,
                       paste0(round(contagem, 1), "%"), "")), 
    size = 4, 
    position = position_stack(vjust = 0.5), 
    color = "white")

plot.gen

ggsave("Plots/genera.png",
       plot = plot.gen, width = 10, height = 10, dpi = 300, units = "in")


#GRAFICO DE FAMILIA                       ----

#   grafico modificado em 07.3.25 para retirar a % do 1, deixando mais limpo  #

# 1.  Chamando od dados-----
top_10_fam<- readxl::read_xlsx("Data/top_10_fam.xlsx")

# 1. Substituir NA
top_10_fam$Regions[is.na(top_10_fam$Regions)] <- "UR/AR"

# 2. Criar marcação com * antes de agrupar
top_10_fam <- top_10_fam %>%
  mutate(Family = ifelse(Ordem == "A", paste0(Family, "*"), Family))

# 3. Definir ordem dos níveis de Regions
top_10_fam$Regions <- factor(top_10_fam$Regions, levels = c("CA", "BR", "UR/AR", "PA"))

# 4. Ordenar por contagem total (sem repetir nomes!)
top_10_Family <- top_10_fam %>%
  group_by(Family) %>%
  mutate(total = sum(contagem)) %>%
  ungroup() %>%
  mutate(Family = reorder(Family, -total))


# 3. plot do grafico
plot.fam <- ggplot(top_10_Family, aes(y = contagem, x = Family, fill = Regions)) + 
  geom_bar(
    position = "stack", stat = "identity", size = 0.4
  ) +
  labs(y = "Species number", x = "Family", fill = "Regions", color = "Regions") +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    rect = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    text = element_text(size = 14),
    legend.position = "none"  # excluindo legenda para ficar apenas
    
  ) +
  scale_fill_manual(values = c("CA" = "#CD3A18", "BR" = "#1D6329", 
                               "UR/AR" = "#91D6DE", "PA" = "#213778")) +
  scale_color_manual(values = c("CA" = "#CD3A18", "BR" = "#1D6329", 
                                "UR/AR" = "#91D6DE", "PA" = "#213778")) +
  geom_text(
    aes(label = ifelse(round(contagem, 1) > 1,
                       paste0(round(contagem, 1), "%"), "")), 
    size = 4, 
    position = position_stack(vjust = 0.5), 
    color = "white"
  )

plot.fam

ggsave("Plots/family.png",
       plot = plot.fam, width = 10, height = 10, dpi = 300, units = "in")
