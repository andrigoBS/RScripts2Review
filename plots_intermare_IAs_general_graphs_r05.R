###################################################################################
## Bibliometric analysis of intertidal remote sensing and artificial intelligence #
## Mario Luiz Mascagni 09/06/2024                                                 #
###################################################################################
## Step 01 - Libraries -----

library(dplyr)
library(ggplot2)
library(kableExtra)
library(RColorBrewer)
library(esquisse)
library(wordcloud)
library(bibliometrix)

## Step 02 - Import data -----
wos_raw <- convert2df("C:/Users/locfa/OneDrive/Documentos/R/ias_itertidal/wos_andrigo_36p.bib", dbsource='wos', format='bibtex')
wos_intertidal_ias <- wos_raw

## Step 03 - Analyze and/or summarize results -----
results <- biblioAnalysis(wos_intertidal_ias)
sumary_res <- summary(object = results, k = 35, pause = FALSE)
synonyms <- readLines("C:/Users/locfa/OneDrive/Documentos/R/ias_itertidal/synonyms.txt")

## Graph 01 - Productivity per year -----
df_year <- sumary_res$AnnualProduction %>%
  transmute(year = as.numeric(trimws(`Year   `)),
            articles = Articles) %>%
  filter(year < 2025)

ggplot(df_year, aes(x = year, y = articles)) +
  geom_bar(stat = 'identity', fill = 'darkgray') +    # Shades of gray
  #geom_smooth(se = FALSE, colour = 'black') +         # Line in black
  theme_classic() +
  labs(y = 'Nº of Articles', x = "Year of publication", title = 'Annual scientific production') +
  scale_y_continuous(breaks = seq(0, max(df_year$articles), by = 1), expand = c(0, 0)) + # Y-axis increments of 1 and force Y to start at 0
  scale_x_continuous(breaks = seq(min(df_year$year), max(df_year$year), by = 2)) +  # X-axis every 2 years
  theme(plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.border = element_rect(colour = "black", fill = NA, size = 0.2))  # Adds a box around the plot

ggsave("C:/Users/locfa/OneDrive/Documentos/R/ias_itertidal/outputs/Graph01_ProdPerYear.png")

## Graph 02 - Word network -----
NetMatrix <- biblioNetwork(wos_intertidal_ias,
                           analysis = "co-occurrences",
                           network = "author_keywords",
                           #short = TRUE,
                           synonyms = synonyms,
                           #remove.terms = remover
                           )

# Plot the network
net <- networkPlot(NetMatrix, n = 35, type = "auto", Title = "Word Network",
                   labelsize=1, curved=T, verbose = F)

net

# Open VOSviewer for editing
net2VOSviewer(net, vos.path="C:/Users/locfa/OneDrive/Área de Trabalho/VOSviewer/jar_ver")

# Display the modified plot
print(net)

# Save the plot
ggsave("C:/Users/locfa/OneDrive/Documentos/R/ias_itertidal/outputs/Graph02_WordNet.png")

## Graph 03 - Citations by Year by Author Chart -----
res <- authorProdOverTime(wos_intertidal_ias, k = 35, graph = FALSE)
dfAu <- res$dfAU

ggplot(dfAu) +
  aes(x = year, y = Author, colour = TCpY, size = freq) +
  geom_point(shape = "circle") +
  scale_color_gradient(low = "darkgray", high = 'black') +  # Gray color gradient
  scale_y_discrete(limits = rev) +
  scale_x_continuous(breaks = seq(min(df_year$year), max(df_year$year), by = 5)) +  # X-axis every 2 years
  labs(x = NULL, y = NULL, color = "Nº of Citations per year", size = "Nº of Articles", title = "Annual scientific production by author") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), 
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.border = element_rect(colour = "black", fill = NA, size = 0.2))  # Adds a box around the plot

ggsave("C:/Users/locfa/OneDrive/Documentos/R/ias_itertidal/outputs/Graph03_CitationsByAuthor.png")

graphs_bibliometrix <- plot(results)

## Graph 04 - Most productive authors -----
# Processing and plotting the top 20 most productive authors
top_authors <- wos_intertidal_ias %>% 
  tidyr::separate(AU, sep = ";", into = glue::glue("author_{1:7}")) %>%
  tidyr::pivot_longer(
    cols = glue::glue("author_{1:7}"), 
    names_to = "order_author", 
    values_to = "author_name", 
    values_drop_na = TRUE
  ) %>%
  dplyr::group_by(author_name) %>%
  dplyr::count() %>%
  dplyr::arrange(desc(n)) %>%
  dplyr::ungroup() %>%
  dplyr::slice(1:20)

# Creating the plot
ggplot(top_authors) +
  geom_col(aes(x = reorder(author_name, n), y = n), fill = 'darkgray') +  # Gray color
  coord_flip() +
  theme_minimal() +
  labs(x = NULL, y = "Number of articles", title = "Most productive authors") +
  scale_y_continuous(breaks = seq(0, max(top_authors$n), by = 1)) +  # X-axis increments of 1
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 0, hjust = 1),
    panel.border = element_rect(color = "black", fill = NA, size = 0.2)  # Adds a box around the plot
  )

ggsave("C:/Users/locfa/OneDrive/Documentos/R/ias_itertidal/outputs/Graph04_MostProductiveAuthors.png")

## Graph 05 - Production graph in the years -----
empty_years <- tibble::tibble(
  PY = min(wos_intertidal_ias$PY):max(wos_intertidal_ias$PY),
  n = 0
) %>%
  dplyr::filter(!PY %in% unique(wos_intertidal_ias$PY))

wos_intertidal_ias %>%
  dplyr::count(PY) %>%
  dplyr::full_join(empty_years) %>%
  dplyr::arrange(PY) %>%
  ggplot(aes(x = PY, y = n)) +
  geom_line() +
  geom_area(fill = 'darkgray', alpha = .5) +  # Gray area
  theme_minimal() +
  labs(x = "Years", y = "Number of articles", title = "Scientific production over the years") +
  scale_y_continuous(breaks = seq(0, max(df_year$articles), by = 1), expand = c(0, 0)) +  # X-axis increments of 1
  scale_x_continuous(breaks = seq(min(df_year$year), max(df_year$year), by = 5)) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.border = element_rect(color = "black", fill = NA, size = 0.2)  # Adds a box around the plot
  )

ggsave("C:/Users/locfa/OneDrive/Documentos/R/ias_itertidal/outputs/Graph05_ProductionOverYears.png")

## Graph 06 - Most productive country graph -----
most_productive_countries <- graphs_bibliometrix[[2]][["data"]]

palette <- c("darkgray", "black")  # Gray palette

most_productive_countries %>%
  ggplot() + 
  geom_col(aes(x = reorder(Country, Freq), y = Freq, fill = Collaboration)) +
  scale_fill_manual(values = palette) + 
  coord_flip() +
  theme_minimal() +
  labs(x = NULL, y = "Number of articles", fill = "Collaboration", title = "Most productive countries") +
  theme(plot.title = element_text(hjust = 0.5), 
      axis.text.x = element_text(angle = 0, hjust = 1),
      panel.border = element_rect(colour = "black", fill = NA, size = 0.2))  # Adds a box around the plot

ggsave("C:/Users/locfa/OneDrive/Documentos/R/ias_itertidal/outputs/Graph06_MostProductiveCountries.png")

## Graph 07 - Most Cited Papers -----
res2 <- sumary_res$MostCitedPapers %>%
  transmute(TC = as.numeric(trimws(TC)),
            Paper = `Paper         `) %>%
  mutate(TC = as.numeric(gsub("[^0-9]", "", TC))) %>%
  na.omit()  # Remove rows with NA values

ggplot(res2, aes(x = TC, y = reorder(Paper, TC))) +
  geom_col(fill = "darkgray") +  # Gray bars
  labs(x = "N° of scientific citations", y = NULL, title = "Most cited articles") +
  theme_minimal()
  theme(plot.title = element_text(hjust = 0.5), 
      axis.text.x = element_text(angle = 0, hjust = 1),
      panel.border = element_rect(colour = "black", fill = NA, size = 0.2))  # Adds a box around the plot

ggsave("C:/Users/locfa/OneDrive/Documentos/R/ias_itertidal/outputs/Graph07_MostCitedPapers.png")

## Graph 08 - Word Cloud
df_key <- as.data.frame(results$DE)
wordcloud(words = df_key$Tab, freq = df_key$Freq, min.freq = 1,
          max.words=50, random.order=FALSE, rot.per=0.1,
          colors=brewer.pal(8, "Dark2"))

# GENERAL TOOLS -----
esquisser() # graphical interface graph generator and editor
biblioshiny() # ready-made graphics generator
graphs_bibliometrix <- plot(results) # To see all possible graphs
