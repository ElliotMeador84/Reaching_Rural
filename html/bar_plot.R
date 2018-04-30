library(tidyverse)
library(RColorBrewer)


# Bar_plot --------------------------------------------------------------
colors <- RColorBrewer::brewer.pal(9, 'Blues')[c(4, 7)]

bar_plot <- code_percent %>%
    ggplot(aes(fct_reorder(Description, Percent), Percent, label = n)) +
    geom_bar(stat = 'identity',
             width = .85,
             position = position_dodge(),
             aes(color = MO,
                 fill = MO)) +
    coord_flip() +
    theme_bw() +
    scale_y_continuous(labels = scales::percent) +
    scale_color_manual(values = colors) +
    scale_fill_manual(values = colors) +
    theme(
        legend.position = 'bottom',
        legend.title = element_blank(),
        panel.grid  = element_blank(),
        strip.background = element_blank(),
        panel.border = element_blank(),
        plot.title = element_text(hjust = 0)
    ) +
    labs(
        x = 'USDA Rural - Urban\nClassification',
        y = 'Percent of total counties',
        caption = '*Not adjacent to\nan urban county'
    )
bar_plot



