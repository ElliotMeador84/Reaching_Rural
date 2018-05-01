library(tidyverse)
library(RColorBrewer)
library(scales)


density_plot <- codes %>% # interesting plot, not sure what to do with it
    mutate(Population_2010 = Population_2010) %>% 
    select(MO, Population_2010, Description) %>%
    mutate(Population_2010 = log(Population_2010)) %>% 
    ggplot(aes(Population_2010)) +
    geom_density(aes(color = MO,
                     fill = MO),
                 alpha = .8,
                 show.legend = T) +
    theme_bw() +
    scale_color_manual(values = colors) +
    scale_fill_manual(values = colors) +
    theme(
        legend.position = 'bottom',
        panel.grid  = element_blank(),
        strip.background = element_blank(),
        panel.border = element_blank()
    ) +
    facet_wrap( ~ Description,scales = 'free')+
    labs(
        title = 'Comparing U.S. and Missouri',
        x = 'Population transformed\nusing natural log',
        y = 'Proportion',
        caption = '*Not adjacent to\nan urban county'
    ) 



density_plot


density_plot_legend <- data_frame(x = 6:12) %>% 
    mutate(y = exp(x)) %>% 
    ggplot(aes(x,y,group = 1))+
    geom_point(color = colors[2],fill = colors[2])+
    geom_line(color = colors[2])+
    theme_bw() +
    scale_x_continuous(breaks = seq(6,16,1))+
    scale_y_continuous(labels = scales::comma_format())+
    theme(axis.line = element_line(color = 'black'),
          legend.position = 'bottom',
          panel.grid  = element_blank(),
          strip.background = element_blank(),
          panel.border = element_blank()
    ) +
    labs(y = 'Population',
         x = 'Log (population)')


density_plot_legend











