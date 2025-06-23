# Author: Delphine Mossman
# Date Created: 19 Sept 2024
# Date Last Modified: 23 June 2025

# 1. Load in the data created by the processing script
# 2. Plot total concentration/biomass by day
# 3. Plot concentration/biomass by time and depth
# 4. Plot depth-integrated concentration/biomass by lat/long over the glider track
# 5. Create a plot with an inset of the wind farm lease areas and glider track

#####

rm(list = ls())

library(tidyverse)
library(readxl)
library(hms)
library(sf)
library(sfheaders)
library(lubridate)
library(marmap)
library(rstatix)
library(R.utils)
library(ggpubr)
library(ggnewscale)
library(rnaturalearth)
library(tcltk)
library(cowplot)

sourceDirectory("H:/dm1679/Code/R_Functions", modifiedOnly = F)

glider_dep = choose_directory() %>% substring(., regexpr("ru[0-9]{2}-*", .))
year = substr(glider_dep, 6, 9)

data_dir = paste0("C:/Users/Delphine/Box/Glider Data/",
                  glider_dep,
                  "/Derived Biomass Data/")
figure_dir = paste0("C:/Users/Delphine/Box/Glider Data/", glider_dep, "/Figures/")

world = ne_countries(scale = "medium")
world = world[world$geounit == "United States of America", ]

load(paste0(data_dir, "Processed_Abundance_Biomass_Data.rda"))
load(paste0(data_dir, "Glider_Data.rda"))
load(paste0(data_dir, "Peripheral_Data.rda"))

#####

## 3 frequency plots
# for (i in 1:length(data_ldf)) {
#   df = data_ldf[[i]]
#   if (nrow(df) == 0) {
#     next
#   } else {
#     # if (i < 7) {
#       df$Echo_Num = rep(1:(nrow(df) / 3), times = 3)
#     # } else {
#     #   df$Echo_Num = rep(1:(nrow(df) / 2), times = 2)
#     # }
#     
#     
#     df2 = df %>% group_by(Echo_Num) %>% reframe(
#       freq = as.factor(Frequency),
#       spec = Species,
#       scat_vol = Sv_mean,
#       freq_response = log10(10^(Sv_mean / 10) / 10^(Sv_mean[1] / 10))
#     )
#     
#     plot1 = ggplot(data = df2, aes(
#       x = freq,
#       y = scat_vol,
#       group = Echo_Num,
#       color = spec
#     )) +
#       geom_line() +
#       scale_x_discrete(expand = c(0, 0)) +
#       labs(x = "Frequency", y = "Mean Sv (dB)", color = "Species") +
#       theme_bw()
#     
#     fname = paste0(
#       figure_dir,
#       '/Sv By Frequency/',
#       str_sub(data_filenames[i], -30, -18),
#       "_Sv_By_Frequency_Curves.png"
#     )
#     
#     ggsave(plot1, filename = fname, scale = 2)
#     
#     plot2 = ggplot(data = df2,
#                    aes(
#                      x = freq,
#                      y = freq_response,
#                      group = Echo_Num,
#                      color = spec
#                    )) +
#       geom_line() +
#       scale_x_discrete(expand = c(0, 0)) +
#       labs(x = "Frequency", y = "log10 of r(f)", color = "Species") +
#       theme_bw()
#     
#     fname = paste0(
#       figure_dir,
#       '/Frequency Response/',
#       str_sub(data_filenames[i], -30, -18),
#       "_Frequency_Response_Curves.png"
#     )
#     
#     ggsave(plot2, filename = fname, scale = 2)
#     
#     # if (i < 7) {
#       df3 = df %>% group_by(Echo_Num) %>% reframe(
#         Sv_38 = Sv_mean[1],
#         Sv_125 = Sv_mean[2],
#         Sv_200 = Sv_mean[3],
#         spec = Species
#       )
#     # } else {
#     #   df3 = df %>% group_by(Echo_Num) %>% reframe(Sv_38 = Sv_mean[1],
#     #                                               Sv_125 = Sv_mean[2],
#     #                                               spec = Species)
#     # }
#     
#     plot3 = ggplot(data = df3, aes(
#       x = Sv_38,
#       y = Sv_125,
#       group = spec,
#       color = spec
#     )) +
#       geom_point() +
#       labs(x = "Sv (38 kHz)", y = "Sv (125 kHz)", color = "Species") +
#       xlim(-115, -10) + ylim(-115, -10) +
#       theme_bw()
#     
#     fname = paste0(
#       figure_dir,
#       '/Sv Scatterplots/',
#       str_sub(data_filenames[i], -30, -18),
#       "_38_125_Sv_Scatter.png"
#     )
#     
#     ggsave(plot3, filename = fname, scale = 2)
#     
#     # if (i < 7) {
#       plot4 = ggplot(data = df3, aes(
#         x = Sv_125,
#         y = Sv_200,
#         group = spec,
#         color = spec
#       )) +
#         geom_point() +
#         labs(x = "Sv (125 kHz)", y = "Sv (200 kHz)", color = "Species") +
#         xlim(-115, -10) + ylim(-115, -10) +
#         theme_bw()
#       
#       fname = paste0(
#         figure_dir,
#         '/Sv Scatterplots/',
#         str_sub(data_filenames[i], -30, -18),
#         "_125_200_Sv_Scatter.png"
#       )
#       
#       ggsave(plot4, filename = fname, scale = 2)
#       
#       plot5 = ggplot(data = df3, aes(
#         x = Sv_38,
#         y = Sv_200,
#         group = spec,
#         color = spec
#       )) +
#         geom_point() +
#         labs(x = "Sv (38 kHz)", y = "Sv (200 kHz)", color = "Species") +
#         xlim(-115, -10) + ylim(-115, -10) +
#         theme_bw()
#       
#       fname = paste0(
#         figure_dir,
#         '/Sv Scatterplots/',
#         str_sub(data_filenames[i], -30, -18),
#         "_38_200_Sv_Scatter.png"
#       )
#       
#       ggsave(plot5, filename = fname, scale = 2)
#     # }
#   }
# }

## 4 frequency plots
for (i in 1:length(data_ldf)) {
  df = data_ldf[[i]]
  if (nrow(df) == 0)
    next
  else {
    df$Echo_Num = rep(1:(nrow(df) / 4), times = 4)


    df2 = df %>% group_by(Echo_Num) %>% reframe(
      freq = as.factor(Frequency),
      spec = Species,
      scat_vol = Sv_mean,
      freq_response = log10(10 ^
                              (Sv_mean / 10) / 10 ^ (Sv_mean[1] / 10))
    )

    plot1 = ggplot(data = df2,
                   aes(
                     x = freq,
                     y = scat_vol,
                     group = Echo_Num,
                     color = spec
                   )) +
      geom_line() +
      scale_x_discrete(expand = c(0, 0)) +
      labs(x = "Frequency", y = "Mean Sv (dB)", color = "Species") +
      theme_bw()

    fname = paste0(
      figure_dir,
      '/Sv By Frequency/',
      str_sub(data_filenames[i],-30,-18),
      "_Sv_By_Frequency_Curves.png"
    )

    ggsave(filename = fname, scale = 2)

    plot2 = ggplot(data = df2,
                   aes(
                     x = freq,
                     y = freq_response,
                     group = Echo_Num,
                     color = spec
                   )) +
      geom_line() +
      scale_x_discrete(expand = c(0, 0)) +
      labs(x = "Frequency", y = "log10 of r(f)", color = "Species") +
      theme_bw()

    fname = paste0(
      figure_dir,
      '/Frequency Response/',
      str_sub(data_filenames[i],-30,-18),
      "_Frequency_Response_Curves.png"
    )

    ggsave(filename = fname, scale = 2)

    df3 = df %>% group_by(Echo_Num) %>% reframe(
      Sv_120 = Sv_mean[1],
      Sv_200 = Sv_mean[2],
      Sv_455 = Sv_mean[3],
      Sv_769 = Sv_mean[4],
      spec = Species
    )

    plot3 = ggplot(data = df3, aes(
      x = Sv_120,
      y = Sv_200,
      group = spec,
      color = spec
    )) +
      geom_point() +
      labs(x = "Sv (120 kHz)", y = "Sv (200 kHz)", color = "Species") +
      xlim(-135, -55) + ylim(-135, -55) +
      theme_bw()

    fname = paste0(
      figure_dir,
      '/Sv Scatterplots/',
      str_sub(data_filenames[i], -30, -18),
      "_120_200_Sv_Scatter.png"
    )

    ggsave(filename = fname, scale = 2)

    plot4 = ggplot(data = df3, aes(
      x = Sv_200,
      y = Sv_455,
      group = spec,
      color = spec
    )) +
      geom_point() +
      labs(x = "Sv (200 kHz)", y = "Sv (455 kHz)", color = "Species") +
      xlim(-135, -55) + ylim(-135, -55) +
      theme_bw()

    fname = paste0(
      figure_dir,
      '/Sv Scatterplots/',
      str_sub(data_filenames[i], -30, -18),
      "_200_455_Sv_Scatter.png"
    )

    ggsave(filename = fname, scale = 2)

    plot5 = ggplot(data = df3, aes(
      x = Sv_455,
      y = Sv_769,
      group = spec,
      color = spec
    )) +
      geom_point() +
      labs(x = "Sv (455 kHz)", y = "Sv (769 kHz)", color = "Species") +
      xlim(-135, -55) + ylim(-135, -55) +
      theme_bw()

    fname = paste0(
      figure_dir,
      '/Sv Scatterplots/',
      str_sub(data_filenames[i], -30, -18),
      "_455_769_Sv_Scatter.png"
    )

    ggsave(filename = fname, scale = 2)
  }
}

#####
# manova_data = data %>%
#   filter(month(data$Time_M) == 8 & day(data$Time_M) < 23) %>%
#   group_by(Echo_Num) %>%
#   reframe(
#     Species = Species[1],
#     Sv_38 = Sv_mean[1],
#     Sv_125 = Sv_mean[2],
#     Sv_200 = Sv_mean[3],
#     sv_38 = 10^(Sv_mean[1] / 10),
#     sv_125 = 10^(Sv_mean[2] / 10),
#     sv_200 = 10^(Sv_mean[3] / 10)
#   ) %>%
#   mutate(
#     resp_125 = sv_125 / sv_38,
#     resp_200 = sv_200 / sv_38,
#     diff_125 = Sv_125 - Sv_38,
#     diff_200 = Sv_200 - Sv_125
#   )
# 
# manova_data %>%
#   group_by(Species) %>%
#   summarise(
#     mean(resp_125, na.rm = T),
#     mean(resp_200, na.rm = T),
#     mean(diff_125, na.rm = T),
#     mean(diff_200, na.rm = T)
#   )
# 
# Manova(lm(cbind(resp_125, resp_200, diff_125, diff_200) ~ Species, data = manova_data), test.statistic = "Pillai")
# 
# manova_data %>%
#   na.omit() %>%
#   gather(key = "variable",
#          value = "value",
#          resp_125,
#          resp_200,
#          diff_125,
#          diff_200) %>%
#   group_by(variable) %>%
#   anova_test(value ~ Species)
# 
# manova_data %>%
#   na.omit() %>%
#   gather(key = "variables",
#          value = "value",
#          resp_125,
#          resp_200,
#          diff_125,
#          diff_200) %>%
#   group_by(variables) %>%
#   games_howell_test(value ~ Species) %>%
#   select(-estimate, -conf.low, -conf.high) %>%
#   print(n = 40)

#####
# Concentration and biomass bar graphs by day

Concentration_By_Date = ggplot(data = data2, aes(
  x = Species,
  y = Abundance,
  group = Date,
  fill = Date
)) +
  geom_col(position = "dodge") +
  scale_fill_viridis_d() +
  labs(y = expression(paste(
    "Log10 of Total Concentration (individuals/m"^"3" * ")"
  )))

fname = paste0(figure_dir, "Concentration_By_Date.png")

ggsave(Concentration_By_Date, filename = fname, scale = 2)

Biomass_By_Date = ggplot(data = data2, aes(
  x = Species,
  y = Biomass,
  group = Date,
  fill = Date
)) +
  geom_col(position = "dodge") +
  scale_fill_viridis_d() +
  labs(y = expression(paste("Log10 of Total Biomass (g/m"^"3" * ")")))

fname = paste0(figure_dir, "Biomass_By_Date.png")

ggsave(Biomass_By_Date, filename = fname, scale = 2)

#####

# Presence/absence, concentration, and biomass plots by depth and time

Presence_Absence_Bubble_Time = ggplot() +
  geom_tile(
    data = day_data,
    aes(
      x = Date,
      y = median(data3$Depth),
      fill = TOD
    ),
    alpha = 0.2,
    height = 400
  ) +
  geom_tile(
    data = gdata,
    aes(
      x = time,
      y = median(data3$Depth),
      fill = Wind_Farm
    ),
    alpha = 0.2,
    height = 400
  ) +
  scale_fill_manual(
    values = c("white", "white", "black", "red"),
    labels = c("Day", "Outside", "Night", "Inside")
  ) +
  geom_point(
    data = data3[data3$Species != "Empty Cell",],
    inherit.aes = F,
    aes(x = Date, y = Depth, color = Species),
    alpha = 0.7,
    size = 2.5
  ) +
  scale_color_viridis_d(begin = 0.2, end = 0.9, direction = -1) +
  # scale_color_manual(values=c("#BCDF2A","#424242")) +
  scale_y_reverse() +
  # geom_vline(
  #   xintercept = ISOdatetime(2023, 8, 22, 20, 0, 0),
  #   show.legend = F,
  #   color = "red",
  #   linetype = 2,
  #   linewidth = 1
  # ) +
  geom_smooth(
    data = closest,
    aes(x = Time, y = Depth),
    method = "loess",
    span = 0.15,
    color = "black",
    se = F,
  ) +
  theme_bw() +
  labs(y = "Depth", fill = "Time of Day") +
  coord_cartesian(expand = FALSE, ylim = c(max(data3$Depth) + 5, 0))
# xlim=c(as.POSIXct("2024-03-01 02:00:00", tz="EST"),
#        as.POSIXct("2024-03-01 04:30:00", tz="EST")))

fname = paste0(figure_dir, "Presence_Absence_Bubble_Plot.png")

ggsave(Presence_Absence_Bubble_Time,
       filename = fname,
       scale = 2)

Biomass_Bubble_Time = ggplot() +
  geom_tile(
    data = day_data,
    aes(
      x = Date,
      y = median(data3$Depth),
      fill = TOD
    ),
    alpha = 0.2,
    height = 400
  ) +
  geom_tile(
    data = gdata,
    aes(
      x = time,
      y = median(data3$Depth),
      fill = Wind_Farm
    ),
    alpha = 0.2,
    height = 400
  ) +
  scale_fill_manual(
    values = c("white", "white", "black", "red"),
    labels = c("Day", "Outside", "Night", "Inside")
  ) +
  geom_point(
    data = data3[!(data3$Species %in% c("Empty Cell", "Gelatinous Zooplankton")),],
    inherit.aes = F,
    aes(
      x = Date,
      y = Depth,
      size = Biomass,
      color = Species
    ),
    alpha = 0.7
  ) +
  # geom_point(data = data3[data3$Species %in% c("Menhaden","Unknown (potentially copepods)"),], inherit.aes=F, aes(x=Date,y=Depth,size=log10(Biomass),color=Species),alpha = 0.7) +
  # geom_point(data = data3[data3$Species == "Gelatinous Zooplankton",], inherit.aes = F, aes(x=Date, y=Depth, color=Species),alpha = 0.7,size=2.5) +
  scale_y_reverse() +
  scale_size_binned(
    range = c(1, 10),
    limits = c(0, 5),
    breaks = seq(0, 5, 0.5)
  ) +
  scale_color_viridis_d(begin = 0.2, end = 0.9, guide = "none") +
  # scale_color_manual(values=c("#BCDF2A","#424242")) +
  geom_vline(
    xintercept = ISOdatetime(2023, 8, 22, 20, 0, 0),
    show.legend = F,
    color = "red",
    linetype = 2,
    linewidth = 1
  ) +
  geom_smooth(
    data = closest,
    aes(x = Time, y = Depth),
    method = "loess",
    span = 0.15,
    color = "black",
    se = F
  ) +
  theme_bw() +
  labs(size = expression("Large"~"Copepod"~"Biomass"~"(g/m"^"3"*")"),
       y = "Depth",
       fill = "Time of Day") +
  coord_cartesian(expand = FALSE, ylim = c(max(data3$Depth) + 5, 0)) +
  theme(text = element_text(size = 16))

fname = paste0(figure_dir, "Biomass_Bubble_Plot.png")

ggsave(Biomass_Bubble_Time, filename = fname, scale = 2)

Concentration_Bubble_Time = ggplot() +
  geom_tile(
    data = day_data,
    aes(
      x = Date,
      y = median(data3$Depth),
      fill = TOD
    ),
    alpha = 0.2,
    height = 400
  ) +
  geom_tile(
    data = gdata,
    aes(
      x = time,
      y = median(data3$Depth),
      fill = Wind_Farm
    ),
    alpha = 0.2,
    height = 400
  ) +
  scale_fill_manual(
    values = c("white", "white", "black", "red"),
    labels = c("Day", "Outside", "Night", "Inside")
  ) +
  geom_point(
    data = data3[!(data3$Species %in% c("Empty Cell", "Gelatinous Zooplankton")),],
    inherit.aes = F,
    aes(
      x = Date,
      y = Depth,
      size = log10(Abundance),
      color = Species
    ),
    alpha = 0.7
  ) +
  # scale_size_continuous(limits = c(-1, 6), breaks = seq(0, 5, 1)) +
  # geom_point(data = data3[data3$Species %in% c("Menhaden","Unknown (potentially copepods)"),], inherit.aes=F, aes(x=Date,y=Depth,size=log10(Abundance),color=Species),alpha = 0.7) +
  # geom_point(data = data3[data3$Species == "Gelatinous Zooplankton",], inherit.aes=F, aes(x=Date,y=Depth,color=Species),alpha = 0.7,size=2.5) +
  scale_y_reverse() +
  # scale_radius(breaks=c(-2,-1,0,1,2)) +
  # scale_color_manual(values=c("#BCDF2A","#424242")) +
  scale_color_viridis_d(begin = 0.2, end = 0.9, guide="none") +
  geom_vline(
    xintercept = ISOdatetime(2023, 8, 22, 20, 0, 0),
    show.legend = F,
    color = "red",
    linetype = 2,
    linewidth = 1
  ) +
  # geom_smooth(data = data3, aes(x = Date, y = Seafloor_Depth),method="loess",span=0.15,color="black",se=F) +
  geom_smooth(
    data = closest,
    aes(x = Time, y = Depth),
    method = "loess",
    span = 0.15,
    color = "black",
    se = F
  ) +
  theme_bw() +
  labs(
    size = expression(paste(
      "log10 of Large Copepod \nConcentration (individuals/m"^"3" * ")"
    )),
    y = "Depth",
    # x = "March 1st, 2024",
    fill = "Time of Day"
  ) +
  coord_cartesian(
    expand = FALSE,
    ylim = c(max(data3$Depth + 5), 0)
    # xlim = c(
    #   as.POSIXct("2024-03-02 23:00:00", tz = "EST"),
    #   as.POSIXct("2024-03-02 23:30:00", tz = "EST")
    # )
  ) +
  theme(text = element_text(size = 16))

fname = paste0(figure_dir, "Concentration_Bubble_Plot.png")

ggsave(Concentration_Bubble_Time,
       filename = fname,
       scale = 2)

#####

# Depth-integrated plots along the glider track

xlim = c(-75, -72.5)
ylim = c(38.5, 41.5)

## Isobath lines for RMI project proposal
#bathy_35m = bathy %>% filter(y < 41) %>% filter(z < 36 & z > 34)
#bathy_50m = bathy %>% filter(z < 51 & z > 49)

# bathy_35m_line = bathy2 %>% filter(z < 36 & z > 34)  %>%
#   summarize() %>%
#   st_cast("LINESTRING")
# bathy_50m_line = bathy2 %>% filter(z < 51 & z > 49) %>%
#   summarize() %>%
#   st_cast("LINESTRING")
#
# g_coords_line = g_coords  %>%
#   summarize() %>%
#   st_cast("LINESTRING")
#
# Intersection_35m = st_intersection(bathy_35m_line,g_coords_line) %>%
#   st_cast("POINT") %>%
#   sf_to_df() %>%
#   filter(x > -73.6 & x < -73.4 & y > 39.45 & y < 39.55)
# Intersection_50m = st_intersection(bathy_50m_line,g_coords_line) %>%
#   st_cast("POINT") %>%
#   sf_to_df() %>%
#   filter(x > -73.5 & x < -73.0 & y > 39 & y < 39.5)

Biomass_Bubble_Map = ggplot() +
  geom_contour_filled(
    data = bathy,
    aes(x = x, y = y, z = z),
    colour = NA,
    breaks = c(seq(
      from = 0, to = 100, by = 10
    )),
    show.legend = F
  ) +
  scale_fill_grey(start = 0.9, end = 0.3) +
  geom_sf(data = world, fill = "gray15") +
  geom_sf(
    data = Study_Area_Final,
    color = "black",
    fill = "red",
    alpha = 0.5
  ) +
  # geom_path(data=bathy_35m, aes(x = x, y = y, linetype = "35m Isobath"),linewidth = 0.5) +
  # geom_path(data=bathy_50m, aes(x = x, y = y, linetype = "50m Isobath"),linewidth = 0.5) +
  # scale_linetype_manual("Isobath Lines",breaks=c("35m Isobath","50m Isobath"),
  #                       values = c(2, 3)) +
  # annotate("point", x=-72.68182, y = 40.610171666667, shape = 8, color="blue") +
  # annotate("text", x=-72.68182, y = 40.610171666667, label = "NYB35", vjust=-1.6, hjust=0.5, color="blue", size=3, fontface="bold") +
  # annotate("point", x=-72.6365866666667, y = 40.471816666667, shape = 8, color="purple") +
  # annotate("text", x=-72.365866666667, y = 40.471816666667, label = "NYB50", vjust=2.5, hjust=1.25, color="purple", size=3, fontface="bold") +
  geom_sf(data = g_coords, size = 0.4) +
  # geom_sf(data = g_SA_intersect,size=0.5,color="blue") +
  geom_sf(data = data4[!(data4$Species %in% c("Empty Cell", "Gelatinous Zooplankton")),],
          aes(size = D_Int_Biomass, color = Species),
          alpha = 0.6) +
  scale_color_viridis_d(begin = 0.2, end = 0.9, guide="none") +
  scale_size_binned(
    range = c(1, 10),
    limits = c(0, 2),
    breaks = seq(0, 2, 0.25)
  ) +
  # annotate("point", x = -73.48, y = 39.45, color="green", shape=15, size=2) + # 35 m isobath buoy
  # annotate("point", x = -73.24, y = 39.47, color="green", shape=15, size=2) + # 50 m isobath buoy
  theme_bw() +
  coord_sf(crs = st_crs(g_coords),
           xlim = xlim,
           ylim = ylim) +
  labs(x = "Longitude", y = "Latitude", size = bquote(atop(
    Depth - Integrated ~ Large ~ phantom(), Copepod ~ Biomass ~ (g / m^2)
  ))) +
  theme(text = element_text(size = 16), legend.title = element_text(hjust = 0))

fname = paste0(figure_dir, 'Glider_Path_Biomass_Map.png')
ggsave(Biomass_Bubble_Map, file = fname, scale = 2)

Concentration_Bubble_Map = ggplot() +
  geom_contour_filled(
    data = bathy,
    aes(x = x, y = y, z = z),
    colour = NA,
    breaks = c(seq(
      from = 0, to = 100, by = 10
    )),
    show.legend = F
  ) +
  scale_fill_grey(start = 0.9, end = 0.3) +
  geom_sf(data = world, fill = "gray15") +
  geom_sf(
    data = Study_Area_Final,
    color = "black",
    fill = "red",
    alpha = 0.5
  ) +
  geom_sf(data = g_coords, size = 0.4) +
  # geom_sf(data = g_SA_intersect,size=0.5,color="blue") +
  geom_sf(data = data4[!(data4$Species %in% c("Empty Cell", "Gelatinous Zooplankton")),],
          aes(size = log10(D_Int_Abundance), color = Species),
          alpha = 0.6) +
  scale_color_viridis_d(begin = 0.2, end = 0.9, guide="none") +
  scale_size_binned(
    limits = c(0.5, 4),
    breaks = seq(0.5, 4, 0.5),
    range = c(0, 8)
  ) +
  theme_bw() +
  coord_sf(crs = st_crs(g_coords),
           xlim = xlim,
           ylim = ylim) +
  labs(x = "Longitude",
       y = "Latitude",
       size = expression(
         paste(
           "Log10 Depth-Integrated Large\nCopepod Concentration (individuals/m"^"2" * ")"
         )
       ))

fname = paste0(figure_dir, 'Glider_Path_Concentration_Map.png')
ggsave(Concentration_Bubble_Map, file = fname, scale = 2)

#####

# Depth-integrated plots along the glider track with marine mammal detections
if(exists("marine_mammal_detections")) {
marine_mammal_detections = marine_mammal_detections %>%
  filter(between(datetime_utc, day_data$Date[1], day_data$Date[nrow(day_data)])) # %>%
# filter(Whale_Species %in% c("Sei","North Atlantic Right"))

# whale_colors = setNames(c("#18D6CB","#E1DD37","#C42503"),c("Fin","Humpback","North Atlantic Right"))
whale_colors = setNames(
  c("#FC14AC", "#BD00FD", "#F89441", "#18D6CB"),
  c("Fin", "Humpback", "North Atlantic Right", "Sei")
)

Biomass_Bubble_Map_Whales = Biomass_Bubble_Map +
  new_scale_color() +
  geom_sf(
    inherit.aes = F,
    data = marine_mammal_detections,
    aes(color = Whale_Species, group = Whale_Species),
    stroke = 0.5,
    size = 2,
    shape = 4
  ) +
  scale_color_manual(values = whale_colors) +
  coord_sf(crs = st_crs(g_coords),
           xlim = xlim,
           ylim = ylim) +
  labs(color = "Whale Detection") +
  guides(color = guide_legend(order = 1)) +
  theme(text = element_text(size = 16)) +
  guides(colour = guide_legend(override.aes = list(size = 3), position = "top"))
fname = paste0(figure_dir, 'Glider_Path_Biomass_Whales_Map.png')
ggsave(file = fname, scale = 2)

Concentration_Bubble_Map_Whales = Concentration_Bubble_Map +
  new_scale_color() +
  geom_sf(
    inherit.aes = F,
    data = marine_mammal_detections,
    aes(color = Whale_Species, group = Whale_Species),
    stroke = 0.5,
    size = 2,
    shape = 4
  ) +
  scale_color_manual(values = whale_colors) +
  coord_sf(crs = st_crs(g_coords),
           xlim = xlim,
           ylim = ylim) +
  labs(color = "Whale Detection") +
  guides(color = guide_legend(order = 1)) +
  guides(colour = guide_legend(override.aes = list(size = 3)))
fname = paste0(figure_dir, 'Glider_Path_Concentration_Whales_Map.png')
ggsave(file = fname, scale = 2)
}
#####

# Code to make presence/absence plot with wind farm map inset

xlim = c(-75, -72.5)
ylim = c(38.5, 41.5)

Wind_Farm_Map = ggplot() +
  geom_contour_filled(
    data = bathy,
    aes(x = x, y = y, z = z),
    colour = NA,
    breaks = c(seq(
      from = 0, to = 100, by = 10
    )),
    show.legend = F
  ) +
  scale_fill_grey(start = 0.9, end = 0.3) +
  geom_sf(data = world, fill = "gray15") +
  geom_sf(
    data = Study_Area_Final,
    color = "black",
    fill = "red",
    alpha = 0.5
  ) +
  geom_sf(data = g_coords, size = 0.5) +
  geom_sf(data = g_SA_intersect,
          size = 0.5,
          color = "blue") +
  coord_sf(crs = st_crs(g_coords),
           xlim = xlim,
           ylim = ylim) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    rect = element_blank(),
    plot.background = element_rect(fill = "white"),
    panel.border = element_rect(color = "black", linewidth = 1)
  )
fname = paste0(figure_dir, 'Wind_Farm_Areas_Map.png')
ggsave(Wind_Farm_Map, file = fname, scale = 2)

ggdraw() +
  draw_plot(Presence_Absence_Bubble_Time) +
  draw_plot(
    Wind_Farm_Map,
    x = 0.65,
    y = 0.08,
    width = 0.3,
    height = 0.3
  )

#####

# Plots with depth/shelf-type as a factor

xlim = c(-75, -72.5)
ylim = c(38, 41)

ggplot() +
  geom_contour_filled(
    data = bathy,
    aes(x = x, y = y, z = z),
    colour = NA,
    breaks = c(seq(
      from = 0, to = 100, by = 5
    )),
    show.legend = F
  ) +
  scale_fill_grey(start = 0.9, end = 0.3) +
  geom_sf(data = world, fill = "gray15") +
  geom_sf(
    data = Study_Area_Final,
    color = "black",
    fill = "red",
    alpha = 0.5
  ) +
  geom_sf(data = g_coords,
          size = 0.5,
          aes(group = Depth_Type, color = Depth_Type)) +
  scale_color_viridis_d(option = "B", end = 0.8) +
  coord_sf(crs = st_crs(g_coords),
           xlim = xlim,
           ylim = ylim) +
  labs(x = "Longitude", y = "Latitude", color = "Depth Designation")
fname = paste0(figure_dir, "Glider_Track_Bathymetry.png")
ggsave(file = fname, scale = 2)

xlim = c(-75, -72.5)
ylim = c(38, 41)

ggplot() +
  geom_contour_filled(
    data = bathy,
    aes(x = x, y = y, z = z),
    colour = NA,
    breaks = c(seq(
      from = 0, to = 100, by = 5
    )),
    show.legend = F
  ) +
  scale_fill_grey(start = 0.9, end = 0.3) +
  geom_sf(data = world,
          show.legend = F,
          fill = "gray15") +
  theme_bw() +
  geom_sf(data = g_coords,
          size = 0.5,
          aes(group = Shelf_Type, color = Shelf_Type)) +
  scale_color_viridis_d(
    option = "B",
    end = 0.8,
    breaks = c("Inshore", "Midshelf", "Offshore")
  ) +
  guides(colour = guide_legend(override.aes = list(size = 3))) +
  labs(
    x = "Longitude",
    y = "Latitude",
    color = "Shelf Type Designation",
    size = expression(
      paste(
        "Log10 of Depth-Integrated\nConcentration (individuals/m"^"2" * ")"
      )
    )
  ) +
  new_scale_color() +
  geom_sf(data = data4,
          alpha = 0.5,
          aes(
            group = Species,
            color = Species,
            size = log10(D_Int_Abundance)
          )) +
  scale_color_viridis_d(begin = 0.2, end = 0.9) +
  scale_size_binned(
    limits = c(2, 5),
    breaks = seq(2, 5, 0.5),
    range = c(0, 8)
  ) +
  guides(colour = guide_legend(override.aes = list(size = 3))) +
  labs(color = "Species") +
  coord_sf(crs = st_crs(g_coords),
           xlim = xlim,
           ylim = ylim)
fname = paste0(figure_dir, "Glider_Shelf_Type_Concentration_Plot.png")
ggsave(file = fname, scale = 2)

#####
## Zooplankton dB window overlap visualization

# Cladocerans: primarily freshwater, P. avirostris is oceanic
# Larvaceans: tunicates, oceanic

# zoop_windows = data.frame(
#   Species = c(
#     "Small Copepods",
#     "Large Copepods",
#     #"Cladocerans",
#     "Gelatinous Zooplankton"
#   ),
#   Lower_dB_Bound = c(10.1,
#                      14.1,
#                      #10.5,
#                      -0.93),
#   Upper_dB_Bound = c(13.8,
#                      14.6,
#                      #13.8,
#                      8.23)
# )
#
# ggplot(data = zoop_windows) +
#   geom_crossbar(
#     aes(
#       y = Species,
#       x = Lower_dB_Bound,
#       xmin = Lower_dB_Bound,
#       xmax = Upper_dB_Bound,
#       group = Species,
#       fill = Species
#     ),
#     color = NA
#   ) +
#   geom_vline(
#     aes(xintercept = Lower_dB_Bound, color = Species),
#     linewidth = 1,
#     linetype = 2
#   ) +
#   geom_vline(
#     aes(xintercept = Upper_dB_Bound, color = Species),
#     linewidth = 1,
#     linetype = 3
#   ) +
#   scale_fill_viridis_d(option = "H") + scale_color_viridis_d(option = "H") +
#   scale_y_discrete(limits = rev) +
#   theme_bw() +
#   labs(x = "dB Window (Sv_455 kHz minus Sv_200 kHz)")
#
# fname = "C:/Users/Delphine/Box/Glider Data/Zooplankton_dB_Window_Figure.png"
# ggsave(file = fname, scale = 2)
