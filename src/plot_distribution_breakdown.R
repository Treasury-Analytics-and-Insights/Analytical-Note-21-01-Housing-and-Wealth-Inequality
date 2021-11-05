plot_distribution_breakdown <- function(dt, ownership_var, fill_var, fill_var_breaks) {
  p <- ggplot(
    data = dt,
    aes(
      x = Net_Worth_quantile, y = Value,
      fill = get(fill_var)
    )
  ) +
    geom_col(position = "stack") +
    facet_grid(get(ownership_var)~.) +
    scale_y_continuous(
      name = "Households",
      labels = scales::label_comma(accuracy = 1)
    ) +
    scale_x_continuous(
      name = "Total Wealth quantile",
      labels = scales::label_percent(accuracy = 1),
      breaks = scales::pretty_breaks(10)
    ) +
    scale_fill_discrete(
      name = "",
      breaks = fill_var_breaks,
      labels = names(fill_var_breaks)
    ) +
    coord_cartesian(xlim = c(0.0225, 1.0275), expand = FALSE) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "top",
      panel.border = element_rect(color="grey", fill = NA)
    )
  return(p)
}