library(tidyverse)

df <- tibble(
  Days = c(5, 6, 7, 8, 9, 10, 11, 12, 13),
  Runtime = c(70.6, 358.8, 235.4, 427.1, 960.6, 932.6, 4519.8, 5055.5, 28080),
  Type = c("Completed", "Completed", "Completed", "Completed", "Completed", "Completed", "Completed", "Completed",  "Censored")
)

breaks <- 10^(-10:10)
minor_breaks <- rep(1:9, 21)*(10^rep(-10:10, each=9))

png("runtime_study.png", width = 6, height = 4, units = "in", res = 200, type = "cairo")
df %>%
  ggplot(aes(x = Days, y = Runtime, color = Type)) +
  geom_point() +
  geom_smooth(method="glm", method.args=list(family=gaussian(link="log")), se=F, color = "blue") +
  scale_y_log10(breaks=breaks, minor_breaks=minor_breaks) +
  annotation_logticks(sides = "l") +
  theme_bw() +
  labs(
    title = "Gurobi Model Runtime",
    subtitle = "Household #1 runtime in seconds vs. number of planned days",
    x = "Planning Days Value",
    y = "Model Runtime (s)",
    color = element_blank()
  ) +
  scale_color_manual(values=c("red", "black"))
dev.off()

summary(glm(df$Runtime ~ df$Days, family=gaussian(link="log")))

