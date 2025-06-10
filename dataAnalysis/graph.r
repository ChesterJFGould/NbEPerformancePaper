library(tidyverse)
library(jsonlite)

benchmark_names <- c(
  "asymptotics",
  "conv_eval",
  "stlc100k",
  "stlc10k",
  "stlc5k",
  "stlc_lessimpl10k",
  "stlc_lessimpl5k",
  "stlc_lessimpl",
  "stlc_small10k",
  "stlc_small5k",
  "stlc_small",
  "stlc")


syntax_data <- list_rbind(map(benchmark_names, \(n) data.frame(name = n, type="syntax directed", data = fromJSON(paste("../data/syntax-directed/", n, ".stt.json", sep="")))))

type_data <- list_rbind(map(benchmark_names, \(n) data.frame(name = n, type="type directed", data = fromJSON(paste("../data/type-directed/", n, ".stt.json", sep="")))))

merged_data <- transform(
  merge(syntax_data, type_data, by="name"),
  data.results.mean.y = data.results.mean.y / data.results.mean.x,
  data.results.stddev.x = data.results.stddev.x / data.results.mean.x,
  data.results.stddev.y = data.results.stddev.y / data.results.mean.x,
  data.results.mean.x = 1)

plot <- ggplot(merged_data, aes(name, data.results.mean.x)) +
  geom_col(aes(fill = "syntax-directed"), width = 0.45, position = position_nudge(x = -0.225)) +
  geom_errorbar(aes(ymin = -2*data.results.stddev.x + data.results.mean.x, ymax = 2*data.results.stddev.x + data.results.mean.x), width = 0.45, position = position_nudge(x = -0.225)) +
  geom_col(aes(y = data.results.mean.y, fill = "type-directed"), width = 0.45, position = position_nudge(x = 0.225)) +
  geom_errorbar(aes(ymin = -2*data.results.stddev.y + data.results.mean.y, ymax = 2*data.results.stddev.y + data.results.mean.y), width = 0.45, position = position_nudge(x = 0.225)) +
  theme(text = element_text(size = 20), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(x = "benchmark", y = "relative mean time", fill = "implementation")

normalized_mean_type_time <- mean(filter(merged_data, data.results.mean.y != Inf)$data.results.mean.y)
ggsave("benchmark_graph.png", plot=last_plot())