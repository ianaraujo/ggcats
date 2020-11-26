
ggcats <- function(data, x, y, position = 1, labels = FALSE) {
  if (labels == FALSE) {
    df <- as_tibble(data)
    df %>%
      dplyr::group_by({{x}}, {{y}}) %>%
      dplyr::count() %>%
      dplyr::group_by({{x}}) %>%
      dplyr::mutate(percent = round(n/sum(n), 3)) %>%
      ggplot2::ggplot(aes(x = {{x}}, y = percent, fill = {{y}})) +
      geom_bar(stat = "identity", position = ifelse(position == 1, "dodge", "stack")) +
      scale_y_continuous(labels = scales::percent) -> cat
    return(cat)
  } else {
    if (position == 1) {
      df <- as_tibble(data)
      df %>%
        dplyr::group_by({{x}}, {{y}}) %>%
        dplyr::count() %>%
        dplyr::group_by({{x}}) %>%
        dplyr::mutate(percent = round(n/sum(n), 3)) %>%
        ggplot2::ggplot(aes(x = {{x}}, y = percent, fill = {{y}})) +
        geom_bar(stat = "identity", position = ifelse(position == 1, "dodge", "stack")) +
        geom_text(aes({{x}}, percent, label = scales::percent(percent)),
                  stat="identity", position = position_dodge(0.9), vjust= -0.5, size = 3) +
        scale_y_continuous(labels = scales::percent) -> cat
      return(cat)
    } else {
      df <- as_tibble(data)
      df %>%
        dplyr::group_by({{x}}, {{y}}) %>%
        dplyr::count() %>%
        dplyr::group_by({{x}}) %>%
        dplyr::mutate(percent = round(n/sum(n), 3)) %>%
        ggplot2::ggplot(aes(x = {{x}}, y = percent, fill = {{y}})) +
        geom_bar(stat = "identity", position = ifelse(position == 1, "dodge", "stack")) +
        geom_text(aes(label = scales::percent(percent)),
                  size = 3, position = position_stack(vjust = 0.5)) +
        scale_y_continuous(labels = scales::percent) -> cat
      return(cat)
    }
  }
}
