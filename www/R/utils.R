# this script contains helper functions used in the app

# function to render a bar chart in the background of the cell
# adapted from https://glin.github.io/reactable/articles/cookbook/cookbook.html
bar_style <- function(width = 1, fill = "#c1c1c2", height = "75%",
                      align = c("left", "right"), color = NULL) {
  align <- match.arg(align)
  if (align == "left") {
    position <- paste0(width * 100, "%")
    image <- sprintf("linear-gradient(90deg, %1$s %2$s, transparent %2$s)", fill, position)
  } else {
    position <- paste0(100 - width * 100, "%")
    image <- sprintf("linear-gradient(90deg, transparent %1$s, %2$s %1$s)", position, fill)
  }
  list(
    backgroundImage = image,
    backgroundSize = paste("100%", height),
    backgroundRepeat = "no-repeat",
    backgroundPosition = "center",
    color = color,
    fontFamily = "JetBrains Mono"
  )
}
