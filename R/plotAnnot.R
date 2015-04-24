plotAnnot <- function(LIBS, SCOPE, TITLE) {
  ggplot( mapStats(LIBS, scope=SCOPE)
        , aes( x    = group
             , y    = value
             , fill = variable)
        , main = all) +
    geom_bar(stat="identity") +
    geom_segment(aes( xend = group
                    , y    = ystart
                    , yend = yend)) +
    geom_point( aes( x = group
                   , y = yend)
              , shape = "|"
              , show_guide = FALSE) +
    coord_flip() +
    theme(title=element_text(TITLE))
}

# Inspiration: http://stackoverflow.com/questions/10417003/stacked-barplot-with-errorbars-using-ggplot2
