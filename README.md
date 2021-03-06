<!-- README.md is generated from README.Rmd. Please edit that file -->
ggverbs
=======

Constructing a plot using the **ggplot2** package is like adding a bunch of things together. Right? Except the verb "add" isn't a particularly good verb for some of the things we use the `+` symbol for. Consider the following example:

``` r
ggplot(mtcars, aes(wt, mpg, col = disp)) + 
  geom_point() +
  scale_colour_gradient(low = "black", high = "white") +
  labs(x = "Weight")
```

![](README-plot-ggplot-1.png)

In the above code, `+ geom_point()` **adds** a layer to the plot, `+ scale_colour_gradient(low = "black", high = "white")` **replaces** (or **sets**) the colour scale, and `+ labs(x = "Weight")` **updates** the current set of labels. This package transforms the multitude of element constructors (currently nouns) into verbs that describe what they do to the plot. This has the added benefit of using the pipe (`%>%`) rather than the `+` operator to construct a plot, without masking any functions exported by **ggplot2**.

``` r
library(ggverbs)
ggplot(mtcars, aes(wt, mpg, col = disp)) %>%
  add_geom_point() %>%
  set_scale_colour_gradient(low = "black", high = "white") %>%
  update_labs(x = "Weight")
```

![](README-plot-ggverb-1.png)

Installation
------------

Install from github:

``` r
# install.packages("devtools")
devtools::install_github("paleolimbot/ggverbs")
```

Details
-------

The **ggverbs** package doesn't actually define any functions. Instead, it uses whatever the currently installed version of **ggplot2** exports, and uses a couple of regular expressions to change nouns into verbs. This has the advantage of not depending on any particular version of **ggplot2**, and because the functions are created on namespace load, it isn't bothered by the user updating **ggplot2**, and doesn't care if you have either package attached. The list of regexes looks something like this:

``` r
verbs_regexes <- c(
    "^aes_?$" = "update",
    "^aes_(string|q)$" = "update",
    "^layer$"  = "add",
    "^(geom|stat|annotation)_" = "add",
    "^scale_[a-z0-9]+_[a-z0-9]+$" = "set",
    "^theme_(?!get|set|update|replace)[a-z]+$" = "set",
    "^theme$" = "update",
    "^coord_(?!munch)[a-z]+$" = "set",
    "^facet_" = "set",
    "^labs$" = "update",
    "^guides$" = "update"
  )
```

Verbifying noun functions currently takes the strategy of modifying the call to the verb function to remove the `.plot` argument and pass on all the others. This has the advantage of keeping the autocomplete of arguments, although there is still no way to use R's help system with this approach.

Disclaimer
----------

This package is mostly a concept, and is completely untested. Use it for entertainment purposes only!
