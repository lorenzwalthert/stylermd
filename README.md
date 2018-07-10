
# Style your markdown

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis build
status](https://travis-ci.org/lorenzwalthert/stylermd.svg?branch=master)](https://travis-ci.org/lorenzwalthert/stylermd)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/lorenzwalthert/stylermd?branch=master&svg=true)](https://ci.appveyor.com/project/lorenzwalthert/stylermd)

This package is simiar to styler, but it styles non-code elements and
files and it does really just one thing: Making every line at most 80
characters. It supports:

  - Files with and without code chunks (any extension).
  - Enumerations and bullet list.
  - YAML Header.
  - Preserving paragraphs.

You can install it from GitHub:

``` r
remotes::install_github("lorenzwalthert/stylermd")
```

You can use `tidy_text()`, `tidy_file()` and the RStudio Addin
(restarting RStudio after installation).

``` r
library(magrittr)
text <- "1. introduced above. We do this by first creating a style guide with the designated wrapper function `create_style_guide()`. It takes transformer"
stylermd::tidy_text(text) %>%
  cat(sep = "\n")
```

    1. introduced above. We do this by first creating a style guide with the
       designated wrapper function `create_style_guide()`. It takes transformer
