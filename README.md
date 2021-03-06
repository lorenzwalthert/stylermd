
# Style your markdown

[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis build
status](https://travis-ci.org/lorenzwalthert/stylermd.svg?branch=master)](https://travis-ci.org/lorenzwalthert/stylermd)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/lorenzwalthert/stylermd?branch=master&svg=true)](https://ci.appveyor.com/project/lorenzwalthert/stylermd)

> Just keep up one rule: 80 characters per line. I promise it will take
> 80% of your time. – <cite>Lorenz Walthert</cite>

The main motivation for creating this package was because when working
with bookdown / blogdown or Rmarkdown in general, I spent **way too much
time** just keeping up the 80 character width rule as every word you add
or remove in a paragraph affects all subsequent lines.

**Functionality**

This package is similar to styler, but it styles non-code elements and
files and it does really just one thing: Making every line at most 80
characters, or a custom line width you can set with the option
`stylermd.line_width`, i.e. `options(stylermd.line_width = 60)`. It
supports:

  - Both adding and removing line breaks for float text, nested
    enumerations and bullet lists.
  - Files with and without code chunks (any extension).
  - Ignoring code chunks and latex equations.
  - Files with YAML Header.
  - Preserves paragraphs.

You can install it from GitHub:

``` r
remotes::install_github("lorenzwalthert/stylermd")
```

**API**

You can use:

  - `tidy_text()`.
  - `tidy_file()`.
  - the RStudio Addins (restarting RStudio after installation). One
    styles the highlighted text, the other styles the active file,
    remembering the cursor position. Set the environment variable
    `save_after_styling` to `TRUE`, for example in your `.Rprofile` to
    enable saving after styling. Hint: Assign a keyboard short-cut for
    the Addin with *Menu -\> Tools -\> Modify Keyboard Shortcuts* for
    most productive use.

**Example**

``` r
library(magrittr)
text <- "1. introduced above. We do this by first creating a style guide with the designated wrapper function `create_style_guide()`. It takes transformer"
stylermd::tidy_text(text) %>%
  cat(sep = "\n")
```

    1. introduced above. We do this by first creating a style guide with the
       designated wrapper function `create_style_guide()`. It takes transformer
