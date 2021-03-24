#!/usr/bin/env Rscript

box::use(
    dplyr[`%>%`, filter, mutate],
    ggplot2[...],
    tibble[tibble]
)

main = function (args = commandArgs(TRUE)) {
    if (length(args) == 0L || grepl('^--help$|^-h$', args)) {
        box::use(glue[glue])
        cmd = sub('.*=', '', grep('^--file=', commandArgs(), value = TRUE))
        message(glue('Usage:\n\n    {cmd} [--colors|-c colorscheme] filenames...\n\n'))
        return()
    }

    colorscheme = if (length((idx = grep('^--colors$|^-c$', args))) != 0L) {
        colorschemes[[args[idx + 1L]]]
    } else {
        colorschemes$tomorrow
    }
    filenames = args[setdiff(seq_along(args), idx + 0 : 1)]

    box::use(tools)

    for (filename in filenames) {
        p = plot_code_minimap(filename, colorscheme)
        dims = minimap_dim(filename)
        outfile = sprintf('%s.png', tools$file_path_sans_ext(filename))
        ggsave(outfile, p, width = dims[[1L]], height = dims[[2L]])
    }
}

#' Predefined color schemes
#'
#' @format A list of color schemes. Each color scheme in turn is a named list
#' with the names \code{fg} and \code{bg}. \code{fg} is a named vector with
#' colors for the different token types. \code{bg} is a single color value for
#' the background. Colors must be encoded as character strings.
#' @export
colorschemes = list(
    # Colors from <https://github.com/chriskempson/tomorrow-theme>
    tomorrow = list(
        fg = c(
            COMMENT = '#8e908c',
            KEYWORD = '#8959a8',
            PUNCTUATION = '#4d4d4c',
            STR_CONST = '#718c00',
            NUM_CONST = '#f5871f',
            NULL_CONST = '#eab700',
            SYMBOL = '#3e999f',
            CALL = '#4271ae'
        ),
        bg = 'white'
    ),

    tomorrow_night_eighties = list(
        fg = c(
            COMMENT = '#999999',
            KEYWORD = '#cc99cc',
            PUNCTUATION = '#cccccc',
            STR_CONST = '#99cc99',
            NUM_CONST = '#f99157',
            NULL_CONST = '#ffcc66',
            SYMBOL = '#66cccc',
            CALL = '#6699cc'
        ),
        bg = '#2d2d2d'
    )
)

punctuation = c(
    paste0("'", strsplit('()[]{},;+-*/^!~$@:', '')[[1L]], "'"),
    strsplit('EQ NE LT LE GT GE NS_GET NS_GET_INT LBB OR AND OR2 AND2 LEFT_ASSIGN EQ_ASSIGN EQ_FORMALS EQ_SUB', ' ')[[1L]]
)

keywords = strsplit('BREAK ELSE FOR FUNCTION IF IN NEXT REPEAT WHILE', ' ')[[1L]]

symbols = strsplit('SYMBOL SLOT SYMBOL_PACKAGE SYMBOL_FORMALS SYMBOL_SUB', ' ')[[1L]]

replacement_tokens = tibble(
    original = c (
        punctuation, 'SPECIAL',
        keywords,
        symbols,
        'SYMBOL_FUNCTION_CALL',
        'NULL_CONST', 'NUM_CONST', 'STR_CONST',
        'COMMENT'
    ),
    replace_with = c(
        rep('PUNCTUATION', length(punctuation)), 'PUNCTUATION',
        rep('KEYWORD', length(keywords)),
        rep('SYMBOL', length(symbols)),
        'CALL',
        'NULL_CONST', 'NUM_CONST', 'STR_CONST',
        'COMMENT'
    )
)

#' Plot a code minimap
#'
#' \code{plot_code_minimap} plots a \dQuote{minimap} of a given source code
#' file. That is, a pictogram describing the code outline, with syntax
#' highlighting.
#'
#' @param filename [character] the name of an R source file.
#' @param colorscheme [list] a list that describes a color scheme; see
#' \emph{Details}.
#' @return \code{plot_code_minimap} and \code{plot_code_minimap_from_tokens}
#' both return a \code{\link[ggplot2]{ggplot}} object representing the minimap.
#'
#' @details The \code{colorscheme} parameter needs to be a suitably defined
#' color scheme. Inspect \code{colorschemes$tomorrow} for an example.
#' @export
plot_code_minimap = function (
    filename,
    colorscheme = getOption('minimap.default_colors', colorschemes[[1L]])
) {
    plot_code_minimap_from_tokens(tokenize_code(filename), colorscheme)
}

#' Tokenize an R source code file
#'
#' @param filename [character] the name of a file to parse.
#' @return \code{tokenize_code} returns a \code{data.frame} of tokens suitable
#' for use with \code{plot_code_minimap}.
#' @export
tokenize_code = function (filename) {
    box::use(utils[getParseData])

    tokens = parse(filename, keep.source = TRUE) %>%
        getParseData() %>%
        filter(terminal) %>%
        mutate(token = with(replacement_tokens, replace_with[match(token, original)]))

    # We don’t handle tokens spanning multiple lines!
    stopifnot(all(tokens$line1 == tokens$line2))
    stopifnot(! anyNA(tokens$token))
    tokens
}

#' @param tokens [data.frame] the output of \code{\link{tokenize_code}()}.
#' @name plot_code_minimap
#' @export
plot_code_minimap_from_tokens = function (tokens, colorscheme) {
    # Without the `add = 1`, very short code has squished margins.
    expand = expansion(mult = 0.05, add = 1)

    tokens %>%
        ggplot() +
        aes(x = col1 - 0.1, y = line1, xend = col2 + 0.1, yend = line2, color = token) +
        geom_segment(size = 1, lineend = 'round') +
        scale_x_continuous(expand = expand) +
        scale_y_continuous(trans = 'reverse', expand = expand) +
        scale_color_manual(values = colorscheme$fg, guide = FALSE) +
        labs(x = '', y = '') +
        theme_void() +
        theme(panel.background = element_rect(fill = colorscheme$bg))
}

#' @return \code{minimap_dim} returns a numeric vector \code{c(width, height)}
#' containing the optimal dimensions (in inches) of the minimap plot for the
#' given \code{filename}.
#' @name plot_code_minimap
#' @export
minimap_dim = function (filename) {
    code = readLines(filename)
    w = max(vapply(code, nchar, 0L, type = 'width'))
    h = length(code)
    expand = c(mult = 0.05, add = 1)
    # These scaling factors tend to look good.
    scaling = c(0.04, 0.045)

    (c(width = w, height = h) * (1 + expand[1L] * 2) + expand[2L] * 2) * scaling
}

if (is.null(box::name())) invisible(main())

# vim: ft=r
