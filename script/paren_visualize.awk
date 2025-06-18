# Ignore input formatting entirely and visualize LISP structure by indentation only.

# 1. Concatenate all input lines into variable `code` with a single space as separator.
{
    code = code $0 " "
}

# 2. After reading all input lines, start formatting in the END block.
END {
    # Variable to track indentation level
    level = 0
    # String used for one indentation level (two spaces)
    indent_unit = "  "

    # 3. Insert newlines around parentheses so that they are separate tokens
    gsub(/\(/, "\n(\n", code)
    gsub(/\)/, "\n)\n", code)

    # 4. Replace any sequence of whitespace with a single newline so that all tokens
    #    are separated by newlines.
    gsub(/[ \t\r\n]+/, "\n", code)

    # 5. Split `code` into array `tokens` using newline as separator.
    n = split(code, tokens, "\n")

    # 6. Process each token in order.
    for (i = 1; i <= n; i++) {
        token = tokens[i]

        # Skip empty tokens
        if (token == "") {
            continue
        }

        # If we see a closing paren, decrease indentation level first
        if (token == ")") {
            level--
            if (level < 0) level = 0
        }

        # Generate indentation for the current level
        indent = ""
        for (j = 0; j < level; j++) {
            indent = indent indent_unit
        }

        # Print the indented token (no spaces other than indentation)
        print indent token

        # If token is an opening paren, increase the level after printing
        if (token == "(") {
            level++
        }
    }
}
