module TestStuff exposing (quiver)


quiver =
    """
|| quiver
https://d.img.vision/scripta/3471908172-image.png Figure 1
---
%% https://q.uiver.app/?q=WzAsNixbMiwzLCJBIl0sWzQsMywiQiJdLFszLDIsIlUiXSxbMywwLCJYIl0sWzAsMywiUyJdLFs2LDMsIlQiXSxbMiwwLCJwIiwxXSxbMiwxLCJxIiwxXSxbMywwLCJmIiwxLHsiY3VydmUiOjJ9XSxbMywxLCJnIiwxLHsiY3VydmUiOi0yfV0sWzMsMiwibSIsMV0sWzAsNF0sWzEsNV0sWzMsNCwiZSIsMSx7ImN1cnZlIjozfV0sWzMsNSwiaCIsMSx7ImN1cnZlIjotM31dXQ==
\\[\\begin{tikzcd}
\t&&& X \\
\t\\
\t&&& U \\
\tS && A && B && T
\t\\arrow["p"{description}, from=3-4, to=4-3]
\t\\arrow["q"{description}, from=3-4, to=4-5]
\t\\arrow["f"{description}, curve={height=12pt}, from=1-4, to=4-3]
\t\\arrow["g"{description}, curve={height=-12pt}, from=1-4, to=4-5]
\t\\arrow["m"{description}, from=1-4, to=3-4]
\t\\arrow[from=4-3, to=4-1]
\t\\arrow[from=4-5, to=4-7]
\t\\arrow["e"{description}, curve={height=18pt}, from=1-4, to=4-1]
\t\\arrow["h"{description}, curve={height=-18pt}, from=1-4, to=4-7]
\\end{tikzcd}\\]
"""
