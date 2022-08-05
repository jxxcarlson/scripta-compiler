module Text exposing (microLaTeXDemo, l0Demo, xMarkdown, info)


info = """
| title
About the Scripta compiler

| banner
[link Scripta.io https://scripta.io]

| contents


"""

microLaTeXDemo = """
\\title{Demo (MicroLaTeX)}

| banner
\\link{Scrpta.io https://scripta.io}

\\contents

\\section{Images}

\\image{https://see.news/wp-content/uploads/2020/12/UK_wildbirds-01-robin.jpg}

\\section{Math}

Pythagoras says: $a^2 + b^2 = c^2$

From calculus:

$$
\\int_0^1 x^n dx = \\frac{1}{n+1}
$$

"""


l0Demo = """
| title
Demo (L0)

| banner
[link Scripta.io https://scripta.io]

| contents

| section 1
Images

[image https://nas-national-prod.s3.amazonaws.com/styles/hero_image/s3/web_h_apa_2016-a1_2474_8_cedar-waxwing_peter_brannon_kk_female.jpg?itok=VdeVVmGA]

| section 1
Math

Pythagoras says: $a^2 + b^2 = c^2$

From calculus:

$$
\\int_0^1 x^n dx = \\frac{1}{n+1}
$$

"""

xMarkdown = """
| title
Demo (XMarkdown)

| banner
[Scripta.io](https://scripta.io)

| contents

# Images

![Yellow bird](https://i.ibb.co/XFzZYby/image.png)

# Math

Pythagoras says: $a^2 + b^2 = c^2$

From calculus:

$$
\\int_0^1 x^n dx = \\frac{1}{n+1}
$$

"""
