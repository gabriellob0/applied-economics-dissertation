#let project(
  title: "",
  authors: (),
  body,
) = {
  // Set the document font and size
  set text(font: "Arial", size: 12pt)

  // Set the line spacing
  set par(leading: 2em)

  // Set the page margins
  set page(margin: (x: 1.5cm, y: 1.5cm))

  // Set the page numbering
  set page(numbering: "1")

  // Add the title and authors
  align(center)[
    #block(text(weight: 700, 1.75em, title))
    #v(1em, weak: true)
    #authors.map(author => text(size: 1.25em, author)).join(", ")
  ]

  // Add the main content
  set par(justify: true)
  body
}