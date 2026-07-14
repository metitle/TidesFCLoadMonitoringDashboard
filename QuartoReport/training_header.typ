// training_header.typ
#set page(
  width: 8.5in,
  height: 11in,
  fill: rgb("#FFFFFF"),
  margin: (top: 0.8in, bottom: 0.8in, left: 0.8in, right: 0.8in),
  header: context {
      pad(x: -0.65in, y: -0.1in)[
        #align(left)[
          #image("Halifax.png", width: 0.5in)
          ]
        ]
    },
  footer: context {
    let page_num = counter(page).get().first()
    align(center, text(fill: rgb("#221C35"), size: 9pt)[#page_num])
  }
)

#set text(fill: rgb("#221C35"), font: "Oswald", size: 9pt)


#set par(justify: false)
