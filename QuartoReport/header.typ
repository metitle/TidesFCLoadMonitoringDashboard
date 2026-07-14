// header.typ
#set page(
  width: 11in,
  height: 8.5in,
  fill: rgb("#221C35"),
  margin: (top: 1in, bottom: 1in, left: 1in, right: 1in),
  header: context {
    let page_num = counter(page).get().first()
    if not (page_num in (1, 2, 8, 12)) {
      pad(x: -0.75in)[
        #align(left)[
          #image("Halifax.png", width: 0.5in)
          ]
        ]
    }
  },
  footer: context {
    let page_num = counter(page).get().first()
    if not (page_num in (1, 2, 8, 12)) {
      align(center, text(fill: rgb("#FFFFFF"), size: 9pt)[#page_num])
    }
  }
)

#set text(fill: rgb("#FFFFFF"), font: "Oswald", size: 9pt)

#let section-page(bg-img, header-text) = {
  page(
    header: none,
    footer: none,
    margin: 0in,
    // Using the native background property ensures physical printers see the image
    background: image(bg-img, width: 100%, height: 100%, fit: "cover")
  )[
    // Content placed here automatically layers cleanly on top of the background
    #place(center + horizon)[
      #text(
        size: 60pt, 
        weight: "bold", 
        fill: rgb("#FFFFFF"),
        stroke: 1pt + rgb("#222222"),
        header-text
      )
    ]
  ]
}

#set par(justify: false)
