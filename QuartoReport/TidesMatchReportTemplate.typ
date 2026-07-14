// Some definitions presupposed by pandoc's typst output.
#let blockquote(body) = [
  #set text( size: 0.92em )
  #block(inset: (left: 1.5em, top: 0.2em, bottom: 0.2em))[#body]
]

#let horizontalrule = line(start: (25%,0%), end: (75%,0%))

#let endnote(num, contents) = [
  #stack(dir: ltr, spacing: 3pt, super[#num], contents)
]

#show terms: it => {
  it.children
    .map(child => [
      #strong[#child.term]
      #block(inset: (left: 1.5em, top: -0.4em))[#child.description]
      ])
    .join()
}

// Some quarto-specific definitions.

#show raw.where(block: true): set block(
    fill: luma(230),
    width: 100%,
    inset: 8pt,
    radius: 2pt
  )

#let block_with_new_content(old_block, new_content) = {
  let d = (:)
  let fields = old_block.fields()
  fields.remove("body")
  if fields.at("below", default: none) != none {
    // TODO: this is a hack because below is a "synthesized element"
    // according to the experts in the typst discord...
    fields.below = fields.below.abs
  }
  return block.with(..fields)(new_content)
}

#let empty(v) = {
  if type(v) == str {
    // two dollar signs here because we're technically inside
    // a Pandoc template :grimace:
    v.matches(regex("^\\s*$")).at(0, default: none) != none
  } else if type(v) == content {
    if v.at("text", default: none) != none {
      return empty(v.text)
    }
    for child in v.at("children", default: ()) {
      if not empty(child) {
        return false
      }
    }
    return true
  }

}

// Subfloats
// This is a technique that we adapted from https://github.com/tingerrr/subpar/
#let quartosubfloatcounter = counter("quartosubfloatcounter")

#let quarto_super(
  kind: str,
  caption: none,
  label: none,
  supplement: str,
  position: none,
  subrefnumbering: "1a",
  subcapnumbering: "(a)",
  body,
) = {
  context {
    let figcounter = counter(figure.where(kind: kind))
    let n-super = figcounter.get().first() + 1
    set figure.caption(position: position)
    [#figure(
      kind: kind,
      supplement: supplement,
      caption: caption,
      {
        show figure.where(kind: kind): set figure(numbering: _ => numbering(subrefnumbering, n-super, quartosubfloatcounter.get().first() + 1))
        show figure.where(kind: kind): set figure.caption(position: position)

        show figure: it => {
          let num = numbering(subcapnumbering, n-super, quartosubfloatcounter.get().first() + 1)
          show figure.caption: it => {
            num.slice(2) // I don't understand why the numbering contains output that it really shouldn't, but this fixes it shrug?
            [ ]
            it.body
          }

          quartosubfloatcounter.step()
          it
          counter(figure.where(kind: it.kind)).update(n => n - 1)
        }

        quartosubfloatcounter.update(0)
        body
      }
    )#label]
  }
}

// callout rendering
// this is a figure show rule because callouts are crossreferenceable
#show figure: it => {
  if type(it.kind) != str {
    return it
  }
  let kind_match = it.kind.matches(regex("^quarto-callout-(.*)")).at(0, default: none)
  if kind_match == none {
    return it
  }
  let kind = kind_match.captures.at(0, default: "other")
  kind = upper(kind.first()) + kind.slice(1)
  // now we pull apart the callout and reassemble it with the crossref name and counter

  // when we cleanup pandoc's emitted code to avoid spaces this will have to change
  let old_callout = it.body.children.at(1).body.children.at(1)
  let old_title_block = old_callout.body.children.at(0)
  let old_title = old_title_block.body.body.children.at(2)

  // TODO use custom separator if available
  let new_title = if empty(old_title) {
    [#kind #it.counter.display()]
  } else {
    [#kind #it.counter.display(): #old_title]
  }

  let new_title_block = block_with_new_content(
    old_title_block, 
    block_with_new_content(
      old_title_block.body, 
      old_title_block.body.body.children.at(0) +
      old_title_block.body.body.children.at(1) +
      new_title))

  block_with_new_content(old_callout,
    block(below: 0pt, new_title_block) +
    old_callout.body.children.at(1))
}

// 2023-10-09: #fa-icon("fa-info") is not working, so we'll eval "#fa-info()" instead
#let callout(body: [], title: "Callout", background_color: rgb("#dddddd"), icon: none, icon_color: black, body_background_color: white) = {
  block(
    breakable: false, 
    fill: background_color, 
    stroke: (paint: icon_color, thickness: 0.5pt, cap: "round"), 
    width: 100%, 
    radius: 2pt,
    block(
      inset: 1pt,
      width: 100%, 
      below: 0pt, 
      block(
        fill: background_color, 
        width: 100%, 
        inset: 8pt)[#text(icon_color, weight: 900)[#icon] #title]) +
      if(body != []){
        block(
          inset: 1pt, 
          width: 100%, 
          block(fill: body_background_color, width: 100%, inset: 8pt, body))
      }
    )
}



#let article(
  title: none,
  subtitle: none,
  authors: none,
  date: none,
  abstract: none,
  abstract-title: none,
  cols: 1,
  lang: "en",
  region: "US",
  font: "libertinus serif",
  fontsize: 11pt,
  title-size: 1.5em,
  subtitle-size: 1.25em,
  heading-family: "libertinus serif",
  heading-weight: "bold",
  heading-style: "normal",
  heading-color: black,
  heading-line-height: 0.65em,
  sectionnumbering: none,
  toc: false,
  toc_title: none,
  toc_depth: none,
  toc_indent: 1.5em,
  doc,
) = {
  set par(justify: true)
  set text(lang: lang,
           region: region,
           font: font,
           size: fontsize)
  set heading(numbering: sectionnumbering)
  if title != none {
    align(center)[#block(inset: 2em)[
      #set par(leading: heading-line-height)
      #if (heading-family != none or heading-weight != "bold" or heading-style != "normal"
           or heading-color != black) {
        set text(font: heading-family, weight: heading-weight, style: heading-style, fill: heading-color)
        text(size: title-size)[#title]
        if subtitle != none {
          parbreak()
          text(size: subtitle-size)[#subtitle]
        }
      } else {
        text(weight: "bold", size: title-size)[#title]
        if subtitle != none {
          parbreak()
          text(weight: "bold", size: subtitle-size)[#subtitle]
        }
      }
    ]]
  }

  if authors != none {
    let count = authors.len()
    let ncols = calc.min(count, 3)
    grid(
      columns: (1fr,) * ncols,
      row-gutter: 1.5em,
      ..authors.map(author =>
          align(center)[
            #author.name \
            #author.affiliation \
            #author.email
          ]
      )
    )
  }

  if date != none {
    align(center)[#block(inset: 1em)[
      #date
    ]]
  }

  if abstract != none {
    block(inset: 2em)[
    #text(weight: "semibold")[#abstract-title] #h(1em) #abstract
    ]
  }

  if toc {
    let title = if toc_title == none {
      auto
    } else {
      toc_title
    }
    block(above: 0em, below: 2em)[
    #outline(
      title: toc_title,
      depth: toc_depth,
      indent: toc_indent
    );
    ]
  }

  if cols == 1 {
    doc
  } else {
    columns(cols, doc)
  }
}

#set table(
  inset: 6pt,
  stroke: none
)

#set page(
  paper: "us-letter",
  margin: (x: 1.25in, y: 1.25in),
  numbering: "1",
)

#show: doc => article(
  font: ("Oswald",),
  toc_title: [Table of contents],
  toc_depth: 3,
  cols: 1,
  doc,
)
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


#show heading: set align(center)


#set table(inset: (x, y) => (
  x: 5pt,
  y: if y == 0 { 5pt } else { 2.5pt }
))


// Page 1 Visual Layout Context Frame
#align(center + horizon)[
  #grid(
    columns: 3,
    gutter: 1cm,
    image("Halifax.png", width: 5cm),
    image("vs.png", width: 5cm),
    image("Calgary.png", width: 5cm)
  )
  #v(1cm)
  #text(size: 60pt, weight: "bold")[MATCH REPORT] \
  #v(0.4cm)
  #text(size: 30pt, style: "italic", fill: rgb("#00B0B9"))[Season 2026 - Game 7] \
  #v(1cm)
  #text(size: 24pt)[Saturday, June 13, 2026]
]
#pagebreak()

// Page 2 Section Divider
#section-page("TidesFCImage5.jpg", "SQUAD GAME DATA")
#pagebreak()
#show figure: set block(breakable: true)

#block[ // start block

  #let style-dict = (
    // tinytable style-dict after
    "1_1": 0, "2_1": 0, "3_1": 0, "4_1": 0, "5_1": 0, "6_1": 0, "7_1": 0, "8_1": 0, "9_1": 0, "10_1": 0, "11_1": 0, "12_1": 0, "13_1": 0, "14_1": 0, "1_2": 0, "2_2": 0, "3_2": 0, "4_2": 0, "5_2": 0, "6_2": 0, "7_2": 0, "8_2": 0, "9_2": 0, "10_2": 0, "11_2": 0, "12_2": 0, "13_2": 0, "14_2": 0, "1_3": 0, "2_3": 0, "3_3": 0, "4_3": 0, "5_3": 0, "6_3": 0, "7_3": 0, "8_3": 0, "9_3": 0, "10_3": 0, "11_3": 0, "12_3": 0, "13_3": 0, "14_3": 0, "1_4": 0, "2_4": 0, "3_4": 0, "4_4": 0, "5_4": 0, "6_4": 0, "7_4": 0, "8_4": 0, "9_4": 0, "10_4": 0, "11_4": 0, "12_4": 0, "13_4": 0, "14_4": 0, "1_5": 0, "2_5": 0, "3_5": 0, "4_5": 0, "5_5": 0, "6_5": 0, "7_5": 0, "8_5": 0, "9_5": 0, "10_5": 0, "11_5": 0, "12_5": 0, "13_5": 0, "14_5": 0, "1_6": 0, "2_6": 0, "3_6": 0, "4_6": 0, "5_6": 0, "6_6": 0, "7_6": 0, "8_6": 0, "9_6": 0, "10_6": 0, "11_6": 0, "12_6": 0, "13_6": 0, "14_6": 0, "1_7": 0, "2_7": 0, "3_7": 0, "4_7": 0, "5_7": 0, "6_7": 0, "7_7": 0, "8_7": 0, "9_7": 0, "10_7": 0, "11_7": 0, "12_7": 0, "13_7": 0, "14_7": 0, "1_8": 0, "2_8": 0, "3_8": 0, "4_8": 0, "5_8": 0, "6_8": 0, "7_8": 0, "8_8": 0, "9_8": 0, "10_8": 0, "11_8": 0, "12_8": 0, "13_8": 0, "14_8": 0, "1_0": 1, "2_0": 1, "3_0": 1, "4_0": 1, "5_0": 1, "6_0": 1, "7_0": 1, "8_0": 1, "9_0": 1, "10_0": 1, "11_0": 1, "0_1": 2, "0_2": 2, "0_3": 2, "0_4": 2, "0_5": 2, "0_6": 2, "0_7": 2, "0_8": 2, "0_0": 3, "12_0": 4, "13_0": 4, "14_0": 4
  )

  #let style-array = ( 
    // tinytable cell style after
    (align: center + horizon,),
    (align: left + horizon,),
    (bold: true, align: center + top,),
    (bold: true, align: left + top,),
    (color: rgb("#00B0B9"), align: left + horizon,),
  )

  // Helper function to get cell style
  #let get-style(x, y) = {
    let key = str(y) + "_" + str(x)
    if key in style-dict { style-array.at(style-dict.at(key)) } else { none }
  }

  // tinytable align-default-array before
  #let align-default-array = ( left, left, left, left, left, left, left, left, left, ) // tinytable align-default-array here
  #show table.cell: it => {
    if style-array.len() == 0 { return it }
    
    let style = get-style(it.x, it.y)
    if style == none { return it }
    
    let tmp = it
    if ("fontsize" in style) { tmp = text(size: style.fontsize, tmp) }
    if ("color" in style) { tmp = text(fill: style.color, tmp) }
    if ("indent" in style) { tmp = pad(left: style.indent, tmp) }
    if ("underline" in style) { tmp = underline(tmp) }
    if ("italic" in style) { tmp = emph(tmp) }
    if ("bold" in style) { tmp = strong(tmp) }
    if ("mono" in style) { tmp = math.mono(tmp) }
    if ("strikeout" in style) { tmp = strike(tmp) }
    if ("smallcaps" in style) { tmp = smallcaps(tmp) }
    tmp
  }

  #align(center, [

  #table( // tinytable table start
    columns: (auto, auto, auto, auto, auto, auto, auto, auto, auto),
    stroke: none,
    rows: auto,
    align: (x, y) => {
      let style = get-style(x, y)
      if style != none and "align" in style { style.align } else { left }
    },
    fill: (x, y) => {
      let style = get-style(x, y)
      if style != none and "background" in style { style.background }
    },
 table.hline(y: 1, start: 0, end: 9, stroke: 0.05em + rgb("#FFFFFF")),
 table.hline(y: 15, start: 0, end: 9, stroke: 0.05em + rgb("#FFFFFF")),
 table.hline(y: 0, start: 0, end: 9, stroke: 0.05em + rgb("#FFFFFF")),
    // tinytable lines before

    // tinytable header start
    table.header(
      repeat: true,
[Player], [Time (min)], [Total Distance (m)], [HSR Distance (m)], [Sprint Distance (m)], [Accel Efforts], [Decel Efforts], [%HSR+Sprint Distance], [Max Speed (kph)],
    ),
    // tinytable header end

    // tinytable cell content after
[Rylee Foster], [97], [#image("tinytable_assets/tinytable_04_idn8680b4ksev1gwcjyi56.png", height: 2em)], [#image("tinytable_assets/tinytable_01_idsb1v1wwxnium6i6c7bi3.png", height: 2em)], [#image("tinytable_assets/tinytable_01_idcbt8brqnnpzr3asx7y9z.png", height: 2em)], [#image("tinytable_assets/tinytable_02_idr819v5emjwcsz6nurgbp.png", height: 2em)], [#image("tinytable_assets/tinytable_02_idp56dvo4helcjhbbotkvk.png", height: 2em)], [0.06%], [21.9],
[Addison Weichers], [97], [#image("tinytable_assets/tinytable_08_id57uesajjjnif3cv1cd0n.png", height: 2em)], [#image("tinytable_assets/tinytable_03_ida8kgxzj5wps8rfkgtk5l.png", height: 2em)], [#image("tinytable_assets/tinytable_04_idb1m4auq6qamamnx5hiw9.png", height: 2em)], [#image("tinytable_assets/tinytable_06_idgqx3s4my5p8t7oah0x5x.png", height: 2em)], [#image("tinytable_assets/tinytable_05_idymabxfmh0wgikrzio7fq.png", height: 2em)], [1.51%], [24.0],
[Annika Leslie], [97], [#image("tinytable_assets/tinytable_10_idx72ffuj9kv24dfobsm5m.png", height: 2em)], [#image("tinytable_assets/tinytable_06_idf28h3c04g90l6fyffio0.png", height: 2em)], [#image("tinytable_assets/tinytable_06_idav3z1e6h4hgvuzlyp7zv.png", height: 2em)], [#image("tinytable_assets/tinytable_04_idaj5cgwf61hr0pxavn49h.png", height: 2em)], [#image("tinytable_assets/tinytable_06_id1h97xxapid5ls485n53v.png", height: 2em)], [2.27%], [26.5],
[Julianne Vallerand], [97], [#image("tinytable_assets/tinytable_12_iddepr3spv0avqmq9w6qj6.png", height: 2em)], [#image("tinytable_assets/tinytable_10_id1yn629ioeci755c43rno.png", height: 2em)], [#image("tinytable_assets/tinytable_07_idvw2rzhy5j2j4enf540fn.png", height: 2em)], [#image("tinytable_assets/tinytable_12_id91vxj2bk2yzl0jmtw4p4.png", height: 2em)], [#image("tinytable_assets/tinytable_11_idiqv9cxgfgitztia2id5l.png", height: 2em)], [4.07%], [25.8],
[Sheyenne Allen], [97], [#image("tinytable_assets/tinytable_13_idyvar1ym76viicc8y8x8o.png", height: 2em)], [#image("tinytable_assets/tinytable_11_idqdafsr426h9j5eieoejn.png", height: 2em)], [#image("tinytable_assets/tinytable_13_idfl12qtp1p02bivjidk3a.png", height: 2em)], [#image("tinytable_assets/tinytable_13_id2ieb9q69jj142r86mp6y.png", height: 2em)], [#image("tinytable_assets/tinytable_12_idxax3fcfvo6s0e50n9khy.png", height: 2em)], [5.15%], [27.5],
[Cho So\-Hyun], [97], [#image("tinytable_assets/tinytable_14_idodv2j3r2lzoauxgfhqx8.png", height: 2em)], [#image("tinytable_assets/tinytable_13_idpprhcnre5nipy2n90r78.png", height: 2em)], [#image("tinytable_assets/tinytable_08_id9cgive2fo9pic0kd1cl1.png", height: 2em)], [#image("tinytable_assets/tinytable_11_id04b35jhzeivmqpt8weur.png", height: 2em)], [#image("tinytable_assets/tinytable_13_id3vkv3ujmsz0ar6qyfgek.png", height: 2em)], [4.64%], [24.6],
[Julia Benati], [60], [#image("tinytable_assets/tinytable_07_id6ewhecy015k9cwr1j4nk.png", height: 2em)], [#image("tinytable_assets/tinytable_07_idu939lf0tpfqh9avu89yv.png", height: 2em)], [#image("tinytable_assets/tinytable_03_idv3lc0somqlf3huw25nrj.png", height: 2em)], [#image("tinytable_assets/tinytable_10_idb53mqvmggmvned54jq0l.png", height: 2em)], [#image("tinytable_assets/tinytable_10_id5acuj0j3htgzp68yuwws.png", height: 2em)], [2.78%], [24.0],
[Synne Fredriksen Moe], [51], [#image("tinytable_assets/tinytable_06_idctrs45hk6fjryyjpuihc.png", height: 2em)], [#image("tinytable_assets/tinytable_04_id93bajv4ehuv3ol7fxgf1.png", height: 2em)], [#image("tinytable_assets/tinytable_02_id2h3nd0qfjjdesbqtu9jv.png", height: 2em)], [#image("tinytable_assets/tinytable_07_idqk8jcjfn3dkotuzyyrfb.png", height: 2em)], [#image("tinytable_assets/tinytable_08_ida7os9pz6dmaz888a9xgc.png", height: 2em)], [2.33%], [22.4],
[Jordyn Rhodes], [97], [#image("tinytable_assets/tinytable_11_id66k2fe8kwbkj3s3qejgm.png", height: 2em)], [#image("tinytable_assets/tinytable_14_id9gmklk6cxt568dfllp2j.png", height: 2em)], [#image("tinytable_assets/tinytable_14_idw9p5ddgbholm1lnknxcu.png", height: 2em)], [#image("tinytable_assets/tinytable_14_id37jo2bpwvit90uijvce7.png", height: 2em)], [#image("tinytable_assets/tinytable_14_id9q6rkfyl0xjugxn8xqjh.png", height: 2em)], [7.70%], [30.4],
[Saorla Miller], [84], [#image("tinytable_assets/tinytable_09_id7w03auadmn015vj0dhyj.png", height: 2em)], [#image("tinytable_assets/tinytable_12_idth2w77rza91cxppsy97x.png", height: 2em)], [#image("tinytable_assets/tinytable_10_idsdlwo4kslp3y7daqbr7h.png", height: 2em)], [#image("tinytable_assets/tinytable_08_idcdx68gmbwsiyf7n3umq9.png", height: 2em)], [#image("tinytable_assets/tinytable_07_id2wxur4b0l6985eazessl.png", height: 2em)], [4.82%], [26.4],
[Sydney Kennedy], [61], [#image("tinytable_assets/tinytable_05_id1f6wbpovu896k0wxiqa1.png", height: 2em)], [#image("tinytable_assets/tinytable_08_idivo34k0gqczoyt4us3qz.png", height: 2em)], [#image("tinytable_assets/tinytable_12_idgkywvrydyquxvlpcpku5.png", height: 2em)], [#image("tinytable_assets/tinytable_03_iduz7edfjnf7qcuckf9680.png", height: 2em)], [#image("tinytable_assets/tinytable_04_idm1171r19068wautszpc6.png", height: 2em)], [5.72%], [27.2],
[Sarah Taylor], [38], [#image("tinytable_assets/tinytable_03_id5jr3965cedxqbnaebslp.png", height: 2em)], [#image("tinytable_assets/tinytable_02_idoxbpnw29wdkz3cz25r24.png", height: 2em)], [#image("tinytable_assets/tinytable_05_iduzsam47c8zjqmlvqv248.png", height: 2em)], [#image("tinytable_assets/tinytable_01_idqrm6exm5r4d1leub0dpm.png", height: 2em)], [#image("tinytable_assets/tinytable_01_id6pbtkt7i22r7jt80wsf9.png", height: 2em)], [2.64%], [25.5],
[Stella Downing], [38], [#image("tinytable_assets/tinytable_02_id3zkbi0zpgf6xcuzyvep9.png", height: 2em)], [#image("tinytable_assets/tinytable_09_idbub8sav92x14mgr48mst.png", height: 2em)], [#image("tinytable_assets/tinytable_09_idr41o8p71z1g8n5cdk64p.png", height: 2em)], [#image("tinytable_assets/tinytable_09_idxjkkl5rq4p4f7ih8f1jt.png", height: 2em)], [#image("tinytable_assets/tinytable_09_idmvp4f8ha4cr546tje5uc.png", height: 2em)], [7.37%], [26.5],
[Tiffany Cameron], [13], [#image("tinytable_assets/tinytable_01_idx5ijecl1no51z90v564f.png", height: 2em)], [#image("tinytable_assets/tinytable_05_id0740m18khtbz2ma41n4e.png", height: 2em)], [#image("tinytable_assets/tinytable_11_idzlstxueqpvcww63pe1vk.png", height: 2em)], [#image("tinytable_assets/tinytable_05_idd3nvn9bcovsyzc1b9k6t.png", height: 2em)], [#image("tinytable_assets/tinytable_03_id9xy9dyouh4ibksvcdthn.png", height: 2em)], [13.50%], [26.7],

    // tinytable footer after

  ) // end table

  ]) // end align

] // end block
#pagebreak()
=== OVERALL HALF COMPARISON
<overall-half-comparison>
#box(image("TidesMatchReportTemplate_files/figure-typst/distance_per_half-1.svg"))

#pagebreak()
=== Total Distance and Total Distance/Min By Player (per half)
<total-distance-and-total-distancemin-by-player-per-half>
#box(image("TidesMatchReportTemplate_files/figure-typst/md_by_player_total_distance-1.svg"))

#box(image("TidesMatchReportTemplate_files/figure-typst/md_by_player_total_distance_per_min-1.svg"))

#pagebreak()
=== High Speed Running Distance and HSR Distance/Min By Player (per half)
<high-speed-running-distance-and-hsr-distancemin-by-player-per-half>
#box(image("TidesMatchReportTemplate_files/figure-typst/md_by_player_hsr_distance-1.svg"))

#box(image("TidesMatchReportTemplate_files/figure-typst/md_by_player_hsr_distance_per_min-1.svg"))

#pagebreak()
=== Sprint Distance and Sprint Distance/Min By Player (per half)
<sprint-distance-and-sprint-distancemin-by-player-per-half>
#box(image("TidesMatchReportTemplate_files/figure-typst/md_by_player_sprint_distance-1.svg"))

#box(image("TidesMatchReportTemplate_files/figure-typst/md_by_player_sprint_distance_per_min-1.svg"))

#pagebreak()

// Page 8 Section Divider
#section-page("TidesFCImage6.jpg", "MATCH DAY COMPARISON")
#pagebreak()
#box(image("TidesMatchReportTemplate_files/figure-typst/md_comparison_total_distance-1.svg"))

#box(image("TidesMatchReportTemplate_files/figure-typst/md_comparison_total_distance_per_min-1.svg"))

#pagebreak()
#box(image("TidesMatchReportTemplate_files/figure-typst/md_comparison_hsr_distance-1.svg"))

#box(image("TidesMatchReportTemplate_files/figure-typst/md_comparison_hsr_distance_per_min-1.svg"))

#pagebreak()
#box(image("TidesMatchReportTemplate_files/figure-typst/md_comparison_sprint_distance-1.svg"))

#box(image("TidesMatchReportTemplate_files/figure-typst/md_comparison_sprint_distance_per_min-1.svg"))

#pagebreak()
// Page 12 Section Divider
#section-page("TidesFCImage7.jpg", "DISTANCE ACROSS THE MATCH")
#pagebreak()
#box(image("TidesMatchReportTemplate_files/figure-typst/15min_total_distance-1.svg"))

#pagebreak()
#box(image("TidesMatchReportTemplate_files/figure-typst/15min_hsr_distance-1.svg"))

#pagebreak()
#box(image("TidesMatchReportTemplate_files/figure-typst/15min_sprint_distance-1.svg"))
