// Some definitions presupposed by pandoc's typst output.
#let blockquote(body) = [
  #set text( size: 0.92em )
  #block(inset: (left: 1.5em, top: 0.2em, bottom: 0.2em))[#body]
]

#let horizontalrule = [
  #line(start: (25%,0%), end: (75%,0%))
]

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

#show raw.where(block: true): block.with(
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
    fields.below = fields.below.amount
  }
  return block.with(..fields)(new_content)
}

#let empty(v) = {
  if type(v) == "string" {
    // two dollar signs here because we're technically inside
    // a Pandoc template :grimace:
    v.matches(regex("^\\s*$")).at(0, default: none) != none
  } else if type(v) == "content" {
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

#show figure: it => {
  if type(it.kind) != "string" {
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
    new_title_block +
    old_callout.body.children.at(1))
}

#show ref: it => locate(loc => {
  let target = query(it.target, loc).first()
  if it.at("supplement", default: none) == none {
    it
    return
  }

  let sup = it.supplement.text.matches(regex("^45127368-afa1-446a-820f-fc64c546b2c5%(.*)")).at(0, default: none)
  if sup != none {
    let parent_id = sup.captures.first()
    let parent_figure = query(label(parent_id), loc).first()
    let parent_location = parent_figure.location()

    let counters = numbering(
      parent_figure.at("numbering"), 
      ..parent_figure.at("counter").at(parent_location))
      
    let subcounter = numbering(
      target.at("numbering"),
      ..target.at("counter").at(target.location()))
    
    // NOTE there's a nonbreaking space in the block below
    link(target.location(), [#parent_figure.at("supplement") #counters#subcounter])
  } else {
    it
  }
})

// 2023-10-09: #fa-icon("fa-info") is not working, so we'll eval "#fa-info()" instead
#let callout(body: [], title: "Callout", background_color: rgb("#dddddd"), icon: none, icon_color: black) = {
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
      block(
        inset: 1pt, 
        width: 100%, 
        block(fill: white, width: 100%, inset: 8pt, body)))
}



#let article(
  title: none,
  authors: none,
  date: none,
  abstract: none,
  cols: 1,
  margin: (x: 1.25in, y: 1.25in),
  paper: "us-letter",
  lang: "en",
  region: "US",
  font: (),
  fontsize: 11pt,
  sectionnumbering: none,
  toc: false,
  toc_title: none,
  toc_depth: none,
  doc,
) = {
  set page(
    paper: paper,
    margin: margin,
    numbering: "1",
  )
  set par(justify: true)
  set text(lang: lang,
           region: region,
           font: font,
           size: fontsize)
  set heading(numbering: sectionnumbering)

  if title != none {
    align(center)[#block(inset: 2em)[
      #text(weight: "bold", size: 1.5em)[#title]
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
    #text(weight: "semibold")[Abstract] #h(1em) #abstract
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
      depth: toc_depth
    );
    ]
  }

  if cols == 1 {
    doc
  } else {
    columns(cols, doc)
  }
}
#show: doc => article(
  title: [Gender Norms, Hispanicity, and Household Production: Evidence from the US],
  authors: (
    ( name: [Gabriel Lobo de Oliveira],
      affiliation: [],
      email: [] ),
    ),
  abstract: [Example of abstract.],
  fontsize: 12pt,
  toc_title: [Table of contents],
  toc_depth: 3,
  cols: 1,
  doc,
)


#pagebreak()
== Introduction
<introduction>
Example of text here.

== Literature Review
<literature-review>
=== Social Norms and Identity
<social-norms-and-identity>
Social norms have received considerable attention from economists starting in the early 2000s to explain economic behaviours that traditional models could not. Some notable recent examples have included conflict-related sexual violence \(Guarnieri and Tur-Prats, 2023), domestic violence \(Alessina, Brioschi and La Ferrara, 2020), labour market participation, marriage markets, and household production \(Bertrand, Kamenica and Pan, 2015; Senik, Georgieff and Lippmann, 2020).

Much of this literature has focused on the role of gender norms on economic \(and otherwise) behaviours, following previous research in other social sciences. West and Zimmerman \(1987) developed many ideas in the sociology literature that economists later incorporated. In particular, they provided an analytical formulation of gender as #emph[produced] from individual behaviours.

Given the presence of #emph[normative conceptions] on the appropriateness of different behaviours for individuals belonging to different #emph[sex categories];#footnote[West and Zimmerman \(1987) propose an analytical distinction between #emph[sex];, #emph[sex category];, and #emph[gender];. They define #emph[sex] as determined by biological criteria and #emph[sex category] as a categorisation using these criteria in a social context.];, individuals use these behaviours to associate themselves with these categories. West and Zimmerman \(1987) contextualise the gendered division of labour within this framework. Women do housework, and men do not because it helps them to establish their identity.

These ideas were primarily introduced in the economic literature by Akerlof and Kranton \(2000, 2010). Drawing from social psychology and sociology research, they propose a utility function that depends on social identity. Social identity can comprise different categories \(such as sex categories), which are each associated with specific prescriptions. Violating these prescriptions generates disutility in itself since they evoke negative feelings \(in particular, anxiety).

A critical aspect of the model is that individuals self-assign to categories and also assign others to categories. Furthermore, a person’s self-assigned category does not need to match how others perceive her. Formally, "U\_i" denotes individual "i’s" utility function.

$ U_i = U_i (a_i , a_(- i) , I_i) $

where "a\={a\_i, a\_-i}" is a strategy profile and "I\_i" is the identity. Identity is a function of the strategy profile given "i’s" assignment of individuals to categories "c\_i element of C" \(where C is the set of all categories), personal characteristics "e\_i", and the prescriptions associated with different categories P.

$ I_i = I_i (a_i , a_(- i) ; c_i , epsilon.alt_i , bold("P")) $

Since individual "i" assigns categories to other players and holds that, given these categories, they should adhere to specific behaviours, this function captures externalities from violating prescriptions. For example, a wife who outearns her husband could challenge his "manhood". Consequently, players might attempt to preserve their identity by punishing violators. In the example above, this could be by threatening divorce.

A substantial strand of the literature has understood social norms as behaviours that are followed given a threat of punishment. Here, I understand social norms broadly, as implied by the prescriptions in Akerlof and Kranton’s \(2000) model or the #emph[normative conceptions] used by West and Zimmerman \(1987). However, the term lacks standard usage within economics or between social sciences.

Akerlof and Kranton \(2000) also contextualise housework within their model. In particular, as with previous research, they highlight that women do the majority of housework and child care, even if they are primary earners in the household. Standard economic models \(CITE) would instead predict that labour market income would negatively affect housework. The opportunity cost of household production is higher for an individual with higher wages so that household members would specialise based on relative incomes.

=== Next Part
<next-part>
Some text example #cite(<smith2024>);.

= Code Test
<code-test>
Table from imagine

#align(center)[
#box(width: 1736.0pt, image("regression_table.png"))
]
#pagebreak()



#set bibliography(style: "harvard-cite-them-right")

#bibliography("refs.bib")

