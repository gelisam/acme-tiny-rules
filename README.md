# acme-tiny-rules

![nonsense inference rules appear and disappear around the lines drawn and erased by the user](demo.gif)

`acme-tiny-rules` allows you to create beautiful (but nonsensical) inference rules by drawing and erasing lines. It is a parody of [Tiny Glade](https://store.steampowered.com/app/2198150/Tiny_Glade/), which allows you to create beautiful castles by drawing and erasing lines. Here is the part of the Tiny Glade trailer I was trying to emulate:

![a beautiful castle appears and disappears around the lines drawn and erased by the user](tiny-glades.gif)

## The joke

`acme` is the traditional prefix for joke packages in the Haskell ecosystem.

Someone posted the above Tiny Glades trailer, saying "this but for compilers, that's my goal". Presumably they meant that they want creating programming languages to be effortless, with the program fleshing out the rest of the compiler from a few user-given constraints. I wrote `acme-tiny-rules` as a joke response.

The joke, of course, is that `acme-tiny-rules` takes the analogy too literally. It borrows the aestetics of continuously completing user-drawn lines with something beautiful, or at least beautifully-typeset. But it doesn't deliver an iota of value, and thus doesn't actually build towards the dream of deriving compilers from constraints.
