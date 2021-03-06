# fox-goose-bag-of-corn

One of Lewis Carroll's favorite puzzles to ask children was the one
about the _Fox, Goose, and Bag of Corn_.  It has to do with getting
them all safely across a river.

![alice swimming](/images/storytelling.gif)


The rules for this puzzle are:

- You must get the fox, goose, and bag of corn safely across the other side of the river
- You can only carry 1 item on the boat across with you.
- The fox cannot be left alone with the goose, (or it will be eaten).
- The goose cannot be left alone with the corn, (or it will be eaten).

The data structure to represent this puzzle is a vector of vectors.

The starting position is you, the fox, the goose, and corn on one side of the river. The boat is empty. The other river bank is empty.

```clojure
[[[:fox :goose :corn :you] [:boat] []]]
```

You could take the corn on the boat with you

```clojure
[[[:fox :goose :corn :you] [:boat] []]
 [[:fox :goose] [:boat :corn :you] []]]
```

But then the fox would eat the goose!

The goal is to have the plan in steps so that all make it safely to the other side

```
[[[:fox :goose :corn :you] [:boat] []]
 ...
 [[[] [:boat] [:fox :goose :corn :you]]]]
```

## Instructions

- Run the tests with `sbt FoxGooseBagOfCorn`
- Make the tests pass!

## Solutions

Once you have your kata solution, you are welcome to submit a link to your repo to share here in this section with others.

If you haven't solved your kata yet - Don't Peek!

## License

Copyright © 2015 Byron Samaripa

Copyright © 2014 Carin Meier

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
