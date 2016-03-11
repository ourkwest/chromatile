# Chromatile

A chromatic tile game written in ClojureScript

## Overview

The bulk of this game was hacked together over three days in early 2015, with a (very) little polish to the overall
look and feel applied in the following weeks.

The project was undertaken to gain experience with ClojureScript, and prototype a game idea of mine.

The code quality is low, as it was optimised for learning rather than maintainability.
In particular there are no tests, and no use of appropriate libraries beyond the core Clojure(Script) libraries.
Additionally, there is almost no documentation, some whimsical naming and a lax approach to modularisation.
At least one function is over 100 lines long and the level-design data structures are rather cryptic.

That said, it might be possible to play the game by running:

> lein cljsbuild auto

and then opening release/index.html in your browser.

