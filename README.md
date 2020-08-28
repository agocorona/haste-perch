Perch
=================
NOTE: This version works with the version 0.5 of haste-compiler, get it from the git repo:

https://github.com/valderman/haste-compiler

![perch](http://www.designboom.com/history/wirehangers/whnew.gif)

Perch defines builder elements (perchs) for Haste.DOM elements that are appendable, so that dynamic HTML can be created in the client in a natural way, like textual HTML, but programmatically and with the advantage of static type checking. It can be ported to other haskell-js compilers

Haste is a compiler that generates Javascript code from Haskell.

https://github.com/valderman/haste-compiler

The Haste.DOM module define a thin layer over the JavaScript DOM. The DOM is a low level HTML tree manipulation API. That makes the creation and manipulation of DOM elements almost as painful as in JavaScript.

This package makes the creation of DOM elements easy with a syntax  similar to other haskell HTML generators, using monoids and monads, such is the case of the package blaze-html.

http://hackage.haskell.org/package/blaze-html

This is an example. `withElem`  is a Haste.DOM call that give the DOM object whose id is "idelem", that has been created "by hand" in Main.hs. The program takes this element and add content to it:

```haskell
import Haste.Perch
import Haste
import Haste.Events
import Haste.DOM
import Prelude hiding (div)

main :: IO ()
main = do
  withElem "idelem" $ build $ do
    div $ do
      addEvent this Click $ \_ -> alert "hello, world!"
      div $ do
        p "hello"
        p ! atr "style" "color:red" $ "world"
  return ()
```

Creates these element:

```html
<div id="idelem">  <!-- was already in the HTNL -->
  <div>
    <div>
      <p>hello</p>
      <p style="color:red">world</p>
    </div>
  </div>
</div>
```

This other example modifies the previosly created elements when the event is raised using jQuery-like wildcards to modify all the elements of the class ".modify":

```haskell
import Haste.Perch
import Haste
import Haste.Events
import Haste.DOM hiding (style)
import Prelude hiding (div)

main= do
  body <- getBody
  (flip build) body $ do
      div ! atr "class" "modify" $ "click"
      div $ "not changed"
      div ! atr "class" "modify" $ "here"

      addEvent this Click $ \_ -> do
          forElems' ".modify" $ this ! style "color:red" `child` " modified"
```

The  monoid expression can also be used, by concatenating elements with the operator <>

        ... term1 <> term2 ...


Is equivalent to

    do ...
       term1
       term2
       ...

How to run
----------

### Docker file


https://hub.docker.com/r/agocorona/tryhplay

Contains everything necessary to use haste-perch

### install from scratch

install the ghc compiler

install Haste:

    >cabal install haste-compiler

boot Haste:

    >haste-boot

clone haste-perch

    >git clone http://github.com/agocorona/haste-perch.git

install perch

    >cd haste-perch
    >haste-inst install

compile the main program

    >hastec Main.hs

browse the Main.html file. In windows simply execute it in the command line:

    >Main.html

Execute it in the same directory where Main.js is, since it references it assuming that it is in the current folder

Perch not only is for creating HTML.DOM elements, it also can be used to navigate the tree, search
etc.


How it works
------------

The basic element is a "builder" that has a "hole" parameter and a IO action which creates the DOM element. The hole will receive the parent (Elem) of the element/s that will be created by the builder. So a builder can be considered like a perch that has other perchs that hang from it. Either a no one or an entire tree.

the call `nelem` (new element) is a perch that creates a single DOM element. Upon created, it  is added to the parent and return itself as parent of the next elements that can be hooked from it if `child` is used. When appending two elements, both are added to the parent.

The Monad instance is there in order to use the do notation, that add a new level of syntax, in the style of the package blaze-html. This monad invokes the same appending mechanism.

A perch is a generalization of a list and it is handled in the same way.

While a list is an unary tree, perch create n-ary trees. While in a list the monoid instance add child nodes
down in the only direction that it can grow, the perch monoid add childs at the same level, horizontally.
Is the `child` primitive the one that creates branches down.
