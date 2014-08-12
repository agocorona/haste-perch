perch
=================

Perch defines builder elements (perchs) for Haste.DOM elements that are appendable, so that dynamic HTML can be created in the client in a natural way, like textual HTML, but programmatically and with the advantage of static type checking. It can be ported to other haskell-js compilers

Haste is a compiler that generates Javascript code from Haskell.

https://github.com/valderman/haste-compiler

The Haste.DOM module define a thin layer over the JavaScript DOM. The DOM is a low level HTML tree manipulation API. That makes the creation and manipulation of DOM elements almost as painful as in JavaScript.

This package makes the creation of DOM elements easy with a syntax  similar to other haskell HTML generators, using monoids and monads, such is the case of the package blaze-html.

http://hackage.haskell.org/package/blaze-html

This is an example. `withElem`  is a Haste.DOM call that give the DOM object whose id is "idelem", that has been created "by hand" in Main.hs. The program takes this element and add content to it:

```haskell
main :: IO ()
main = do
  withElem "idelem" $ build $ do
    div $ do
      addEvent this OnClick $ \_ _ -> alert "hello, world!"
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

The  monoid expression can also be used, by concatenating elements with the operator <>

        ... term1 <> term2 ...


Is equivalent to

    do ...
       term1
       term2
       ...

How to run
----------

install the ghc compiler

install Haste:

    >cabal install haste-compiler

clone haskell-js-html-builder

    >git clone http://github.com/agocorona/haste-perch.git

install perch

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


The basic element is a "builder" that has a "hole" parameter and a IO action about what element will be created. The hole will receive the parent (Elem) of the element/s that will be created by the builder. So a builder can be considered like a perch that has other perchs that hang from it. either a single element or an entire tree.

the call `nelem` (new element) is a perch that creates a single tag element. Upon created, it  is added to the parent and return itself as parent of the next elements that can be hooked. To append two elements, both are added to the parent.

The Monad instance is there in order to use the do notation, that add a new level of syntax, in the style of the package blaze-html. This monad invokes the same appending mechanism.
