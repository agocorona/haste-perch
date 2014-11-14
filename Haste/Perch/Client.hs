-----------------------------------------------------------------------------
--
-- Module      :  Builder
-- Copyright   :
-- License     :  BSD3
--
-- Maintainer  :  agocorona@gmail.com
-- Stability   :  experimental
-- Portability :
--
-- | Monad and Monoid instances for a builder that hang DOM elements from the
-- current parent element. It uses Haste.DOM from the haste-compiler
--
-----------------------------------------------------------------------------
{-#LANGUAGE CPP, ForeignFunctionInterface, TypeSynonymInstances, FlexibleInstances
             , DeriveDataTypeable, UndecidableInstances
            , OverlappingInstances #-}
module Haste.Perch.Client where
import Data.Typeable
import Haste.App
import Haste.Foreign
import Data.Maybe
import Data.Monoid
import Unsafe.Coerce
import Data.String
import Control.Monad.IO.Class
import Control.Applicative


newtype PerchM a= Perch{build :: Elem -> Client Elem} deriving Typeable

type Perch = PerchM ()

instance Monoid (PerchM a) where
    mappend mx my= Perch $ \e -> do
         build mx e
         build my e
         return e
    mempty  = Perch return

instance Functor PerchM
instance Applicative PerchM

instance Monad PerchM where
   (>>) x y= mappend (unsafeCoerce x) y
   (>>=) = error "bind (>>=) invocation in the Perch monad creating DOM elements"
   return  = mempty

instance MonadIO PerchM where
  liftIO mx= Perch $ \e ->  liftIO mx >> return e

instance IsString Perch where
  fromString= toElem

class ToElem a where
  toElem :: a -> Perch

instance ToElem String where
   toElem s= Perch $ \e -> do
        e' <- newTextElem s
        addChild e' e
        return e'

instance Show a => ToElem a where toElem = toElem . show

instance ToElem (PerchM a) where toElem e = unsafeCoerce e

attr tag (n, v)=Perch $ \e -> do
        tag' <- build tag e
        setAttr tag' n v
        return tag'

nelem :: String -> Perch
nelem s= Perch $ \e ->do
        e' <- newElem s
        addChild e' e
        return e'

child :: ToElem a => Perch -> a -> Perch
child me ch= Perch $ \e' -> do
        e <- build me e'
        let t = toElem ch
        r <- build t e
        return e

setHtml :: Perch -> String -> Perch
setHtml me text= Perch $ \e' -> do
    e <- build me e'
    inner e text
    return e'
  where
  inner :: Elem -> String -> Client ()
  inner e txt = setProp e "innerHTML" txt

-- | create an element and add a Haste event handler to it.
addEvent :: ClientCallback a => Perch -> Event Client a -> a -> Perch
addEvent be event action= Perch $ \e -> do
     e' <- build be e
     let atr= evtName event
     has <- getAttr e'  atr
     case has of
       "true" -> return e'
       _ -> do
        onEvent e' event  action
        setAttr e' atr "true"
        return e'



instance JSType JSString where
  toJSString x= x
  fromJSString x= Just x





-- Leaf DOM nodes
--
area = nelem "area"
base = nelem "base"
br = nelem "br"
col = nelem "col"
embed = nelem "embed"
hr = nelem "hr"
img = nelem "img"
input = nelem "input"
keygen = nelem "keygen"
link = nelem "link"
menuitem = nelem "menuitem"
meta = nelem "meta"
param = nelem "param"
source = nelem "source"
track = nelem "track"
wbr = nelem "wbr"

-- Parent DOM nodes
--

a cont = nelem  "a" `child` cont
abbr cont = nelem  "abbr" `child` cont
address cont = nelem  "address" `child` cont
article cont = nelem  "article" `child` cont
aside cont = nelem  "aside" `child` cont
audClient cont = nelem  "audio" `child` cont
b cont = nelem  "b" `child` cont
bdo cont = nelem  "bdo" `child` cont
blockquote cont = nelem  "blockquote" `child` cont
body cont = nelem  "body" `child` cont
button cont = nelem  "button" `child` cont
canvas cont = nelem  "canvas" `child` cont
caption cont = nelem  "caption" `child` cont
cite cont = nelem  "cite" `child` cont
code cont = nelem  "code" `child` cont
colgroup cont = nelem  "colgroup" `child` cont
command cont = nelem  "command" `child` cont
datalist cont = nelem  "datalist" `child` cont
dd cont = nelem  "dd" `child` cont
del cont = nelem  "del" `child` cont
details cont = nelem  "details" `child` cont
dfn cont = nelem  "dfn" `child` cont
div cont = nelem  "div" `child` cont
dl cont = nelem  "dl" `child` cont
dt cont = nelem  "dt" `child` cont
em cont = nelem  "em" `child` cont
fieldset cont = nelem  "fieldset" `child` cont
figcaption cont = nelem  "figcaption" `child` cont
figure cont = nelem  "figure" `child` cont
footer cont = nelem  "footer" `child` cont
form cont = nelem  "form" `child` cont
h1 cont = nelem  "h1" `child` cont
h2 cont = nelem  "h2" `child` cont
h3 cont = nelem  "h3" `child` cont
h4 cont = nelem  "h4" `child` cont
h5 cont = nelem  "h5" `child` cont
h6 cont = nelem  "h6" `child` cont
head cont = nelem  "head" `child` cont
header cont = nelem  "header" `child` cont
hgroup cont = nelem  "hgroup" `child` cont
html cont = nelem  "html" `child` cont
i cont = nelem  "i" `child` cont
iframe cont = nelem  "iframe" `child` cont
ins cont = nelem  "ins" `child` cont
kbd cont = nelem  "kbd" `child` cont
label cont = nelem  "label" `child` cont
legend cont = nelem  "legend" `child` cont
li cont = nelem  "li" `child` cont
map cont = nelem  "map" `child` cont
mark cont = nelem  "mark" `child` cont
menu cont = nelem  "menu" `child` cont
meter cont = nelem  "meter" `child` cont
nav cont = nelem  "nav" `child` cont
noscript cont = nelem  "noscript" `child` cont
object cont = nelem  "object" `child` cont
ol cont = nelem  "ol" `child` cont
optgroup cont = nelem  "optgroup" `child` cont
option cont = nelem  "option" `child` cont
output cont = nelem  "output" `child` cont
p cont = nelem  "p" `child` cont
pre cont = nelem  "pre" `child` cont
progress cont = nelem  "progress" `child` cont
q cont = nelem  "q" `child` cont
rp cont = nelem  "rp" `child` cont
rt cont = nelem  "rt" `child` cont
ruby cont = nelem  "ruby" `child` cont
samp cont = nelem  "samp" `child` cont
script cont = nelem  "script" `child` cont
section cont = nelem  "section" `child` cont
select cont = nelem  "select" `child` cont
small cont = nelem  "small" `child` cont
span cont = nelem  "span" `child` cont
strong cont = nelem  "strong" `child` cont
{-style cont = nelem  "style" `child` cont-}
sub cont = nelem  "sub" `child` cont
summary cont = nelem  "summary" `child` cont
sup cont = nelem  "sup" `child` cont
table cont = nelem  "table" `child` cont
tbody cont = nelem  "tbody" `child` cont
td cont = nelem  "td" `child` cont
textarea cont = nelem  "textarea" `child` cont
tfoot cont = nelem  "tfoot" `child` cont
th cont = nelem  "th" `child` cont
thead cont = nelem  "thead" `child` cont
time cont = nelem  "time" `child` cont
title cont = nelem  "title" `child` cont
tr cont = nelem  "tr" `child` cont
ul cont = nelem  "ul" `child` cont
var cont = nelem  "var" `child` cont
video cont = nelem  "video" `child` cont


ctag tag cont= nelem tag `child` cont

-- HTML4 support
center cont= nelem "center" `child` cont

noHtml= mempty :: Perch

type Attribute = (String,String)

class Attributable h where
 (!) :: h -> Attribute -> h

instance ToElem a => Attributable (a -> Perch) where
 (!) pe atrib = \e -> pe e `attr` atrib

instance Attributable Perch where
 (!) = attr


atr n v= (n,v)

style= atr "style"

id = atr "id"

width= atr "width"

height= atr "height"

href= atr "href"

src= atr "src"


---------------- DOM Tree navigation

-- | return the current node
this :: Perch
this= Perch $ \e -> return e

-- | goes to the parent node of the first and execute the second
goParent :: Perch -> Perch -> Perch
goParent pe pe'= Perch $ \e' -> do
  e <- build pe e'
  p <- liftIO  $ parent e
  e2 <- build pe' p
  return e2

-- | delete the current node. Return the parent
delete :: Perch
delete= Perch $ \e -> do
             p <- liftIO $ parent e
             removeChild e p
             return p

-- | delete the children of the current node.
clear :: Perch
clear= Perch $ \e -> clearChildren e >> return e


parent :: Elem -> IO Elem
parent= ffi $ toJSString "(function(e){return e.parentNode;})"


getBody :: IO Elem
getBody= ffi $ toJSString "(function(){return document.body;})"

getDocument :: IO Elem
getDocument= ffi $ toJSString  "(function(){return document;})"



-- ! JQuery-like DOM manipulation: using a selector for querySelectorAll,
-- it apply the Perch DOM manipulation of the second parameter for each of the matches
--
-- Example
--
-- > main= do
-- >  body <- getBody
-- >  (flip build) body $ pre $ do
-- >      div ! atr "class" "modify" $ "click"
-- >      div $ "not changed"
-- >      div ! atr "class" "modify" $ "here"
-- >
-- >      addEvent this OnClick $ \_ _ -> do
-- >          forElems' ".modify" $  this ! style "color:red"
forElems' :: String -> Perch -> Client ()
forElems' for doit= do
    (flip build) undefined (forElems for doit)
    return ()

-- ! JQuery-like DOM manipulation: using a selector for querySelectorAll,
-- it apply the Perch DOM manipulation of the second parameter for each of the matches
forElems :: String -> Perch -> Perch
forElems selectors dosomething= Perch $ \e -> do
    es <- liftIO $ queryAll  selectors
    mapM (build dosomething) es
    return e
    where
    queryAll ::  String -> IO  [Elem]
    queryAll = ffi $ toJSString "(function(sel){return document.querySelectorAll(sel);})"

