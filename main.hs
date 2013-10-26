 {-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import Control.Applicative ((<$>), optional)
import Control.Monad (forM_)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Lazy (unpack)
import Happstack.Lite
import Text.Blaze.Html5 (Html, (!), a, form, input, p, toHtml, label, ul, li, h2)
import Text.Blaze.Html5.Attributes (action, enctype, href, name, size, type_, value)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Calendar
import BabyCoop

main :: IO ()
main = serve Nothing myApp

myApp :: ServerPart Response
myApp = msum
  [ 
    dir "week"    $ week
    , homePage
  ]

--myApp :: ServerPart Response
--myApp = msum
--  [ dir "echo"    $ echo
--  , dir "query"   $ queryParams
--  , dir "form"    $ formPage
--  , homePage
--  ]

template :: Text -> Html -> Response
template title body = toResponse $
  H.html $ do
    H.head $ do
      H.title (toHtml title)
    H.body $ do
      body
      p $ a ! href "/" $ "back home"

homePage :: ServerPart Response
homePage =
    ok $ template "Home" $ ul $ forM_ theCalendar weekLink
    where weekLink (date@(year, month, day), _) = li $ a ! href (weekHref date) $ toHtml $ ((show month) ++ "/" ++ (show day))
          weekHref (year, month, day) = H.toValue $ "/week/" ++ (show year) ++ "/" ++ (show month) ++ "/" ++ (show day)

week :: ServerPart Response
week =
    path $ \(year :: Int) ->
      path $ \(month :: Int) ->
        path $ \(day :: Int) ->
          ok $ template "Week" $ do
            h2 $ toHtml $ (show month) ++ "/" ++ (show day)
            let Just slots = lookup date theCalendar
                date = (year, month, day)
                showSlot slot  = (show $ person slot) ++ ": " ++ (show $ attendance slot)
            ul $ forM_ slots (li . toHtml . showSlot)

--homePage :: ServerPart Response
--homePage =
--    ok $ template "home page" $ do
--           H.h1 "Hello!"
--           H.p "Writing applications with happstack-lite is fast and simple!"
--           H.p "Check out these killer apps."
--           H.p $ a ! href "/echo/secret%20message"  $ "echo"
--           H.p $ a ! href "/query?foo=bar" $ "query parameters"
--           H.p $ a ! href "/form"          $ "form processing"

--queryParams :: ServerPart Response
--queryParams =
--    do mFoo <- optional $ lookText "foo"
--       ok $ template "query params" $ do
--         p $ "foo is set to: " >> toHtml (show mFoo)
--         p $ "change the url to set it to something else."

--formPage :: ServerPart Response
--formPage = msum [ viewForm, processForm ]
--  where
--    viewForm :: ServerPart Response
--    viewForm =
--        do method GET
--           ok $ template "form" $
--              form ! action "/form" ! enctype "multipart/form-data" ! A.method "POST" $ do
--                label ! A.for "msg" $ "Say something clever"
--                input ! type_ "text" ! A.id "msg" ! name "msg"
--                input ! type_ "submit" ! value "Say it!"

--    processForm :: ServerPart Response
--    processForm =
--        do method POST
--           msg <- lookText "msg"
--           ok $ template "form" $ do
--             H.p "You said:"
--             H.p (toHtml msg)
