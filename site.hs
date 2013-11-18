 {-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import Prelude hiding (head, id, div, span)
import Control.Applicative ((<$>), optional)
import Control.Monad (forM_)
import Control.Monad.IO.Class
import Data.Maybe (fromMaybe)
import Data.Text (Text, pack)
import Data.Text.Lazy (unpack)
import Data.ByteString.Lazy as B (readFile)
import Data.Aeson
import Happstack.Lite
import Text.Blaze.Html5 (Html, (!), a, div, form, input, p, toHtml, label, ul, li, h2, span)
import Text.Blaze.Html5.Attributes (action, enctype, href, name, size, type_, value, rel, content, class_)
import qualified Text.Blaze.Html5 as H hiding (map)
import qualified Text.Blaze.Html5.Attributes as A hiding (title)

import Calendar
import Scheduler

main :: IO ()
main = serve config myApp
  where config = Just ServerConfig {
                port      = 8000
              , ramQuota  = 1 * 10^6
              , diskQuota = 20 * 10^6
              , tmpDir    = "/tmp/"
              }

myApp :: ServerPart Response
myApp = msum [ 
    dir "week" $ weekPage
  , dir "static" $ serveDirectory DisableBrowsing [] "public"
  , homePage
  ]

template :: Text -> Html -> Response
template title body = toResponse $
  H.html $ do
    H.head $ do
      H.meta ! name "viewport" ! content "width=device-width, initial-scale=1.0"
      H.link ! rel "stylesheet" ! href "//netdna.bootstrapcdn.com/bootstrap/3.0.0/css/bootstrap.min.css"
      H.link ! rel "stylesheet" ! href "//netdna.bootstrapcdn.com/bootstrap/3.0.0/css/bootstrap-theme.min.css"
      H.link ! rel "stylesheet" ! href "/static/site.css"
      -- H.script ! H.src "//netdna.bootstrapcdn.com/bootstrap/3.0.0/js/bootstrap.min.js"
      H.title $ toHtml title
      --preEscapedString "<!--[if lt IE 9]>"
      --preEscapedString "<script src=\"../../assets/js/html5shiv.js\"></script>"
      --preEscapedString "<script src=\"../../assets/js/respond.min.js\"></script>"
      --preEscapedString "<![endif]-->"
    H.body $ do
      div ! class_ "container" $ do
        body
        div ! class_ "well" $ do
          "If you cannot make it or want to reserve a spot, email Tony Mann at "
          a ! href "mailto:thephatmann@gmail.com" $ "thephatmann@gmail.com"

homePage :: ServerPart Response
homePage = do
    calendar <- liftIO readCalendar
    ok $ template "Seattle League of Awesome Moms Baby Co-op (SLAM)" $ do
      h2 "Seattle League of Awesome Moms Baby Co-op (SLAM)"
      ul $ forM_ calendar weekLink
    where weekLink (Meeting date@(year, month, day) _) = li $ a ! href (weekHref date) $ toHtml $ ((show month) ++ "/" ++ (show day))
          weekHref (year, month, day) = H.toValue $ "/week/" ++ (show year) ++ "/" ++ (show month) ++ "/" ++ (show day)

weekPage :: ServerPart Response
weekPage = msum [ view, process ]
  where
    view :: ServerPart Response
    view = do 
      calendar <- liftIO readCalendar
      editParam <- optional $ lookText "edit"

      method GET
      path $ \(year :: Int) ->
        path $ \(month :: Int) ->
          path $ \(day :: Int) ->
            ok $ template "SLAM - Week" $ do
              h2 $ toHtml $ (show month) ++ "/" ++ (show day)
              let Just (Meeting _ slots) = findMeeting date calendar
                  date = (year, month, day)
                  slotClass :: Slot -> H.AttributeValue
                  slotClass slot =  case status slot of
                    Proposed  -> "proposed"
                    Confirmed -> "confirmed"
                    Requested -> "requested"
                  attendanceValues = [minBound .. maxBound] :: [Attendance]
                  attendanceSelect slot = H.select ! name selectName $ toHtml options
                                              where selectName = H.toValue  $ "attendance[" ++ (show $ person slot) ++ "]"
                                                    options = map selectOption attendanceValues
                                                    selectOption a =  (if a == (attendance slot)
                                                                       then H.option ! A.selected "selected"
                                                                       else H.option) $ toHtml $ show a
                  showSlot slot = do
                    toHtml $ show $ person slot
                    ": "
                    case editParam of
                      Nothing -> span ! class_ (slotClass slot) $ toHtml $ show $ attendance slot
                      otherwise -> attendanceSelect slot
              p $ a ! href "/" $ "Back to calendar"
              ul $ forM_ slots (\slot -> li $ showSlot slot)
    process :: ServerPart Response
    process = undefined
      --do method POST 
        

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
