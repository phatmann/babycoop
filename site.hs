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
import Text.Blaze.Html5 (Html, (!), a, div, form, input, p, toHtml, label, ul, li, h2, span, br)
import Text.Blaze.Html5.Attributes (action, enctype, href, name, size, type_, value, rel, content, class_, title, id)
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
    dir "meeting" $ meetingPage
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

meetingHref :: Date -> String
meetingHref (year, month, day) = "/meeting/" ++ (show year) ++ "/" ++ (show month) ++ "/" ++ (show day)

homePage :: ServerPart Response
homePage = do
    fullCalendar <- liftIO readCalendar
    pastParam <- optional $ lookText "past"
    (past, future) <- liftIO $ pastAndFuture fullCalendar

    let (calendar, linkMsg :: String, link :: String) = case pastParam of
                                      Just _ -> (past, "Upcoming meetings", "/")
                                      Nothing -> (future, "Past meetings", "/?past=yes")

    ok $ template "Seattle League of Awesome Moms Baby Co-op (SLAM)" $ do
      h2 "Seattle League of Awesome Moms Baby Co-op (SLAM)"
      ul $ forM_ calendar meetingLink
      p $ a ! href (H.toValue link) $ toHtml linkMsg
      div ! class_ "well" $ do
                "Any questions, contact "
                a ! href "mailto:thephatmann@gmail.com" $ "thephatmann@gmail.com."
    where  meetingLink (Meeting date@(year, month, day) _) = li $ a ! href (H.toValue $ meetingHref date) $ toHtml $ ((show month) ++ "/" ++ (show day))

meetingPage :: ServerPart Response
meetingPage = msum [ view, process ]
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

                  editPerson :: Maybe Person
                  editPerson = case editParam of
                      Nothing -> Nothing
                      Just personText -> Just (read $ unpack $ personText)

                  slotClass :: Slot -> H.AttributeValue
                  slotClass slot =  case status slot of
                    Proposed  -> "proposed"
                    Confirmed -> "confirmed"
                    Requested -> "requested"

                  attendanceValues = [minBound .. maxBound] :: [Attendance]
                  attendanceHref slot = H.toValue $ meetingHref (year, month, day) ++ "?edit=" ++ (show $ person slot)

                  showDate :: Date -> String
                  showDate (0, 0, 0) = "not recently"
                  showDate (year, month, day) = (show month) ++ "/" ++ (show day) ++ "/" ++ (show year)

                  showStat :: Stat -> String
                  showStat stat =  
                    let inStr      = show $ inCount stat
                        outStr     = show $ outCount stat
                        absentStr  = show $ absentCount stat
                        lastHosted = showDate $ lastHostDate stat
                    in "Last 6 weeks: In " ++ inStr ++ ", Out " ++ outStr ++ ", Absent " ++ absentStr ++ ", Last Hosted " ++ lastHosted

                  attendanceTooltip slot = H.toValue $ showStat $ stat slot
                  attendanceLink slot = span ! class_ (slotClass slot) $ a ! href (attendanceHref slot) ! title (attendanceTooltip slot) $ toHtml $ show $ attendance slot
                  attendanceSelect slot = H.select ! name "attendance" $ toHtml options
                                          where options = map selectOption attendanceValues
                                                selectOption a =  (if a == (attendance slot)
                                                                   then H.option ! A.selected "selected"
                                                                   else H.option) $ toHtml $ show a
                  showSlot slot = do
                    toHtml $ show $ person slot
                    ": "
                    case editPerson of
                      Just p -> if p == (person slot) then attendanceSelect slot else attendanceLink slot
                      Nothing -> attendanceLink slot

              p $ a ! href "/" $ "Back to calendar"
              form ! action (H.toValue $ meetingHref (year, month, day)) ! enctype "multipart/form-data" ! A.method "POST" $ do
                ul $ forM_ slots (\slot -> li $ showSlot slot)
                input ! type_ "hidden" ! value (H.toValue $ year) ! name "year"
                input ! type_ "hidden" ! value (H.toValue $ month) ! name "month"
                input ! type_ "hidden" ! value (H.toValue $ day) ! name "day"
                case editPerson of
                  Just person -> do 
                    br
                    a ! href (H.toValue $ meetingHref (year, month, day)) $ "Cancel"
                    input ! type_ "submit" ! value "Save Changes" ! id "submit-button"
                    input ! type_ "hidden" ! value (H.toValue $ show person) ! name "person"
                  Nothing -> ""
              div ! class_ "well" $ do
                "Click on the link next to your name to change your status. Any questions, contact "
                a ! href "mailto:thephatmann@gmail.com" $ "thephatmann@gmail.com."

    process :: ServerPart Response
    process = do method POST
                 yearText         <- lookText "year"
                 monthText        <- lookText "month"
                 dayText          <- lookText "day"
                 personText       <- lookText "person"
                 attendanceText   <- lookText "attendance"
                 let person = (read $ unpack personText) :: Person
                     attendance = (read $ unpack attendanceText) :: Attendance
                     date = (read $ unpack yearText, read $ unpack monthText, read $ unpack dayText)
                     attendanceUpdates = [(person, attendance)]
                     meetingURL = meetingHref date 
                 liftIO $ updateCalendar date attendanceUpdates
                 seeOther (meetingURL :: String) (toResponse ())
