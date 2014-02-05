{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Data.Maybe
import           Data.ByteString (ByteString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as ES
import qualified Text.XmlHtml as X
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth
import           Snap.Snaplet.Auth.Backends.JsonFile
import           Snap.Snaplet.Heist
import           Snap.Snaplet.Session.Backends.CookieSession
import           Snap.Util.FileServe
import           Snap.Extras.SpliceUtils.Interpreted
import           Heist
import qualified Heist.Interpreted as I
import           Heist.Splices
------------------------------------------------------------------------------
import           Application

import           Control.Monad.IO.Class
import           System.Cmd
import           Calendar
import           Scheduler
import           Control.Monad
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.Aeson

------------------------------------------------------------------------------
-- | Render login form
handleLogin :: Maybe T.Text -> Handler App (AuthManager App) ()
handleLogin authError = heistLocal (I.bindSplices errs) $ render "login"
  where
    errs = maybe noSplices splice authError
    splice err = "loginError" ## I.textSplice err


------------------------------------------------------------------------------
-- | Handle login submit
handleLoginSubmit :: Handler App (AuthManager App) ()
handleLoginSubmit =
    loginUser "login" "password" Nothing
              (\_ -> handleLogin err) (redirect "/")
  where
    err = Just "Unknown user or password"


------------------------------------------------------------------------------
-- | Logs out and redirects the user to the site index.
handleLogout :: Handler App (AuthManager App) ()
handleLogout = logout >> redirect "/"

------------------------------------------------------------------------------
-- | Handle new coop form
handleNewUser :: Handler App (AuthManager App) ()
handleNewUser = do
      user <- currentUser

      case user of
        Nothing -> redirect "/"
        Just u -> if isAdminUser user then
                    method GET handleForm <|> method POST handleFormSubmit
                  else
                    redirect "/"
                    
  where
    handleForm = render "new_user"
    handleFormSubmit = do
      user <- registerUser "login" "password"

      case user of 
        Left err  -> redirect "/new_user"
        Right u   -> do
          calendar <- getPar "calendar"

          case calendar of
            Nothing -> redirect "/new_user"
            Just c  -> do
              saveUser $ u { userMeta = HM.singleton "calendars" (toJSON [c]) }
              redirect "/"

------------------------------------------------------------------------------
-- | Handle home page
handleHome :: Handler App (AuthManager App) ()
handleHome = do
  user <- currentUser
  
  case user of 
    Nothing   -> render "home"
    Just _    -> if isAdminUser user then
                   render "home"
                 else
                   handleHomeLoggedIn

handleHomeLoggedIn :: Handler App (AuthManager App) ()
handleHomeLoggedIn = do
  user                <- currentUser
  calendar            <- liftIO $ readCalendar $ calendarFileNameForAuthUser user
  pastParam           <- getPar "past"
  (past, future)      <- liftIO $ pastAndFuture $ meetings calendar

  let pastMeetingsName   = "Past meetings"
      futureMeetingsName = "Upcoming meetings"
      (meetings, calendarName, otherCalendarName, otherCalendarURL) = case pastParam of
                                                        Just _  -> (past, pastMeetingsName, futureMeetingsName, "/")
                                                        Nothing -> (future, futureMeetingsName, pastMeetingsName, "/?past=yes")
                      
      splices :: Splices (SnapletISplice App)
      splices = do
        "meetings"          ## (meetingsSplice meetings)
        "calendarName"      ## I.textSplice calendarName
        "otherCalendarName" ## I.textSplice otherCalendarName
        "otherCalendarURL"  ## I.textSplice otherCalendarURL
        "coopName"          ## I.textSplice $ T.pack $ title calendar

  renderWithSplices "home" splices

  where
    meetingsSplice :: [Meeting] -> SnapletISplice App
    meetingsSplice = I.mapSplices $ I.runChildrenWith . meetingSplices

    meetingSplices :: Monad n => Meeting -> Splices (I.Splice n)
    meetingSplices meeting = do
      "meetingURL"   ## I.textSplice $ meetingURL meeting
      "meetingName"  ## I.textSplice $ meetingName meeting
 
------------------------------------------------------------------------------
-- | Handle meeting
handleMeeting :: Handler App (AuthManager App) ()
handleMeeting = do
  user            <- currentUser
  calendar   <- liftIO $ readCalendar $ calendarFileNameForAuthUser user
  editParam       <- getPar "edit"
  date            <- getDateFromParams

  let Just meeting = findMeeting date $ meetings calendar
      editPerson = fromMaybe "" editParam

      splices :: Splices (SnapletISplice App)
      splices = do
        "slots"       ## slotsSplice $ slots meeting
        "meetingName" ## I.textSplice $ meetingName meeting
        "meetingURL"  ## I.textSplice $ meetingURL meeting
        "editPerson"  ## I.textSplice $ T.pack $ editPerson
        "coopName"    ## I.textSplice $ T.pack $ title calendar
        "ifEditing"   ## ifEditing

      slotsSplice :: [Slot] -> SnapletISplice App
      slotsSplice = I.mapSplices $ I.runChildrenWith . slotSplices

      slotSplices :: Monad m => Slot -> Splices (I.Splice m)
      slotSplices slot = do
        "slotClass"        ## I.textSplice $ slotClass slot
        "slotPerson"       ## I.textSplice $ T.pack $ person slot
        "slotAttendance"   ## I.textSplice $ T.pack $ show $ attendance slot
        "ifSlotViewing"    ## ifSlotViewing slot
        "ifSlotEditing"    ## ifSlotEditing slot
        "selectAttendance" ## selectAttendance slot

      ifSlotViewing :: Monad m => Slot -> I.Splice m
      ifSlotViewing slot = 
        ifISplice $ editPerson /= person slot

      ifSlotEditing :: Monad m => Slot -> I.Splice m
      ifSlotEditing slot = 
        ifISplice $ editPerson == person slot

      ifEditing :: Monad m => I.Splice m
      ifEditing = 
        ifISplice $ editParam /= Nothing

      selectAttendance :: Monad m => Slot -> I.Splice m
      selectAttendance slot = selectSplice name name options (Just $ attendanceName $ attendance slot)
        where name = T.pack "attendance"
              options = map (\x -> (attendanceName x, attendanceName x)) attendanceValues
              attendanceName attendance = T.pack $ show attendance
              attendanceValues = [minBound .. maxBound] :: [Attendance]

      slotClass :: Slot -> T.Text
      slotClass slot =  case status slot of
                          Proposed  -> "proposed"
                          Confirmed -> "confirmed"
                          Requested -> "requested"

  renderWithSplices "meeting" splices
------------------------------------------------------------------------------
-- | Handle meeting edit
handleMeetingEdit :: Handler App (AuthManager App) ()
handleMeetingEdit = do
  user       <- currentUser
  let calendarFileName = calendarFileNameForAuthUser user
  calendar   <- liftIO $ readCalendar calendarFileName
  date            <- getDateFromParams
  person          <- (getPar "person")     >>= return . fromMaybe ""
  attendance      <- (getPar "attendance") >>= return . fromMaybe ""
  let Just meeting = findMeeting date $ meetings calendar
      meetingUpdates = [(person, read attendance :: Attendance)]
  liftIO $ updateCalendar calendarFileName date meetingUpdates
  liftIO $ system "bin/sync"
  redirect $ ES.encodeUtf8 $ meetingURL meeting

------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [ ("",                           with auth (ifTop handleHome))
         , ("/meeting/:year/:month/:day", with auth (method GET handleMeeting))
         , ("/meeting/:year/:month/:day", with auth (method POST handleMeetingEdit))
         , ("/login",                     with auth handleLoginSubmit)
         , ("/logout",                    with auth handleLogout)
         , ("/new_user",                  with auth handleNewUser)
         , ("",                           serveDirectory "static")
         ]


------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "SLAM Coop" Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    s <- nestSnaplet "sess" sess $
           initCookieSessionManager "site_key.txt" "sess" (Just 3600)
    a <- nestSnaplet "auth" auth $
           initJsonFileAuthManager defAuthSettings sess "data/users.json"
    addRoutes routes
    addAuthSplices h auth
    return $ App h s a

------------------------------------------------------------------------------

meetingURL :: Meeting -> T.Text
meetingURL (Meeting (year, month, day) _) = T.pack $ "/meeting/" ++ (show year) ++ "/" ++ (show month) ++ "/" ++ (show day)

meetingName :: Meeting -> T.Text
meetingName (Meeting (_, month, day) _) =  T.pack $ (show month) ++ "/" ++ (show day)

getPar :: String -> Handler App (AuthManager App) (Maybe String)
getPar name = do
  p <- getParam $ ES.encodeUtf8 $ T.pack $ name
  return $ fmap (T.unpack . ES.decodeUtf8) p

getDateFromParams :: Handler App (AuthManager App) (Date)
getDateFromParams = do
  yearParam  <- (getPar "year")  >>= return . fromMaybe ""
  monthParam <- (getPar "month") >>= return . fromMaybe ""
  dayParam   <- (getPar "day")   >>= return . fromMaybe ""

  return (read yearParam, read monthParam, read dayParam)

calendarFileNameForAuthUser :: Maybe AuthUser -> String
calendarFileNameForAuthUser user = calendarFileNameForUser $ head $ calendarsForAuthUser user

calendarsForAuthUser :: Maybe AuthUser -> [String]
calendarsForAuthUser user = 
  case fromJSON $ meta HM.! "calendars" of 
    Error msg -> []
    Success calendars -> calendars
  where meta = userMeta $ fromJust user

isAdminUser :: Maybe AuthUser -> Bool
isAdminUser user = (userLogin $ fromJust user) == "admin"

