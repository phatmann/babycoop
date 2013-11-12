module Requests where

import Scheduler

theRequests :: Calendar
theRequests = [
   Meeting (2013, 11, 4)   [slot Rebecca In Requested
                           ,slot Neha Absent Requested
                           ,slot Erica Absent Requested
                           ,slot Jenny Absent Requested]
  ,Meeting (2013, 11, 11)  [slot Kasey Out Requested
                           ,slot Kate In Requested]
  ,Meeting (2013, 11, 18)  [slot Rebecca Absent Requested]
  ,Meeting (2013, 12, 2)   [slot Jenny Absent Requested]]