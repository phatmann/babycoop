module Calendar where

import Scheduler

theCalendar :: Calendar
theCalendar =
  [ Meeting
      { date = ( 2013 , 10 , 7 )
      , slots =
          [ Slot
              { person = Kate
              , attendance = In
              , status = Confirmed
              , stat =
                  Stat
                    { inDates = []
                    , outDates = []
                    , hostDates = []
                    , absentDates = []
                    }
              , rank = 0
              }
          , Slot
              { person = Erica
              , attendance = In
              , status = Confirmed
              , stat =
                  Stat
                    { inDates = []
                    , outDates = []
                    , hostDates = []
                    , absentDates = []
                    }
              , rank = 0
              }
          , Slot
              { person = Rebecca
              , attendance = Out
              , status = Confirmed
              , stat =
                  Stat
                    { inDates = []
                    , outDates = []
                    , hostDates = []
                    , absentDates = []
                    }
              , rank = 0
              }
          , Slot
              { person = Jenny
              , attendance = Host
              , status = Confirmed
              , stat =
                  Stat
                    { inDates = []
                    , outDates = []
                    , hostDates = []
                    , absentDates = []
                    }
              , rank = 0
              }
          , Slot
              { person = Kasey
              , attendance = Absent
              , status = Confirmed
              , stat =
                  Stat
                    { inDates = []
                    , outDates = []
                    , hostDates = []
                    , absentDates = []
                    }
              , rank = 0
              }
          , Slot
              { person = Neha
              , attendance = Absent
              , status = Confirmed
              , stat =
                  Stat
                    { inDates = []
                    , outDates = []
                    , hostDates = []
                    , absentDates = []
                    }
              , rank = 0
              }
          ]
      }
  , Meeting
      { date = ( 2013 , 10 , 14 )
      , slots =
          [ Slot
              { person = Kasey
              , attendance = In
              , status = Confirmed
              , stat =
                  Stat
                    { inDates = []
                    , outDates = []
                    , hostDates = []
                    , absentDates = [ ( 2013 , 10 , 7 ) ]
                    }
              , rank = 0
              }
          , Slot
              { person = Neha
              , attendance = In
              , status = Confirmed
              , stat =
                  Stat
                    { inDates = []
                    , outDates = []
                    , hostDates = []
                    , absentDates = [ ( 2013 , 10 , 7 ) ]
                    }
              , rank = 0
              }
          , Slot
              { person = Jenny
              , attendance = Out
              , status = Confirmed
              , stat =
                  Stat
                    { inDates = [ ( 2013 , 10 , 7 ) ]
                    , outDates = []
                    , hostDates = [ ( 2013 , 10 , 7 ) ]
                    , absentDates = []
                    }
              , rank = 0
              }
          , Slot
              { person = Kate
              , attendance = Out
              , status = Confirmed
              , stat =
                  Stat
                    { inDates = [ ( 2013 , 10 , 7 ) ]
                    , outDates = []
                    , hostDates = []
                    , absentDates = []
                    }
              , rank = 0
              }
          , Slot
              { person = Erica
              , attendance = Out
              , status = Confirmed
              , stat =
                  Stat
                    { inDates = [ ( 2013 , 10 , 7 ) ]
                    , outDates = []
                    , hostDates = []
                    , absentDates = []
                    }
              , rank = 0
              }
          , Slot
              { person = Rebecca
              , attendance = Host
              , status = Confirmed
              , stat =
                  Stat
                    { inDates = []
                    , outDates = [ ( 2013 , 10 , 7 ) ]
                    , hostDates = []
                    , absentDates = []
                    }
              , rank = 0
              }
          ]
      }
  , Meeting
      { date = ( 2013 , 10 , 21 )
      , slots =
          [ Slot
              { person = Jenny
              , attendance = In
              , status = Confirmed
              , stat =
                  Stat
                    { inDates = [ ( 2013 , 10 , 7 ) ]
                    , outDates = [ ( 2013 , 10 , 14 ) ]
                    , hostDates = [ ( 2013 , 10 , 7 ) ]
                    , absentDates = []
                    }
              , rank = 0
              }
          , Slot
              { person = Erica
              , attendance = In
              , status = Confirmed
              , stat =
                  Stat
                    { inDates = [ ( 2013 , 10 , 7 ) ]
                    , outDates = [ ( 2013 , 10 , 14 ) ]
                    , hostDates = []
                    , absentDates = []
                    }
              , rank = 0
              }
          , Slot
              { person = Rebecca
              , attendance = Out
              , status = Confirmed
              , stat =
                  Stat
                    { inDates = [ ( 2013 , 10 , 14 ) ]
                    , outDates = [ ( 2013 , 10 , 7 ) ]
                    , hostDates = [ ( 2013 , 10 , 14 ) ]
                    , absentDates = []
                    }
              , rank = 0
              }
          , Slot
              { person = Kasey
              , attendance = Out
              , status = Confirmed
              , stat =
                  Stat
                    { inDates = [ ( 2013 , 10 , 14 ) ]
                    , outDates = []
                    , hostDates = []
                    , absentDates = [ ( 2013 , 10 , 7 ) ]
                    }
              , rank = 0
              }
          , Slot
              { person = Neha
              , attendance = Out
              , status = Confirmed
              , stat =
                  Stat
                    { inDates = [ ( 2013 , 10 , 14 ) ]
                    , outDates = []
                    , hostDates = []
                    , absentDates = [ ( 2013 , 10 , 7 ) ]
                    }
              , rank = 0
              }
          , Slot
              { person = Kate
              , attendance = Host
              , status = Confirmed
              , stat =
                  Stat
                    { inDates = [ ( 2013 , 10 , 7 ) ]
                    , outDates = [ ( 2013 , 10 , 14 ) ]
                    , hostDates = []
                    , absentDates = []
                    }
              , rank = 0
              }
          ]
      }
  , Meeting
      { date = ( 2013 , 10 , 28 )
      , slots =
          [ Slot
              { person = Rebecca
              , attendance = In
              , status = Confirmed
              , stat =
                  Stat
                    { inDates = [ ( 2013 , 10 , 14 ) ]
                    , outDates = [ ( 2013 , 10 , 7 ) , ( 2013 , 10 , 21 ) ]
                    , hostDates = [ ( 2013 , 10 , 14 ) ]
                    , absentDates = []
                    }
              , rank = 0
              }
          , Slot
              { person = Kate
              , attendance = In
              , status = Confirmed
              , stat =
                  Stat
                    { inDates = [ ( 2013 , 10 , 7 ) , ( 2013 , 10 , 21 ) ]
                    , outDates = [ ( 2013 , 10 , 14 ) ]
                    , hostDates = [ ( 2013 , 10 , 21 ) ]
                    , absentDates = []
                    }
              , rank = 0
              }
          , Slot
              { person = Jenny
              , attendance = Out
              , status = Confirmed
              , stat =
                  Stat
                    { inDates = [ ( 2013 , 10 , 7 ) , ( 2013 , 10 , 21 ) ]
                    , outDates = [ ( 2013 , 10 , 14 ) ]
                    , hostDates = [ ( 2013 , 10 , 7 ) ]
                    , absentDates = []
                    }
              , rank = 0
              }
          , Slot
              { person = Kasey
              , attendance = Out
              , status = Confirmed
              , stat =
                  Stat
                    { inDates = [ ( 2013 , 10 , 14 ) ]
                    , outDates = [ ( 2013 , 10 , 21 ) ]
                    , hostDates = []
                    , absentDates = [ ( 2013 , 10 , 7 ) ]
                    }
              , rank = 0
              }
          , Slot
              { person = Erica
              , attendance = Out
              , status = Confirmed
              , stat =
                  Stat
                    { inDates = [ ( 2013 , 10 , 7 ) , ( 2013 , 10 , 21 ) ]
                    , outDates = [ ( 2013 , 10 , 14 ) ]
                    , hostDates = []
                    , absentDates = []
                    }
              , rank = 0
              }
          , Slot
              { person = Neha
              , attendance = Host
              , status = Confirmed
              , stat =
                  Stat
                    { inDates = [ ( 2013 , 10 , 14 ) ]
                    , outDates = [ ( 2013 , 10 , 21 ) ]
                    , hostDates = []
                    , absentDates = [ ( 2013 , 10 , 7 ) ]
                    }
              , rank = 0
              }
          ]
      }
  , Meeting
      { date = ( 2013 , 11 , 4 )
      , slots =
          [ Slot
              { person = Rebecca
              , attendance = In
              , status = Requested
              , stat =
                  Stat
                    { inDates = [ ( 2013 , 10 , 14 ) , ( 2013 , 10 , 28 ) ]
                    , outDates = [ ( 2013 , 10 , 7 ) , ( 2013 , 10 , 21 ) ]
                    , hostDates = [ ( 2013 , 10 , 14 ) ]
                    , absentDates = []
                    }
              , rank = 0
              }
          , Slot
              { person = Kate
              , attendance = Out
              , status = Confirmed
              , stat =
                  Stat
                    { inDates =
                        [ ( 2013 , 10 , 7 ) , ( 2013 , 10 , 21 ) , ( 2013 , 10 , 28 ) ]
                    , outDates = [ ( 2013 , 10 , 14 ) ]
                    , hostDates = [ ( 2013 , 10 , 21 ) ]
                    , absentDates = []
                    }
              , rank = 0
              }
          , Slot
              { person = Kasey
              , attendance = Host
              , status = Confirmed
              , stat =
                  Stat
                    { inDates = [ ( 2013 , 10 , 14 ) ]
                    , outDates = [ ( 2013 , 10 , 21 ) , ( 2013 , 10 , 28 ) ]
                    , hostDates = []
                    , absentDates = [ ( 2013 , 10 , 7 ) ]
                    }
              , rank = 0
              }
          , Slot
              { person = Jenny
              , attendance = Absent
              , status = Requested
              , stat =
                  Stat
                    { inDates = [ ( 2013 , 10 , 7 ) , ( 2013 , 10 , 21 ) ]
                    , outDates = [ ( 2013 , 10 , 14 ) , ( 2013 , 10 , 28 ) ]
                    , hostDates = [ ( 2013 , 10 , 7 ) ]
                    , absentDates = []
                    }
              , rank = 0
              }
          , Slot
              { person = Neha
              , attendance = Absent
              , status = Requested
              , stat =
                  Stat
                    { inDates = [ ( 2013 , 10 , 14 ) , ( 2013 , 10 , 28 ) ]
                    , outDates = [ ( 2013 , 10 , 21 ) ]
                    , hostDates = [ ( 2013 , 10 , 28 ) ]
                    , absentDates = [ ( 2013 , 10 , 7 ) ]
                    }
              , rank = 0
              }
          , Slot
              { person = Erica
              , attendance = Absent
              , status = Requested
              , stat =
                  Stat
                    { inDates = [ ( 2013 , 10 , 7 ) , ( 2013 , 10 , 21 ) ]
                    , outDates = [ ( 2013 , 10 , 14 ) , ( 2013 , 10 , 28 ) ]
                    , hostDates = []
                    , absentDates = []
                    }
              , rank = 0
              }
          ]
      }
  , Meeting
      { date = ( 2013 , 11 , 11 )
      , slots =
          [ Slot
              { person = Jenny
              , attendance = In
              , status = Confirmed
              , stat =
                  Stat
                    { inDates = [ ( 2013 , 10 , 7 ) , ( 2013 , 10 , 21 ) ]
                    , outDates = [ ( 2013 , 10 , 14 ) , ( 2013 , 10 , 28 ) ]
                    , hostDates = [ ( 2013 , 10 , 7 ) ]
                    , absentDates = [ ( 2013 , 11 , 4 ) ]
                    }
              , rank = 0
              }
          , Slot
              { person = Kate
              , attendance = In
              , status = Requested
              , stat =
                  Stat
                    { inDates =
                        [ ( 2013 , 10 , 7 ) , ( 2013 , 10 , 21 ) , ( 2013 , 10 , 28 ) ]
                    , outDates = [ ( 2013 , 10 , 14 ) , ( 2013 , 11 , 4 ) ]
                    , hostDates = [ ( 2013 , 10 , 21 ) ]
                    , absentDates = []
                    }
              , rank = 0
              }
          , Slot
              { person = Rebecca
              , attendance = Out
              , status = Confirmed
              , stat =
                  Stat
                    { inDates =
                        [ ( 2013 , 10 , 14 ) , ( 2013 , 10 , 28 ) , ( 2013 , 11 , 4 ) ]
                    , outDates = [ ( 2013 , 10 , 7 ) , ( 2013 , 10 , 21 ) ]
                    , hostDates = [ ( 2013 , 10 , 14 ) ]
                    , absentDates = []
                    }
              , rank = 0
              }
          , Slot
              { person = Kasey
              , attendance = Out
              , status = Requested
              , stat =
                  Stat
                    { inDates = [ ( 2013 , 10 , 14 ) , ( 2013 , 11 , 4 ) ]
                    , outDates = [ ( 2013 , 10 , 21 ) , ( 2013 , 10 , 28 ) ]
                    , hostDates = [ ( 2013 , 11 , 4 ) ]
                    , absentDates = [ ( 2013 , 10 , 7 ) ]
                    }
              , rank = 0
              }
          , Slot
              { person = Neha
              , attendance = Out
              , status = Confirmed
              , stat =
                  Stat
                    { inDates = [ ( 2013 , 10 , 14 ) , ( 2013 , 10 , 28 ) ]
                    , outDates = [ ( 2013 , 10 , 21 ) ]
                    , hostDates = [ ( 2013 , 10 , 28 ) ]
                    , absentDates = [ ( 2013 , 10 , 7 ) , ( 2013 , 11 , 4 ) ]
                    }
              , rank = 0
              }
          , Slot
              { person = Erica
              , attendance = Host
              , status = Confirmed
              , stat =
                  Stat
                    { inDates = [ ( 2013 , 10 , 7 ) , ( 2013 , 10 , 21 ) ]
                    , outDates = [ ( 2013 , 10 , 14 ) , ( 2013 , 10 , 28 ) ]
                    , hostDates = []
                    , absentDates = [ ( 2013 , 11 , 4 ) ]
                    }
              , rank = 0
              }
          ]
      }
  ]