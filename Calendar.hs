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
  , Meeting
      { date = ( 2013 , 11 , 18 )
      , slots =
          [ Slot
              { person = Neha
              , attendance = In
              , status = Proposed
              , stat =
                  Stat
                    { inDates = [ ( 2013 , 10 , 14 ) , ( 2013 , 10 , 28 ) ]
                    , outDates = [ ( 2013 , 10 , 21 ) , ( 2013 , 11 , 11 ) ]
                    , hostDates = [ ( 2013 , 10 , 28 ) ]
                    , absentDates = [ ( 2013 , 10 , 7 ) , ( 2013 , 11 , 4 ) ]
                    }
              , rank = 82
              }
          , Slot
              { person = Kasey
              , attendance = In
              , status = Proposed
              , stat =
                  Stat
                    { inDates = [ ( 2013 , 10 , 14 ) , ( 2013 , 11 , 4 ) ]
                    , outDates =
                        [ ( 2013 , 10 , 21 ) , ( 2013 , 10 , 28 ) , ( 2013 , 11 , 11 ) ]
                    , hostDates = [ ( 2013 , 11 , 4 ) ]
                    , absentDates = [ ( 2013 , 10 , 7 ) ]
                    }
              , rank = 92
              }
          , Slot
              { person = Erica
              , attendance = Out
              , status = Proposed
              , stat =
                  Stat
                    { inDates =
                        [ ( 2013 , 10 , 7 ) , ( 2013 , 10 , 21 ) , ( 2013 , 11 , 11 ) ]
                    , outDates = [ ( 2013 , 10 , 14 ) , ( 2013 , 10 , 28 ) ]
                    , hostDates = [ ( 2013 , 11 , 11 ) ]
                    , absentDates = [ ( 2013 , 11 , 4 ) ]
                    }
              , rank = 93
              }
          , Slot
              { person = Kate
              , attendance = Out
              , status = Proposed
              , stat =
                  Stat
                    { inDates =
                        [ ( 2013 , 10 , 7 )
                        , ( 2013 , 10 , 21 )
                        , ( 2013 , 10 , 28 )
                        , ( 2013 , 11 , 11 )
                        ]
                    , outDates = [ ( 2013 , 10 , 14 ) , ( 2013 , 11 , 4 ) ]
                    , hostDates = [ ( 2013 , 10 , 21 ) ]
                    , absentDates = []
                    }
              , rank = 11
              }
          , Slot
              { person = Rebecca
              , attendance = Out
              , status = Proposed
              , stat =
                  Stat
                    { inDates =
                        [ ( 2013 , 10 , 14 ) , ( 2013 , 10 , 28 ) , ( 2013 , 11 , 4 ) ]
                    , outDates =
                        [ ( 2013 , 10 , 7 ) , ( 2013 , 10 , 21 ) , ( 2013 , 11 , 11 ) ]
                    , hostDates = [ ( 2013 , 10 , 14 ) ]
                    , absentDates = []
                    }
              , rank = 90
              }
          , Slot
              { person = Jenny
              , attendance = Host
              , status = Proposed
              , stat =
                  Stat
                    { inDates =
                        [ ( 2013 , 10 , 7 ) , ( 2013 , 10 , 21 ) , ( 2013 , 11 , 11 ) ]
                    , outDates = [ ( 2013 , 10 , 14 ) , ( 2013 , 10 , 28 ) ]
                    , hostDates = [ ( 2013 , 10 , 7 ) ]
                    , absentDates = [ ( 2013 , 11 , 4 ) ]
                    }
              , rank = 14
              }
          ]
      }
  , Meeting
      { date = ( 2013 , 11 , 25 )
      , slots =
          [ Slot
              { person = Kasey
              , attendance = In
              , status = Proposed
              , stat =
                  Stat
                    { inDates =
                        [ ( 2013 , 10 , 14 ) , ( 2013 , 11 , 4 ) , ( 2013 , 11 , 18 ) ]
                    , outDates =
                        [ ( 2013 , 10 , 21 ) , ( 2013 , 10 , 28 ) , ( 2013 , 11 , 11 ) ]
                    , hostDates = [ ( 2013 , 11 , 4 ) ]
                    , absentDates = []
                    }
              , rank = 63
              }
          , Slot
              { person = Erica
              , attendance = In
              , status = Proposed
              , stat =
                  Stat
                    { inDates = [ ( 2013 , 10 , 21 ) , ( 2013 , 11 , 11 ) ]
                    , outDates =
                        [ ( 2013 , 10 , 14 ) , ( 2013 , 10 , 28 ) , ( 2013 , 11 , 18 ) ]
                    , hostDates = [ ( 2013 , 11 , 11 ) ]
                    , absentDates = [ ( 2013 , 11 , 4 ) ]
                    }
              , rank = 92
              }
          , Slot
              { person = Neha
              , attendance = Out
              , status = Proposed
              , stat =
                  Stat
                    { inDates =
                        [ ( 2013 , 10 , 14 ) , ( 2013 , 10 , 28 ) , ( 2013 , 11 , 18 ) ]
                    , outDates = [ ( 2013 , 10 , 21 ) , ( 2013 , 11 , 11 ) ]
                    , hostDates = [ ( 2013 , 10 , 28 ) ]
                    , absentDates = [ ( 2013 , 11 , 4 ) ]
                    }
              , rank = 76
              }
          , Slot
              { person = Jenny
              , attendance = Out
              , status = Proposed
              , stat =
                  Stat
                    { inDates =
                        [ ( 2013 , 10 , 21 ) , ( 2013 , 11 , 11 ) , ( 2013 , 11 , 18 ) ]
                    , outDates = [ ( 2013 , 10 , 14 ) , ( 2013 , 10 , 28 ) ]
                    , hostDates = [ ( 2013 , 11 , 18 ) ]
                    , absentDates = [ ( 2013 , 11 , 4 ) ]
                    }
              , rank = 21
              }
          , Slot
              { person = Kate
              , attendance = Out
              , status = Proposed
              , stat =
                  Stat
                    { inDates =
                        [ ( 2013 , 10 , 21 ) , ( 2013 , 10 , 28 ) , ( 2013 , 11 , 11 ) ]
                    , outDates =
                        [ ( 2013 , 10 , 14 ) , ( 2013 , 11 , 4 ) , ( 2013 , 11 , 18 ) ]
                    , hostDates = [ ( 2013 , 10 , 21 ) ]
                    , absentDates = []
                    }
              , rank = 86
              }
          , Slot
              { person = Rebecca
              , attendance = Host
              , status = Proposed
              , stat =
                  Stat
                    { inDates =
                        [ ( 2013 , 10 , 14 ) , ( 2013 , 10 , 28 ) , ( 2013 , 11 , 4 ) ]
                    , outDates =
                        [ ( 2013 , 10 , 21 ) , ( 2013 , 11 , 11 ) , ( 2013 , 11 , 18 ) ]
                    , hostDates = [ ( 2013 , 10 , 14 ) ]
                    , absentDates = []
                    }
              , rank = 28
              }
          ]
      }
  , Meeting
      { date = ( 2013 , 12 , 2 )
      , slots =
          [ Slot
              { person = Rebecca
              , attendance = In
              , status = Proposed
              , stat =
                  Stat
                    { inDates =
                        [ ( 2013 , 10 , 28 ) , ( 2013 , 11 , 4 ) , ( 2013 , 11 , 25 ) ]
                    , outDates =
                        [ ( 2013 , 10 , 21 ) , ( 2013 , 11 , 11 ) , ( 2013 , 11 , 18 ) ]
                    , hostDates = [ ( 2013 , 11 , 25 ) ]
                    , absentDates = []
                    }
              , rank = 73
              }
          , Slot
              { person = Erica
              , attendance = Out
              , status = Proposed
              , stat =
                  Stat
                    { inDates =
                        [ ( 2013 , 10 , 21 ) , ( 2013 , 11 , 11 ) , ( 2013 , 11 , 25 ) ]
                    , outDates = [ ( 2013 , 10 , 28 ) , ( 2013 , 11 , 18 ) ]
                    , hostDates = [ ( 2013 , 11 , 11 ) ]
                    , absentDates = [ ( 2013 , 11 , 4 ) ]
                    }
              , rank = 38
              }
          , Slot
              { person = Neha
              , attendance = Out
              , status = Proposed
              , stat =
                  Stat
                    { inDates = [ ( 2013 , 10 , 28 ) , ( 2013 , 11 , 18 ) ]
                    , outDates =
                        [ ( 2013 , 10 , 21 ) , ( 2013 , 11 , 11 ) , ( 2013 , 11 , 25 ) ]
                    , hostDates = [ ( 2013 , 10 , 28 ) ]
                    , absentDates = [ ( 2013 , 11 , 4 ) ]
                    }
              , rank = 39
              }
          , Slot
              { person = Kasey
              , attendance = Out
              , status = Proposed
              , stat =
                  Stat
                    { inDates =
                        [ ( 2013 , 11 , 4 ) , ( 2013 , 11 , 18 ) , ( 2013 , 11 , 25 ) ]
                    , outDates =
                        [ ( 2013 , 10 , 21 ) , ( 2013 , 10 , 28 ) , ( 2013 , 11 , 11 ) ]
                    , hostDates = [ ( 2013 , 11 , 4 ) ]
                    , absentDates = []
                    }
              , rank = 36
              }
          , Slot
              { person = Kate
              , attendance = Host
              , status = Proposed
              , stat =
                  Stat
                    { inDates =
                        [ ( 2013 , 10 , 21 ) , ( 2013 , 10 , 28 ) , ( 2013 , 11 , 11 ) ]
                    , outDates =
                        [ ( 2013 , 11 , 4 ) , ( 2013 , 11 , 18 ) , ( 2013 , 11 , 25 ) ]
                    , hostDates = [ ( 2013 , 10 , 21 ) ]
                    , absentDates = []
                    }
              , rank = 54
              }
          , Slot
              { person = Jenny
              , attendance = Absent
              , status = Requested
              , stat =
                  Stat
                    { inDates =
                        [ ( 2013 , 10 , 21 ) , ( 2013 , 11 , 11 ) , ( 2013 , 11 , 18 ) ]
                    , outDates = [ ( 2013 , 10 , 28 ) , ( 2013 , 11 , 25 ) ]
                    , hostDates = [ ( 2013 , 11 , 18 ) ]
                    , absentDates = [ ( 2013 , 11 , 4 ) ]
                    }
              , rank = 70
              }
          ]
      }
  , Meeting
      { date = ( 2013 , 12 , 9 )
      , slots =
          [ Slot
              { person = Jenny
              , attendance = In
              , status = Proposed
              , stat =
                  Stat
                    { inDates = [ ( 2013 , 11 , 11 ) , ( 2013 , 11 , 18 ) ]
                    , outDates = [ ( 2013 , 10 , 28 ) , ( 2013 , 11 , 25 ) ]
                    , hostDates = [ ( 2013 , 11 , 18 ) ]
                    , absentDates = [ ( 2013 , 11 , 4 ) , ( 2013 , 12 , 2 ) ]
                    }
              , rank = 49
              }
          , Slot
              { person = Kate
              , attendance = In
              , status = Proposed
              , stat =
                  Stat
                    { inDates =
                        [ ( 2013 , 10 , 28 ) , ( 2013 , 11 , 11 ) , ( 2013 , 12 , 2 ) ]
                    , outDates =
                        [ ( 2013 , 11 , 4 ) , ( 2013 , 11 , 18 ) , ( 2013 , 11 , 25 ) ]
                    , hostDates = [ ( 2013 , 12 , 2 ) ]
                    , absentDates = []
                    }
              , rank = 85
              }
          , Slot
              { person = Rebecca
              , attendance = Out
              , status = Proposed
              , stat =
                  Stat
                    { inDates =
                        [ ( 2013 , 10 , 28 )
                        , ( 2013 , 11 , 4 )
                        , ( 2013 , 11 , 25 )
                        , ( 2013 , 12 , 2 )
                        ]
                    , outDates = [ ( 2013 , 11 , 11 ) , ( 2013 , 11 , 18 ) ]
                    , hostDates = [ ( 2013 , 11 , 25 ) ]
                    , absentDates = []
                    }
              , rank = 23
              }
          , Slot
              { person = Kasey
              , attendance = Out
              , status = Proposed
              , stat =
                  Stat
                    { inDates =
                        [ ( 2013 , 11 , 4 ) , ( 2013 , 11 , 18 ) , ( 2013 , 11 , 25 ) ]
                    , outDates =
                        [ ( 2013 , 10 , 28 ) , ( 2013 , 11 , 11 ) , ( 2013 , 12 , 2 ) ]
                    , hostDates = [ ( 2013 , 11 , 4 ) ]
                    , absentDates = []
                    }
              , rank = 3
              }
          , Slot
              { person = Erica
              , attendance = Out
              , status = Proposed
              , stat =
                  Stat
                    { inDates = [ ( 2013 , 11 , 11 ) , ( 2013 , 11 , 25 ) ]
                    , outDates =
                        [ ( 2013 , 10 , 28 ) , ( 2013 , 11 , 18 ) , ( 2013 , 12 , 2 ) ]
                    , hostDates = [ ( 2013 , 11 , 11 ) ]
                    , absentDates = [ ( 2013 , 11 , 4 ) ]
                    }
              , rank = 31
              }
          , Slot
              { person = Neha
              , attendance = Host
              , status = Proposed
              , stat =
                  Stat
                    { inDates = [ ( 2013 , 10 , 28 ) , ( 2013 , 11 , 18 ) ]
                    , outDates =
                        [ ( 2013 , 11 , 11 ) , ( 2013 , 11 , 25 ) , ( 2013 , 12 , 2 ) ]
                    , hostDates = [ ( 2013 , 10 , 28 ) ]
                    , absentDates = [ ( 2013 , 11 , 4 ) ]
                    }
              , rank = 86
              }
          ]
      }
  , Meeting
      { date = ( 2013 , 12 , 16 )
      , slots =
          [ Slot
              { person = Kate
              , attendance = In
              , status = Proposed
              , stat =
                  Stat
                    { inDates =
                        [ ( 2013 , 11 , 11 ) , ( 2013 , 12 , 2 ) , ( 2013 , 12 , 9 ) ]
                    , outDates =
                        [ ( 2013 , 11 , 4 ) , ( 2013 , 11 , 18 ) , ( 2013 , 11 , 25 ) ]
                    , hostDates = [ ( 2013 , 12 , 2 ) ]
                    , absentDates = []
                    }
              , rank = 84
              }
          , Slot
              { person = Neha
              , attendance = In
              , status = Proposed
              , stat =
                  Stat
                    { inDates = [ ( 2013 , 11 , 18 ) , ( 2013 , 12 , 9 ) ]
                    , outDates =
                        [ ( 2013 , 11 , 11 ) , ( 2013 , 11 , 25 ) , ( 2013 , 12 , 2 ) ]
                    , hostDates = [ ( 2013 , 12 , 9 ) ]
                    , absentDates = [ ( 2013 , 11 , 4 ) ]
                    }
              , rank = 81
              }
          , Slot
              { person = Jenny
              , attendance = Out
              , status = Proposed
              , stat =
                  Stat
                    { inDates =
                        [ ( 2013 , 11 , 11 ) , ( 2013 , 11 , 18 ) , ( 2013 , 12 , 9 ) ]
                    , outDates = [ ( 2013 , 11 , 25 ) ]
                    , hostDates = [ ( 2013 , 11 , 18 ) ]
                    , absentDates = [ ( 2013 , 11 , 4 ) , ( 2013 , 12 , 2 ) ]
                    }
              , rank = 91
              }
          , Slot
              { person = Erica
              , attendance = Out
              , status = Proposed
              , stat =
                  Stat
                    { inDates = [ ( 2013 , 11 , 11 ) , ( 2013 , 11 , 25 ) ]
                    , outDates =
                        [ ( 2013 , 11 , 18 ) , ( 2013 , 12 , 2 ) , ( 2013 , 12 , 9 ) ]
                    , hostDates = [ ( 2013 , 11 , 11 ) ]
                    , absentDates = [ ( 2013 , 11 , 4 ) ]
                    }
              , rank = 33
              }
          , Slot
              { person = Rebecca
              , attendance = Out
              , status = Proposed
              , stat =
                  Stat
                    { inDates =
                        [ ( 2013 , 11 , 4 ) , ( 2013 , 11 , 25 ) , ( 2013 , 12 , 2 ) ]
                    , outDates =
                        [ ( 2013 , 11 , 11 ) , ( 2013 , 11 , 18 ) , ( 2013 , 12 , 9 ) ]
                    , hostDates = [ ( 2013 , 11 , 25 ) ]
                    , absentDates = []
                    }
              , rank = 3
              }
          , Slot
              { person = Kasey
              , attendance = Host
              , status = Proposed
              , stat =
                  Stat
                    { inDates =
                        [ ( 2013 , 11 , 4 ) , ( 2013 , 11 , 18 ) , ( 2013 , 11 , 25 ) ]
                    , outDates =
                        [ ( 2013 , 11 , 11 ) , ( 2013 , 12 , 2 ) , ( 2013 , 12 , 9 ) ]
                    , hostDates = [ ( 2013 , 11 , 4 ) ]
                    , absentDates = []
                    }
              , rank = 29
              }
          ]
      }
  , Meeting
      { date = ( 2013 , 12 , 23 )
      , slots =
          [ Slot
              { person = Neha
              , attendance = In
              , status = Proposed
              , stat =
                  Stat
                    { inDates =
                        [ ( 2013 , 11 , 18 ) , ( 2013 , 12 , 9 ) , ( 2013 , 12 , 16 ) ]
                    , outDates =
                        [ ( 2013 , 11 , 11 ) , ( 2013 , 11 , 25 ) , ( 2013 , 12 , 2 ) ]
                    , hostDates = [ ( 2013 , 12 , 9 ) ]
                    , absentDates = []
                    }
              , rank = 84
              }
          , Slot
              { person = Kasey
              , attendance = In
              , status = Proposed
              , stat =
                  Stat
                    { inDates =
                        [ ( 2013 , 11 , 18 ) , ( 2013 , 11 , 25 ) , ( 2013 , 12 , 16 ) ]
                    , outDates =
                        [ ( 2013 , 11 , 11 ) , ( 2013 , 12 , 2 ) , ( 2013 , 12 , 9 ) ]
                    , hostDates = [ ( 2013 , 12 , 16 ) ]
                    , absentDates = []
                    }
              , rank = 3
              }
          , Slot
              { person = Jenny
              , attendance = Out
              , status = Proposed
              , stat =
                  Stat
                    { inDates =
                        [ ( 2013 , 11 , 11 ) , ( 2013 , 11 , 18 ) , ( 2013 , 12 , 9 ) ]
                    , outDates = [ ( 2013 , 11 , 25 ) , ( 2013 , 12 , 16 ) ]
                    , hostDates = [ ( 2013 , 11 , 18 ) ]
                    , absentDates = [ ( 2013 , 12 , 2 ) ]
                    }
              , rank = 39
              }
          , Slot
              { person = Kate
              , attendance = Out
              , status = Proposed
              , stat =
                  Stat
                    { inDates =
                        [ ( 2013 , 11 , 11 )
                        , ( 2013 , 12 , 2 )
                        , ( 2013 , 12 , 9 )
                        , ( 2013 , 12 , 16 )
                        ]
                    , outDates = [ ( 2013 , 11 , 18 ) , ( 2013 , 11 , 25 ) ]
                    , hostDates = [ ( 2013 , 12 , 2 ) ]
                    , absentDates = []
                    }
              , rank = 7
              }
          , Slot
              { person = Rebecca
              , attendance = Out
              , status = Proposed
              , stat =
                  Stat
                    { inDates = [ ( 2013 , 11 , 25 ) , ( 2013 , 12 , 2 ) ]
                    , outDates =
                        [ ( 2013 , 11 , 11 )
                        , ( 2013 , 11 , 18 )
                        , ( 2013 , 12 , 9 )
                        , ( 2013 , 12 , 16 )
                        ]
                    , hostDates = [ ( 2013 , 11 , 25 ) ]
                    , absentDates = []
                    }
              , rank = 45
              }
          , Slot
              { person = Erica
              , attendance = Host
              , status = Proposed
              , stat =
                  Stat
                    { inDates = [ ( 2013 , 11 , 11 ) , ( 2013 , 11 , 25 ) ]
                    , outDates =
                        [ ( 2013 , 11 , 18 )
                        , ( 2013 , 12 , 2 )
                        , ( 2013 , 12 , 9 )
                        , ( 2013 , 12 , 16 )
                        ]
                    , hostDates = [ ( 2013 , 11 , 11 ) ]
                    , absentDates = []
                    }
              , rank = 2
              }
          ]
      }
  ]