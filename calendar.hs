module Calendar where

data Status = Available | Unavailable | Hosting | Requested | Rejected | Chosen | NotChosen deriving (Eq, Ord)

type Year = Int
type Month = Int
type Day = Int
type Date = (Year, Month, Day)
type Person = String
type Slot = (Person, Status)
type Week = (Date, [Slot])
type Calendar = [Week]

persons = ["Rebecca", "Jenny", "Kate", "Kasey", "Neha", "Erica"]
allAvailable = map (\x -> (x, Available)) persons

theCalendar :: Calendar
theCalendar = [((2013, 10, 7),
                  [("Rebecca", Chosen),
                   ("Kasey",   Unavailable),
                   ("Neha",    Unavailable),
                   ("Kate",    NotChosen),
                   ("Erica",   NotChosen),
                   ("Jenny",   Hosting)]),
                ((2013, 10, 14),
                  [("Rebecca", Hosting),
                   ("Kasey",   NotChosen),
                   ("Neha",    NotChosen),
                   ("Kate",    Chosen),
                   ("Erica",   Chosen),
                   ("Jenny",   Chosen)]),
               ((2013, 10, 21), 
                  [("Rebecca", Chosen),
                   ("Kasey",   Chosen),
                   ("Neha",    Chosen),
                   ("Kate",    Hosting),
                   ("Erica",   NotChosen),
                   ("Jenny",   NotChosen)]),
               ((2013, 10, 28), 
                  [("Rebecca", NotChosen),
                   ("Kasey",   Chosen),
                   ("Neha",    Hosting),
                   ("Kate",    NotChosen),
                   ("Erica",   Chosen),
                   ("Jenny",   Chosen)]),
               ((2013, 11, 4), 
                  [("Rebecca", NotChosen),
                   ("Kasey",   Hosting),
                   ("Neha",    Chosen),
                   ("Kate",    Chosen),
                   ("Erica",   NotChosen),
                   ("Jenny",   Chosen)]),
               ((2013, 11, 11), 
                  [("Rebecca", Chosen),
                   ("Kasey",   NotChosen),
                   ("Neha",    Chosen),
                   ("Kate",    Chosen),
                   ("Erica",   Hosting),
                   ("Jenny",   NotChosen)]),
               ((2013, 11, 18), 
                  [("Rebecca", Chosen),
                   ("Kasey",   NotChosen),
                   ("Neha",    NotChosen),
                   ("Kate",    Chosen),
                   ("Erica",   Chosen),
                   ("Jenny",   Hosting)]),
               --((2013, 11, 25), 
               --   [("Rebecca", Hosting),
               --    ("Kasey",   NotChosen),
               --    ("Neha",    Chosen),
               --    ("Kate",    NotChosen),
               --    ("Erica",   Chosen),
               --    ("Jenny",   Chosen)])
               ((2013, 12, 2), 
                  [("Rebecca", Available),
                   ("Kasey",   Available),
                   ("Neha",    Available),
                   ("Kate",    Hosting),
                   ("Erica",   Available),
                   ("Jenny",   Unavailable)])]