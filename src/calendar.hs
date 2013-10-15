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
                  [("Rebecca", Available),
                   ("Kasey",   Available),
                   ("Neha",    Hosting),
                   ("Kate",    Available),
                   ("Erica",   Available),
                   ("Jenny",   Available)]),
               ((2013, 11, 4), 
                  [("Rebecca", Available),
                   ("Kasey",   Hosting),
                   ("Neha",    Available),
                   ("Kate",    Available),
                   ("Erica",   Available),
                   ("Jenny",   Available)]),
               ((2013, 11, 11), 
                  [("Rebecca", Available),
                   ("Kasey",   Available),
                   ("Neha",    Available),
                   ("Kate",    Available),
                   ("Erica",   Hosting),
                   ("Jenny",   Available)]),
               ((2013, 11, 18), 
                  [("Rebecca", Available),
                   ("Kasey",   Available),
                   ("Neha",    Available),
                   ("Kate",    Available),
                   ("Erica",   Available),
                   ("Jenny",   Hosting)]),
               ((2013, 11, 25), 
                  [("Rebecca", Hosting),
                   ("Kasey",   Available),
                   ("Neha",    Available),
                   ("Kate",    Available),
                   ("Erica",   Available),
                   ("Jenny",   Available)])]