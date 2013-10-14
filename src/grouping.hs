--type Group  = [Person]
--type Grouping = (Group, Group)

--split_into_groupings :: Int -> [Name] -> [Grouping]
--split_into_groupings 0 xs     = [([], xs)]
--split_into_groupings n []     = []
--split_into_groupings n (x:xs) = x_groups ++ xs_groups
--  where
--    x_groups     = [ (x:group1,group2) | (group1,group2) <- split_into_groupings (n-1) xs ]
--    xs_groups    = [ (group1,x:group2) | (group1,group2) <- split_into_groupings n xs ]

--print_group :: [Char] -> Group -> IO ()
--print_group name group = do
--  putStr name
--  putStrLn $ intercalate ", " group

--print_week :: (Int, Grouping) -> IO ()
--print_week (week_number, grouping@(group1, group2)) = do
--  putStrLn $ "Week " ++ show week_number
--  print_group "Babysit: " group1
--  print_group "Go out: " group2
--  putStrLn ""