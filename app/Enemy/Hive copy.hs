spawnsHive Hive1 dT pos = do
    spawnWave 20 dT 3 1.3 $ getEnemyFromType Normal 1 pos
    spawnWave 40 dT 3 1.3 $ getEnemyFromType Normal 1 pos

    doMinute 4 dT 3 3 pos Normal
    doMinute 7 dT 8 3 pos Normal
    doMinute 8 dT 3 3 pos Normal
    doMinute 10 dT 3 3 pos Fast
    doMinute 13 dT 8 3 pos Normal
    doMinute 15 dT 5 3 pos Fast

spawnsHive Hive2 dT pos = do
    doMinute 1 dT 3 3 pos Normal
    doMinute 4 dT 3 3 pos Normal
    doMinute 7 dT 5 3 pos Normal
    doMinute 8 dT 3 3 pos Normal
    doMinute 11 dT 8 3 pos Normal
    doMinute 13 dT 3 3 pos Tank
    doMinute 15 dT 3 3 pos Tank

spawnsHive Hive3 dT pos = do
    doMinute 2 dT 5 3 pos Normal
    doMinute 5 dT 8 3 pos Normal
    doMinute 8 dT 3 3 pos Normal
    doMinute 9 dT 5 3 pos Normal
    doMinute 11 dT 5 3 pos Fast
    doMinute 14 dT 8 3 pos Normal

spawnsHive Hive4 dT pos = do
    doMinute 3 dT 5 3 pos Normal
    doMinute 6 dT 8 3 pos Normal
    doMinute 8 dT 3 3 pos Normal
    doMinute 9 dT 3 3 pos Fast
    doMinute 12 dT 5 3 pos Normal
    doMinute 14 dT 5 3 pos Tank

spawnsHive Hive5 dT pos = do
    doMinute 4 dT 3 3 pos Normal
    doMinute 6 dT 3 3 pos Normal
    doMinute 8 dT 3 3 pos Normal
    doMinute 10 dT 8 3 pos Normal
    doMinute 12 dT 3 3 pos Tank
    doMinute 15 dT 8 3 pos Normal