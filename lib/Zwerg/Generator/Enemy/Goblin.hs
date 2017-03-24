module Zwerg.Generator.Enemy.Goblin where

import Zwerg.Component.All
import Zwerg.Generator
import Zwerg.UI.Font

goblinGenerator :: Generator UUID
goblinGenerator = MkGenerator $ do
    traceM "generating individual goblin"
    goblinUUID <- getNewUUID

    traceM $ show (__LINE__ :: Int)
    addComp goblinUUID name "Goblin"
    traceM $ show (__LINE__ :: Int)
    addComp goblinUUID glyph $ Glyph Normal 'g' $ mkColor 100 255 100
    traceM $ show (__LINE__ :: Int)
    zConstruct (5,5) >>= addComp goblinUUID hp
    traceM $ show (__LINE__ :: Int)
    addComp goblinUUID entityType Enemy
    traceM $ show (__LINE__ :: Int)
    addComp goblinUUID equipment emptyEquipment
    traceM $ show (__LINE__ :: Int)
    addComp goblinUUID stats zeroStats
    traceM $ show (__LINE__ :: Int)
    return goblinUUID

