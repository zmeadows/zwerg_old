module Zwerg.Generator.Enemy.Goblin where

import Data.Text (append)
import Zwerg.Generator

goblinGenerator :: Generator UUID
goblinGenerator =
  MkGenerator $ do
    goblinUUID <- popUUID
    generateGoblinName >>= addComp goblinUUID name
    addComp goblinUUID glyph $ Glyph 'g' 42 40 Nothing Nothing
    goblinHP <- getRandomR (3, 7)
    zConstruct (goblinHP, goblinHP) >>= addComp goblinUUID hp
    getRandomR (1, 100) >>= addComp goblinUUID ticks
    addComp goblinUUID entityType Enemy
    addComp goblinUUID equipment emptyEquipment
    addComp goblinUUID stats zeroStats
    assignUniformRandomStat goblinUUID STR (1, 4)
    assignUniformRandomStat goblinUUID DEX (2, 6)
    assignUniformRandomStat goblinUUID INT (1, 2)
    assignUniformRandomStat goblinUUID CHA (1, 2)
    assignUniformRandomStat goblinUUID CON (1, 3)
    assignUniformRandomStat goblinUUID WIS (1, 2)
    return goblinUUID

generateGoblinName
  :: (MonadRandom m)
  => m Text
generateGoblinName =
  let firstNameFirstSyllables = ["Gol", "Kra", "Bah", "Quo"]
      firstNameSecondSyllables = ["ith", "xul", "nix", "oth"]
  in do f1 <- pickRandom firstNameFirstSyllables
        f2 <- pickRandom firstNameSecondSyllables
        return $ append f1 f2
