module Zwerg.Generator.Enemy.Goblin (goblin) where

import Data.Text (append)
import Zwerg.Generator
import Zwerg.Generator.Default
import Zwerg.Generator.Item.Weapon

goblin :: Generator
goblin = do
    goblinUUID <- popUUID
    generateSkeleton goblinUUID Enemy
    generateGoblinName >>= addComp goblinUUID name
    addComp goblinUUID description "It is foul-smelling and wrinkly."
    addComp goblinUUID species "Goblin"

    goblinHP <- getRandomR (3, 7)
    zConstruct (goblinHP, goblinHP) >>= addComp goblinUUID hp

    addComp goblinUUID glyph $ Glyph 'g' Green0 Green3 Nothing Nothing
    addComp goblinUUID ticks 1
    addComp goblinUUID aiType SimpleMeleeCreature
    addComp goblinUUID blocksPassage True
    addComp goblinUUID blocksVision False

    assignUniformRandomStat goblinUUID STR (1, 5)
    assignUniformRandomStat goblinUUID DEX (1, 5)
    assignUniformRandomStat goblinUUID INT (1, 5)
    assignUniformRandomStat goblinUUID CHA (1, 5)
    assignUniformRandomStat goblinUUID CON (1, 5)
    assignUniformRandomStat goblinUUID WIS (1, 5)

    swordUUID <- sword
    equipItem swordUUID goblinUUID
    return goblinUUID

generateGoblinName :: (MonadRandom m) => m Text
generateGoblinName =
  let firstNameFirstSyllables = "Gol" :| ["Kra", "Bah", "Quo"]
      firstNameSecondSyllables = "ith" :| ["xul", "nix", "oth"]
  in do f1 <- pickRandom firstNameFirstSyllables
        f2 <- pickRandom firstNameSecondSyllables
        return $ append f1 f2
