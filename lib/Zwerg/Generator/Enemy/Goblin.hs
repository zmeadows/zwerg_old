module Zwerg.Generator.Enemy.Goblin (goblin) where

import Data.Text (append)
import Zwerg.Generator
import Zwerg.Generator.Default
import Zwerg.Generator.Verify
import Zwerg.Generator.Item.Weapon

goblin :: Generator
goblin = do
    goblinUUID <- generateSkeleton Enemy
    let (<@-) :: Component a -> a -> Generator' ()
        (<@-) = addComp goblinUUID

    newGoblinName <- generateGoblinName
    newGoblinHP <- getRandomR (3,7) >>= \x -> return $ wrapOrDefault (x,x)

    name          <@- newGoblinName
    hp            <@- newGoblinHP
    description   <@- "It is foul-smelling and wrinkly."
    species       <@- "Goblin"
    glyph         <@- (Glyph 'g' (CellColor Green0 Green3) Nothing)
    ticks         <@- 100
    aiType        <@- SimpleMeleeCreature
    blocksPassage <@- True
    blocksVision  <@- False
    viewRange     <@- 5

    assignUniformRandomStat goblinUUID STR (1, 5)
    assignUniformRandomStat goblinUUID DEX (1, 5)
    assignUniformRandomStat goblinUUID INT (1, 5)
    assignUniformRandomStat goblinUUID CHA (1, 5)
    assignUniformRandomStat goblinUUID CON (1, 5)
    assignUniformRandomStat goblinUUID WIS (1, 5)

    generateAndEquip sword goblinUUID

    verifyAndReturn goblinUUID

generateGoblinName :: (MonadRandom m) => m Text
generateGoblinName =
  let firstNameFirstSyllables = "Gol" :| ["Kra", "Bah", "Quo"]
      firstNameSecondSyllables = "ith" :| ["xul", "nix", "oth"]
  in do f1 <- pickRandom firstNameFirstSyllables
        f2 <- pickRandom firstNameSecondSyllables
        return $ append f1 f2
