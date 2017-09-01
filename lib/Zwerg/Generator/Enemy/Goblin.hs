module Zwerg.Generator.Enemy.Goblin (goblin) where

import Zwerg.Generator
import Zwerg.Generator.Default
-- import Zwerg.Generator.Verify
import Zwerg.Generator.Item.Weapon

goblin :: Generator
goblin = Generator goblinHatch []
           +> assignUniformRandomStats (zip (enumFrom STR) (repeat (1,5)))
           +> generateAndEquip sword

goblinHatch :: EntityHatcher
goblinHatch = MkEntityHatcher $ do
    goblinUUID <- generateSkeleton Enemy
    let (<@-) :: Component a -> a -> MonadCompState ()
        (<@-) = addComp goblinUUID

    newGoblinName <- generateGoblinName
    newGoblinHP <- getRandomR (3,7) >>= \x -> return $ wrapOrDefault (x,x)

    name          <@- newGoblinName
    hp            <@- newGoblinHP
    description   <@- "It is foul-smelling and wrinkly."
    species       <@- "Goblin"
    glyph         <@- (Glyph 'g' $ CellColor lime Nothing)
    ticks         <@- 100
    aiType        <@- SimpleMeleeCreature
    blocksPassage <@- True
    blocksVision  <@- False
    viewRange     <@- 5

    return goblinUUID

generateGoblinName :: (MonadRandom m) => m Text
generateGoblinName =
    let firstNameFirstSyllables = "Gol" :| ["Kra", "Bah", "Quo"]
        firstNameSecondSyllables = "ith" :| ["xul", "nix", "oth"]
    in do f1 <- pickRandom firstNameFirstSyllables
          f2 <- pickRandom firstNameSecondSyllables
          return $ f1 <> f2
