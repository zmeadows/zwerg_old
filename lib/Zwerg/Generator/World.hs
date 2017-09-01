module Zwerg.Generator.World (world) where

import Zwerg.Generator
-- import Zwerg.Generator.Default
-- import Zwerg.Generator.Verify
import Zwerg.Generator.Enemy.Goblin
-- import Zwerg.Generator.Item.Weapon
import Zwerg.Generator.Level.TestSquare
import Zwerg.Generator.Level.Cave
import Zwerg.Generator.Stairs
import Zwerg.Generator.Player.TestPlayer

world :: Generator
world = Generator worldHatch []

stackLevel :: Int -> EntityAssembler
stackLevel i = MkEntityAssembler $ \levelUUID -> do
    case (wrap i) of
      Just z -> setComp levelUUID zLevel z
      Nothing -> debug "Attempted to stack a level with an improper zLevel!"

worldHatch :: EntityHatcher
worldHatch = MkEntityHatcher $ do
    caveUUID <- generate $ cave
                  <+ stackLevel 0
                  ++> replicate 5 (generateOnRandomEmptyTile goblin)
    _ <- generate $ testSquare
                  <+ stackLevel 1
                  ++> replicate 4 (buildRandomStairs caveUUID)
                  ++> replicate 5 (generateOnRandomEmptyTile goblin)
    _ <- generate $ testPlayer +> putOnRandomEmptyTile caveUUID

    return worldUUID

