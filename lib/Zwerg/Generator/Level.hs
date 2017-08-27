module Zwerg.Generator.Level
    ( module EXPORTED
    , setZLevel
    ) where

import Zwerg.Generator as EXPORTED

--FIXME: set zLevel for enemies/occupants?
setZLevel :: UUID -> ZLevel -> Generator' ()
setZLevel levelUUID z = do
    setComp levelUUID zLevel z
    levelTiles <- tiles <@> levelUUID
    forM_ (unwrap levelTiles) $ \tileUUID -> do
        setComp tileUUID zLevel z
        occs <- occupants <@> tileUUID
        forM_ (unwrap occs) $ \occUUID -> setComp occUUID zLevel z


