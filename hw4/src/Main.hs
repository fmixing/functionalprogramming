{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}


module Main where

import Lib4

$(showText' ''MyData)
$(showText' ''MyNewType)
$(showText' ''D)
$(showText' ''A)
$(showText' ''MyType)
-- $(showTextInstance ''D)



main = do
    print $ showText $ MyNewType "123"
    print $ showText $ MyData "123" 123
    print $ showText $ B
    print $ showText $ C
    print $ showText $ K 123
    print $ showText $ L 123
    print $ showText $ ("abc" :: MyType)
    -- print $ showText $ K 123