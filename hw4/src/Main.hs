{-# LANGUAGE TemplateHaskell #-}

module Main where

import Lib4

$(showText' ''MyData)
$(showText' ''MyNewType)
$(showText' ''D)
$(showText' ''A)
-- $(showTextInstance ''D)



main = do
    print $ MyNewType "123"
    print $ MyData "123" 123
    print $ B
    print $ C
    print $ K 123
    print $ L 123
    -- print $ showText $ K 123