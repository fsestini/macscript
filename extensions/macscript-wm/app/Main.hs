module Main where

import MacScript
import MacScript.WM
import MacScript.WM.DefaultConfig

main :: IO ()
main = runScript $ liftIO $ wmMain defaultConfig
