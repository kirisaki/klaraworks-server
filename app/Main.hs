{-# LANGUAGE OverloadedStrings #-}

module Main where

import Klaraworks

main :: IO ()
main = runServer "klaraworks.yaml"
