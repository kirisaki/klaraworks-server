{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Klaraworks.Types where

import           Data.Text
import           Data.Time
import           Data.Time.Clock
import           Data.Extensible
import           Database.Persist
import           Database.Persist.TH

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
  Info
    dir Text
    date Day
    worksType Text
    fanart Bool
    UniqueDir dir
       
  Detail
    dir Text
    lang Text
    title Text
    event Text
    origin Text
    contents [Text]
    status Text
    text Text
    link Text
    deriving Show Eq

  User
    login_id Text
    pass_hash Text

  Session
    hash Text
    expire UTCTime
|]
  

type ApiLogin = Record
  '[ "login_id" >: Text
   , "password" >: Text
   ]

type Dir = Text
  
type ApiInfo = Record
  '[ "dir" >: Dir
   , "date" >: Day
   , "worksType" >: Text
   , "fanart" >: Bool
   ]

type ApiDetail = Record
  '[ "dir" >: Dir
   , "lang" >: Text
   , "title" >: Text
   , "event" >: Text
   , "origin" >: Text
   , "contents" >: [Text]
   , "status" >: Text
   , "text" >: Text
   , "link" >: Text
   ]

type ApiWorksHeader = Record
  '[ "dir" >: Dir
   , "title" >: Text
   , "date" >: Day
   ]

  
type ApiWorks = Record
  '[ "dir" >: Dir
   , "title" >: Text
   , "date" >: Day
   , "event" >:  Text
   , "worksType" >: Text
   , "origin" >:  Text
   , "fanart" >: Bool
   , "contents" >: [Text]
   , "status" >:  Text
   , "text" >:  Text
   , "link" >:  Text
   ]
