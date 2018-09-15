{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Klaraworks.Api where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Extensible hiding((:>))
import           Data.Text (Text)
import           Database.Persist
import           Klaraworks.Types
import           Servant
import           Servant.API
import           Servant.API.Experimental.Auth

type KlaraWorksApi =
  "_api" :>
  ( "works" :>
    ( Capture "language" Text :> Get '[JSON] [ApiWorksHeader]  :<|>
      Capture "language" Text :> Capture "worksDir" Text :> Get '[JSON] ApiWorks
    ):<|>
    "login" :>
    ( ReqBody '[JSON] ApiLogin :> Post '[JSON] () :<|>
      ReqBody '[JSON] ApiLogin :> Delete '[JSON] ()
    ) :<|>
    "info" :>
    ( Get '[JSON] [ApiInfo]  :<|>
      Capture "infoDir" Text :> Get '[JSON] ApiInfo -- :<|>
      --AuthProtect "cookie-auth" :>  ReqBody '[JSON] ApiInfo :> Post '[JSON] () :<|>
      --AuthProtect "cookie-auth" :>  Capture "infoDir" Text :> ReqBody '[JSON] ApiInfo :> Put '[JSON] () :<|>
      --AuthProtect "cookie-auth" :>  Capture "infoDir" Text :> Delete '[JSON] ()
    ):<|>
    "detail" :>
    ( Get '[JSON] [ApiDetail] :<|>
      Capture "detailDir" Text :> Get '[JSON] ApiDetail -- :<|>
      --AuthProtect "cookie-auth" :> ReqBody '[JSON] ApiDetail :> Post '[JSON] () :<|>
      --AuthProtect "cookie-auth" :> Capture "detailDir" Text :> ReqBody '[JSON] ApiDetail :> Put '[JSON] () :<|>
      --AuthProtect "cookie-auth" :> Capture "detailDir" Text :> Delete '[JSON] ()
    )
  )

runSql = undefined
  
server :: Server KlaraWorksApi
server = ( getWorksList :<|>
           getWorks
         ):<|>
         ( postLogin :<|>
           deleteLogin
         ):<|>
         ( getInfoList :<|>
           getInfo
         --postInfo :<|>
         --putInfo :<|>
         --deleteInfo :<|> 
         ):<|>
         ( getDetailList :<|>
           getDetail -- :<|>
         --postDetail :<|>
         --putDetail :<|>
         --deleteDetail 
         )

getWorksList :: Text -> Handler [ApiWorksHeader]
getWorksList language = do
  liftIO $ runSql $ do
    infoList <- selectList [] [Desc InfoDir]
    details <- forM infoList $ \(Entity iid info) -> do
      detail <- selectFirst [ DetailDir ==. infoDir info
                            , DetailLang ==. language
                            ] []
      return (info, detail)
    return $ map makeHeader (removeNoDetail details)
      where
        removeNoDetail = filter $ \(i, d) ->
          case d of
            Nothing -> False
            _ -> True
        makeHeader (info, Just (Entity did detail)) =
          #dir @= infoDir info <:
          #title @= detailTitle detail <:
          #date @= infoDate info <:
          nil
                    
getWorks :: Text -> Dir -> Handler ApiWorks
getWorks language inDir = do
  meta <- liftIO $ runSql $ do
    info <- selectFirst [ InfoDir ==. inDir ] []
    detail <- selectFirst [ DetailDir ==. inDir
                          , DetailLang ==. language
                          ] []
    case (info, detail) of
      (Just(Entity iid i), Just(Entity did d)) ->
        return $ Just $ makeWorksMeta i d
      (Just(Entity iid i), Nothing) -> do
        detail' <- selectFirst [ DetailDir ==. inDir ] []
        case detail' of
          Just(Entity did' d') ->
            return $ Just $ makeWorksMeta i d'
          Nothing ->
            return Nothing
      _ ->
        return Nothing
  case meta of
    Just meta' -> return meta'
    Nothing -> throwError err404
  where
    makeWorksMeta i d = #dir @= infoDir i <:
                        #title @= detailTitle d <:
                        #date @= infoDate i <:
                        #event @= detailEvent d <:
                        #worksType @= infoWorksType i <:
                        #origin @= detailOrigin d <:
                        #fanart @= infoFanart i <:
                        #contents @= detailContents d <:
                        #status @= detailStatus d <:
                        #text @= detailText d <:
                        #link @= detailLink d <:
                        nil


postLogin :: ApiLogin -> Handler ()
postLogin login_info  = undefined

deleteLogin :: ApiLogin -> Handler ()
deleteLogin login_info  = undefined

getInfoList :: Handler [ApiInfo]
getInfoList = undefined

getInfo :: Dir -> Handler ApiInfo
getInfo infoDir = undefined

--postInfo :: Account -> ApiInfo -> Handler()
--postInfo acc info = undefined

--putInfo :: Account -> Dir -> ApiInfo -> Handler ()
--putInfo acc infoDir info = undefined

--deleteInfo :: Account -> Dir -> Handler ()
--deleteInfo acc infoDir  = undefined

getDetailList :: Handler [ApiDetail]
getDetailList = undefined

getDetail :: Dir -> Handler ApiDetail
getDetail detailDir = undefined

--postDetail :: Account -> ApiDetail -> Handler()
--postDetail acc detail = undefined

--putDetail :: Account -> Dir -> ApiDetail -> Handler ()
--putDetail acc detailDir detail = undefined

--deleteDetail :: Account -> Dir -> Handler ()
--deleteDetail acc detailDir  = undefined
