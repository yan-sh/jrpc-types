{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BlockArguments #-}

module JRPC.Types where

import Data.Aeson
import Data.Text
import GHC.TypeLits
import GHC.Prim
import Prelude hiding (lookup)
import qualified Data.Vector as V
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as K
import qualified Data.HashMap.Strict as HM

data CustomError = CustomError !Text (Maybe Value) !Int

data JsonRpcError n =
    ParseError
  | InvalidRequest
  | MethodNotFound !n
  | InvalidParams !n
  | InternalError !n

newtype Param (name :: Symbol) = Param (Maybe Value)

class ToMethod f m where
  mkMethod :: f -> Either Array Object -> m (Either CustomError Value)

instance 
    ( Applicative m
    , ToMethodArray f m
    , ToMethodObject f m
    ) => ToMethod f m where
  mkMethod f = either
    do mkMethodArray f
    do mkMethodObject f
  {-# INLINE mkMethod #-}


class ToMethodObject f m where
  mkMethodObject :: f -> Object -> m (Either CustomError Value)

instance
    ( KnownSymbol n
    , Applicative m
    , ToMethodObject fs m
    ) => ToMethodObject (Param n -> fs) m where
  mkMethodObject f = \obj_ ->
    mkMethodObject
      (f $ Param $ KM.lookup (K.fromText $ pack $ symbolVal' (proxy# @n)) obj_)
      obj_
  {-# INLINE mkMethodObject #-}

instance ToMethodObject (m (Either CustomError Value)) m where
  mkMethodObject = const
  {-# INLINE mkMethodObject #-}



class ToMethodArray f m where
  mkMethodArray :: f -> Array -> m (Either CustomError Value)

instance
    ( KnownSymbol n
    , Applicative m
    , ToMethodArray fs m
    ) => ToMethodArray (Param n -> fs) m where
  mkMethodArray f = \array ->
      case V.uncons array of
        Nothing       -> mkMethodArray (f (Param Nothing)) array
        Just (h, ps)  -> mkMethodArray (f (Param $ Just h)) ps
  {-# INLINE mkMethodArray #-}

instance ToMethodArray (m (Either CustomError Value)) m where
  mkMethodArray = const
  {-# INLINE mkMethodArray #-}

newtype Method m = Method (Either Array Object -> m (Either CustomError Value)) 

newtype MethodMap m = MethodMap (HM.HashMap Text (Method m))
