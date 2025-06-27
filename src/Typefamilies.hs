{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Typefamilies (run) where

import Data.Kind (Constraint)
import GHC.TypeLits

-- Define roles and actions
data Role = Admin | Manager | Guest
data Action = View | Edit | Delete

-- Permission logic
type family CanPerform (r :: Role) (a :: Action) :: Bool where
    CanPerform 'Admin _ = 'True
    CanPerform 'Manager 'View = 'True
    CanPerform 'Manager 'Edit = 'True
    CanPerform 'Manager 'Delete = 'False
    CanPerform 'Guest 'View = 'True
    CanPerform 'Guest _ = 'False

type family Assert (b :: Bool) (r :: Role) (a :: Action) :: Constraint where
    Assert 'True r a = ()
    Assert 'False r a =
        TypeError
            ( 'Text "Permission denied: role "
                ':<>: 'ShowType r
                ':<>: 'Text " cannot perform action "
                ':<>: 'ShowType a
            )

class (Assert (CanPerform r a) r a) => Allow (r :: Role) (a :: Action) where
    perform :: String -> IO ()

instance (Assert (CanPerform r a) r a) => Allow r a where
    perform s = putStrLn ("Action performed: " ++ s)

test1 :: IO ()
test1 = perform @'Admin @'Delete "Admin deletes something"

test2 :: IO ()
test2 = perform @'Manager @'Edit "Manager edits something"

-- Failes type check
-- test3 :: IO ()
-- test3 = perform @'Guest @'Delete "Guest tries to delete"

run :: IO ()
run = test1 >> test2
