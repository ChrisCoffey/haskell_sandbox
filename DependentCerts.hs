{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}

data Jan = Jan
data Feb = Feb
data Mar = Mar

type family TMonth (m :: Month) :: *
type instance TMonth 'Jan = Jan
type instance TMonth 'Feb = Feb
type instance TMonth 'Mar = Mar

data SMonth :: Month -> * where
    SJan :: SMonth 'Jan
    SFeb :: SMonth 'Feb
    SMar :: SMonth 'Mar

getMonth :: SMonth m -> TMonth m
getMonth SJan = Jan
