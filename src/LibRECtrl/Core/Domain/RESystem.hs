{-# LANGUAGE DuplicateRecordFields #-}

module LibRECtrl.Core.Domain.RESystem
  ( ReSystem,
    PVSystem,
    ConsumerLoad,
  )
where

import LibRECtrl.Core.Domain.Id

data ReSystem = ReSystem
  { id :: ID,
    pvSystem :: Maybe PVSystem,
    consumerLoad :: Maybe ConsumerLoad
  }

newtype PVSystem = PVSystem
  { id :: ID
  }

newtype ConsumerLoad = ConsumerLoad
  { id :: ID
  }
