{-# LANGUAGE OverloadedStrings #-}

import Data.List (sort)
import DBus
import DBus.Client
import qualified Data.ByteString.Char8 as Char8
import Data.Word
import Control.Monad
import Data.Maybe

type NMDeviceType = Word32

nm_DEVICE_TYPE_WIFI :: NMDeviceType
nm_DEVICE_TYPE_WIFI = 2

isWifiDevice :: Client -> ObjectPath -> IO Bool
isWifiDevice client objpath = do
    let callarg = [ toVariant ("org.freedesktop.NetworkManager.Device" :: String) 
                  , toVariant ("DeviceType" :: String)
                  ]
    reply <- call_ client (methodCall objpath "org.freedesktop.DBus.Properties" "Get")
        { methodCallDestination = Just "org.freedesktop.NetworkManager"
        , methodCallBody = callarg
        }
    let response = methodReturnBody reply !! 0
    let devType = fromJust $ fromVariant response >>= fromVariant

    return $ devType == nm_DEVICE_TYPE_WIFI

wifiDevices :: Client -> IO [ObjectPath]
wifiDevices client = do
    deviceObjPaths <- allDevices client
    filterM (isWifiDevice client) deviceObjPaths

allDevices :: Client -> IO [ObjectPath]
allDevices client = do
    reply <- call_ client (methodCall "/org/freedesktop/NetworkManager" "org.freedesktop.NetworkManager" "GetDevices")
        { methodCallDestination = Just "org.freedesktop.NetworkManager"
        }
    let k = methodReturnBody reply !! 0
    let Just deviceObjPaths = fromVariant k 
    return deviceObjPaths

accessPoints :: Client -> ObjectPath -> IO [ObjectPath]
accessPoints client wifiDev = do
    reply <- call_ client (methodCall wifiDev "org.freedesktop.NetworkManager.Device.Wireless" "GetAccessPoints")
        { methodCallDestination = Just "org.freedesktop.NetworkManager"
        }
    return $ fromJust $ fromVariant $ methodReturnBody reply !! 0

hwAddress :: Client -> ObjectPath -> IO String
hwAddress client ap = do
    let callarg = [ toVariant ("org.freedesktop.NetworkManager.AccessPoint" :: String)
                  , toVariant ("HwAddress" :: String)
                  ]
    reply <- call_ client (methodCall ap "org.freedesktop.DBus.Properties" "Get")
        { methodCallDestination = Just "org.freedesktop.NetworkManager"
        , methodCallBody = callarg
        }
    let response = methodReturnBody reply !! 0
    let hwAddr = fromJust $ (fromVariant response >>= fromVariant)
    return hwAddr

wifiSsid :: Client -> ObjectPath -> IO Char8.ByteString
wifiSsid client ap = do
    let callarg = [ toVariant ("org.freedesktop.NetworkManager.AccessPoint" :: String)
                  , toVariant ("Ssid" :: String)
                  ]
    reply <- call_ client (methodCall ap "org.freedesktop.DBus.Properties" "Get")
        { methodCallDestination = Just "org.freedesktop.NetworkManager"
        , methodCallBody = callarg
        }
    let response = methodReturnBody reply !! 0
    let ssid = fromMaybe "(null)" $ (fromVariant response >>= fromVariant)
    return ssid

concatMapM f xs = liftM concat $ mapM f xs

main = do
    client <- connectSystem
    devs <- allDevices client
    wifiDevs <- filterM (isWifiDevice client) devs
    aps <- concatMapM (accessPoints client) wifiDevs
    addrs <- mapM (hwAddress client) aps
    ssids <- mapM (wifiSsid client) aps
    mapM_ print $ zip addrs ssids
