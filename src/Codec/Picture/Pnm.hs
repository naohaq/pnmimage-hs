{- -*- coding: utf-8-unix -*- -}
{-# OPTIONS_GHC -Wall #-}

module Codec.Picture.Pnm (readPnmImage)
where

import Codec.Picture
import System.IO
import System.IO.Error

import Data.Char
import qualified Data.ByteString as B

data MagicNumber = P1 | P2 | P3 | P4 | P5 | P6 deriving (Show,Eq)

skipWS :: String -> Maybe (String, String)
skipWS str = Just $ span isSpace str

parseInt :: String -> Maybe (Int, String)
parseInt str = case readsPrec 0 str of
                 ((x,str'):_) -> Just (x,str')
                 _ -> Nothing

readSize :: String -> Maybe (Int,Int)
readSize str = do
  (w, r ) <- parseInt str
  (_, r') <- skipWS r
  (h, _ ) <- parseInt r'
  return (w,h)

readHeaderLine :: Handle -> IO String
readHeaderLine h = do
  ln <- hGetLine h
  case ln of
    ('#':_) -> do
      readHeaderLine h
    _ -> do
      return ln

readMagicNumber :: Handle -> IO (Maybe MagicNumber)
readMagicNumber h = catchIOError f g
  where g e = if isEOFError e then return Nothing else ioError e
        f = do
          ln <- readHeaderLine h
          case ln of
            ('P':'1':_) -> return (Just P1)
            ('P':'2':_) -> return (Just P2)
            ('P':'3':_) -> return (Just P3)
            ('P':'4':_) -> return (Just P4)
            ('P':'5':_) -> return (Just P5)
            ('P':'6':_) -> return (Just P6)
            _           -> return Nothing

findSizeHeader :: Handle -> IO (Maybe (Int,Int))
findSizeHeader h = catchIOError f g
  where g e = if isEOFError e then return Nothing else ioError e
        f = do
          ln <- readHeaderLine h
          return $ readSize ln

readMaxValue :: Handle -> IO (Maybe Int)
readMaxValue h = catchIOError f g
  where g e = if isEOFError e then return Nothing else ioError e
        f = do
          ln <- readHeaderLine h
          case parseInt ln of
            Just (x, _) -> return (Just x)
            Nothing     -> return Nothing

sampleP5At :: (Int,Int,B.ByteString) -> Int -> Int -> Pixel8
sampleP5At (w,_,body) x y = v
  where k = w*y + x
        v = B.index body (k+0)

sampleP6At :: (Int,Int,B.ByteString) -> Int -> Int -> PixelRGB8
sampleP6At (w,_,body) x y = PixelRGB8 r g b
  where k = w*3*y + x*3
        r = B.index body (k+0)
        g = B.index body (k+1)
        b = B.index body (k+2)

failUnless :: Bool -> String -> IO ()
failUnless False msg = fail msg
failUnless _     _   = return ()

loadP5Image_main :: Handle -> IO (Image Pixel8)
loadP5Image_main h = do
  result <- findSizeHeader h
  case result of
    Just (width,height) -> do
      mag <- readMaxValue h
      case mag of
        Just mag' -> do
          failUnless (mag' <= 255) "Unsupported format."
          let sz = width * height
          body <- B.hGet h sz
          failUnless (B.length body >= sz) "Insufficient image data."
          return $ generateImage (sampleP5At (width,height,body)) width height
        Nothing -> do
          fail "Malformed PNM header."
    Nothing -> do
      fail "Malformed PNM header."

loadP6Image_main :: Handle -> IO (Image PixelRGB8)
loadP6Image_main h = do
  result <- findSizeHeader h
  case result of
    Just (width,height) -> do
      mag <- readMaxValue h
      case mag of
        Just mag' -> do
          failUnless (mag' <= 255) "Unsupported format."
          let sz = width * height * 3
          body <- B.hGet h sz
          failUnless (B.length body >= sz) "Insufficient image data."
          return $ generateImage (sampleP6At (width,height,body)) width height
        Nothing -> do
          fail "Malformed PNM header."
    Nothing -> do
      fail "Malformed PNM header."

loadPNM_main :: Handle -> IO DynamicImage
loadPNM_main h = do
  mn <- readMagicNumber h
  case mn of
    Nothing -> fail "Magic number not found."
    Just n  -> case n of
      P5 -> do
        img <- loadP5Image_main h
        return (ImageY8 img)
      P6 -> do
        img <- loadP6Image_main h
        return (ImageRGB8 img)
      _  -> fail "Unsupported format."

readPnmImage :: String -> IO (Either String DynamicImage)
readPnmImage fn = do
  result <- tryIOError (withBinaryFile fn ReadMode loadPNM_main)
  case result of
    Right img -> return (Right img)
    Left e -> if isUserError e
              then return (Left (ioeGetErrorString e))
              else return (Left (show e))

-- EOF
