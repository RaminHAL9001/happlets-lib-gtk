module Happlets.Provider.Gtk2.Debug where

import           Control.Concurrent.MVar
import           Control.Exception

import           Data.Bits
import           Data.Word

------------------

-- Debug function selection tags
_mousevt, _keyevt, _animevt, _notanimevt, _drawevt, _audioevt, _textevt, _winevt, _ctxevt,
  _setup, _locks, _infra, _workers, _fulldebug, _allbutanim, _nonverbose,
  _miscA, _miscB, _exceptn, _allevt :: DebugTag

_mousevt    = DebugTag 0x00000001 -- only mouse events
_keyevt     = DebugTag 0x00000002 -- only key events
_animevt    = DebugTag 0x00000004 -- only animation events
_drawevt    = DebugTag 0x00000008 -- only redraw events
_textevt    = DebugTag 0x00000010 -- only screen printer events
_audioevt   = DebugTag 0x00000020 -- only audio events
_ctxevt     = DebugTag 0x00000100 -- only context switching events
_winevt     = DebugTag 0x00000200 -- window manager related events
_allevt     = _mousevt <> _keyevt <> _animevt <> _drawevt <> _audioevt <>
              _textevt <> _winevt <> _ctxevt
_notanimevt = _exclude _animevt _allevt -- all events but animation/audio events
_locks      = DebugTag 0x00010000 -- MVar locking and unlocking  -- WARNING: extremely verbose!!!
_infra      = DebugTag 0x00020000 -- the callback infrastructure -- WARNING: extremely verbose!!!
_setup      = DebugTag 0x00040000
_exceptn    = DebugTag 0x04000000 -- is always set, used to print errors in exception handlers
_workers    = DebugTag 0x08000000 -- when evaluating a 'Worker' thread (can be very verbose)
_miscA      = DebugTag 0x01000000 -- is used to temporarily force a debug message
_miscB      = DebugTag 0x02000000 -- is used to temporarily force a debug message
_fulldebug  = _setup <> _allevt <> _locks <> _infra <> _exceptn <> _workers <> _miscA <> _miscB
_allbutanim = _exclude _animevt _fulldebug
_nonverbose = _exclude (_animevt <> _infra <> _locks <> _workers) _fulldebug

-- Two miscelanea tags are provided, '_miscA' and '_miscB'. When looking through the source code in
-- this file, if you find a debug logging function that you want to force to output, even if it's
-- ordinary tag is not selected, edit the source code on that log function to match one of the
-- miscelanea tags. For example, if you want to report on all calls to 'Cairo.renderWith' but not
-- report on any of the other '_drawevt' tags, then wherever there is a @(logIO _drawevt)@
-- function reporting on the call of of 'Cairo.renderWith', change the tag to
-- @(logIO (_drawevt<>_miscA))@ and then set 'debugThisModule' to '_miscA'.

newtype DebugTag = DebugTag Word32 deriving (Eq, Bits)
instance Semigroup DebugTag where { (<>) = (.|.); }
instance Monoid DebugTag where { mempty = DebugTag 0; mappend = (<>); }

-- | Exclude A from B
_exclude :: DebugTag -> DebugTag -> DebugTag
_exclude excluded selector = selector .&. complement excluded

----------------------------------------------------------------------------------------------------

type Log m = DebugTag -> String -> m ()

newtype CaughtException = CaughtException SomeException
deriving instance Show CaughtException
instance Exception CaughtException

logSubIO :: Log IO -> DebugTag -> String -> IO a -> IO a
logSubIO logIO sel msg f = catches
  (logIO sel ("[BEGIN] "++msg) >> f <* logIO sel ("[ END ] "++msg))
  [ Handler $ \ (CaughtException e) -> throw e
  , Handler $ \ some@(SomeException e) -> do
      logIO _exceptn (msg++' ':show e) >>= evaluate
      throw (CaughtException some)
  ]
{-# INLINE logSubIO #-}

logModMVar :: Log IO -> DebugTag -> String -> MVar a -> (a -> IO (a, b)) -> IO b
logModMVar logIO sel label mvar =
  logSubIO logIO (_locks<>sel) ("modifyMVar "++label) . modifyMVar mvar
{-# INLINE logModMVar #-}

logModMVar_ :: Log IO -> DebugTag -> String -> MVar a -> (a -> IO a) -> IO ()
logModMVar_ logIO sel label mvar =
  logSubIO logIO (_locks<>sel) ("modifyMVar_ "++label) . modifyMVar_ mvar
{-# INLINE logModMVar_ #-}

logWithMVar :: Log IO -> DebugTag -> String -> MVar a -> (a -> IO b) -> IO b
logWithMVar logIO sel label mvar =
  logSubIO logIO (_locks<>sel) ("withMVar "++label) . withMVar mvar
{-# INLINE logWithMVar #-}

