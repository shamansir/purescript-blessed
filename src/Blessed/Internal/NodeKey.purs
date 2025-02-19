module Blessed.Internal.NodeKey where

import Prelude
import Prim.Symbol (class Append) as S

import Data.Enum (class Enum)
import Data.Newtype (class Newtype)
import Data.Symbol (class IsSymbol, reflectSymbol, reifySymbol)
import Data.Maybe (maybe, Maybe(..))
import Type.Proxy (Proxy(..))
import Data.Tuple (curry, uncurry)
import Data.Tuple.Nested ((/\), type (/\))
import Unsafe.Coerce (unsafeCoerce)
import Data.Unfoldable1 (class Unfoldable1)
import Data.Unfoldable1.Extra (iterateN)


import Data.Argonaut.Encode (class EncodeJson)

import Blessed.Internal.BlessedSubj as K



data NodeKey (kind :: K.Subject) (symbol :: Symbol) = NodeKey (Maybe Int)
newtype RawNodeKey =
    RawNodeKey
        { subject :: K.Subject_
        , userId :: String
        , mbIndex :: Maybe Int
        }


derive instance Newtype RawNodeKey _
derive newtype instance EncodeJson RawNodeKey


-- private
{-
raw :: K.Subject_ -> String -> Int -> RawNodeKey
raw subject userId index = RawNodeKey { subject, userId, index }
-}


infixl 6 make as <^>
infixl 6 type NodeKey as <^>
infixl 6 type NKAppend as <<>>


type NKAppend subj symA symB = forall symC symD. S.Append symA "::" symC => S.Append symC symB symD => NodeKey subj symD
-- type NKAppend' subj symA symB = forall symC symD. S.Append symA "::" symC => S.Append symC symB symD => NodeKey subj symD


make :: forall subj sym. K.IsSubject subj => IsSymbol sym => Proxy subj -> Proxy sym -> NodeKey subj sym
make _ _ = NodeKey Nothing


makeN :: forall subj sym. K.IsSubject subj => IsSymbol sym => Proxy subj -> (Proxy sym /\ Int) -> NodeKey subj sym
makeN _ ( _ /\ n) = NodeKey $ Just n


nk :: forall subj sym. NodeKey subj sym
nk = NodeKey Nothing


first :: forall subj sym. NodeKey subj sym
first = NodeKey $ Just 0


next :: forall subj sym. NodeKey subj sym -> NodeKey subj sym
next (NodeKey maybeN) = NodeKey $ nextN maybeN
    where
        nextN (Just n) = if n < top then Just $ n + 1 else Nothing
        nextN Nothing = Just bottom


prev :: forall subj sym. NodeKey subj sym -> NodeKey subj sym
prev (NodeKey maybeN) = NodeKey $ prevN maybeN
    where
        prevN (Just n) = if n > bottom then Just $ n - 1 else Nothing
        prevN Nothing = Just top


-- private
setN :: forall subj sym. Int -> NodeKey subj sym -> NodeKey subj sym
setN n _ = NodeKey $ Just n


getN :: forall subj sym.  NodeKey subj sym -> Maybe Int
getN (NodeKey maybeN) = maybeN


-- append :: forall subjA symA subjB symB symC symD. S.Append symA "::" symC => S.Append symC symB symD => NodeKey subjA symA -> NodeKey subjB symB -> NodeKey subjB symD
append :: forall subjA symA subjB symB. NodeKey subjA symA -> NodeKey subjB symB -> NKAppend subjB symA symB
append (NodeKey (Just nA)) (NodeKey (Just nB)) = nk # setN (nA * 1000 + nB)
append (NodeKey Nothing) (NodeKey (Just nB)) = nk # setN nB
append (NodeKey (Just nA)) (NodeKey Nothing) = nk # setN (nA * 1000)
append (NodeKey Nothing) (NodeKey Nothing) = nk


makeUnsafe :: forall subj sym. IsSymbol sym => K.IsSubject subj => Proxy subj -> String -> NodeKey subj sym
makeUnsafe subj s = unsafeCoerce $ reifySymbol s \sym -> unsafeCoerce $ make subj sym


toRaw :: forall subj uid. K.IsSubject subj => IsSymbol uid => NodeKey subj uid -> RawNodeKey
toRaw (NodeKey mbIndex) =
    RawNodeKey
        { subject : K.reflectSubject ( Proxy :: _ subj )
        , userId : reflectSymbol ( Proxy :: _ uid )
        , mbIndex
        }


uniqueId :: forall subj uid. K.IsSubject subj => IsSymbol uid => NodeKey subj uid -> String
uniqueId = toRaw >>> uniqueIdRaw


uniqueIdRaw :: RawNodeKey -> String
uniqueIdRaw (RawNodeKey { subject, userId, mbIndex }) =
    K.toString subject <> "__" <> userId <> "__" <> case mbIndex of
        Just n -> show n
        Nothing -> "key"


process :: K.Ext K.Process <^> ""
process = NodeKey Nothing


instance Ord (NodeKey subj sym) where
    compare (NodeKey mbA) (NodeKey mbB) = compare mbA mbB


instance Bounded (NodeKey subj sym) where
    top = NodeKey $ Just top
    bottom = NodeKey $ Just bottom


instance Enum (NodeKey subj sym) where
    succ prevNk =
        let nextNk = next prevNk
        in case nextNk of
            NodeKey (Just _) -> Just nextNk
            NodeKey Nothing -> Nothing
    pred nextNk =
        let prevNk = prev nextNk
        in case prevNk of
            NodeKey (Just _) -> Just prevNk
            NodeKey Nothing -> Nothing


chain :: forall f subj id. Unfoldable1 f => Int -> f (NodeKey subj id)
chain = iterateN next first


continue :: forall f subjA subjB idA idB. Unfoldable1 f => NodeKey subjA idA -> Int -> f (NodeKey subjB idB)
continue (NodeKey (Just n)) = iterateN next $ NodeKey $ Just n
continue (NodeKey Nothing) = iterateN next first


nestChain :: forall f subjA subjB idA idB. Unfoldable1 f => NodeKey subjA idA -> Int -> f (NodeKey subjB idB)
nestChain (NodeKey (Just n)) = iterateN next $ NodeKey $ Just $ n * 1000
nestChain (NodeKey Nothing) = iterateN next first


-- FIXME: `Belongs`?
class (K.Extends parent subj, K.IsSubject parent, K.IsSubject subj, IsSymbol id) <= Respresents parent subj id
instance (K.Extends parent subj, K.IsSubject parent, K.IsSubject subj, IsSymbol id) => Respresents parent subj id


instance Show RawNodeKey where
    show (RawNodeKey { subject, userId, mbIndex }) =
        "(" <> K.toString subject <> "::" <> show userId <> "::" <> maybe "-" show mbIndex <> ")"


instance Eq (NodeKey subj id) where
    eq (NodeKey nA) (NodeKey nB) = nA == nB


instance (K.IsSubject subj, IsSymbol id) => Show (NodeKey subj id) where
    show = toRaw >>> show


instance Eq RawNodeKey where
    eq (RawNodeKey nkA) (RawNodeKey nkB) =
        (K.toString nkA.subject == K.toString nkB.subject)
        && (nkA.userId == nkB.userId)
        && (nkA.mbIndex == nkB.mbIndex)


instance Ord RawNodeKey where
    compare (RawNodeKey nkA) (RawNodeKey nkB) =
        compare (K.toString nkA.subject) (K.toString nkB.subject) <> compare nkA.userId nkB.userId <> compare nkA.mbIndex nkB.mbIndex


newtype HoldsNodeKey = HoldsNodeKey (forall r. (forall subj id. K.IsSubject subj => IsSymbol id => NodeKey subj id -> r) -> r)
