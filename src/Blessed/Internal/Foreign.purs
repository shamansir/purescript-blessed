module Blessed.Internal.Foreign where

import Prelude

import Effect (Effect)
import Effect.Console as Console
import Effect.Ref (Ref)
import Effect.Ref as Ref


import Data.Array ((:))
import Data.Array as Array
import Data.Tuple (snd) as Tuple
import Data.Tuple.Nested ((/\), type (/\))
import Data.Profunctor (wrapIso)
import Data.Maybe (Maybe(..))
import Data.Map (Map)
import Data.Map as Map
import Data.Newtype (class Newtype, unwrap)
import Data.Foldable (foldr)
import Data.FoldableWithIndex (foldrWithIndex)
import Data.Bifunctor (lmap)

import Data.Argonaut.Core (Json)
-- import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Codec.Argonaut as CA

import Blessed.Internal.BlessedSubj as K
import Blessed.Internal.NodeKey as NK
import Blessed.Internal.JsApi as I
import Blessed.Internal.Command as C
import Blessed.Internal.Codec as Codec
import Blessed.Internal.Emitter as E


encode :: forall state. Ref state -> I.SNode state -> I.BlessedEnc
encode stateRef rootNode =
    case encodeRoot stateRef rootNode of
        nodeEnc /\ handlers ->
            I.BlessedEnc $
                { marker : "Blessed"
                , root : CA.encode Codec.nodeEnc nodeEnc
                , handlersFns : handlers
                }


encodeRoot :: forall state. Ref state -> I.SNode state -> I.NodeEnc /\ Array I.HandlerCallEnc
encodeRoot stateRef = encode' stateRef Nothing


newtype HandlerIndex = HandlerIndex Int


uniqueIndex :: NK.RawNodeKey -> E.EventId -> HandlerIndex -> String
uniqueIndex rawNodeKey (E.EventId e) (HandlerIndex localIndex) = NK.uniqueIdRaw rawNodeKey <> "-" <> e.uniqueId <> "-" <> show localIndex


encodeHandler :: forall state. Ref state -> NK.RawNodeKey -> HandlerIndex -> I.SHandler state -> I.HandlerCallEnc
encodeHandler stateRef rnk@(NK.RawNodeKey rawNodeKey) localIndex (I.SHandler (E.EventId e) args fn) =
    I.HandlerCallEnc
        { marker : "HandlerCall"
        , nodeId : I.toUniqueJsKey rnk
        , nodeSubj : K.toString rawNodeKey.subject
        , event : e.type
        , eventUniqueId : e.uniqueId
        , args
        , index : uniqueIndex rnk (E.EventId e) localIndex -- include parent id & total index?
        , call : fn stateRef rnk
        }


encodeHandlerRef :: forall state. NK.RawNodeKey -> HandlerIndex -> I.SHandler state -> I.HandlerRefEnc
encodeHandlerRef rnk@(NK.RawNodeKey rawNodeKey) localIndex (I.SHandler (E.EventId e) _ _) =
    I.HandlerRefEnc
        { marker : "HandlerRef"
        , nodeId : I.toUniqueJsKey rnk
        , nodeSubj : K.toString rawNodeKey.subject
        , event : e.type
        , eventUniqueId : e.uniqueId
        , index : uniqueIndex rnk (E.EventId e) localIndex -- include parent id & total index?
        }


encode' :: forall state. Ref state -> Maybe NK.RawNodeKey -> I.SNode state -> I.NodeEnc /\ Array I.HandlerCallEnc
encode'
    stateRef
    maybeParent
    (I.SNode rawNodeKey@(NK.RawNodeKey rnk) sprops snodes shandlers)

    =
    -- BlessedEnc (CA.encode CA.null unit /\ [ HandlerEnc { nodeId : "test", event: "test", index : -1, call: const $ Console.log "foo" }])
    nodeEncoded /\ (handlersCalls <> childrenHandlers)
    where

        -- propsToMap :: Array I.SProp -> Map String Json
        -- propsToMap = Map.fromFoldable <<< map I.unwrapProp

        adaptProps :: Array I.SProp -> Array I.PropJson
        adaptProps = map (I.unwrapProp >>> \(name /\ value ) -> { name, value })

        encodeHandler' :: HandlerIndex -> I.SHandler state -> I.HandlerCallEnc
        encodeHandler' =
            encodeHandler stateRef rawNodeKey

        encodeHandlerRef' :: HandlerIndex -> I.SHandler state -> I.HandlerRefEnc
        encodeHandlerRef' =
            encodeHandlerRef rawNodeKey

        (storedHandlers :: Array I.HandlerRefEnc) /\ (handlersCalls :: Array I.HandlerCallEnc)
            = foldrWithIndex
                    (\localIdx handler (storedHandlers /\ handlersCalls) ->
                        (
                            (encodeHandlerRef' (HandlerIndex localIdx) handler : storedHandlers)
                            /\ (encodeHandler' (HandlerIndex localIdx) handler : handlersCalls)
                        )
                    )
                    ([] /\ [])
                    shandlers

        (children :: Array I.NodeEnc) /\ (innerHandlersCalls :: Array I.HandlerCallEnc) =
            foldr
                (\(child /\ itsHandlers) ( allChildren /\ allHandlers ) ->
                    (child : allChildren) /\ (itsHandlers <> allHandlers)
                )
                ([] /\ [])
                (encode' stateRef (Just rawNodeKey) <$> snodes)

        (nodeEncoded :: I.NodeEnc) /\ (childrenHandlers :: Array I.HandlerCallEnc) =
            I.NodeEnc
                { marker : "Node"
                , nodeId : I.toUniqueJsKey rawNodeKey
                , nodeSubj : K.toString rnk.subject
                , props : adaptProps sprops
                , children : children
                , handlers : storedHandlers
                , parent : I.toUniqueJsKey <$> maybeParent
                }
            /\ innerHandlersCalls


-- import Data.Codec ((~))
-- import Data.Codec.Argonaut as CA

-- type Person = { name ∷ String, age ∷ Int }

-- codecPerson ∷ CA.JsonCodec Person
-- codecPerson = CA.indexedArray "Test Object" $
--   { name: _, age: _ }
--     <$> _.name ~ CA.index 0 CA.string
--     <*> _.age ~ CA.index 1 CA.int


-- import Data.Codec.Argonaut as CA
-- import Type.Proxy (Proxy(..))

-- type Person = { name ∷ String, age ∷ Int }

-- codecPerson ∷ CA.JsonCodec Person
-- codecPerson =
--   CA.object "Person" $ CA.record
--     # CA.recordProp (Proxy :: _ "name") CA.string
--     # CA.recordProp (Proxy :: _ "age") CA.int


commandToJson :: C.Command -> Json /\ Array I.HandlerCallEnc
commandToJson =
    case _ of
        C.Call { cmd, args } ->
            (CA.encode Codec.callCommandEnc $ I.CallCommandEnc $ { marker : "CallCommand", args, method : cmd, type : "call" }) /\ []
        C.CallEx { cmd, args, handlers } ->
            (CA.encode Codec.callCommandEnc $ I.CallCommandEnc $ { marker : "CallCommandEx", args, method : cmd, type : "call" }) /\ handlers
        C.Get { prop } ->
            (CA.encode Codec.getCommandEnc $ I.GetCommandEnc $ { marker : "GetCommand", property : prop, type : "get" }) /\ []
        C.GetP { path } ->
            (CA.encode Codec.getPCommandEnc $ I.GetPCommandEnc $ { marker : "GetPCommand", path, type : "getp" }) /\ []
        C.Set { prop, value } ->
            (CA.encode Codec.setCommandEnc $ I.SetCommandEnc $ { marker : "SetCommand", value, property : prop, type : "set" }) /\ []
        C.SetP { path, value } ->
            (CA.encode Codec.setPCommandEnc $ I.SetPCommandEnc $ { marker : "SetPCommand", value, path, type : "setp" }) /\ []
        C.Sub { args, event, handler } ->
            (CA.encode Codec.subCommandEnc $ I.SubCommandEnc $ { marker : "SubCommand", args, event, type : "sub" }) /\ [ handler ]
        C.WithProcess { cmd, args } ->
            (CA.encode Codec.processCommandEnc $ I.ProcessCommandEnc $ { marker : "ProcessCommand", args, method : cmd, type : "process" }) /\ []


encodeCommand :: C.Command -> I.CommandEnc /\ Array I.HandlerCallEnc
encodeCommand =
    lmap I.CommandEnc <<< commandToJson