module Blessed.Internal.BlessedOp where


import Prelude


import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Exception (Error)
import Effect.Exception as Error

import Data.Bifunctor (lmap, bimap)
import Data.Either as Either
import Data.Traversable (traverse, traverse_)
import Data.Tuple.Nested ((/\), type (/\))
import Data.Tuple (uncurry)
import Data.Tuple (snd) as Tuple

import Data.Codec.Argonaut (JsonCodec, JsonDecodeError, decode, printJsonDecodeError) as CA
import Data.Argonaut.Core (Json, jsonNull)
import Data.Argonaut.Core (fromString) as Json
import Data.Argonaut.Decode (class DecodeJson, decodeJson)
import Data.Argonaut.Encode (class EncodeJson)

import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.Free (Free)
import Control.Monad.Free as Free
import Control.Monad.Rec.Class (class MonadRec, tailRecM, Step(..))
import Control.Monad.State.Class (class MonadState)

import Blessed.Internal.Foreign (encodeCommand) as Foreign
import Blessed.Internal.Command (Command) as I
import Blessed.Internal.NodeKey (NodeKey, RawNodeKey, process, toRaw)  as I
import Blessed.Internal.JsApi as I
import Blessed.Internal.Dump as Dump
import Blessed.Internal.ArgonautCodecExtra as ACX
import Blessed.Internal.Emitter as E


data BlessedOpF state m a
    = GetStateRef (Ref state -> a)
    | State (state -> a /\ state)
    | Lift (m a)
    | PerformOne I.RawNodeKey I.Command a
    | PerformSome I.RawNodeKey (Array I.Command) a
    | PerformGet I.RawNodeKey I.Command (Json -> a)
    | PerformOnProcess I.Command a
    | ConfigureJs BlessedJsConfig a


instance functorBlessedOpF :: Functor m => Functor (BlessedOpF state m) where
    map f = case _ of
        GetStateRef k -> GetStateRef $ map f k
        State k -> State $ lmap f <<< k
        Lift m -> Lift $ map f m
        PerformOne nid cmd a -> PerformOne nid cmd $ f a
        PerformSome nid cmds a -> PerformSome nid cmds $ f a
        PerformGet nid getCmd k -> PerformGet nid getCmd $ map f k
        PerformOnProcess cmd a -> PerformOnProcess cmd $ f a
        ConfigureJs cfg a -> ConfigureJs cfg $ f a


type BlessedOp state m = BlessedOpM state m Unit
type BlessedOp' state m a = BlessedOpM state m a

-- FIXME: these are not needed to distinguish between
type BlessedOpDef state m = Ref state -> BlessedOp state m
-- type BlessedOpJsonGet state m a = BlessedOpM state m a
type BlessedOpGet state m a = BlessedOp' state m a
type BlessedOpSet state m = BlessedOp state m


newtype BlessedOpM state m a = BlessedOpM (Free (BlessedOpF state m) a)


derive newtype instance functorBlessedOpM :: Functor (BlessedOpM state m)
derive newtype instance applyBlessedOpM :: Apply (BlessedOpM state m)
derive newtype instance applicativeBlessedOpM :: Applicative (BlessedOpM state m)
derive newtype instance bindBlessedOpM :: Bind (BlessedOpM state m)
derive newtype instance monadBlessedOpM :: Monad (BlessedOpM state m)
derive newtype instance semigroupBlessedOpM :: Semigroup a => Semigroup (BlessedOpM state m a)
derive newtype instance monoidBlessedOpM :: Monoid a => Monoid (BlessedOpM state m a)


instance monadEffectBlessedOpM :: MonadEffect m => MonadEffect (BlessedOpM state m) where
  liftEffect = BlessedOpM <<< Free.liftF <<< Lift <<< liftEffect


instance monadAfBlessedOpM :: MonadAff m => MonadAff (BlessedOpM state m) where
  liftAff = BlessedOpM <<< Free.liftF <<< Lift <<< liftAff


instance monadStateBlessedOpM :: MonadState state (BlessedOpM state m) where
  state = BlessedOpM <<< Free.liftF <<< State


instance monadThrowBlessedOpM :: MonadThrow e m => MonadThrow e (BlessedOpM state m) where
  throwError = BlessedOpM <<< Free.liftF <<< Lift <<< throwError


instance monadRecBlessedOpM :: MonadRec (BlessedOpM state m) where
  tailRecM k a = k a >>= case _ of
    Loop x -> tailRecM k x
    Done y -> pure y


getStateRef :: forall state m. BlessedOpGet state m (Ref state)
getStateRef = BlessedOpM $ Free.liftF $ GetStateRef identity


{- Processing -}

data BlessedError =
    FromJson CA.JsonDecodeError


toError :: BlessedError -> Error
toError (FromJson jsonError) = Error.error $ CA.printJsonDecodeError jsonError


-- FIXME: simplify the chain of classes

class Gets :: (Type -> Type) -> Type -> Constraint
class (MonadThrow Error m, DecodeJson a) <= Gets m a
instance (MonadThrow Error m, DecodeJson a) => Gets m a


class GetsC :: forall k. (Type -> Type) -> k -> Constraint
class (MonadThrow Error m) <= GetsC m a
instance (MonadThrow Error m) => GetsC m a


class Sets :: (Type -> Type) -> Type -> Constraint
class (MonadThrow Error m, EncodeJson a) <= Sets m a
instance (MonadThrow Error m, EncodeJson a) => Sets m a


class SetsC :: forall k. (Type -> Type) -> k -> Constraint
class (MonadThrow Error m) <= SetsC m a
instance (MonadThrow Error m) => SetsC m a


perform :: forall state m. I.RawNodeKey -> I.Command -> BlessedOp state m
perform nid cmd = BlessedOpM $ Free.liftF $ PerformOne nid cmd unit


performGet :: forall state m a. Gets m a => I.RawNodeKey -> I.Command -> BlessedOpGet state m a
performGet nid cmd = do -- ?wh <$> (BlessedOpM $ Free.liftF $ PerformGet nid cmd $ (decodeJson >>> lmap ACX.convertJsonError))
    val <- BlessedOpM $ Free.liftF $ PerformGet nid cmd $ (decodeJson >>> lmap ACX.convertJsonError)
    Either.either (FromJson >>> toError >>> throwError) pure val -- should be resolved by instances of `MonadThrow`?


performGetC :: forall state m a. GetsC m a => CA.JsonCodec a -> I.RawNodeKey -> I.Command -> BlessedOpGet state m a
performGetC codec nid cmd = do
    val <- BlessedOpM $ Free.liftF $ PerformGet nid cmd $ CA.decode codec
    Either.either (FromJson >>> toError >>> throwError) pure val -- should be resolved by instances of `MonadThrow`?


performSome :: forall state m. I.RawNodeKey -> Array I.Command -> BlessedOp state m
performSome nid cmds = BlessedOpM $ Free.liftF $ PerformSome nid cmds unit


performOnProcess :: forall state m. I.Command -> BlessedOp state m
performOnProcess cmd = BlessedOpM $ Free.liftF $ PerformOnProcess cmd unit


configureJs :: forall state m. BlessedJsConfig -> BlessedOp state m
configureJs cfg = BlessedOpM $ Free.liftF $ ConfigureJs cfg unit


configureJs' :: BlessedJsConfig -> Effect Unit
configureJs' = configureBlessedAndLogging_ <<< _adaptConfigRec


-- type Performer m = I.Command -> m Unit -- TODO: return m (Maybe Json), for getters


lift :: forall state m. m Unit -> BlessedOpM state m Unit
lift = lift'


lift' :: forall state m. m ~> BlessedOpM state m
lift' m = BlessedOpM $ Free.liftF $ Lift m


impair :: forall state m x. MonadEffect m => MonadRec m => BlessedOpM state m x -> BlessedOpM state m (m x)
impair op =
    getStateRef <#> \stateRef -> runM' stateRef op


impair1 :: forall state m a x. MonadEffect m => MonadRec m => (a -> BlessedOpM state m x) -> BlessedOpM state m (a -> m x)
impair1 fn =
    getStateRef <#> \stateRef a -> runM' stateRef $ fn a


impair2 :: forall state m a b x. MonadEffect m => MonadRec m => (a -> b -> BlessedOpM state m x) -> BlessedOpM state m (a -> b -> m x)
impair2 fn =
    getStateRef <#> \stateRef a b -> runM' stateRef $ fn a b


impair3 :: forall state m a b c x. MonadEffect m => MonadRec m => (a -> b -> c -> BlessedOpM state m x) -> BlessedOpM state m (a -> b -> c -> m x)
impair3 fn =
    getStateRef <#> \stateRef a b c -> runM' stateRef $ fn a b c


impair4 :: forall state m a b c d x. MonadEffect m => MonadRec m => (a -> b -> c -> d -> BlessedOpM state m x) -> BlessedOpM state m (a -> b -> c -> d -> m x)
impair4 fn =
    getStateRef <#> \stateRef a b c d -> runM' stateRef $ fn a b c d



runM
    :: forall state m
     . MonadEffect m
    => MonadRec m
    => state
    -> BlessedOpM state m
    ~> m
runM state blessedM =
    liftEffect (Ref.new state) >>= \stateRef -> runM' stateRef blessedM -- flip?



runM'
    :: forall state m
     . MonadEffect m
    => MonadRec m
    => Ref state
    -> BlessedOpM state m
    ~> m
runM' stateRef (BlessedOpM blessedFree) =
    runFreeM stateRef blessedFree


runFreeM
    :: forall state m
     . MonadEffect m
    => MonadRec m
    => Ref state
    -> Free (BlessedOpF state m)
    ~> m
runFreeM stateRef fn = do
    Free.runFreeM go fn
    where
        go :: forall a. BlessedOpF state m a -> m a

        go (GetStateRef getV) =
            pure $ getV stateRef

        go (State f) = do
            state <- getUserState
            case f state of
                next /\ nextState -> do
                    writeUserState state nextState
                    pure next

        go (Lift m) = m

        go (PerformOne target cmd next) = do
            logCommandToPerform cmd
            _ <- liftEffect $ callForeignCommand target cmd
            logCommandWasPerformed cmd
            pure next

        go (PerformSome target cmds next) = do
            traverse_ logCommandToPerform cmds
            _ <- traverse (liftEffect <<< callForeignCommand target) cmds
            traverse_ logCommandWasPerformed cmds
            pure next

        go (PerformGet target cmd getV) = do
            logCommandToPerform cmd
            value <- liftEffect $ callForeignCommand target cmd
            logCommandWasPerformed cmd
            pure $ getV value

        go (PerformOnProcess cmd next) = do
            logCommandToPerform cmd
            _ <- liftEffect $ callForeignCommand (I.toRaw I.process) cmd
            logCommandWasPerformed cmd
            pure next

        go (ConfigureJs cfg next) = do
            _ <- liftEffect $ configureJs' cfg
            pure next

        logCommandToPerform    cmd = liftEffect $ logCommandWhenEnabled_ $ \_ -> "before :: " <> Dump.dump cmd
        logCommandWasPerformed cmd = liftEffect $ logCommandWhenEnabled_ $ \_ -> "after  :: " <> Dump.dump cmd
        getUserState = liftEffect $ Ref.read stateRef
        writeUserState _ nextState = liftEffect $ Ref.modify_ (const nextState) stateRef
        callForeignCommand target = Foreign.encodeCommand >>> uncurry (callCommandEx_ $ I.toUniqueJsKey target)
            -- FIXME: always calls `CallCommandEx_` now, add some flag to easily distinguish if we really need it
            -- case Foreign.encodeCommand cmd of
            --     cmd_ /\ [] -> callCommand_ target cmd_
            --     cmd_ /\ handlers -> callCommandEx_ target cmd_ handlers
            -- FIXME: also the marker for the check in `BlessedOp.js` :
            -- case 'call':
            --      if (command.marker == 'CallCommandEx') {
            -- should be set to the according one this way

runAndGet
    :: forall state m a
     . MonadEffect m
    => MonadRec m
    => state
    -> BlessedOpM state m a
    -> m (a /\ state)
runAndGet state blessedM = do
    stateRef <- liftEffect $ Ref.new state
    a <- runM' stateRef blessedM
    sAfter <- liftEffect $ Ref.read stateRef
    pure $ a /\ sAfter


runAndGet'
    :: forall state m
     . MonadEffect m
    => MonadRec m
    => state
    -> BlessedOpM state m Unit
    -> m state
runAndGet' state blessedM = do
    runAndGet state blessedM <#> Tuple.snd


runOn :: forall s s' m a. MonadRec m => MonadEffect m => s -> BlessedOpM s m a -> BlessedOpM s' m a
runOn s = lift' <<< runM s


runOnUnit :: forall s m a. MonadRec m => MonadEffect m => BlessedOpM Unit m a -> BlessedOpM s m a
runOnUnit = runOn unit


runOver :: forall s s' m a. MonadState s m => MonadRec m => MonadEffect m => s -> BlessedOpM s m a -> BlessedOpM s' m (a /\ s)
runOver s op = lift' $ runAndGet s op


runOver' :: forall s s' m. MonadState s m => MonadRec m => MonadEffect m => s -> BlessedOpM s m Unit -> BlessedOpM s' m s
runOver' s op = lift' $ runAndGet' s op


runEffect :: forall s m a. MonadRec m => MonadEffect m => s -> BlessedOpM s Effect a -> BlessedOpM s m a
runEffect s = lift' <<< liftEffect <<< runM s


makeHandler :: forall state subj sym. I.NodeKey subj sym -> E.EventId -> Array Json -> (I.NodeKey subj sym -> I.EventJson -> BlessedOp state Effect) -> I.SHandler state
makeHandler nodeKey eventId arguments op =
    I.SHandler eventId arguments
        $ \stateRef rawNodeKey evtJson -> do
            -- TODO: check IDs match?
            liftEffect $ logCommandWhenEnabled_ $ \_ -> Dump.dump $ Dump.toCallDump rawNodeKey eventId arguments
            runM' stateRef $ op nodeKey evtJson


imapStateF :: forall stateA stateB m a. MonadEffect m => (stateA -> stateB) -> (stateB -> stateA) -> BlessedOpF stateA m a -> BlessedOpF stateB m (m a)
imapStateF toStateB toStateA = case _ of
    GetStateRef k -> GetStateRef $ \refB -> liftEffect $ Ref.read refB >>= (toStateA >>> Ref.new) >>= (k >>> pure)
    State k -> State \stateB -> bimap pure toStateB $ k $ toStateA stateB
    Lift m -> Lift $ pure m
    PerformOne nid cmd a -> PerformOne nid cmd $ pure a
    PerformSome nid cmds a -> PerformSome nid cmds $ pure a
    PerformGet nid getCmd k -> PerformGet nid getCmd (k >>> pure)
    PerformOnProcess cmd a -> PerformOnProcess cmd $ pure a
    ConfigureJs config a -> ConfigureJs config $ pure a



-- TODO: To/FromRepr + imapState


-- imapStateM :: forall stateA stateB m a. MonadEffect m => (stateA -> stateB) -> (stateB -> stateA) -> BlessedOpM stateA m a -> BlessedOpM stateB m (m a)
-- imapStateM atob btoa (BlessedOpM free) = BlessedOpM $ Free.liftF $ imapState atob btoa $ ?wh free


data LoggingTarget
    = LoggingOff
    | Console
    | File String


type BlessedJsConfig =
    { blessedOn :: Boolean
    , loggingBlessedTo  :: LoggingTarget
    , loggingCommandsTo :: LoggingTarget
    }


_adaptConfigRec :: BlessedJsConfig -> I.ConfigEnc
_adaptConfigRec cfg =
    I.ConfigEnc
        { blessedOn : cfg.blessedOn
        , loggingBlessedTo  : convertLogging cfg.loggingBlessedTo
        , loggingCommandsTo : convertLogging cfg.loggingCommandsTo
        }
    where
        convertLogging = case _ of
            LoggingOff -> jsonNull
            Console -> Json.fromString "console"
            File fileName -> Json.fromString fileName


foreign import execute_ :: I.BlessedEnc -> Effect Unit
foreign import registerNode_ :: I.NodeEnc -> Effect Unit
foreign import callCommandEx_ :: I.JsNodeUniqueKey -> I.CommandEnc -> Array I.HandlerCallEnc -> Effect Json
foreign import configureBlessedAndLogging_ :: I.ConfigEnc -> Effect Unit
foreign import logCommandWhenEnabled_ :: (Unit -> String) -> Effect Unit
