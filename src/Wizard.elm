module Wizard exposing
  ( Msg, OutMsg(..), StepMsg(..), Model, Wizard, Step, View, ViewModel
  , wizard, embedMsg, forward, back
  )

{-| A module for building multi-step wizards with model validation at each step
to control progress.

@docs Model

# Messages
@docs Msg, OutMsg, StepMsg, forward, back, embedMsg

# The Wizard
@docs Wizard, Step, wizard

# Views
@docs View, ViewModel

-}

import Cons exposing (Cons)
import Either exposing (..)
import Html exposing (Html)
import List.Extra as List
import Maybe.Extra as Maybe
import Task


{-| Messages delivered to the steps.
-}
type StepMsg err
  = InvalidModel (List err)


{-| Private wizard messages.
-}
type Msg msg
  = Forward
  | Back
  | StepMsg msg


{-| Messages returned to the component embedding the Wizard.
-}
type OutMsg model
  = Completed model
  | Cancelled


{-| A wizard model.  It uses a zipper representation for the steps.
-}
type Model err msg model
  = Model
    { model : model
    , step : Step err msg model
    , left : List (Step err msg model)
    , right : List (Step err msg model)
    }


{-| `update` can generate `OutMsg` messages which pass information about
completion or cancellation up to the embedding component. -}
type alias Wizard err msg model
  = { init : Model err msg model
    , update : Msg msg
             -> Model err msg model
             -> (Model err msg model, Either (OutMsg model) (Cmd (Msg msg)))
    , view : Model err msg model -> Html (Msg msg)
    }


{-| A step of a wizard.  A step's `update` function can receive `StepMsg`
messages that indicate that a check of the model failed.
-}
type alias Step err msg model
  = { update : Either (StepMsg err) msg -> model -> (model, Cmd msg)
    , view : model -> Html msg
    , check : model -> Result (List err) ()
    }


{-| A view function for the wizard.  It has access to a model of the wizard, and
`Html` which the current step has generated that it can embed.  Forward and back
messages may be generated by a `View` function.
-}
type alias View msg model
  = ViewModel model -> Html msg -> Html (Msg msg)


{-| Model data that can be used for generating the wizard's view.
-}
type alias ViewModel model
  = { model : model
    , numSteps : Int
    , currentStep : Int
    }


{-| Build a wizard value.  It needs a view function to render the wizard
controls, a non-empty list of steps of the wizard, and the initial model.
-}
wizard : View msg model
       -> Cons (Step err msg model)
       -> model
       -> Wizard err msg model
wizard render steps init =
  let model =
        Model
          { model = init
          , step = Cons.head steps
          , left = []
          , right = Cons.tail steps
          }
  in { init = model, update = update, view = view render }


{-| Return a value as a Cmd.
-}
command : a -> Cmd a
command x =
  Task.perform identity identity (Task.succeed x)


update : Msg msg
       -> Model err msg model
       -> (Model err msg model, Either (OutMsg model) (Cmd (Msg msg)))
update msg (Model m) =
  case msg of
    Forward ->
      case m.step.check m.model of
        Ok () ->
          stepForward (Model m) |> updateMove (Completed m.model) (Model m)
        Err errs ->
          updateStep (Left (InvalidModel errs)) (Model m)
    Back ->
      stepBack (Model m) |> updateMove Cancelled (Model m)
    StepMsg smsg ->
      updateStep (Right smsg) (Model m)


updateStep : Either (StepMsg err) msg
           -> Model err msg model
           -> (Model err msg model, Either (OutMsg model) (Cmd (Msg msg)))
updateStep msg (Model m) =
  case m.step.update msg m.model of
    (m', cmd) ->
      (Model { m | model = m' }, Right (Cmd.map StepMsg cmd))


updateMove : OutMsg model
           -> Model err msg model
           -> Maybe (Model err msg model)
           -> (Model err msg model, Either (OutMsg model) (Cmd (Msg msg)))
updateMove omsg m =
  Maybe.mapDefault (m, Left omsg) (\m' -> (m', Right Cmd.none))


view : View msg model
     -> Model err msg model
     -> Html (Msg msg)
view render (Model m) =
  let m' = { model = m.model
           , numSteps = List.length m.left + List.length m.right + 1
           , currentStep = List.length m.left
           }
  in render m' (m.step.view m.model)


{-| A message to move forward in the sequence of steps.
-}
forward : Msg msg
forward =
  Forward


{-| A message to move backward in the sequence of steps.
-}
back : Msg msg
back =
  Back


{-| Embed a `msg` into a `Msg`.
-}
embedMsg : msg -> Msg msg
embedMsg =
  StepMsg


--
-- ZIPPERS
--


stepForward : Model err msg model -> Maybe (Model err msg model)
stepForward (Model m) =
  List.uncons m.right |>
    Maybe.mapDefault
      Nothing
      (\(r, rs) -> Just (Model { m | step = r, left = m.step :: m.left, right = rs }))


stepBack : Model err msg model -> Maybe (Model err msg model)
stepBack (Model m) =
  List.uncons m.left |>
    Maybe.mapDefault
      Nothing
      (\(l, ls) -> Just (Model { m | step = l, left = ls, right = m.step :: m.right }))
