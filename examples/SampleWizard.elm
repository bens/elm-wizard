module SampleWizard exposing (Msg, ReturnMsg, Model, WizardModel, init, restart, update, view)

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events exposing (onClick, onInput)

import Wizard as W exposing (Step, Wizard)


type alias Msg =
  W.WizardMsg SMsg Model


type alias ReturnMsg =
  W.Msg SMsg Model


type alias WizardModel =
  W.Model SMsg Model


type SMsg
  = Title String
  | Incr


type alias Model
  = (String, Int)


wizard : Model -> Wizard SMsg Model
wizard =
  W.wizard (W.defaultView True) stepA [stepB]


{ init, update, view } =
  wizard ("", 0)


restart : Model -> WizardModel
restart m =
  (wizard m).init


stepA : Step SMsg Model
stepA =
  let update msg (name, x) =
        case msg of
          Title name' ->
            ((name', x), Cmd.none)
          Incr ->
            ((name, x), Cmd.none)

      view (name, x) =
        Html.div []
          [ Html.text "Name:"
          , Html.input [ Attr.value name
                       , Attr.size 10
                       , onInput Title
                       ] []
          ]

  in { update = update, view = view }


stepB : Step SMsg Model
stepB =
  let update msg (name, x) =
        case msg of
          Title _ ->
            ((name, x), Cmd.none)
          Incr ->
            ((name, x + 1), Cmd.none)

      view (name, x) =
        Html.div []
          [ Html.text ("Name: " ++ name)
          , Html.br [] []
          , Html.text ("Number: " ++ toString x)
          , Html.br [] []
          , Html.button [onClick Incr] [Html.text "+1"]
          ]

  in { update = update, view = view }
