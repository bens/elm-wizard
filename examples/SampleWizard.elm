module SampleWizard exposing (Msg, ReturnMsg, Model, init, update, view)

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events exposing (onClick, onInput)

import Wizard as W exposing (Step, Wizard)


type alias Msg =
  W.WizardMsg SMsg SModel


type alias ReturnMsg =
  W.Msg SMsg SModel


type alias Model =
  W.Model SMsg SModel


type SMsg
  = Title String
  | Incr


type alias SModel
  = (String, Int)


{ init, update, view } =
  W.wizard (W.defaultView True) stepA [stepB, stepA] ("", 0)


stepA : Step SMsg SModel
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


stepB : Step SMsg SModel
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
