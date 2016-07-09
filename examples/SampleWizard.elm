module SampleWizard exposing (Msg, Model, WizardModel, init, restart, update, view)

import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events exposing (onClick, onInput)
import String

import Wizard as W exposing (Step, Wizard)


type alias Msg =
  W.ServerMsg SError SMsg Model


type alias WizardModel =
  W.Model SError SMsg Model


type SMsg
  = Title String
  | Incr


type SError
  = NoName
  | Zero


type Model
  = Model (Validated String) (Validated Int)


type Validated a
  = Ok a
  | Bad a


vvalue : Validated a -> a
vvalue v =
  case v of
    Ok x -> x
    Bad x -> x


vvalid : Validated a -> Bool
vvalid v =
  case v of
    Ok _ -> True
    Bad _ -> False


wizard : Model -> Wizard SError SMsg Model
wizard =
  W.wizard (W.defaultView True) stepA [stepB]


{ init, update, view } =
  wizard (Model (Ok "") (Ok 0))


restart : Model -> WizardModel
restart m =
  (wizard m).init


stepA : Step SError SMsg Model
stepA =
  let update msg (Model name x) =
        case msg of
          W.Msg (Title name') ->
            (Model (Ok name') x, Cmd.none)
          W.Msg Incr ->
            (Model name x, Cmd.none)
          W.InvalidModel errs ->
            (Model (Bad (vvalue name)) x, Cmd.none)
      view (Model name x) =
        Html.div []
          ( [ Html.text "Name: "
            , Html.input [ Attr.value (vvalue name)
                         , Attr.size 10
                         , onInput Title
                         ] []
            ] ++
            (if vvalid name then [] else [Html.text "●"])
          )
      check (Model name _) =
        if String.isEmpty (vvalue name) then W.Failed [NoName] else W.Ok
  in { update = update, view = view, check = check }


stepB : Step SError SMsg Model
stepB =
  let update msg (Model name x) =
        case msg of
          W.Msg (Title _) ->
            (Model name x, Cmd.none)
          W.Msg Incr ->
            (Model name (Ok (vvalue x + 1)), Cmd.none)
          W.InvalidModel errs ->
            (Model name (Bad (vvalue x)), Cmd.none)
      view (Model name x) =
        Html.div []
          ( [ Html.text ("Name: " ++ vvalue name)
            , Html.br [] []
            , Html.text ("Number: " ++ toString (vvalue x))
            ] ++
            (if vvalid x then [] else [Html.text "●"]) ++
            [ Html.br [] []
            , Html.button [onClick Incr] [Html.text "+1"]
            ]
          )
      check (Model _ x) =
        if vvalue x > 0 then W.Ok else W.Failed [Zero]
  in { update = update, view = view, check = check }
