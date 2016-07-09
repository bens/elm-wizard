module Main exposing (main)

import Html exposing (Html)
import Html.App as App

import Wizard as W
import SampleWizard as Samp


main : Program Never
main =
  App.program
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }


type Msg
  = WizardMsg Samp.ReturnMsg


type alias Model
  = { wizard : Samp.Model }


init : (Model, Cmd Msg)
init =
  ({ wizard = Samp.init }, Cmd.none)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    WizardMsg (W.Completed model') ->
      (always model (Debug.log "COMPLETED" model'), Cmd.none)
    WizardMsg W.Cancelled ->
      (always model (Debug.log "CANCELLED" 0), Cmd.none)
    WizardMsg (W.WizardMsg wmsg) ->
      case Samp.update wmsg model.wizard of
        (wmodel, wcmd) ->
          ({ model | wizard = wmodel }, Cmd.map WizardMsg wcmd)


view : Model -> Html Msg
view model =
  App.map (W.WizardMsg >> WizardMsg) (Samp.view model.wizard)


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none
