module Main exposing (main)

import Html exposing (Html)
import Html.App as App
import Html.Attributes as Attr
import Html.Events exposing (onClick)

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
  = NewWizard
  | EditWizard Int Samp.Model
  | WizardMsg Samp.Msg


type alias Model
  = { wizard : WizardState
    , items : List Samp.Model
    }


type WizardState
  = NoWizard
  | CreatingWizard Samp.WizardModel
  | EditingWizard Int Samp.WizardModel


init : (Model, Cmd Msg)
init =
  ({ wizard = NoWizard, items = [] }, Cmd.none)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NewWizard ->
      ({ model | wizard = CreatingWizard Samp.init }, Cmd.none)
    EditWizard i m ->
      ({ model | wizard = EditingWizard i (Samp.restart m) }, Cmd.none)
    WizardMsg (W.Completed model') ->
      case model.wizard of
        NoWizard ->
          (model, Cmd.none)
        CreatingWizard _ ->
          ({ model | wizard = NoWizard, items = model.items ++ [model'] }, Cmd.none)
        EditingWizard i _ ->
          let items' =
                List.indexedMap (\j m -> if i == j then model' else m) model.items
          in ({ model | wizard = NoWizard, items = items' }, Cmd.none)
    WizardMsg W.Cancelled ->
      ({ model | wizard = NoWizard }, Cmd.none)
    WizardMsg (W.WizardMsg wmsg) ->
      case model.wizard of
        NoWizard ->
          (model, Cmd.none)
        CreatingWizard m ->
          case Samp.update wmsg m of
            (m', wcmd) ->
              ({ model | wizard = CreatingWizard m' }, Cmd.map WizardMsg wcmd)
        EditingWizard i m ->
          case Samp.update wmsg m of
            (m', wcmd) ->
              ({ model | wizard = EditingWizard i m' }, Cmd.map WizardMsg wcmd)


view : Model -> Html Msg
view model =
  let wizard m =
        [App.map (W.WizardMsg >> WizardMsg) (Samp.view m)]
      newBtn =
        Html.button [onClick NewWizard]
          [Html.text "New"]
      items =
        Html.ul [ Attr.class "items"
                , Attr.style [ ("width", "20em")
                             , ("border", "solid thin lightgrey")
                             , ("padding", "2em")
                             ]
                ]
          (List.indexedMap item model.items)
      item i m =
        Html.li [ Attr.style [ ("border", "solid thin #CCC")
                             , ("background-color", "#EEE")
                             , ("margin", "0.3em")
                             , ("padding", "0.3em")
                             ]
                ]
          [Html.a [ Attr.class "link", onClick (EditWizard i m) ]
             [Html.text (toString m)]]
  in Html.div [ Attr.style [ ("margin", "2em") ] ]
       (case model.wizard of
          NoWizard ->
            [newBtn, items]
          CreatingWizard m ->
            wizard m
          EditingWizard _ m ->
            wizard m)


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none
