module Main exposing (main)

import Html exposing (Html)
import Html.App as App
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import Maybe.Extra as Maybe

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
  | WizardMsg Samp.ReturnMsg


type alias Model
  = { wizard : Maybe (Maybe Int, Samp.WizardModel)
    , items : List Samp.Model
    }


init : (Model, Cmd Msg)
init =
  ({ wizard = Nothing, items = [] }, Cmd.none)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NewWizard ->
      ({ model | wizard = Just (Nothing, Samp.init) }, Cmd.none)

    EditWizard i m ->
      ({ model | wizard = Just (Just i, Samp.restart m) }, Cmd.none)

    WizardMsg (W.Completed model') ->
      case model.wizard of
        Nothing ->
          (model, Cmd.none)
        Just (Nothing, _) ->
          ({ model | wizard = Nothing, items = model.items ++ [model'] }, Cmd.none)
        Just (Just i, _) ->
          let items' =
                List.indexedMap (\j m -> if i == j then model' else m) model.items
          in ({ model | wizard = Nothing, items = items' }, Cmd.none)

    WizardMsg W.Cancelled ->
      ({ model | wizard = Nothing }, Cmd.none)

    WizardMsg (W.WizardMsg wmsg) ->
      case model.wizard of
        Nothing ->
          (model, Cmd.none)
        Just (iMay, wmodel) ->
          case Samp.update wmsg wmodel of
            (wmodel', wcmd) ->
              ({ model | wizard = Just (iMay, wmodel') }, Cmd.map WizardMsg wcmd)


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
                             , ("padding", "2em") ] ]
          (List.indexedMap item model.items)
      item i m =
        Html.li []
          [Html.a [Attr.class "link", onClick (EditWizard i m)]
             [Html.text (toString m)]]
  in Html.div [ Attr.style [ ("margin", "2em") ] ]
       (Maybe.mapDefault [newBtn, items] (snd >> wizard) model.wizard)


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none
