module SampleWizard exposing (Msg, Model, WizardModel, init, restart, update, view)

import Cons exposing (Cons)
import Either exposing (..)
import Html exposing (Html)
import Html.App as App
import Html.Attributes as Attr
import Html.Events exposing (onClick, onInput)
import String

import Wizard as W exposing (Step, Wizard)


type alias Msg =
  W.Msg SMsg


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
  = Good a
  | Bad a


vvalue : Validated a -> a
vvalue v =
  case v of
    Good x -> x
    Bad x -> x


vvalid : Validated a -> Bool
vvalid v =
  case v of
    Good _ -> True
    Bad _ -> False


wizard : Model -> Wizard SError SMsg Model
wizard =
  W.wizard (viewWizard True) (Cons.cons stepA [stepB])


{ init, update, view } =
  wizard (Model (Good "") (Good 0))


restart : Model -> WizardModel
restart m =
  (wizard m).init


stepA : Step SError SMsg Model
stepA =
  let update msg (Model name x) =
        case msg of
          Right (Title name') ->
            (Model (Good name') x, Cmd.none)
          Right Incr ->
            (Model name x, Cmd.none)
          Left (W.InvalidModel errs) ->
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
        if String.isEmpty (vvalue name) then Err [NoName] else Ok ()
  in { update = update, view = view, check = check }


stepB : Step SError SMsg Model
stepB =
  let update msg (Model name x) =
        case msg of
          Right (Title _) ->
            (Model name x, Cmd.none)
          Right Incr ->
            (Model name (Good (vvalue x + 1)), Cmd.none)
          Left (W.InvalidModel errs) ->
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
        if vvalue x > 0 then Ok () else Err [Zero]
  in { update = update, view = view, check = check }


viewWizard : Bool -> W.ViewModel model -> Html msg -> Html (W.Msg msg)
viewWizard showDebug m body =
  let empty = Html.text "○"
      full = Html.text "●"
      html =
        [ Html.p [ Attr.class "wizard-step"
                 , Attr.style [("width", "15em"), ("height", "5em")]
                 , border
                 , spacing 1
                 ]
            [App.map W.embedMsg body]
        , Html.span [ spacing 0.5 ]
            [ Html.button [spacing 0.2, onClick W.back]
                [Html.text (if m.currentStep == 0 then "Cancel" else "Back ⇐")]
            , Html.span [spacing 0.2]
                (List.repeat m.currentStep empty ++
                   [full] ++
                   List.repeat (m.numSteps - m.currentStep - 1) empty)
            , Html.button [ spacing 0.2
                          , onClick W.forward
                          ]
                [Html.text (if m.currentStep == m.numSteps - 1 then "Finish" else "⇒ Next")]
            ]
        ]
      debug =
        [ Html.p [ Attr.class "wizard-debug"
                 , Attr.style [ ("width", "15em")
                              , ("background-color", "#eeeeee")
                              ]
                 , border
                 , spacing 1
                 ]
            [ Html.text ("DEBUG: " ++ toString m.model) ]
        ]
  in
  Html.div [ Attr.class "wizard"
           , border
           , spacing 1
           ]
    (html ++ (if showDebug then debug else []))


spacing : Float -> Html.Attribute msg
spacing n =
  Attr.style [ ("margin", toString n ++ "em")
             , ("padding", "0.5em")
             ]


border : Html.Attribute msg
border =
  Attr.style [ ("border", "thin lightgrey solid") ]
