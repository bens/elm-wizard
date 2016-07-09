module Wizard exposing (WizardMsg, Msg(..), Model, Wizard, Step, ViewModel, wizard, defaultView)

{-| A wizard module.

@docs Msg, WizardMsg, Model

# The Wizard
@docs Wizard, Step, wizard

# View Rendering
@docs ViewModel, defaultView

-}

import Html exposing (Html)
import Html.App as App
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import List.Extra as List
import Maybe.Extra as Maybe
import Task


{-| A wizard message.
-}
type WizardMsg msg
  = Forward
  | Back
  | StepMsg msg


{-| Possibly a message for the parent
-}
type Msg msg model
  = WizardMsg (WizardMsg msg)
  | Completed model
  | Cancelled


{-| A step of a wizard.
-}
type alias Step msg model
  = { update : msg -> model -> (model, Cmd msg)
    , view : model -> Html msg
    }


{-| A wizard model.
-}
type Model msg model
  = Model
    { model : model
    , step : Step msg model
    , left : List (Step msg model)
    , right : List (Step msg model)
    }


{-| A wizard.
-}
type alias Wizard msg model
  = { init : Model msg model
    , update : WizardMsg msg -> Model msg model -> (Model msg model, Cmd (Msg msg model))
    , view : Model msg model -> Html (WizardMsg msg)
    }


{-| A model for generating the view
-}
type alias ViewModel model
  = { model : model
    , numSteps : Int
    , currentStep : Int
    }


{-| Build a wizard value.
-}
wizard : (ViewModel model -> Html msg -> Html (WizardMsg msg))
       -> Step msg model
       -> List (Step msg model)
       -> model
       -> Wizard msg model
wizard render step steps init =
  let model =
        Model
          { model = init
          , step = step
          , left = []
          , right = steps
          }
  in
  { init = model, update = update, view = view render }


command : a -> Cmd a
command x =
  Task.perform identity identity (Task.succeed x)


update : WizardMsg msg
       -> Model msg model
       -> (Model msg model, Cmd (Msg msg model))
update msg (Model model) =
  case msg of
    Forward ->
      Maybe.mapDefault
        (Model model, command (Completed model.model))
        (\m -> (m, Cmd.none))
        (forward (Model model))
    Back ->
      Maybe.mapDefault
        (Model model, command Cancelled)
        (\m -> (m, Cmd.none))
        (back (Model model))
    StepMsg m ->
      case model.step.update m model.model of
        (model', cmd) ->
          (Model { model | model = model' }, Cmd.map (StepMsg >> WizardMsg) cmd)


view : (ViewModel model -> Html msg -> Html (WizardMsg msg))
     -> Model msg model
     -> Html (WizardMsg msg)
view render (Model m) =
  let m' = { model = m.model
           , numSteps = List.length m.left + List.length m.right + 1
           , currentStep = List.length m.left
           }
  in render m' (m.step.view m.model)


{-| The default way to render a wizard.
-}
defaultView : Bool -> ViewModel model -> Html msg -> Html (WizardMsg msg)
defaultView showDebug m body =
  let empty = Html.text "○"
      full = Html.text "●"
      html =
        [ Html.p [ Attr.class "wizard-step"
                 , Attr.style [("width", "8em"), ("height", "5em")]
                 , border
                 , spacing 1
                 ]
            [App.map StepMsg body]
        , Html.span [ spacing 0.5 ]
            [ Html.button [spacing 0.2, onClick Back]
                [Html.text (if m.currentStep == 0 then "Cancel" else "Back ⇐")]

            , Html.span [spacing 0.2]
                (List.repeat m.currentStep empty ++
                   [full] ++
                   List.repeat (m.numSteps - m.currentStep - 1) empty)
            , Html.button [ spacing 0.2
                          , onClick Forward
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


--
-- ZIPPERS
--


forward : Model msg model -> Maybe (Model msg model)
forward (Model m) =
  List.uncons m.right |>
    Maybe.mapDefault
      Nothing
      (\(r, rs) -> Just (Model { m | step = r, left = m.step :: m.left, right = rs }))


back : Model msg model -> Maybe (Model msg model)
back (Model m) =
  List.uncons m.left |>
    Maybe.mapDefault
      Nothing
      (\(l, ls) -> Just (Model { m | step = l, left = ls, right = m.step :: m.right }))
