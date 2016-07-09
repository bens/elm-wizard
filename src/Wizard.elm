module Wizard exposing
  ( StepMsg(..), ServerMsg(..), WizardMsg, Model, Check(..), Wizard, Step, ViewModel
  , wizard, defaultView
  )

{-| A wizard module.

@docs ServerMsg, StepMsg, WizardMsg, Check, Model

# The Wizard
@docs Wizard, Step, wizard

# Views
@docs ViewModel, defaultView

-}

import Html exposing (Html)
import Html.App as App
import Html.Attributes as Attr
import Html.Events exposing (onClick)
import List.Extra as List
import Maybe.Extra as Maybe
import Task


{-| Messages delivered to the steps.
-}
type StepMsg err msg
  = Msg msg
  | InvalidModel (List err)


{-| Private wizard messages.
-}
type WizardMsg err msg
  = Forward
  | Back
  | StepMsg (StepMsg err msg)


{-| Messages returned to the component embedding the Wizard.
-}
type ServerMsg err msg model
  = WizardMsg (WizardMsg err msg)
  | Completed model
  | Cancelled


{-| Result of model validation.
-}
type Check err
  = Ok
  | Failed (List err)


{-| A wizard model.
-}
type Model err msg model
  = Model
    { model : model
    , step : Step err msg model
    , left : List (Step err msg model)
    , right : List (Step err msg model)
    }


{-| A wizard.  `update` can generate `ServerMsg` messages which pass information
about completion or cancellation of the wizard up to the embedding component.
`WizardMsg` messages should be forwarded back to the `update` function as usual.
-}
type alias Wizard err msg model
  = { init : Model err msg model
    , update : WizardMsg err msg
             -> Model err msg model
             -> (Model err msg model, Cmd (ServerMsg err msg model))
    , view : Model err msg model -> Html (WizardMsg err msg)
    }


{-| A step of a wizard.  A step's `update` function can receive `StepMsg`
messages that indicate that a check of the model failed or succeeded, as well as
its own messages.
-}
type alias Step err msg model
  = { update : StepMsg err msg -> model -> (model, Cmd msg)
    , view : model -> Html msg
    , check : model -> Check err
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
wizard : (ViewModel model -> Html msg -> Html (WizardMsg err msg))
       -> Step err msg model
       -> List (Step err msg model)
       -> model
       -> Wizard err msg model
wizard render step steps init =
  let model =
        Model
          { model = init
          , step = step
          , left = []
          , right = steps
          }
  in { init = model, update = update, view = view render }


command : a -> Cmd a
command x =
  Task.perform identity identity (Task.succeed x)


update : WizardMsg err msg
       -> Model err msg model
       -> (Model err msg model, Cmd (ServerMsg err msg model))
update msg (Model model) =
  case msg of
    Forward ->
      case model.step.check model.model of
        Ok ->
          Maybe.mapDefault
            (Model model, command (Completed model.model))
            (\m -> (m, Cmd.none))
            (forward (Model model))
        Failed errs ->
          (Model model, command (WizardMsg (StepMsg (InvalidModel errs))))
    Back ->
      case back (Model model) of
        Nothing ->
          (Model model, command Cancelled)
        Just m ->
          (m, Cmd.none)
    StepMsg m ->
      case model.step.update m model.model of
        (model', cmd) ->
          (Model { model | model = model' }, Cmd.map (Msg >> StepMsg >> WizardMsg) cmd)


view : (ViewModel model -> Html msg -> Html (WizardMsg err msg))
     -> Model err msg model
     -> Html (WizardMsg err msg)
view render (Model m) =
  let m' = { model = m.model
           , numSteps = List.length m.left + List.length m.right + 1
           , currentStep = List.length m.left
           }
  in render m' (m.step.view m.model)


{-| The default way to render a wizard.
-}
defaultView : Bool -> ViewModel model -> Html msg -> Html (WizardMsg err msg)
defaultView showDebug m body =
  let empty = Html.text "○"
      full = Html.text "●"
      html =
        [ Html.p [ Attr.class "wizard-step"
                 , Attr.style [("width", "15em"), ("height", "5em")]
                 , border
                 , spacing 1
                 ]
            [App.map (Msg >> StepMsg) body]
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


forward : Model err msg model -> Maybe (Model err msg model)
forward (Model m) =
  List.uncons m.right |>
    Maybe.mapDefault
      Nothing
      (\(r, rs) -> Just (Model { m | step = r, left = m.step :: m.left, right = rs }))


back : Model err msg model -> Maybe (Model err msg model)
back (Model m) =
  List.uncons m.left |>
    Maybe.mapDefault
      Nothing
      (\(l, ls) -> Just (Model { m | step = l, left = ls, right = m.step :: m.right }))
