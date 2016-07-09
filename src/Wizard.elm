module Wizard exposing (Msg, Model, Wizard, Step, ViewModel, wizard, defaultView)

{-| A wizard module.

@docs Msg, Model

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


{-| A wizard message.
-}
type Msg msg
  = Forward
  | Back
  | Msg msg


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
    , scratch : model
    , step : Step msg model
    , left : List (Step msg model)
    , right : List (Step msg model)
    }


{-| A wizard.
-}
type alias Wizard msg model
  = { init : Model msg model
    , update : Msg msg -> Model msg model -> (Model msg model, Cmd (Msg msg))
    , view : Model msg model -> Html (Msg msg)
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
wizard : (ViewModel model -> Html msg -> Html (Msg msg))
       -> Step msg model
       -> List (Step msg model)
       -> model
       -> Wizard msg model
wizard render step steps init =
  let model =
        Model
          { model = init
          , scratch = init
          , step = step
          , left = []
          , right = steps
          }
  in
  { init = model, update = update, view = view render }


update : Msg msg
       -> Model msg model
       -> (Model msg model, Cmd (Msg msg))
update msg (Model model) =
  case msg of
    Forward ->
      case forward (Model model) of
        Nothing ->
          (start (Model { model | model = model.scratch }), Cmd.none)
        Just model' ->
          (model', Cmd.none)
    Back ->
      case back (Model model) of
        Nothing ->
          (Model { model | scratch = model.model }, Cmd.none)
        Just model' ->
          (model', Cmd.none)
    Msg m ->
      case model.step.update m model.scratch of
        (model', cmd) ->
          (Model { model | scratch = model' }, Cmd.map Msg cmd)


view : (ViewModel model -> Html msg -> Html (Msg msg))
     -> Model msg model
     -> Html (Msg msg)
view render (Model m) =
  let m' = { model = m.scratch
           , numSteps = List.length m.left + List.length m.right + 1
           , currentStep = List.length m.left
           }
  in render m' (m.step.view m.scratch)


{-| The default way to render a wizard.
-}
defaultView : Bool -> ViewModel model -> Html msg -> Html (Msg msg)
defaultView showDebug m body =
  let html =
        [ Html.p [ Attr.class "wizard-step"
                 , Attr.style [("width", "8em"), ("height", "5em")]
                 , border
                 , spacing 1
                 ]
            [App.map Msg body]
        , Html.span [ spacing 1 ]
            [ Html.span [spacing 0.2]
                (List.repeat m.currentStep (Html.text "-") ++
                   [Html.text "O"] ++
                   List.repeat (m.numSteps - m.currentStep - 1) (Html.text "-"))
            , Html.button [spacing 0.2, onClick Back] [Html.text "<<"]
            , Html.button [ spacing 0.2
                          , onClick Forward
                          ]
                [Html.text ">>"]
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
            [ Html.text ("MODEL: " ++ toString m.model) ]
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
-- UTILITIES
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


start : Model msg model -> Model msg model
start model =
  Maybe.mapDefault model start (back model)


end : Model msg model -> Model msg model
end model =
  Maybe.mapDefault model end (forward model)
