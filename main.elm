import Html exposing (Html, div, fromElement, select, option, text, label, input, Attribute)
import Html.Events exposing (on, targetValue)
import Html.Attributes exposing (style, attribute, value, placeholder, type')
import Json.Decode as Json
import StartApp.Simple as StartApp
import Graphics.Element exposing (show)
import Maybe exposing (..)
import String

type Gender = Male | Female
type Units = Kg | Lbs

toGender : String -> Gender
toGender gender =
  case gender of
    "Male" -> Male
    "Female" -> Female
    _ -> Female

toUnits : String -> Units
toUnits unit =
  case unit of
    "kg" -> Kg
    "lbs" -> Lbs
    _ -> Lbs

type alias Model = {
    gender : Gender,
    bodyweightUnits: Units,
    bodyweight: Maybe Float,
    weightUnits: Units,
    weight: Maybe Float
  }

initModel = { gender = Male,
              bodyweightUnits = Lbs,
              bodyweight = Nothing,
              weightUnits = Lbs,
              weight = Nothing }

-- Update
type Action =
  UpdateGender Gender |
  UpdateBodyweightUnits Units |
  UpdateBodyweight (Maybe Float) |
  UpdateWeightUnits Units |
  UpdateWeight (Maybe Float) |
  NoOp


update : Action -> Model -> Model
update action model =
  case action of
    UpdateGender gender -> { model | gender = gender }
    UpdateBodyweightUnits unit -> { model | bodyweightUnits = unit }
    UpdateBodyweight weight -> { model | bodyweight = weight }
    UpdateWeightUnits unit -> { model | weightUnits = unit }
    UpdateWeight weight -> { model | weight = weight }
    NoOp -> model

-- View
view : Signal.Address Action -> Model -> Html
view address model =
  div []
        [ selectDiv address onChangeGender "Gender" ["Male", "Female"]
        , selectDiv address (\v -> onChangeUnit v (\units -> UpdateBodyweightUnits <| toUnits units)) "Bodyweight Units" ["lbs", "kg"]
        , selectDiv address (\v -> onChangeUnit v (\units -> UpdateWeightUnits <| toUnits units)) "Weight Units" ["lbs", "kg"]
        , field "text" address UpdateBodyweight "Body Weight" <| withDefault "" <| map toString model.bodyweight
        , field "text" address UpdateWeight "Weight" <| withDefault "" <| map toString model.weight
        , fromElement <| show model
        ]


removeSpace : String -> String
removeSpace = String.filter (\char -> char /= ' ')

selectDiv : Signal.Address Action -> (Signal.Address Action -> Attribute) -> String -> List String -> Html
selectDiv address onChange name options =
  div []
        [ div [fieldNameStyle "160px"] [label [ attribute "for" <| removeSpace name ] [ text name ] ]
        , select [ attribute "name" <| removeSpace name
                 ,  onChange address ] (List.map (\opt -> option [] [text opt]) options)
        ]


onChangeGender : Signal.Address Action -> Attribute
onChangeGender address =
  on "change" targetValue (\val -> Signal.message address <| UpdateGender <| toGender val)

onChangeUnit : Signal.Address Action -> (String -> Action) -> Attribute
onChangeUnit address toAction =
  on "change" targetValue (\val -> Signal.message address <| toAction val)



field : String -> Signal.Address Action -> (Maybe Float -> Action) -> String -> String -> Html
field fieldType address toAction name content =
  div []
        [ div [ fieldNameStyle "160px" ] [ label [ attribute "for" <| removeSpace name ] [text name] ]
        , input
            [ type' fieldType
            , placeholder name
            , value content
            , attribute "name" <| removeSpace name
            , on "input" targetValue <| stringToAction address toAction
            ]
            []
        ]

stringToAction : Signal.Address Action -> (Maybe Float -> Action) -> String -> Signal.Message
stringToAction address toAction string = Signal.message address (toAction (case (String.toFloat string) of
                                                           Ok r -> Just r
                                                           Err _ -> Nothing))

fieldNameStyle : String -> Attribute
fieldNameStyle px =
  style
    [ ("width", px)
    , ("padding", "10px")
    , ("text-align", "right")
    , ("display", "inline-block")
    ]

main =
  StartApp.start { model = initModel, update = update, view = view }
