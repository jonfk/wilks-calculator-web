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
    partialBodyweight: String,
    bodyweightUnits: Units,
    bodyweight: Maybe Float,
    weightUnits: Units,
    partialWeight: String,
    weight: Maybe Float
  }

initModel = { gender = Male,
              bodyweightUnits = Lbs,
              partialBodyweight = "",
              bodyweight = Nothing,
              weightUnits = Lbs,
              partialWeight = "",
              weight = Nothing }

-- Update
type Action =
  UpdateGender Gender |
  UpdateBodyweightUnits Units |
  UpdateBodyweight (Maybe Float) String |
  UpdateWeightUnits Units |
  UpdateWeight (Maybe Float) String |
  NoOp


update : Action -> Model -> Model
update action model =
  case action of
    UpdateGender gender -> { model | gender = gender }
    UpdateBodyweightUnits unit -> { model | bodyweightUnits = unit }
    UpdateBodyweight weight partial -> { model | bodyweight = weight, partialBodyweight = partial }
    UpdateWeightUnits unit -> { model | weightUnits = unit }
    UpdateWeight weight partial -> { model | weight = weight, partialWeight = partial }
    NoOp -> model

-- View
view : Signal.Address Action -> Model -> Html
view address model =
  let
    toKg = (\x -> x * 0.453592)
    bodyweight = if model.bodyweightUnits == Kg then model.bodyweight else map toKg model.bodyweight
    weight = if model.weightUnits == Kg then model.weight else map toKg model.weight
    maybeWilks = Maybe.map2 (wilksScoreKg model.gender) bodyweight weight
    textWilks = Maybe.withDefault "" <| map toString maybeWilks
  in
    div []
          [ selectDiv address onChangeGender "Gender" ["Male", "Female"]
          , selectDiv address (\v -> onChangeUnit v (\units -> UpdateBodyweightUnits <| toUnits units)) "Bodyweight Units" ["lbs", "kg"]
          , selectDiv address (\v -> onChangeUnit v (\units -> UpdateWeightUnits <| toUnits units)) "Weight Units" ["lbs", "kg"]
          , field "text" address UpdateBodyweight "Body Weight" model.partialBodyweight
          , field "text" address UpdateWeight "Weight" model.partialWeight
          , div [] [ div [ fieldNameStyle "160px"] [ text "Wilks Score" ]
                   , text textWilks]
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



field : String -> Signal.Address Action -> (Maybe Float -> String -> Action) -> String -> String -> Html
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

stringToAction : Signal.Address Action -> (Maybe Float -> String -> Action) -> String -> Signal.Message
stringToAction address toAction string =
  case (String.toFloat string) of
    Ok r ->
      if String.endsWith "." string then
        Signal.message address <| toAction (Just r) string
      else
        Signal.message address <| toAction (Just r) (toString r)
    Err _ -> Signal.message address <| toAction Nothing string

fieldNameStyle : String -> Attribute
fieldNameStyle px =
  style
    [ ("width", px)
    , ("padding", "10px")
    , ("text-align", "right")
    , ("display", "inline-block")
    ]

-- All Wilks formulas are calculated with Kg weights
wilksScoreKg : Gender -> Float -> Float -> Float
wilksScoreKg gender bodyweight weight =
  (wilksCoeffKg gender bodyweight) * weight

wilksCoeffKg : Gender -> Float -> Float
wilksCoeffKg gender bodyweight =
  case gender of
    Male -> maleWilksCoeffKg bodyweight
    Female -> femaleWilksCoeffKg bodyweight

maleWilksCoeffKg : Float -> Float
maleWilksCoeffKg bodyweight =
  let
    a = -216.0475144
    b = 16.2606339
    c = -0.002388645
    d = -0.00113732
    e = 0.00000701863
    f = -0.00000001291
  in
    wilksCoeffWithCoeffs a b c d e f bodyweight

femaleWilksCoeffKg : Float -> Float
femaleWilksCoeffKg bodyweight =
  let
    a = 594.31747775582
    b = -27.23842536447
    c = 0.82112226871
    d = -0.00930733913
    e = 0.00004731582
    f = -0.00000009054
  in
    wilksCoeffWithCoeffs a b c d e f bodyweight

wilksCoeffWithCoeffs : Float -> Float -> Float -> Float -> Float -> Float -> Float -> Float
wilksCoeffWithCoeffs a b c d e f bodyweight =
  let
    x = bodyweight
  in
    500 / (a + (b * x) + (c * (x^2)) + (d * (x^3)) + (e * (x^4)) + (f * (x^5)))

main =
  StartApp.start { model = initModel, update = update, view = view }
