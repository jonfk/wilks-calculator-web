import Html exposing (Html, div, select, option, text, label, input, Attribute)
import Html.Events exposing (on, targetValue)
import Html.Attributes exposing (style, attribute, value, placeholder, type_)
import Maybe exposing (..)
import String
import Json.Decode as Json
import Wilks

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

initModel : Model
initModel = { gender = Male,
                      bodyweightUnits = Lbs,
                      partialBodyweight = "",
                      bodyweight = Nothing,
                      weightUnits = Lbs,
                      partialWeight = "",
                      weight = Nothing }

-- Update
type Msg =
  UpdateGender Gender |
  UpdateBodyweightUnits Units |
  UpdateBodyweight (Maybe Float) String |
  UpdateWeightUnits Units |
  UpdateWeight (Maybe Float) String |
  NoOp


update : Msg -> Model -> Model
update msg model =
  case msg of
    UpdateGender gender -> { model | gender = gender }
    UpdateBodyweightUnits unit -> { model | bodyweightUnits = unit }
    UpdateBodyweight weight partial -> { model | bodyweight = weight, partialBodyweight = partial }
    UpdateWeightUnits unit -> { model | weightUnits = unit }
    UpdateWeight weight partial -> { model | weight = weight, partialWeight = partial }
    NoOp -> model

-- View
view : Model -> Html Msg
view model =
  let
    toKg = (\x -> x * 0.453592)
    bodyweight = if model.bodyweightUnits == Kg then model.bodyweight else map toKg model.bodyweight
    weight = if model.weightUnits == Kg then model.weight else map toKg model.weight
    maybeWilks = Maybe.map2 (wilksScoreKg model.gender) bodyweight weight
    textWilks = Maybe.withDefault "" <| map toString maybeWilks
  in
    div []
          [ selectDiv onChangeGender "Gender" ["Male", "Female"]
          , selectDiv (onChangeUnit (\units -> UpdateBodyweightUnits <| toUnits units)) "Bodyweight Units" ["lbs", "kg"]
          , selectDiv (onChangeUnit (\units -> UpdateWeightUnits <| toUnits units)) "Weight Units" ["lbs", "kg"]
          , field "text" UpdateBodyweight "Body Weight" model.partialBodyweight
          , field "text" UpdateWeight "Weight" model.partialWeight
          , div [] [ div [ fieldNameStyle "160px"] [ text "Wilks Score" ]
                   , text textWilks]
          ]


removeSpace : String -> String
removeSpace = String.filter (\char -> char /= ' ')

selectDiv : Attribute Msg -> String -> List String -> Html Msg
selectDiv onChange name options =
  div []
        [ div [fieldNameStyle "160px"] [label [ attribute "for" <| removeSpace name ] [ text name ] ]
        , select [ attribute "name" <| removeSpace name
                 ,  onChange ] (List.map (\opt -> option [] [text opt]) options)
        ]


onChangeGender : Attribute Msg
onChangeGender =
    let
        toMsg = \x -> UpdateGender <| toGender x
    in
        on "change" (Json.map (toMsg) targetValue)

onChangeUnit : (String -> Msg) -> Attribute Msg
onChangeUnit toMsg =
  on "change" (Json.map toMsg targetValue)



field : String -> (Maybe Float -> String -> Msg) -> String -> String -> Html Msg
field fieldType toMsg name content =
  div []
        [ div [ fieldNameStyle "160px" ] [ label [ attribute "for" <| removeSpace name ] [text name] ]
        , input
            [ type_ fieldType
            , placeholder name
            , value content
            , attribute "name" <| removeSpace name
            , on "input" (Json.map (stringToMsg toMsg) targetValue)
            ]
            []
        ]

stringToMsg : (Maybe Float -> String -> Msg) -> String -> Msg
stringToMsg toMsg string =
  case (String.toFloat string) of
    Ok r ->
      if String.endsWith "." string then
          toMsg (Just r) string
      else
          toMsg (Just r) (toString r)
    Err _ -> toMsg Nothing string

fieldNameStyle : String -> Attribute Msg
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
    Male -> Wilks.maleWilksCoeffKg bodyweight
    Female -> Wilks.femaleWilksCoeffKg bodyweight

main : Program Never Model Msg
main =
  Html.beginnerProgram { model = initModel, update = update, view = view }
