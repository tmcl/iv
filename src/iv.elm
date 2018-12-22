import Browser
import Html exposing (Html, Attribute, div, input, text, span)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, on, keyCode)
import Json.Decode as Json

import Keyboard exposing (RawKey)

subscriptions: Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.downs KeyDown
        , Keyboard.ups KeyUp
        ]

-- MAIN

main : Program () Model Msg
main =
  Browser.element { init = \_ -> (init, Cmd.none), view = view, update = update, subscriptions = subscriptions }

type alias CharacterCoordinate = Int

type Mode = Insert | Normal

-- MODEL
type alias Model =
  { content: String
  , currentLocation: CharacterCoordinate
  , mode: Mode
  , newline: String
  , repeats: Maybe Int
  }

init : Model
init =
  { currentLocation = 13
  , content = "01234567890\n01234567891\n01234567892\n01234567893"
  , mode = Normal
  , newline = "\n"
  , repeats = Nothing
  }

-- UPDATE

type Msg
  = KeyDown RawKey
  | KeyUp RawKey

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    KeyDown key ->
        (respondToRawKey key model, Cmd.none)
    KeyUp key ->
        (model, Cmd.none)

respondToRawKey: RawKey -> Model -> Model
respondToRawKey key =
    case Keyboard.anyKey key of
        Just k -> respondToKey k
        _ -> Debug.todo "who knows"

ident : a -> a
ident a = a

respondToKey: Keyboard.Key -> Model -> Model
respondToKey key model =
    case (key, model.mode) of
        (Keyboard.Escape, _) -> { model | mode = Normal }
        (_, Normal) -> respondToNormalModeKey key model
        (_, Insert) -> respondToInsertModeKey key model

respondToInsertModeKey: Keyboard.Key -> Model -> Model
respondToInsertModeKey key model =
    case key of
        Keyboard.Character char -> insertString char model
        _ -> Debug.todo "one day"

charIndex: CharacterCoordinate -> String -> Int
charIndex location _ = location

-- charIndex: CharacterCoordinate -> String -> Int
-- charIndex location content =
--     case location.row of
--         0 -> location.column
--         r -> let
--                 indices = String.indices "\n" content
--                 line = pickByIndex (location.row - 1) indices
--             in case line of
--                 Nothing -> String.length content
--                 Just l -> l + location.column

insertString: String -> Model -> Model
insertString str model =
    let
        idx = charIndex model.currentLocation model.content
        pre = String.left idx model.content
        post = String.dropLeft idx model.content
        currentLocation = model.currentLocation
    in
        ensureTerminated {model | content = pre ++ str ++ post, currentLocation = currentLocation + String.length str }

ensureTerminated: Model -> Model
ensureTerminated model =  if not <| String.endsWith model.newline model.content then {model | content = model.content ++ model.newline} else model

respondToNormalModeKey: Keyboard.Key -> Model -> Model
respondToNormalModeKey key model =
    case key of
        Keyboard.Character "1" -> forcePushRepeats model 1
        Keyboard.Character "2" -> forcePushRepeats model 2
        Keyboard.Character "3" -> forcePushRepeats model 3
        Keyboard.Character "4" -> forcePushRepeats model 4
        Keyboard.Character "5" -> forcePushRepeats model 5
        Keyboard.Character "6" -> forcePushRepeats model 6
        Keyboard.Character "7" -> forcePushRepeats model 7
        Keyboard.Character "8" -> forcePushRepeats model 8
        Keyboard.Character "9" -> forcePushRepeats model 9
        Keyboard.Character "0" -> tryPushRepeats model 0
        Keyboard.Character "h" -> repAction moveLeft model |> ensureBounded
        Keyboard.Character "l" -> repAction moveRight model |> ensureBounded
        Keyboard.Character "j" -> repAction moveDown model |> ensureBounded
        Keyboard.ArrowDown  -> repAction moveDown model |> ensureBounded
        Keyboard.Character "k" -> repAction moveUp model |> ensureBounded
        Keyboard.ArrowUp -> repAction moveUp model |> ensureBounded
        Keyboard.Character "i" -> { model | mode = Insert }
        Keyboard.Character "a" -> let m = moveRight model in { m | mode = Insert }
        _ -> Debug.todo ("maybe later" ++ Debug.toString key)

popRepeats: Model -> (Int, Model)
popRepeats model = (Maybe.withDefault 1 model.repeats, {model | repeats = Nothing})

pushDigit: Int -> Int -> Int
pushDigit digit into = into * 10 + digit

tryPushDigit: Int -> Maybe Int -> Maybe Int
tryPushDigit digit = Maybe.map (pushDigit digit)

forcePushDigit: Int -> Maybe Int -> Int
forcePushDigit digit into = pushDigit digit <| Maybe.withDefault 0 into

tryPushRepeats: Model -> Int -> Model
tryPushRepeats model reps = {model | repeats = model.repeats |> tryPushDigit reps}

forcePushRepeats: Model -> Int -> Model
forcePushRepeats model reps = {model | repeats = Just <| forcePushDigit reps model.repeats}

pickByIndex: Int -> List a -> Maybe a
pickByIndex index list =
    case List.drop index list of
        [] -> Nothing
        head::_ -> Just head

pickLine: String -> Int -> Maybe String
pickLine lines num = String.lines lines |> pickByIndex num

reduceMaybe: b -> (a -> b) -> Maybe a -> b
reduceMaybe default map val = Maybe.withDefault default <| Maybe.map map val

isLessThanEqLast: List Int -> Int -> Bool
isLessThanEqLast list needle =
    let
        isLessThanEqLast_ l last =
            case l of
                [] -> needle <= last
                x::xs -> isLessThanEqLast_ xs x
    in case list of
        [] -> False
        x::xs -> isLessThanEqLast_ xs x

isGreaterThanFirst: List Int -> Int -> Bool
isGreaterThanFirst list needle =
    case list of
        [] -> False
        x::_ -> needle > x

rep: (a -> a) -> a -> Int -> a
rep f a times = if times > 0 then rep f (f a) (times - 1) else a

repAction: (Model -> Model) -> Model -> Model
repAction f model = let (reps, m) = popRepeats model in rep f m reps

moveUp: Model -> Model
moveUp model =
    let
        newlines = String.indexes model.newline model.content
        len = String.length model.content
    in
        if isGreaterThanFirst newlines model.currentLocation then
            { model | currentLocation = actuallyMoveUp newlines len model.currentLocation }
        else
            model

actuallyMoveUp: List Int -> Int -> Int -> Int
actuallyMoveUp newlines len currentLocation =
    let
        scanner =  \location curr (newLow_, oldLow_) ->
            if curr > location then (newLow_, Basics.min oldLow_ curr) else (Basics.max curr newLow_, oldLow_)
        (newLow, oldLow) = Debug.log "results 3" <| List.foldl (scanner currentLocation) (0, len) newlines
    in
        newLow + (currentLocation - oldLow)

moveDown: Model -> Model
moveDown model =
    let
        newlines = String.indexes "\n" model.content
        len = String.length model.content
    in
        if isLessThanEqLast newlines model.currentLocation then
            { model | currentLocation = actuallyMoveDown newlines len model.currentLocation }
        else
            model

actuallyMoveDown: List Int -> Int -> Int -> Int
actuallyMoveDown newlines len currentLocation =
    let
        scanner =  \location curr (oldLow_, newLow_) ->
            if curr < location then (curr, newLow_) else (oldLow_, Basics.min curr newLow_)
        (oldLow, newLow) = Debug.log "results 2" <| List.foldl (scanner currentLocation) (-1, len) newlines
    in
        newLow + (currentLocation - oldLow)

moveLeft: Model -> Model
moveLeft model =
    let
        count = 1
        range = String.slice (model.currentLocation - count) model.currentLocation model.content
        newlines = String.indexes model.newline range |> List.reverse
    in
    case newlines of
        [] -> {model | currentLocation = model.currentLocation - count }
        newline::_ -> { model | currentLocation = model.currentLocation - newline }

moveRight: Model -> Model
moveRight model =
    let
        count = 1
        range = String.slice model.currentLocation (model.currentLocation + count) model.content
        newlines = String.indexes model.newline range
    in
    case newlines of
        [] -> {model | currentLocation = model.currentLocation + count }
        newline::_ -> { model | currentLocation = model.currentLocation + newline }

ensureBounded: Model -> Model
ensureBounded model = {model | currentLocation = Basics.min model.currentLocation (String.length model.content) |> Basics.max 0 }

left: String -> Char -> String -> (String, Char, String)
left early curr late =
    case (String.toList <| String.reverse early) of
        [] -> (early, curr, late)
        f::rest -> (String.reverse <| String.fromList rest, f, String.fromChar curr ++ late)

right: String -> Char ->String -> (String, Char, String)
right early curr late =
    case String.toList late of
        [] -> (early, curr, late)
        f::rest -> (early ++ String.fromChar curr, f, String.fromList rest)

-- VIEW

makeLines : List String -> List (Html Msg)
makeLines lines = List.map makeLine lines

makeLine: String -> Html Msg
makeLine line = div [] [text line]

type alias Lines =
    { earlyLines: List String
    , earlyContent: String
    , currentContent: Char
    , lateContent: String
    , lateLines: List String
    }

unconsList: List a -> Maybe (a, List a)
unconsList list =
   case list of
       [] -> Nothing
       line::rest -> Just (line, rest)

splitLines: List String -> (String, List String)
splitLines lines = unconsList lines |> Maybe.withDefault ("", [])

splitString: String -> (Char, String)
splitString string = String.uncons string |> Maybe.withDefault ('\u{00A0}', "")

itOrDef: Maybe a -> a -> Maybe a
itOrDef it def = case it of
    Nothing -> Just def
    _ -> it

getLines: Model -> Lines
getLines model =
    let
        newlines = String.indexes "\n" model.content
        scanner =  \location curr (low, up) -> if  curr < location then (curr, up) else (low, itOrDef up curr)
        (lowCut, upCutP) = Debug.log "results" <| List.foldl (scanner model.currentLocation) (0, Nothing) newlines
        earlyStr = String.slice 0 lowCut model.content
        earlyContent = String.slice lowCut model.currentLocation model.content
        restContent =
            case upCutP of
                Nothing -> String.dropLeft model.currentLocation model.content
                Just upCut -> String.slice model.currentLocation upCut model.content
        contentLine =
            case upCutP of
                Nothing -> String.dropLeft lowCut model.content
                Just upCut -> String.slice lowCut upCut model.content
        lateStr =
            case upCutP of
                Nothing -> ""
                Just upCut -> String.dropLeft upCut model.content

        (currentContent, lateContent) = splitString <| restContent
    in
        { earlyLines = String.lines earlyStr
        , earlyContent = earlyContent
        , currentContent = if model.mode == Insert then '|' else currentContent
        , lateContent = if model.mode == Insert then restContent  else  lateContent
        , lateLines = String.lines lateStr
        }


view : Model -> Html Msg
view model =
    let
        lines = getLines model

        statusbar = [text <| (String.fromInt model.currentLocation) ++ ", " ++ (String.fromInt model.currentLocation)]

        currentLine = [div [tabindex 0]
            [ span [] [text lines.earlyContent]
            , span [style "width" "20px", style "backgroundColor" "red"] [text <| String.fromChar lines.currentContent]
            , span [] [text lines.lateContent] ] ]

        document = makeLines lines.earlyLines ++  currentLine  ++ makeLines lines.lateLines
    in
        div []
            [ div [class "content"] document
            , div [class "status"] statusbar ]

onKeyDown: (Int -> msg) -> Attribute msg
onKeyDown tagger =
    on "keydown" (Json.map tagger keyCode)
