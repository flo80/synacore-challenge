module Main exposing (main)

import Array
import Browser
import Bytes exposing (Bytes, Endianness(..))
import Bytes.Decode as BD
import Element exposing (..)
import Element.Background as BG
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input exposing (button)
import Element.Region
import File exposing (File)
import File.Select as Select
import Html exposing (Html)
import Machine exposing (..)
import String exposing (fromInt)
import Task
import Time



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { image : Maybe (List Int)
    , machine : Maybe Machine
    , autoStep : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Nothing Nothing False, Cmd.none )



-- UPDATE


type Msg
    = ImageRequested
    | ImageSelected File
    | ImageDecoded (List Int)
    | ResetMachine
    | StepMachine
    | RunMachine
    | Tick Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ImageRequested ->
            ( model
            , Select.file [ "data" ] ImageSelected
            )

        ImageSelected file ->
            ( model
            , Task.perform ImageDecoded (Task.map decodeFile <| File.toBytes file)
            )

        ImageDecoded content ->
            let
                machine =
                    newMachine content
            in
            ( { model | image = Just content, machine = Just machine }, Cmd.none )

        ResetMachine ->
            case model.image of
                Nothing ->
                    ( model, Cmd.none )

                Just mi ->
                    ( { model | machine = Just (newMachine mi) }, Cmd.none )

        StepMachine ->
            case model.machine of
                Nothing ->
                    ( model, Cmd.none )

                Just m ->
                    case m.state of
                        Running ->
                            ( { model | machine = Just (step m) }, Cmd.none )

                        _ ->
                            ( model, Cmd.none )

        RunMachine ->
            ( { model | autoStep = not model.autoStep }, Cmd.none )

        Tick _ ->
            if model.autoStep then
                update StepMachine model

            else
                ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    let
        loadButton =
            case model.image of
                Nothing ->
                    button [] { label = text "Load Image", onPress = Just ImageRequested }

                Just content ->
                    Element.none

        stepButton =
            case model.image of
                Nothing ->
                    Element.none

                Just _ ->
                    row [ spaceEvenly, width fill ]
                        [ button [] { label = text "Step VM", onPress = Just StepMachine }
                        , button [] { label = text "Run VM", onPress = Just RunMachine }
                        , button [] { label = text "Reset VM", onPress = Just ResetMachine }
                        ]

        machineStatus =
            case model.machine of
                Nothing ->
                    Element.none

                Just machine ->
                    viewMachineDebug machine

        moutput =
            case model.machine of
                Nothing ->
                    [ text "No machine" ]

                Just m ->
                    List.map (\x -> paragraph [] [ text x ]) (String.lines <| getOutput m)
    in
    layout [ Font.size 12 ] <|
        row [ height fill, width fill, alignTop, padding 5, spacing 5 ]
            [ column [ width <| fillPortion 60, height fill, alignTop ]
                [ row
                    [ width fill
                    , height fill
                    , Border.width 1
                    , alignTop
                    , Font.color white
                    , BG.color black
                    , Font.family [ Font.monospace ]
                    , padding 3
                    ]
                    [ paragraph [ alignTop ] moutput
                    ]
                ]
            , column [ width <| fillPortion 40, alignTop, spacing 5 ]
                [ text "DEBUG"
                , row [ spaceEvenly, width fill ] [ loadButton, stepButton ]
                , machineStatus
                ]
            ]


viewMachineDebug : Machine -> Element msg
viewMachineDebug machine =
    let
        pp t =
            paragraph [] [ text t ]

        state =
            case machine.state of
                Running ->
                    pp "Running"

                Halted ->
                    pp "Halted"

                Crashed x ->
                    paragraph [ Font.color red ] [ text ("Crashed: " ++ x) ]

                WaitingForInput ->
                    pp "Waiting for Input"

        ( currentInstruction, _ ) =
            fetchInstruction machine

        ( disasInstruction, _ ) =
            disassemble machine.memory ipVal

        (Pointer ipVal) =
            machine.ip

        disassembledProgram =
            List.map
                (\( i, str ) ->
                    paragraph [ width fill ]
                        [ el [ Font.size 8, width <| px 20 ] (text <| fromInt i ++ " ")
                        , text str
                        ]
                )
                (disasAll machine.memory ipVal 40)

        registers =
            column [ width <| px 100, alignTop ]
                (paragraph [ Font.bold ] [ text "Registers" ]
                    :: List.map
                        (\addr ->
                            let
                                (Value v) =
                                    Maybe.withDefault (Value 0) <| Array.get addr machine.registers
                            in
                            paragraph []
                                [ el [ width <| px 50 ] (text (fromInt addr ++ ": "))
                                , text <| fromInt v
                                ]
                        )
                        (List.range 0 7)
                )

        stack =
            column [ width <| px 100, alignTop ]
                (paragraph [ Font.bold ] [ text "Stack" ]
                    :: List.map (\(Value val) -> paragraph [] [ text <| fromInt val ]) machine.stack
                )
    in
    column [ spacing 10, Font.family [ Font.monospace ], width fill ]
        [ state
        , row [ spacing 5, width <| px 150 ]
            [ el [ width <| px 100 ] (text ("IP: " ++ fromInt ipVal))
            , el [ width <| px 150 ] (text <| showInstruction currentInstruction)
            , el [ width <| px 150 ] (text disasInstruction)
            ]
        , row [ spacing 5, alignTop ]
            [ registers
            , stack
            ]
        , column [ alignTop, width fill ]
            (paragraph [ Font.bold ] [ text "Disassembly" ]
                :: disassembledProgram
            )
        ]


viewImage : List Int -> Element msg
viewImage image =
    let
        str =
            String.concat <| List.map (\x -> fromInt x ++ " ") image
    in
    paragraph [] [ text str ]


white =
    rgb255 255 255 255


black =
    rgb255 0 0 0


red =
    rgb255 255 0 0



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 1 Tick



-- FILE DECODING


decodeFile : Bytes -> List Int
decodeFile bs =
    let
        size =
            Bytes.width bs // 2
    in
    List.reverse <|
        Maybe.withDefault [] <|
            BD.decode
                (BD.loop ( size, [] ) decodeFileByte)
                bs


decodeFileByte : ( Int, List Int ) -> BD.Decoder (BD.Step ( Int, List Int ) (List Int))
decodeFileByte ( n, xs ) =
    if n == 0 then
        BD.succeed (BD.Done xs)

    else
        BD.map (\x -> BD.Loop ( n - 1, x :: xs )) (BD.unsignedInt16 LE)


disasAll : Array.Array Int -> Int -> Int -> List ( Int, String )
disasAll memory pointer steps =
    if pointer >= Array.length memory || steps == 0 then
        []

    else
        let
            ( str, newP ) =
                disassemble memory pointer
        in
        ( pointer, str ) :: disasAll memory newP (steps - 1)
