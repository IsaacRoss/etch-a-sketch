module Main exposing (..)

import Color exposing (..)
import Collage exposing (..)
import Element exposing (..)
import Keyboard
import Html exposing (..)
import Html.App as App
import Keyboard.Extra
import Time exposing (Time, second)


initialModel : ( Model, Cmd Msg )
initialModel =
    let
        ( keyboardModel, keyboardCmd ) =
            Keyboard.Extra.init
    in
        ( { points = [ ( 0, 0 ) ]
          , x = 0
          , y = 0
          , keyboardModel = keyboardModel
          }
        , Cmd.map KeyboardExtraMsg keyboardCmd
        )


type alias Model =
    { points : List Point
    , x : Int
    , y : Int
    , keyboardModel : Keyboard.Extra.Model
    }


type alias Point =
    ( Int, Int )


view : Model -> Html Msg
view model =
    collage 800
        800
        [ (drawLine model.points) ]
        |> Element.toHtml


drawLine : List Point -> Form
drawLine points =
    let
        intsToFloats : ( Int, Int ) -> ( Float, Float )
        intsToFloats ( x, y ) =
            ( toFloat x, toFloat y )

        shape =
            path (List.map intsToFloats points)
    in
        shape
            |> traced (solid red)


main : Program Never
main =
    App.program
        { init = initialModel
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type Msg
    = KeyboardExtraMsg Keyboard.Extra.Msg
    | Tick Time


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map KeyboardExtraMsg Keyboard.Extra.subscriptions
        , Time.every (1 / 30 * second) Tick
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyboardExtraMsg keyMsg ->
            let
                ( keyboardModel, keyboardCmd ) =
                    Keyboard.Extra.update keyMsg model.keyboardModel
            in
                ( { model | keyboardModel = keyboardModel }
                , Cmd.map KeyboardExtraMsg keyboardCmd
                )

        Tick _ ->
            let
                { x, y } =
                    Keyboard.Extra.arrows model.keyboardModel

                newX =
                    model.x + x

                newY =
                    model.y + y
            in
                case ( x, y ) of
                    ( 0, 0 ) ->
                        model ! []

                    _ ->
                        { model
                            | points = ( newX, newY ) :: model.points
                            , x = newX
                            , y = newY
                        }
                            ! []


keyUp : Keyboard.KeyCode -> Model -> Model
keyUp keyCode model =
    case keyCode of
        38 ->
            --up
            { model | y = model.y + 1, points = ( model.x, model.y + 1 ) :: model.points }

        40 ->
            -- down
            { model | y = model.y - 1, points = ( model.x, model.y - 1 ) :: model.points }

        37 ->
            -- left
            { model | x = model.x - 1, points = ( model.x - 1, model.y ) :: model.points }

        39 ->
            -- right
            { model | y = model.x + 1, points = ( model.x + 1, model.y ) :: model.points }

        _ ->
            model
