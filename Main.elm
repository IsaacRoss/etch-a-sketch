module Main exposing (..)

import Color exposing (..)
import Collage exposing (..)
import Element exposing (..)
import Html exposing (..)
import Html.Events exposing (onClick)
import Html.App as App
import Keyboard.Extra
import Time exposing (Time, second)
import AnimationFrame
import Animation exposing (..)


type alias Model =
    { points : List Point
    , x : Int
    , y : Int
    , keyboardModel : Keyboard.Extra.Model
    , clock : Time
    , animation : Animation
    }


type alias Point =
    ( Int, Int )


type Msg
    = KeyboardExtraMsg Keyboard.Extra.Msg
    | Tick Time
    | Shake


shakeAnimation : Time -> Animation
shakeAnimation t =
    animation t
        |> from 0
        |> to 360
        |> duration (500 * Time.millisecond)


init : ( Model, Cmd Msg )
init =
    let
        ( keyboardModel, keyboardCmd ) =
            Keyboard.Extra.init
    in
        ( { points = [ ( 0, 0 ) ]
          , x = 0
          , y = 0
          , keyboardModel = keyboardModel
          , clock =
                0
          , animation = static 0
          }
        , Cmd.batch
            [ Cmd.map KeyboardExtraMsg keyboardCmd
            ]
        )


shakeButton : Html Msg
shakeButton =
    Html.button [ onClick Shake ] [ Html.text "Shake it good" ]


view : Model -> Html Msg
view model =
    let
        angle =
            animate model.clock model.animation
    in
        div []
            [ collage 800
                800
                [ (rotate (degrees angle) (drawLine model.points)) ]
                |> Element.toHtml
            , shakeButton
            ]


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

        Tick dt ->
            let
                { x, y } =
                    Keyboard.Extra.arrows model.keyboardModel

                newX =
                    model.x + x

                newY =
                    model.y + y

                newClock =
                    model.clock + dt

                ( newPoints, newAnimation ) =
                    case (model.animation `equals` (static 0)) of
                        True ->
                            ( model.points, model.animation )

                        False ->
                            case (isDone model.clock model.animation) of
                                True ->
                                    ( [], (static 0) )

                                False ->
                                    ( model.points, model.animation )

                newPoints' =
                    case ( x, y ) of
                        ( 0, 0 ) ->
                            newPoints

                        _ ->
                            ( newX, newY ) :: newPoints

                model' =
                    { model
                        | points = newPoints'
                        , clock = newClock
                        , animation = newAnimation
                    }
            in
                case ( x, y ) of
                    ( 0, 0 ) ->
                        model' ! []

                    _ ->
                        { model'
                            | x = newX
                            , y = newY
                        }
                            ! []

        Shake ->
            { model
                | animation = shakeAnimation model.clock
            }
                ! []


main : Program Never
main =
    App.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map KeyboardExtraMsg Keyboard.Extra.subscriptions
        , AnimationFrame.diffs Tick
        ]
