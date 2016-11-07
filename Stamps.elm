module Stamps exposing (..)

import Color exposing (..)
import Html exposing (..)
import Element exposing (..)
import Collage exposing (..)
import Html.App as App
import Mouse
import Keyboard


type Shape
    = Pentagon
    | Circle


type alias Stamp =
    { position : ( Int, Int )
    , shape : Shape
    }


type alias Position =
    ( Int, Int )


type alias Model =
    { stamps : List Stamp
    , shift : Bool
    }


type Msg
    = AddClick Position
    | HandleShift Bool
    | NoOp


model : Model
model =
    { stamps = []
    , shift = False
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddClick pos ->
            let
                newStamp =
                    if model.shift then
                        Stamp pos Pentagon
                    else
                        Stamp pos Circle
            in
                { model | stamps = newStamp :: model.stamps } ! []

        HandleShift pressed ->
            { model | shift = pressed } ! []

        NoOp ->
            model ! []


mapKeyDown : Int -> Msg
mapKeyDown keyCode =
    case keyCode of
        16 ->
            HandleShift True

        _ ->
            NoOp


mapKeyUp : Int -> Msg
mapKeyUp keyCode =
    case keyCode of
        16 ->
            HandleShift False

        _ ->
            NoOp


drawStamp : Stamp -> Form
drawStamp stamp =
    let
        ( x, y ) =
            stamp.position

        shape =
            case stamp.shape of
                Pentagon ->
                    ngon 5 50

                Circle ->
                    circle 50
    in
        shape
            |> filled red
            |> move ( toFloat (x), toFloat (-y) )


view : Model -> Html Msg
view model =
    let
        theGroup =
            group (List.map drawStamp model.stamps)

        originGroup =
            move ( -400, 400 ) theGroup
    in
        collage 800 800 [ originGroup ]
            |> Element.toHtml


clicks : List ( Int, Int )
clicks =
    [ ( 0, 0 ), ( 100, 100 ), ( 200, 100 ) ]


main : Program Never
main =
    App.program
        { init = ( model, Cmd.none )
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Mouse.clicks (\{ x, y } -> AddClick ( x, y ))
        , Keyboard.downs mapKeyDown
        , Keyboard.ups mapKeyUp
        ]
