module Main exposing (..)

import Html exposing (Html, text)
import List exposing (map)
import Random exposing (Generator, bool)


type alias Rect =
    { x : Int
    , y : Int
    , w : Int
    , h : Int
    }


type Node
    = Node
        { lchild : Maybe Node
        , rchild : Maybe Node
        , value : Value
        }


type Value
    = Container Rect
    | Room Rect


someRect : Rect
someRect =
    { x = 0
    , y = 0
    , w = 800
    , h = 600
    }


root : Node
root =
    Node
        { lchild = Nothing
        , rchild = Nothing
        , value = Container someRect
        }


type Flip
    = Heads
    | Tails


coinFlip : Generator Flip
coinFlip =
    map
        (\b ->
            if b then
                Heads
            else
                Tails
        )
        bool


splitContainer : Node -> Node
splitContainer { value } =
    let
        splitRect =
            case coinFlip of
                Heads ->
                    ( Rect { x = 0, y = 0, w = value.w / 2, h = value.h }
                    , Rect { x = value.w / 2, y = 0, w = value.w / 2, h = value.h }
                    )

                Tails ->
                    ( Rect { x = 0, y = 0, w = value.w, h = value.h / 2 }
                    , Rect { x = 0, y = value.h / 2, w = value.w, h = value.h / 2 }
                    )

        ( lRect, rRect ) =
            splitRect

        lNode =
            Node
                { lchild = Nothing
                , rchild = Nothing
                , value = Container lRect
                }

        rNode =
            Node
                { lchild = Nothing
                , rchild = Nothing
                , value = Container rRect
                }
    in
        Node
            { lchild = lNode
            , rchild = rNode
            , value = value
            }


main : Html msg
main =
    text "Hello World!!!"
