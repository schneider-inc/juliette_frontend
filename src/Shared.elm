module Shared exposing
    ( Model
    , Msg
    , init
    , subscriptions
    , update
    , initialModel
    , Album
    , Song
    , Status(..)
    , Flags
    , buttonStyles
    , albumDecoder
    , songDecoder
    )

import Json.Decode as Json
import Request exposing (Request)
import Element exposing (Element, rgb255, padding)
import Element.Background as Background
import Element.Border as Border
import Json.Decode exposing (Decoder, succeed, bool, string, float, int, list)
import Json.Decode.Pipeline exposing (required, optional)

type Status
    = Loading
    | Loaded (List Album)
    | Errored String

type alias Flags =
    Json.Value


type alias Album =
    { id : String 
    , name : String
    , artists : List String
    , year : Int
    , coverArtUrl : String
    , spotifyId : String
    , price : Float
    , borrowed : Bool
    , lentOut : Bool
    , borrowedFrom : String
    , lentOutTo : String
    , liked : Bool
    , borrowedReturnDate : String
    , lentOutReturnDate : String
    , songs : List Song
    }

type alias Song =
    { name : String
    , listenedTo : Bool
    , liked : Bool
    }

type alias Model =
    { albums : List Album
    , status : Status
    , selectedFilter : String
    }

initialModel : Model
initialModel = 
    { albums = []
    , status = Loading
    , selectedFilter = "All"
    }

buttonStyles : List (Element.Attribute msg)
buttonStyles =
    [ Background.color (rgb255 50 200 255 )
    , Border.rounded 10
    , padding 10 
    ]

songDecoder : Decoder Song
songDecoder =
    succeed Song
        |> required "name" string
        |> optional "listenedTo" bool False
        |> optional "liked" bool False

albumDecoder : Decoder Album
albumDecoder =
    succeed Album
        |> optional "_id" string ""
        |> required "name" string
        |> required "artists" (list string)
        |> required "year" int
        |> required "coverArtUrl" string
        |> required "spotifyId" string
        |> optional "price" float 0.0 
        |> optional "borrowed" bool False
        |> optional "lentOut" bool False
        |> optional "borrowedFrom" string ""
        |> optional "lentOutTo" string ""
        |> optional "liked" bool False
        |> optional "borrowedReturnDate" string ""
        |> optional "lentOutReturnDate" string ""
        |> optional "songs" (list songDecoder) []

type Msg
    = NoOp


init : Request -> Flags -> ( Model, Cmd Msg )
init _ _ =
    ( initialModel, Cmd.none )


update : Request -> Msg -> Model -> ( Model, Cmd Msg )
update _ msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


subscriptions : Request -> Model -> Sub Msg
subscriptions _ _ =
    Sub.none
