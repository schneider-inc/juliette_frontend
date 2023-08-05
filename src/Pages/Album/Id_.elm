module Pages.Album.Id_ exposing (Model, Msg, page)

import Gen.Params.Album.Id_ exposing (Params)
import Page
import Request
import Shared
import View exposing (View)
import Element exposing (Element, el, centerX, text, column, row, spacing, padding, width, image, height, px, link)
import Http
import Json.Decode as Decode exposing (list)
import Element.Border as Border
import Element.Input as Input
import Element.Background as Background
import Element.Font as Font
import Json.Encode as Encode


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.element
        { init = init req.params
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- INIT


type alias Model =
    { albumName : String
    , albumId : String
    , albumCoverArt : String
    , albumYear : String
    , albumArtists : List String
    , albumLiked : Bool
    , albumPrice : String
    , songs : List Shared.Song
    }

initialModel : Params -> Model
initialModel params =
    { albumName = ""
    , albumId = params.id
    , albumCoverArt = ""
    , albumYear = ""
    , albumArtists = []
    , albumLiked = False
    , albumPrice = ""
    , songs = []
    }

init : Params -> ( Model, Cmd Msg )
init params =
    ( initialModel params
    , Http.get(
               { url = "https://juliette-backend.onrender.com/albums/" ++ (initialModel params).albumId
               , expect = Http.expectJson GotAlbum Shared.albumDecoder
               } 
              )
    )



-- UPDATE


type Msg
    = GotAlbum (Result Http.Error Shared.Album)
    | ToggleLike Shared.Song
    | ToggleAlbumLike Bool
    | ToggleListenedTo Shared.Song

songEncoder : Shared.Song -> Encode.Value
songEncoder songObject =
    Encode.object
        [ ("name", Encode.string songObject.name)
        , ("listenedTo", Encode.bool songObject.listenedTo)
        , ("liked", Encode.bool songObject.liked)
        ]

changedSongsLiked : Model -> Shared.Song -> List Shared.Song
changedSongsLiked model songObject =
    List.map (\song -> if song == songObject then { song | liked = not song.liked } else song) model.songs

changedSongsListenedTo : Model -> Shared.Song -> List Shared.Song
changedSongsListenedTo model songObject =
    List.map (\song -> if song == songObject then { song | listenedTo = not song.listenedTo } else song) model.songs

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotAlbum (Ok album) ->
            ( Model album.name album.id album.coverArtUrl (String.fromInt album.year) album.artists album.liked (String.fromFloat album.price) album.songs, Cmd.none )
        
        GotAlbum (Err _) ->
            ( model, Cmd.none )

        ToggleLike songObject ->
            ( model
            , Http.request(
                { method = "PATCH"
                , headers = []
                , url = "https://juliette-backend.onrender.com/album/" ++ model.albumId
                , body = Http.jsonBody (Encode.object [("songs", Encode.list songEncoder (changedSongsLiked model songObject))])
                , expect = Http.expectJson GotAlbum Shared.albumDecoder
                , tracker = Nothing
                , timeout = Nothing
                }
              ) 
            )

        ToggleListenedTo songObject ->
            ( model
            , Http.request(
                { method = "PATCH"
                , headers = []
                , url = "https://juliette-backend.onrender.com/album/" ++ model.albumId
                , body = Http.jsonBody (Encode.object [("songs", Encode.list songEncoder (changedSongsListenedTo model songObject))])
                , expect = Http.expectJson GotAlbum Shared.albumDecoder
                , tracker = Nothing
                , timeout = Nothing
                }
              )
            )

        ToggleAlbumLike liked ->
            ( { model | albumLiked = liked }
            , Http.request(
                { method = "PATCH"
                , headers = []
                , url = "https://juliette-backend.onrender.com/album/" ++ model.albumId
                , body = Http.jsonBody (Encode.object [("liked", Encode.bool liked)])
                , expect = Http.expectJson GotAlbum Shared.albumDecoder
                , tracker = Nothing
                , timeout = Nothing
                }
              )
            )




-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = model.albumName
    , element = 
        column []
            [ basicAlbum model
            , songs model.songs
            ]
    }

albumStyles : List (Element.Attribute Msg)
albumStyles =
    [ spacing 10, padding 10, Border.color Shared.accentColor, Border.width 1, Border.rounded 10 ]

basicAlbum : Model -> Element Msg
basicAlbum model = 
    row
        albumStyles
        [ Input.button [ Background.image (if model.albumLiked then "/full_heart.jpeg" else "/empty_heart.png"), height (px 35), width (px 35) ]
                       { onPress = Just (ToggleAlbumLike (not model.albumLiked))
                       , label = text ""
                       }        
        , image [ height (px 50), width (px 50) ] 
                { src = model.albumCoverArt
                , description = ("cover art for " ++ model.albumName)
                }
        , column
            []
            [ el [] (text (model.albumName ++ " (" ++ (model.albumYear) ++ ")"))
            , el [Font.size 12] (text (String.join ", " model.albumArtists))
            ]
        ]

songs : List Shared.Song -> Element Msg
songs songsList = 
    column
        [ spacing 10, padding 10, Border.color Shared.accentColor, Border.width 1, Border.rounded 10 ]
        <| link ( padding 5  :: Shared.buttonStyles) { url = "/albums", label = text "Back to her collection!"} :: List.map songEl songsList

songEl : Shared.Song -> Element Msg
songEl songObject =
    row
        [ spacing 5 ]
        [ Input.button [ Background.image (if songObject.liked then "/full_heart.jpeg" else "/empty_heart.png"), height (px 35), width (px 35) ]
                       { onPress = Just (ToggleLike songObject)
                       , label = text ""
                       }   
        , el [] (text songObject.name)
        , Input.checkbox
            [ centerX ]
            { onChange = \_ -> ToggleListenedTo songObject
            , icon = Input.defaultCheckbox
            , checked = songObject.listenedTo
            , label = Input.labelAbove [centerX] (text "listened to?")
            }
        ]