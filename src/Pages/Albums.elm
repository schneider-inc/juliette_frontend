module Pages.Albums exposing (Model, Msg, page)

import Gen.Params.Albums exposing (Params)
import Page
import Request
import Shared
import View exposing (View)
import Element exposing (Element, link, alignRight, alignLeft, scrollbarY, px, image, row, centerX, column, el, fill, height, padding, spacing, text, width)
import Element.Font as Font
import Element.Background as Background
import Element.Border as Border
import Element.Input as Input
import Element.Font as Font
import Http
import Json.Decode as Decode exposing (Decoder, list)
import Json.Encode as Encode
import Html.Events

page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- INIT



type alias Model =
    { searchBar : String
    , selectedFilter : String
    , suggestedAlbums : List Shared.Album
    , albums : List Shared.Album
    , error : String
    }

initialModel : Model
initialModel =
    { searchBar = ""
    , selectedFilter = "All"
    , suggestedAlbums = []
    , albums = []
    , error = ""
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel
    , Http.get(
        { url = "https://juliette-backend.onrender.com/albums"
        , expect = Http.expectJson GotAlbums (list Shared.albumDecoder)
        }
      )
    )



-- UPDATE


type Msg
    = ClickedSidebar String
    | ChangedSearch String
    | GotSuggestedAlbums (Result Http.Error (List Shared.Album))
    | EnteredSearch
    | ChoseSuggestedAlbum Shared.Album
    | GotAlbums (Result Http.Error (List Shared.Album))
    | ToggleLike {id : String, liked : Bool }
    | ClickedDelete String
    | ChoseWishlistAlbum Shared.Album

queryJson : Model -> Encode.Value
queryJson model =
    Encode.object
        [ ( "query", Encode.string model.searchBar ) ]



albumJson : Shared.Album -> Encode.Value
albumJson albumObject =
    Encode.object
        [ ("name", Encode.string albumObject.name)
        , ("artists", Encode.list Encode.string albumObject.artists)
        , ("year", Encode.int albumObject.year)
        , ("coverArtUrl", Encode.string albumObject.coverArtUrl)
        , ("spotifyId", Encode.string albumObject.spotifyId)
        , ("price", Encode.float albumObject.price)
        , ("borrowed", Encode.bool albumObject.borrowed)
        , ("lentOut", Encode.bool albumObject.lentOut)
        , ("borrowedFrom", Encode.string albumObject.borrowedFrom)
        , ("liked", Encode.bool albumObject.liked)
        , ("borrowedReturnDate", Encode.string albumObject.borrowedReturnDate)
        , ("wishlist", Encode.bool albumObject.wishlist)
        , ("lentOutReturnDate", Encode.string albumObject.lentOutReturnDate)
        ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedSidebar label ->
            ( { model | selectedFilter = label }, Cmd.none )

        ChangedSearch query ->
            ( { model | searchBar = query }, Cmd.none)

        EnteredSearch ->
            ( model
            , Http.get(
                { url = "https://juliette-backend.onrender.com/album/search?q=" ++ model.searchBar
                , expect = Http.expectJson GotSuggestedAlbums (list Shared.albumDecoder)
                }
              )
            )

        GotAlbums (Ok receivedAlbums) -> 
            ( { model | albums = receivedAlbums }, Cmd.none )

        GotAlbums (Err _) ->
            ( { model | error = "there has been an error getting the albums"}, Cmd.none )

        ChoseSuggestedAlbum chosenAlbum ->
            ( { model | suggestedAlbums = [] }
            , Http.post(
                { url = "https://juliette-backend.onrender.com/albums/add"
                , body = Http.jsonBody (albumJson chosenAlbum)
                , expect = Http.expectJson GotAlbums (list Shared.albumDecoder)
                }
              )
            )

        GotSuggestedAlbums (Ok receivedAlbums) ->
            ( { model | suggestedAlbums = receivedAlbums }, Cmd.none )

        GotSuggestedAlbums (Err _) ->
            ( model, Cmd.none )

        ToggleLike toUpdate ->
            ( model
            , Http.request(
                { method = "PATCH"
                , headers = []
                , url = "https://juliette-backend.onrender.com/albums/" ++ toUpdate.id
                , body = Http.jsonBody (Encode.object [("liked", Encode.bool toUpdate.liked)])
                , expect = Http.expectJson GotAlbums (list Shared.albumDecoder)
                , tracker = Nothing
                , timeout = Nothing
                }
              )
            )

        ClickedDelete id ->
            ( model
            , Http.request(
                { method = "DELETE"
                , headers = []
                , url = "https://juliette-backend.onrender.com/albums/" ++ id
                , body = Http.emptyBody
                , expect = Http.expectJson GotAlbums (list Shared.albumDecoder)
                , tracker = Nothing
                , timeout = Nothing
                }
              )
            )

        ChoseWishlistAlbum chosenAlbum ->
            ( model
            , Http.post(
                { url = "https://juliette-backend.onrender.com/albums/add"
                , body = Http.jsonBody (albumJson chosenAlbum)
                , expect = Http.expectJson GotAlbums (list Shared.albumDecoder)
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
    { title = "Records" 
    , element = 
        column
            [width fill, height fill, padding 20, spacing 20]
            [ searchBar model
            , suggestedAlbums model
            , likedAlbums model
            , el [] (text ("Total spent: " ++ String.fromFloat (List.sum (List.map (\album -> album.price) model.albums))))
            , ownedAlbums model
            , borrowedAlbums model
            , lentOutAlbums model
            , wishlistAlbums model
            ]
    }

-- ripped from https://ellie-app.com/5X6jBKtxzdpa1
onEnter : Msg -> Element.Attribute Msg
onEnter msg =
    Element.htmlAttribute
        (Html.Events.on "keyup"
            (Decode.field "key" Decode.string
                |> Decode.andThen
                    (\key ->
                        if key == "Enter" then
                            Decode.succeed msg

                        else
                            Decode.fail "Not the enter key"
                    )
            )
        )

searchBar : Model -> Element Msg
searchBar model =
    Input.text 
        [ onEnter EnteredSearch ]
        { onChange = \query -> ChangedSearch query 
        , text = model.searchBar
        , placeholder = Just (Input.placeholder [] (text "search albums"))
        , label = Input.labelHidden "search"
        }

suggestedAlbums : Model -> Element Msg
suggestedAlbums model =
    if not (List.isEmpty model.suggestedAlbums) then
        column
            [ Border.width 3, height (px 150), scrollbarY, spacing 10, padding 10 ]
            <| List.map suggestedAlbum model.suggestedAlbums
    else
        el [] (text "")

suggestedAlbum : Shared.Album -> Element Msg
suggestedAlbum albumObject =
    row
        [ spacing 10, padding 10, Border.width 1, Border.rounded 10 ]
        [ image [ height (px 50), width (px 50) ] 
                { src = albumObject.coverArtUrl
                , description = ("cover art for " ++ albumObject.name)
                }
        , column
            []
            [ Input.button [ Font.size 18 ] 
                            { onPress = Just (ChoseSuggestedAlbum albumObject)
                            , label = (text (albumObject.name ++ " (" ++ (String.fromInt albumObject.year) ++ ")"))
                            }
            , el [Font.size 12] (text (String.join ", " albumObject.artists))
            ]
        , column
            [ spacing 5 ]
            [ Input.button [] { onPress = Just (ChoseSuggestedAlbum albumObject), label = text "Add to Owned"}
            , Input.button [] { onPress = Just (ChoseWishlistAlbum { albumObject | wishlist = True }), label = text "Add to Wishlist"}
            ]
        ]

basicAlbumStart : Shared.Album -> List (Element Msg)
basicAlbumStart albumObject =
    [ Input.button [ Background.image (if albumObject.liked then "full_heart.jpeg" else "empty_heart.png"), height (px 35), width (px 35) ]
                   { onPress = Just (ToggleLike { id = albumObject.id, liked = not albumObject.liked })
                   , label = text ""
                   }
    , image [ height (px 50), width (px 50) ] 
            { src = albumObject.coverArtUrl
            , description = ("cover art for " ++ albumObject.name)
            }
    , column
        []
        [ link [ Font.size 18 ] 
               { url = "/album/" ++ albumObject.id
               , label = (text (albumObject.name ++ " (" ++ (String.fromInt albumObject.year) ++ ")"))
               }
        , el [Font.size 12] (text (String.join ", " albumObject.artists))
        ]
    ]

basicAlbumEnd : Shared.Album -> List (Element Msg)
basicAlbumEnd albumObject =
    [ el [ alignRight ] (text ("₱" ++ String.fromFloat albumObject.price))
    , link [ ] { url = "edit/" ++ albumObject.id, label = text "Edit" }
    , Input.button []
                   { onPress = Just (ClickedDelete albumObject.id)
                   , label = text "Delete"
                   }
    ]


albumsWrapper : String -> (Shared.Album -> Bool) -> (Shared.Album -> Element Msg) -> Model -> Element Msg
albumsWrapper title filterfn albumWrapper model = 
    column []
           [ el [] (text title)
           , column
                [ Border.width 3, padding 10 ]
                <| List.map albumWrapper (List.filter filterfn model.albums)
           ]

ownedAlbums : Model -> Element Msg
ownedAlbums model = 
    albumsWrapper "Owned" (\album -> not album.wishlist) basicAlbum model

albumStyles : List (Element.Attribute Msg)
albumStyles =
    [ spacing 10, padding 10, Border.width 1, Border.rounded 10, width fill ]

basicAlbum : Shared.Album -> Element Msg
basicAlbum albumObject = 
    row
        albumStyles
        <| basicAlbumStart albumObject ++ basicAlbumEnd albumObject

likedAlbums : Model -> Element Msg
likedAlbums model =
    albumsWrapper "Liked" (\album -> album.liked) basicAlbum model

borrowedAlbums : Model -> Element Msg
borrowedAlbums model =
    albumsWrapper "Borrowed" (\album -> album.borrowed) borrowedAlbum model

borrowedAlbum : Shared.Album -> Element Msg
borrowedAlbum albumObject =
    row
        albumStyles
        <| basicAlbumStart albumObject ++ 
           [ infoEl "return date" albumObject.borrowedReturnDate
           , infoEl "borrowed from" albumObject.borrowedFrom
           ] ++
           basicAlbumEnd albumObject

lentOutAlbums : Model -> Element Msg
lentOutAlbums model =
    albumsWrapper "Lent Out" (\album -> album.lentOut) lentOutAlbum model

lentOutAlbum : Shared.Album -> Element Msg
lentOutAlbum albumObject =
    row
        albumStyles
        <| basicAlbumStart albumObject ++ 
           [ infoEl "return date" albumObject.lentOutReturnDate
           , infoEl "lent out to" albumObject.lentOutTo
           ] ++
           basicAlbumEnd albumObject

wishlistAlbums : Model -> Element Msg
wishlistAlbums model =
    albumsWrapper "Wishlist" (\album -> album.wishlist) basicAlbum model

infoEl : String -> String -> Element Msg
infoEl txt data =
    column
        [ spacing 5 ]
        [ el [ centerX ] (text txt)
        , el [ centerX ] (text data)
        ]
