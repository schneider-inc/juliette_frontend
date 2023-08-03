module Pages.Add exposing (Model, Msg, page)

import Gen.Params.Add exposing (Params)
import Page
import Request
import Shared
import View exposing (View)
import Element exposing (Element, rgb255, el, padding, spacing, column, text, link)
import Element.Input as Input
import Element.Background as Background
import Http
import Json.Encode as Encode
import Json.Decode as Decode exposing (at, string)

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
    { name : String
    , artists : String
    , year : String
    , price : String
    , borrowed : Bool
    , favorited : Bool
    , returnDate : String
    , formSubmitted : Bool
    , message : String
    }

initialModel : Model
initialModel =
    { name = ""
    , artists = ""
    , year = ""
    , price = ""
    , borrowed = False
    , favorited = False
    , returnDate = ""
    , formSubmitted = False
    , message = ""
    }

init : ( Model, Cmd msg )
init =
    ( initialModel, Cmd.none )



-- UPDATE


type Msg
    = FormSubmitted
    | ChangedName String
    | ChangedArtists String
    | ChangedYear String
    | ChangedPrice String
    | ChangedBorrowed Bool
    | ChangedReturnDate String
    | GotResponse (Result Http.Error String)

modelJson : Model -> Encode.Value
modelJson model = 
    Encode.object
        [ ("name", Encode.string model.name)
        , ("artists", Encode.string model.artists)
        , ("year", Encode.string model.year) 
        , ("price", Encode.string model.price)
        , ("borrowed", Encode.bool model.borrowed)
        , ("returnDate", Encode.string model.returnDate)
        , ("favorited", Encode.bool False)
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FormSubmitted ->
            ( { model | formSubmitted = True } 
            , Http.post
                { url = "http://localhost:3000/album"
                , body = Http.jsonBody (modelJson model)
                -- figure out expect
                , expect = Http.expectJson GotResponse (at ["message"] string)
                } 
            )

        GotResponse (Ok message) ->
            ( { model | message = message}, Cmd.none)

        GotResponse (Err _) ->
            ( { model | message = "error with POST request"}, Cmd.none)

        ChangedName val ->
            ( { model | name = val }, Cmd.none )

        ChangedArtists val ->
            ( { model | artists = val }, Cmd.none)

        ChangedYear val ->
            ( { model | year = val }, Cmd.none )
        
        ChangedPrice val ->
            ( { model | price = val }, Cmd.none )
        
        ChangedReturnDate val ->
            ( { model | returnDate = val }, Cmd.none)

        ChangedBorrowed val ->
            ( { model | borrowed = val }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Add Album"
    , element = addAlbumForm model
    }

addAlbumForm : Model -> Element Msg
addAlbumForm model =
    column
        [ padding 30, spacing 20 ]
        [ Input.text [] { onChange = \val -> ChangedName val
                        , text = model.name
                        , placeholder = Just (Input.placeholder [] (text "Smells Like Juliette Spirit"))
                        , label = Input.labelLeft [] (text "name")
                        }
        , Input.text [] { onChange = \val -> ChangedArtists val
                        , text = model.artists
                        , placeholder = Just (Input.placeholder [] (text "Juliette Hoschka, Justin Schneider"))
                        , label = Input.labelLeft [] (text "artists")
                        }
        , Input.text [] { onChange = \val -> ChangedYear val
                        , text = model.year
                        , placeholder = Just (Input.placeholder [] (text "2023"))
                        , label = Input.labelLeft [] (text "release year")
                        }
        , Input.text [] { onChange = \val -> ChangedPrice val
                        , text = model.price
                        , placeholder = Just (Input.placeholder [] (text "2700.0"))
                        , label = Input.labelLeft [] (text "price (in pesos)")
                        }
        , Input.checkbox [] { onChange = \val -> ChangedBorrowed val
                        , icon = Input.defaultCheckbox
                        , checked = model.borrowed
                        , label = Input.labelLeft [] (text "borrowed?")
                        }
        , borrowedOptions model
        , Input.button Shared.buttonStyles
            { onPress = Just FormSubmitted
            , label = text "submit album"
            }
        , el [] (text model.message)
        ]

borrowedOptions : Model -> Element Msg
borrowedOptions model =
    if model.borrowed then
        Input.text
            []
            { onChange = \val -> ChangedReturnDate val
            , text = model.returnDate
            , placeholder = Just (Input.placeholder [] (text "20/01/2005"))
            , label = Input.labelLeft [] (text "return date DD/MM/YYYY")
            }

    else
        text ""

