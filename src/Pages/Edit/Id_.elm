module Pages.Edit.Id_ exposing (Model, Msg, page)

import Gen.Params.Edit.Id_ exposing (Params)
import Page
import Request
import Shared
import View exposing (View)
import Element exposing (Element, link, el, rgb, text, column, spacing, padding)
import Element.Input as Input
import Element.Font as Font
import Http
import Json.Encode as Encode
import Json.Decode as Decode exposing (string, field)


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
    { id : String
    , price : String
    , priceMessage : String
    , borrowed : Bool
    , lentOut : Bool
    , borrowedFrom : String
    , lentOutTo : String
    , borrowedReturnDate : String
    , lentOutReturnDate : String
    , submitMessage : String
    }

initialModel : Params -> Model
initialModel params =
    { id = params.id
    , price = ""
    , priceMessage = ""
    , borrowed = False
    , lentOut = False
    , borrowedFrom = ""
    , lentOutTo = ""
    , borrowedReturnDate = ""
    , lentOutReturnDate = ""
    , submitMessage = ""
    }

init : Params -> ( Model, Cmd Msg )
init params =
    ( initialModel params
    , Http.get(
        { url = "https://juliette-backend.onrender.com/albums/" ++ (initialModel params).id
        , expect = Http.expectJson GotAlbum Shared.albumDecoder
        }
    ) 
    )



-- UPDATE


type Msg
    = ChangedPrice String
    | ChangedBorrowed Bool
    | ChangedLentOut Bool
    | ChangedBorrowedFrom String
    | ChangedLentOutTo String
    | ChangedBorrowedReturnDate String
    | ChangedLentOutReturnDate String
    | ClickedSubmit
    | Submitted (Result Http.Error Shared.Album)
    | GotAlbum (Result Http.Error Shared.Album)

patchJson : Model -> Encode.Value
patchJson model =
    Encode.object
        [ ("id", Encode.string model.id)
        , ("price", Encode.float (Maybe.withDefault 0.0 (String.toFloat model.price)))
        , ("borrowed", Encode.bool model.borrowed)
        , ("lentOut", Encode.bool model.lentOut)
        , ("borrowedFrom", Encode.string model.borrowedFrom)
        , ("lentOutTo", Encode.string model.lentOutTo)
        , ("borrowedReturnDate", Encode.string model.borrowedReturnDate)
        , ("lentOutReturnDate", Encode.string model.lentOutReturnDate)
        ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotAlbum (Ok album) ->
            ( { model | price = String.fromFloat album.price, borrowed = album.borrowed, lentOut = album.lentOut, borrowedFrom = album.borrowedFrom, lentOutTo = album.lentOutTo, borrowedReturnDate = album.borrowedReturnDate, lentOutReturnDate = album.lentOutReturnDate }, Cmd.none)
        GotAlbum (Err _) ->
            ( { model | submitMessage = "could not fetch info, please enter it in manually" }, Cmd.none )
        ChangedPrice price ->
            ( { model | price = price, priceMessage = "" }, Cmd.none )

        ChangedBorrowed val ->
            ( { model | borrowed = val }, Cmd.none)

        ChangedLentOut val ->
            ( { model | lentOut = val }, Cmd.none )

        ChangedBorrowedFrom val ->
            ( { model | borrowedFrom = val }, Cmd.none )

        ChangedLentOutTo val ->
            ( { model | lentOutTo = val }, Cmd.none )

        ChangedBorrowedReturnDate val ->
            ( { model | borrowedReturnDate = val }, Cmd.none )

        ChangedLentOutReturnDate val ->
            ( { model | lentOutReturnDate = val }, Cmd.none )

        ClickedSubmit -> 
            ( model
            , Http.request(
                { method = "PATCH"
                , headers = []
                , url = "https://juliette-backend.onrender.com/albums/" ++ model.id
                , body = Http.jsonBody (patchJson model)
                , expect = Http.expectJson Submitted Shared.albumDecoder
                , tracker = Nothing
                , timeout = Nothing
                }
              )
            )

        Submitted (Ok _) ->
            ( { model | submitMessage = "succesfully updated record!" }, Cmd.none )

        Submitted (Err _) ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Edit Album"
    , element =
        column 
            [ padding 30, spacing 10 ]
            [ Input.text []
                         { onChange = \val -> ChangedPrice val
                         , label = Input.labelLeft [] (text "price")
                         , placeholder = Just (Input.placeholder [] (text "price"))
                         , text = model.price
                         }
            , el [ Font.color (rgb 1 0 0) ] (text model.priceMessage)
            , Input.checkbox []
                             { onChange = \val -> ChangedBorrowed val
                             , icon = Input.defaultCheckbox
                             , checked = model.borrowed
                             , label = Input.labelLeft [] (text "borrowed?")
                             }
            , if model.borrowed then 
                Input.text []
                           { onChange = \val -> ChangedBorrowedFrom val
                           , label = Input.labelLeft [] (text "borrowed from")
                           , placeholder = Just (Input.placeholder [] (text ""))
                           , text = model.borrowedFrom
                           }
              else
                el [] (text "")
            , if model.borrowed then 
                Input.text []
                           { onChange = \val -> ChangedBorrowedReturnDate val
                           , label = Input.labelLeft [] (text "return date (day/month/year)")
                           , placeholder = Just (Input.placeholder [] (text "DD/MM/YYY"))
                           , text = model.borrowedReturnDate
                           }
              else
                el [] (text "")
            , Input.checkbox []
                             { onChange = \val -> ChangedLentOut val
                             , icon = Input.defaultCheckbox
                             , checked = model.lentOut
                             , label = Input.labelLeft [] (text "lent out?")
                             }
            , if model.lentOut then 
                Input.text []
                           { onChange = \val -> ChangedLentOutTo val
                           , label = Input.labelLeft [] (text "lent out to")
                           , placeholder = Just (Input.placeholder [] (text ""))
                           , text = model.lentOutTo
                           }
              else
                el [] (text "")
            , if model.lentOut then 
                Input.text []
                           { onChange = \val -> ChangedLentOutReturnDate val
                           , label = Input.labelLeft [] (text "return date (day/month/year)")
                           , placeholder = Just (Input.placeholder [] (text "DD/MM/YYY"))
                           , text = model.lentOutReturnDate
                           }
              else
                el [] (text "")
            , Input.button Shared.buttonStyles  { onPress = Just (ClickedSubmit)
                                                , label = text "Submit Changes"
                                                }
            , el [] (text model.submitMessage)
            , link [] { url = "/albums", label = text "Back to her collection!" }
            ]
    }
