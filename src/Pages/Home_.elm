module Pages.Home_ exposing (page)

import Gen.Params.Home_ exposing (Params)
import Page exposing (Page)
import Request
import Shared
import View exposing (View)
import Element exposing (Element, column, centerX, centerY, el, text, link)


page : Shared.Model -> Request.With Params -> Page
page shared req =
    Page.static
        { view = view
        }


view : View msg
view =
    { title = "Homepage"
    , element =
        column [ centerX, centerY ]
            [ el [ centerX ] (text "Welcome to Juliette's Record Collection!")
            , link (centerX :: Shared.buttonStyles)  { url = "/albums", label = text "Click here to view her collection!" }
            ]
    }
