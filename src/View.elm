module View exposing (View, map, none, placeholder, toBrowserDocument)

import Browser
import Element exposing (Element, text, layout)
import Element.Background as Background
import Element exposing (rgb)


type alias View msg =
    { title : String
    , element : Element msg
    }


placeholder : String -> View msg
placeholder str =
    { title = str
    , element = text str
    }


none : View msg
none =
    { title = ""
    , element = Element.none
    }


map : (a -> b) -> View a -> View b
map fn view =
    { title = view.title
    , element = Element.map fn view.element
    }


toBrowserDocument : View msg -> Browser.Document msg
toBrowserDocument view =
    { title = view.title
    , body = [ layout [] view.element ]
    }
