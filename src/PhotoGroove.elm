module PhotoGroove exposing (main)

import Array exposing (Array)
import Browser
import Html exposing (Html, button, div, h1, h3, img, input, label, text)
import Html.Attributes exposing (checked, class, classList, id, name, src, type_)
import Html.Events exposing (onClick)
import Random


type ThumbnailSize
    = Small
    | Medium
    | Large


enumThumbnailSize =
    [ Small, Medium, Large ]


sizeToString : ThumbnailSize -> String
sizeToString s =
    case s of
        Small ->
            "small"

        Medium ->
            "med"

        Large ->
            "large"


sizeToClass : ThumbnailSize -> Html.Attribute msg
sizeToClass =
    class << sizeToString


type alias Photo =
    { url : String }


type alias Model =
    { photos : List Photo
    , selectedUrl : String
    , chosenSize : ThumbnailSize
    }


type Msg
    = ClickedPhoto String
    | ClickedSize ThumbnailSize
    | ClickedSurpriseMe
    | GotSelectedIndex Int


main : Program () Model Msg
main =
    Browser.element
        { init = \flags -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \model -> Sub.none
        }


initialModel : Model
initialModel =
    { photos =
        [ { url = "1.jpeg" }
        , { url = "2.jpeg" }
        , { url = "3.jpeg" }
        ]
    , selectedUrl = "1.jpeg"
    , chosenSize = Medium
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedPhoto url ->
            ( { model | selectedUrl = url }, Cmd.none )

        ClickedSurpriseMe ->
            ( model, Random.generate GotSelectedIndex <| randomPhotoPicker photoArray )

        ClickedSize size ->
            ( { model | chosenSize = size }, Cmd.none )

        GotSelectedIndex index ->
            ( { model | selectedUrl = gotPhotoUrl index photoArray }, Cmd.none )


urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"


photoArray : Array Photo
photoArray =
    Array.fromList initialModel.photos


randomPhotoPicker : Array Photo -> Random.Generator Int
randomPhotoPicker photos =
    Random.int 0 (Array.length photos - 1)


gotPhotoUrl : Int -> Array Photo -> String
gotPhotoUrl index photos =
    case Array.get index photos of
        Just photo ->
            photo.url

        Nothing ->
            "1.jpeg"


viewThumbnail : String -> Photo -> Html Msg
viewThumbnail selectedUrl photo =
    img
        [ src (urlPrefix ++ photo.url)
        , classList [ ( "selected", selectedUrl == photo.url ) ]
        , onClick <| ClickedPhoto photo.url
        ]
        []


viewSizeChooser : ThumbnailSize -> ThumbnailSize -> Html Msg
viewSizeChooser chosenSize size =
    label []
        [ input
            [ type_ "radio"
            , name "size"
            , checked (chosenSize == size)
            , onClick <| ClickedSize size
            ]
            []
        , (text << sizeToString) size
        ]


view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ h1 [] [ text "Photo Groove" ]
        , h3 [] [ text "Thumbnail Size:" ]
        , button [ onClick ClickedSurpriseMe ]
            [ text "Surprise Me!" ]
        , div [ id "choose-size" ]
            (List.map (viewSizeChooser model.chosenSize) enumThumbnailSize)
        , div [ id "thumbnails", sizeToClass model.chosenSize ]
            (List.map (viewThumbnail model.selectedUrl) model.photos)
        , img
            [ class "large"
            , src (urlPrefix ++ "large/" ++ model.selectedUrl)
            ]
            []
        ]
