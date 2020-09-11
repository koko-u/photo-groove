module PhotoGroove exposing (main)

import Browser
import Html exposing (Html, button, div, h1, h3, img, input, label, text)
import Html.Attributes exposing (checked, class, classList, id, name, src, title, type_)
import Html.Events exposing (onClick)
import Http
import Json.Decode as D
import Json.Decode.Pipeline as P
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
    { url : String
    , size : Int
    , title : String
    }


photoDecoder : D.Decoder Photo
photoDecoder =
    D.succeed Photo
        |> P.required "url" D.string
        |> P.required "size" D.int
        |> P.optional "title" D.string "(no title)"


type Status
    = Loading
    | Loaded (List Photo) String
    | Errored String


type alias Model =
    { status : Status
    , chosenSize : ThumbnailSize
    }


type Msg
    = ClickedPhoto String
    | ClickedSize ThumbnailSize
    | ClickedSurpriseMe
    | GotRandomPhoto Photo
    | GotPhotos (Result Http.Error (List Photo))


main : Program () Model Msg
main =
    Browser.element
        { init = \flags -> ( initialModel, initialCmd )
        , view = view
        , update = update
        , subscriptions = \model -> Sub.none
        }


initialModel : Model
initialModel =
    { status = Loading
    , chosenSize = Medium
    }


initialCmd : Cmd Msg
initialCmd =
    Http.get
        { url = "http://elm-in-action.com/photos/list.json"
        , expect = Http.expectJson GotPhotos (D.list photoDecoder)
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedPhoto url ->
            ( { model | status = selectUrl url model.status }, Cmd.none )

        ClickedSurpriseMe ->
            case model.status of
                Loaded (firstPhoto :: otherPhotos) _ ->
                    Random.uniform firstPhoto otherPhotos
                        |> Random.generate GotRandomPhoto
                        |> Tuple.pair model

                _ ->
                    ( model, Cmd.none )

        ClickedSize size ->
            ( { model | chosenSize = size }, Cmd.none )

        GotRandomPhoto photo ->
            ( { model | status = selectUrl photo.url model.status }, Cmd.none )

        GotPhotos (Ok photos) ->
            case photos of
                first :: _ ->
                    ( { model | status = Loaded photos first.url }, Cmd.none )

                [] ->
                    ( { model | status = Errored "0 photos found." }, Cmd.none )

        GotPhotos (Err _) ->
            ( { model | status = Errored "Server error!" }, Cmd.none )


selectUrl : String -> Status -> Status
selectUrl url status =
    case status of
        Loaded photos _ ->
            Loaded photos url

        _ ->
            status


urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"


viewThumbnail : String -> Photo -> Html Msg
viewThumbnail selectedUrl photo =
    img
        [ src (urlPrefix ++ photo.url)
        , title <| photo.title ++ " [" ++ String.fromInt photo.size ++ " KB]"
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
    div [ class "content" ] <|
        case model.status of
            Loaded photos selectedUrl ->
                viewLoaded photos selectedUrl model.chosenSize

            Loading ->
                []

            Errored errorMsg ->
                [ text <| "Error: " ++ errorMsg ]


viewLoaded : List Photo -> String -> ThumbnailSize -> List (Html Msg)
viewLoaded photos selectedUrl chosenSize =
    [ h1 [] [ text "Photo Groove" ]
    , h3 [] [ text "Thumbnail Size:" ]
    , button [ onClick ClickedSurpriseMe ]
        [ text "Surprise Me!" ]
    , div [ id "choose-size" ]
        (List.map (viewSizeChooser chosenSize) enumThumbnailSize)
    , div [ id "thumbnails", sizeToClass chosenSize ]
        (List.map (viewThumbnail selectedUrl) photos)
    , img
        [ class "large"
        , src (urlPrefix ++ "large/" ++ selectedUrl)
        ]
        []
    ]
