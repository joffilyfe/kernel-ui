module Changes exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (class, classList, href)
import Html.Events exposing (onClick)
import Http
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (optional, optionalAt, required, requiredAt)


type alias Change =
    { id : String
    , timestamp : String
    , deleted : Bool
    }


type alias Response =
    { since : String
    , limit : Int
    , changes : List Change
    }


type alias Model =
    { errorMessage : Maybe String
    , apiUrl : String
    , response : Status Response
    , menuOpened : Bool
    }


type Status a
    = Loading
    | Loaded a
    | Failed


type alias Document msg =
    { title : String
    , body : List (Html msg)
    }


{-| Display header menu
showHeader { model }
-}
showHeader : Model -> Html Msg
showHeader model =
    header [ class "site-header bg-black" ]
        [ nav [ class "navbar navbar-expand-lg navbar-dark bg-dark" ]
            [ div [ class "container" ]
                [ a [ class "navbar-brand", href "/" ] [ text "Kernel UI" ]
                , button [ class "navbar-toggler", onClick ToggleMenuCollapse ] [ span [ class "navbar-toggler-icon" ] [] ]
                , div
                    [ classList
                        [ ( "collapse", True )
                        , ( "navbar-collapse", True )
                        , ( "show", model.menuOpened )
                        ]
                    ]
                    [ ul [ class "navbar-nav mr-auto mt-2 mt-lg-0" ] []
                    ]
                ]
            ]
        ]



-- VIEW


view : Model -> Document Msg
view model =
    { title = "Home"
    , body =
        [ showHeader model
        , div []
            [ div [ class "container" ]
                [ div [ class "row" ]
                    [ div [ class "col-12" ]
                        [ div []
                            [ viewChanges model
                            , button
                                [ class "btn btn-secondary btn-lg btn-block"
                                , onClick FetchChangeAPI
                                ]
                                [ text "Reload Changes" ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
    }


viewError : String -> Html msg
viewError message =
    div [] [ text message ]


viewChanges : Model -> Html msg
viewChanges model =
    case model.response of
        Loading ->
            div [] [ text "Loading API changes, please wait." ]

        Failed ->
            div [] [ text "Something goes wrong, please try fetch changes again" ]

        Loaded response ->
            div []
                [ h1 [] [ text "Changes" ]
                , ul [] (List.map viewChange response.changes)
                ]


viewChange : Change -> Html msg
viewChange change =
    if change.deleted then
        li []
            [ span [] [ text change.id ]
            , span [] [ text change.timestamp ]
            , span [] [ text " Deletado" ]
            ]

    else
        li []
            [ span [] [ text change.id ]
            , span [] [ text change.timestamp ]
            ]



-- DECODERS


responseDecoder : Decoder Response
responseDecoder =
    Json.Decode.succeed Response
        |> required "since" string
        |> required "limit" int
        |> required "results" (list changeDecoder)


changeDecoder : Decoder Change
changeDecoder =
    Json.Decode.succeed Change
        |> required "id" string
        |> required "timestamp" string
        |> optional "deleted" bool False


httpCommand : String -> Cmd Msg
httpCommand apiUrl =
    Http.get
        { url = apiUrl
        , expect = Http.expectJson CompletedChangesFetch responseDecoder
        }



-- UPDATE


type Msg
    = ToggleMenuCollapse
    | FetchChangeAPI
    | CompletedChangesFetch (Result Http.Error Response)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchChangeAPI ->
            ( { model | response = Loading }, httpCommand model.apiUrl )

        CompletedChangesFetch (Ok response) ->
            ( { model
                | response = Loaded response
                , errorMessage = Nothing
              }
            , Cmd.none
            )

        CompletedChangesFetch (Err httpError) ->
            ( { model
                | errorMessage = Just (buildErrorMessage httpError)
              }
            , Cmd.none
            )

        ToggleMenuCollapse ->
            ( { model | menuOpened = not model.menuOpened }, Cmd.none )



-- HELPER


buildErrorMessage : Http.Error -> String
buildErrorMessage httpError =
    case httpError of
        Http.BadUrl message ->
            message

        Http.Timeout ->
            "Server is taking too long to respond. Please try again later."

        Http.NetworkError ->
            "Unable to reach server."

        Http.BadStatus statusCode ->
            "Request failed with status code: " ++ String.fromInt statusCode

        Http.BadBody message ->
            message



-- INIT


init : () -> ( Model, Cmd Msg )
init _ =
    let
        apiUrl =
            "http://0.0.0.0:6543/changes"
    in
    ( { response = Loading
      , errorMessage = Nothing
      , apiUrl = apiUrl
      , menuOpened = False
      }
    , httpCommand apiUrl
    )


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
