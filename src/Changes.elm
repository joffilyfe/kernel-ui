module Changes exposing (..)

import Browser
import Http
import Html exposing (..)
import Html.Events exposing (onClick)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (optional, optionalAt, required, requiredAt)


type alias Change =
    { id : String
    , timestamp : String
    , deleted : Bool
    }


type alias ChangeResponse =
    { since : String
    , limit : Int
    , results : List Change
    }


type alias Model =
    { errorMessage : Maybe String
    , response : ChangeResponse
    }



-- VIEW


type Msg
    = SendHttpRequest
    | DataReceived (Result Http.Error ChangeResponse)


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick SendHttpRequest ] [ text "SendHttpRequest" ]
        , viewPostsOrError model
        ]


viewPostsOrError : Model -> Html Msg
viewPostsOrError model =
    case model.errorMessage of
        Just message ->
            viewError message

        Nothing ->
            viewChanges model.response.results


viewError : String -> Html msg
viewError message =
    div [] [ text message ]


viewChanges : List Change -> Html msg
viewChanges changes =
    div []
        [ ul [] (List.map viewChange changes)
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


changeResponseDecoder : Decoder ChangeResponse
changeResponseDecoder =
    Json.Decode.succeed ChangeResponse
        |> required "since" string
        |> required "limit" int
        |> required "results" (list changeDecoder)


changeDecoder : Decoder Change
changeDecoder =
    Json.Decode.succeed Change
        |> required "id" string
        |> required "timestamp" string
        |> optional "deleted" bool False


httpCommand : Cmd Msg
httpCommand =
    Http.get
        { url = "http://0.0.0.0:6543/changes"
        , expect = Http.expectJson DataReceived changeResponseDecoder
        }



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SendHttpRequest ->
            ( model, httpCommand )

        DataReceived (Ok response) ->
            ( { model
                | response = response
                , errorMessage = Nothing
              }
            , Cmd.none
            )

        DataReceived (Err httpError) ->
            ( { model
                | errorMessage = Just (buildErrorMessage httpError)
              }
            , Cmd.none
            )



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
    ( { response = ChangeResponse "a" 500 []
      , errorMessage = Nothing
      }
    , Cmd.none
    )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
