module Page.Opponents exposing (Model, Msg(..), init, update, view)

import Api.UserPrefs
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http


type alias Model =
    { opponent1 : String
    , opponent2 : String
    , opponent3 : String
    , favorites : List String
    , loading : Bool
    }


type Msg
    = SetOpponent1 String
    | SetOpponent2 String
    | SetOpponent3 String
    | AddBot1
    | AddBot2
    | AddBot3
    | SelectFavorite Int String
    | GotFavorites (Result Http.Error (List String))
    | CreateGame
    | GoBack


init : String -> String -> ( Model, Cmd Msg )
init baseUrl token =
    ( { opponent1 = ""
      , opponent2 = ""
      , opponent3 = ""
      , favorites = []
      , loading = True
      }
    , Api.UserPrefs.fetchBookmarked baseUrl token GotFavorites
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetOpponent1 s ->
            ( { model | opponent1 = s }, Cmd.none )

        SetOpponent2 s ->
            ( { model | opponent2 = s }, Cmd.none )

        SetOpponent3 s ->
            ( { model | opponent3 = s }, Cmd.none )

        AddBot1 ->
            ( { model | opponent1 = "bot1" }, Cmd.none )

        AddBot2 ->
            ( { model | opponent2 = "bot2" }, Cmd.none )

        AddBot3 ->
            ( { model | opponent3 = "bot3" }, Cmd.none )

        SelectFavorite slot name ->
            case slot of
                1 ->
                    ( { model | opponent1 = name }, Cmd.none )

                2 ->
                    ( { model | opponent2 = name }, Cmd.none )

                3 ->
                    ( { model | opponent3 = name }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        GotFavorites (Ok names) ->
            ( { model | favorites = names, loading = False }, Cmd.none )

        GotFavorites (Err _) ->
            ( { model | loading = False }, Cmd.none )

        CreateGame ->
            ( model, Cmd.none )

        GoBack ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div [ class "page opponents-page" ]
        [ div [ class "login-card opponents-card" ]
            [ h1 [] [ text "New Game" ]
            , h2 [] [ text "Choose opponents" ]
            , viewOpponentSlot 1 "Opponent 1" model.opponent1 SetOpponent1 AddBot1 model.favorites
            , viewOpponentSlot 2 "Opponent 2 (optional)" model.opponent2 SetOpponent2 AddBot2 model.favorites
            , viewOpponentSlot 3 "Opponent 3 (optional)" model.opponent3 SetOpponent3 AddBot3 model.favorites
            , if not (List.isEmpty model.favorites) then
                div [ class "favorites-section" ]
                    [ h3 [] [ text "Favorites" ]
                    , div [ class "favorites-chips" ]
                        (List.map
                            (\name ->
                                button
                                    [ class "chip"
                                    , onClick (SelectFavorite (nextEmptySlot model) name)
                                    ]
                                    [ text name ]
                            )
                            model.favorites
                        )
                    ]

              else
                text ""
            , div [ class "opponents-actions" ]
                [ button [ onClick CreateGame, class "btn btn-primary" ] [ text "Start Game" ]
                , button [ onClick GoBack, class "btn btn-secondary" ] [ text "Back" ]
                ]
            ]
        ]


viewOpponentSlot : Int -> String -> String -> (String -> Msg) -> Msg -> List String -> Html Msg
viewOpponentSlot slot placeholder_ value_ onInputMsg botMsg favorites =
    div [ class "opponent-row" ]
        [ div [ class "opponent-input-wrap" ]
            [ input
                [ type_ "text"
                , placeholder placeholder_
                , value value_
                , onInput onInputMsg
                , class "input-field"
                , list ("favorites-" ++ String.fromInt slot)
                ]
                []
            , datalist [ id ("favorites-" ++ String.fromInt slot) ]
                (List.map (\n -> option [ value n ] []) favorites)
            ]
        , button [ onClick botMsg, class "btn btn-small btn-bot" ] [ text "Bot" ]
        ]


nextEmptySlot : Model -> Int
nextEmptySlot model =
    if model.opponent1 == "" then
        1

    else if model.opponent2 == "" then
        2

    else if model.opponent3 == "" then
        3

    else
        1
