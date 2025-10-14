module SearchTCM exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Http
import Json.Decode as Decode exposing (Decoder)
import Set exposing (Set)


main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }



-- MODEL


type alias Model =
    { yearMinimum : Maybe Int
    , yearMaximum : Maybe Int
    , genresSelected : Set String
    , genresAvailable : Set String
    , moviesAvailable : Loadable (List Movie)
    , moviesShown : List Movie
    }


type alias Movie =
    { titleId : Int
    , title : String
    , genres : Set String
    , releaseYear : Int
    , description : Maybe String
    , runtime : Maybe String
    , rating : Maybe String
    , thumbnail : String
    }


init : () -> ( Model, Cmd Msg )
init () =
    ( { yearMinimum = Nothing
      , yearMaximum = Nothing
      , genresSelected = Set.empty
      , genresAvailable = Set.empty
      , moviesAvailable = LoadingInProgress
      , moviesShown = []
      }
    , getMovies
    )



-- MESSAGE


type Msg
    = GotMovies (Result Http.Error (List Movie))
    | SetYearMinimum (Maybe Int)
    | SetYearMaximum (Maybe Int)
    | DelGenre String
    | AddGenre String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotMovies result ->
            case result of
                Ok movies ->
                    ( moviesChanged { model | moviesAvailable = Loaded movies }, Cmd.none )

                Err problem ->
                    ( { model | moviesAvailable = LoadingFailed problem }, Cmd.none )

        SetYearMinimum year ->
            ( filtersChanged { model | yearMinimum = year }, Cmd.none )

        SetYearMaximum year ->
            ( filtersChanged { model | yearMaximum = year }, Cmd.none )

        AddGenre genre ->
            ( filtersChanged { model | genresSelected = Set.insert genre model.genresSelected }, Cmd.none )

        DelGenre genre ->
            ( filtersChanged { model | genresSelected = Set.remove genre model.genresSelected }, Cmd.none )


moviesChanged : Model -> Model
moviesChanged model =
    let
        moviesAvailable : List Movie
        moviesAvailable =
            case model.moviesAvailable of
                Loaded movies ->
                    movies

                _ ->
                    []

        mergeSets : List (Set comparable) -> Set comparable
        mergeSets =
            List.foldl Set.union Set.empty
    in
    { model
        | genresAvailable = mergeSets <| List.map .genres moviesAvailable
        , moviesShown = List.filter (matchesFilters model) moviesAvailable
    }


filtersChanged : Model -> Model
filtersChanged model =
    case model.moviesAvailable of
        Loaded movies ->
            { model | moviesShown = List.filter (matchesFilters model) movies }

        _ ->
            model


matchesFilters : Model -> Movie -> Bool
matchesFilters filters movie =
    let
        overlap : List a -> List a -> Bool
        overlap listA listB =
            List.any (hasMember listA) listB

        hasMember : List a -> a -> Bool
        hasMember xs x =
            List.member x xs

        matchYearMinimum : Bool
        matchYearMinimum =
            case filters.yearMinimum of
                Just year ->
                    movie.releaseYear >= year

                Nothing ->
                    True

        matchYearMaximum : Bool
        matchYearMaximum =
            case filters.yearMaximum of
                Just year ->
                    movie.releaseYear <= year

                Nothing ->
                    True

        matchGenres : Bool
        matchGenres =
            Set.isEmpty filters.genresSelected
                || (Set.intersect filters.genresSelected movie.genres |> Set.isEmpty |> not)
    in
    matchYearMinimum
        && matchYearMaximum
        && matchGenres



-- VIEW


view : Model -> Browser.Document Msg
view model =
    { title = "Search TCM"
    , body =
        [ Html.header headerStyle
            [ Html.h1 h1Style [ Html.text "Search TCM" ]
            , Html.hr [] []
            , viewFilters model
            ]
        , case model.moviesAvailable of
            LoadingInProgress ->
                Html.div [] [ Html.text "Loading..." ]

            LoadingFailed error ->
                Html.div [] [ Html.text ("Failed to load movies: " ++ explain error) ]

            Loaded _ ->
                Html.div moviesStyle (List.map viewMovie model.moviesShown)
        , Html.small []
            [ Html.text "Created by "
            , Html.a
                [ Attributes.href "mailto:cptakzero@gmail.com" ]
                [ Html.text "Christopher Ptak" ]
            , Html.text ". Email me for bug reports or feature requests."
            ]
        ]
    }


viewFilters : Model -> Html Msg
viewFilters model =
    Html.form formStyle
        [ Html.label
            (labelStyle ++ [ Attributes.for "year-minimum" ])
            [ Html.text "Released after" ]
        , Html.input
            (inputStyle
                ++ [ Attributes.id "year-minimum"
                   , Events.onInput (SetYearMinimum << String.toInt)
                   ]
            )
            []
        , Html.label
            (labelStyle ++ [ Attributes.for "year-maximum" ])
            [ Html.text "Released before" ]
        , Html.input
            (inputStyle
                ++ [ Attributes.id "year-maximum"
                   , Events.onInput (SetYearMaximum << String.toInt)
                   ]
            )
            []
        , Html.label
            (labelStyle ++ [ Attributes.for "genres" ])
            [ Html.text "Filter by Genre" ]
        , Html.select
            (inputStyle ++ [ Events.on "change" (Decode.map AddGenre Events.targetValue) ])
            ([ Html.option [] [ Html.text "Choose a genre" ] ]
                ++ (List.map viewGenreOption <| Set.toList model.genresAvailable)
            )
        , Html.ul [ Attributes.style "list-style" "none" ]
            (List.map viewGenreSelected <| Set.toList model.genresSelected)
        ]


viewGenreOption : String -> Html Msg
viewGenreOption genre =
    Html.option
        [ Attributes.value genre ]
        [ Html.text genre ]


viewGenreSelected : String -> Html Msg
viewGenreSelected genre =
    Html.li []
        [ Html.a
            [ Events.onClick (DelGenre genre) ]
            [ Html.text "🞪" ]
        , Html.text " "
        , Html.text genre
        ]


viewMovie : Movie -> Html Msg
viewMovie movie =
    let
        link =
            "https://www.tcm.com/watchtcm/titles/" ++ String.fromInt movie.titleId

        releaseYear =
            String.fromInt movie.releaseYear

        runtime =
            movie.runtime |> Maybe.map ((++) " | ") |> Maybe.withDefault ""

        rating =
            movie.rating |> Maybe.map ((++) " | ") |> Maybe.withDefault ""
    in
    Html.div movieCardStyle
        [ Html.a
            [ Attributes.href link ]
            [ Html.img (movieThumbnailStyle ++ [ Attributes.src movie.thumbnail ]) [] ]
        , Html.div
            movieDescriptionStyle
            [ Html.a (movieTitleStyle ++ [ Attributes.href link ]) [ Html.text movie.title ]
            , Html.div [] [ Html.text (releaseYear ++ runtime ++ rating) ]
            , Html.div []
                [ case movie.description of
                    Nothing ->
                        Html.i [] [ Html.text "No description" ]

                    Just text ->
                        Html.text text
                ]
            ]
        ]



-- STYLES


h1Style : List (Html.Attribute msg)
h1Style =
    [ Attributes.style "margin" "0" ]


headerStyle : List (Html.Attribute msg)
headerStyle =
    List.map (\( name, value ) -> Attributes.style name value)
        [ ( "padding-left", "20px" )
        , ( "padding-right", "20px" )
        , ( "padding-top", "10px" )
        , ( "padding-bottom", "10px" )
        , ( "margin-top", "10px" )
        , ( "margin-bottom", "10px" )
        , ( "position", "sticky" )
        , ( "top", "0" )
        , ( "border-radius", "10px" )
        , ( "background-color", "rgba(18, 18, 18, 0.75)" )
        , ( "backdrop-filter", "blur(5px)" )
        ]


formStyle : List (Html.Attribute msg)
formStyle =
    List.map (\( name, value ) -> Attributes.style name value)
        [ ( "width", "50%" )
        , ( "display", "grid" )
        , ( "grid-template-columns", "[label] auto [input] auto" )
        ]


labelStyle : List (Html.Attribute msg)
labelStyle =
    List.map (\( name, value ) -> Attributes.style name value)
        [ ( "grid-column", "label" )
        , ( "align-self", "center" )
        , ( "line-height", "2em" )
        ]


inputStyle : List (Html.Attribute msg)
inputStyle =
    List.map (\( name, value ) -> Attributes.style name value)
        [ ( "grid-column", "input" )
        , ( "align-self", "center" )
        , ( "height", "1.6em" )
        , ( "border", "none" )
        , ( "border-radius", "8px" )
        ]


moviesStyle : List (Html.Attribute msg)
moviesStyle =
    List.map (\( name, value ) -> Attributes.style name value)
        [ ( "display", "grid" )
        , ( "grid-template-columns", "repeat(4, 1fr)" )
        , ( "grid-gap", "19.37px" )
        ]


movieCardStyle : List (Html.Attribute msg)
movieCardStyle =
    List.map (\( name, value ) -> Attributes.style name value)
        [ ( "width", "285.467px" )
        , ( "min-height", "317px" )
        , ( "border-radius", "10px" )
        , ( "color", "#212121" )
        , ( "background-color", "#f5f5f5" )
        ]


movieThumbnailStyle : List (Html.Attribute msg)
movieThumbnailStyle =
    List.map (\( name, value ) -> Attributes.style name value)
        [ ( "width", "285.467px" )
        , ( "height", "187px" )
        , ( "border-radius", "10px 10px 0px 0px" )
        ]


movieDescriptionStyle : List (Html.Attribute msg)
movieDescriptionStyle =
    List.map (\( name, value ) -> Attributes.style name value)
        [ ( "padding-left", "14px" )
        , ( "padding-right", "14px" )
        , ( "padding-top", "4px" )
        , ( "padding-bottom", "14px" )
        ]


movieTitleStyle : List (Html.Attribute msg)
movieTitleStyle =
    List.map (\( name, value ) -> Attributes.style name value)
        [ ( "color", "#212121" )
        , ( "text-decoration", "none" )
        , ( "font-weight", "bold" )
        ]



-- LOADING STATUS


type Loadable a
    = LoadingInProgress
    | LoadingFailed Http.Error
    | Loaded a


explain : Http.Error -> String
explain error =
    case error of
        Http.BadUrl s ->
            "Bad URL: " ++ s

        Http.Timeout ->
            "Connection timed out"

        Http.NetworkError ->
            "Network error"

        Http.BadStatus code ->
            "HTTP error " ++ String.fromInt code

        Http.BadBody reason ->
            "Bad response: " ++ reason



-- API


getMovies : Cmd Msg
getMovies =
    Http.get
        { url = "https://tcmws.tcm.com/tcmws/latest/250"
        , expect = Http.expectJson GotMovies decodeMovies
        }


decodeMovies : Decoder (List Movie)
decodeMovies =
    Decode.at [ "tcm", "titles" ] (Decode.list decodeMovie)


decodeMovie : Decoder Movie
decodeMovie =
    Decode.map8 Movie
        (Decode.field "titleId" Decode.int)
        (Decode.field "name" Decode.string)
        (Decode.field "tvGenresArr" (Decode.map Set.fromList (Decode.list Decode.string)))
        (Decode.field "releaseYear" Decode.int)
        (Decode.field "description" (Decode.nullable Decode.string))
        (Decode.field "alternateRuntime" (Decode.nullable Decode.string))
        (Decode.field "tvRating" (Decode.nullable Decode.string))
        (Decode.field "imageProfiles" decodeThumbnailUrl)


type alias Thumbnail =
    { width : Int
    , height : Int
    , url : String
    , usage : String
    }


decodeThumbnailUrl : Decoder String
decodeThumbnailUrl =
    let
        decodeThumbnail : Decoder Thumbnail
        decodeThumbnail =
            Decode.map4 Thumbnail
                (Decode.field "width" Decode.int)
                (Decode.field "height" Decode.int)
                (Decode.field "url" Decode.string)
                (Decode.field "usage" Decode.string)

        resolution : Thumbnail -> Int
        resolution thumbnail =
            thumbnail.width * thumbnail.height

        chooseUrl : List Thumbnail -> String
        chooseUrl thumbnails =
            case List.filter (.usage >> (==) "homepageExploreThumb") thumbnails of
                first :: _ ->
                    first.url ++ "?w=" ++ String.fromInt first.width ++ "&h=" ++ String.fromInt first.height

                [] ->
                    "https://prod-images.tcm.com/img/global/placeholder-films.png?w=319&h=180"
    in
    Decode.map chooseUrl (Decode.list decodeThumbnail)
