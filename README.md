# elm-pagination

Simple and robust pagination in elm.

Encapsulate your pagination concerns from the rest of your app.

## Example usage

Below is a fully featured example ([demo](https://jschomay.github.io/elm-paginate/) | [source](https://github.com/jschomay/elm-paginate/blob/master/src/Example.elm)).


    module Example exposing (main)

    import Paginate exposing (..)
    import Html exposing (..)
    import Html.Attributes exposing (..)
    import Html.Events exposing (..)


    type alias Model =
        { things : PaginatedList String
        , reversed : Bool
        , query : String
        }


    type Msg
        = Next
        | Prev
        | First
        | Last
        | GoTo Int
        | ChangePageSize String
        | DeleteItem String
        | AddItem
        | Reverse
        | Find String


    main : Program Never Model Msg
    main =
        Html.beginnerProgram
            { model = init
            , view = filterAndSortThings >> view
            , update = update
            }


    init : Model
    init =
        { things = Paginate.fromList 10 <| List.map (toString >> (++) "item ") <| List.range 1 37
        , reversed = False
        , query = ""
        }


    update : Msg -> Model -> Model
    update msg model =
        case msg of
            GoTo index ->
                { model | things = Paginate.goTo index model.things }

            Next ->
                { model | things = Paginate.next model.things }

            Prev ->
                { model | things = Paginate.prev model.things }

            First ->
                { model | things = Paginate.first model.things }

            Last ->
                { model | things = Paginate.last model.things }

            ChangePageSize size ->
                let
                    sizeAsInt =
                        Result.withDefault 10 <| String.toInt size
                in
                    { model | things = Paginate.changeItemsPerPage sizeAsInt model.things }

            DeleteItem item ->
                let
                    removeItem =
                        List.filter ((/=) item)
                in
                    { model | things = Paginate.map removeItem model.things }

            AddItem ->
                let
                    addItem existing =
                        existing ++ (List.repeat 1 "new item")
                in
                    { model | things = Paginate.map addItem model.things }

            Reverse ->
                { model | reversed = not model.reversed }

            Find query ->
                { model | query = query }


    filterAndSortThings : Model -> PaginatedList String
    filterAndSortThings model =
        let
            sort =
                if model.reversed then
                    List.reverse
                else
                    identity

            filter =
                if model.query == "" then
                    identity
                else
                    List.filter (\thing -> String.contains model.query (toString thing))
        in
            Paginate.map (filter >> sort) model.things


    view : PaginatedList String -> Html Msg
    view filteredSortedThings =
        let
            displayInfoView =
                div []
                    [ div []
                        [ text <|
                            String.join " " <|
                                [ "showing"
                                , (toString <| List.length <| Paginate.page filteredSortedThings)
                                , "of"
                                , (toString <| Paginate.length filteredSortedThings)
                                , "items"
                                ]
                        , u [ onClick <| AddItem, style [ ( "cursor", "pointer" ) ] ] [ text " (add more!)" ]
                        ]
                    , text <|
                        String.join " "
                            [ "page"
                            , toString <| Paginate.currentPage filteredSortedThings
                            , "of"
                            , toString <| Paginate.totalPages filteredSortedThings
                            ]
                    , div []
                        [ text <|
                            String.join " "
                                [ "including"
                                , Paginate.foldMap
                                    (List.filter (String.contains "new item") >> List.length >> toString)
                                    filteredSortedThings
                                , "new items"
                                ]
                        ]
                    ]

            itemView item =
                li []
                    [ span [] [ text item ]
                    , u [ onClick <| DeleteItem item, style [ ( "cursor", "pointer" ) ] ] [ text " (delete)" ]
                    ]

            itemsPerPageSelector =
                div []
                    [ text "show"
                    , select [ onInput ChangePageSize ]
                        [ option [ value "10" ] [ text "10" ]
                        , option [ value "20" ] [ text "20" ]
                        , option [ value "30" ] [ text "30" ]
                        ]
                    , text "items per page"
                    ]

            prevButtons =
                [ button [ onClick First, disabled <| Paginate.isFirst filteredSortedThings ] [ text "<<" ]
                , button [ onClick Prev, disabled <| Paginate.isFirst filteredSortedThings ] [ text "<" ]
                ]

            nextButtons =
                [ button [ onClick Next, disabled <| Paginate.isLast filteredSortedThings ] [ text ">" ]
                , button [ onClick Last, disabled <| Paginate.isLast filteredSortedThings ] [ text ">>" ]
                ]

            pagerButtonView index isActive =
                button
                    [ style
                        [ ( "font-weight"
                          , if isActive then
                                "bold"
                            else
                                "normal"
                          )
                        ]
                    , onClick <| GoTo index
                    ]
                    [ text <| toString index ]
        in
            div [] <|
                [ displayInfoView
                , itemsPerPageSelector
                , button [ onClick Reverse ] [ text "Reverse list" ]
                , input [ placeholder "Search...", onInput Find ] []
                , ul [] (List.map itemView <| Paginate.page filteredSortedThings)
                ]
                    ++ prevButtons
                    ++ [ span [] <| Paginate.pager pagerButtonView filteredSortedThings ]
                    ++ nextButtons
