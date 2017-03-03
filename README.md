# elm-pagination

Simple and robust pagination in elm.

Separate the presentation-domain concerns of pagination from the business-domain of your data.

## Example usage

Below is a fully featured example ([demo](https://jschomay.github.io/elm-paginate/) | [source](https://github.com/jschomay/elm-paginate/blob/master/src/Example.elm)).


    module Example exposing (main)

    import Paginate exposing (..)
    import Html exposing (..)
    import Html.Attributes exposing (..)
    import Html.Events exposing (..)


    type alias Model =
        { things : List String
        , pager : Pager
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
        | AddItems
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
        { things = List.range 1 37 |> List.map (toString >> (++) "item ")
        , pager = Paginate.init 10 37
        , reversed = False
        , query = ""
        }


    update : Msg -> Model -> Model
    update msg model =
        case msg of
            GoTo index ->
                { model | pager = Paginate.goTo index model.pager }

            Next ->
                { model | pager = Paginate.next model.pager }

            Prev ->
                { model | pager = Paginate.prev model.pager }

            First ->
                { model | pager = Paginate.first model.pager }

            Last ->
                { model | pager = Paginate.last model.pager }

            ChangePageSize size ->
                { model
                    | pager =
                        Paginate.update
                            (Result.withDefault 10 <| String.toInt size)
                            (List.length model.things)
                            model.pager
                }

            DeleteItem item ->
                let
                    newThings =
                        List.filter ((/=) item) model.things
                in
                    { model
                        | things = newThings
                        , pager =
                            Paginate.update
                                (Paginate.itemsPerPage model.pager)
                                (List.length newThings)
                                model.pager
                    }

            AddItems ->
                let
                    newThings =
                        model.things ++ (List.repeat 1 "new item")
                in
                    { model
                        | things = newThings
                        , pager =
                            Paginate.update
                                (Paginate.itemsPerPage model.pager)
                                (List.length newThings)
                                model.pager
                    }

            Reverse ->
                { model | reversed = not model.reversed }

            Find query ->
                { model | query = query }


    filterAndSortThings : Model -> Model
    filterAndSortThings model =
        let
            filteredAndSortedThings =
                (model.things |> filter |> sort)

            filteredAndSortedPager =
                Paginate.update
                    (Paginate.itemsPerPage model.pager)
                    (List.length filteredAndSortedThings)
                    model.pager

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
            { model | things = filteredAndSortedThings, pager = filteredAndSortedPager }


    view : Model -> Html Msg
    view filteredSortedModel =
        let
            displayingView =
                div []
                    [ text <|
                        String.join " "
                            [ "Showing page"
                            , toString <| Paginate.currentPage filteredSortedModel.pager
                            , "of"
                            , toString <| Paginate.totalPages filteredSortedModel.pager
                            ]
                    ]

            itemView item =
                li []
                    [ span [] [ text item ]
                    , u [ onClick <| DeleteItem item, style [ ( "cursor", "pointer" ) ] ] [ text " (delete)" ]
                    ]

            itemsPerPageView =
                div []
                    [ text "Show"
                    , select [ onInput ChangePageSize ]
                        [ option [ value "10" ] [ text "10" ]
                        , option [ value "20" ] [ text "20" ]
                        , option [ value "30" ] [ text "30" ]
                        ]
                    , text "items per page"
                    ]

            prevButtons =
                [ button [ onClick First, disabled <| Paginate.isFirst filteredSortedModel.pager ] [ text "<<" ]
                , button [ onClick Prev, disabled <| Paginate.isFirst filteredSortedModel.pager ] [ text "<" ]
                ]

            nextButtons =
                [ button [ onClick Next, disabled <| Paginate.isLast filteredSortedModel.pager ] [ text ">" ]
                , button [ onClick Last, disabled <| Paginate.isLast filteredSortedModel.pager ] [ text ">>" ]
                ]

            pagerView index isActive =
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
                [ div []
                    [ text <| (toString <| List.length filteredSortedModel.things) ++ " total items"
                    , u [ onClick <| AddItems, style [ ( "cursor", "pointer" ) ] ] [ text " (add more!)" ]
                    ]
                , displayingView
                , itemsPerPageView
                , button [ onClick Reverse ] [ text "Reverse list" ]
                , input [ placeholder "Search...", onInput Find ] []
                , ul [] (List.map itemView <| Paginate.page filteredSortedModel.pager filteredSortedModel.things)
                ]
                    ++ prevButtons
                    ++ [ span [] <| Paginate.toList pagerView filteredSortedModel.pager ]
                    ++ nextButtons

