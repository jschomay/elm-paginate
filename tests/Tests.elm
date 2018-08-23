module Tests exposing (all)

import Expect
import Fuzz exposing (constant, int, list, string, tuple)
import Paginate exposing (..)
import Test exposing (..)


all : Test
all =
    describe "Paginate"
        [ describe "fromList"
            [ fuzz2 int (list (constant ())) "always starts the pager at 1" <|
                \itemsPerPage items ->
                    Expect.equal 1 (currentPage <| fromList itemsPerPage items)
            , fuzz2 int (list (constant ())) "itemsPerPage is never less than 1" <|
                \itemsPerPage items ->
                    Expect.true "itemsPerPage went under 1" (1 <= (Paginate.itemsPerPage <| fromList itemsPerPage items))
            , fuzz2 int (list (constant ())) "totalPages is never less than 1" <|
                \itemsPerPage items ->
                    Expect.true "total pages went under 1" (1 <= (Paginate.totalPages <| fromList itemsPerPage items))
            ]
        , describe "map"
            [ test "only modifies the list when the total number of pages doesn't change" <|
                \_ ->
                    Expect.equal (next <| fromList 3 [ -1, -2, -3 ]) (map (List.map negate) <| next <| fromList 3 [ 1, 2, 3 ])
            , test "recalculates the total number of pages when the total number of items changes" <|
                \_ ->
                    Expect.equal 1 (totalPages <| map (List.take 2) <| fromList 2 <| List.range 1 10)
            , test "maintains the current page when the total number of pages is greater than the current page" <|
                \_ ->
                    Expect.equal 3 (currentPage <| map (always (List.range 1 20)) <| goTo 3 <| fromList 2 <| List.range 1 10)
            , test "sets the current page to the last page when the total number of pages is greater than the current page" <|
                \_ ->
                    Expect.equal 2 (currentPage <| map (always [ (), (), () ]) <| goTo 5 <| fromList 2 <| List.repeat 10 ())
            , fuzz2 int (list (constant ())) "current page never goes out of bounds" <|
                \itemsPerPage items ->
                    Expect.true "current page went out of bounds" <|
                        (\p -> currentPage p <= totalPages p && currentPage p >= 1) <|
                            (map (always items) <| last <| fromList itemsPerPage <| List.repeat 10 ())
            , describe "obeys the functor laws"
                [ fuzz2 int (list (constant ())) "law of identity" <|
                    \itemsPerPage items ->
                        Expect.equal (identity <| fromList itemsPerPage items) (map identity <| fromList itemsPerPage items)
                , fuzz2 int (list int) "law of composition" <|
                    \itemsPerPage items ->
                        Expect.equal (map (List.map negate >> List.map negate) <| fromList itemsPerPage items)
                            (map (List.map negate) >> map (List.map negate) <| fromList itemsPerPage items)
                ]
            ]
        , describe "changeItemsPerPage"
            [ test "the minimum paging size is 1 item per page." <|
                \_ ->
                    Expect.equal 1 (itemsPerPage <| changeItemsPerPage -99 <| fromList 3 [ 1, 2, 3 ])
            , test "only modifies the items per page when the total number of pages doesn't change" <|
                \_ ->
                    let
                        updated =
                            changeItemsPerPage 3 <| goTo 2 <| fromList 2 [ 1, 2, 3, 4 ]
                    in
                    Expect.equal ( 2, 3, [ 1, 2, 3, 4 ] ) ( currentPage updated, itemsPerPage updated, allItems updated )
            , test "recalculates the total number of pages" <|
                \_ ->
                    Expect.equal 3 (totalPages <| changeItemsPerPage 1 <| fromList 3 [ 1, 2, 3 ])
            , test "maintains the current page when the total number of pages is greater than the current page" <|
                \_ ->
                    Expect.equal 2 (currentPage <| changeItemsPerPage 1 <| goTo 2 <| fromList 2 [ 1, 2, 3 ])
            , test "sets the current page to the last page when the total number of pages is greater than the current page" <|
                \_ ->
                    Expect.equal 1 (currentPage <| changeItemsPerPage 3 <| goTo 2 <| fromList 2 [ 1, 2, 3 ])
            , fuzz2 int (list (constant ())) "current page never goes out of bounds" <|
                \itemsPerPage items ->
                    Expect.true "current page went out of bounds" <|
                        (\p -> currentPage p <= totalPages p && currentPage p >= 1) <|
                            (map (always items) <| last <| fromList itemsPerPage <| List.repeat 10 ())
            ]
        , describe "goTo"
            [ fuzz2 int int "stays within bounds" <|
                \itemsPerPage index ->
                    Expect.true "went out of bounds" <|
                        (\p -> currentPage p <= totalPages p && currentPage p >= 1) <|
                            (goTo index <| fromList itemsPerPage <| List.range 1 30)
            ]
        , describe "next"
            [ test "goes to the next page when before the last page" <|
                \_ ->
                    Expect.equal 2 (currentPage <| next <| fromList 2 <| List.range 1 10)
            , test "stays on last page when on last page" <|
                \_ ->
                    Expect.equal 5 (currentPage <| next <| goTo 5 <| fromList 2 <| List.range 1 10)
            ]
        , describe "prev"
            [ test "goes to the previous page when after the first page" <|
                \_ ->
                    Expect.equal 2 (currentPage <| prev <| goTo 3 <| fromList 2 <| List.range 1 10)
            , test "stays on first page when on first page" <|
                \_ ->
                    Expect.equal 1 (currentPage <| prev <| fromList 2 <| List.range 1 10)
            ]
        , describe "first"
            [ test "goes to the first page" <|
                \_ ->
                    Expect.equal 1 (currentPage <| first <| goTo 3 <| fromList 2 <| List.range 1 10)
            ]
        , describe "last"
            [ test "goes to the last page" <|
                \_ ->
                    Expect.equal 5 (currentPage <| last <| fromList 2 <| List.range 1 10)
            ]
        , describe "length"
            [ fuzz2 int (list (constant ())) "gives the length of the list" <|
                \itemsPerPage items ->
                    Expect.equal (List.length items) (Paginate.length <| fromList itemsPerPage items)
            ]
        , describe "isFirst"
            [ fuzz int "only true when on first page" <|
                \i ->
                    Expect.equal (i <= 1) (isFirst <| goTo i <| fromList 2 <| List.range 1 10)
            ]
        , describe "isLast"
            [ fuzz int "only true when on first page" <|
                \i ->
                    Expect.equal (i >= 5) (isLast <| goTo i <| fromList 2 <| List.range 1 10)
            ]
        , describe "query"
            [ fuzz (list int) "escapes the context" <|
                \items ->
                    Expect.equal (List.head items) (foldMap List.head <| fromList 3 items)
            ]
        , describe "page"
            [ test "gives the correct slice of a list" <|
                \_ ->
                    Expect.equal (List.range 21 30) (page <| goTo 3 <| fromList 10 <| List.range 1 100)
            , fuzz3 int int int "the slice is never more than the pagination size" <|
                \itemsPerPage totalItems_ pageNum ->
                    let
                        totalItems =
                            Basics.min totalItems_ 100 |> Basics.max 0
                    in
                    Expect.true "page size exceeded itemsPerPage" <|
                        (List.length <| page <| goTo pageNum <| fromList itemsPerPage <| List.range 1 totalItems)
                            <= totalItems
            ]
        , describe "pager"
            [ test "makes a pager" <|
                \_ ->
                    Expect.equal [ ( 1, False ), ( 2, True ), ( 3, False ) ]
                        (pager (\a b -> ( a, b )) <| next <| fromList 2 [ 1, 2, 3, 4, 5, 6 ])
            ]
        ]
