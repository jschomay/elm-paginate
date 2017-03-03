module Tests exposing (all)

import Paginate exposing (..)
import ElmTest.Extra exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)


all : Test
all =
    describe "Paginate"
        [ describe "init"
            [ fuzz2 int int "always starts the pager at 1" <|
                \itemsPerPage totalItems ->
                    Expect.equal 1 (currentPage <| init itemsPerPage totalItems)
            , fuzz2 int int "itemsPerPage is never less than 1" <|
                \itemsPerPage totalItems ->
                    Expect.true "itemsPerPage went under 1" (1 <= (Paginate.itemsPerPage <| init itemsPerPage totalItems))
            , fuzz2 int int "totalPages is never less than 1" <|
                \itemsPerPage totalItems ->
                    Expect.true "total pages went under 1" (1 <= (Paginate.totalPages <| init itemsPerPage totalItems))
            ]
        , describe "update"
            [ test "recalculates the total number of pages when the total number of items changes" <|
                \_ ->
                    Expect.equal 1 (totalPages <| update 2 2 <| init 2 10)
            , test "recalculates the total number of pages when the items per page changes" <|
                \_ ->
                    Expect.equal 2 (totalPages <| update 5 10 <| init 2 10)
            , test "maintains the current page when the total number of pages is greater than the current page" <|
                \_ ->
                    Expect.equal 3 (currentPage <| update 2 20 <| goTo 3 <| init 2 10)
            , fuzz2 int int "current page is never goes out of bounds" <|
                \itemsPerPage totalItems ->
                    Expect.true "current page went out of bounds" <|
                        (\p -> currentPage p <= totalPages p && currentPage p >= 1) <|
                            (update itemsPerPage totalItems <| goTo 3 <| init 2 10)
            ]
        , describe "next"
            [ test "goes to the next page when before the last page" <|
                \_ ->
                    Expect.equal 2 (currentPage <| next <| init 2 10)
            , test "stays on last page when on last page" <|
                \_ ->
                    Expect.equal 5 (currentPage <| next <| goTo 5 <| init 2 10)
            ]
        , describe "prev"
            [ test "goes to the previous page when after the first page" <|
                \_ ->
                    Expect.equal 2 (currentPage <| prev <| goTo 3 <| init 2 10)
            , test "stays on first page when on first page" <|
                \_ ->
                    Expect.equal 1 (currentPage <| prev <| init 2 10)
            ]
        , describe "first"
            [ test "goes to the first page" <|
                \_ ->
                    Expect.equal 1 (currentPage <| first <| goTo 3 <| init 2 10)
            ]
        , describe "last"
            [ test "goes to the last page" <|
                \_ ->
                    Expect.equal 5 (currentPage <| last <| init 2 10)
            ]
        , describe "page"
            [ test "gives the correct slice of a list" <|
                \_ ->
                    Expect.equal (List.range 21 30) (page (goTo 3 <| init 10 100) (List.range 1 100))
            , fuzz3 int int int "the slice is never more than the pagination size" <|
                \itemsPerPage totalItems_ pageNum ->
                    let
                        totalItems =
                            Basics.min totalItems_ 100 |> Basics.max 0
                    in
                        Expect.true "page size exceeded itemsPerPage" <|
                            (List.length <|
                                page
                                    (goTo pageNum <| init itemsPerPage totalItems)
                                    (List.range 1 totalItems)
                            )
                                <= totalItems
            ]
        , describe "isFirst"
            [ fuzz int "only true when on first page" <|
                \i ->
                    Expect.equal (i <= 1) (isFirst <| goTo i <| init 2 10)
            ]
        , describe "isLast"
            [ fuzz int "only true when on first page" <|
                \i ->
                    Expect.equal (i >= 5) (isLast <| goTo i <| init 2 10)
            ]
        , describe "toList"
            [ test "maps the pager into a list" <|
                \_ ->
                    Expect.equal [ ( 1, False ), ( 2, True ), ( 3, False ) ]
                        (toList (,) <| goTo 2 <| init 1 3)
            ]
        ]
