module Paginate
    exposing
        ( Pager
        , init
        , update
        , goTo
        , next
        , prev
        , first
        , last
        , page
        , currentPage
        , itemsPerPage
        , totalPages
        , toList
        )

{-| Separate the presentation-domain concerns of pagination from the business-domain of your data.

# The Pager type
@docs Pager

# Constructing and updating
@docs init,update

# Changing pages
@docs goTo, next, prev, first, last

# Paginating
@docs page

# Accessors
@docs currentPage, itemsPerPage, totalPages

# Rendering
@docs toList
-}

import Number.Bounded as Bounded exposing (Bounded)


{-| The `Pager` type holds all of the information necessary to track your pagination without directly affecting your data.
-}
type Pager
    = Pager { itemsPerPage : Int, currentPage : Bounded Int }


{-| Create a new pager.  Pass it the desired number of items per page and total number of items to be paginated.  The current page is always initialized to 1.  The number of items per page will be set to 1 if you provide 0 or a negative number.

      init 10 (List.length allMyThings)
          |> currentPage
      -- (equals 1)

-}
init : Int -> Int -> Pager
init itemsPerPage totalItems =
    let
        max =
            if totalItems <= 0 then
                1
            else
                ceiling <| toFloat totalItems / toFloat (Basics.max 1 itemsPerPage)
    in
        Pager { itemsPerPage = Basics.max 1 itemsPerPage, currentPage = Bounded.between 1 max }


{-| Update a pager by passing in the new items per page and new total number of items to be paginated.  You would want to do this any time the number of items in the data you are paginating changes or if you want a different page size.  If the newly calculated number of pages is less than the current page, the current page will be set to the new last page.
-}
update : Int -> Int -> Pager -> Pager
update newItemsPerPage newTotalItems (Pager { currentPage }) =
    init newItemsPerPage newTotalItems
        |> goTo (Bounded.value currentPage)


{-| Set the current page directly.  If the specified page is "out of bounds" of the pager, it will be set to the first or last page accordingly.
-}
goTo : Int -> Pager -> Pager
goTo i (Pager { itemsPerPage, currentPage }) =
    Bounded.set i (currentPage)
        |> \newCurrentPage ->
            Pager { itemsPerPage = itemsPerPage, currentPage = newCurrentPage }


{-| Go to the next page.  If you already on the last page, next has no effect.
-}
next : Pager -> Pager
next ((Pager { currentPage }) as pager) =
    goTo (Bounded.value currentPage + 1) pager


{-| Go to the previous page.  If you already on the first page, prev has no effect.
-}
prev : Pager -> Pager
prev ((Pager { currentPage }) as pager) =
    goTo (Bounded.value currentPage - 1) pager


{-| Go to the first page.
-}
first : Pager -> Pager
first pager =
    goTo 1 pager


{-| Go to the last page.
-}
last : Pager -> Pager
last pager =
    goTo (totalPages pager) pager


{-| Provide a list and you will get the "slice" of that list according to the pager's state.

    page (goTo 3 <| init 10 100) (List.range 1 100)
    -- (equals [31, 32, 33, 34, 35, 36, 37, 38, 39 40])

-}
page : Pager -> List a -> List a
page (Pager { itemsPerPage, currentPage }) list =
    list
        |> List.drop (Bounded.value currentPage * itemsPerPage)
        |> List.take (itemsPerPage)


{-| Get the current page of the `Pager`
-}
currentPage : Pager -> Int
currentPage (Pager { currentPage }) =
    Bounded.value currentPage


{-| Get the number of items per page
-}
itemsPerPage : Pager -> Int
itemsPerPage (Pager { itemsPerPage }) =
    itemsPerPage


{-| Get the total number of pages
-}
totalPages : Pager -> Int
totalPages (Pager { currentPage }) =
    Bounded.maxBound currentPage


{-| Turn your pager into a list.  Usually you will do this when you want to render a view for your pager, but it could be a list of anything.  The supplied function is given the current page number being iterated over and whether that page is the current page of the pager or not.

    init 1 3
        |> next
        |> toList ((,))
    -- (equals [(1,False),(2,True),(3,False)])

-}
toList : (Int -> Bool -> a) -> Pager -> List a
toList f (Pager { currentPage }) =
    List.range 1 (Bounded.maxBound currentPage)
        |> List.map (\i -> f i (i == (Bounded.value currentPage)))
