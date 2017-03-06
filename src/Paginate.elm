module Paginate
    exposing
        ( PaginatedList
        , fromList
        , map
        , changeItemsPerPage
        , goTo
        , next
        , prev
        , first
        , last
        , length
        , currentPage
        , itemsPerPage
        , totalPages
        , isFirst
        , isLast
        , toList
        , query
        , page
        , pager
        )

{-| Encapsulate your pagination concerns from the rest of your app.

# The PaginatedList type
@docs PaginatedList

# Constructing and modifying
@docs fromList, map, changeItemsPerPage

# Changing pages
@docs goTo, next, prev, first, last

# Accessors
@docs length, currentPage, itemsPerPage, totalPages, isFirst, isLast, toList, query

# Rendering
@docs page, pager
-}

import Number.Bounded as Bounded exposing (Bounded)


{-| The `PaginatedList` type wraps your `list` and holds all of the information necessary to track pagination.  It does not modify your list in any way (unless you call `Paginate.map`).
-}
type PaginatedList a
    = PaginatedList { itemsPerPage : Int, currentPage : Bounded Int, items : List a }


{-| Create a new paginated list.  Pass it the desired number of items per page and the list of items to be paginated.  The current page is always initialized to 1.  The minimum number of items per page is 1.  The minimum number of total pages is 1 (even if you pass in an empty list).

      fromList 10 myItems
          |> currentPage
      -- equals 1

-}
fromList : Int -> List a -> PaginatedList a
fromList itemsPerPage items =
    let
        max =
            if List.isEmpty items then
                1
            else
                ceiling <| toFloat (List.length items) / toFloat (Basics.max 1 itemsPerPage)
    in
        PaginatedList
            { itemsPerPage = Basics.max 1 itemsPerPage
            , currentPage = Bounded.between 1 max
            , items = items
            }


{-| Transform the list inside the `PaginatedList` by providing a function to apply to the wrapped list.  This is how you map, filter, sort and update items in the paginated list.  If this function changes the length of the list, the pagination calculations will be updated accordingly.  If the newly calculated number of pages is less than the current page, the current page will be set to the new last page.

    filtered = Paginate.map (List.filter isFavorited) myPaginatedList
    -- the paginated list now only contains the items matching your filter
    -- also the number of pages will update to stay in sync


    sorted = Paginate.map List.sort myPaginatedList


    filteredAndSorted = Paginate.map (List.filter isFavorited >> List.sort) myPaginatedList


    updated = Paginate.map (\items -> updateById id newData items) myPaginatedList

-}
map : (List a -> List a) -> PaginatedList a -> PaginatedList a
map f (PaginatedList { currentPage, itemsPerPage, items }) =
    fromList itemsPerPage (f items)
        |> goTo (Bounded.value currentPage)


{-| Change the paging size.  The total number of pages will be updated accordingly, and the current page will remain unchanged if possible.  If the newly calculated number of pages is less than the current page, the current page will be set to the new last page.  The minimum paging size is 1 item per page.
-}
changeItemsPerPage : Int -> PaginatedList a -> PaginatedList a
changeItemsPerPage newItemsPerPage (PaginatedList { currentPage, items }) =
    fromList newItemsPerPage items
        |> goTo (Bounded.value currentPage)


{-| Set the current page directly.  If the specified page is "out of bounds" of the paginated list, it will be set to the first or last page accordingly.
-}
goTo : Int -> PaginatedList a -> PaginatedList a
goTo i (PaginatedList p) =
    PaginatedList { p | currentPage = Bounded.set i p.currentPage }


{-| Go to the next page.  Has no effect if you are already on the last page.
-}
next : PaginatedList a -> PaginatedList a
next (PaginatedList p) =
    PaginatedList { p | currentPage = Bounded.inc 1 p.currentPage }


{-| Go to the previous page.  Has no effect if you are already on the first page.
-}
prev : PaginatedList a -> PaginatedList a
prev (PaginatedList p) =
    PaginatedList { p | currentPage = Bounded.dec 1 p.currentPage }


{-| Go to the first page.
-}
first : PaginatedList a -> PaginatedList a
first paginatedList =
    goTo 1 paginatedList


{-| Go to the last page.
-}
last : PaginatedList a -> PaginatedList a
last paginatedList =
    goTo (totalPages paginatedList) paginatedList


{-| Useful to conditionally show a "prev" button.
-}
isFirst : PaginatedList a -> Bool
isFirst (PaginatedList { currentPage }) =
    Bounded.value currentPage == 1


{-| Useful to conditionally show a "next" button.
-}
isLast : PaginatedList a -> Bool
isLast (PaginatedList { currentPage }) =
    Bounded.value currentPage == Bounded.maxBound currentPage


{-| Get the length of the wrapped list.
-}
length : PaginatedList a -> Int
length (PaginatedList { items }) =
    List.length items


{-| Get the current page of the `PaginatedList`.
-}
currentPage : PaginatedList a -> Int
currentPage (PaginatedList { currentPage }) =
    Bounded.value currentPage


{-| Get the number of items per page.
-}
itemsPerPage : PaginatedList a -> Int
itemsPerPage (PaginatedList { itemsPerPage }) =
    itemsPerPage


{-| Get the total number of pages.
-}
totalPages : PaginatedList a -> Int
totalPages (PaginatedList { currentPage }) =
    Bounded.maxBound currentPage


{-| Pull out the wrapped list (losing the pagination context).  Consider using `query` instead.
-}
toList : PaginatedList a -> List a
toList (PaginatedList { items }) =
    items


{-| Run an arbitrary function on the wrapped list (losing the pagination context).  Probably most useful for querying the list.  Or if you want to pull certain items out of pagination to making them "sticky."

    hasUnread = query (List.any isUnread) myPaginatedList


    numberOfFavorites = query (List.filter isFavorite >> List.length) myPaginatedList


    nonPaginatedFavorites = query (List.filter isFavorite) myPaginatedList

-}
query : (List a -> b) -> PaginatedList a -> b
query f (PaginatedList { items }) =
    f items


{-| Get the "slice" of the wrapped list for the current page.  Usually you would call this and pass the result on to your view function.

    List.range 1 100 |> fromList 10 |> goTo 3 |> page
    -- equals [ 21, 22, 23, 24, 25, 26, 27, 28, 29 30 ]


    view = page myPaginatedList |> renderCurrentPageItems

-}
page : PaginatedList a -> List a
page (PaginatedList { itemsPerPage, currentPage, items }) =
    items
        |> List.drop ((Bounded.value currentPage - 1) * itemsPerPage)
        |> List.take (itemsPerPage)


{-| Build a "pager" for your paginated list.  Usually you would use this to render the pager view.  The supplied function is given the current page number being iterated over and whether that page is the current page.

    fromList 2 [ 1, 2, 3, 4, 5, 6 ]
        |> next
        |> pager (,)
    -- equals [ (1, False), (2, True), (3, False) ]


    pagerView =
        div [ class "mypager" ] <|
            pager (\pageNum isCurrentPage -> renderPagerButton pageNum isCurrentPage) myPaginatedList

-}
pager : (Int -> Bool -> b) -> PaginatedList a -> List b
pager f (PaginatedList { currentPage }) =
    List.range 1 (Bounded.maxBound currentPage)
        |> List.map (\i -> f i (i == (Bounded.value currentPage)))
