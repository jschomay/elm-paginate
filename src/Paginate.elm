module Paginate exposing
    ( PaginatedList
    , fromList, map, changeItemsPerPage
    , goTo, next, prev, first, last
    , page, allItems, foldMap
    , pager, length, currentPage, itemsPerPage, totalPages, isFirst, isLast
    , PagerOptions, elidedPager
    )

{-| Pagination for `List`'s.


# The PaginatedList type

@docs PaginatedList


# Constructing and modifying

@docs fromList, map, changeItemsPerPage


# Changing pages

@docs goTo, next, prev, first, last


# Retrieving items

@docs page, allItems, foldMap


# Pager helpers

Functions to help build a "pager" and useful paging data

@docs pager, PagerOptions. elidedPager, length, currentPage, itemsPerPage, totalPages, isFirst, isLast

-}

import Paginate.Custom


{-| The `PaginatedList` type wraps your `list` and holds all of the information necessary to track pagination. It does not modify your list in any way (unless you call `Paginate.map`).
-}
type alias PaginatedList a =
    Paginate.Custom.Paginated (List a)


{-| Create a new paginated list. Pass it the desired number of items per page and the list of items to be paginated. The current page is always initialized to 1. The minimum number of items per page is 1. The minimum number of total pages is 1 (even if you pass in an empty list).

      fromList 10 myItems
          |> currentPage
      -- equals 1

-}
fromList : Int -> List a -> PaginatedList a
fromList a b =
    Paginate.Custom.init List.length a b


{-| Transform the list inside the `PaginatedList` by providing a function to apply to the wrapped list. This is how you map, filter, sort and update items in the paginated list. If this function changes the length of the list, the pagination calculations will be updated accordingly. If the newly calculated number of pages is less than the current page, the current page will be set to the new last page.

    filtered =
        Paginate.map (List.filter isFavorited) myPaginatedList


    -- the paginated list now only contains the items matching your filter
    -- also the number of pages will update to stay in sync
    sorted =
        Paginate.map List.sort myPaginatedList

    filteredAndSorted =
        Paginate.map (List.filter isFavorited >> List.sort) myPaginatedList

    updated =
        Paginate.map (\items -> updateById id newData items) myPaginatedList

-}
map : (List a -> List a) -> PaginatedList a -> PaginatedList a
map =
    Paginate.Custom.map List.length


{-| Change the paging size. The total number of pages will be updated accordingly, and the current page will remain unchanged if possible. If the newly calculated number of pages is less than the current page, the current page will be set to the new last page. The minimum paging size is 1 item per page.
-}
changeItemsPerPage : Int -> PaginatedList a -> PaginatedList a
changeItemsPerPage =
    Paginate.Custom.changeItemsPerPage List.length


{-| Set the current page directly. If the specified page is "out of bounds" of the paginated list, it will be set to the first or last page accordingly.
-}
goTo : Int -> PaginatedList a -> PaginatedList a
goTo =
    Paginate.Custom.goTo


{-| Go to the next page. Has no effect if you are already on the last page.
-}
next : PaginatedList a -> PaginatedList a
next =
    Paginate.Custom.next


{-| Go to the previous page. Has no effect if you are already on the first page.
-}
prev : PaginatedList a -> PaginatedList a
prev =
    Paginate.Custom.prev


{-| Go to the first page.
-}
first : PaginatedList a -> PaginatedList a
first =
    Paginate.Custom.first


{-| Go to the last page.
-}
last : PaginatedList a -> PaginatedList a
last =
    Paginate.Custom.last


{-| Useful to conditionally show a "prev" button.
-}
isFirst : PaginatedList a -> Bool
isFirst =
    Paginate.Custom.isFirst


{-| Useful to conditionally show a "next" button.
-}
isLast : PaginatedList a -> Bool
isLast =
    Paginate.Custom.isLast


{-| Get the length of the wrapped list. Equivalent to `foldMap List.length`.
-}
length : PaginatedList a -> Int
length =
    Paginate.Custom.foldMap List.length


{-| Get the current page of the `PaginatedList`.
-}
currentPage : PaginatedList a -> Int
currentPage =
    Paginate.Custom.currentPage


{-| Get the number of items per page.
-}
itemsPerPage : PaginatedList a -> Int
itemsPerPage =
    Paginate.Custom.itemsPerPage


{-| Get the total number of pages.
-}
totalPages : PaginatedList a -> Int
totalPages =
    Paginate.Custom.totalPages


{-| Pull out the wrapped list (losing the pagination context). Equivalent to `foldMap identity`.
-}
allItems : PaginatedList a -> List a
allItems =
    Paginate.Custom.foldMap identity


{-| Remove the pagination context and run a function on the wrapped list. Useful for many needs such as:

    hasUnread =
        foldMap (List.any isUnread) myPaginatedList

    numberOfFavorites =
        foldMap (List.filter isFavorite >> List.length) myPaginatedList

    stickyItems =
        foldMap (List.filter isFavorite) myPaginatedList

-}
foldMap : (List a -> b) -> PaginatedList a -> b
foldMap =
    Paginate.Custom.foldMap


{-| Get the "slice" of the wrapped list for the current page. Usually you would call this and pass the result on to your view function.

    List.range 1 100 |> fromList 10 |> goTo 3 |> page
    -- equals [ 21, 22, 23, 24, 25, 26, 27, 28, 29 30 ]


    view = page myPaginatedList |> renderCurrentPageItems

-}
page : PaginatedList a -> List a
page =
    Paginate.Custom.page <|
        \from to ->
            List.drop from >> List.take (to - from)


{-| Build a "pager" for your paginated list. Usually you would use this to render the pager view. The supplied function is given the current page number being iterated over and whether that page is the current page.

    fromList 2 [ 1, 2, 3, 4, 5, 6 ]
        |> next
        |> pager (,)
    -- equals [ (1, False), (2, True), (3, False) ]


    pagerView =
        div [ class "mypager" ] <|
            pager (\pageNum isCurrentPage -> renderPagerButton pageNum isCurrentPage) myPaginatedList

-}
pager : (Int -> Bool -> b) -> PaginatedList a -> List b
pager =
    Paginate.Custom.pager


{-| `PagerOptions` is used by the `elidedPager` function to configure window sizes and output format. See `elidedPager` for examples of its use. The available options are as follows:


### `innerWindow`

The number of page numbers to display on either side of the current page number. A negative number will be treated as `0`.


### `outerWindow`

The number of page numbers to display at the beginning and end of the page numbers. `0` means that the first and last pages will not be displayed. A negative number will be treated as `0`.


### `pageNumberView`

How to display the page numbers provided by the pager.


### `gapView`

How to represent the gaps between page windows (if there are any).

-}
type alias PagerOptions a =
    Paginate.Custom.PagerOptions a


{-| Builds an "elided" pager, which displays a "gap" placeholder in-between the first and last page(s) and the current page, if there are enough pages to justify doing so. This is useful for large collections where the number of pages might be huge and you don't want to display all of the page numbers at once.

    renderPageNumberString pageNum isCurrentPage =
        if isCurrentPage then
            ">" ++ String.fromInt pageNum ++ "<"

        else
            String.fromInt pageNum

    pagerOptions =
        { innerWindow = 1
        , outerWindow = 1
        , pageNumberView = renderPageNumberString
        , gapView = "..."
        }

    paginatedList = fromList 2 (List.range 20) |> goTo 5

    elidedPager pagerOptions paginatedList
    --> [ "1", "...", "4", ">5<", "6", "...", "10" ]

    elidedPager { pagerOptions | outerWindow = 0 } paginatedList
    --> [ "4", ">5<", "6" ]

-}
elidedPager : PagerOptions b -> PaginatedList a -> List b
elidedPager =
    Paginate.Custom.elidedPager
