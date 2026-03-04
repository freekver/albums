module Main exposing (Model, Msg, init, main, update, view)
import Browser
import Dict
import Html exposing (Html, div, h1, input, p, span, table, td, text, tr, th, option, select)
import Html.Attributes exposing (style,value)
import Html.Events exposing (onInput,onClick)
import List
import Simple exposing (Status(..))
import Albums exposing (albums,Album)

---- Model ---------------------------------------------------------------------

type SortByOption = SortByTitle | SortByArtist | SortByYear

type Decade = None | Decade Int

type alias Model =
  { sortState : List SortByOption
  , query : String
  , decade : Decade
  , mark : Maybe String
  }

init : Model
init = { sortState = [] , query = "", decade = None, mark = Nothing }

---- View ----------------------------------------------------------------------

view : Model -> Html Msg
view model =
  div []
    [ p [] [ input [ onInput Query ] []
           , select
            [ onInput SelectChanged ]
            [ option [ value "none"    ] [ text "None" ]
            , option [ value "early"   ] [ text "..-59" ]
            , option [ value "dec1960" ] [ text "60-69" ]
            , option [ value "dec1970" ] [ text "70-79" ]
            , option [ value "dec1980" ] [ text "80-89" ]
            , option [ value "dec1990" ] [ text "90-99" ]
            , option [ value "dec2000" ] [ text "00-09" ]
            , option [ value "dec2010" ] [ text "10-19" ]
            , option [ value "dec2020" ] [ text "20-.." ]
            ]
           , select
            [ onInput MarkSelectChanged ]
            [ option [ value "none"    ] [ text "None" ]
            , option [ value "l" ]       [ text "Live" ]
            , option [ value "g" ]       [ text "Gigant" ]
            , option [ value "j" ]       [ text "Jazz" ]
            , option [ value "s" ]       [ text "Soul" ]
            , option [ value "x" ]       [ text "Marked" ]
            ]
           ]
    , p [] []
    , albumsTable model.sortState model.decade model.mark model.query
    ]

albumsTable : List SortByOption -> Decade -> Maybe String -> String -> Html Msg
albumsTable sb dec m q = Albums.albums |> List.filter (applyFilter q dec m) |> do_sorting sb |> showTable

showTable : List Album -> Html Msg
showTable ts =
  table []
    [ Html.thead [] [ showHead ]
    , Html.tbody [] (List.map showTrend ts)
    ]

showHead : Html Msg
showHead = header_row [ div [onClick (MsgSortBy SortByArtist)] [text "Artiest"], div [onClick (MsgSortBy SortByTitle)] [text "Titel"], div [onClick (MsgSortBy SortByYear)] [text "Year"] ]

showTrend : Album -> Html msg
showTrend album =
  row [ text album.artist, text album.title, text (String.fromInt album.year) ]


do_sorting : List SortByOption -> List Album -> List Album
do_sorting sb l =
  case l of 
    [] -> List.sortBy .year l
    _  -> List.foldr do_single_sort l sb

do_single_sort sb l = 
  case sb of
    SortByTitle  -> List.sortBy .title l
    SortByArtist -> List.sortBy .artist l
    SortByYear   -> List.sortBy .year l

applyFilter : String -> Decade -> Maybe String -> Album -> Bool
applyFilter q dec m album =
  let
    a_ = String.toLower album.artist
    t_ = String.toLower album.title
    q_ = String.toLower q
  in
  if not (correctDecade dec album.year) then
    False
  else if not (matchesMark m album.mark) then
    False
  else if String.isEmpty q_ then
    True
  else
    String.contains q_ a_ || String.contains q_ t_

correctDecade dec year =
  case dec of
    None        -> True
    Decade 1900 -> year <= 1959
    Decade d    -> d <= year && year < d + 10

matchesMark m am =
  case (m,am) of
    (Nothing,_) -> True
    (_,Nothing) -> False
    (Just m_,Just am_) -> String.contains m_ am_


---- Update --------------------------------------------------------------------

type Msg
  = MsgSortBy SortByOption
  | SelectChanged String
  | MarkSelectChanged String
  | Query String

update : Msg -> Model -> Model
update msg model =
  case msg of
    MsgSortBy sb -> { model | sortState = sb :: (List.filter (\sb_ -> sb_ /= sb) model.sortState) }
    Query str -> { model | query = str }
    SelectChanged "early"   -> { model | decade = Decade 1900 }
    SelectChanged "dec1960" -> { model | decade = Decade 1960 }
    SelectChanged "dec1970" -> { model | decade = Decade 1970 }
    SelectChanged "dec1980" -> { model | decade = Decade 1980 }
    SelectChanged "dec1990" -> { model | decade = Decade 1990 }
    SelectChanged "dec2000" -> { model | decade = Decade 2000 }
    SelectChanged "dec2010" -> { model | decade = Decade 2010 }
    SelectChanged "dec2020" -> { model | decade = Decade 2020 }
    SelectChanged _         -> { model | decade = None }
    MarkSelectChanged "none" -> { model | mark = Nothing }
    MarkSelectChanged s     -> { model  | mark = Just s }



---- Main ----------------------------------------------------------------------

main : Program () Model Msg
main =
  Browser.sandbox
    { init = init
    , view = view
    , update = update
    }

---- Helpers -------------------------------------------------------------------

empty = div [] []

cell x = td [] [ x ]

header_cell x = th [] [ x ]

header_row = List.map header_cell >> tr []

row = List.map cell >> tr []

color c x =
  span [ style "color" c ] [ text x ]

green = color "green"

red = color "red"

blue = color "blue"

gray = color "gray"

unwords =
  List.intersperse " " >> String.concat
