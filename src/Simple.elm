module Simple exposing
  ( Artist
  , Entry
  , Place
  , Status(..)
  , Title
  , find
  , status
  , trend
  , trends
  )
import Html
import List.Extra as List



---- Types ---------------------------------------------------------------------

type alias Place =
  Maybe Int

type alias Artist =
  String

type alias Title =
  String

type alias Entry =
  ( Artist, Title )

---- Finding an entry ----------------------------------------------------------

find : Entry -> List Entry -> Place
find e entries =
  let
    helper p list =
      case list of
        [] -> Nothing
        x :: xs ->
          if x == e then
            Just p
          else
            helper (p + 1) xs
  in
  helper 1 entries

---- Deterimining the status ---------------------------------------------------

type Status
  = In Int
  | Out
  | Moved Int
  | Steady

status : Place -> Place -> Status
status old new =
  case ( old, new ) of
    ( Nothing, Nothing ) -> Steady
    ( Just _, Nothing ) -> Out
    ( Nothing, Just n ) -> In n
    ( Just p1, Just p2 ) ->
      if p1 == p2 then
        Steady
      else
        Moved (p1 - p2)

---- Determining the trend -----------------------------------------------------

trend : Entry -> ( List Entry, List Entry ) -> Status
trend e ( y1, y2 ) =
  status (find e y1) (find e y2)

---- Finding all trends --------------------------------------------------------

trends : ( List Entry, List Entry ) -> List ( Entry, Status )
trends ( y1, y2 ) =
  (y1 ++ y2)
    |> List.unique
    |> List.map (\e -> ( e, trend e ( y1, y2 ) ))
