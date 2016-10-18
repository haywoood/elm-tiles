import Html exposing (..)
import Html.App as App
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List exposing (map, indexedMap)
import List.Extra exposing (updateAt)
import Debug exposing (log)
import Maybe exposing (Maybe(..))


type alias Color =
  { backgroundColor : String
  , color : String
  }


blankTile : Color
blankTile = Color "white" "red"


legendTiles : TileRow
legendTiles =
  [ Color "#444" "white"
  , Color "blue" "white"
  , Color "cyan" "blue"
  , Color "red" "white"
  , Color "pink" "white"
  , Color "yellow" "red"
  , Color "#64c7cc" "cyan"
  , Color "#00a64d" "#75f0c3"
  , Color "#f5008b" "#ffdbbf"
  , Color "#0469bd" "#75d2fa"
  , Color "#fcf000" "#d60000"
  , Color "#010103" "#fa8e66"
  , Color "#7a2c02" "#fff3e6"
  , Color "white" "red"
  , Color "#f5989c" "#963e03"
  , Color "#ed1c23" "#fff780"
  , Color "#f7f7f7" "#009e4c"
  , Color "#e04696" "#9c2c4b"
  , blankTile
  ]


type alias TileRow = List Color


type alias Model =
  { tiles : List Color
  , grid : List TileRow
  , dragging : Bool
  , activeTile : Maybe Color
  }

createTileRow : XCoordinate -> YCoordinate -> TileRow
createTileRow numItems n =
  map (\n -> blankTile) [0..numItems]

createGrid : XCoordinate -> YCoordinate -> List TileRow
createGrid x y =
  map (createTileRow x) [0..y]


model : Model
model = Model legendTiles
              (createGrid 25 17)
              False
              Nothing


type Msg = SelectLegendColor Color
         | UpdateTile TileCoordinates
         | TileHovered TileCoordinates
         | SetDragging Bool
         | NoOp


type alias XCoordinate = Int
type alias YCoordinate = Int
type alias TileCoordinates = (YCoordinate, XCoordinate)


updateTileInRow : XCoordinate -> Maybe Color -> TileRow -> TileRow
updateTileInRow x color tileRow =
 let
   ccolor = Maybe.withDefault (Color "pink" "white") color
 in
   case (updateAt x (\_ -> ccolor) tileRow) of
     Nothing -> tileRow
     Just newTileRow -> newTileRow


update : Msg -> Model -> Model
update msg model =
  case msg of
    SelectLegendColor color ->
      { model | activeTile = (Just color) }

    TileHovered (y, x) ->
      if model.dragging then
        case (updateAt y (updateTileInRow x model.activeTile) model.grid) of
          Nothing ->
            model
          Just grid ->
            { model | grid = grid }
      else
        model

    SetDragging val ->
      { model | dragging = val }

    UpdateTile (x, y) ->
      let
        newModel = update (SetDragging True) model
      in
        case (updateAt x (updateTileInRow y model.activeTile) model.grid) of
          Nothing ->
            newModel
          Just grid ->
            { newModel | grid = grid }

    NoOp ->
      model


createTile : Msg -> Msg -> Color -> Html Msg
createTile tileAction tileHoverAction color =
  div [ style [ ("width", "17px")
              , ("height", "31px")
              , ("position", "relative")
              , ("backgroundColor", color.backgroundColor)
              ]
      , onMouseDown tileAction
      , onMouseUp (SetDragging False)
      , onMouseOver tileHoverAction
      ]
      [ div [ style [ ("width", "4px")
                    , ("height", "4px")
                    , ("borderRadius", "2px")
                    , ("position", "absolute")
                    , ("bottom", "7px")
                    , ("left", "7px")
                    , ("backgroundColor", color.color)
                    ]
            ]
            []
      ]

tileRowCreateTile : (XCoordinate -> Msg) -> YCoordinate -> XCoordinate -> Color -> Html Msg
tileRowCreateTile tileAction y x tile =
  createTile (tileAction x) (TileHovered (y, x)) tile

renderTileRow : (YCoordinate -> XCoordinate -> Msg) -> YCoordinate -> TileRow -> Html Msg
renderTileRow tileAction y tileRow =
  div [ style [ ("display", "flex")
              ]
      ]
      (indexedMap (tileRowCreateTile (tileAction y) y) tileRow)


updateTileAction : YCoordinate -> XCoordinate -> Msg
updateTileAction y x =
  UpdateTile (y, x)


view : Model -> Html Msg
view model =
  div [ style [ ("width", "100%")
              , ("height", "100%")
              , ("backgroundColor", "#eee")
              , ("display", "flex")
              , ("justify-content", "center")
              , ("padding-top", "50px")
              ]
      ]
      [ div [ style [ ("width", "200px")
                    , ("display", "flex")
                    , ("flex-wrap", "wrap")
                    , ("height", "100px")
                    , ("margin-right", "25px")
                    , ("margin-left", "-225px")
                    ]
            ]
            (map (\color -> createTile (SelectLegendColor color) NoOp color) model.tiles)

      , div [ style [ ("display", "flex")
                    , ("flex-direction", "column")
                    ]
            ]
            (indexedMap (renderTileRow updateTileAction) model.grid)
      ]

main =
  App.beginnerProgram { model = model, update = update, view = view }
