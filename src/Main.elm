module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as JD exposing (Decoder, field, string, int, map2)
import Json.Encode as JE

main = Browser.element
  { init = init
  , update = update
  , subscriptions = noSubscriptions
  , view = view
  }

type alias Person =
  { name: String
  , age: Int
  }

type alias EditModel =
  { person: Maybe Person
  , msg: String
  }

type alias ListModel =
  { people: List Person
  , msg: String
  }

type Model
  = EditPage EditModel
  | ListPage ListModel

type Msg
  = FetchPeople
  -- | FetchPerson Int
  | FetchPerson
  | GotPeople (Result Http.Error (List Person))
  | GotPerson (Result Http.Error Person)


init : () -> (Model, Cmd Msg)
init _ = (ListPage (ListModel [] "OK"), Cmd.none)
-- startIt _ = ({ people = [], msg = "OK" }, Cmd.none)

update:  Msg -> Model -> (Model, Cmd Msg)
update message model =
  case (message, model) of
    (FetchPeople, EditPage editModel) -> (ListPage (ListModel [] "OK"), fetchPeople)

    -- (FetchPerson id, ListPage listModel) -> (EditPage (EditModel Nothing "OK"), fetchPerson id)
    (FetchPerson, ListPage listModel) -> (EditPage (EditModel Nothing "OK"), fetchPerson)

    (GotPeople (Ok people), ListPage listModel) ->
      (ListPage {listModel | msg = "OK", people = people}, Cmd.none)

    (GotPerson (Ok person), EditPage editModel) ->
      (EditPage {editModel | msg = "OK", person = Just person}, Cmd.none)

    (GotPeople (Err error), ListPage listModel) ->
      (ListPage {listModel | msg = (printError error)}, Cmd.none)

    (GotPerson (Err error), EditPage editModel) ->
      (EditPage {editModel | msg = (printError error)}, Cmd.none)

    (_, _) -> (model, Cmd.none)


printError: Http.Error -> String
printError error =
  case error of
    Http.BadBody m -> "Bad body "++m
    Http.BadUrl u -> "Bad URL: "++u
    Http.Timeout -> "Timeout"
    Http.NetworkError -> "Network panic"
    Http.BadStatus i -> "Bad Status: "++(String.fromInt i)



fetchPerson : Cmd Msg
fetchPerson =
  Http.get
    { url = "person.json"
    , expect = Http.expectJson GotPerson personDecoder
    }

{--
fetchPerson : Int -> Cmd Msg
fetchPerson id =
  Http.get
    { url = "person.json/" ++ (String.fromInt id)
    , expect = Http.expectJson GotPerson personDecoder
    }
--}

fetchPeople: Cmd Msg
fetchPeople =
  Http.request
    { method = "GET"
    , headers = []
    , url = "people.json"
    , body = Http.emptyBody
    , expect = Http.expectJson GotPeople personListDecoder
    , timeout = Nothing
    , tracker = Nothing
    }

{--
putPerson: Person -> Cmd Msg
putPerson person =
  Http.request
    { method = "PUT"
    , headers = []
    , url = "..."
    , body = Http.jsonBody (personEncoder person)
    , expect = Http.expectWhatever PersonPutted
    , timeout = Nothing
    , tracker = Nothing
    }
--}

personDecoder: Decoder Person
personDecoder =
  map2 Person
    (field "name" string)
    (field "age" int)

personListDecoder: Decoder (List Person)
personListDecoder =
  JD.list personDecoder

personEncoder: Person -> JE.Value
personEncoder person =
  JE.object
    [ ("name", JE.string person.name)
    , ("age", JE.int person.age)
    ]

noSubscriptions: Model -> Sub Msg
noSubscriptions model =
  Sub.none

view : Model -> Html Msg
view model =
  case model of
    ListPage listModel -> viewList listModel
    EditPage editModel -> viewEdit editModel

viewList : ListModel -> Html Msg
viewList model =
  div []
    (  [ text model.msg
       , hr [] []
       , button [onClick FetchPerson] [text "Get single"]
       , hr [] []
       ]
    ++ (List.map (viewPerson "Look here: ") model.people)
    )

viewEdit : EditModel -> Html Msg
viewEdit model =
  div []
    [ text (getName model.person)
    , button [onClick FetchPeople] [text "Get List"]
    ]

-- viewPerson : Person -> Html Msg

viewPerson : String -> (Person -> Html Msg)
viewPerson prefix person =
  div []
    [ text prefix
    , text person.name
    , text (String.fromInt person.age)
    --, button [onClick FetchPerson person.id] ["List this"]
    ]

getName: Maybe Person -> String
getName mp =
  case mp of
    Just person -> person.name++" age: "++(String.fromInt person.age)
    Nothing ->     "Øhh"
