module %s where

-- MODEL

type alias Model = { }


-- UPDATE

type Action = Reset

update : Action → Model → Model
update action model =
  case action of
    Reset → model
