type t = FaunaDB | Localhost | Other(string)

let fromString = t => {
  switch t {
  | "faunadb" => FaunaDB
  | "localhost" => Localhost
  | str => Other(str)
  }
}
let toString = t => {
  switch t {
  | FaunaDB => "faunadb"
  | Localhost => "localhost"
  | Other(str) => str
  }
}
let isOther = t => {
  switch t {
  | Other(_) => true
  | _ => false
  }
}

let initial = FaunaDB
