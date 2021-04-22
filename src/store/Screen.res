type t = Welcome | Main

let initial = Welcome

let toString = t => {
  switch t {
  | Welcome => "welcome"
  | Main => "main"
  }
}

let fromString = t => {
  switch t {
  | "welcome" => Welcome
  | "main" => Main
  | _ => initial
  }
}
