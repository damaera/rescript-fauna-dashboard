type hostT = FaunaDB | Localhost | Other(string)

type screenT = Welcome | Main

type stateT = {
  screen: screenT,
  secret: string,
  host: hostT,
}
type actionT =
  | SetScreen(screenT)
  | SetSecret(string)
  | SetHost(hostT)

let initialState = {
  screen: Welcome,
  secret: "",
  host: FaunaDB,
}

let store = Restorative.createStore(initialState, (state, action) =>
  switch action {
  | SetScreen(screen) => {
      ...state,
      screen: screen,
    }
  | SetSecret(secret) => {
      ...state,
      secret: secret,
    }
  | SetHost(host) => {
      ...state,
      host: host,
    }
  }
)
