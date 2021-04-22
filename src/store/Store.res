type stateT = {
  screen: Screen.t,
  secret: string,
  host: Host.t,
}
type actionT =
  | SetScreen(Screen.t)
  | SetSecret(string)
  | SetHost(Host.t)

let initialState = {
  screen: Screen.initial,
  secret: "",
  host: Host.initial,
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
