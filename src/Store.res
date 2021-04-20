type hostT = FaunaDB | Localhost | Other(string)

type pageT = Welcome | Auth | App

type stateT = {
  page: pageT,
  secret: string,
  host: hostT,
}
type actionT =
  | SetPage(pageT)
  | SetSecret(string)
  | SetHost(hostT)

let initialState = {
  page: Welcome,
  secret: "",
  host: Other(""),
}

let store = Restorative.createStore(initialState, (state, action) =>
  switch action {
  | SetPage(page) => {
      ...state,
      page: page,
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
