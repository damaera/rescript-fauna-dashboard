@val external localStorage: {..} = "localStorage"

@set external persistHost: ('a, string) => unit = "faunaHost"
@set external persistSecret: ('a, string) => unit = "faunaSecret"
@set external persistScreen: ('a, string) => unit = "screen"

let useLoadFromStorage = () => {
  let appStore = Store.store

  React.useEffect0(() => {
    switch localStorage["faunaHost"] {
    | Some(host) => appStore.dispatch(SetHost(Host.fromString(host)))
    | _ => ()
    }
    switch localStorage["faunaSecret"] {
    | Some(secret) => appStore.dispatch(SetSecret(secret))
    | _ => ()
    }
    switch localStorage["screen"] {
    | Some(screen) => appStore.dispatch(SetScreen(Screen.fromString(screen)))
    | _ => ()
    }

    None
  })
}
