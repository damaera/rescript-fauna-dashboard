%%raw(`import styles from "./WelcomeScreen.module.css"`)
@val external styles: {..} = "styles"

@val external localStorage: {..} = "localStorage"
@set external persistHost: ('a, string) => unit = "faunaHost"
@set external persistSecret: ('a, string) => unit = "faunaSecret"

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

    None
  })
}

let getFieldValue = e => {
  open ReactEvent.Form
  let target = e->target
  target["value"]
}

@react.component
let make = () => {
  let appStore = Store.store
  let appState = appStore.useStore()

  useLoadFromStorage()

  let (_, doPingServer) = Client.useQuery(
    ~query=True,
    ~fetchOnMount=false,
    ~onSuccess=_ => {
      appStore.dispatch(SetScreen(Main))
    },
    (),
  )

  let onSubmit = e => {
    open ReactEvent.Form
    e->preventDefault

    doPingServer()
    //
    localStorage->persistHost(Host.toString(appState.host))
    localStorage->persistSecret(appState.secret)
  }

  let onChangeHost = {
    e => {
      let val = getFieldValue(e)
      appStore.dispatch(SetHost(Host.fromString(val)))
    }
  }
  let onChangeSecret = {
    e => {
      let val = getFieldValue(e)
      appStore.dispatch(SetSecret(val))
    }
  }

  <div className={styles["container"]}>
    <form onSubmit>
      <label>
        <div> {"Fauna Host"->React.string} </div>
        <select value={Host.toString(appState.host)} onChange=onChangeHost>
          <option value="faunadb"> {"https://db.fauna.com"->React.string} </option>
          <option value="localhost"> {"http://localhost:8443"->React.string} </option>
          <option value=""> {"Other"->React.string} </option>
        </select>
      </label>
      {Host.isOther(appState.host)
        ? <label>
            <div> {"Other host"->React.string} </div>
            <input
              placeholder="Host URL" value={Host.toString(appState.host)} onChange={onChangeHost}
            />
          </label>
        : React.null}
      <label>
        <div> {"Fauna Secret"->React.string} </div>
        <input placeholder="Fauna Secret" value={appState.secret} onChange={onChangeSecret} />
      </label>
      <br />
      <button type_="submit"> {"Submit"->React.string} </button>
    </form>
  </div>
}
