%%raw(`import styles from "./WelcomeScreen.module.css"`)
@val external styles: {..} = "styles"

open Storage

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
      localStorage->persistScreen(Screen.toString(Main))
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

  <Page>
    <div className={styles["container"]}>
      <h3> {"FaunaDB dashboard"->React.string} </h3>
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
  </Page>
}
