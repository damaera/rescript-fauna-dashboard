@val external localStorage: {..} = "localStorage"
@set external persistHost: ('a, string) => unit = "faunaHost"
@set external persistSecret: ('a, string) => unit = "faunaSecret"

@react.component
let make = () => {
  let appStore = Store.store
  let appState = appStore.useStore()

  React.useEffect0(() => {
    switch localStorage["faunaHost"] {
    | Some(host) =>
      appStore.dispatch(
        SetHost(
          switch host {
          | "faunadb" => FaunaDB
          | "localhost" => Localhost
          | str => Other(str)
          },
        ),
      )
    | _ => ()
    }
    switch localStorage["faunaSecret"] {
    | Some(secret) => appStore.dispatch(SetSecret(secret))
    | _ => ()
    }

    None
  })

  let (_, doPingServer) = Client.useQuery(
    ~query=True,
    ~fetchOnMount=false,
    ~onSuccess=_ => {
      appStore.dispatch(SetScreen(Main))
    },
    (),
  )

  <div>
    <form
      onSubmit={e => {
        open ReactEvent.Form
        e->preventDefault
        doPingServer()
        //
        localStorage->persistHost(
          switch appState.host {
          | FaunaDB => "faunadb"
          | Localhost => "localhost"
          | Other(str) => str
          },
        )
        localStorage->persistSecret(appState.secret)
      }}>
      <label>
        <div> {"Fauna Host"->React.string} </div>
        <select
          value={switch appState.host {
          | FaunaDB => "faunadb"
          | Localhost => "localhost"
          | Other(str) => str
          }}
          onChange={e => {
            open ReactEvent.Form
            let target = e->target
            let val = target["value"]
            // appStore.dispatch(Set(val))
            switch val {
            | "faunadb" => FaunaDB
            | "localhost" => Localhost
            | str => Other(str)
            }
            ->SetHost
            ->appStore.dispatch
          }}>
          <option value="faunadb"> {"https://db.fauna.com"->React.string} </option>
          <option value="localhost"> {"http://localhost:8443"->React.string} </option>
        </select>
      </label>
      <label>
        <div> {"Fauna Secret"->React.string} </div>
        <input
          placeholder="Fauna Secret"
          value={appState.secret}
          onChange={e => {
            open ReactEvent.Form
            let target = e->target
            let val = target["value"]
            appStore.dispatch(SetSecret(val))
          }}
        />
      </label>
      <br />
      <button type_="submit"> {"Submit"->React.string} </button>
    </form>
  </div>
}
