module AppScreen = {
  @react.component
  let make = () => {
    let query = {
      open Query
      Select(
        //
        ["data"->String]->Array,
        Paginate(
          //
          Collections(None),
          None,
        ),
        None,
      )
    }

    let (collections, _) = Client.useQuery(~query, ())

    Js.log(collections)

    <div> {"hello"->React.string} </div>
  }
}

module WelcomeScreen = {
  @react.component
  let make = () => {
    let appStore = Store.store
    // let appState = appStore.useStore()

    let (_, doPingServer) = Client.useQuery(
      ~query=True,
      ~fetchOnMount=false,
      ~onSuccess=_ => {
        appStore.dispatch(SetPage(App))
      },
      (),
    )

    <div>
      <form
        onSubmit={e => {
          open ReactEvent.Form
          e->preventDefault
          doPingServer()
        }}>
        <label>
          <div> {"Fauna Host"->React.string} </div>
          <select
            value={""}
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
}

@react.component
let make = () => {
  let appStore = Store.store
  let appState = appStore.useStore()

  switch appState.page {
  | Welcome => <WelcomeScreen />
  | App => <AppScreen />
  | _ => "Not implemeneted"->React.string
  }
}

let default = make
