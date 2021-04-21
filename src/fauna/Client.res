external fromJson: Js.Json.t => 'a = "%identity"
type errorT<'a> = PromiseError(Js.Promise.error) | FaunaError('a) | SyntaxError(string)

let exec = (~faunaHost, ~secret, ~query, ~onSuccess, ~onError) => {
  open Fetch
  open Js.Promise
  fetchWithInit(
    faunaHost,
    RequestInit.make(
      ~method_=Post,
      ~headers=HeadersInit.makeWithArray([
        ("Content-Type", "application/json"),
        ("Authorization", `Bearer ${secret}`),
      ]),
      ~body=BodyInit.make(query->Query.make),
      (),
    ),
  )
  |> then_(Fetch.Response.json)
  |> then_(data => {
    let responseData = data->fromJson
    try {
      if responseData["errors"] {
        onError(FaunaError(responseData["errors"]))
      } else {
        onSuccess(responseData["resource"])
      }
    } catch {
    | err => Js.log(err)
    }

    ()->resolve
  })
  |> catch(err => {
    onError(PromiseError(err))
    ()->resolve
  })
  |> ignore
}

type stateT<'data, 'err> = Idle | Loading | Success('data) | Error('err)

let useQuery = (~query, ~fetchOnMount=true, ~onSuccess=?, ~onError=?, ()) => {
  let appStore = Store.store
  let appState = appStore.useStore()

  let (state, setState) = React.useState(() => Idle)

  let execQuery = () =>
    exec(
      ~faunaHost={
        switch appState.host {
        | FaunaDB => "https://db.fauna.com"
        | Localhost => `http://localhost:8443`
        | Other(string) => string
        }
      },
      ~secret=appState.secret,
      ~query,
      ~onSuccess=data => {
        switch onSuccess {
        | Some(onSuccess) => onSuccess(data)
        | None => ()
        }
        setState(_ => Success(data))
      },
      ~onError=err => {
        switch onError {
        | Some(onError) => onError(err)
        | None => ()
        }
        setState(_ => Error(err))
      },
    )

  React.useEffect0(() => {
    if fetchOnMount {
      execQuery()
    }

    None
  })

  (state, execQuery)
}

let useFQL = (~fql, ~fetchOnMount=true, ~onSuccess=?, ~onError=?, ()) => {
  let appStore = Store.store
  let appState = appStore.useStore()

  let (state, setState) = React.useState(() => Idle)

  let query = try {
    fql->FQL.make
  } catch {
  | FQL.SyntaxError(str) =>
    Js.log(str)
    switch onError {
    | Some(onError) => {
        onError(SyntaxError(str))
        Null
      }
    | None => Null
    }
  }

  let execQuery = () =>
    exec(
      ~faunaHost={
        switch appState.host {
        | FaunaDB => "https://db.fauna.com"
        | Localhost => `http://localhost:8443`
        | Other(string) => string
        }
      },
      ~secret=appState.secret,
      ~query,
      ~onSuccess=data => {
        switch onSuccess {
        | Some(onSuccess) => onSuccess(data)
        | None => ()
        }
        setState(_ => Success(data))
      },
      ~onError=err => {
        switch onError {
        | Some(onError) => onError(err)
        | None => ()
        }
        setState(_ => Error(err))
      },
    )

  React.useEffect0(() => {
    if fetchOnMount {
      execQuery()
    }

    None
  })

  (state, execQuery)
}
