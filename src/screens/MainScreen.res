module Queries = {
  open Query
  let allCollections = Select(
    //
    ["data"->String]->Array,
    Paginate(
      //
      Collections(None),
      None,
    ),
    None,
  )

  let allIndexes = Select(
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
@react.component
let make = () => {
  // let (allCollections, _) = Client.useQuery(~query=Queries.allCollections, ())

  // Js.log(allCollections)

  let (state, setState) = React.useState(() => "")
  let (queryData, submitQuery) = Client.useFQL(~fql=state, ~fetchOnMount=false, ())

  let responseEl = switch queryData {
  | Idle => React.null
  | Loading => "Loading..."->React.string
  | Success(data) => Js.Json.stringifyWithSpace(data, 2)->React.string
  | Error(err) =>
    switch err {
    | FaunaError(err) => Js.Json.stringifyWithSpace(err, 2)->React.string
    | SyntaxError(str) => str->React.string
    | _ => "Error happened"->React.string
    }
  }
  <div>
    <textarea
      value={state}
      onChange={e => {
        let target = e->ReactEvent.Form.target
        let value = target["value"]
        setState(_ => value)
      }}
    />
    <button onClick={_ => {submitQuery()}}> {"submit query"->React.string} </button>
    <div> <pre> <code> {responseEl} </code> </pre> </div>
  </div>
}
