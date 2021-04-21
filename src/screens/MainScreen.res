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
  let (allCollections, _) = Client.useQuery(~query=Queries.allCollections, ())

  Js.log(allCollections)

  let (state, setState) = React.useState(() => "")
  let (queryData, submitQuery) = Client.useQuery(~query=state->FQL.make, ~fetchOnMount=false, ())

  Js.log(queryData)
  let responseEl = switch queryData {
  | Idle => React.null
  | Loading => "Loading..."->React.string
  | Success(data) => Js.Json.stringifyWithSpace(data, 2)->React.string
  | Error(err) => {
      Js.log(err)
      "Error happened"->React.string
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
