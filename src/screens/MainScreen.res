%%raw(`import styles from "./MainScreen.module.css"`)
@val external styles: {..} = "styles"

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

  let response = switch queryData {
  | Idle => ""
  | Loading => "Loading..."
  | Success(data) => Js.Json.stringifyWithSpace(data, 2)
  | Error(err) =>
    switch err {
    | FaunaError(err) => Js.Json.stringifyWithSpace(err, 2)
    | SyntaxError(str) => str
    | _ => "Error happened"
    }
  }
  <Page>
    <div className={styles["container"]}>
      <div className={styles["input-container"]}>
        <strong> <label> {"Input"->React.string} </label> </strong>
        <Editor code={state} setCode={setState} />
        <button onClick={_ => {submitQuery()}}> {"submit query"->React.string} </button>
      </div>
      <div className={styles["result-container"]}>
        <strong> <label> {"Results"->React.string} </label> </strong>
        <Editor code={response} setCode={_ => ()} language="json" />
      </div>
    </div>
  </Page>
}
