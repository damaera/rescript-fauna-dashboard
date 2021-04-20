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

  switch allCollections {
  | Idle
  | Loading =>
    <div> {"Loading..."->React.string} </div>
  | Success(data) =>
    data
    ->Js.Array2.map(item => {
      <div> {item["@ref"]["id"]} </div>
    })
    ->React.array
  | Error(err) => {
      Js.log(err)
      React.null
    }
  }
}
