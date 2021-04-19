@react.component
let make = () => {
  React.useEffect0(() => {
    let query = {
      open Query
      Collections(None)
    }

    Client.query(
      ~faunaHost="https://db.fauna.com",
      ~secret="fnAEG3BiuIACCji7JhhL_ZD_zrncyYsKu20RxFNd",
      ~query,
      (),
    )->ignore

    None
  })

  React.null
}

let default = make
