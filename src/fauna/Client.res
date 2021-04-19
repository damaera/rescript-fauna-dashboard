let query = (~faunaHost, ~secret, ~query) => {
  Fetch.make(
    ~url=faunaHost,
    ~method=#POST,
    ~headers={
      "Content-Type": "application/json",
      "Authorization": `Bearer ${secret}`,
    },
    ~body=query->Query.make,
  )
}
