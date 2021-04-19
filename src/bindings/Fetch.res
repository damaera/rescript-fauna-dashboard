@deriving(abstract)
type options<'headers, 'body> = {
  @optional
  method: [
    | #GET
    | #POST
    | #DELETE
    | #PATCH
    | #PUT
    | #HEAD
    | #OPTIONS
    | #CONNECT
  ],
  @optional headers: 'headers,
  @optional body: 'body,
}

@val("fetch") external fetchInit: (string, options<'headers, 'body>) => Js.Promise.t<'a> = ""

let make = (~url, ~method=#GET, ~headers=?, ~body=?, ()) => {
  fetchInit(url, options(~method, ~headers, ~body, ()))
}
