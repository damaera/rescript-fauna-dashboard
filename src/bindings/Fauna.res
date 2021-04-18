module Query = FaunaQuery

module RequestResult = {
  type t = {
    endTime: int,
    method: [#GET | #POST | #PUT | #PATCH | #DELETE],
    path: string,
    query: Js.Json.t,
    requestContent: Js.Json.t,
    requestRaw: string,
    responseContent: Js.Json.t,
    responseHeaders: Js.Dict.t<string>,
    responseRaw: string,
    startTime: int,
    statusCode: int,
  }
}

module Client = {
  type t

  @deriving(abstract)
  type options<'fetch> = {
    @optional domain: string,
    @optional headers: Js.Dict.t<string>,
    @optional scheme: [#http | #https],
    @optional port: string,
    @optional secret: string,
    @optional timeout: int,
    @optional observer: RequestResult.t => unit,
    @optional keepAlive: bool,
    @optional fetch: 'fetch,
    @optional queryTimeout: int,
    @optional http2SessionIdleTime: int,
  }

  @module("faunadb") @new external init: options<'a> => t = "Client"

  @send external query: (t, 'a) => Js.Promise.t<'b> = "query"
}

// let client = Client.init(Client.options())
// let cols = client->Client.query(Query.abort())
