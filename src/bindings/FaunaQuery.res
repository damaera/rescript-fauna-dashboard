module Boolean = {
  type t
}
module Null = {
  type t
}
module Number = {
  type t
  external makeInt: int => t = "%identity"
  external makeFloat: float => t = "%identity"
}
module String = {
  type t
  external make: string => t = "%identity"
}
module Error = {
  type t
}
module Ref = {
  type t
}
module Set = {
  type t
}
module Timestamp = {
  type t
}
module Date = {
  type t
}
module Object = {
  type t
  external make: {..} => t = "%identity"
}

type t
external make: 'a => t = "%identity"

@module("faunadb") @scope("query")
external abort: String.t => Error.t = "Abort"

@module("faunadb") @scope("query")
external abs: Number.t => Number.t = "Abs"

@module("faunadb") @scope("query")
external accessProvider: String.t => Ref.t = "AccessProvider"

@module("faunadb") @scope("query")
external accessProviders: unit => Set.t = "AccessProviders"

@module("faunadb") @scope("query")
external acos: Number.t => Number.t = "Acos"

@module("faunadb") @scope("query") @variadic
external add: array<Number.t> => Number.t = "Add"

@module("faunadb") @scope("query")
external all: array<Boolean.t> => Boolean.t = "All"

@module("faunadb") @scope("query")
external and_: (Boolean.t, Boolean.t) => Boolean.t = "And"

@module("faunadb") @scope("query") @variadic
external any: array<Boolean.t> => Boolean.t = "Any"

@module("faunadb") @scope("query")
external append: (t, array<t>) => array<t> = "Append"

@module("faunadb") @scope("query")
external asin: Number.t => Number.t = "Asin"

@module("faunadb") @scope("query")
external at: (Timestamp.t, t) => t = "At"

@module("faunadb") @scope("query")
external atan: Number.t => Number.t = "Atan"

@module("faunadb") @scope("query") @variadic
external bitAnd: array<Number.t> => Number.t = "BitAnd"

@module("faunadb") @scope("query")
external bitNot: Number.t => Number.t = "BitNot"

@module("faunadb") @scope("query") @variadic
external bitOr: array<Number.t> => Number.t = "BitOr"

@module("faunadb") @scope("query") @variadic
external bitXor: array<Number.t> => Number.t = "BitXor"

@module("faunadb") @scope("query")
external call: (t, t) => t = "Call"

@module("faunadb") @scope("query")
external caseFold: String.t => String.t = "Casefold"

@module("faunadb") @scope("query")
external caseFoldWithNormalizer: (
  String.t,
  [#NFKCCaseFold | #NFC | #NFD | #NFKC | #NFKD],
) => String.t = "Casefold"

@module("faunadb") @scope("query")
external ceil: Number.t => Number.t = "Ceil"

@module("faunadb") @scope("query")
external collection: String.t => Ref.t = "Collection"

@module("faunadb") @scope("query")
external collectionWithDatabase: (String.t, Ref.t) => Ref.t = "Collection"

@module("faunadb") @scope("query")
external collections: unit => Set.t = "Collections"

@module("faunadb") @scope("query")
external collectionsWithDatabase: (unit, Ref.t) => Set.t = "Collections"

@module("faunadb") @scope("query")
external concat: (array<String.t>, String.t) => String.t = "Concat"

@module("faunadb") @scope("query")
external containsField: (String.t, Object.t) => Boolean.t = "ContainsField"

@module("faunadb") @scope("query")
external containsPath: (array<t>, t) => Boolean.t = "ContainsPath"

@module("faunadb") @scope("query")
external containsStr: (String.t, String.t) => Boolean.t = "ContainsStr"

@module("faunadb") @scope("query")
external containsStrRegex: (String.t, String.t) => Boolean.t = "ContainsStrRegex"

@module("faunadb") @scope("query")
external containsValue: (t, Object.t) => Boolean.t = "ContainsValue"

@module("faunadb") @scope("query")
external cos: Number.t => Number.t = "Cos"

@module("faunadb") @scope("query")
external cosh: Number.t => Number.t = "Cosh"

@module("faunadb") @scope("query")
external countArray: array<t> => Number.t = "Count"

@module("faunadb") @scope("query")
external countSet: Set.t => Number.t = "Count"

@deriving(abstract)
type createParamObject = {
  data: Object.t,
  @optional creadentials: Object.t,
  @optional delegates: Object.t,
  @optional ttl: Timestamp.t,
}
@module("faunadb") @scope("query")
external create: (Ref.t, createParamObject) => Object.t = "Create"

@deriving(abstract)
type createAccessProviderParamObject = {
  name: String.t,
  issuer: String.t,
  @as("jwks_uri") jwksUri: String.t,
  @optional roles: array<Ref.t>,
  @optional data: Object.t,
}
@module("faunadb") @scope("query")
external createAccessProvider: createAccessProviderParamObject => Object.t = "CreateAccessProvider"

@deriving(abstract)
type createCollectionParamObject = {
  name: String.t,
  @optional data: Object.t,
  @optional @as("history_days") historyDays: Number.t,
  @optional @as("ttl_days") ttlDays: Number.t,
  @optional permissions: Object.t,
}
@module("faunadb") @scope("query")
external createCollection: createCollectionParamObject => Object.t = "CreateCollection"

@module("faunadb") @scope("query")
external createDatabase: t => t = "CreateDatabase"

@module("faunadb") @scope("query")
external createFunction: t => t = "CreateFunction"

@module("faunadb") @scope("query")
external createIndex: t => t = "CreateIndex"

@module("faunadb") @scope("query")
external createKey: t => t = "CreateKey"

@module("faunadb") @scope("query")
external createRole: t => t = "CreateRole"

@module("faunadb") @scope("query")
external credentials: unit => t = "Credentials"

@module("faunadb") @scope("query")
external credentialsWithDatabase: Ref.t => Set.t = "Credentials"

@module("faunadb") @scope("query")
external currentIdentity: unit => Ref.t = "CurrentIdentity"

@module("faunadb") @scope("query")
external currentToken: unit => t = "CurrentToken"

@module("faunadb") @scope("query")
external database: String.t => Ref.t = "Database"

@module("faunadb") @scope("query")
external databaseWithDatabase: (String.t, Ref.t) => Ref.t = "Database"

@module("faunadb") @scope("query")
external databases: String.t => Ref.t = "Databases"

@module("faunadb") @scope("query")
external databasesWithDatabase: (String.t, Ref.t) => Set.t = "Databases"

@module("faunadb") @scope("query")
external date: String.t => Date.t = "Date"

// let a = createCollection(String.make("1"))
// call()
