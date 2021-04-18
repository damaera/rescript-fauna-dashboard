type rec t =
  | Abort(t)
  | Abs(t)
  | AccessProvider(t)
  | AccessProviders(unit)
  | Acos(t)
  | Add(t)
  | All(t)
  | And(t)
  | Any(t)
  | Append(t, t)
  | Asin(t)
  | At(t, t)
  | Atan(t)
  | BitAnd(t)
  | BitNot(t)
  | BitOr(t)
  | BitXor(t)
  | Call(t, t)
  | CaseFold(t, option<t>)
  | Ceil(t)
  | Collection(t, option<t>)
  | Collections(option<t>)
  | Concat(t, option<t>)
  | ContainsField(t, t)
  | ContainsPath(t, t)
  | ContainsStr(t, t)
  | ContainsStrRegex(t, t)
  | ContainsValue(t, t)
  | Cos(t)
  | Cosh(t)
  | Count(t)
  | Create(t, t)
  | CreateAccessProvider(t)
  | CreateCollection(t)
  | CreateDatabase(t)
  | CreateFunction(t)
  | CreateIndex(t)
  | CreateKey(t)
  | CreateRole(t)
  | Credentials(unit)
  | CurrentIdentity(unit)
  | CurrentToken(unit)
  | Database(t, option<t>)
  | Databases(option<t>)
  | Date(t)
  | DayOfMonth(t)
  | DayOfWeek(t)
  | DayOfYear(t)
  | Degrees(t)
  | Delete(t)
  | Difference(t, t)
  | Distinct(t)
  | Divide(t, t)
  | Do(t)
  | Documents(t)
  | Drop(t, t)
  | EndsWith(t, t)
  | Epoch(t, t)
  | Equals(t, t)
  | Events(t)
  | Exists(t, option<t>)
  | Exp(t)
  | Filter(t, t)
  | FindStr(t, t, option<t>)
  | FindStrRegex(t, t, option<t>, option<t>)
  | Floor(t)
  | ForEach(t, t)
  | Format(t, t)
  | Function(t, option<t>)
  | Functions(option<t>)
  | GT(t)
  | GTE(t)
  | Get(t)
  | Get_ts(t)
  | HasCurrentIdentity(unit)
  | HasCurrentToken(unit)
  | Hour(t)
  | Hypot(t, t)
  | Identify(t, t)
  | If(t, t, t)
  | Index(t, option<t>)
  | Indexes(option<t>)
  | Insert(t, t, t, t)
  | Intersection(t)
  | IsArray(t)
  | IsBoolean(t)
  | IsBytes(t)
  | IsCollection(t)
  | IsCredentials(t)
  | IsDatabase(t)
  | IsDate(t)
  | IsDoc(t)
  | IsDouble(t)
  | IsEmpty(t)
  | IsFunction(t)
  | IsIndex(t)
  | IsInteger(t)
  | IsKey(t)
  | IsLambda(t)
  | IsNonEmpty(t)
  | IsNull(t)
  | IsNumber(t)
  | IsObject(t)
  | IsRef(t)
  | IsRole(t)
  | IsSet(t)
  | IsString(t)
  | IsTimestamp(t)
  | IsToken(t)
  | Join(t, t)
  | KeyFromSecret(t)
  | Keys(option<t>)
  | LT(t)
  | LTE(t)
  | LTrim(t)
  | Lambda(t, t)
  | Length(t)
  | Let(t, t)
  | Ln(t)
  | Log(t)
  | Login(t, t)
  | Logout(t)
  | LowerCase(t)
  | Map(t, t)
  | Match(t, t)
  | Max(t)
  | Mean(t)
  | Merge(t, t, option<t>)
  | Min(t)
  | Minute(t)
  | Modulo(t)
  | Month(t)
  | MoveDatabase(t, t)
  | Multiply(t)
  | NewId(unit)
  | Not(t)
  | Now(unit)
  | Or(t)
  | Paginate(t, t)
  | Pow(t, t)
  | Prepend(t, t)
  | Query(t)
  | RTrim(t)
  | Radians(t)
  | Range(t, t, t)
  | Reduce(t, t, t)
  | Ref(t, t)
  | RegexEscape(t)
  | Remove(t, t, t)
  | Repeat(t, option<t>)
  | Replace(t, t)
  | ReplaceStr(t, t, t)
  | ReplaceStrRegex(t, t, t, option<t>)
  | Reverse(t)
  | Role(t, option<t>)
  | Roles(option<t>)
  | Round(t, option<t>)
  | Second(t)
  | Select(t, t, t)
  | Sign(t)
  | Sin(t)
  | Singleton(t)
  | Sinh(t)
  | Space(t)
  | Sqrt(t)
  | StartsWith(t, t)
  | SubString(t, t, option<t>)
  | Subtract(t)
  | Sum(t)
  | Take(t, t)
  | Tan(t)
  | Tanh(t)
  | Time(t)
  | TimeAdd(t, t, t)
  | TimeDiff(t, t, t)
  | TimeSubtract(t, t, t)
  | TitleCase(t)
  | ToArray(t)
  | ToDate(t)
  | ToDouble(t)
  | ToInteger(t)
  | ToMicros(t)
  | ToMillis(t)
  | ToNumber(t)
  | ToObject(t)
  | ToSeconds(t)
  | ToString(t)
  | ToTime(t)
  | Tokens(option<t>)
  | Trim(t)
  | Trunc(t, option<t>)
  | Union(t)
  | Update(t, t)
  | UpperCase(t)
  | Var(t)
  | Year(t)
  // primitives
  | String(string)
  | Int(int)
  | Float(float)
  | Array(array<t>)
  | Object(t)
  | True
  | False
  | Null

external makeObject: {..} => t = "%identity"

let rec make = t => {
  let s1 = (key, val) => {
    `{"${key}":${make(val)}}`
  }
  let s2 = (key1, val1, key2, val2) => {
    `{"${key1}":${make(val1)},"${key2}":${make(val2)}}`
  }
  // let s3 = (key1, val1, key2, val2, key3, val3) => {
  //   `{"${key1}":${make(val1)},"${key2}":${make(val2)}},"${key3}":${make(val3)}}`
  // }

  switch t {
  | Abort(q1) => s1("abort", q1)
  | Abs(q1) => s1("abs", q1)
  | AccessProvider(q1) => s1("access_provider", q1)
  | AccessProviders(_) => s1("access_providers", Null)
  | Acos(q1) => s1("acos", q1)
  | Add(q1) => s1("add", q1)
  | All(q1) => s1("all", q1)
  | And(q1) => s1("and", q1)
  | Any(q1) => s1("any", q1)
  | Append(q1, q2) => s2("append", q1, "collection", q2)
  | Asin(q1) => s1("asin", q1)
  | At(q1, q2) => s2("at", q1, "expr", q2)
  | Atan(q1) => s1("atan", q1)
  | BitAnd(q1) => s1("bitand", q1)
  | BitNot(q1) => s1("bitnot", q1)
  | BitOr(q1) => s1("bitor", q1)
  | BitXor(q1) => s1("bitxor", q1)
  | Call(q1, q2) => s2("call", q1, "arguments", q2)
  | CaseFold(q1, q2) =>
    switch q2 {
    | None => s1("casefold", q1)
    | Some(q2) => s2("casefold", q1, "normalizer", q2)
    }
  | Ceil(q1) => s1("ceil", q1)
  | Collection(q1, q2) =>
    switch q2 {
    | None => s1("collection", q1)
    | Some(q2) => s2("collection", q1, "scope", q2)
    }
  | Collections(q1) =>
    switch q1 {
    | None => s1("collections", Null)
    | Some(q1) => s1("collections", q1)
    }
  | Concat(q1, q2) =>
    switch q2 {
    | None => s1("concat", q1)
    | Some(q2) => s2("concat", q1, "separator", q2)
    }
  | ContainsField(q1, q2) => s2("contains_field", q1, "in", q2)
  | ContainsPath(q1, q2) => s2("contains_path", q1, "in", q2)
  | ContainsStr(q1, q2) => s2("contains_str", q1, "search", q2)
  | ContainsStrRegex(q1, q2) => s2("containsstrregex", q1, "pattern", q2) // special case
  | ContainsValue(q1, q2) => s2("contains_value", q1, "in", q2)
  | Cos(q1) => s1("cos", q1)
  | Cosh(q1) => s1("cosh", q1)
  | Count(q1) => s1("count", q1)
  | Create(q1, q2) => s2("create", q1, "params", q2)
  | CreateAccessProvider(q1) => s1("create_access_provider", q1)
  | CreateCollection(q1) => s1("create_collection", q1)
  | CreateDatabase(q1) => s1("create_database", q1)
  | CreateFunction(q1) => s1("create_function", q1)
  | CreateIndex(q1) => s1("create_index", q1)
  | CreateKey(q1) => s1("create_key", q1)
  | CreateRole(q1) => s1("create_role", q1)
  | Credentials(_) => s1("credentials", Null)
  | CurrentIdentity(_) => s1("current_identity", Null)
  | CurrentToken(_) => s1("current_token", Null)
  //
  // | Select(q1, q2) => `{"select":${make(q1)},"from":${make(q2)}}`
  // | Ref(q1, q2) => `{"ref":${make(q1)},"id":${make(q2)}}`
  // primitives
  | String(str) => str
  | Int(int) => Js.Int.toString(int)
  | Float(float) => Js.Float.toString(float)
  | Array(arr) => `[${arr->Js.Array2.map(item => make(item))->Js.Array2.joinWith(",")}]`
  | True => "true"
  | False => "false"
  | Null => "null"
  | Object(obj) =>
    switch Js.Json.stringifyAny(obj) {
    | Some(obj) => s1("object", String(obj))
    | None => ""
    }
  | _ => ""
  }
}

let a = CreateCollection(
  Object(
    makeObject({
      "name": "test2",
    }),
  ),
)->make

Js.log(a)