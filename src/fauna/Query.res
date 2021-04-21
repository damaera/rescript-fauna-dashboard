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
  | Credentials(option<t>)
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
  | Difference(t)
  | Distinct(t)
  | Divide(t)
  | Do(t)
  | Documents(t)
  | Drop(t, t)
  | EndsWith(t, t)
  | Epoch(t, t)
  | Equals(t)
  | Events(t)
  | Exists(t, option<t>)
  | Exp(t)
  | Filter(t, t)
  | FindStr(t, t, option<t>)
  | FindStrRegex(t, t, option<t>, option<t>)
  | Floor(t)
  | Foreach(t, t)
  | Format(t, option<t>)
  | Function(t, option<t>)
  | Functions(option<t>)
  | GT(t)
  | GTE(t)
  | Get(t, option<t>)
  | HasCurrentIdentity(unit)
  | HasCurrentToken(unit)
  | Hour(t)
  | Hypot(t, option<t>)
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
  | Match(t, option<t>)
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
  | Paginate(t, option<t>)
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
  | Select(t, t, option<t>)
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
  | Dict(Js.Dict.t<t>)
  | Object(t)
  | True
  | False
  | Null

external makeAny: {..} => t = "%identity"

let rec make = t => {
  let s1 = (key, val) => {
    `{"${key}":${make(val)}}`
  }
  let s2 = (key1, val1, key2, val2) => {
    `{"${key1}":${make(val1)},"${key2}":${make(val2)}}`
  }
  let s3 = (key1, val1, key2, val2, key3, val3) => {
    `{"${key1}":${make(val1)},"${key2}":${make(val2)}},"${key3}":${make(val3)}}`
  }
  let s4 = (key1, val1, key2, val2, key3, val3, key4, val4) => {
    `{"${key1}":${make(val1)},"${key2}":${make(val2)}},"${key3}":${make(val3)},"${key4}":${make(
        val4,
      )}}`
  }

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
  | CaseFold(q1, q2opt) =>
    switch q2opt {
    | None => s1("casefold", q1)
    | Some(q2) => s2("casefold", q1, "normalizer", q2)
    }
  | Ceil(q1) => s1("ceil", q1)
  | Collection(q1, q2opt) =>
    switch q2opt {
    | None => s1("collection", q1)
    | Some(q2) => s2("collection", q1, "scope", q2)
    }
  | Collections(q1opt) =>
    switch q1opt {
    | None => s1("collections", Null)
    | Some(q1) => s1("collections", q1)
    }
  | Concat(q1, q2opt) =>
    switch q2opt {
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
  | Database(q1, q2opt) =>
    switch q2opt {
    | None => s1("database", q1)
    | Some(q2opt) => s2("database", q1, "scope", q2opt)
    }
  | Databases(q1) =>
    switch q1 {
    | None => s1("databases", Null)
    | Some(q1) => s1("databases", q1)
    }
  | Date(q1) => s1("date", q1)
  | DayOfMonth(q1) => s1("day_of_month", q1)
  | DayOfWeek(q1) => s1("day_of_week", q1)
  | DayOfYear(q1) => s1("day_of_year", q1)
  | Degrees(q1) => s1("degrees", q1)
  | Delete(q1) => s1("delete", q1)
  | Difference(q1) => s1("difference", q1)
  | Distinct(q1) => s1("distinct", q1)
  | Divide(q1) => s1("divide", q1)
  | Do(q1) => s1("do", q1)
  | Documents(q1) => s1("documents", q1)
  | Drop(q1, q2) => s2("drop", q1, "collection", q2)
  | EndsWith(q1, q2) => s2("endswith", q1, "search", q2)
  | Epoch(q1, q2) => s2("epoch", q1, "unit", q2)
  | Equals(q1) => s1("equals", q1)
  | Events(q1) => s1("events", q1)
  | Exists(q1, q2opt) =>
    switch q2opt {
    | None => s1("exists", q1)
    | Some(q2) => s2("exists", q1, "ts", q2)
    }
  | Exp(q1) => s1("exp", q1)
  | Filter(q1, q2) => s2("filter", q1, "lambda", q2)
  | FindStr(q1, q2, q3opt) =>
    switch q3opt {
    | None => s2("findstr", q1, "find", q2)
    | Some(q3) => s3("findstr", q1, "find", q2, "start", q3)
    }
  | FindStrRegex(q1, q2, q3opt, q4opt) =>
    switch (q3opt, q4opt) {
    | (None, None) => s2("findstrregex", q1, "pattern", q2)
    | (Some(q3), None) => s3("findstrregex", q1, "pattern", q2, "start", q3)
    | (None, Some(q4)) => s3("findstrregex", q1, "pattern", q2, "num_results", q4)
    | (Some(q3), Some(q4)) => s4("findstrregex", q1, "pattern", q2, "start", q3, "num_results", q4)
    }
  | Floor(q1) => s1("floor", q1)
  | Foreach(q1, q2) => s2("foreach", q2, "collection", q1) // <->
  | Format(q1, q2opt) =>
    switch q2opt {
    | None => s1("format", q1)
    | Some(q2) => s2("format", q1, "values", q2)
    }
  | Function(q1, q2opt) =>
    switch q2opt {
    | None => s1("function", q1)
    | Some(q2) => s2("function", q1, "scope", q2)
    }
  | Functions(q1opt) =>
    switch q1opt {
    | None => s1("functions", Null)
    | Some(q1) => s1("functions", q1)
    }
  | GT(q1) => s1("gt", q1)
  | GTE(q1) => s1("gte", q1)
  | Get(q1, q2opt) =>
    switch q2opt {
    | None => s1("get", q1)
    | Some(q2) => s2("get", q1, "ts", q2)
    }
  | HasCurrentIdentity(_) => s1("has_current_identity", Null)
  | HasCurrentToken(_) => s1("has_current_token", Null)
  | Hour(q1) => s1("hour", q1)
  | Hypot(q1, q2opt) =>
    switch q2opt {
    | None => s1("hypot", q1)
    | Some(q2) => s2("hypot", q1, "b", q2)
    }
  | Identify(q1, q2) => s2("identify", q1, "password", q2)
  | If(q1, q2, q3) => s3("if", q1, "then", q2, "else", q3)
  | Index(q1, q2opt) =>
    switch q2opt {
    | None => s1("index", q1)
    | Some(q2) => s2("index", q1, "scope", q2)
    }
  | Indexes(q1opt) =>
    switch q1opt {
    | None => s1("indexes", Null)
    | Some(q1) => s1("indexes", q1)
    }
  | Insert(q1, q2, q3, q4) => s4("insert", q1, "ts", q2, "action", q3, "params", q4)
  | Intersection(q1) => s1("intersection", q1)
  | IsArray(q1) => s1("is_array", q1)
  | IsBoolean(q1) => s1("is_boolean", q1)
  | IsBytes(q1) => s1("is_bytes", q1)
  | IsCollection(q1) => s1("is_collection", q1)
  | IsCredentials(q1) => s1("is_credentials", q1)
  | IsDatabase(q1) => s1("is_database", q1)
  | IsDate(q1) => s1("is_date", q1)
  | IsDoc(q1) => s1("is_doc", q1)
  | IsDouble(q1) => s1("is_double", q1)
  | IsEmpty(q1) => s1("is_empty", q1)
  | IsFunction(q1) => s1("is_function", q1)
  | IsIndex(q1) => s1("is_index", q1)
  | IsInteger(q1) => s1("is_integer", q1)
  | IsKey(q1) => s1("is_key", q1)
  | IsLambda(q1) => s1("is_lambda", q1)
  | IsNonEmpty(q1) => s1("is_nonempty", q1)
  | IsNull(q1) => s1("is_null", q1)
  | IsNumber(q1) => s1("is_number", q1)
  | IsObject(q1) => s1("is_object", q1)
  | IsRef(q1) => s1("is_ref", q1)
  | IsRole(q1) => s1("is_role", q1)
  | IsSet(q1) => s1("is_set", q1)
  | IsString(q1) => s1("is_string", q1)
  | IsTimestamp(q1) => s1("is_timestamp", q1)
  | IsToken(q1) => s1("is_token", q1)
  | Join(q1, q2) => s2("join", q1, "with", q2)
  | KeyFromSecret(q1) => s1("key_from_secret", q1)
  | Keys(q1opt) =>
    switch q1opt {
    | None => s1("keys", Null)
    | Some(q1) => s1("keys", q1)
    }
  | LT(q1) => s1("lt", q1)
  | LTE(q1) => s1("lte", q1)
  | LTrim(q1) => s1("ltrim", q1)
  | Lambda(q1, q2) => s2("lambda", q1, "expr", q2)
  | Length(q1) => s1("length", q1)
  | Let(q1, q2) => s2("let", q1, "in", q2)
  | Ln(q1) => s1("ln", q1)
  | Log(q1) => s1("log", q1)
  | Login(q1, q2) => s2("login", q1, "params", q2)
  | Logout(q1) => s1("logout", q1)
  | LowerCase(q1) => s1("lowercase", q1)
  | Map(q1, q2) => s2("collection", q1, "map", q2)
  | Match(q1, q2opt) =>
    switch q2opt {
    | None => s1("match", q1)
    | Some(q2) => s2("match", q1, "terms", q2)
    }
  | Max(q1) => s1("max", q1)
  | Mean(q1) => s1("mean", q1)
  | Merge(q1, q2, q3opt) =>
    switch q3opt {
    | None => s2("merge", q1, "with", q2)
    | Some(q3) => s3("merge", q1, "with", q2, "lambda", q3)
    }
  | Min(q1) => s1("min", q1)
  | Minute(q1) => s1("minute", q1)
  | Modulo(q1) => s1("modulo", q1)
  | Month(q1) => s1("month", q1)
  | MoveDatabase(q1, q2) => s2("move_database", q1, "to", q2)
  | Multiply(q1) => s1("multiply", q1)
  | NewId(_) => s1("new_id", Null)
  | Not(q1) => s1("not", q1)
  | Now() => s1("now", Null)
  | Or(q1) => s1("or", q1)
  | Paginate(q1, q2) =>
    switch q2 {
    | Some(Dict(dict)) => {
        let opts =
          dict
          ->Js.Dict.entries
          ->Js.Array2.map(((key, val_)) => {
            `${key}:${val_->make}`
          })
          ->Js.Array2.joinWith(",")
        `{"paginate":${make(q1)},${opts}}`
      }
    | _ => s1("paginate", q1)
    }
  | Pow(q1, q2) => s2("pow", q1, "exp", q2)
  | Prepend(q1, q2) => s2("prepend", q1, "collection", q2)
  | Query(q1) => s1("query", q1)
  | RTrim(q1) => s1("rtrim", q1)
  | Radians(q1) => s1("radians", q1)
  | Range(q1, q2, q3) => s3("range", q1, "from", q2, "to", q3)
  | Reduce(q1, q2, q3) => s3("reduce", q1, "initial", q2, "collection", q3)
  | Ref(q1, q2) => s2("ref", q1, "id", q2)
  | RegexEscape(q1) => s1("regexescape", q1)
  | Remove(q1, q2, q3) => s3("remove", q1, "ts", q2, "action", q3)
  | Repeat(q1, q2opt) =>
    switch q2opt {
    | None => s1("repeat", q1)
    | Some(q2) => s2("repeat", q1, "number", q2)
    }
  | Replace(q1, q2) => s2("replace", q1, "params", q2)
  | ReplaceStr(q1, q2, q3) => s3("replacestr", q1, "find", q2, "replace", q3)
  | ReplaceStrRegex(q1, q2, q3, q4opt) =>
    switch q4opt {
    | None => s3("replacestrregex", q1, "pattern", q2, "replace", q3)
    | Some(q4) => s4("replacestrregex", q1, "pattern", q2, "replace", q3, "first", q4)
    }
  | Reverse(q1) => s1("reverse", q1)
  | Role(q1, q2opt) =>
    switch q2opt {
    | None => s1("role", q1)
    | Some(q2) => s2("role", q1, "scope", q2)
    }
  | Roles(q1opt) =>
    switch q1opt {
    | None => s1("roles", Null)
    | Some(q1) => s1("roles", q1)
    }
  | Round(q1, q2opt) =>
    switch q2opt {
    | None => s1("round", q1)
    | Some(q2) => s2("round", q1, "precision", q2)
    }
  | Second(q1) => s1("second", q1)
  | Select(q1, q2, q3opt) =>
    switch q3opt {
    | None => s2("select", q1, "from", q2)
    | Some(q3) => s3("select", q1, "from", q2, "default", q3)
    }
  | Sign(q1) => s1("sign", q1)
  | Sin(q1) => s1("sin", q1)
  | Singleton(q1) => s1("singleton", q1)
  | Sinh(q1) => s1("sinh", q1)
  | Space(q1) => s1("space", q1)
  | Sqrt(q1) => s1("sqrt", q1)
  | StartsWith(q1, q2) => s2("startswith", q1, "search", q2)
  | SubString(q1, q2, q3opt) =>
    switch q3opt {
    | None => s2("substring", q1, "start", q2)
    | Some(q3) => s3("substring", q1, "start", q2, "length", q3)
    }
  | Subtract(q1) => s1("subtract", q1)
  | Sum(q1) => s1("sum", q1)
  | Take(q1, q2) => s2("take", q1, "collection", q2)
  | Tan(q1) => s1("tan", q1)
  | Tanh(q1) => s1("tanh", q1)
  | Time(q1) => s1("time", q1)
  | TimeAdd(q1, q2, q3) => s3("time_add", q1, "offset", q2, "unit", q3)
  | TimeDiff(q1, q2, q3) => s3("time_diff", q1, "other", q2, "unit", q3)
  | TimeSubtract(q1, q2, q3) => s3("time_subtract", q1, "offset", q2, "unit", q3)
  | TitleCase(q1) => s1("titlecase", q1)
  | ToArray(q1) => s1("to_array", q1)
  | ToDate(q1) => s1("to_date", q1)
  | ToDouble(q1) => s1("to_double", q1)
  | ToInteger(q1) => s1("to_integer", q1)
  | ToMicros(q1) => s1("to_micros", q1)
  | ToMillis(q1) => s1("to_millis", q1)
  | ToNumber(q1) => s1("to_number", q1)
  | ToObject(q1) => s1("to_object", q1)
  | ToSeconds(q1) => s1("to_seconds", q1)
  | ToString(q1) => s1("to_string", q1)
  | ToTime(q1) => s1("to_time", q1)
  | Tokens(q1opt) =>
    switch q1opt {
    | None => s1("tokens", Null)
    | Some(q1) => s1("tokens", q1)
    }
  | Trim(q1) => s1("trim", q1)
  | Trunc(q1, q2opt) =>
    switch q2opt {
    | None => s1("trunc", q1)
    | Some(q2) => s2("trunc", q1, "precision", q2)
    }
  | Union(q1) => s1("union", q1)
  | Update(q1, q2) => s2("update", q1, "params", q2)
  | UpperCase(q1) => s1("uppercase", q1)
  | Var(q1) => s1("var", q1)
  | Year(q1) => s1("year", q1)

  // primitives
  | String(str) => `"${str}"`
  | Int(int) => Js.Int.toString(int)
  | Float(float) => Js.Float.toString(float)
  | Array(arr) => `[${arr->Js.Array2.map(item => make(item))->Js.Array2.joinWith(",")}]`
  | True => "true"
  | False => "false"
  | Null => "null"
  | Dict(dict) => {
      let opts =
        dict
        ->Js.Dict.entries
        ->Js.Array2.map(((key, val_)) => {
          `${key}:${val_->make}`
        })
        ->Js.Array2.joinWith(",")
      `{${opts}}`
    }
  | Object(obj) => s1("object", obj)
  }
}
